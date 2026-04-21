module lisp;

import std::io;
import std::collections::list;
import std::core::mem;
import main;
// =============================================================================
// SECTION 4: JIT COMPILER
// =============================================================================

struct JitCompiler {
    void*    state;       // jit_state_t*
    Interp*  interp;
    bool     initialized;
}

// Local variable table for JIT-compiled let bindings.
// Maps SymbolId -> stack frame offset (from JIT_FP).
struct JitLocal {
    SymbolId name;
    int stack_offset;  // offset from JIT_FP, returned by _jit_allocai
    bool is_mutable;   // true if closure-captured-and-mutated, uses env-based boxing
}

alias JitLocalList = List{JitLocal};

struct JitLocals {
    JitLocalList locals;
    int interp_stack_offset;
}

// Global JIT initialization flag
bool g_jit_initialized = false;
// Number of live interpreters that may still hold JIT function pointers.
usz g_jit_interp_refcount = 0;

/**
 * Compiled function type: takes Interp*, returns Value*
 */
alias JitFn = fn Value*(Interp*);
// JIT runtime globals are process-wide, but not free-threaded. Every mutating
// access must pass through jit_require_owner_thread; cross-thread use is a hard
// invariant violation rather than a data race tolerated by atomics.
bool g_jit_pool_warned = false;
bool g_jit_gc_needed = false;  // signals that GC should run between evals
usz g_jit_exec_depth = 0;      // active generated JIT frame depth
usz g_jit_suspended_guard_count = 0; // suspended StackCtx frames retained by JIT scope guards

// JIT compilation cache: Expr* → compiled-handle (open-addressing hash on pointer value)
struct JitCompiledFn {
    JitFn code_fn;
    ulong compile_nonce;
}

ulong g_jit_compile_nonce_counter = 0;
tlocal char g_jit_thread_marker;
void* g_jit_owner_thread_token = null;

fn JitCompiledFn jit_compiled_fn_null() @inline {
    return { .code_fn = null, .compile_nonce = 0 };
}

fn bool jit_compiled_fn_is_null(JitCompiledFn compiled) @inline {
    return compiled.code_fn == null || compiled.compile_nonce == 0;
}

fn bool jit_compiled_fn_equals(JitCompiledFn lhs, JitCompiledFn rhs) @inline {
    return lhs.code_fn == rhs.code_fn && lhs.compile_nonce == rhs.compile_nonce;
}

fn RuntimeCompiledExpr runtime_backend_wrap_compiled_expr(JitCompiledFn compiled) @inline {
    return { .code_fn = (void*)compiled.code_fn, .compile_nonce = compiled.compile_nonce };
}

fn JitCompiledFn runtime_backend_unwrap_compiled_expr(RuntimeCompiledExpr compiled) @inline {
    return {
        .code_fn = (JitFn)compiled.code_fn,
        .compile_nonce = compiled.compile_nonce,
    };
}
