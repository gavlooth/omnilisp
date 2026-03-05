module lisp;

import std::io;
import std::collections::list;
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
}

// Global JIT initialization flag
bool g_jit_initialized = false;

// JIT state cleanup tracking (C9: increased from 64 to 256, then to 4096).
// Larger headroom avoids overflow warnings during long nested JIT chains where
// safe-point GC cannot run until execution unwinds.
const usz JIT_STATE_POOL_SIZE = 16384;
const usz JIT_STATE_GC_THRESHOLD = JIT_STATE_POOL_SIZE * 3 / 4;  // GC when pool reaches 75%
void*[JIT_STATE_POOL_SIZE] g_jit_states;
usz g_jit_state_count = 0;
bool g_jit_pool_warned = false;
bool g_jit_gc_needed = false;  // signals that GC should run between evals
usz g_jit_exec_depth = 0;      // active generated JIT frame depth

// JIT compilation cache: Expr* → JitFn (open-addressing hash on pointer value)
struct JitCacheEntry {
    Expr* expr;     // cache key (pointer identity)
    JitFn compiled; // compiled function
}

const usz JIT_CACHE_SIZE = 1024;
const usz JIT_CACHE_GC_THRESHOLD = 768;  // clear cache at 75% capacity
JitCacheEntry[JIT_CACHE_SIZE] g_jit_cache;
usz g_jit_cache_count = 0;

fn JitFn jit_cache_lookup(Expr* expr) {
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (g_jit_cache[slot].expr == expr) return g_jit_cache[slot].compiled;
        if (g_jit_cache[slot].expr == null) return null;
    }
    return null;
}

fn void jit_cache_store(Expr* expr, JitFn compiled) {
    // If cache is too full, clear it entirely (codes remain valid in state pool)
    if (g_jit_cache_count >= JIT_CACHE_GC_THRESHOLD) {
        $if DEBUG_BUILD:
            io::eprintfn("[debug] JIT cache full (%d/%d), clearing", g_jit_cache_count, JIT_CACHE_SIZE);
        $endif
        jit_cache_clear();
    }
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (g_jit_cache[slot].expr == null || g_jit_cache[slot].expr == expr) {
            g_jit_cache[slot].expr = expr;
            g_jit_cache[slot].compiled = compiled;
            g_jit_cache_count++;
            return;
        }
    }
}

/**
 * Clear the JIT cache. Safe to call anytime — just causes recompilation on next access.
 * Does NOT free state memory (codes remain valid but unreachable from cache).
 */
fn void jit_cache_clear() {
    for (usz i = 0; i < JIT_CACHE_SIZE; i++) {
        g_jit_cache[i].expr = null;
        g_jit_cache[i].compiled = null;
    }
    g_jit_cache_count = 0;
}

fn void jit_global_init() {
    if (!g_jit_initialized) {
        init_jit(null);
        g_jit_initialized = true;
    }
}

fn void jit_global_shutdown() {
    if (g_jit_initialized) {
        // Destroy all tracked JIT states
        for (usz i = 0; i < g_jit_state_count; i++) {
            _jit_destroy_state(g_jit_states[i]);
            g_jit_states[i] = null;
        }
        g_jit_state_count = 0;
        jit_cache_clear();
        g_jit_gc_needed = false;
        g_jit_pool_warned = false;
        finish_jit();
        g_jit_initialized = false;
    }
}

/**
 * Garbage-collect JIT states and cache.
 *
 * SAFETY: Must only be called between top-level evaluations, when NO JIT
 * code is on the call stack. Destroys all state objects (freeing their code
 * buffers) and clears the compilation cache. Subsequent jit_eval() calls
 * will recompile on demand.
 */
fn void jit_gc() {
    if (!g_jit_gc_needed) return;

    // Full teardown between top-level evals: no JIT frames should be active.
    for (usz i = 0; i < g_jit_state_count; i++) {
        _jit_destroy_state(g_jit_states[i]);
        g_jit_states[i] = null;
    }
    g_jit_state_count = 0;
    jit_cache_clear();
    g_jit_gc_needed = false;
    g_jit_pool_warned = false;
}

fn void jit_exec_enter() @inline {
    g_jit_exec_depth++;
}

fn void jit_exec_leave() @inline {
    assert(g_jit_exec_depth > 0, "jit exec depth underflow");
    g_jit_exec_depth--;
}

fn void jit_track_compiled_state(void* s) {
    if (g_jit_state_count < JIT_STATE_POOL_SIZE) {
        g_jit_states[g_jit_state_count] = s;
        g_jit_state_count++;
        // Signal GC needed when approaching capacity.
        if (g_jit_state_count >= JIT_STATE_GC_THRESHOLD) {
            if (!g_jit_gc_needed) {
                g_jit_gc_needed = true;
                $if DEBUG_BUILD:
                    io::eprintfn("[debug] JIT pool at %d/%d (%.0f%%), GC scheduled",
                        g_jit_state_count, JIT_STATE_POOL_SIZE,
                        (double)g_jit_state_count / (double)JIT_STATE_POOL_SIZE * 100.0);
                $endif
            }
        }
        return;
    }

    if (!g_jit_pool_warned) {
        io::printfn("WARNING: JIT state pool full (%d states), further states leaked", JIT_STATE_POOL_SIZE);
        g_jit_pool_warned = true;
    }
}

/**
 * Compiled function type: takes Interp*, returns Value*
 */
alias JitFn = fn Value*(Interp*);

/**
 * Compile an expression to a native function.
 *
 * Returns null if compilation fails.
 * The compiled function signature is: Value* fn(Interp*)
 */
fn JitFn jit_compile(Expr* expr, Interp* interp) {
    jit_global_init();
    if (g_jit_gc_needed && g_jit_exec_depth == 0) {
        jit_gc();
    }

    void* s = jit_new_state();
    if (s == null) return null;

    // Function prolog: Value* fn(Interp* interp)
    _jit_prolog(s);
    void* arg = _jit_arg(s, CODE_ARG_L);
    _jit_getarg_l(s, JIT_V0, arg);  // V0 = interp (callee-saved)

    // Compile the expression, result goes to R0.
    JitLocals locals = {};
    if (catch err = jit_compile_expr(s, expr, interp, &locals, true)) {
        _jit_destroy_state(s);
        return null;
    }

    // Return R0
    _jit_retr(s, JIT_R0, CODE_RETR_L);

    // Emit machine code
    void* code = _jit_emit(s);

    // C10: If emit failed, destroy the state immediately and bail out
    if (code == null) {
        _jit_destroy_state(s);
        return null;
    }

    _jit_clear_state(s);

    // Track state for cleanup (code buffer lives inside it).
    // C9: Cannot destroy_state here — code buffer lives inside the state.
    // States are destroyed by jit_gc() between top-level evaluations.
    jit_track_compiled_state(s);

    return (JitFn)code;
}
