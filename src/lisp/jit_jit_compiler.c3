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
}

// Global JIT initialization flag
bool g_jit_initialized = false;
// Number of live interpreters that may still hold JIT function pointers.
usz g_jit_interp_refcount = 0;

// JIT state cleanup tracking (C9: increased from 64 to 256, then to 4096).
// Larger headroom avoids overflow warnings during long nested JIT chains where
// safe-point GC cannot run until execution unwinds.
const usz JIT_STATE_POOL_SIZE = 16384;
const usz JIT_STATE_GC_THRESHOLD = JIT_STATE_POOL_SIZE * 3 / 4;  // GC when pool reaches 75%

/**
 * Compiled function type: takes Interp*, returns Value*
 */
alias JitFn = fn Value*(Interp*);

struct JitTrackedState {
    void* state;
    void* code;
    Interp* owner_interp;
    ulong owner_attach_serial;
    ulong compile_nonce;
}

JitTrackedState[JIT_STATE_POOL_SIZE] g_jit_states;
usz g_jit_state_count = 0;
bool g_jit_pool_warned = false;
bool g_jit_gc_needed = false;  // signals that GC should run between evals
usz g_jit_exec_depth = 0;      // active generated JIT frame depth
usz g_jit_suspended_guard_count = 0; // suspended StackCtx frames retained by JIT scope guards

struct JitStateSpillNode {
    void* state;
    void* code;
    Interp* owner_interp;
    ulong owner_attach_serial;
    ulong compile_nonce;
    JitStateSpillNode* next;
}

JitStateSpillNode* g_jit_spill_states = null;
usz g_jit_spill_count = 0;

// JIT compilation cache: Expr* → compiled-handle (open-addressing hash on pointer value)
struct JitCompiledFn {
    JitFn code_fn;
    ulong compile_nonce;
}

struct JitCacheEntry {
    Expr* expr;     // cache key (pointer identity)
    JitCompiledFn compiled;
    Interp* owner_interp;
    ulong owner_attach_serial;
}

const usz JIT_CACHE_SIZE = 1024;
const usz JIT_CACHE_GC_THRESHOLD = 768;  // clear cache at 75% capacity
JitCacheEntry[JIT_CACHE_SIZE] g_jit_cache;
usz g_jit_cache_count = 0;

struct JitAttachedInterp {
    Interp* interp;
    ulong attach_serial;
    usz active_exec_depth;
}

const usz JIT_MAX_ATTACHED_INTERPS = 1024;
JitAttachedInterp[JIT_MAX_ATTACHED_INTERPS] g_jit_attached_interps;
ulong g_jit_attach_serial_counter = 0;
ulong g_jit_compile_nonce_counter = 0;

struct JitRetiredCode {
    void* code;
    ulong owner_attach_serial;
}

const usz JIT_RETIRED_CODE_MAX = 262144;
JitRetiredCode[JIT_RETIRED_CODE_MAX] g_jit_retired_codes;
usz g_jit_retired_code_count = 0;
bool g_jit_retired_code_saturated = false;
tlocal char g_jit_thread_marker;
void* g_jit_owner_thread_token = null;

fn void* jit_current_thread_token() @inline {
    return (void*)&g_jit_thread_marker;
}

fn JitCompiledFn jit_compiled_fn_null() @inline {
    return { .code_fn = null, .compile_nonce = 0 };
}

fn bool jit_compiled_fn_is_null(JitCompiledFn compiled) @inline {
    return compiled.code_fn == null || compiled.compile_nonce == 0;
}

fn bool jit_compiled_fn_equals(JitCompiledFn lhs, JitCompiledFn rhs) @inline {
    return lhs.code_fn == rhs.code_fn && lhs.compile_nonce == rhs.compile_nonce;
}

fn void jit_thread_violation(char[] op) @inline {
    io::eprintfn("jit runtime thread-affinity violation in %s", op);
    unreachable("jit runtime used from non-owner thread");
}

fn void jit_require_owner_thread(char[] op) @inline {
    void* cur = jit_current_thread_token();
    if (g_jit_owner_thread_token == null) {
        g_jit_owner_thread_token = cur;
        return;
    }
    if (g_jit_owner_thread_token != cur) {
        jit_thread_violation(op);
    }
}

fn isz jit_attached_interp_index(Interp* interp) {
    if (interp == null) return -1;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (g_jit_attached_interps[i].interp == interp) return (isz)i;
    }
    return -1;
}

fn bool jit_interp_is_attached(Interp* interp) {
    return jit_attached_interp_index(interp) >= 0;
}

fn bool jit_attached_interp_serial(Interp* interp, ulong* serial_out) {
    if (serial_out == null) return false;
    isz idx = jit_attached_interp_index(interp);
    if (idx < 0) return false;
    *serial_out = g_jit_attached_interps[(usz)idx].attach_serial;
    return true;
}

fn bool jit_retired_code_contains(void* code, ulong owner_attach_serial) {
    if (code == null || owner_attach_serial == 0) return false;
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        if (g_jit_retired_codes[i].code == code &&
            g_jit_retired_codes[i].owner_attach_serial == owner_attach_serial) {
            return true;
        }
    }
    return false;
}

fn bool jit_retired_code_insert(void* code, ulong owner_attach_serial) {
    if (code == null || owner_attach_serial == 0) return true;
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        if (g_jit_retired_codes[i].code == code &&
            g_jit_retired_codes[i].owner_attach_serial == owner_attach_serial) {
            return true;
        }
    }
    if (g_jit_retired_code_count < JIT_RETIRED_CODE_MAX) {
        g_jit_retired_codes[g_jit_retired_code_count].code = code;
        g_jit_retired_codes[g_jit_retired_code_count].owner_attach_serial = owner_attach_serial;
        g_jit_retired_code_count++;
        return true;
    }
    if (!g_jit_retired_code_saturated) {
        io::eprintfn("[warn] jit retired-code tombstone table saturated; pointer reuse guard degraded");
        g_jit_retired_code_saturated = true;
    }
    return false;
}

fn void jit_retired_code_forget_serial(ulong owner_attach_serial) {
    if (owner_attach_serial == 0) return;
    usz write = 0;
    for (usz read = 0; read < g_jit_retired_code_count; read++) {
        if (g_jit_retired_codes[read].owner_attach_serial == owner_attach_serial) continue;
        if (write != read) {
            g_jit_retired_codes[write] = g_jit_retired_codes[read];
        }
        write++;
    }
    for (usz i = write; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = write;
}

fn void jit_retired_code_reset() {
    for (usz i = 0; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = 0;
    g_jit_retired_code_saturated = false;
}

fn bool jit_is_attached_serial(ulong attach_serial) {
    if (attach_serial == 0) return false;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (g_jit_attached_interps[i].interp == null) continue;
        if (g_jit_attached_interps[i].attach_serial == attach_serial) return true;
    }
    return false;
}

fn void jit_retired_code_prune_detached_serials() {
    usz write = 0;
    for (usz read = 0; read < g_jit_retired_code_count; read++) {
        ulong serial = g_jit_retired_codes[read].owner_attach_serial;
        if (!jit_is_attached_serial(serial)) continue;
        if (write != read) {
            g_jit_retired_codes[write] = g_jit_retired_codes[read];
        }
        write++;
    }
    for (usz i = write; i < g_jit_retired_code_count; i++) {
        g_jit_retired_codes[i].code = null;
        g_jit_retired_codes[i].owner_attach_serial = 0;
    }
    g_jit_retired_code_count = write;
}

fn bool jit_interp_has_active_exec(Interp* interp) {
    isz idx = jit_attached_interp_index(interp);
    return idx >= 0 && g_jit_attached_interps[(usz)idx].active_exec_depth > 0;
}

fn bool jit_interp_on_current_stack(Interp* interp) {
    if (interp == null) return false;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) return false;
    return current.owner_pool == &interp.stack_ctx_pool;
}

fn void jit_register_attached_interp(Interp* interp) {
    assert(interp != null, "jit_register_attached_interp: null interp");
    if (jit_interp_is_attached(interp)) return;
    g_jit_attach_serial_counter++;
    if (g_jit_attach_serial_counter == 0) g_jit_attach_serial_counter = 1;
    ulong serial = g_jit_attach_serial_counter;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (g_jit_attached_interps[i].interp == null) {
            g_jit_attached_interps[i].interp = interp;
            g_jit_attached_interps[i].attach_serial = serial;
            g_jit_attached_interps[i].active_exec_depth = 0;
            return;
        }
    }
    unreachable("jit attached interp table full");
}

fn void jit_unregister_attached_interp(Interp* interp) {
    assert(interp != null, "jit_unregister_attached_interp: null interp");
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (g_jit_attached_interps[i].interp == interp) {
            g_jit_attached_interps[i].interp = null;
            g_jit_attached_interps[i].attach_serial = 0;
            g_jit_attached_interps[i].active_exec_depth = 0;
            return;
        }
    }
}

<*
@require expr != null : "jit_cache_lookup: expr must not be null"
*>
fn JitCompiledFn jit_cache_lookup(Expr* expr, Interp* interp = null) {
    jit_require_owner_thread("jit_cache_lookup_with_interp");
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    ulong interp_serial = 0;
    bool require_owner_match = interp != null;
    if (require_owner_match && !jit_attached_interp_serial(interp, &interp_serial)) {
        return jit_compiled_fn_null();
    }
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (g_jit_cache[slot].expr == expr) {
            if (!require_owner_match) return g_jit_cache[slot].compiled;
            if (g_jit_cache[slot].owner_interp != interp ||
                g_jit_cache[slot].owner_attach_serial != interp_serial) {
                // Owner/serial mismatch means this expr key is stale for current
                // interpreter epoch; drop cache to avoid probing ambiguity.
                jit_cache_clear();
                return jit_compiled_fn_null();
            }
            JitCompiledFn compiled = g_jit_cache[slot].compiled;
            if (jit_compiled_fn_is_null(compiled)) return jit_compiled_fn_null();
            if (!jit_compiled_fn_is_live_for_interp(compiled, interp)) {
                jit_cache_clear();
                return jit_compiled_fn_null();
            }
            return compiled;
        }
        if (g_jit_cache[slot].expr == null) return jit_compiled_fn_null();
    }
    return jit_compiled_fn_null();
}

<*
@require expr != null : "jit_cache_find_slot: expr must not be null"
@require slot_out != null : "jit_cache_find_slot: slot_out must not be null"
@require is_new_entry_out != null : "jit_cache_find_slot: is_new_entry_out must not be null"
*>
fn bool jit_cache_find_slot(Expr* expr, usz* slot_out, bool* is_new_entry_out) {
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (g_jit_cache[slot].expr == expr) {
            *slot_out = slot;
            *is_new_entry_out = false;
            return true;
        }
        if (g_jit_cache[slot].expr == null) {
            *slot_out = slot;
            *is_new_entry_out = true;
            return true;
        }
    }
    return false;
}

<*
@require expr != null : "jit_cache_commit_slot: expr must not be null"
@require compiled.code_fn != null : "jit_cache_commit_slot: compiled fn must not be null"
@require compiled.compile_nonce != 0 : "jit_cache_commit_slot: compiled nonce must not be zero"
*>
fn void jit_cache_commit_slot(
    usz slot,
    Expr* expr,
    JitCompiledFn compiled,
    bool is_new_entry,
    Interp* owner_interp,
    ulong owner_attach_serial
) {
    g_jit_cache[slot].expr = expr;
    g_jit_cache[slot].compiled = compiled;
    g_jit_cache[slot].owner_interp = owner_interp;
    g_jit_cache[slot].owner_attach_serial = owner_attach_serial;
    if (is_new_entry) g_jit_cache_count++;
}

<*
@require expr != null : "jit_cache_store: expr must not be null"
@require compiled.code_fn != null : "jit_cache_store: compiled fn must not be null"
@require compiled.compile_nonce != 0 : "jit_cache_store: compiled nonce must not be zero"
*>
fn void jit_cache_store(Expr* expr, JitCompiledFn compiled, Interp* interp = null) {
    jit_require_owner_thread("jit_cache_store");
    Interp* owner_interp = null;
    ulong owner_serial = 0;
    if (interp != null) {
        if (!jit_attached_interp_serial(interp, &owner_serial)) return;
        owner_interp = interp;
    }
    usz slot = 0;
    bool is_new_entry = false;
    if (jit_cache_find_slot(expr, &slot, &is_new_entry)) {
        // Only clear on true growth; overwrites must not evict unrelated entries.
        if (is_new_entry && g_jit_cache_count >= JIT_CACHE_GC_THRESHOLD) {
            $if DEBUG_BUILD:
                io::eprintfn("[debug] JIT cache full (%d/%d), clearing", g_jit_cache_count, JIT_CACHE_SIZE);
            $endif
            jit_cache_clear();
            if (!jit_cache_find_slot(expr, &slot, &is_new_entry) || !is_new_entry) {
                $if DEBUG_BUILD:
                    io::eprintfn("[warn] jit_cache_store: cache clear retry failed for expr=%p", expr);
                $endif
                return;
            }
        }
        jit_cache_commit_slot(slot, expr, compiled, is_new_entry, owner_interp, owner_serial);
        return;
    }

    // A saturated local probe window would otherwise drop the entry silently and
    // force recompilation churn for this expression. Reset and retry once.
    $if DEBUG_BUILD:
        io::eprintfn("[debug] JIT cache probe window saturated for expr=%p, clearing", expr);
    $endif
    jit_cache_clear();
    if (!jit_cache_find_slot(expr, &slot, &is_new_entry) || !is_new_entry) {
        $if DEBUG_BUILD:
            io::eprintfn("[warn] jit_cache_store: cache clear retry failed for expr=%p after saturation", expr);
        $endif
        return;
    }
    jit_cache_commit_slot(slot, expr, compiled, true, owner_interp, owner_serial);
}

/**
 * Clear the JIT cache. Safe to call anytime — just causes recompilation on next access.
 * Does NOT free state memory (codes remain valid but unreachable from cache).
 */
fn void jit_cache_clear() {
    jit_require_owner_thread("jit_cache_clear");
    for (usz i = 0; i < JIT_CACHE_SIZE; i++) {
        g_jit_cache[i].expr = null;
        g_jit_cache[i].compiled = jit_compiled_fn_null();
        g_jit_cache[i].owner_interp = null;
        g_jit_cache[i].owner_attach_serial = 0;
    }
    g_jit_cache_count = 0;
}

fn void jit_global_init() {
    jit_require_owner_thread("jit_global_init");
    if (!g_jit_initialized) {
        init_jit(null);
        g_jit_initialized = true;
    }
}

fn void jit_runtime_attach_interp(Interp* interp) {
    jit_require_owner_thread("jit_runtime_attach_interp");
    jit_register_attached_interp(interp);
    g_jit_interp_refcount++;
}

fn void jit_runtime_detach_interp(Interp* interp) {
    jit_require_owner_thread("jit_runtime_detach_interp");
    ulong serial = 0;
    bool has_serial = jit_attached_interp_serial(interp, &serial);
    if (jit_interp_on_current_stack(interp)) {
        assert(!jit_interp_has_active_exec(interp), "jit_runtime_detach_interp: interp has active jit frames on current stack");
    }
    assert(g_jit_interp_refcount > 0, "jit interp refcount underflow");
    g_jit_interp_refcount--;
    jit_unregister_attached_interp(interp);
    if (has_serial) {
        // GC inserts retired tombstones; do not prune this serial eagerly before
        // pending state teardown runs.
        g_jit_gc_needed = true;
    }
    if (g_jit_interp_refcount == 0 && !g_jit_initialized) {
        g_jit_owner_thread_token = null;
        jit_retired_code_reset();
    }
}

fn void jit_global_shutdown() {
    jit_require_owner_thread("jit_global_shutdown");
    if (g_jit_initialized) {
        // Keep JIT runtime alive while any interpreter is still live.
        if (g_jit_interp_refcount > 0) return;
        if (g_jit_exec_depth > 0 || g_jit_suspended_guard_count > 0) {
            g_jit_gc_needed = true;
            return;
        }

        // Destroy all tracked JIT states
        for (usz i = 0; i < g_jit_state_count; i++) {
            _jit_destroy_state(g_jit_states[i].state);
            g_jit_states[i].state = null;
            g_jit_states[i].code = null;
            g_jit_states[i].owner_interp = null;
            g_jit_states[i].owner_attach_serial = 0;
            g_jit_states[i].compile_nonce = 0;
        }
        while (g_jit_spill_states != null) {
            JitStateSpillNode* node = g_jit_spill_states;
            g_jit_spill_states = node.next;
            _jit_destroy_state(node.state);
            mem::free(node);
        }
        g_jit_state_count = 0;
        g_jit_spill_count = 0;
        jit_cache_clear();
        g_jit_gc_needed = false;
        g_jit_pool_warned = false;
        jit_retired_code_reset();
        for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
            g_jit_attached_interps[i].interp = null;
            g_jit_attached_interps[i].attach_serial = 0;
            g_jit_attached_interps[i].active_exec_depth = 0;
        }
        finish_jit();
        g_jit_initialized = false;
        g_jit_owner_thread_token = null;
    }
    if (!g_jit_initialized && g_jit_interp_refcount == 0) {
        g_jit_owner_thread_token = null;
        jit_retired_code_reset();
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
    jit_require_owner_thread("jit_gc");
    if (!g_jit_gc_needed) return;
    if (g_jit_exec_depth > 0 || g_jit_suspended_guard_count > 0) return;

    // Full teardown between top-level evals: no JIT frames should be active.
    for (usz i = 0; i < g_jit_state_count; i++) {
        (void)jit_retired_code_insert(g_jit_states[i].code, g_jit_states[i].owner_attach_serial);
        _jit_destroy_state(g_jit_states[i].state);
        g_jit_states[i].state = null;
        g_jit_states[i].code = null;
        g_jit_states[i].owner_interp = null;
        g_jit_states[i].owner_attach_serial = 0;
        g_jit_states[i].compile_nonce = 0;
    }
    while (g_jit_spill_states != null) {
        JitStateSpillNode* node = g_jit_spill_states;
        g_jit_spill_states = node.next;
        (void)jit_retired_code_insert(node.code, node.owner_attach_serial);
        _jit_destroy_state(node.state);
        mem::free(node);
    }
    g_jit_state_count = 0;
    g_jit_spill_count = 0;
    jit_cache_clear();
    jit_retired_code_prune_detached_serials();
    g_jit_gc_needed = false;
    g_jit_pool_warned = false;
}

fn void jit_exec_enter(Interp* interp) @inline {
    jit_require_owner_thread("jit_exec_enter");
    isz idx = jit_attached_interp_index(interp);
    assert(idx >= 0, "jit_exec_enter: interp is not attached");
    g_jit_attached_interps[(usz)idx].active_exec_depth++;
    g_jit_exec_depth++;
}

fn void jit_exec_leave(Interp* interp) @inline {
    jit_require_owner_thread("jit_exec_leave");
    assert(g_jit_exec_depth > 0, "jit exec depth underflow");
    isz idx = jit_attached_interp_index(interp);
    assert(idx >= 0, "jit_exec_leave: interp is not attached");
    assert(g_jit_attached_interps[(usz)idx].active_exec_depth > 0, "jit_exec_leave: interp active exec depth underflow");
    g_jit_attached_interps[(usz)idx].active_exec_depth--;
    g_jit_exec_depth--;
}

fn void jit_suspend_guard_register() {
    jit_require_owner_thread("jit_suspend_guard_register");
    g_jit_suspended_guard_count++;
}

fn void jit_suspend_guard_unregister(char[] op) {
    jit_require_owner_thread(op);
    assert(g_jit_suspended_guard_count > 0, "jit suspended guard count underflow");
    g_jit_suspended_guard_count--;
}

fn bool jit_compiled_fn_is_live_for_interp(JitCompiledFn compiled, Interp* interp) {
    jit_require_owner_thread("jit_compiled_fn_is_live_for_interp");
    ulong interp_serial = 0;
    if (jit_compiled_fn_is_null(compiled) ||
        interp == null ||
        !jit_attached_interp_serial(interp, &interp_serial)) {
        return false;
    }
    for (usz i = 0; i < g_jit_state_count; i++) {
        if (g_jit_states[i].code == (void*)compiled.code_fn &&
            g_jit_states[i].owner_interp == interp &&
            g_jit_states[i].owner_attach_serial == interp_serial &&
            g_jit_states[i].compile_nonce == compiled.compile_nonce &&
            jit_interp_is_attached(g_jit_states[i].owner_interp)) {
            return true;
        }
    }
    JitStateSpillNode* node = g_jit_spill_states;
    while (node != null) {
        if (node.code == (void*)compiled.code_fn &&
            node.owner_interp == interp &&
            node.owner_attach_serial == interp_serial &&
            node.compile_nonce == compiled.compile_nonce &&
            jit_interp_is_attached(node.owner_interp)) {
            return true;
        }
        node = node.next;
    }
    return false;
}

// Legacy pointer-only liveness check retained for compatibility paths.
fn bool jit_compiled_code_is_live_for_interp(void* code_ptr, Interp* interp) {
    jit_require_owner_thread("jit_compiled_code_is_live_for_interp");
    ulong interp_serial = 0;
    if (code_ptr == null || interp == null || !jit_attached_interp_serial(interp, &interp_serial)) return false;
    for (usz i = 0; i < g_jit_state_count; i++) {
        if (g_jit_states[i].code == code_ptr &&
            g_jit_states[i].owner_interp == interp &&
            g_jit_states[i].owner_attach_serial == interp_serial &&
            jit_interp_is_attached(g_jit_states[i].owner_interp)) {
            return true;
        }
    }
    JitStateSpillNode* node = g_jit_spill_states;
    while (node != null) {
        if (node.code == code_ptr &&
            node.owner_interp == interp &&
            node.owner_attach_serial == interp_serial &&
            jit_interp_is_attached(node.owner_interp)) {
            return true;
        }
        node = node.next;
    }
    return false;
}

fn bool jit_track_compiled_state(void* s, void* code_ptr, Interp* owner_interp, ulong* compile_nonce_out) {
    jit_require_owner_thread("jit_track_compiled_state");
    ulong owner_serial = 0;
    if (compile_nonce_out != null) *compile_nonce_out = 0;
    if (owner_interp == null || compile_nonce_out == null) return false;
    if (!jit_attached_interp_serial(owner_interp, &owner_serial)) return false;
    g_jit_compile_nonce_counter++;
    if (g_jit_compile_nonce_counter == 0) g_jit_compile_nonce_counter = 1;
    ulong compile_nonce = g_jit_compile_nonce_counter;
    if (g_jit_state_count < JIT_STATE_POOL_SIZE) {
        g_jit_states[g_jit_state_count].state = s;
        g_jit_states[g_jit_state_count].code = code_ptr;
        g_jit_states[g_jit_state_count].owner_interp = owner_interp;
        g_jit_states[g_jit_state_count].owner_attach_serial = owner_serial;
        g_jit_states[g_jit_state_count].compile_nonce = compile_nonce;
        g_jit_state_count++;
        *compile_nonce_out = compile_nonce;
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
        return true;
    }

    JitStateSpillNode* spill = (JitStateSpillNode*)mem::malloc(JitStateSpillNode.sizeof);
    if (spill == null) {
        if (!g_jit_pool_warned) {
            io::printfn("WARNING: JIT state pool full (%d states), spill allocation failed; JIT compile will fail for this expression", JIT_STATE_POOL_SIZE);
            g_jit_pool_warned = true;
        }
        return false;
    }

    spill.state = s;
    spill.code = code_ptr;
    spill.owner_interp = owner_interp;
    spill.owner_attach_serial = owner_serial;
    spill.compile_nonce = compile_nonce;
    spill.next = g_jit_spill_states;
    g_jit_spill_states = spill;
    g_jit_spill_count++;
    g_jit_gc_needed = true;
    *compile_nonce_out = compile_nonce;

    if (!g_jit_pool_warned) {
        io::printfn(
            "WARNING: JIT state pool full (%d states), tracking overflow in spill list (count=%d)",
            JIT_STATE_POOL_SIZE,
            g_jit_spill_count
        );
        g_jit_pool_warned = true;
    }
    return true;
}

/**
 * Compile an expression to a native function.
 *
 * Returns a null handle if compilation fails.
 * The compiled function signature is: Value* fn(Interp*)
 */
fn JitCompiledFn jit_compile(Expr* expr, Interp* interp) {
    jit_require_owner_thread("jit_compile");
    jit_global_init();
    if (interp == null) return jit_compiled_fn_null();

    void* s = jit_new_state();
    if (s == null) return jit_compiled_fn_null();

    // Function prolog: Value* fn(Interp* interp)
    _jit_prolog(s);
    void* arg = _jit_arg(s, CODE_ARG_L);
    _jit_getarg_l(s, JIT_V0, arg);  // V0 = interp (callee-saved)

    // Compile the expression, result goes to R0.
    JitLocals locals = {};
    if (catch err = jit_compile_expr(s, expr, interp, &locals, true)) {
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    // Return R0
    _jit_retr(s, JIT_R0, CODE_RETR_L);

    // Emit machine code
    void* code = _jit_emit(s);

    // C10: If emit failed, destroy the state immediately and bail out
    if (code == null) {
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    _jit_clear_state(s);

    // Track state for cleanup (code buffer lives inside it).
    // C9: Cannot destroy_state here — code buffer lives inside the state.
    // States are destroyed by jit_gc() between top-level evaluations.
    ulong compile_nonce = 0;
    if (!jit_track_compiled_state(s, code, interp, &compile_nonce)) {
        // Tracking failed (spill OOM edge). Destroy state so code buffer is not leaked.
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    return { .code_fn = (JitFn)code, .compile_nonce = compile_nonce };
}
