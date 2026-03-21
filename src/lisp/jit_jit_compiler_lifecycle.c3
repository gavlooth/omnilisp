module lisp;

import std::io;
import std::core::mem;
import main;

/**
 * Clear the JIT cache. Safe to call anytime - just causes recompilation on next access.
 * Does NOT free state memory (codes remain valid but unreachable from cache).
 */
fn void jit_cache_clear() {
    runtime_cache_clear();
}

fn void jit_global_init() {
    jit_require_owner_thread("jit_global_init");
    if (!runtime_backend_is_initialized()) {
        init_jit(null);
        runtime_backend_mark_initialized();
    }
}

fn void jit_runtime_attach_interp(Interp* interp) {
    runtime_attach_interp(interp);
}

fn void jit_runtime_detach_interp(Interp* interp) {
    runtime_detach_interp(interp);
}

fn void runtime_backend_destroy_tracked_state_slot(usz idx, bool retire_code) {
    if (retire_code) {
        (void)jit_retired_code_insert(
            runtime_backend_tracked_state_code_at(idx),
            runtime_backend_tracked_state_owner_attach_serial_at(idx)
        );
    }
    _jit_destroy_state(runtime_backend_tracked_state_state_at(idx));
    runtime_backend_clear_tracked_state_slot(idx);
}

fn void runtime_backend_destroy_spill_state_node(JitStateSpillNode* node, bool retire_code) {
    if (node == null) return;
    if (retire_code) {
        (void)jit_retired_code_insert(
            runtime_backend_spill_state_code(node),
            runtime_backend_spill_state_owner_attach_serial(node)
        );
    }
    _jit_destroy_state(runtime_backend_spill_state_state(node));
    mem::free(node);
}

fn void runtime_backend_destroy_all_tracked_compiled_state(bool retire_code) {
    for (usz i = 0; i < runtime_backend_tracked_state_count(); i++) {
        runtime_backend_destroy_tracked_state_slot(i, retire_code);
    }
    while (runtime_backend_spill_state_head() != null) {
        JitStateSpillNode* node = runtime_backend_spill_state_head();
        runtime_backend_set_spill_state_head(runtime_backend_spill_state_next(node));
        runtime_backend_destroy_spill_state_node(node, retire_code);
    }
    runtime_backend_set_tracked_state_count(0);
    runtime_backend_set_spill_state_count(0);
}

fn void runtime_backend_reset_attachment_table() {
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        runtime_backend_clear_attached_interp(i);
    }
}

fn void jit_global_shutdown() {
    jit_require_owner_thread("jit_global_shutdown");
    if (runtime_backend_is_initialized()) {
        // Keep JIT runtime alive while any interpreter is still live.
        if (runtime_backend_interp_refcount() > 0) return;
        if (runtime_backend_exec_depth() > 0 || runtime_backend_suspended_guard_count() > 0) {
            runtime_backend_mark_gc_needed();
            return;
        }

        runtime_backend_destroy_all_tracked_compiled_state(false);
        jit_cache_clear();
        runtime_backend_clear_gc_needed();
        runtime_backend_clear_pool_warned();
        jit_retired_code_reset();
        runtime_backend_reset_attachment_table();
        finish_jit();
        runtime_backend_clear_initialized();
        runtime_backend_clear_owner_thread_token();
    }
    if (!runtime_backend_is_initialized() && runtime_backend_interp_refcount() == 0) {
        runtime_backend_clear_owner_thread_token();
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
    if (!runtime_backend_gc_needed()) return;
    if (runtime_backend_exec_depth() > 0 || runtime_backend_suspended_guard_count() > 0) return;

    // Full teardown between top-level evals: no JIT frames should be active.
    runtime_backend_destroy_all_tracked_compiled_state(true);
    jit_cache_clear();
    jit_retired_code_prune_detached_serials();
    runtime_backend_clear_gc_needed();
    runtime_backend_clear_pool_warned();
}

fn void jit_exec_enter(Interp* interp) @inline {
    runtime_exec_enter(interp);
}

fn void jit_exec_leave(Interp* interp) @inline {
    runtime_exec_leave(interp);
}

fn void jit_suspend_guard_register() {
    jit_require_owner_thread("jit_suspend_guard_register");
    runtime_backend_increment_suspended_guard_count();
}

fn void jit_suspend_guard_unregister(char[] op) {
    jit_require_owner_thread(op);
    assert(runtime_backend_suspended_guard_count() > 0, "jit suspended guard count underflow");
    runtime_backend_decrement_suspended_guard_count();
}

fn bool runtime_backend_tracked_state_matches_compiled_expr(
    usz idx,
    RuntimeCompiledExpr compiled,
    Interp* interp,
    ulong interp_serial
) @inline {
    return runtime_backend_tracked_state_code_at(idx) == compiled.code_fn &&
        runtime_backend_tracked_state_owner_interp_at(idx) == interp &&
        runtime_backend_tracked_state_owner_attach_serial_at(idx) == interp_serial &&
        runtime_backend_tracked_state_compile_nonce_at(idx) == compiled.compile_nonce &&
        jit_interp_is_attached(runtime_backend_tracked_state_owner_interp_at(idx));
}

fn bool runtime_backend_spill_state_matches_compiled_expr(
    JitStateSpillNode* node,
    RuntimeCompiledExpr compiled,
    Interp* interp,
    ulong interp_serial
) @inline {
    return runtime_backend_spill_state_code(node) == compiled.code_fn &&
        runtime_backend_spill_state_owner_interp(node) == interp &&
        runtime_backend_spill_state_owner_attach_serial(node) == interp_serial &&
        runtime_backend_spill_state_compile_nonce(node) == compiled.compile_nonce &&
        jit_interp_is_attached(runtime_backend_spill_state_owner_interp(node));
}

fn bool runtime_backend_tracked_state_matches_code_ptr(
    usz idx,
    void* code_ptr,
    Interp* interp,
    ulong interp_serial
) @inline {
    return runtime_backend_tracked_state_code_at(idx) == code_ptr &&
        runtime_backend_tracked_state_owner_interp_at(idx) == interp &&
        runtime_backend_tracked_state_owner_attach_serial_at(idx) == interp_serial &&
        jit_interp_is_attached(runtime_backend_tracked_state_owner_interp_at(idx));
}

fn bool runtime_backend_spill_state_matches_code_ptr(
    JitStateSpillNode* node,
    void* code_ptr,
    Interp* interp,
    ulong interp_serial
) @inline {
    return runtime_backend_spill_state_code(node) == code_ptr &&
        runtime_backend_spill_state_owner_interp(node) == interp &&
        runtime_backend_spill_state_owner_attach_serial(node) == interp_serial &&
        jit_interp_is_attached(runtime_backend_spill_state_owner_interp(node));
}

fn bool runtime_backend_any_tracked_state_matches_compiled_expr(
    RuntimeCompiledExpr compiled,
    Interp* interp,
    ulong interp_serial
) {
    for (usz i = 0; i < runtime_backend_tracked_state_count(); i++) {
        if (runtime_backend_tracked_state_matches_compiled_expr(i, compiled, interp, interp_serial)) {
            return true;
        }
    }
    JitStateSpillNode* node = runtime_backend_spill_state_head();
    while (node != null) {
        if (runtime_backend_spill_state_matches_compiled_expr(node, compiled, interp, interp_serial)) {
            return true;
        }
        node = runtime_backend_spill_state_next(node);
    }
    return false;
}

fn bool runtime_backend_any_tracked_state_matches_code_ptr(
    void* code_ptr,
    Interp* interp,
    ulong interp_serial
) {
    for (usz i = 0; i < runtime_backend_tracked_state_count(); i++) {
        if (runtime_backend_tracked_state_matches_code_ptr(i, code_ptr, interp, interp_serial)) {
            return true;
        }
    }
    JitStateSpillNode* node = runtime_backend_spill_state_head();
    while (node != null) {
        if (runtime_backend_spill_state_matches_code_ptr(node, code_ptr, interp, interp_serial)) {
            return true;
        }
        node = runtime_backend_spill_state_next(node);
    }
    return false;
}

fn bool runtime_backend_compiled_expr_is_live_for_interp(RuntimeCompiledExpr compiled, Interp* interp) {
    jit_require_owner_thread("runtime_backend_compiled_expr_is_live_for_interp");
    ulong interp_serial = 0;
    if (runtime_compiled_expr_is_null(compiled) ||
        interp == null ||
        !runtime_attached_interp_serial(interp, &interp_serial)) {
        return false;
    }
    return runtime_backend_any_tracked_state_matches_compiled_expr(compiled, interp, interp_serial);
}

fn bool jit_compiled_fn_is_live_for_interp(JitCompiledFn compiled, Interp* interp) {
    return runtime_backend_compiled_expr_is_live_for_interp(runtime_backend_wrap_compiled_expr(compiled), interp);
}

// Legacy pointer-only liveness check retained for compatibility paths.
fn bool jit_compiled_code_is_live_for_interp(void* code_ptr, Interp* interp) {
    jit_require_owner_thread("jit_compiled_code_is_live_for_interp");
    ulong interp_serial = 0;
    if (code_ptr == null || interp == null || !jit_attached_interp_serial(interp, &interp_serial)) return false;
    return runtime_backend_any_tracked_state_matches_code_ptr(code_ptr, interp, interp_serial);
}
