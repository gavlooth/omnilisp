module lisp;

import main;

// Single-scope wrapper: evaluates closure body in ONE child scope.
// Used for non-loop function calls. No TCO recycle scope is set.
// Avoids the overhead of two scopes (result_scope + call_scope).
fn Value* jit_eval_in_single_scope(Expr* body, Env* call_env, Interp* interp) {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* call_scope = boundary_push_child_scope(interp, &saved_scope);
    if (call_scope == null) {
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate call scope");
    }
    boundary_assert_interp_scope_chain(interp);

    main::StackCtx* current_ctx = main::g_current_stack_ctx;
    bool registered_defer = false;
    if (current_ctx != null) {
        main::StackCtxDeferOps ops;
        init_scope_guard_defer_ops(&ops);
        registered_defer = main::stack_ctx_defer(current_ctx, &ops, (void*)call_scope);
        if (!registered_defer) {
            boundary_pop_child_scope(interp, saved_scope, call_scope);
            return jit_raise(interp, "runtime/out-of-memory", "jit: failed to register call-scope cleanup");
        }
    }

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_single_after_eval(interp.current_scope, interp.tco_recycle_scope, saved_scope, call_scope, result);
    }

    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        interp.current_scope = saved_scope;
        boundary_assert_interp_scope_chain(interp);
        return jit_raise(interp, "runtime/invalid-state", "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_pop_defer("after-pop-defer", active_scope, saved_scope, call_scope);
    }

    boundary_leave_scope(interp, saved_scope);
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_leave(interp.current_scope, active_scope, saved_scope);
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_finalize("after-finalize", interp.current_scope, final);
    }
    boundary_assert_interp_scope_chain(interp);
    return final;
}

// Scope-per-call wrapper: evaluates closure body in a child scope.
// Unified lane model: one scope per call with TEMP+ESCAPE lanes.
fn Value* jit_eval_in_call_scope(Expr* body, Env* call_env, Interp* interp, bool enable_escape_env) {
    boundary_assert_interp_scope_chain(interp);
    JitCallScopeState saved_state = jit_save_call_scope_state(interp);
    main::ScopeRegion* saved_scope = null;

    // Unified one-scope lane model: TEMP recycles, ESCAPE splices.
    main::ScopeRegion* call_scope = boundary_push_child_scope(interp, &saved_scope);
    if (call_scope == null) {
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate call scope");
    }
    boundary_assert_interp_scope_chain(interp);

    if (enable_escape_env) {
        interp.escape_env_mode = true;
    }

    main::StackCtx* current_ctx = main::g_current_stack_ctx;
    bool registered_defer = false;
    usz defer_slot = 0;
    if (current_ctx != null) {
        main::StackCtxDeferOps ops;
        init_scope_guard_defer_ops(&ops);
        defer_slot = current_ctx.defer_count;
        registered_defer = main::stack_ctx_defer(current_ctx, &ops, (void*)call_scope);
        if (!registered_defer) {
            boundary_pop_child_scope(interp, saved_scope, call_scope);
            jit_restore_call_scope_state(interp, saved_state);
            return jit_raise(interp, "runtime/out-of-memory", "jit: failed to register call-scope cleanup");
        }
    }

    // Mark call_scope as eligible for TCO recycling.
    jit_activate_call_scope_recycle(interp, call_scope, defer_slot, registered_defer);

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_call_after_eval(interp.current_scope, interp.tco_recycle_scope, saved_scope, call_scope, result);
    }

    // After jit_eval, current_scope may differ from call_scope (TCO recycling)
    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        jit_restore_call_scope_state(interp, saved_state);
        return jit_raise(interp, "runtime/invalid-state", "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_pop_defer("call-after-pop-defer", active_scope, saved_scope, call_scope);
    }

    // Restore saved state
    jit_restore_call_scope_state(interp, saved_state);
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_restore(interp.current_scope, active_scope, saved_scope);
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        jit_trace_after_finalize("call-after-finalize", interp.current_scope, final);
    }
    boundary_assert_interp_scope_chain(interp);
    return final;
}
