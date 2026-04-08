module lisp;

import std::io;
import main;

// Single-scope wrapper: evaluates closure body in ONE child scope.
// Used for non-loop function calls. No TCO recycle scope is set.
// Avoids the overhead of two scopes (result_scope + call_scope).
fn Value* jit_eval_in_single_scope(Expr* body, Env* call_env, Interp* interp) {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* call_scope = boundary_push_child_scope(interp, &saved_scope);
    if (call_scope == null) {
        return raise_error(interp, "jit: failed to allocate call scope");
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
            return raise_error(interp, "jit: failed to register call-scope cleanup");
        }
    }

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        main::ScopeRegion* trace_scope = interp.current_scope;
        main::ScopeRegion* trace_recycle = interp.tco_recycle_scope;
        main::ScopeRegion* trace_saved = saved_scope;
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
            trace_scope != null ? (int)trace_scope.generation : -1,
            trace_recycle != null ? (int)trace_recycle.generation : -1,
            trace_saved != null ? (int)trace_saved.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1,
            result != null ? (int)result.tag : -1
        );
    }

    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        interp.current_scope = saved_scope;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-pop-defer active_gen=%d saved_gen=%d call_gen=%d",
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1
        );
    }

    boundary_leave_scope(interp, saved_scope);
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-leave current_gen=%d active_gen=%d saved_gen=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1
        );
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-finalize current_gen=%d final_tag=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            final != null ? (int)final.tag : -1
        );
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
        return raise_error(interp, "jit: failed to allocate call scope");
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
            return raise_error(interp, "jit: failed to register call-scope cleanup");
        }
    }

    // Mark call_scope as eligible for TCO recycling.
    jit_activate_call_scope_recycle(interp, call_scope, defer_slot, registered_defer);

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        if (result != null && result.tag == ERROR) {
            io::printfn(
                "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d error=%s",
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                interp.tco_recycle_scope != null ? (int)interp.tco_recycle_scope.generation : -1,
                saved_scope != null ? (int)saved_scope.generation : -1,
                call_scope != null ? (int)call_scope.generation : -1,
                (int)result.tag,
                (ZString)result.str_chars
            );
        } else {
            io::printfn(
                "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                interp.tco_recycle_scope != null ? (int)interp.tco_recycle_scope.generation : -1,
                saved_scope != null ? (int)saved_scope.generation : -1,
                call_scope != null ? (int)call_scope.generation : -1,
                result != null ? (int)result.tag : -1
            );
        }
    }

    // After jit_eval, current_scope may differ from call_scope (TCO recycling)
    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        jit_restore_call_scope_state(interp, saved_state);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-pop-defer active_gen=%d saved_gen=%d call_gen=%d",
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1
        );
    }

    // Restore saved state
    jit_restore_call_scope_state(interp, saved_state);
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-restore current_gen=%d active_gen=%d saved_gen=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1
        );
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-finalize current_gen=%d final_tag=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            final != null ? (int)final.tag : -1
        );
    }
    boundary_assert_interp_scope_chain(interp);
    return final;
}
