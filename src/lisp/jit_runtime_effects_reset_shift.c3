module lisp;

import main;

fn Expr* parse_for_jit(char[] source, Interp* interp) {
    return parse(source, interp);
}

// Parallel state for Value-based (AOT) effects
struct ResetValueState {
    Value*  body;         // Checkpoint body closure
    Interp* interp;
    bool    shifted;
    Value*  shift_body;   // Capture body closure
    Value*  shift_k;      // Captured k for capture body
}

fn void reset_value_entry(void* arg) {
    ResetValueState* state = (ResetValueState*)arg;
    Value* result = jit_apply_value(state.body, make_nil(state.interp), state.interp);
    main::g_current_stack_ctx.result = result;
}

fn Value* jit_reset_value(Interp* interp, Value* body) {
    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/stack-context-create", "stack engine: failed to create context"
        );
    }

    ResetValueState state;
    state.body = body;
    state.interp = interp;
    state.shifted = false;
    state.shift_body = null;
    state.shift_k = null;

    main::stack_ctx_init(ctx, &reset_value_entry, &state);

    void* saved_state = interp.current_reset_state;
    interp.current_reset_state = &state;

    SavedInterpState saved;
    save_interp_state(interp, &saved);
    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);
    JitBlockedFiberDriveStatus drive_status = jit_continuation_drive_blocked_fiber(ctx, interp, &parent_ctx);
    if (jit_blocked_fiber_drive_failed(drive_status)) {
        restore_interp_state_preserve_raise(interp, &saved);
        interp.current_reset_state = saved_state;
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return jit_blocked_fiber_drive_raise(
            interp,
            drive_status,
            "checkpoint: failed to yield blocked scheduler fiber",
            "checkpoint: async yield propagation limit exceeded"
        );
    }
    restore_interp_state_preserve_raise(interp, &saved);
    interp.current_reset_state = saved_state;

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow in checkpoint");
    }

    if (state.shifted) {
        Value* shift_result = jit_apply_value(state.shift_body, state.shift_k, interp);
        if (shift_result != null && shift_result.tag == ERROR) {
            if (state.shift_k != null && state.shift_k.tag == CONTINUATION) {
                interp_discard_lisp_continuation(interp, state.shift_k.cont_val);
            }
            main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
            return shift_result;
        }
        if (state.shift_k != null &&
            state.shift_k.tag == CONTINUATION &&
            continuation_result_references_target(shift_result, state.shift_k.cont_val)) {
            return shift_result;
        }
        if (state.shift_k != null && state.shift_k.tag == CONTINUATION) {
            interp_discard_lisp_continuation(interp, state.shift_k.cont_val);
        }
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return shift_result;
    }
    if (ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/checkpoint-resuspended",
            "checkpoint: body suspended before completion"
        );
    }

    Value* v = (Value*)ctx.result;
    main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
    return v;
}

fn Value* jit_shift_value(Interp* interp, Value* body) {
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/capture-outside-checkpoint", "capture outside of checkpoint");
    }

    ResetValueState* rstate = (ResetValueState*)interp.current_reset_state;
    if (rstate == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/capture-outside-checkpoint", "capture outside of checkpoint");
    }

    Continuation* k = interp.alloc_lisp_continuation();
    if (k == null) {
        return make_error(interp, "capture: failed to allocate continuation");
    }
    k.ctx = current;
    k.is_coroutine_based = true;
    Value* snapshot_error = null;
    if (!continuation_attach_aot_snapshot_if_active(k, &snapshot_error)) {
        interp_discard_lisp_continuation(interp, k);
        return snapshot_error != null ? snapshot_error : make_error(interp, "capture: failed to snapshot continuation state");
    }
    Value* k_val = make_continuation(interp, k);
    if (k_val == null || k_val.tag == ERROR) {
        interp_discard_lisp_continuation(interp, k);
        return k_val != null ? k_val : make_error(interp, "capture: failed to allocate continuation wrapper");
    }

    rstate.shifted = true;
    rstate.shift_body = body;
    rstate.shift_k = k_val;

    SavedInterpState saved;
    save_interp_state(interp, &saved);

    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        rstate.shifted = false;
        rstate.shift_body = null;
        rstate.shift_k = null;
        interp_discard_lisp_continuation(interp, k);
        restore_interp_state_preserve_raise(interp, &saved);
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/suspend-scope-guard", "stack engine: failed to register suspend scope guard"
        );
    }
    void* resumed_reset_state = interp.current_reset_state;
    restore_interp_state_preserve_raise(interp, &saved);
    interp.current_reset_state = resumed_reset_state;
    return interp_consume_resume_value_or_nil(interp);
}
