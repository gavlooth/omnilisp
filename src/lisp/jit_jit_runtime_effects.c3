module lisp;

import main;

fn Value* jit_resolve_value(Interp* interp, Value* k_val, Value* val) {
    Continuation* k = null;
    Value* validation_err = jit_resolve_validate_input(interp, k_val, &k);
    if (validation_err != null) return validation_err;
    bool had_retained = k.handle_retained;

    // Reinstall effect handler copy from when the signal occurred
    HandleEffectState* hstate = jit_resolve_push_handler_copy(interp, k);
    if (k.handle_state != null && hstate == null) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/out-of-memory",
            "resolve: failed to grow handler stack"
        );
    }
    main::StackCtx* target_ctx = k.ctx;

    // Consume one-shot continuation before resuming, so re-entrant use from
    // resumed code fails closed as already-resumed.
    k.ctx = null;
    k.handle_state = null;
    k.handle_retained = false;

    SavedInterpState saved;
    save_interp_state(interp, &saved);

    // Pass resolve value to suspended context
    interp.resume_value = val;

    // Resume the context
    main::StackContext parent_ctx;
    main::stack_ctx_resume(target_ctx, &parent_ctx);
    if (!jit_continuation_drive_blocked_fiber(target_ctx, interp, &parent_ctx)) {
        restore_interp_state_preserve_raise(interp, &saved);
        jit_resolve_pop_handler_copy(interp, hstate);
        interp_clear_resume_value(interp);
        if (had_retained && hstate != null && hstate.continuation_refcount > 0) hstate.continuation_refcount--;
        jit_resolve_destroy_ctx(interp, target_ctx);
        if (hstate != null && !hstate.frame_active && hstate.continuation_refcount == 0) {
            handle_effect_state_release(hstate);
        }
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/fiber-yield-failed",
            "resolve: failed to yield blocked scheduler fiber"
        );
    }
    restore_interp_state_preserve_raise(interp, &saved);

    // Context returned (either completed, signaled again, or died)
    jit_resolve_pop_handler_copy(interp, hstate);
    if (hstate != null &&
        !hstate.frame_active &&
        target_ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
        Value* err = raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/resolve-resuspended",
            "resolve: continuation suspended again outside an active handle"
        );
        interp_clear_resume_value(interp);
        if (had_retained && hstate.continuation_refcount > 0) hstate.continuation_refcount--;
        jit_resolve_destroy_ctx(interp, target_ctx);
        if (hstate.continuation_refcount == 0) {
            handle_effect_state_release(hstate);
        }
        return err;
    }
    if (target_ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
        // A resumed handler body may legitimately suspend again on a later
        // signal while the enclosing handle frame is still active. Leave the
        // suspended context alive so the next clause can wrap it in a fresh
        // continuation and let the enclosing handle loop keep dispatching.
        interp_clear_resume_value(interp);
        if (had_retained && hstate != null && hstate.continuation_refcount > 0) hstate.continuation_refcount--;
        return make_nil(interp);
    }
    Value* result = jit_resolve_finish_result(interp, target_ctx, hstate);

    // Single-shot: invalidate continuation
    if (had_retained && hstate != null) {
        if (hstate.continuation_refcount > 0) hstate.continuation_refcount--;
    }
    if (hstate != null && !hstate.frame_active && hstate.continuation_refcount == 0) {
        handle_effect_state_release(hstate);
    }

    return result;
}

fn Value* jit_apply_handle_continuation_value(Interp* interp, Value* k_val, Value* val) {
    if (main::stack_runtime_asan_enabled()) {
        // ASAN stack instrumentation cannot safely clone suspended stacks. Fall
        // back to the single-shot resolve path in sanitizer builds.
        return jit_resolve_value(interp, k_val, val);
    }

    Continuation* k = null;
    Value* validation_err = jit_resolve_validate_input(interp, k_val, &k);
    if (validation_err != null) return validation_err;

    main::StackCtx* target_ctx = main::stack_ctx_clone(k.ctx, &interp.stack_ctx_pool);
    if (target_ctx == null) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/out-of-memory",
            "continuation: failed to clone handler continuation"
        );
    }

    HandleEffectState* hstate = jit_resolve_push_handler_copy(interp, k);
    if (k.handle_state != null && hstate == null) {
        jit_resolve_destroy_ctx(interp, target_ctx);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/out-of-memory",
            "continuation: failed to grow handler stack"
        );
    }

    SavedInterpState saved;
    save_interp_state(interp, &saved);
    interp.resume_value = val;

    main::StackContext parent_ctx;
    main::stack_ctx_resume(target_ctx, &parent_ctx);
    if (!jit_continuation_drive_blocked_fiber(target_ctx, interp, &parent_ctx)) {
        restore_interp_state_preserve_raise(interp, &saved);
        jit_resolve_pop_handler_copy(interp, hstate);
        interp_clear_resume_value(interp);
        jit_resolve_destroy_ctx(interp, target_ctx);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/fiber-yield-failed",
            "continuation: async yield propagation failed"
        );
    }
    restore_interp_state_preserve_raise(interp, &saved);
    jit_resolve_pop_handler_copy(interp, hstate);

    if (target_ctx.status == main::StackCtxStatus.CTX_DEAD) {
        interp_clear_resume_value(interp);
        jit_resolve_destroy_ctx(interp, target_ctx);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/stack-overflow",
            "stack overflow in continuation"
        );
    }

    if (target_ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
        // Multi-shot handler continuations may re-suspend on a later signal.
        // Keep the clone alive so the newly captured continuation remains valid
        // and let the enclosing handler loop dispatch the next signal.
        interp_clear_resume_value(interp);
        return make_nil(interp);
    }

    Value* result = (Value*)target_ctx.result;
    interp_clear_resume_value(interp);
    jit_resolve_destroy_ctx(interp, target_ctx);
    return result;
}
