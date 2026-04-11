module lisp;

import main;

fn Value* jit_resolve_reject_inactive_handle_continuation(
    Interp* interp,
    Continuation* k,
    HandleEffectState* hstate
) {
    if (hstate != null && k.handle_retained && hstate.continuation_refcount > 0) {
        hstate.continuation_refcount--;
    }
    if (k.ctx != null) {
        main::StackPool* owner_pool = k.ctx.owner_pool;
        if (owner_pool == null) owner_pool = &interp.stack_ctx_pool;
        main::stack_ctx_destroy(k.ctx, owner_pool);
    }
    k.ctx = null;
    k.handle_state = null;
    k.handle_retained = false;
    if (hstate != null && hstate.continuation_refcount == 0) {
        handle_effect_state_release(hstate);
    }
    return raise_error_with_payload_names(
        interp,
        "runtime",
        "runtime/continuation-frame-inactive",
        "resolve: continuation escaped an inactive handle frame"
    );
}

fn Value* jit_resolve_validate_input(Interp* interp, Value* k_val, Continuation** out_k) {
    if (k_val == null || k_val.tag != CONTINUATION) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation", "resolve: __k is not a valid continuation"
        );
    }

    Continuation* k = k_val.cont_val;
    if (k != null && k.owner_interp != null && k.owner_interp != interp) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/continuation-owner-mismatch",
            "resolve: continuation belongs to a different interpreter"
        );
    }

    if (k.ctx == null) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/continuation-resumed", "resolve: continuation already resumed (one-shot only)"
        );
    }

    if (!k.is_coroutine_based) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation-kind", "resolve: expected coroutine-based continuation"
        );
    }

    if (k.ctx.status == main::StackCtxStatus.CTX_COMPLETED || k.ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/dead-continuation", "resolve: continuation context is dead"
        );
    }
    if (k.ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation-state", "resolve: continuation context is not suspended"
        );
    }
    if (k.ctx.owner_pool != &interp.stack_ctx_pool) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/continuation-owner-mismatch",
            "resolve: continuation belongs to a different interpreter"
        );
    }
    HandleEffectState* hstate = (HandleEffectState*)k.handle_state;
    if (hstate != null && hstate.interp != interp) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/continuation-owner-mismatch",
            "resolve: continuation handle state belongs to a different interpreter"
        );
    }
    if (hstate != null && !hstate.frame_active) {
        return jit_resolve_reject_inactive_handle_continuation(interp, k, hstate);
    }

    *out_k = k;
    return null;
}

fn HandleEffectState* jit_resolve_push_handler_copy(Interp* interp, Continuation* k) {
    HandleEffectState* hstate = (HandleEffectState*)k.handle_state;
    if (hstate != null) {
        if (interp.handler_count >= interp.handler_capacity && !interp.grow_handler_stack()) return null;
        interp.handler_stack[interp.handler_count] = hstate.handler_copy;
        interp.handler_stack[interp.handler_count].effect_state = hstate;
        interp.handler_count++;
    }
    return hstate;
}

fn void jit_resolve_pop_handler_copy(Interp* interp, HandleEffectState* hstate) @inline {
    if (hstate != null) {
        interp.handler_count--;
    }
}

fn void jit_resolve_destroy_ctx(Interp* interp, main::StackCtx* ctx) @inline {
    if (ctx == null) return;
    main::StackPool* owner_pool = ctx.owner_pool;
    if (owner_pool == null) owner_pool = &interp.stack_ctx_pool;
    main::stack_ctx_destroy(ctx, owner_pool);
}

fn Value* jit_resolve_finish_result(Interp* interp, main::StackCtx* target_ctx, HandleEffectState* hstate) {
    if (target_ctx.status == main::StackCtxStatus.CTX_DEAD) {
        interp_clear_resume_value(interp);
        if (hstate == null || !hstate.frame_active) {
            jit_resolve_destroy_ctx(interp, target_ctx);
        }
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow in resolve");
    }
    if (target_ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        Value* result = (Value*)target_ctx.result;
        interp_clear_resume_value(interp);
        if (hstate == null || !hstate.frame_active) {
            jit_resolve_destroy_ctx(interp, target_ctx);
        }
        return result;
    }
    return interp_consume_resume_value_or_nil(interp);
}
