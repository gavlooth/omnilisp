module lisp;


import main;

fn Value* prim_resume_error_and_cleanup(
    Value* coroutine_val,
    main::StackCtx* ctx,
    Interp* interp,
    char[] msg
) {
    coroutine_clear_yield_boundary_state(interp);
    interp_clear_resume_value(interp);
    coroutine_ctx_cleanup(coroutine_val, ctx, interp);
    return runtime_raise(interp, "runtime/evaluation-error", msg);
}

fn Value* prim_resume_complete(Value* coroutine_val, main::StackCtx* ctx, Interp* interp) {
    Value* result = (Value*)ctx.result;
    if (result == null) {
        return prim_resume_error_and_cleanup(coroutine_val, ctx, interp, "resume: missing completed result");
    }
    coroutine_clear_yield_boundary_state(interp);
    interp_clear_resume_value(interp);
    coroutine_ctx_cleanup(coroutine_val, ctx, interp);
    return result;
}

fn Value* prim_resume_yield_result(Interp* interp) {
    Value* yielded = interp.yield_value;
    main::ScopeRegion* yield_scope = interp.yield_scope;
    coroutine_clear_yield_boundary_state(interp);

    if (yielded == null) return runtime_raise(interp, "runtime/invalid-state", "resume: missing yielded value");
    if (yield_scope == null) {
        return runtime_raise(interp, "runtime/invalid-state", "resume: missing yield-scope provenance");
    }
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = session.current_scope;
    interp.releasing_scope = yield_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    BoundaryCopyResult copied = boundary_copy_to_parent_site_ctx_checked(
        yielded,
        interp,
        COPY_SITE_PRIM_RESUME,
        &copy_ctx
    );
    if (!boundary_copy_result_ok(copied)) {
        return runtime_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(copied.fault_code));
    }
    return copied.value;
}

fn Value* prim_resume_require_ctx(
    Value*[] args,
    Value** coroutine_out,
    main::StackCtx** ctx_out,
    Interp* interp
) {
    if (args.len < 1) return runtime_raise(interp, "type/arity", "resume: expected a coroutine argument");
    Value* coroutine_val = args[0];
    if (coroutine_val == null || coroutine_val.tag != COROUTINE) {
        return runtime_raise(interp, "type/arg-mismatch", "resume: argument must be a coroutine");
    }

    main::StackCtx* ctx = coroutine_val.coroutine_val;
    if (ctx == null) return runtime_raise(interp, "runtime/evaluation-error", "resume: null coroutine");
    if (!coroutine_ctx_matches_interp(ctx, interp)) {
        return runtime_raise(interp, "runtime/evaluation-error", "resume: coroutine belongs to a different interpreter");
    }

    *coroutine_out = coroutine_val;
    *ctx_out = ctx;
    return null;
}

fn void prim_resume_switch_context(main::StackCtx* ctx, Interp* interp) {
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    if (ctx.status == main::StackCtxStatus.CTX_READY) {
        main::stack_ctx_switch_to(ctx, &parent_ctx);
    } else {
        main::stack_ctx_resume(ctx, &parent_ctx);
    }

    restore_interp_state_preserve_raise(interp, &saved);
}

fn Value* prim_resume_post_switch(Value* coroutine_val, main::StackCtx* ctx, Interp* interp) {
    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return prim_resume_error_and_cleanup(coroutine_val, ctx, interp, "stack overflow in coroutine");
    }
    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        return prim_resume_complete(coroutine_val, ctx, interp);
    }
    return prim_resume_yield_result(interp);
}

fn Value* prim_resume(Value*[] args, Env* env, Interp* interp) {
    Value* coroutine_val = null;
    main::StackCtx* ctx = null;
    Value* parse_err = prim_resume_require_ctx(args, &coroutine_val, &ctx, interp);
    if (parse_err != null) return parse_err;

    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED || ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return prim_resume_error_and_cleanup(coroutine_val, ctx, interp, "resume: coroutine already completed");
    }
    if (ctx.status != main::StackCtxStatus.CTX_READY && ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
        return runtime_raise(interp, "runtime/evaluation-error", "resume: coroutine is not resumable");
    }

    Value* resume_val = args.len > 1 ? args[1] : make_nil(interp);
    interp.resume_value = resume_val;

    prim_resume_switch_context(ctx, interp);
    return prim_resume_post_switch(coroutine_val, ctx, interp);
}

fn Value* prim_yield_require_context(main::StackCtx** current_out, Interp* interp) {
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) {
        return runtime_raise(interp, "runtime/evaluation-error", "yield: not inside a coroutine");
    }
    *current_out = current;
    return null;
}

fn bool prim_yield_suspend_and_restore(main::StackCtx* current, Interp* interp) {
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        restore_interp_state_preserve_raise(interp, &saved);
        return false;
    }

    // Resumed — restore full interpreter state.
    restore_interp_state_preserve_raise(interp, &saved);
    return true;
}

fn Value* prim_yield_resume_result(Interp* interp) {
    return interp_consume_resume_value_or_nil(interp);
}

fn Value* prim_yield(Value*[] args, Env* env, Interp* interp) {
    main::StackCtx* current = null;
    Value* parse_err = prim_yield_require_context(&current, interp);
    if (parse_err != null) return parse_err;

    Value* yield_val = args.len > 0 ? args[0] : make_nil(interp);
    interp.yield_value = yield_val;
    interp.yield_scope = interp.current_scope;

    if (!prim_yield_suspend_and_restore(current, interp)) {
        coroutine_clear_yield_boundary_state(interp);
        return runtime_raise(interp, "runtime/invalid-state", "stack engine: failed to register suspend scope guard");
    }
    return prim_yield_resume_result(interp);
}
