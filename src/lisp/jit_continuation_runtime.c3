module lisp;

import main;
import std::io;

fn bool jit_cont_trace_enabled() @inline {
    ZString raw = main::getenv("OMNI_CONT_TRACE");
    return raw != null && raw[0] != 0;
}

fn bool jit_continuation_validate_ctx(main::StackCtx* ctx, Interp* interp, EvalResult* out_err) {
    if (ctx == null) {
        *out_err = eval_error("cannot resume continuation: null stack context");
        return false;
    }
    if (ctx.owner_pool != &interp.stack_ctx_pool) {
        *out_err = eval_error("cannot resume continuation from a different interpreter");
        return false;
    }
    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED || ctx.status == main::StackCtxStatus.CTX_DEAD) {
        *out_err = eval_error("cannot resume completed/dead continuation");
        return false;
    }
    if (ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
        *out_err = eval_error("cannot resume running continuation");
        return false;
    }
    return true;
}

fn void jit_continuation_init_resume_state(ResetState* resume_state, Interp* interp) {
    resume_state.shifted = false;
    resume_state.shift_body = null;
    resume_state.shift_env = null;
    resume_state.body = null;
    resume_state.env = null;
    resume_state.interp = interp;
}

fn void jit_continuation_destroy_ctx(main::StackCtx* ctx, Interp* interp) @inline {
    if (ctx == null) return;
    main::StackPool* owner_pool = ctx.owner_pool;
    if (owner_pool == null) owner_pool = &interp.stack_ctx_pool;
    main::stack_ctx_destroy(ctx, owner_pool);
}

fn bool jit_continuation_needs_fiber_yield() @inline {
    if (!g_scheduler.running) return false;
    usz fid = g_scheduler.current_fiber_id;
    if (fid >= g_scheduler.fiber_count) return false;
    return g_scheduler.fibers[fid].state == FIBER_BLOCKED;
}

fn bool jit_continuation_drive_blocked_fiber(
    main::StackCtx* target,
    Interp* interp,
    main::StackContext* parent_ctx
) {
    while (target != null &&
           parent_ctx != null &&
           target.status == main::StackCtxStatus.CTX_SUSPENDED &&
           jit_continuation_needs_fiber_yield()) {
        main::StackCtx* outer_ctx = main::g_current_stack_ctx;
        if (outer_ctx == null) return false;
        if (!prim_yield_suspend_and_restore(outer_ctx, interp)) return false;

        SavedInterpState saved;
        save_interp_state(interp, &saved);
        main::stack_ctx_resume(target, parent_ctx);
        restore_interp_state_preserve_raise(interp, &saved);
    }
    return true;
}

fn EvalResult jit_continuation_finish(main::StackCtx* target, ResetState* resume_state, Interp* interp) {
    if (resume_state.shifted) {
        // The resumed body hit another shift — evaluate that shift's body.
        // Keep `target` alive here: the newly captured continuation points at
        // this suspended context as its multi-shot template.
        Value* shift_result = jit_eval(resume_state.shift_body, resume_state.shift_env, interp);
        if (shift_result != null && shift_result.tag == ERROR) {
            return eval_error(shift_result.str_chars[:shift_result.str_len]);
        }
        return eval_ok(shift_result);
    }

    if (target.status == main::StackCtxStatus.CTX_SUSPENDED) {
        // Multi-shot continuation replay may suspend again on a later capture.
        // Leave the resumed clone alive so the newly captured continuation can
        // keep pointing at it and let the enclosing checkpoint logic continue.
        interp_clear_resume_value(interp);
        return eval_ok(make_nil(interp));
    }

    Value* result = (Value*)target.result;
    jit_continuation_destroy_ctx(target, interp);
    interp_clear_resume_value(interp);
    if (result != null && result.tag == ERROR) {
        return eval_error(result.str_chars[:result.str_len]);
    }
    return eval_ok(result);
}

/**
 * Apply a continuation: clone the context and resume it.
 *
 * Always clones for multi-shot safety — the original stays suspended
 * as a "template" for potential future invocations.
 */
fn EvalResult jit_apply_continuation_impl(Value* k_val, Value* arg, Interp* interp) {
    Continuation* k = k_val.cont_val;
    if (k == null) return eval_error("cannot resume continuation: null continuation");
    if (k.owner_interp != null && k.owner_interp != interp) {
        return eval_error("cannot resume continuation from a different interpreter");
    }
    main::StackCtx* ctx = k.ctx;
    EvalResult err;
    if (!jit_continuation_validate_ctx(ctx, interp, &err)) return err;
    if (jit_cont_trace_enabled()) {
        main::ScopeRegion* scope = interp.current_scope;
        io::printfn(
            "cont-apply: template=%p id=%d status=%d arg_tag=%d scope=%p scope_gen=%d",
            (void*)ctx,
            ctx != null ? (int)ctx.id : -1,
            ctx != null ? (int)ctx.status : -1,
            arg != null ? (int)arg.tag : -1,
            (void*)scope,
            scope != null ? (int)scope.generation : -1
        );
    }
    if (k.handle_state != null) {
        // `resolve` stays single-shot, but explicit `(k ...)` applications from
        // `with-continuation` are the multi-shot escape hatch.
        Value* resolved = jit_apply_handle_continuation_value(interp, k_val, arg);
        if (resolved != null && resolved.tag == ERROR) {
            return eval_error(resolved.str_chars[:resolved.str_len]);
        }
        return eval_ok(resolved);
    }

    bool asan_runtime = main::stack_runtime_asan_enabled();
    main::StackCtx* target = null;
    if (asan_runtime) {
        // ASAN fiber instrumentation does not support cloned suspended stacks.
        // Fall back to one-shot continuation resume in sanitizer builds.
        target = ctx;
        k.ctx = null;
    } else {
        // Clone for multi-shot safety (original stays suspended as template)
        target = main::stack_ctx_clone(ctx, &interp.stack_ctx_pool);
        if (target == null) {
            return eval_error("stack engine: failed to clone context for continuation");
        }
    }
    if (jit_cont_trace_enabled()) {
        main::ScopeRegion* scope = interp.current_scope;
        io::printfn(
            "cont-apply: target=%p id=%d status=%d asan=%d scope=%p scope_gen=%d",
            (void*)target,
            target != null ? (int)target.id : -1,
            target != null ? (int)target.status : -1,
            asan_runtime ? 1 : 0,
            (void*)scope,
            scope != null ? (int)scope.generation : -1
        );
    }

    // Pass the resume value to the shift function
    interp.resume_value = arg;

    // Set up a reset-like context to catch any shifts in the resumed body
    ResetState resume_state;
    jit_continuation_init_resume_state(&resume_state, interp);

    void* saved_state = interp.current_reset_state;
    interp.current_reset_state = &resume_state;

    SavedInterpState saved;
    save_interp_state(interp, &saved);

    // Resume the clone
    main::StackContext parent_ctx;
    main::stack_ctx_resume(target, &parent_ctx);
    if (jit_cont_trace_enabled()) {
        main::ScopeRegion* scope = interp.current_scope;
        io::printfn(
            "cont-apply: after-resume target=%p id=%d status=%d tpl_status=%d scope=%p scope_gen=%d",
            (void*)target,
            target != null ? (int)target.id : -1,
            target != null ? (int)target.status : -1,
            ctx != null ? (int)ctx.status : -1,
            (void*)scope,
            scope != null ? (int)scope.generation : -1
        );
    }

    restore_interp_state_preserve_raise(interp, &saved);
    interp.current_reset_state = saved_state;

    if (!jit_continuation_drive_blocked_fiber(target, interp, &parent_ctx)) {
        jit_continuation_destroy_ctx(target, interp);
        interp_clear_resume_value(interp);
        return eval_error("continuation: async yield propagation failed");
    }
    if (jit_cont_trace_enabled()) {
        main::ScopeRegion* scope = interp.current_scope;
        io::printfn(
            "cont-apply: finish target=%p id=%d status=%d tpl_status=%d scope=%p scope_gen=%d",
            (void*)target,
            target != null ? (int)target.id : -1,
            target != null ? (int)target.status : -1,
            ctx != null ? (int)ctx.status : -1,
            (void*)scope,
            scope != null ? (int)scope.generation : -1
        );
    }

    // Stack overflow — clone hit guard page
    if (target.status == main::StackCtxStatus.CTX_DEAD) {
        jit_continuation_destroy_ctx(target, interp);
        interp_clear_resume_value(interp);
        return eval_error("stack overflow in continuation");
    }

    return jit_continuation_finish(target, &resume_state, interp);
}
