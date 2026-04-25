module lisp;

import std::collections::list;
import main;
import std::io;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (effects/checkpoint/handle/continuations)
// =============================================================================

/**
 * Shared state between jit_reset_impl (parent) and jit_shift_impl (body context).
 * Lives on the parent's stack frame. The body reads/writes via interp.current_reset_state.
 */
struct ResetState {
    Expr*   body;         // Checkpoint body expression
    Env*    env;          // Environment at checkpoint time
    Interp* interp;       // Interpreter (for jit_eval)
    bool    shifted;      // Set true by shift before suspending
    Expr*   shift_body;   // Shift's body expression (to be eval'd in parent)
    Env*    shift_env;    // Environment for shift body (includes k binding)
    Continuation* shift_k; // Captured continuation backing the suspended ctx
}

fn void reset_entry(void* arg) {
    ResetState* state = (ResetState*)arg;
    Value* v = jit_eval(state.body, state.env, state.interp);
    main::g_current_stack_ctx.result = v;
}

/**
 * Stack-context-based checkpoint: runs body on a separate stack.
 *
 * If the body completes normally, returns its value.
 * If the body calls (capture k BODY), the context suspends and BODY is
 * evaluated in the parent context with k bound to the suspended context.
 * The capture body's return value becomes the checkpoint's return value.
 */
fn EvalResult jit_reset_impl(Expr* expr, Env* env, Interp* interp) {
    // Precompile checkpoint body on parent stack to avoid cache-miss compilation on switched stacks.
    jit_warm_expr_cache(expr.reset.body, interp);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) return eval_error("stack engine: failed to create context for checkpoint");

    ResetState state;
    state.body = expr.reset.body;
    state.env = env;
    state.interp = interp;
    state.shifted = false;
    state.shift_body = null;
    state.shift_env = null;
    state.shift_k = null;

    main::stack_ctx_init(ctx, &reset_entry, &state);

    // Save/restore the checkpoint state pointer for nesting
    void* saved_state = interp.current_reset_state;
    interp.current_reset_state = &state;

    // Save/restore interp state across context switch
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);

    restore_interp_state_preserve_raise(interp, &saved);
    interp.current_reset_state = saved_state;

    // Stack overflow — context hit guard page
    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return eval_error("stack overflow in checkpoint body");
    }

    if (state.shifted) {
        // Shift occurred — evaluate shift body in parent context
        Value* shift_result = jit_eval(state.shift_body, state.shift_env, interp);
        if (shift_result != null && shift_result.tag == ERROR) {
            interp_discard_lisp_continuation(interp, state.shift_k);
            main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
            return eval_error(shift_result.str_chars[:shift_result.str_len]);
        }
        if (continuation_result_references_target(shift_result, state.shift_k)) {
            return eval_ok(shift_result);
        }
        interp_discard_lisp_continuation(interp, state.shift_k);
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return eval_ok(shift_result);
    }

    // Normal completion (no shift)
    Value* v = (Value*)ctx.result;
    main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_chars[:v.str_len]);
    }
    return eval_ok(v);
}

/**
 * Stack-context-based capture: suspends the current context and creates a continuation.
 *
 * This function is called INSIDE a context (created by jit_reset_impl).
 * It captures the current context as a continuation, stores the shift body info
 * in the parent's ResetState, and suspends. When resumed by (k val),
 * it returns the resume value.
 */
fn Value* jit_shift_impl(Expr* expr, Env* env, Interp* interp) {
    // IMPORTANT: Returns Value* (not EvalResult) so the return fits in RAX.
    // EvalResult is >16 bytes, triggering the System V ABI hidden return pointer.
    // After stack_ctx_clone that pointer is stale, causing writes to the wrong stack.
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/capture-outside-checkpoint", "capture outside of checkpoint");
    }

    ResetState* rstate = (ResetState*)interp.current_reset_state;
    if (rstate == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/capture-outside-checkpoint", "capture outside of checkpoint (no checkpoint state)");
    }

    // Create continuation wrapping the current context
    Continuation* k = interp.alloc_lisp_continuation();
    if (k == null) {
        return runtime_effect_raise(
            interp,
            "runtime/capture-continuation-allocation-failed",
            "capture: failed to allocate continuation"
        );
    }
    k.ctx = current;
    k.is_coroutine_based = true;

    Value* k_val = make_continuation(interp, k);
    if (k_val == null || k_val.tag == ERROR) {
        interp_discard_lisp_continuation(interp, k);
        return k_val != null ? k_val : runtime_effect_raise(
            interp,
            "runtime/capture-continuation-allocation-failed",
            "capture: failed to allocate continuation wrapper"
        );
    }

    // Bind k in the shift body's environment
    Env* shift_env = jit_env_extend(interp, env, expr.shift.k_name, k_val);
    if (shift_env == null) {
        interp_discard_lisp_continuation(interp, k);
        return runtime_effect_raise(
            interp,
            "runtime/out-of-memory",
            "capture: out of memory while binding continuation"
        );
    }

    // Store shift body info for the parent (jit_reset_impl) to evaluate
    rstate.shifted = true;
    rstate.shift_body = expr.shift.body;
    rstate.shift_env = shift_env;
    rstate.shift_k = k;

    // Save interpreter state that the parent's evaluation will corrupt.
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    if (jit_cont_trace_enabled()) {
        main::ScopeRegion* scope = interp.current_scope;
        io::printfn(
            "cont-capture: ctx=%p id=%d status=%d resume_tag=%d scope=%p scope_gen=%d",
            (void*)current,
            (int)current.id,
            (int)current.status,
            interp.resume_value != null ? (int)interp.resume_value.tag : -1,
            (void*)scope,
            scope != null ? (int)scope.generation : -1
        );
    }

    // Suspend the context — control returns to jit_reset_impl
    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        rstate.shifted = false;
        rstate.shift_body = null;
        rstate.shift_env = null;
        rstate.shift_k = null;
        interp_discard_lisp_continuation(interp, k);
        restore_interp_state_preserve_raise(interp, &saved);
        return raise_error_with_payload_names(interp, "runtime", "runtime/suspend-scope-guard", "stack engine: failed to register suspend scope guard");
    }

    // === RESUMED HERE by (k val) via jit_apply_continuation_impl ===
    void* resumed_reset_state = interp.current_reset_state;
    restore_interp_state_preserve_raise(interp, &saved);
    interp.current_reset_state = resumed_reset_state;
    if (jit_cont_trace_enabled()) {
        main::StackCtx* resumed_ctx = main::g_current_stack_ctx;
        main::ScopeRegion* resumed_scope = interp.current_scope;
        io::printfn(
            "cont-capture: resumed ctx=%p id=%d status=%d resume_tag=%d scope=%p scope_gen=%d",
            (void*)resumed_ctx,
            resumed_ctx != null ? (int)resumed_ctx.id : -1,
            resumed_ctx != null ? (int)resumed_ctx.status : -1,
            interp.resume_value != null ? (int)interp.resume_value.tag : -1,
            (void*)resumed_scope,
            resumed_scope != null ? (int)resumed_scope.generation : -1
        );
    }

    return interp_consume_resume_value_or_nil(interp);
}
