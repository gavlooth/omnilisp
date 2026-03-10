module lisp;

import std::io;
import std::collections::list;
import main;

module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (effects/reset/handle/continuations)
// =============================================================================

/**
 * Shared state between jit_reset_impl (parent) and jit_shift_impl (body context).
 * Lives on the parent's stack frame. The body reads/writes via interp.current_reset_state.
 */
struct ResetState {
    Expr*   body;         // Reset body expression
    Env*    env;          // Environment at reset time
    Interp* interp;       // Interpreter (for jit_eval)
    bool    shifted;      // Set true by shift before suspending
    Expr*   shift_body;   // Shift's body expression (to be eval'd in parent)
    Env*    shift_env;    // Environment for shift body (includes k binding)
}

fn void reset_entry(void* arg) {
    ResetState* state = (ResetState*)arg;
    Value* v = jit_eval(state.body, state.env, state.interp);
    main::g_current_stack_ctx.result = v;
}

/**
 * Stack-context-based reset: runs body on a separate stack.
 *
 * If the body completes normally, returns its value.
 * If the body calls (shift k BODY), the context suspends and BODY is
 * evaluated in the parent context with k bound to the suspended context.
 * The shift body's return value becomes the reset's return value.
 */
fn EvalResult jit_reset_impl(Expr* expr, Env* env, Interp* interp) {
    // Precompile reset body on parent stack to avoid cache-miss compilation on switched stacks.
    jit_warm_expr_cache(expr.reset.body, interp);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) return eval_error("stack engine: failed to create context for reset");

    ResetState state;
    state.body = expr.reset.body;
    state.env = env;
    state.interp = interp;
    state.shifted = false;
    state.shift_body = null;
    state.shift_env = null;

    main::stack_ctx_init(ctx, &reset_entry, &state);

    // Save/restore the reset state pointer for nesting
    void* saved_state = interp.current_reset_state;
    interp.current_reset_state = &state;

    // Save/restore interp state across context switch
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);

    interp.current_reset_state = saved_state;
    restore_interp_state(interp, &saved);

    // Stack overflow — context hit guard page
    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return eval_error("stack overflow in reset body");
    }

    if (state.shifted) {
        // Shift occurred — evaluate shift body in parent context
        Value* shift_result = jit_eval(state.shift_body, state.shift_env, interp);
        if (shift_result != null && shift_result.tag == ERROR) {
            return eval_error(shift_result.str_chars[:shift_result.str_len]);
        }
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
 * Stack-context-based shift: suspends the current context and creates a continuation.
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
        return raise_error_with_payload_names(interp, "runtime", "runtime/shift-outside-reset", "shift outside of reset");
    }

    ResetState* rstate = (ResetState*)interp.current_reset_state;
    if (rstate == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/shift-outside-reset", "shift outside of reset (no reset state)");
    }

    // Create continuation wrapping the current context
    Continuation* k = interp.alloc_lisp_continuation();
    k.ctx = current;
    k.is_coroutine_based = true;

    Value* k_val = make_continuation(interp, k);

    // Bind k in the shift body's environment
    Env* shift_env = env.extend(interp, expr.shift.k_name, k_val);

    // Store shift body info for the parent (jit_reset_impl) to evaluate
    rstate.shifted = true;
    rstate.shift_body = expr.shift.body;
    rstate.shift_env = shift_env;

    // Save interpreter state that the parent's evaluation will corrupt.
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    // Suspend the context — control returns to jit_reset_impl
    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/suspend-scope-guard", "stack engine: failed to register suspend scope guard");
    }

    // === RESUMED HERE by (k val) via jit_apply_continuation_impl ===
    restore_interp_state(interp, &saved);

    return interp.resume_value;
}

fn bool jit_continuation_validate_ctx(main::StackCtx* ctx, EvalResult* out_err) {
    if (ctx == null) {
        *out_err = eval_error("cannot resume continuation: null stack context");
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

    Value* result = (Value*)target.result;
    main::stack_ctx_destroy(target, &interp.stack_ctx_pool);
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
    main::StackCtx* ctx = k_val.cont_val.ctx;
    EvalResult err;
    if (!jit_continuation_validate_ctx(ctx, &err)) return err;

    bool asan_runtime = main::stack_runtime_asan_enabled();
    main::StackCtx* target = null;
    if (asan_runtime) {
        // ASAN fiber instrumentation does not support cloned suspended stacks.
        // Fall back to one-shot continuation resume in sanitizer builds.
        target = ctx;
        k_val.cont_val.ctx = null;
    } else {
        // Clone for multi-shot safety (original stays suspended as template)
        target = main::stack_ctx_clone(ctx, &interp.stack_ctx_pool);
        if (target == null) {
            return eval_error("stack engine: failed to clone context for continuation");
        }
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

    interp.current_reset_state = saved_state;
    restore_interp_state(interp, &saved);

    // Stack overflow — clone hit guard page
    if (target.status == main::StackCtxStatus.CTX_DEAD) {
        main::stack_ctx_destroy(target, &interp.stack_ctx_pool);
        return eval_error("stack overflow in continuation");
    }

    return jit_continuation_finish(target, &resume_state, interp);
}
