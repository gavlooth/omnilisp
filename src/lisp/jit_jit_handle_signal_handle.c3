module lisp;

import std::io;
import std::collections::list;
import main;

fn EvalResult value_to_eval_result(Value* v) @inline {
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_chars[:v.str_len]);
    }
    return eval_ok(v);
}

fn Value* dispatch_pending_raise(Expr* expr, EffectHandler* h, Env* env, Interp* interp) {
    if (!interp.flags.raise_pending) return null;
    SymbolId raise_sym = interp.sym_raise;
    for (usz ci = 0; ci < expr.handle.clause_count; ci++) {
        if ((h.tags != null && (uint)h.tags[ci] == (uint)raise_sym) ||
            (h.tags == null && (uint)expr.handle.clauses[ci].effect_tag == (uint)raise_sym)) {
            EffectClause* clause = &expr.handle.clauses[ci];
            Value* msg_val = interp.raise_payload;
            if (msg_val == null) {
                msg_val = make_string(interp, interp.raise_msg[:interp.raise_msg_len]);
            }
            if (msg_val == null) {
                return make_error(interp, "handle: failed to materialize raise payload");
            }
            if (msg_val.tag == ERROR) return msg_val;
            Value* k_val = make_nil(interp);
            Env* clause_env = jit_env_extend(interp, env, clause.k_name, k_val);
            if (clause_env == null) {
                return runtime_effect_raise(
                    interp,
                    "runtime/raise-handler-bind-failed",
                    "raise handler failed to bind clause environment"
                );
            }
            clause_env = jit_env_extend(interp, clause_env, clause.arg_name, msg_val);
            if (clause_env == null) {
                return runtime_effect_raise(
                    interp,
                    "runtime/raise-handler-bind-failed",
                    "raise handler failed to bind clause environment"
                );
            }
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.raise_msg[0] = 0;
            Value* handler_v = jit_eval(clause.handler_body, clause_env, interp);
            if (handler_v == null) {
                return runtime_effect_raise(interp, "runtime/raise-handler-returned-nil", "raise handler returned nil");
            }
            return handler_v;
        }
    }
    return null;
}

/**
 * Stack-context-based handle: runs body on a context, dispatches signals to handler clauses.
 *
 * Signal suspends the context. The handle loop finds the matching clause, creates
 * a continuation k, and evaluates the clause in the parent context. Resolve
 * resumes the body context (single-shot).
 */
fn void jit_handle_warm_clauses(Expr* expr, Interp* interp) {
    jit_warm_expr_cache(expr.handle.body, interp);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        jit_warm_expr_cache(expr.handle.clauses[i].handler_body, interp);
    }
}

fn void jit_handle_init_state(HandleEffectState* state, Expr* expr, Env* env, Interp* interp) {
    handle_effect_state_init(state, interp);
    state.body = expr.handle.body;
    state.env = env;
}

fn void jit_handle_init_handler(EffectHandler* h, Expr* expr, Env* env, HandleEffectState* state) {
    h.clause_count = expr.handle.clause_count;
    h.handler_env = env;
    h.body_expr = expr.handle.body;
    h.body_env = env;
    h.strict_mode = expr.handle.strict_mode;
    h.effect_state = state;
}

fn Value* jit_handle_copy_clauses(EffectHandler* h, Expr* expr, Env* env, Interp* interp) {
    if (expr.handle.clause_count > usz.max / SymbolId.sizeof ||
        expr.handle.clause_count > usz.max / EffectClause.sizeof) {
        return make_error(interp, "handle: failed to allocate clauses");
    }
    h.tags = (SymbolId*)mem::malloc(SymbolId.sizeof * expr.handle.clause_count);
    h.clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * expr.handle.clause_count);
    h.closures = null;
    if (h.tags == null || h.clauses == null) {
        if (h.tags != null) mem::free(h.tags);
        if (h.clauses != null) mem::free(h.clauses);
        return make_error(interp, "handle: failed to allocate clauses");
    }
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        h.clauses[i] = expr.handle.clauses[i];
        Value* tag_err = resolve_effect_tag_symbol(
            expr.handle.clauses[i].effect_tag,
            expr.handle.clauses[i].effect_tag_expr,
            expr.handle.clauses[i].has_effect_tag_expr,
            env,
            interp,
            &h.tags[i],
            "handle"
        );
        if (tag_err != null) {
            mem::free(h.tags);
            mem::free(h.clauses);
            h.tags = null;
            h.clauses = null;
            return tag_err;
        }
    }
    return null;
}

fn bool jit_handle_body_needs_fiber_yield() @inline {
    if (!g_scheduler.running) return false;
    usz fid = g_scheduler.current_fiber_id;
    if (fid >= g_scheduler.fiber_count) return false;
    return g_scheduler.fibers[fid].state == FIBER_BLOCKED;
}

fn EvalResult jit_handle_switch_to_body(main::StackCtx* ctx, Interp* interp) {
    SavedInterpState saved;
    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);
    restore_interp_state_preserve_raise(interp, &saved);

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return eval_error("stack overflow in handle body");
    }

    // Async I/O yield propagation: handle body suspended due to async I/O inside a fiber.
    // Yield the outer context (fiber or enclosing handle) until I/O completes.
    while (ctx.status == main::StackCtxStatus.CTX_SUSPENDED &&
           jit_handle_body_needs_fiber_yield()) {
        main::StackCtx* outer_ctx = main::g_current_stack_ctx;
        if (outer_ctx == null) break;
        if (!prim_yield_suspend_and_restore(outer_ctx, interp)) {
            return eval_error("handle: async yield propagation failed");
        }
        // Outer context resumed (I/O complete) — resume handle body.
        SavedInterpState saved2;
        save_interp_state(interp, &saved2);
        main::stack_ctx_resume(ctx, &parent_ctx);
        restore_interp_state_preserve_raise(interp, &saved2);
        if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
            return eval_error("stack overflow in handle body");
        }
    }

    return eval_ok(null);
}

fn bool jit_handle_dispatch_signals(
    HandleEffectState* state,
    Expr* expr,
    Env* env,
    Interp* interp,
    main::StackCtx* ctx,
    EvalResult* out_result
) {
    while (state.signaled) {
        Value* handler_v = jit_handle_dispatch_signal(state, expr, env, interp, ctx);

        // resolve was called, body completed
        if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
            *out_result = value_to_eval_result((Value*)ctx.result);
            return true;
        }

        // Body hit another signal after being resumed — loop to dispatch.
        if (state.signaled) {
            continue;
        }

        // Abort or unmatched clause: context still suspended, return handler result.
        *out_result = value_to_eval_result(handler_v);
        return true;
    }
    return false;
}

fn EvalResult jit_handle_finish_no_signal(Expr* expr, EffectHandler* h, Env* env, Interp* interp, main::StackCtx* ctx) {
    Value* raise_v = dispatch_pending_raise(expr, h, env, interp);
    if (raise_v != null) return value_to_eval_result(raise_v);
    return value_to_eval_result((Value*)ctx.result);
}

fn EvalResult jit_handle_impl(Expr* expr, Env* env, Interp* interp) {
    jit_handle_warm_clauses(expr, interp);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) return eval_error("stack engine: failed to create context for handle");
    bool destroy_ctx_on_exit = true;
    defer if (destroy_ctx_on_exit) main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);

    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    if (state == null) return eval_error("handle: failed to allocate state");
    jit_handle_init_state(state, expr, env, interp);
    defer {
        state.frame_active = false;
        if (state.continuation_refcount == 0) {
            handle_effect_state_release(state);
        } else if (ctx.status == main::StackCtxStatus.CTX_SUSPENDED) {
            // Continuation escaped; transfer ctx ownership to continuation/resolution path.
            destroy_ctx_on_exit = false;
        }
    }

    main::stack_ctx_init(ctx, &handle_effect_entry, state);

    if (interp.handler_count >= interp.handler_capacity && !interp.grow_handler_stack()) {
        return eval_error("handle: failed to grow handler stack");
    }
    usz my_idx = interp.handler_count;
    EffectHandler* h = &interp.handler_stack[my_idx];
    jit_handle_init_handler(h, expr, env, state);

    Value* clause_err = jit_handle_copy_clauses(h, expr, env, interp);
    if (clause_err != null) return value_to_eval_result(clause_err);
    SymbolId* owned_tags = h.tags;
    EffectClause* owned_clauses = h.clauses;
    defer mem::free(owned_tags);
    defer mem::free(owned_clauses);

    interp.handler_count = my_idx + 1;
    defer interp.handler_count = my_idx;

    state.handler_idx = my_idx;
    if (!handle_effect_state_capture_handler_copy(state, h)) {
        return eval_error("handle: failed to capture handler state");
    }

    EvalResult run_result = jit_handle_switch_to_body(ctx, interp);
    if (run_result.error.has_error) return run_result;

    EvalResult dispatch_result;
    if (jit_handle_dispatch_signals(state, expr, env, interp, ctx, &dispatch_result)) {
        return dispatch_result;
    }
    return jit_handle_finish_no_signal(expr, h, env, interp, ctx);
}

// =============================================================================
// END CORO-BASED HANDLE/SIGNAL/RESOLVE
// =============================================================================

fn EvalResult jit_apply_continuation(Value* k_val, Value* arg, Interp* interp) {
    if (k_val.cont_val == null) {
        return eval_error("cannot resume continuation: null continuation");
    }
    return jit_apply_continuation_impl(k_val, arg, interp);
}
