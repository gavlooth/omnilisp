module lisp;

import std::io;
import main;

// =============================================================================
// STACK-CONTEXT-BASED HANDLE/SIGNAL/RESOLVE
// =============================================================================

/**
 * Stack-context-based signal: suspends the current context when a matching handler is found.
 *
 * Returns Value* directly (same pattern as jit_shift_impl — avoids the hidden
 * return pointer issue with EvalResult after stack_ctx_clone).
 *
 * Falls through to the I/O fast path or error if no handler matches.
 */
fn Value* jit_signal_impl(Expr* expr, Env* env, Interp* interp) {
    Value* arg_v = jit_eval(expr.perform.arg, env, interp);
    if (arg_v != null && arg_v.tag == ERROR) {
        return arg_v;
    }

    SymbolId tag = (SymbolId)0;
    Value* tag_err = resolve_effect_tag_symbol(
        expr.perform.tag,
        expr.perform.tag_expr,
        expr.perform.has_tag_expr,
        env,
        interp,
        &tag,
        "signal"
    );
    if (tag_err != null) return tag_err;

    Value* type_err = jit_signal_type_check(tag, arg_v, interp);
    if (type_err != null) return type_err;

    main::StackCtx* current = main::g_current_stack_ctx;
    if (current != null) {
        bool hit_strict = false;
        for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
            EffectHandler* h = &interp.handler_stack[(usz)i];
            if (effect_handler_has_tag(h, tag)) {
                Value* resumed = jit_signal_suspend(h, tag, arg_v, interp, current);
                if (resumed != null) return resumed;
            }
            if (h.strict_mode) {
                hit_strict = true;
                break;
            }
        }

        if (hit_strict) {
            return raise_error_with_payload_names(
                interp,
                "runtime",
                "runtime/strict-unhandled-effect",
                "strict handler: unhandled effect"
            );
        }
    }

    Value* fast = jit_signal_try_fast_path(tag, arg_v, interp);
    if (fast != null) return fast;
    return jit_unhandled_effect_error(tag, arg_v, interp);
}

/**
 * Dispatch a single signal to a handler clause: find the matching clause, create
 * a continuation, bind k and arg, evaluate the clause body.
 *
 * Returns the handler clause result (Value*). On unmatched clause, returns an
 * ERROR-tagged value. Caller checks ctx.status and hstate.signaled to determine
 * whether to loop, return body result, or abort.
 */
fn Value* jit_handle_dispatch_signal(HandleEffectState* hstate, Expr* expr, Env* env, Interp* interp, main::StackCtx* ctx) {
    hstate.signaled = false;

    // Pop handler for clause evaluation (clause may push its own)
    interp.handler_count = hstate.handler_idx;

    // Find matching clause
    EffectClause* clause = null;
    for (usz j = 0; j < hstate.handler_copy.clause_count; j++) {
        if ((uint)hstate.handler_copy.tags[j] == (uint)hstate.signal_tag) {
            clause = &hstate.handler_copy.clauses[j];
            break;
        }
    }

    if (clause == null) {
        // No matching clause — return error for caller to handle
        char[256] ubuf;
        char[] utag = interp.symbols.get_name(hstate.signal_tag);
        char[] uarg = interp.symbols.get_name(value_type_name(hstate.signal_arg, interp));
        char[] umsg;
        if (try char[] formatted = io::bprintf(&ubuf, "unhandled effect '%s' with arg type %s", (ZString)utag, (ZString)uarg)) {
            umsg = formatted;
        } else {
            umsg = "unhandled effect";
        }
        return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", umsg);
    }

    // Create continuation wrapping the suspended context
    Continuation* k = interp.alloc_lisp_continuation();
    if (k == null) {
        return runtime_effect_raise(
            interp,
            "runtime/handle-continuation-allocation-failed",
            "handle: failed to allocate continuation"
        );
    }
    k.ctx = ctx;
    k.is_coroutine_based = true;
    k.handle_state = hstate;
    Value* k_val = make_continuation(interp, k);
    if (k_val == null || k_val.tag == ERROR) {
        interp_discard_lisp_continuation(interp, k);
        return k_val != null ? k_val : runtime_effect_raise(
            interp,
            "runtime/handle-continuation-allocation-failed",
            "handle: failed to allocate continuation wrapper"
        );
    }

    // Bind k and arg in clause environment
    Env* clause_env = jit_env_extend(interp, env, clause.k_name, k_val);
    if (clause_env == null) {
        interp_discard_lisp_continuation(interp, k);
        return runtime_effect_raise(
            interp,
            "runtime/handle-clause-bind-failed",
            "handle: out of memory while binding clause"
        );
    }
    clause_env = jit_env_extend(interp, clause_env, clause.arg_name, hstate.signal_arg);
    if (clause_env == null) {
        interp_discard_lisp_continuation(interp, k);
        return runtime_effect_raise(
            interp,
            "runtime/handle-clause-bind-failed",
            "handle: out of memory while binding clause"
        );
    }
    // Also bind __k for resolve
    clause_env = jit_env_extend(interp, clause_env, interp.sym__k, k_val);
    if (clause_env == null) {
        interp_discard_lisp_continuation(interp, k);
        return runtime_effect_raise(
            interp,
            "runtime/handle-clause-bind-failed",
            "handle: out of memory while binding clause"
        );
    }

    // Evaluate handler clause
    Value* handler_v = jit_eval(clause.handler_body, clause_env, interp);
    // Keep only continuations reachable from the returned result graph. Binding
    // k into the ESCAPE-lane clause env may retain transiently even when the
    // handler ignores k, and that must not transfer the suspended ctx.
    continuation_finish_handler_result(interp, handler_v, k);
    return handler_v;
}
