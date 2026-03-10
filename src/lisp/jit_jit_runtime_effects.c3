module lisp;

import main;

fn Expr* parse_for_jit(char[] source, Interp* interp) {
    return parse(source, interp);
}
// Parallel state for Value-based (AOT) effects
struct ResetValueState {
    Value*  body;         // Reset body closure
    Interp* interp;
    bool    shifted;
    Value*  shift_body;   // Shift body closure
    Value*  shift_k;      // Captured k for shift body
}

fn Value* runtime_effects_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

fn void reset_value_entry(void* arg) {
    ResetValueState* state = (ResetValueState*)arg;
    Value* v = jit_apply_value(state.body, make_nil(state.interp), state.interp);
    main::g_current_stack_ctx.result = v;
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

    // Save ALL interp state (was incomplete — only saved 3 of 12 fields)
    SavedInterpState saved;

    save_interp_state(interp, &saved);

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);

    interp.current_reset_state = saved_state;
    restore_interp_state(interp, &saved);

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow in reset");
    }

    if (state.shifted) {
        return jit_apply_value(state.shift_body, state.shift_k, interp);
    }

    Value* v = (Value*)ctx.result;
    main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
    return v;
}
fn Value* jit_shift_value(Interp* interp, Value* body) {
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/shift-outside-reset", "shift outside of reset");
    }

    ResetValueState* rstate = (ResetValueState*)interp.current_reset_state;
    if (rstate == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/shift-outside-reset", "shift outside of reset");
    }

    Continuation* k = interp.alloc_lisp_continuation();
    k.ctx = current;
    k.is_coroutine_based = true;
    Value* k_val = make_continuation(interp, k);

    rstate.shifted = true;
    rstate.shift_body = body;
    rstate.shift_k = k_val;

    if (!suspend_with_scope_guard(current, interp.current_scope)) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/suspend-scope-guard", "stack engine: failed to register suspend scope guard"
        );
    }
    return interp.resume_value;
}

fn Value* jit_signal_eval_result_or_error(EvalResult r, Interp* interp) @inline {
    if (r.error.has_error) return runtime_effects_raise(interp, "runtime/fast-path-primitive-failed", r.error.message[:256]);
    return r.value;
}

fn Value* jit_signal_apply_fast_path_prim(Interp* interp, Value* fp_prim, Value* arg, bool* handled) {
    int fp_arity = fp_prim.prim_val.arity;
    if (fp_arity == 0) {
        *handled = true;
        return jit_signal_eval_result_or_error(apply_primitive(fp_prim, make_nil(interp), interp), interp);
    }
    if (fp_arity == 1 || fp_arity == -1) {
        *handled = true;
        return jit_signal_eval_result_or_error(apply_primitive(fp_prim, arg, interp), interp);
    }
    if (arg == null || !is_cons(arg)) return null;

    *handled = true;
    EvalResult first = apply_primitive(fp_prim, car(arg), interp);
    Value* applied = jit_signal_eval_result_or_error(first, interp);
    if (applied != null && applied.tag == ERROR) return applied;
    Value* result = jit_apply_value(applied, cdr(arg), interp);
    if (result != null && result.tag == ERROR) return result;
    return result;
}

fn bool jit_signal_try_resume_handler(Interp* interp, main::StackCtx* current, SymbolId tag_sym, Value* arg, Value** out_result) {
    if (current == null) return false;
    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* h = &interp.handler_stack[(usz)i];
        for (usz j = 0; j < h.clause_count; j++) {
            if (h.tags[j] != tag_sym || h.effect_state == null) continue;
            HandleEffectState* hstate = (HandleEffectState*)h.effect_state;
            hstate.signaled = true;
            hstate.signal_tag = tag_sym;
            hstate.signal_arg = arg;
            if (!suspend_with_scope_guard(current, interp.current_scope)) {
                *out_result = raise_error_with_payload_names(
                    interp,
                    "runtime",
                    "runtime/suspend-scope-guard",
                    "stack engine: failed to register suspend scope guard"
                );
                return true;
            }
            *out_result = interp.resume_value;
            return true;
        }
    }
    return false;
}

fn Value* jit_signal_try_fast_path_dispatch(Interp* interp, SymbolId tag_sym, Value* arg, bool* handled) {
    for (usz fp_i = 0; fp_i < interp.fast_path_count; fp_i++) {
        if ((uint)interp.fast_path_table[fp_i].tag != (uint)tag_sym) continue;
        return jit_signal_apply_fast_path_prim(interp, interp.fast_path_table[fp_i].prim, arg, handled);
    }
    return null;
}

fn Value* jit_signal_value(Interp* interp, Value* tag, Value* arg) {
    Value* resumed = null;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (jit_signal_try_resume_handler(interp, current, (SymbolId)tag.sym_val, arg, &resumed)) {
        return resumed;
    }

    bool handled = false;
    Value* fast_result = jit_signal_try_fast_path_dispatch(interp, (SymbolId)tag.sym_val, arg, &handled);
    if (handled) return fast_result;
    return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", "unhandled effect");
}

fn Value* jit_resolve_validate_input(Interp* interp, Value* k_val, Continuation** out_k) {
    if (k_val == null || k_val.tag != CONTINUATION) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation", "resolve: __k is not a valid continuation"
        );
    }

    Continuation* k = k_val.cont_val;

    // Single-shot check
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

    *out_k = k;
    return null;
}

fn HandleEffectState* jit_resolve_push_handler_copy(Interp* interp, Continuation* k) {
    HandleEffectState* hstate = (HandleEffectState*)k.handle_state;
    if (hstate != null) {
        if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
        interp.handler_stack[interp.handler_count] = hstate.handler_copy;
        interp.handler_count++;
    }
    return hstate;
}

fn void jit_resolve_pop_handler_copy(Interp* interp, HandleEffectState* hstate) @inline {
    if (hstate != null) {
        interp.handler_count--;
    }
}

fn Value* jit_resolve_finish_result(Interp* interp, Continuation* k) @inline {
    if (k.ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        return (Value*)k.ctx.result;
    }
    return interp.resume_value;
}

fn Value* jit_resolve_value(Interp* interp, Value* k_val, Value* val) {
    Continuation* k = null;
    Value* validation_err = jit_resolve_validate_input(interp, k_val, &k);
    if (validation_err != null) return validation_err;

    // Reinstall effect handler copy from when the signal occurred
    HandleEffectState* hstate = jit_resolve_push_handler_copy(interp, k);

    // Pass resolve value to suspended context
    interp.resume_value = val;

    // Resume the context
    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(k.ctx, &parent_ctx);

    // Context returned (either completed, signaled again, or died)
    jit_resolve_pop_handler_copy(interp, hstate);
    Value* result = jit_resolve_finish_result(interp, k);

    // Single-shot: invalidate continuation
    k.ctx = null;
    k.handle_state = null;

    return result;
}

fn void jit_handle_warm_closures(Interp* interp, Value** closures, usz count, Value* body_closure) {
    if (body_closure != null && body_closure.tag == CLOSURE) {
        jit_warm_expr_cache(body_closure.closure_val.body, interp);
    }
    for (usz i = 0; i < count; i++) {
        if (closures[i] != null && closures[i].tag == CLOSURE) {
            jit_warm_expr_cache(closures[i].closure_val.body, interp);
        }
    }
}

fn bool jit_handle_init_clauses(EffectHandler* h, Value** tags, Value** closures, usz count) {
    h.tags = (SymbolId*)mem::malloc(SymbolId.sizeof * count);
    h.closures = (Value**)mem::malloc(Value*.sizeof * count);
    if (h.tags == null || h.closures == null) {
        if (h.tags != null) mem::free(h.tags);
        if (h.closures != null) mem::free(h.closures);
        return false;
    }
    for (usz i = 0; i < count; i++) {
        h.tags[i] = tags[i].sym_val;
        h.closures[i] = closures[i];
    }
    h.clauses = null;
    return true;
}

fn Value* jit_handle_find_closure(HandleEffectState* state) {
    for (usz i = 0; i < state.handler_copy.clause_count; i++) {
        if ((uint)state.handler_copy.tags[i] == (uint)state.signal_tag) {
            return state.handler_copy.closures[i];
        }
    }
    return null;
}

fn Value* jit_handle_run_signals(Interp* interp, main::StackCtx* ctx, HandleEffectState* state, usz handler_idx) {
    Value* last_handler_v = null;
    while (state.signaled) {
        state.signaled = false;
        interp.handler_count = handler_idx;

        Value* handler_closure = jit_handle_find_closure(state);
        if (handler_closure == null) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/unhandled-effect", "unhandled effect");
        }

        Continuation* k = interp.alloc_lisp_continuation();
        k.ctx = ctx;
        k.is_coroutine_based = true;
        k.handle_state = state;
        Value* k_val = make_continuation(interp, k);
        Value* pair = make_cons(interp, k_val, state.signal_arg);

        last_handler_v = jit_apply_value(handler_closure, pair, interp);
        if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) break;
    }
    return last_handler_v;
}

fn bool jit_handle_alloc_state(Interp* interp, Value* body_closure, HandleEffectState** out_state) {
    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    if (state == null) return false;
    state.body = null;
    state.env = null;
    state.interp = interp;
    state.signaled = false;
    state.signal_tag = (SymbolId)0;
    state.signal_arg = null;
    state.is_aot = true;
    state.aot_body = body_closure;
    *out_state = state;
    return true;
}

fn void jit_handle_fill_handler(EffectHandler* h, usz count, HandleEffectState* state) {
    h.clause_count = count;
    h.handler_env = null;
    h.body_expr = null;
    h.body_env = null;
    h.strict_mode = false;
    h.effect_state = state;
}

fn Value* jit_handle_value(Interp* interp, Value** tags, Value** closures, usz count, Value* body_closure) {
    jit_handle_warm_closures(interp, closures, count, body_closure);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/stack-context-create", "stack engine: failed to create context for handle"
        );
    }
    defer main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);

    HandleEffectState* state = null;
    if (!jit_handle_alloc_state(interp, body_closure, &state)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "handle: failed to allocate state");
    }
    defer mem::free(state);

    main::stack_ctx_init(ctx, &handle_effect_entry, state);

    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    usz my_idx = interp.handler_count;
    EffectHandler* h = &interp.handler_stack[my_idx];
    jit_handle_fill_handler(h, count, state);

    if (!jit_handle_init_clauses(h, tags, closures, count)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "handle: failed to allocate clauses");
    }
    SymbolId* owned_tags = h.tags;
    Value** owned_closures = h.closures;
    defer mem::free(owned_tags);
    defer mem::free(owned_closures);

    interp.handler_count = my_idx + 1;
    defer interp.handler_count = my_idx;

    state.handler_copy = *h;
    state.handler_idx = my_idx;

    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow in handle body");
    }

    Value* last_handler_v = jit_handle_run_signals(interp, ctx, state, my_idx);
    if (last_handler_v != null && last_handler_v.tag == ERROR) return last_handler_v;

    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        return (Value*)ctx.result;
    }
    return last_handler_v;
}
