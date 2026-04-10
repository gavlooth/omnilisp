module lisp;

import main;

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

fn Value* jit_handle_init_clauses(Interp* interp, EffectHandler* h, Value** tags, Value** closures, usz count) {
    if (count == 0) {
        h.tags = null;
        h.closures = null;
        h.clauses = null;
        return null;
    }

    h.tags = (SymbolId*)mem::malloc(SymbolId.sizeof * count);
    h.closures = (Value**)mem::malloc(Value*.sizeof * count);
    if (h.tags == null || h.closures == null) {
        if (h.tags != null) mem::free(h.tags);
        if (h.closures != null) mem::free(h.closures);
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "handle: failed to allocate clauses");
    }
    for (usz i = 0; i < count; i++) {
        if (tags[i] != null && tags[i].tag == ERROR) {
            mem::free(h.tags);
            mem::free(h.closures);
            h.tags = null;
            h.closures = null;
            return tags[i];
        }
        if (tags[i] == null || tags[i].tag != SYMBOL) {
            mem::free(h.tags);
            mem::free(h.closures);
            h.tags = null;
            h.closures = null;
            return runtime_effect_invalid_tag(tags[i], interp, "handle");
        }
        h.tags[i] = tags[i].sym_val;
        h.closures[i] = closures[i];
    }
    h.clauses = null;
    return null;
}

fn Value* jit_handle_find_closure(HandleEffectState* state) {
    for (usz i = 0; i < state.handler_copy.clause_count; i++) {
        if ((uint)state.handler_copy.tags[i] == (uint)state.signal_tag) {
            return state.handler_copy.closures[i];
        }
    }
    return null;
}

fn Value* jit_handle_dispatch_pending_raise_value(Interp* interp, HandleEffectState* state) {
    if (!interp.flags.raise_pending || state == null) return null;
    SymbolId raise_sym = interp.sym_raise;
    for (usz i = 0; i < state.handler_copy.clause_count; i++) {
        if ((uint)state.handler_copy.tags[i] != (uint)raise_sym) continue;
        Value* handler_closure = state.handler_copy.closures[i];
        if (handler_closure == null) {
            return runtime_effects_raise(interp, "runtime/invalid-raise-handler", "handle: raise handler closure missing");
        }
        interp.flags.raise_pending = false;
        Value* msg_val = interp.raise_payload;
        if (msg_val == null) {
            msg_val = make_string(interp, interp.raise_msg[:interp.raise_msg_len]);
        }
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        interp.raise_msg[0] = 0;
        if (msg_val == null) {
            return make_error(interp, "handle: failed to materialize raise payload");
        }
        if (msg_val.tag == ERROR) return msg_val;
        Value* k_val = make_nil(interp);
        Value* pair = make_list2_or_error(interp, k_val, msg_val, "handle: failed to construct raise handler args");
        if (pair.tag == ERROR) return pair;
        return jit_apply_value(handler_closure, pair, interp);
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
        Value* pair = make_list2_or_error(interp, k_val, state.signal_arg, "handle: failed to construct handler args");
        if (pair.tag == ERROR) return pair;

        last_handler_v = jit_apply_value(handler_closure, pair, interp);
        // Retain before frame cleanup when the continuation escapes in the handler result graph.
        continuation_retain_if_referenced_by_result(last_handler_v, k);
        if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) break;
    }
    return last_handler_v;
}

fn bool jit_handle_alloc_state(Interp* interp, Value* body_closure, HandleEffectState** out_state) {
    HandleEffectState* state = (HandleEffectState*)mem::malloc(HandleEffectState.sizeof);
    if (state == null) return false;
    handle_effect_state_init(state, interp);
    state.is_aot = true;
    state.aot_body = body_closure;
    *out_state = state;
    return true;
}

fn void jit_handle_fill_handler(EffectHandler* h, usz count, HandleEffectState* state, bool strict_mode) {
    h.clause_count = count;
    h.handler_env = null;
    h.body_expr = null;
    h.body_env = null;
    h.strict_mode = strict_mode;
    h.effect_state = state;
}

fn Value* jit_handle_value(Interp* interp, Value** tags, Value** closures, usz count, Value* body_closure, bool strict_mode) {
    jit_handle_warm_closures(interp, closures, count, body_closure);

    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/stack-context-create", "stack engine: failed to create context for handle"
        );
    }
    bool destroy_ctx_on_exit = true;
    defer if (destroy_ctx_on_exit) main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);

    HandleEffectState* state = null;
    if (!jit_handle_alloc_state(interp, body_closure, &state)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "handle: failed to allocate state");
    }
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

    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    usz my_idx = interp.handler_count;
    EffectHandler* h = &interp.handler_stack[my_idx];
    jit_handle_fill_handler(h, count, state, strict_mode);

    Value* clause_err = jit_handle_init_clauses(interp, h, tags, closures, count);
    if (clause_err != null) return clause_err;
    SymbolId* owned_tags = h.tags;
    Value** owned_closures = h.closures;
    defer if (owned_tags != null) mem::free(owned_tags);
    defer if (owned_closures != null) mem::free(owned_closures);

    interp.handler_count = my_idx + 1;
    defer interp.handler_count = my_idx;

    state.handler_idx = my_idx;
    if (!handle_effect_state_capture_handler_copy(state, h)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "handle: failed to capture handler state");
    }

    SavedInterpState saved;
    save_interp_state(interp, &saved);
    main::StackContext parent_ctx;
    main::stack_ctx_switch_to(ctx, &parent_ctx);
    if (!jit_continuation_drive_blocked_fiber(ctx, interp, &parent_ctx)) {
        restore_interp_state_preserve_raise(interp, &saved);
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/fiber-yield-failed",
            "handle: failed to yield blocked scheduler fiber"
        );
    }
    restore_interp_state_preserve_raise(interp, &saved);

    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow in handle body");
    }

    Value* last_handler_v = jit_handle_run_signals(interp, ctx, state, my_idx);
    if (last_handler_v != null && last_handler_v.tag == ERROR) return last_handler_v;

    Value* pending_raise_v = jit_handle_dispatch_pending_raise_value(interp, state);
    if (pending_raise_v != null) return pending_raise_v;

    if (!jit_continuation_drive_blocked_fiber(ctx, interp, &parent_ctx)) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/fiber-yield-failed",
            "handle: failed to yield blocked scheduler fiber"
        );
    }

    if (ctx.status == main::StackCtxStatus.CTX_SUSPENDED && state.continuation_refcount == 0) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/handle-resuspended",
            "handle: body suspended before completion"
        );
    }

    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        return (Value*)ctx.result;
    }
    return last_handler_v;
}
