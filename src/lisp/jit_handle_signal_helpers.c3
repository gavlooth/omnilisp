module lisp;

/**
 * Shared state between jit_handle_impl (parent) and jit_signal_impl (body context).
 * Heap-allocated per handle activation and retained while escaped continuations exist.
 */
struct HandleEffectState {
    Expr*          body;
    Env*           env;
    Interp*        interp;
    bool           signaled;       // Set true by signal before suspending
    SymbolId       signal_tag;     // Which effect tag was signaled
    Value*         signal_arg;     // Argument passed to signal
    EffectHandler  handler_copy;   // Copy of handler for reinstallation by resolve
    usz            handler_idx;    // Handler stack index
    bool           is_aot;
    Value*         aot_body;
    uint           continuation_refcount; // Active continuations retaining this state
    bool           frame_active;          // True while enclosing handle frame is live
    HandleEffectState* live_next;         // Intrusive link for live-state registry
}

HandleEffectState* g_handle_effect_live_head = null;

fn bool handle_effect_state_is_live(HandleEffectState* state) {
    if (state == null) return false;
    HandleEffectState* it = g_handle_effect_live_head;
    while (it != null) {
        if (it == state) return true;
        it = it.live_next;
    }
    return false;
}

fn void handle_effect_state_register_live(HandleEffectState* state) {
    if (state == null) return;
    state.live_next = g_handle_effect_live_head;
    g_handle_effect_live_head = state;
}

fn void handle_effect_state_unregister_live(HandleEffectState* state) {
    if (state == null) return;
    HandleEffectState* prev = null;
    HandleEffectState* it = g_handle_effect_live_head;
    while (it != null) {
        if (it == state) {
            if (prev == null) {
                g_handle_effect_live_head = it.live_next;
            } else {
                prev.live_next = it.live_next;
            }
            state.live_next = null;
            return;
        }
        prev = it;
        it = it.live_next;
    }
}

fn void handle_effect_state_reset_handler_copy(HandleEffectState* state) @inline {
    state.handler_copy.tags = null;
    state.handler_copy.clauses = null;
    state.handler_copy.closures = null;
    state.handler_copy.clause_count = 0;
    state.handler_copy.handler_env = null;
    state.handler_copy.body_expr = null;
    state.handler_copy.body_env = null;
    state.handler_copy.strict_mode = false;
    state.handler_copy.effect_state = state;
}

fn void handle_effect_state_init(HandleEffectState* state, Interp* interp) {
    state.body = null;
    state.env = null;
    state.interp = interp;
    state.signaled = false;
    state.signal_tag = (SymbolId)0;
    state.signal_arg = null;
    state.handler_idx = 0;
    state.is_aot = false;
    state.aot_body = null;
    state.continuation_refcount = 0;
    state.frame_active = true;
    state.live_next = null;
    handle_effect_state_reset_handler_copy(state);
    handle_effect_state_register_live(state);
}

fn void handle_effect_state_scrub_continuations(HandleEffectState* state) {
    if (state == null || state.interp == null) return;
    Continuation* k = state.interp.continuation_head;
    while (k != null) {
        if ((HandleEffectState*)k.handle_state == state) {
            // State is being released: invalidate continuation frames that still
            // point at this handle state so later resolve paths fail closed.
            k.ctx = null;
            k.handle_state = null;
            k.handle_retained = false;
        }
        k = k.next_owned;
    }
}

fn void handle_effect_state_release(HandleEffectState* state) {
    if (state == null) return;
    if (!handle_effect_state_is_live(state)) return;
    handle_effect_state_unregister_live(state);
    handle_effect_state_scrub_continuations(state);
    if (state.handler_copy.tags != null) {
        mem::free(state.handler_copy.tags);
        state.handler_copy.tags = null;
    }
    if (state.handler_copy.clauses != null) {
        mem::free(state.handler_copy.clauses);
        state.handler_copy.clauses = null;
    }
    if (state.handler_copy.closures != null) {
        mem::free(state.handler_copy.closures);
        state.handler_copy.closures = null;
    }
    state.interp = null;
    mem::free(state);
}

fn bool handle_effect_state_capture_handler_copy(HandleEffectState* state, EffectHandler* src) {
    assert(state != null, "handle_effect_state_capture_handler_copy: state must not be null");
    assert(src != null, "handle_effect_state_capture_handler_copy: src must not be null");

    handle_effect_state_reset_handler_copy(state);
    state.handler_copy.effect_state = state;
    state.handler_copy.clause_count = src.clause_count;
    state.handler_copy.handler_env = src.handler_env;
    state.handler_copy.body_expr = src.body_expr;
    state.handler_copy.body_env = src.body_env;
    state.handler_copy.strict_mode = src.strict_mode;

    usz count = src.clause_count;
    if (count == 0) {
        return true;
    }

    SymbolId* copied_tags = null;
    EffectClause* copied_clauses = null;
    Value** copied_closures = null;

    if (src.tags != null) {
        if (count > usz.max / SymbolId.sizeof) return false;
        copied_tags = (SymbolId*)mem::malloc(SymbolId.sizeof * count);
        if (copied_tags == null) return false;
        mem::copy(copied_tags, src.tags, SymbolId.sizeof * count);
    }

    if (src.clauses != null) {
        if (count > usz.max / EffectClause.sizeof) {
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        copied_clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * count);
        if (copied_clauses == null) {
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        mem::copy(copied_clauses, src.clauses, EffectClause.sizeof * count);
    }

    if (src.closures != null) {
        if (count > usz.max / Value*.sizeof) {
            if (copied_clauses != null) mem::free(copied_clauses);
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        copied_closures = (Value**)mem::malloc(Value*.sizeof * count);
        if (copied_closures == null) {
            if (copied_clauses != null) mem::free(copied_clauses);
            if (copied_tags != null) mem::free(copied_tags);
            return false;
        }
        for (usz i = 0; i < count; i++) {
            Value* closure = src.closures[i];
            if (closure != null && state.interp != null) {
                Value* promoted = boundary_promote_to_root(closure, state.interp);
                bool promoted_callable = promoted != null
                    && (promoted.tag == CLOSURE || promoted.tag == PRIMITIVE || promoted.tag == PARTIAL_PRIM);
                if (promoted == null || promoted.tag == ERROR || !promoted_callable) {
                    mem::free(copied_closures);
                    if (copied_clauses != null) mem::free(copied_clauses);
                    if (copied_tags != null) mem::free(copied_tags);
                    return false;
                }
                closure = promoted;
            }
            copied_closures[i] = closure;
        }
    }

    state.handler_copy.tags = copied_tags;
    state.handler_copy.clauses = copied_clauses;
    state.handler_copy.closures = copied_closures;
    return true;
}
