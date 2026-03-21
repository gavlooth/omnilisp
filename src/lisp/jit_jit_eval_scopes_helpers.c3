module lisp;

import main;

struct ScopedFinalizeResult {
    Value* value;
    bool releasing_scope_spliced;
}

fn bool env_chain_has_frame_in_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (!cur.is_root_persistent_box() &&
            boundary_ptr_in_scope((void*)cur, scope)) {
            return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool is_in_temp_lane(void* ptr, main::ScopeRegion* scope) {
    if (ptr == null || scope == null) return false;
    main::ScopeChunk* chunk = scope.chunks;
    while (chunk != null) {
        char* data = (char*)chunk + main::ScopeChunk.sizeof;
        if ((char*)ptr >= data && (char*)ptr < data + chunk.capacity) return true;
        chunk = chunk.next;
    }
    return false;
}

fn bool env_chain_has_binding_in_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* v = cur.bindings[i].value;
            if (v != null && is_in_temp_lane((void*)v, scope)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_contains_root_persistent_box(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (cur.is_root_persistent_box()) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool jit_value_is_graph_carrying(Value* v) @inline {
    if (v == null) return false;
    switch (v.tag) {
        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
        case CLOSURE:
        case MODULE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
        case ITERATOR:
        case INSTANCE:
            return true;
        default:
            return false;
    }
}

fn bool env_chain_has_graph_binding_values(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            if (jit_value_is_graph_carrying(cur.bindings[i].value)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool jit_env_parent_needs_rewrite(Env* parent, main::ScopeRegion* releasing_scope) @inline {
    if (parent == null || releasing_scope == null) return false;
    // Scope-generation stamps may be stale after chunk transfers.
    // Parent rewrite must key off physical releasing-scope membership.
    return boundary_ptr_in_scope((void*)parent, releasing_scope);
}

fn bool jit_binding_payload_in_releasing_scope(Value* v, main::ScopeRegion* releasing_scope) @inline {
    if (v == null || releasing_scope == null) return false;
    if (v.tag == CLOSURE) {
        return boundary_closure_payload_in_scope(v, releasing_scope);
    }
    if (v.tag == ITERATOR) {
        Value* payload = v.iterator_val;
        if (payload != null && boundary_ptr_in_scope((void*)payload, releasing_scope)) {
            return true;
        }
        if (payload != null &&
            payload.tag == CLOSURE &&
            boundary_closure_payload_in_scope(payload, releasing_scope)) {
            return true;
        }
    }
    return false;
}

fn bool jit_binding_graph_needs_copy(Value* v, Interp* interp, usz depth = 0) {
    if (v == null || interp == null || interp.releasing_scope == null) return false;
    if (depth >= 128) return true;
    if (boundary_ptr_in_scope((void*)v, interp.releasing_scope)) return true;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen)) return false;
    if (copy_to_parent_payload_in_releasing_scope(v, interp)) return true;

    switch (v.tag) {
        case CONS:
            return jit_binding_graph_needs_copy(v.cons_val.car, interp, depth + 1) ||
                jit_binding_graph_needs_copy(v.cons_val.cdr, interp, depth + 1);
        default:
            return false;
    }
}

fn bool jit_tco_binding_needs_copy(
    Value* v,
    Interp* interp,
    main::ScopeRegion* releasing_scope
) @inline {
    if (v == null || interp == null || releasing_scope == null) return false;
    if (boundary_ptr_in_scope((void*)v, releasing_scope)) return true;
    return jit_binding_graph_needs_copy(v, interp);
}

<*
@require interp != null : "boundary_finalize_scoped_result: null interp"
@require saved_scope != null : "boundary_finalize_scoped_result: null saved_scope"
@require active_scope != null : "boundary_finalize_scoped_result: null active_scope"
*>
fn ScopedFinalizeResult boundary_finalize_scoped_result(
    Value* result,
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope
) {
    boundary_assert_interp_scope_chain(interp);
    ScopedFinalizeResult finalized = {
        .value = result,
        .releasing_scope_spliced = false,
    };
    PromotionContext boundary_ctx = {};
    // Return-boundary finalize must not inherit traversal-budget fallback behavior.
    // Use an unbounded boundary context so commit routing can terminate on
    // destination-aware promotion/splice/copy policy rather than budget abort.
    boundary_promotion_context_begin(interp, &boundary_ctx);
    defer {
        boundary_promotion_context_end(interp, &boundary_ctx);
        boundary_assert_interp_scope_chain(interp);
    }
    BoundaryCommitEscapeResult committed = boundary_commit_escape(
        result,
        interp,
        saved_scope,
        active_scope,
        &boundary_ctx
    );
    finalized.value = committed.value;
    finalized.releasing_scope_spliced = committed.releasing_scope_spliced;
    return finalized;
}

fn Value* jit_finalize_scoped_result(
    Value* result,
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope,
    bool* scope_spliced_out = null
) {
    ScopedFinalizeResult finalized = boundary_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (scope_spliced_out != null) {
        *scope_spliced_out = finalized.releasing_scope_spliced;
    }
    if (!finalized.releasing_scope_spliced) {
        main::scope_release(active_scope);
    }
    return finalized.value;
}

fn Env* jit_copy_tco_env_chain_for_recycle(
    Env* env,
    Interp* interp,
    main::ScopeRegion* releasing_scope
) {
    boundary_assert_interp_scope_chain(interp);
    BoundaryInterpState saved = boundary_save_interp_state(interp);
    defer boundary_restore_interp_state(interp, saved);
    BoundaryReleasingScopeSwap releasing_swap =
        boundary_releasing_scope_swap_begin(interp, releasing_scope);
    defer boundary_releasing_scope_swap_end(interp, &releasing_swap);
    PromotionContext bounce_ctx = {};
    boundary_promotion_context_begin(interp, &bounce_ctx, PROMOTION_CTX_BUDGET_TCO);
    defer boundary_promotion_context_end(interp, &bounce_ctx);
    Env* copied = copy_tco_env_chain(env, interp);
    boundary_assert_interp_scope_chain(interp);
    return copied;
}
