module lisp;

import main;

fn bool boundary_scope_chain_scan_bypass_enabled() @inline {
    return boundary_policy_scope_chain_scan_bypass_enabled();
}

fn usz boundary_scope_chain_scan_budget() @inline {
    return boundary_policy_scope_chain_scan_budget();
}

fn bool boundary_scope_chain_scan_allowed() @inline {
    if (boundary_scope_chain_scan_bypass_enabled()) {
        boundary_telemetry_inc_scope_chain_scan_suppressed();
        return false;
    }

    usz budget = boundary_scope_chain_scan_budget();
    if (budget == 0) return true;
    if (g_boundary_decision_stats.scope_chain_scan_total >= budget) {
        boundary_telemetry_inc_scope_chain_scan_suppressed();
        return false;
    }
    return true;
}

<*
@require interp != null : "boundary_ptr_in_target_scope_chain: null interp"
*>
fn bool boundary_ptr_in_target_scope_chain(void* ptr, Interp* interp) @inline {
    if (ptr == null) return false;
    return boundary_ptr_in_scope_chain(ptr, interp.current_scope);
}

fn bool boundary_ptr_in_scope(void* ptr, main::ScopeRegion* scope) @inline {
    if (ptr == null || scope == null) return false;
    return main::is_in_scope(ptr, scope);
}

fn bool boundary_ptr_in_scope_chain_scan(void* ptr, main::ScopeRegion* scope) @inline {
    if (!boundary_scope_chain_scan_allowed()) return false;

    if (ptr == null || scope == null) return false;
    main::ScopeRegion* cur = scope;
    while (cur != null) {
        if (boundary_ptr_in_scope(ptr, cur)) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool boundary_ptr_in_scope_chain(void* ptr, main::ScopeRegion* scope) @inline {
    if (!boundary_scope_chain_scan_allowed()) return false;

    boundary_telemetry_inc_scope_chain_scan();
    return boundary_ptr_in_scope_chain_scan(ptr, scope);
}

fn bool boundary_ptr_in_scope_chain_with_hint(void* ptr, main::ScopeRegion* scope, uint pinned_gen) @inline {
    return boundary_ptr_in_scope_chain_with_hint_no_cache(ptr, scope, pinned_gen);
}

fn bool boundary_ptr_in_scope_chain_with_hint_no_cache(void* ptr, main::ScopeRegion* scope, uint pinned_gen) @inline {
    if (!boundary_scope_chain_scan_allowed()) return false;

    boundary_telemetry_inc_scope_chain_scan();
    boundary_telemetry_inc_scope_chain_scan_with_hint();

    if (ptr == null || scope == null) return false;
    if (pinned_gen == 0) {
        boundary_telemetry_inc_scope_chain_scan_fallback();
        return boundary_ptr_in_scope_chain_scan(ptr, scope);
    }

    main::ScopeRegion* cur = scope;
    while (cur != null) {
        if (cur.generation == pinned_gen || cur.escape_generation == pinned_gen) {
            return boundary_ptr_in_scope(ptr, cur);
        }
        cur = cur.parent;
    }

    boundary_telemetry_inc_scope_chain_scan_fallback();
    return boundary_ptr_in_scope_chain_scan(ptr, scope);
}

fn bool boundary_ptr_in_target_scope_chain_with_hint(void* ptr, Interp* interp, uint pinned_gen) @inline {
    if (ptr == null || interp == null) return false;

    if (interp.current_scope == null) return false;
    if (pinned_gen == 0) {
        return boundary_ptr_in_scope_chain_with_hint(ptr, interp.current_scope, pinned_gen);
    }

    if (interp.active_promotion_ctx != null) {
        bool cached_result = false;
        if (promotion_context_lookup_scope_chain_cache(
            interp.active_promotion_ctx,
            ptr,
            interp.current_scope,
            pinned_gen,
            &cached_result
        )) {
            return cached_result;
        }
    }

    bool result = boundary_ptr_in_scope_chain_with_hint_no_cache(ptr, interp.current_scope, pinned_gen);
    if (interp.active_promotion_ctx != null) {
        promotion_context_remember_scope_chain_cache(
            interp.active_promotion_ctx,
            ptr,
            interp.current_scope,
            pinned_gen,
            result
        );
    }

    return result;
}

<*
@require interp != null : "boundary_value_in_releasing_scope: null interp"
*>
fn bool boundary_value_in_releasing_scope(Value* v, Interp* interp) @inline {
    if (v == null || interp.releasing_scope == null) return false;
    return boundary_ptr_in_scope((void*)v, interp.releasing_scope);
}

<*
@require interp != null : "boundary_can_reuse_value: null interp"
*>
fn bool boundary_can_reuse_value(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    boundary_assert_interp_scope_chain(interp);
    if (boundary_value_in_releasing_scope(v, interp)) return false;
    return boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
}

<*
@require interp != null : "boundary_classify_return_value: null interp"
@require target_scope != null : "boundary_classify_return_value: null target_scope"
@require releasing_scope != null : "boundary_classify_return_value: null releasing_scope"
*>
fn BoundaryReturnClassification boundary_classify_return_value(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    BoundaryReturnClassification classification = {
        .provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN,
        .in_target_scope_chain = false,
        .in_releasing_scope = false,
    };

    if (v == null) return classification;

    classification.in_releasing_scope = boundary_ptr_in_scope((void*)v, releasing_scope);
    classification.in_target_scope_chain = boundary_ptr_in_scope_chain_with_hint((void*)v, target_scope, v.scope_gen);

    if (!classification.in_releasing_scope && classification.in_target_scope_chain) {
        classification.provenance = BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN;
        return classification;
    }

    if (classification.in_releasing_scope) {
        if (v.scope_gen == releasing_scope.generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;
            return classification;
        }
        if (v.scope_gen == releasing_scope.escape_generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE;
            return classification;
        }
    }

    classification.provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN;
    return classification;
}

fn BoundaryScopeTransferResult boundary_try_scope_splice_escapes_result(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    BoundaryScopeTransferResult result = boundary_check_scope_transfer(parent, child);
    if (result.allowed) {
        main::scope_splice_escapes(parent, child);
    }
    boundary_note_splice_result(result);
    return result;
}

<*
@require interp != null : "boundary_should_copy_env: null interp"
*>
fn bool boundary_should_copy_env(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    return !boundary_can_reuse_value(v, interp);
}

fn bool boundary_should_promote(Value* v, PromotionContext* ctx, main::ScopeRegion* source_scope) @inline {
    if (v == null || ctx == null || source_scope == null) return false;
    if (ctx.aborted) return false;
    return v.scope_gen == source_scope.escape_generation;
}

fn bool boundary_cons_escape_child_edge_safe(Value* edge, main::ScopeRegion* target_scope) @inline {
    if (edge == null || target_scope == null) return true;
    if (!boundary_ptr_in_scope((void*)edge, target_scope)) return true;
    return edge.scope_gen != target_scope.generation;
}

fn bool boundary_nonunique_destination_retry_allowed(Value* v) @inline {
    if (v == null) return false;
    switch (v.tag) {
        case NIL:
        case INT:
        case DOUBLE:
        case SYMBOL:
        case TIME_POINT:
            return true;
        default:
            return false;
    }
}

fn bool boundary_closure_payload_in_scope(Value* v, main::ScopeRegion* scope) @inline {
    if (v == null || scope == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (v.closure_val.scope_gen == scope.generation) return true;
    return boundary_ptr_in_scope((void*)v.closure_val, scope);
}
