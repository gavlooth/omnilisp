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
