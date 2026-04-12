module lisp;

import main;

fn bool boundary_scope_chain_contains(main::ScopeRegion* scope, main::ScopeRegion* ancestor) @inline {
    if (scope == null || ancestor == null) return false;
    main::ScopeRegion* cur = scope;
    while (cur != null) {
        if (cur == ancestor) return true;
        cur = cur.parent;
    }
    return false;
}

fn char[] boundary_scope_transfer_reason_name(BoundaryScopeTransferReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_SCOPE_TRANSFER_REASON_NAMES.len) {
        return BOUNDARY_SCOPE_TRANSFER_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_promotion_abort_reason_name(BoundaryPromotionAbortReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_PROMOTION_ABORT_REASON_NAMES.len) {
        return BOUNDARY_PROMOTION_ABORT_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_return_provenance_name(BoundaryReturnProvenance provenance) @inline {
    usz idx = (usz)provenance;
    if (idx < BOUNDARY_RETURN_PROVENANCE_NAMES.len) {
        return BOUNDARY_RETURN_PROVENANCE_NAMES[idx];
    }
    return "unknown";
}

fn bool boundary_scope_optional_lane_shape_valid(char* bump, char* limit, main::ScopeChunk* chunks) @inline {
    if (chunks == null) return bump == null && limit == null;
    if (bump == null || limit == null) return false;
    return bump <= limit;
}

fn bool boundary_scope_required_lane_shape_valid(char* bump, char* limit, main::ScopeChunk* chunks) @inline {
    if (chunks == null) return false;
    if (bump == null || limit == null) return false;
    return bump <= limit;
}

fn BoundaryScopeTransferResult boundary_scope_transfer_ok() @inline {
    return { .allowed = true, .reason = BOUNDARY_SCOPE_TRANSFER_OK };
}

fn bool boundary_graph_audit_enabled() @inline {
    return boundary_policy_graph_audit_enabled();
}

fn usz boundary_graph_audit_rate() @inline {
    return boundary_policy_graph_audit_rate();
}

fn usz boundary_graph_audit_max_roots() @inline {
    return boundary_policy_graph_audit_max_roots();
}

const usz BOUNDARY_GRAPH_AUDIT_MAX_NODES = 8192;

fn bool boundary_graph_audit_value_seen(Value** seen, usz seen_len, Value* v) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == v) return true;
    }
    return false;
}

fn bool boundary_graph_audit_env_seen(Env** seen, usz seen_len, Env* env) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == env) return true;
    }
    return false;
}

fn bool boundary_graph_audit_push_value(Value** stack, usz* stack_len, Value* v) @inline {
    if (v == null) return true;
    if (*stack_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
    stack[*stack_len] = v;
    (*stack_len)++;
    return true;
}

fn bool boundary_graph_audit_push_env(Env** stack, usz* stack_len, Env* env) @inline {
    if (env == null) return true;
    if (*stack_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
    stack[*stack_len] = env;
    (*stack_len)++;
    return true;
}

fn bool boundary_value_points_into_scope_temp(Value* v, main::ScopeRegion* scope) @inline {
    return v != null &&
        scope != null &&
        boundary_ptr_in_scope((void*)v, scope) &&
        v.scope_gen == scope.generation;
}

fn bool boundary_env_points_into_scope_temp(Env* env, main::ScopeRegion* scope) @inline {
    return env != null &&
        scope != null &&
        boundary_ptr_in_scope((void*)env, scope) &&
        env.scope_gen == scope.generation;
}

fn bool boundary_value_points_into_temp(
    Value* v,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* forbidden_scope = null
) @inline {
    if (v == null || target_scope == null) return false;
    if (boundary_value_points_into_scope_temp(v, forbidden_scope)) return true;
    for (main::ScopeRegion* s = target_scope; s != null && s.parent != null; s = s.parent) {
        if (boundary_value_points_into_scope_temp(v, s)) return true;
    }
    return false;
}

fn bool boundary_env_points_into_temp(
    Env* env,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* forbidden_scope = null,
    Env* global_env = null
) @inline {
    if (env == null || target_scope == null) return false;
    if (env == global_env || env.is_root_persistent_box()) return false;
    if (boundary_env_points_into_scope_temp(env, forbidden_scope)) return true;
    for (main::ScopeRegion* s = target_scope; s != null && s.parent != null; s = s.parent) {
        if (boundary_env_points_into_scope_temp(env, s)) return true;
    }
    return false;
}
