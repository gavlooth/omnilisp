module lisp;

import main;

enum BoundaryGraphAuditReason : char {
    BOUNDARY_GRAPH_AUDIT_OK,
    BOUNDARY_GRAPH_AUDIT_INVALID_TARGET,
    BOUNDARY_GRAPH_AUDIT_ALLOCATION_FAILED,
    BOUNDARY_GRAPH_AUDIT_SEED_OVERFLOW,
    BOUNDARY_GRAPH_AUDIT_VALUE_SET_OVERFLOW,
    BOUNDARY_GRAPH_AUDIT_ENV_SET_OVERFLOW,
    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
    BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
    BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE,
    BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV,
}

const usz BOUNDARY_GRAPH_AUDIT_REASON_COUNT =
    (usz)BoundaryGraphAuditReason.BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV + 1;

const char[][] BOUNDARY_GRAPH_AUDIT_REASON_NAMES = {
    "ok",
    "invalid_target",
    "allocation_failed",
    "seed_overflow",
    "value_set_overflow",
    "env_set_overflow",
    "push_value_overflow",
    "push_env_overflow",
    "reachable_temp_value",
    "reachable_temp_env",
};

$assert(BOUNDARY_GRAPH_AUDIT_REASON_NAMES.len == BOUNDARY_GRAPH_AUDIT_REASON_COUNT);

fn char[] boundary_graph_audit_reason_name(BoundaryGraphAuditReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_GRAPH_AUDIT_REASON_NAMES.len) {
        return BOUNDARY_GRAPH_AUDIT_REASON_NAMES[idx];
    }
    return "unknown";
}

const usz BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT = (usz)ValueTag.VOID + 1;
const usz BOUNDARY_GRAPH_AUDIT_EDGE_TAG_ENV = BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT;
const usz BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE = BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT + 1;

enum BoundaryGraphEdgeClass : char {
    BOUNDARY_GRAPH_EDGE_NONE,
    BOUNDARY_GRAPH_EDGE_VALUE,
    BOUNDARY_GRAPH_EDGE_ENV,
}

fn BoundaryGraphEdgeClass boundary_graph_audit_edge_class(ValueTag tag) @inline {
    switch (boundary_value_edge_policy(tag)) {
        case BOUNDARY_VALUE_EDGE_NONE:
            return BOUNDARY_GRAPH_EDGE_NONE;
        case BOUNDARY_VALUE_EDGE_VALUE:
            return BOUNDARY_GRAPH_EDGE_VALUE;
        case BOUNDARY_VALUE_EDGE_ENV:
            return BOUNDARY_GRAPH_EDGE_ENV;
    }
}

struct BoundaryGraphAuditResult {
    bool ok;
    BoundaryGraphAuditReason reason;
    usz root_tag;
    usz violating_edge_tag;
    usz root_scope_gen;
    usz violating_scope_gen;
    usz target_scope_gen;
    usz target_escape_gen;
}

fn BoundaryGraphAuditResult boundary_graph_audit_result_hot(
    BoundaryGraphAuditReason reason,
    main::ScopeRegion* target_scope,
    usz root_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz root_scope_gen = 0,
    usz violating_scope_gen = 0
) @inline {
    BoundaryGraphAuditResult result = {
        .ok = false,
        .reason = reason,
        .root_tag = root_tag,
        .violating_edge_tag = violating_edge_tag,
        .root_scope_gen = root_scope_gen,
        .violating_scope_gen = violating_scope_gen,
        .target_scope_gen = target_scope != null ? (usz)target_scope.generation : 0,
        .target_escape_gen = target_scope != null ? (usz)target_scope.escape_generation : 0
    };
    return result;
}

fn BoundaryGraphAuditResult boundary_graph_audit_result_emit(
    BoundaryGraphAuditReason reason,
    main::ScopeRegion* target_scope,
    usz root_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz root_scope_gen = 0,
    usz violating_scope_gen = 0
) @noinline {
    BoundaryGraphAuditResult result = boundary_graph_audit_result_hot(
        reason,
        target_scope,
        root_tag,
        violating_edge_tag,
        root_scope_gen,
        violating_scope_gen
    );
    boundary_graph_audit_log_reason(result);
    return result;
}

fn BoundaryGraphAuditResult boundary_graph_audit_result(
    bool emit,
    BoundaryGraphAuditReason reason,
    main::ScopeRegion* target_scope,
    usz root_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
    usz root_scope_gen = 0,
    usz violating_scope_gen = 0
) @inline {
    if (emit) {
        return boundary_graph_audit_result_emit(
            reason,
            target_scope,
            root_tag,
            violating_edge_tag,
            root_scope_gen,
            violating_scope_gen
        );
    }
    return boundary_graph_audit_result_hot(
        reason,
        target_scope,
        root_tag,
        violating_edge_tag,
        root_scope_gen,
        violating_scope_gen
    );
}
