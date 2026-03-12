module lisp;

import main;
import std::io;

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

const usz BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT = (usz)ValueTag.TIME_POINT + 1;
const usz BOUNDARY_GRAPH_AUDIT_EDGE_TAG_ENV = BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT;
const usz BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE = BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT + 1;

enum BoundaryGraphEdgeClass : char {
    BOUNDARY_GRAPH_EDGE_NONE,
    BOUNDARY_GRAPH_EDGE_VALUE,
    BOUNDARY_GRAPH_EDGE_ENV,
}

const BoundaryGraphEdgeClass[] BOUNDARY_GRAPH_AUDIT_TAG_EDGE_CLASS = {
    BOUNDARY_GRAPH_EDGE_NONE,   // NIL
    BOUNDARY_GRAPH_EDGE_NONE,   // INT
    BOUNDARY_GRAPH_EDGE_NONE,   // STRING
    BOUNDARY_GRAPH_EDGE_NONE,   // SYMBOL
    BOUNDARY_GRAPH_EDGE_VALUE,  // CONS
    BOUNDARY_GRAPH_EDGE_ENV,    // CLOSURE
    BOUNDARY_GRAPH_EDGE_NONE,   // CONTINUATION
    BOUNDARY_GRAPH_EDGE_NONE,   // PRIMITIVE
    BOUNDARY_GRAPH_EDGE_VALUE,  // PARTIAL_PRIM
    BOUNDARY_GRAPH_EDGE_NONE,   // ERROR
    BOUNDARY_GRAPH_EDGE_NONE,   // DOUBLE
    BOUNDARY_GRAPH_EDGE_VALUE,  // HASHMAP
    BOUNDARY_GRAPH_EDGE_VALUE,  // SET
    BOUNDARY_GRAPH_EDGE_NONE,   // FFI_HANDLE
    BOUNDARY_GRAPH_EDGE_VALUE,  // ARRAY
    BOUNDARY_GRAPH_EDGE_NONE,   // TYPE_INFO
    BOUNDARY_GRAPH_EDGE_VALUE,  // INSTANCE
    BOUNDARY_GRAPH_EDGE_VALUE,  // METHOD_TABLE
    BOUNDARY_GRAPH_EDGE_ENV,    // MODULE
    BOUNDARY_GRAPH_EDGE_VALUE,  // ITERATOR
    BOUNDARY_GRAPH_EDGE_NONE,   // COROUTINE
    BOUNDARY_GRAPH_EDGE_NONE,   // TIME_POINT
};

$assert(BOUNDARY_GRAPH_AUDIT_TAG_EDGE_CLASS.len == BOUNDARY_GRAPH_AUDIT_VALUE_TAG_COUNT);

fn BoundaryGraphEdgeClass boundary_graph_audit_edge_class(ValueTag tag) @inline {
    usz idx = (usz)tag;
    if (idx >= BOUNDARY_GRAPH_AUDIT_TAG_EDGE_CLASS.len) return BOUNDARY_GRAPH_EDGE_NONE;
    return BOUNDARY_GRAPH_AUDIT_TAG_EDGE_CLASS[idx];
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

fn void boundary_graph_audit_log_reason(BoundaryGraphAuditResult result) @inline {
    switch (result.reason) {
        case BOUNDARY_GRAPH_AUDIT_OK:
            return;
        case BOUNDARY_GRAPH_AUDIT_INVALID_TARGET:
            io::eprintfn("[boundary][graph-audit] invalid target scope for reachability audit");
            return;
        case BOUNDARY_GRAPH_AUDIT_ALLOCATION_FAILED:
            io::eprintfn(
                "[boundary][graph-audit] allocation failure during reachability audit target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_SEED_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] seed overflow while queuing root target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_VALUE_SET_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] value visit overflow while validating escape reachability target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_ENV_SET_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] env visit overflow while validating escape reachability target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] value push overflow while traversing graph target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] env push overflow while traversing graph target_escape_gen=%d",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE:
            io::eprintfn(
                "[boundary][graph-audit] violation: reachable Omni edge enters TEMP root_tag=%d violating_edge_tag=%d root_scope_gen=%d violating_scope_gen=%d target_scope_gen=%d target_escape_gen=%d",
                (long)result.root_tag,
                (long)result.violating_edge_tag,
                (long)result.root_scope_gen,
                (long)result.violating_scope_gen,
                (long)result.target_scope_gen,
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV:
            io::eprintfn(
                "[boundary][graph-audit] violation: reachable Omni edge enters TEMP root_tag=%d violating_edge_tag=%d root_scope_gen=%d violating_scope_gen=%d target_scope_gen=%d target_escape_gen=%d",
                (long)result.root_tag,
                (long)result.violating_edge_tag,
                (long)result.root_scope_gen,
                (long)result.violating_scope_gen,
                (long)result.target_scope_gen,
                (long)result.target_escape_gen
            );
            return;
        default:
            io::eprintfn(
                "[boundary][graph-audit] unknown failure reason=%d target_escape_gen=%d",
                (int)result.reason,
                (long)result.target_escape_gen
            );
        return;
    }
}

fn void boundary_graph_audit_log_commit_context(
    BoundaryCommitEscapeResult committed,
    BoundaryGraphAuditResult audit
) @inline {
    io::eprintfn(
        "[boundary][graph-audit] commit-context outcome=%s commit_value_tag=%d commit_scope_gen=%d target_scope_gen=%d target_escape_gen=%d reason=%s root_tag=%d violating_edge_tag=%d violating_scope_gen=%d",
        (ZString)boundary_commit_outcome_name(committed.outcome),
        (long)committed.value.tag,
        (long)committed.value.scope_gen,
        (long)audit.target_scope_gen,
        (long)audit.target_escape_gen,
        (ZString)boundary_graph_audit_reason_name(audit.reason),
        (long)audit.root_tag,
        (long)audit.violating_edge_tag,
        (long)audit.violating_scope_gen
    );
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

fn bool boundary_value_points_into_temp(Value* v, main::ScopeRegion* target_scope) @inline {
    if (v == null || target_scope == null) return false;
    for (main::ScopeRegion* s = target_scope; s != null; s = s.parent) {
        if (boundary_ptr_in_scope((void*)v, s) && v.scope_gen == s.generation) {
            return true;
        }
    }
    return false;
}

fn bool boundary_env_points_into_temp(Env* env, main::ScopeRegion* target_scope) @inline {
    if (env == null || target_scope == null) return false;
    for (main::ScopeRegion* s = target_scope; s != null; s = s.parent) {
        if (boundary_ptr_in_scope((void*)env, s) && env.scope_gen == s.generation) {
            return true;
        }
    }
    return false;
}
