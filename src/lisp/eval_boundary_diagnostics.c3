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

fn BoundaryGraphAuditResult boundary_graph_audit_escape_reachability_result(
    Value* root,
    main::ScopeRegion* target_scope,
    bool emit = false
) @noinline {
    if (root == null || target_scope == null) {
        return {
            .ok = true,
            .reason = BOUNDARY_GRAPH_AUDIT_OK,
            .root_tag = root != null ? (usz)root.tag : BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .root_scope_gen = root != null ? (usz)root.scope_gen : 0,
            .violating_scope_gen = 0,
            .target_scope_gen = target_scope != null ? (usz)target_scope.generation : 0,
            .target_escape_gen = target_scope != null ? (usz)target_scope.escape_generation : 0
        };
    }

    usz root_tag = (usz)root.tag;
    usz root_scope_gen = (usz)root.scope_gen;

    if (emit && !boundary_graph_audit_enabled()) {
        return {
            .ok = true,
            .reason = BOUNDARY_GRAPH_AUDIT_OK,
            .root_tag = root_tag,
            .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .root_scope_gen = root_scope_gen,
            .violating_scope_gen = 0,
            .target_scope_gen = (usz)target_scope.generation,
            .target_escape_gen = (usz)target_scope.escape_generation
        };
    }

    Value** value_stack = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Value** seen_values = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** env_stack = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** seen_envs = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    if (value_stack == null || seen_values == null || env_stack == null || seen_envs == null) {
        if (value_stack != null) mem::free(value_stack);
        if (seen_values != null) mem::free(seen_values);
        if (env_stack != null) mem::free(env_stack);
        if (seen_envs != null) mem::free(seen_envs);
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_ALLOCATION_FAILED,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            root_scope_gen,
            0
        );
    }
    defer {
        mem::free(value_stack);
        mem::free(seen_values);
        mem::free(env_stack);
        mem::free(seen_envs);
    }

    usz value_stack_len = 0;
    usz seen_value_len = 0;
    usz env_stack_len = 0;
    usz seen_env_len = 0;

    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, root)) {
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_SEED_OVERFLOW,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            root_scope_gen,
            0
        );
    }

    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = value_stack[--value_stack_len];
            if (cur == null) continue;
            if (boundary_graph_audit_value_seen(seen_values, seen_value_len, cur)) continue;
            if (seen_value_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_VALUE_SET_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            seen_values[seen_value_len++] = cur;

            if (boundary_value_points_into_temp(cur, target_scope)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE,
                    target_scope,
                    root_tag,
                    (usz)cur.tag,
                    root_scope_gen,
                    (usz)cur.scope_gen
                );
            }

            // Explicit traversed edge set for reachability audit:
            // - CONS: car/cdr
            // - ARRAY: items[i]
            // - HASHMAP: entry.key / entry.value
            // - CLOSURE: captured env + env parent chain bindings
            // - PARTIAL_PRIM: first_arg / second_arg
            // - METHOD_TABLE: entry implementations + fallback
            // - MODULE: module env + env parent chain bindings
            // - ITERATOR: iterator thunk value
            // - INSTANCE: instance fields (Value slots)
            // Opaque wrappers intentionally excluded:
            // - FFI_HANDLE, COROUTINE, CONTINUATION, PRIMITIVE, TYPE_INFO
            // and any other non-Omni payload that does not own Value edges.
            BoundaryGraphEdgeClass edge_class = boundary_graph_audit_edge_class(cur.tag);
            switch (cur.tag) {
                case CONS:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: CONS edge class mismatch");
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.cons_val.car) ||
                        !boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.cons_val.cdr)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                    break;
                case ARRAY:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: ARRAY edge class mismatch");
                    if (cur.array_val != null) {
                        for (usz i = 0; i < cur.array_val.length; i++) {
                            if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.array_val.items[i])) {
                                return boundary_graph_audit_result(
                                    emit,
                                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                    target_scope,
                                    root_tag,
                                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                    root_scope_gen,
                                    0
                                );
                            }
                        }
                    }
                    break;
                case HASHMAP:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: HASHMAP edge class mismatch");
                    if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                        for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                            Value* key = cur.hashmap_val.entries[i].key;
                            if (key == null) continue;
                            Value* val = cur.hashmap_val.entries[i].value;
                            if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, key) ||
                                !boundary_graph_audit_push_value(value_stack, &value_stack_len, val)) {
                                return boundary_graph_audit_result(
                                    emit,
                                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                    target_scope,
                                    root_tag,
                                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                    root_scope_gen,
                                    0
                                );
                            }
                        }
                    }
                    break;
                case CLOSURE:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_ENV, "boundary graph audit: CLOSURE edge class mismatch");
                    if (cur.closure_val != null &&
                        !boundary_graph_audit_push_env(env_stack, &env_stack_len, cur.closure_val.env)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                    break;
                case PARTIAL_PRIM:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: PARTIAL_PRIM edge class mismatch");
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.partial_val.first_arg) ||
                        !boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.partial_val.second_arg)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                    break;
                case METHOD_TABLE:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: METHOD_TABLE edge class mismatch");
                    if (cur.method_table_val != null) {
                        MethodTable* mt = cur.method_table_val;
                        for (usz i = 0; i < mt.entry_count; i++) {
                            if (!boundary_graph_audit_push_value(
                                    value_stack,
                                    &value_stack_len,
                                    mt.entries[i].implementation
                            )) {
                                return boundary_graph_audit_result(
                                    emit,
                                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                    target_scope,
                                    root_tag,
                                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                    root_scope_gen,
                                    0
                                );
                            }
                        }
                        if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, mt.fallback)) {
                            return boundary_graph_audit_result(
                                emit,
                                BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                target_scope,
                                root_tag,
                                BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                root_scope_gen,
                                0
                            );
                        }
                    }
                    break;
                case MODULE:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_ENV, "boundary graph audit: MODULE edge class mismatch");
                    if (cur.module_val != null &&
                        !boundary_graph_audit_push_env(env_stack, &env_stack_len, cur.module_val.env)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                    break;
                case ITERATOR:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: ITERATOR edge class mismatch");
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.iterator_val)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                    break;
                case INSTANCE:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: INSTANCE edge class mismatch");
                    if (cur.instance_val != null && cur.instance_val.fields != null) {
                        for (usz i = 0; i < cur.instance_val.field_count; i++) {
                            if (!boundary_graph_audit_push_value(
                                    value_stack,
                                    &value_stack_len,
                                    &cur.instance_val.fields[i]
                            )) {
                                return boundary_graph_audit_result(
                                    emit,
                                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                    target_scope,
                                    root_tag,
                                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                    root_scope_gen,
                                    0
                                );
                            }
                        }
                    }
                    break;
                case NIL:
                case INT:
                case STRING:
                case SYMBOL:
                case CONTINUATION:
                case PRIMITIVE:
                case ERROR:
                case DOUBLE:
                case FFI_HANDLE:
                case TYPE_INFO:
                case COROUTINE:
                case TIME_POINT:
                    assert(edge_class == BOUNDARY_GRAPH_EDGE_NONE, "boundary graph audit: leaf edge class mismatch");
                    break;
            }
        }

        while (env_stack_len > 0) {
            Env* env = env_stack[--env_stack_len];
            if (env == null) continue;
            if (boundary_graph_audit_env_seen(seen_envs, seen_env_len, env)) continue;
            if (seen_env_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_ENV_SET_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            seen_envs[seen_env_len++] = env;

            if (boundary_env_points_into_temp(env, target_scope)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_ENV,
                    root_scope_gen,
                    (usz)env.scope_gen
                );
            }

            if (!boundary_graph_audit_push_env(env_stack, &env_stack_len, env.parent)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }

            for (usz i = 0; i < env.binding_count; i++) {
                if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, env.bindings[i].value)) {
                    return boundary_graph_audit_result(
                        emit,
                        BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                        target_scope,
                        root_tag,
                        BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                        root_scope_gen,
                        0
                    );
                }
            }
        }
    }

    return {
        .ok = true,
        .reason = BOUNDARY_GRAPH_AUDIT_OK,
        .root_tag = root_tag,
        .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
        .root_scope_gen = root_scope_gen,
        .violating_scope_gen = 0,
        .target_scope_gen = (usz)target_scope.generation,
        .target_escape_gen = (usz)target_scope.escape_generation
    };
}

fn bool boundary_graph_audit_escape_reachability(Value* root, main::ScopeRegion* target_scope) @noinline {
    return boundary_graph_audit_escape_reachability_result(root, target_scope).ok;
}

fn void boundary_debug_graph_audit_committed_escape_root(
    BoundaryCommitEscapeResult committed,
    main::ScopeRegion* target_scope
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    if (committed.value == null || target_scope == null) return;
    if (committed.value.scope_gen != target_scope.escape_generation) return;

    boundary_telemetry_inc_graph_audit_invoked();

    usz rate = boundary_graph_audit_rate();
    usz max_roots = boundary_graph_audit_max_roots();
    usz audit_index = g_boundary_decision_stats.graph_audit_invoked;

    if (rate == 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return;
    }

    if (max_roots > 0 && audit_index > max_roots) {
        boundary_telemetry_inc_graph_audit_skipped_max_roots();
        return;
    }

    if (rate > 1 && (audit_index - 1) % rate != 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return;
    }

    BoundaryGraphAuditResult audit = boundary_graph_audit_escape_reachability_result(committed.value, target_scope, true);
    if (!audit.ok) {
        boundary_graph_audit_log_reason(audit);
        boundary_graph_audit_log_commit_context(committed, audit);
        BoundaryGraphAuditReason reason = audit.reason;
        io::eprintfn(
            "[boundary][graph-audit] committed escape root reachability violation outcome=%d reason=%s root_tag=%d violating_edge_tag=%d",
            (int)committed.outcome,
            (ZString)boundary_graph_audit_reason_name(reason),
            (long)audit.root_tag,
            (long)audit.violating_edge_tag
        );
    }
}

fn bool boundary_verbose_telemetry_enabled() @inline {
    return boundary_policy_verbose_telemetry_enabled();
}

fn void boundary_dump_decision_stats_verbose(char[] label = "runtime") @noinline {
    if (!boundary_verbose_telemetry_enabled()) return;
    boundary_policy_emit_snapshot_once(label);
    BoundaryDecisionStats stats = boundary_decision_stats_snapshot();

    io::eprintfn(
        "[boundary][telemetry][%s] splice_attempted=%d splice_succeeded=%d promotion_attempted=%d copy_fallback_total=%d scope_chain_scan_total=%d scope_chain_scan_with_hint=%d scope_chain_scan_fallback=%d scope_chain_scan_suppressed=%d graph_audit_invoked=%d graph_audit_skipped_rate=%d graph_audit_skipped_max_roots=%d",
        (ZString)label,
        (long)stats.splice_attempted,
        (long)stats.splice_succeeded,
        (long)stats.promotion_attempted,
        (long)stats.copy_fallback_total,
        (long)stats.scope_chain_scan_total,
        (long)stats.scope_chain_scan_with_hint,
        (long)stats.scope_chain_scan_fallback,
        (long)stats.scope_chain_scan_suppressed,
        (long)stats.graph_audit_invoked,
        (long)stats.graph_audit_skipped_rate,
        (long)stats.graph_audit_skipped_max_roots
    );

    for (usz i = 0; i < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] splice_fail reason=%s count=%d",
            (ZString)label,
            boundary_scope_transfer_reason_name((BoundaryScopeTransferReason)i),
            (long)stats.splice_failed_by_reason[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_PROMOTION_ABORT_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] promotion_abort reason=%s count=%d",
            (ZString)label,
            boundary_promotion_abort_reason_name((BoundaryPromotionAbortReason)i),
            (long)stats.promotion_aborted_by_reason[i]
        );
    }
}

fn void boundary_debug_graph_audit_report_scope_transfer(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    bool parent_non_null = parent != null;
    bool child_non_null = child != null;
    bool same_scope = parent_non_null && child_non_null && parent == child;
    bool immediate_child = parent_non_null && child_non_null && child.parent == parent;
    bool refcount_one = child_non_null && child.refcount == 1;
    bool owner_thread_match = parent_non_null &&
        child_non_null &&
        parent.owner_thread_token != null &&
        child.owner_thread_token != null &&
        parent.owner_thread_token == child.owner_thread_token;
    bool parent_escape_lane_valid = parent_non_null &&
        boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks);
    bool parent_escape_chunks_non_null = parent_non_null && parent.escape_chunks != null;
    bool parent_escape_bump_non_null = parent_non_null && parent.escape_bump != null;
    bool parent_escape_limit_non_null = parent_non_null && parent.escape_limit != null;
    bool child_temp_lane_valid = child_non_null &&
        boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks);
    bool child_escape_lane_valid = child_non_null &&
        boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks);

    io::eprintfn(
        "[boundary][graph-audit] splice-reject reason=%s parent_non_null=%d child_non_null=%d same_scope=%d immediate_child=%d refcount_one=%d owner_thread_match=%d parent_escape_lane_valid=%d parent_escape_chunks_non_null=%d parent_escape_bump_non_null=%d parent_escape_limit_non_null=%d child_temp_lane_valid=%d child_escape_lane_valid=%d",
        boundary_scope_transfer_reason_name(reason),
        parent_non_null ? 1 : 0,
        child_non_null ? 1 : 0,
        same_scope ? 1 : 0,
        immediate_child ? 1 : 0,
        refcount_one ? 1 : 0,
        owner_thread_match ? 1 : 0,
        parent_escape_lane_valid ? 1 : 0,
        parent_escape_chunks_non_null ? 1 : 0,
        parent_escape_bump_non_null ? 1 : 0,
        parent_escape_limit_non_null ? 1 : 0,
        child_temp_lane_valid ? 1 : 0,
        child_escape_lane_valid ? 1 : 0
    );
}

fn BoundaryScopeTransferResult boundary_scope_transfer_reject(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @inline {
    if (boundary_graph_audit_enabled()) {
        boundary_debug_graph_audit_report_scope_transfer(parent, child, reason);
    }
    return { .allowed = false, .reason = reason };
}

fn BoundaryScopeTransferResult boundary_check_scope_transfer(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    if (parent == null) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NULL_PARENT);
    }
    if (child == null) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NULL_CHILD);
    }
    if (parent == child) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE);
    }
    if (child.parent != parent) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD);
    }
    if (child.refcount != 1) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE);
    }
    if (parent.owner_thread_token == null ||
        child.owner_thread_token == null ||
        parent.owner_thread_token != child.owner_thread_token) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH);
    }
    if (!boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID);
    }
    if (!boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID);
    }
    if (!boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID);
    }
    return boundary_scope_transfer_ok();
}

fn void boundary_invariant_fail(char[] domain, char[] msg) @noinline {
    io::eprintfn(
        "[boundary][invariant][%s] %s",
        (ZString)domain,
        (ZString)msg
    );
    unreachable("boundary invariant failure");
}

macro boundary_invariant_require(cond, domain, msg) {
    if (!(cond)) {
        boundary_invariant_fail(domain, msg);
    }
}

fn void boundary_assert_interp_scope_chain(Interp* interp) @inline {
    if (interp == null) return;
    boundary_invariant_require(
        interp.root_scope != null,
        "scope-chain",
        "root_scope must not be null"
    );
    boundary_invariant_require(
        interp.current_scope != null,
        "scope-chain",
        "current_scope must not be null"
    );
}

fn void boundary_assert_saved_state(BoundaryInterpState saved) @inline {
    boundary_invariant_require(
        saved.current_scope != null,
        "saved-state",
        "saved current_scope must not be null"
    );
}

fn void boundary_assert_session_state(BoundarySession* session) @inline {
    boundary_invariant_require(
        session != null,
        "session",
        "session pointer must not be null"
    );
    boundary_invariant_require(
        session.current_scope != null,
        "session",
        "saved current_scope must not be null"
    );
}

fn void boundary_assert_scope_swap_state(BoundaryScopeSwap* swap) @inline {
    boundary_invariant_require(
        swap != null,
        "scope-swap",
        "scope swap pointer must not be null"
    );
    boundary_invariant_require(
        swap.saved_scope != null,
        "scope-swap",
        "saved scope must not be null"
    );
}
