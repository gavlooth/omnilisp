module lisp;

import main;
import std::io;

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
