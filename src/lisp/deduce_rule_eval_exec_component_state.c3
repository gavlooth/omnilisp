module lisp;

fn Value* deduce_seminaive_seed_component_delta_sets(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] delta_sets,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed state");
    }
    if (delta_sets.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed table");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbCursor* cursor = null;
        rc = mdb_cursor_open(txn, dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
        }

        MdbVal key = {};
        MdbVal data = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            Value* err = deduce_delta_set_insert(&delta_sets[pred_idx].additions, key.mv_data, key.mv_size, interp);
            if (err != null) {
                mdb_cursor_close(cursor);
                return err;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        mdb_cursor_close(cursor);
    }
    return null;
}

fn Value* deduce_seminaive_seed_component_support_tables_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceTupleSupportTable[] support_tables,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed state");
    }
    if (support_tables.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed table");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbCursor* cursor = null;
        int rc = mdb_cursor_open(txn, dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
        }

        MdbVal key = {};
        MdbVal data = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            Value* err =
                deduce_tuple_support_table_seed_base_tuple(&support_tables[pred_idx], key.mv_data, key.mv_size, null, interp);
            if (err != null) {
                mdb_cursor_close(cursor);
                return err;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        mdb_cursor_close(cursor);
    }
    return null;
}

fn Value* deduce_seminaive_seed_component_extensional_state_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] delta_sets,
    DeduceTupleSupportTable[] support_tables,
    bool restore_main_relations,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid extensional seed state");
    }

    bool seed_deltas = delta_sets.len >= plan.predicates.len;
    bool seed_support = support_tables.len >= plan.predicates.len;
    char empty = 0;
    MdbVal data = { .mv_size = 0, .mv_data = &empty };

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi main_dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &main_dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbDbi extensional_dbi = 0;
        if (!deduce_open_extensional_dbi_for_predicate(db, predicate, txn, false, &extensional_dbi, interp)) {
            if (schema != null && restore_main_relations) {
                schema.cardinality_estimate = 0;
                schema.distinct_estimate = 0;
            }
            continue;
        }

        MdbCursor* cursor = null;
        int rc = mdb_cursor_open(txn, extensional_dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open extensional scan cursor");
        }
        defer mdb_cursor_close(cursor);

        long restored_count = 0;
        MdbVal key = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            if (seed_support) {
                Value* err =
                    deduce_tuple_support_table_seed_base_tuple(&support_tables[pred_idx], key.mv_data, key.mv_size, null, interp);
                if (err != null) return err;
            }
            if (seed_deltas) {
                Value* err = deduce_delta_set_insert(&delta_sets[pred_idx].additions, key.mv_data, key.mv_size, interp);
                if (err != null) return err;
            }
            if (restore_main_relations) {
                int put_rc = mdb_put(txn, main_dbi, &key, &data, 0);
                if (put_rc != MDB_SUCCESS) {
                    return deduce_raise(interp, "deduce/analyze-put-failed",
                        "deduce/analyze: failed to restore extensional tuple into recursive relation");
                }
            }
            if (restored_count < long.max) restored_count++;
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }

        if (schema != null && restore_main_relations) {
            ulong count = restored_count > 0 ? (ulong)restored_count : 0;
            schema.cardinality_estimate = count;
            schema.distinct_estimate = count;
        }
        mdb_cursor_close(cursor);
        cursor = null;
    }
    return null;
}

fn usz deduce_seminaive_collect_anchor_steps(
    DeduceRuleSignature* sig,
    DeduceSccPlan* plan,
    usz component_id,
    usz[] anchor_steps
) {
    if (sig == null || plan == null) return 0;
    usz count = 0;
    for (usz i = 0; i < sig.body_count; i++) {
        if (sig.body_negated != null && sig.body_negated[i]) continue;
        if (sig.body_predicates == null) continue;
        SymbolId predicate = sig.body_predicates[i];
        if (!deduce_scc_predicate_in_component(plan, predicate, component_id)) continue;
        if (count < anchor_steps.len) anchor_steps[count] = i;
        count++;
    }
    return count;
}

fn Value* deduce_analyze_iteration_limit_payload(
    Interp* interp,
    usz component_id,
    usz iteration_limit
) {
    Value* payload = make_hashmap_no_raise(interp, 8);
    if (payload == null || payload.tag != HASHMAP || payload.hashmap_val == null) return null;
    SymbolId component_key = interp.symbols.intern("component-id");
    SymbolId iteration_key = interp.symbols.intern("iteration-limit");
    if (component_key == INVALID_SYMBOL_ID || iteration_key == INVALID_SYMBOL_ID) return null;
    if (!hashmap_set_symbol_checked(payload.hashmap_val, component_key, make_int(interp, (long)component_id), interp) ||
        !hashmap_set_symbol_checked(payload.hashmap_val, iteration_key, make_int(interp, (long)iteration_limit), interp)) {
        return null;
    }
    return payload;
}

fn Value* deduce_seminaive_seed_component_support_tables(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceTupleSupportTable[] support_tables,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    return deduce_seminaive_seed_component_support_tables_in_txn(
        db,
        plan,
        component_id,
        txn,
        support_tables,
        interp
    );
}
