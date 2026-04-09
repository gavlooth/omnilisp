module lisp;

import std::core::mem;

fn Value* deduce_refresh_apply_incremental_copy_pending_insertions(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    SymbolId* source_predicate,
    bool* applied_incremental_out,
    Interp* interp
) {
    if (applied_incremental_out != null) *applied_incremental_out = false;
    if (!deduce_relation_schema_incremental_extensional_copy_source(db, target_schema, source_predicate) ||
        *source_predicate == (SymbolId)0) {
        *source_predicate = (SymbolId)0;
        if (!deduce_relation_schema_incremental_derived_copy_source(db, target_schema, source_predicate) ||
            *source_predicate == (SymbolId)0) {
            return null;
        }
        DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, *source_predicate);
        if (source_schema == null || deduce_relation_schema_is_materialized_stale(db, source_schema)) {
            return null;
        }
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    MdbDbi target_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, rel.name, &target_dbi, &head_schema, interp) || head_schema == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: materialized relation storage is unavailable for incremental refresh");
    }

    bool[] inserted_mask = mem::new_array(bool, target_schema.maintained_pending_insertions.count);
    if (inserted_mask.ptr == null && target_schema.maintained_pending_insertions.count > 0) {
        return deduce_raise(interp, "deduce/refresh-out-of-memory", "deduce/refresh!: out of memory");
    }
    defer if (inserted_mask.ptr != null) mem::free(inserted_mask.ptr);

    ulong inserted_count = 0;
    for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
        DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
        if (tuple == null || (tuple.key_bytes == null && tuple.key_len > 0)) {
            return deduce_raise(interp, "deduce/refresh-invalid-state",
                "deduce/refresh!: invalid pending incremental tuple state");
        }

        Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
            db,
            head_schema,
            txn,
            target_dbi,
            tuple.key_bytes,
            tuple.key_len,
            interp
        );
        if (integrity_err != null) return integrity_err;

        char empty_key = 0;
        MdbVal key = {
            .mv_size = tuple.key_len,
            .mv_data = tuple.key_len > 0 ? (void*)tuple.key_bytes : &empty_key
        };
        MdbVal existing = {};
        if (mdb_get(txn, target_dbi, &key, &existing) == MDB_SUCCESS) continue;

        MdbVal data = {};
        char empty = 0;
        data.mv_size = 0;
        data.mv_data = &empty;

        rc = mdb_put(txn, target_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/refresh-put-failed",
                "deduce/refresh!: failed to insert incrementally maintained tuple");
        }
        inserted_mask[i] = true;
        if (inserted_count < ulong.max) inserted_count++;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn commit failed");
    }
    txn = null;

    if (inserted_count > 0 && head_schema.cardinality_estimate < ulong.max) {
        ulong new_cardinality = head_schema.cardinality_estimate;
        ulong available = ulong.max - new_cardinality;
        ulong delta = inserted_count < available ? inserted_count : available;
        new_cardinality += delta;
        head_schema.cardinality_estimate = new_cardinality;
        if (head_schema.distinct_estimate < new_cardinality) {
            head_schema.distinct_estimate = new_cardinality;
        }
    }

    bool queued_downstream = true;
    if (inserted_count > 0) {
        for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
            if (inserted_mask.ptr != null && !inserted_mask[i]) continue;
            DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
            if (tuple == null) continue;
            if (!deduce_db_queue_supported_incremental_insert_from_materialized_source(
                    db,
                    rel.name,
                    tuple.key_bytes,
                    tuple.key_len
                )) {
                queued_downstream = false;
                break;
            }
        }
    }
    if (applied_incremental_out != null) *applied_incremental_out = inserted_count > 0;
    deduce_relation_schema_clear_pending_incremental_insertions(target_schema);
    if (!queued_downstream) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
        Value* persist_err = deduce_db_persist_materialized_view_states(db, interp);
        if (persist_err != null) return persist_err;
    }
    return null;
}
