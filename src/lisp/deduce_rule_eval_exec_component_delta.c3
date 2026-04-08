module lisp;

fn Value* deduce_apply_serialized_component_delta_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    MdbTxn* txn,
    DeduceSerializedComponentDeltaPayload* payload,
    long* applied_out,
    bool* changed_out,
    Interp* interp
) {
    if (applied_out != null) *applied_out = 0;
    if (changed_out != null) *changed_out = false;
    if (db == null || plan == null || txn == null || payload == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta apply state");
    }
    if (payload.component_id >= plan.component_count ||
        plan.predicate_component.ptr == null ||
        plan.predicates.ptr == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta apply plan");
    }

    for (usz entry_idx = 0; entry_idx < payload.entry_count; entry_idx++) {
        DeduceSerializedComponentDeltaEntry* entry = &payload.entries[entry_idx];
        usz predicate_index = entry.predicate_index;
        if (predicate_index >= plan.predicates.len || predicate_index >= plan.predicate_component.len) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: invalid component delta predicate index");
        }
        int pred_component = plan.predicate_component[predicate_index];
        if (pred_component < 0 ||
            (usz)pred_component != payload.component_id ||
            plan.predicates[predicate_index] != entry.predicate) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: invalid component delta predicate routing");
        }

        MdbDbi head_dbi = 0;
        DeduceRelationSchema* head_schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, entry.predicate, &head_dbi, &head_schema, interp) ||
            head_schema == null) {
            return deduce_raise(interp, "deduce/relation-not-found",
                "deduce/parallel: component delta target relation is not registered");
        }

        for (usz tuple_idx = 0; tuple_idx < entry.delta_set.additions.count; tuple_idx++) {
            DeduceEncodedTuple* tuple = &entry.delta_set.additions.tuples[tuple_idx];
            Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
                db,
                head_schema,
                txn,
                head_dbi,
                tuple.key_bytes,
                tuple.key_len,
                interp
            );
            if (integrity_err != null) return integrity_err;

            char empty_key = 0;
            MdbVal key = {
                .mv_size = tuple.key_len,
                .mv_data = tuple.key_len > 0 ? tuple.key_bytes : &empty_key
            };
            MdbVal existing = {};
            if (mdb_get(txn, head_dbi, &key, &existing) == MDB_SUCCESS) continue;

            MdbVal data = {};
            char empty = 0;
            data.mv_size = 0;
            data.mv_data = &empty;
            int put_rc = mdb_put(txn, head_dbi, &key, &data, 0);
            if (put_rc != MDB_SUCCESS) {
                return deduce_raise(interp, "deduce/analyze-put-failed",
                    "deduce/parallel: failed to publish worker-computed derived tuple");
            }

            if (applied_out != null && *applied_out < long.max) *applied_out = *applied_out + 1;
            if (changed_out != null) *changed_out = true;
            if (head_schema.cardinality_estimate < ulong.max) {
                head_schema.cardinality_estimate++;
                if (head_schema.distinct_estimate < head_schema.cardinality_estimate) {
                    head_schema.distinct_estimate = head_schema.cardinality_estimate;
                }
            }
        }

        for (usz tuple_idx = 0; tuple_idx < entry.delta_set.removals.count; tuple_idx++) {
            DeduceEncodedTuple* tuple = &entry.delta_set.removals.tuples[tuple_idx];
            char empty_key = 0;
            MdbVal key = {
                .mv_size = tuple.key_len,
                .mv_data = tuple.key_len > 0 ? tuple.key_bytes : &empty_key
            };
            MdbVal existing = {};
            if (mdb_get(txn, head_dbi, &key, &existing) != MDB_SUCCESS) continue;

            int del_rc = mdb_del(txn, head_dbi, &key, null);
            if (del_rc != MDB_SUCCESS && del_rc != MDB_NOTFOUND) {
                return deduce_raise(interp, "deduce/analyze-put-failed",
                    "deduce/parallel: failed to retract worker-computed derived tuple");
            }
            if (del_rc == MDB_NOTFOUND) continue;

            if (applied_out != null && *applied_out < long.max) *applied_out = *applied_out + 1;
            if (changed_out != null) *changed_out = true;
            if (head_schema.cardinality_estimate > 0) {
                head_schema.cardinality_estimate--;
                head_schema.distinct_estimate = head_schema.cardinality_estimate;
            }
        }
    }

    return null;
}
