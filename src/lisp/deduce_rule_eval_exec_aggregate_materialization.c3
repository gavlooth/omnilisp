module lisp;

fn Value* deduce_naive_finalize_aggregate_groups(DeduceNaiveRuleExec* exec) {
    if (exec == null || exec.sig == null || exec.interp == null || exec.aggregate_groups == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }
    if (exec.dry_run) return null;
    if (exec.sig.head_term_kinds == null || exec.sig.head_term_aggregate_ops == null ||
        exec.sig.head_term_literals == null || exec.aggregate_groups.entries == null && exec.aggregate_groups.count > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate finalize metadata");
    }

    DeduceEncodedTuple* tuples = null;
    usz tuple_count = 0;
    Value* err = deduce_naive_materialize_aggregate_groups(exec, &tuples, &tuple_count);
    if (err != null) return err;
    defer deduce_encoded_tuples_release(tuples, tuple_count);

    long current_count = 0;
    err = deduce_relation_count_rows(exec.txn, exec.head_dbi, &current_count, exec.interp);
    if (err != null) return err;

    long retained_count = 0;
    for (usz i = 0; i < tuple_count; i++) {
        char empty_key = 0;
        MdbVal key = { .mv_size = tuples[i].key_len, .mv_data = tuples[i].key_len > 0 ? tuples[i].key_bytes : &empty_key };
        MdbVal existing = {};
        if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
            if (retained_count < long.max) retained_count++;
        }
    }

    bool changed = current_count != (long)tuple_count || retained_count != (long)tuple_count;
    if (!changed) {
        if (exec.head_schema != null) {
            exec.head_schema.cardinality_estimate = (ulong)tuple_count;
            exec.head_schema.distinct_estimate = (ulong)tuple_count;
        }
        return null;
    }

    int rc = mdb_drop(exec.txn, exec.head_dbi, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to reset aggregate head relation");
    }
    if (exec.head_schema != null) {
        exec.head_schema.cardinality_estimate = 0;
        exec.head_schema.distinct_estimate = 0;
    }

    for (usz i = 0; i < tuple_count; i++) {
        err = deduce_naive_insert_encoded_head_tuple(exec, tuples[i].key_bytes, tuples[i].key_len);
        if (err != null) return err;
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) {
        long changed_rows = current_count + (long)tuple_count - 2 * retained_count;
        if (changed_rows < 0) changed_rows = 0;
        if (*exec.derived_out <= long.max - changed_rows) {
            *exec.derived_out = *exec.derived_out + changed_rows;
        } else {
            *exec.derived_out = long.max;
        }
    }
    return null;
}
