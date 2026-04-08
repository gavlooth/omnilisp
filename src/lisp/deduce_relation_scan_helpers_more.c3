module lisp;

import std::core::mem;

fn Value* relation_scan_range(
    Relation* rel,
    Interp* interp,
    char* start_key,
    usz start_key_len,
    bool has_start,
    Value*[] lower_bound,
    Value*[] upper_bound,
    bool has_end,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);
    bool bench_enabled = deduce_scan_range_bench_stats.enabled;
    if (bench_enabled) deduce_scan_range_bench_stats.calls++;
    usz created_key_values = 0;
    Value* key_values_err = deduce_relation_ensure_column_key_values(rel, interp, &created_key_values);
    if (key_values_err != null) return key_values_err;
    if (bench_enabled) deduce_scan_range_bench_stats.key_symbols_allocated += created_key_values;

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(
        &it,
        rel,
        interp,
        start_key,
        start_key_len,
        has_start,
        lower_bound,
        upper_bound,
        has_end
    )) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* result = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        long materialize_start_ms = 0;
        if (bench_enabled) materialize_start_ms = scheduler_monotonic_ms();

        Value* dict = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        if (dict == null || dict.tag == ERROR) {
            return dict;
        }
        if (bench_enabled) {
            deduce_scan_range_bench_stats.rows_materialized++;
            long materialize_end_ms = scheduler_monotonic_ms();
            if (materialize_end_ms > materialize_start_ms) {
                deduce_scan_range_bench_stats.materialize_ms += (materialize_end_ms - materialize_start_ms);
            }
        }
        if (subject_keys_out != null) {
            deduce_goal_directed_read_subject_key_capture_record(
                subject_keys_out,
                it.key.mv_data,
                it.key.mv_size
            );
        }
        result = make_cons(interp, dict, result);
    }

    return result;
}

fn Value* relation_scan_range_in_txn(
    Relation* rel,
    Interp* interp,
    MdbTxn* txn,
    char* start_key,
    usz start_key_len,
    bool has_start,
    Value*[] lower_bound,
    Value*[] upper_bound,
    bool has_end,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || interp == null || txn == null) return make_nil(interp);
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    bool bench_enabled = deduce_scan_range_bench_stats.enabled;
    if (bench_enabled) deduce_scan_range_bench_stats.calls++;
    usz created_key_values = 0;
    Value* key_values_err = deduce_relation_ensure_column_key_values(rel, interp, &created_key_values);
    if (key_values_err != null) return key_values_err;
    if (bench_enabled) deduce_scan_range_bench_stats.key_symbols_allocated += created_key_values;

    Value*[] vals = {};
    if (rel.col_count > 0) {
        vals = mem::new_array(Value*, rel.col_count);
        if (vals.ptr == null) return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    defer if (vals.ptr != null) mem::free(vals.ptr);
    uint dict_capacity = deduce_relation_row_dict_capacity(rel);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) return make_nil(interp);
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    bool started = false;
    Value* result = make_nil(interp);
    for (;;) {
        if (!started) {
            started = true;
            if (has_start) {
                key.mv_data = start_key;
                key.mv_size = start_key_len;
                rc = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);
            } else {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
            }
        } else {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }

        while (rc == MDB_SUCCESS) {
            usz? count_opt = decode_tuple((char*)key.mv_data, key.mv_size, vals, interp);
            if (catch err = count_opt) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (count_opt != rel.col_count) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }

            if (has_start && deduce_compare_tuples(vals, lower_bound, rel.col_count) < 0) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (has_end && deduce_compare_tuples(vals, upper_bound, rel.col_count) > 0) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }

            long materialize_start_ms = 0;
            if (bench_enabled) materialize_start_ms = scheduler_monotonic_ms();

            Value* dict = deduce_relation_materialize_row_dict(rel, vals, interp, dict_capacity);
            if (dict == null || dict.tag == ERROR) {
                return dict;
            }
            if (bench_enabled) {
                deduce_scan_range_bench_stats.rows_materialized++;
                long materialize_end_ms = scheduler_monotonic_ms();
                if (materialize_end_ms > materialize_start_ms) {
                    deduce_scan_range_bench_stats.materialize_ms += (materialize_end_ms - materialize_start_ms);
                }
            }
            if (subject_keys_out != null) {
                deduce_goal_directed_read_subject_key_capture_record(
                    subject_keys_out,
                    key.mv_data,
                    key.mv_size
                );
            }
            result = make_cons(interp, dict, result);
            break;
        }

        if (rc != MDB_SUCCESS) break;
    }

    return result;
}

fn Value* relation_scan_all(
    Relation* rel,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    bool bench_enabled = deduce_scan_range_bench_stats.enabled;
    if (bench_enabled) deduce_scan_range_bench_stats.calls++;
    usz created_key_values = 0;
    Value* key_values_err = deduce_relation_ensure_column_key_values(rel, interp, &created_key_values);
    if (key_values_err != null) return key_values_err;
    if (bench_enabled) deduce_scan_range_bench_stats.key_symbols_allocated += created_key_values;

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* result = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        long materialize_start_ms = 0;
        if (bench_enabled) materialize_start_ms = scheduler_monotonic_ms();

        Value* dict = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        if (dict == null || dict.tag == ERROR) {
            return dict;
        }
        if (bench_enabled) {
            deduce_scan_range_bench_stats.rows_materialized++;
            long materialize_end_ms = scheduler_monotonic_ms();
            if (materialize_end_ms > materialize_start_ms) {
                deduce_scan_range_bench_stats.materialize_ms += (materialize_end_ms - materialize_start_ms);
            }
        }
        if (subject_keys_out != null) {
            deduce_goal_directed_read_subject_key_capture_record(
                subject_keys_out,
                it.key.mv_data,
                it.key.mv_size
            );
        }
        result = make_cons(interp, dict, result);
    }

    return result;
}
