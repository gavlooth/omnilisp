module lisp;

import std::core::mem;

bool g_deduce_force_row_dict_oom = false;
bool g_deduce_force_column_key_values_oom = false;

fn uint deduce_relation_row_dict_capacity(Relation* rel) {
    uint dict_capacity = 8;
    if (rel == null) return dict_capacity;
    uint needed_capacity = (uint)(rel.col_count * 2);
    while (dict_capacity < needed_capacity) dict_capacity *= 2;
    return dict_capacity;
}

fn Value* deduce_relation_materialize_row_dict(
    Relation* rel,
    Value*[] vals,
    Interp* interp,
    uint dict_capacity
) {
    Value* dict = make_hashmap(interp, dict_capacity);
    if (g_deduce_force_row_dict_oom || test_c_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        dict = null;
    }
    if (dict == null || dict.hashmap_val == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-scan-range: out of memory");
    }
    for (usz i = 0; i < rel.col_count && i < vals.len; i++) {
        Value* k = rel.column_key_values[i];
        hashmap_set(dict.hashmap_val, k, vals[i], interp);
    }
    return dict;
}

fn Value* deduce_reverse_list(Value* list, Interp* interp) {
    Value* reversed = make_nil(interp);
    Value* curr = list;
    while (is_cons(curr)) {
        reversed = make_cons(interp, car(curr), reversed);
        curr = cdr(curr);
    }
    return reversed;
}

fn Value* deduce_relation_ensure_column_key_values(Relation* rel, Interp* interp, usz* created_out) {
    if (created_out != null) *created_out = 0;
    if (rel == null) return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: relation state is unavailable");
    if (rel.col_count == 0) return null;
    if (rel.column_key_values != null) return null;

    Value** key_values = (Value**)mem::malloc(Value*.sizeof * rel.col_count);
    if (g_deduce_force_column_key_values_oom || test_c_getenv("OMNI_DEDUCE_FORCE_COLUMN_KEY_VALUES_OOM") != null) {
        key_values = null;
    }
    if (key_values == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }

    for (usz i = 0; i < rel.col_count; i++) {
        Value* key = boundary_alloc_value_in_root(interp);
        key.tag = SYMBOL;
        key.sym_val = rel.columns[i];
        key_values[i] = key;
    }

    rel.column_key_values = key_values;
    if (created_out != null) *created_out = rel.col_count;
    return null;
}

fn int deduce_compare_value(Value* left, Value* right) {
    if (left == null && right == null) return 0;
    if (left == null) return -1;
    if (right == null) return 1;
    if (left.tag < right.tag) return -1;
    if (left.tag > right.tag) return 1;

    if (left.tag == NIL && right.tag == NIL) return 0;
    if (left.tag == INT && right.tag == INT) {
        if (left.int_val < right.int_val) return -1;
        if (left.int_val > right.int_val) return 1;
        return 0;
    }
    if (left.tag == DOUBLE && right.tag == DOUBLE) {
        if (left.double_val < right.double_val) return -1;
        if (left.double_val > right.double_val) return 1;
        return 0;
    }
    if (left.tag == STRING && right.tag == STRING) {
        usz min_len = left.str_len < right.str_len ? left.str_len : right.str_len;
        for (usz i = 0; i < min_len; i++) {
            char l = left.str_chars[i];
            char r = right.str_chars[i];
            if (l < r) return -1;
            if (l > r) return 1;
        }
        if (left.str_len < right.str_len) return -1;
        if (left.str_len > right.str_len) return 1;
        return 0;
    }
    if (left.tag == SYMBOL && right.tag == SYMBOL) {
        if (left.sym_val < right.sym_val) return -1;
        if (left.sym_val > right.sym_val) return 1;
        return 0;
    }
    if (left.tag == TIME_POINT && right.tag == TIME_POINT) {
        TimePoint left_tp = left.time_point_val;
        TimePoint right_tp = right.time_point_val;
        if (left_tp.year < right_tp.year) return -1;
        if (left_tp.year > right_tp.year) return 1;
        if (left_tp.month < right_tp.month) return -1;
        if (left_tp.month > right_tp.month) return 1;
        if (left_tp.day < right_tp.day) return -1;
        if (left_tp.day > right_tp.day) return 1;
        if (left_tp.hour < right_tp.hour) return -1;
        if (left_tp.hour > right_tp.hour) return 1;
        if (left_tp.minute < right_tp.minute) return -1;
        if (left_tp.minute > right_tp.minute) return 1;
        if (left_tp.second < right_tp.second) return -1;
        if (left_tp.second > right_tp.second) return 1;
        if (left_tp.microsecond < right_tp.microsecond) return -1;
        if (left_tp.microsecond > right_tp.microsecond) return 1;
        if (left_tp.tz_offset < right_tp.tz_offset) return -1;
        if (left_tp.tz_offset > right_tp.tz_offset) return 1;
        return 0;
    }

    return 0;
}

fn int deduce_compare_tuples(Value*[] left, Value*[] right, usz len) {
    usz end = len < left.len ? (len < right.len ? len : right.len) : left.len;
    for (usz i = 0; i < end; i++) {
        if (left[i] == null && right[i] != null) return -1;
        if (left[i] != null && right[i] == null) return 1;
        int c = deduce_compare_value(left[i], right[i]);
        if (c != 0) return c;
    }
    if (left.len == right.len) return 0;
    if (left.len < right.len) return -1;
    return 1;
}

fn Value* deduce_parse_bound_tuple(
    Value* bound,
    usz expected_len,
    Value*[] out,
    Interp* interp,
    char[] not_list_code,
    char[] not_list_message,
    char[] arity_code,
    char[] arity_message
) {
    if (expected_len == 0) {
        return is_nil(bound) ? null : deduce_raise(interp, arity_code, arity_message);
    }
    if (!is_cons(bound)) return deduce_raise(interp, not_list_code, not_list_message);

    Value* curr = bound;
    usz i = 0;
    while (is_cons(curr)) {
        if (i >= out.len) return deduce_raise(interp, arity_code, arity_message);
        out[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
        i++;
    }

    if (!is_nil(curr) || i != expected_len) {
        return deduce_raise(interp, arity_code, arity_message);
    }
    return null;
}

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
