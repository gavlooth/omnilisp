module lisp;

import std::core::mem;

struct DeduceScanRangeBenchStats {
    bool enabled;
    usz calls;
    usz rows_materialized;
    usz key_symbols_allocated;
    long materialize_ms;
}

DeduceScanRangeBenchStats deduce_scan_range_bench_stats;

struct DeduceTupleIterator {
    Relation* rel;
    Interp*   interp;
    MdbTxn*   txn;
    MdbCursor* cursor;
    MdbVal    key;
    MdbVal    data;
    Value*[]  vals;
    Value*[]  lower_bound;
    Value*[]  upper_bound;
    char*     start_key;
    usz       start_key_len;
    bool      has_start;
    bool      has_end;
    bool      started;
    bool      open;
}

struct DeduceJoinExecStats {
    bool used_index;
    bool used_hash;
    bool fallback_applied;
    long outer_rows;
    long inner_rows;
    long inner_probes;
    long index_hits;
    long index_misses;
    long rows_emitted;
}

fn void deduce_scan_range_bench_reset() {
    deduce_scan_range_bench_stats.calls = 0;
    deduce_scan_range_bench_stats.rows_materialized = 0;
    deduce_scan_range_bench_stats.key_symbols_allocated = 0;
    deduce_scan_range_bench_stats.materialize_ms = 0;
}

fn void deduce_scan_range_bench_set_enabled(bool enabled) {
    deduce_scan_range_bench_stats.enabled = enabled;
    deduce_scan_range_bench_reset();
}

fn DeduceScanRangeBenchStats deduce_scan_range_bench_snapshot() {
    return deduce_scan_range_bench_stats;
}

fn bool deduce_tuple_iterator_open(
    DeduceTupleIterator* it,
    Relation* rel,
    Interp* interp,
    char* start_key,
    usz start_key_len,
    bool has_start,
    Value*[] lower_bound,
    Value*[] upper_bound,
    bool has_end
) {
    if (it == null || rel == null || interp == null) return false;
    if (rel.db == null || !rel.db.open) return false;

    *it = {};
    it.rel = rel;
    it.interp = interp;
    it.start_key = start_key;
    it.start_key_len = start_key_len;
    it.has_start = has_start;
    it.lower_bound = lower_bound;
    it.upper_bound = upper_bound;
    it.has_end = has_end;

    if (rel.col_count > 0) {
        it.vals = mem::new_array(Value*, rel.col_count);
        if (it.vals.ptr == null) return false;
    }

    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &it.txn);
    if (rc != MDB_SUCCESS || it.txn == null) {
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        return false;
    }

    rc = mdb_cursor_open(it.txn, rel.dbi, &it.cursor);
    if (rc != MDB_SUCCESS || it.cursor == null) {
        mdb_txn_abort(it.txn);
        it.txn = null;
        if (it.vals.ptr != null) {
            mem::free(it.vals.ptr);
            it.vals = {};
        }
        return false;
    }

    it.started = false;
    it.open = true;
    return true;
}

fn void deduce_tuple_iterator_close(DeduceTupleIterator* it) {
    if (it == null) return;
    if (it.cursor != null) {
        mdb_cursor_close(it.cursor);
        it.cursor = null;
    }
    if (it.txn != null) {
        mdb_txn_abort(it.txn);
        it.txn = null;
    }
    if (it.vals.ptr != null) {
        mem::free(it.vals.ptr);
        it.vals = {};
    }
    it.open = false;
}

fn bool deduce_tuple_iterator_next(DeduceTupleIterator* it) {
    if (it == null || !it.open || it.cursor == null || it.rel == null) return false;

    int rc = MDB_NOTFOUND;
    if (!it.started) {
        it.started = true;
        if (it.has_start) {
            it.key.mv_data = it.start_key;
            it.key.mv_size = it.start_key_len;
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_SET_RANGE);
        } else {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_FIRST);
        }
    } else {
        rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
    }

    while (rc == MDB_SUCCESS) {
        usz? count_opt = decode_tuple((char*)it.key.mv_data, it.key.mv_size, it.vals, it.interp);
        if (catch err = count_opt) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        if (count_opt != it.rel.col_count) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }

        if (it.has_start && deduce_compare_tuples(it.vals, it.lower_bound, it.rel.col_count) < 0) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        if (it.has_end && deduce_compare_tuples(it.vals, it.upper_bound, it.rel.col_count) > 0) {
            rc = mdb_cursor_get(it.cursor, &it.key, &it.data, MDB_NEXT);
            continue;
        }
        return true;
    }
    return false;
}

fn bool deduce_join_validate_column_map(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols
) {
    if (outer_rel == null || inner_rel == null) return false;
    if (outer_join_cols.len == 0) return false;
    if (outer_join_cols.len != inner_join_cols.len) return false;
    for (usz i = 0; i < outer_join_cols.len; i++) {
        if (outer_join_cols[i] >= outer_rel.col_count) return false;
        if (inner_join_cols[i] >= inner_rel.col_count) return false;
    }
    return true;
}

fn bool deduce_join_all_bound(Value*[] vals) {
    for (usz i = 0; i < vals.len; i++) {
        if (vals[i] == null) return false;
    }
    return true;
}

fn bool deduce_join_probe_inner_exact(
    MdbTxn* inner_txn,
    Relation* inner_rel,
    Value*[] probe_vals
) {
    if (inner_txn == null || inner_rel == null) return false;
    char[4096] key_buf;
    usz? key_len = encode_tuple(probe_vals, key_buf[..], key_buf.len);
    if (catch err = key_len) return false;

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf[0];
    MdbVal data = {};
    return mdb_get(inner_txn, inner_rel.dbi, &key, &data) == MDB_SUCCESS;
}

fn bool deduce_join_extract_key_values(Value*[] tuple_vals, usz tuple_col_count, usz[] join_cols, Value*[] key_vals) {
    if (join_cols.len != key_vals.len) return false;
    for (usz i = 0; i < join_cols.len; i++) {
        usz col = join_cols[i];
        if (col >= tuple_col_count || col >= tuple_vals.len) return false;
        Value* v = tuple_vals[col];
        if (v == null) return false;
        key_vals[i] = v;
    }
    return true;
}

fn bool deduce_join_inner_row_matches_static_values(Value*[] tuple_vals, usz tuple_col_count, Value*[] static_vals) {
    if (static_vals.len == 0) return true;
    if (static_vals.len != tuple_col_count || tuple_vals.len < tuple_col_count) return false;
    for (usz i = 0; i < tuple_col_count; i++) {
        Value* expected = static_vals[i];
        if (expected == null) continue;
        if (!values_equal(tuple_vals[i], expected)) return false;
    }
    return true;
}

fn bool deduce_index_nested_loop_join_count(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols,
    Value*[] inner_static_vals,
    long* join_count_out,
    DeduceJoinExecStats* stats_out,
    Interp* interp
) {
    if (join_count_out == null || interp == null) return false;
    *join_count_out = 0;
    if (stats_out != null) {
        *stats_out = {};
        stats_out.used_index = false;
        stats_out.used_hash = false;
        stats_out.fallback_applied = false;
    }

    if (!deduce_join_validate_column_map(outer_rel, inner_rel, outer_join_cols, inner_join_cols)) {
        return false;
    }
    if (inner_static_vals.len != 0 && inner_static_vals.len != inner_rel.col_count) {
        return false;
    }

    Value*[] probe_vals = {};
    if (inner_rel.col_count > 0) {
        probe_vals = mem::new_array(Value*, inner_rel.col_count);
        if (probe_vals.ptr == null) return false;
    }
    defer if (probe_vals.ptr != null) mem::free(probe_vals.ptr);

    DeduceTupleIterator outer_it = {};
    if (!deduce_tuple_iterator_open(&outer_it, outer_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    defer deduce_tuple_iterator_close(&outer_it);

    MdbTxn* inner_txn = outer_it.txn;
    bool owns_inner_txn = false;
    if (outer_rel.db.env != inner_rel.db.env) {
        if (mdb_txn_begin(inner_rel.db.env, null, MDB_RDONLY, &inner_txn) != MDB_SUCCESS || inner_txn == null) {
            return false;
        }
        owns_inner_txn = true;
    }
    defer if (owns_inner_txn && inner_txn != null) mdb_txn_abort(inner_txn);

    if (stats_out != null) stats_out.used_index = true;

    while (deduce_tuple_iterator_next(&outer_it)) {
        if (stats_out != null) stats_out.outer_rows++;

        for (usz i = 0; i < probe_vals.len; i++) {
            probe_vals[i] = inner_static_vals.len == inner_rel.col_count ? inner_static_vals[i] : null;
        }
        for (usz i = 0; i < outer_join_cols.len; i++) {
            usz outer_col = outer_join_cols[i];
            usz inner_col = inner_join_cols[i];
            probe_vals[inner_col] = outer_it.vals[outer_col];
        }

        if (!deduce_join_all_bound(probe_vals)) {
            return false;
        }

        if (stats_out != null) stats_out.inner_probes++;
        if (deduce_join_probe_inner_exact(inner_txn, inner_rel, probe_vals)) {
            *join_count_out = *join_count_out + 1;
            if (stats_out != null) {
                stats_out.index_hits++;
                stats_out.rows_emitted++;
            }
        } else if (stats_out != null) {
            stats_out.index_misses++;
        }
    }

    return true;
}

fn bool deduce_hash_join_count(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols,
    Value*[] inner_static_vals,
    long* join_count_out,
    DeduceJoinExecStats* stats_out,
    Interp* interp
) {
    if (join_count_out == null || interp == null) return false;
    *join_count_out = 0;
    if (stats_out != null) {
        *stats_out = {};
        stats_out.used_hash = false;
        stats_out.used_index = false;
        stats_out.fallback_applied = false;
    }

    if (!deduce_join_validate_column_map(outer_rel, inner_rel, outer_join_cols, inner_join_cols)) {
        return false;
    }
    if (inner_static_vals.len != 0 && inner_static_vals.len != inner_rel.col_count) {
        return false;
    }

    Value*[] inner_key_vals = {};
    if (inner_join_cols.len > 0) {
        inner_key_vals = mem::new_array(Value*, inner_join_cols.len);
        if (inner_key_vals.ptr == null) return false;
    }
    defer if (inner_key_vals.ptr != null) mem::free(inner_key_vals.ptr);

    Value*[] outer_key_vals = {};
    if (outer_join_cols.len > 0) {
        outer_key_vals = mem::new_array(Value*, outer_join_cols.len);
        if (outer_key_vals.ptr == null) return false;
    }
    defer if (outer_key_vals.ptr != null) mem::free(outer_key_vals.ptr);

    Value* key_count_map = make_hashmap(interp, 128);
    if (key_count_map == null || key_count_map.tag != HASHMAP || key_count_map.hashmap_val == null) return false;

    DeduceTupleIterator inner_it = {};
    if (!deduce_tuple_iterator_open(&inner_it, inner_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    bool inner_it_open = true;
    defer if (inner_it_open) deduce_tuple_iterator_close(&inner_it);

    while (deduce_tuple_iterator_next(&inner_it)) {
        if (stats_out != null) stats_out.inner_rows++;
        if (!deduce_join_inner_row_matches_static_values(inner_it.vals, inner_rel.col_count, inner_static_vals)) {
            continue;
        }
        if (!deduce_join_extract_key_values(inner_it.vals, inner_rel.col_count, inner_join_cols, inner_key_vals)) {
            return false;
        }
        char[4096] key_buf;
        usz? key_len = encode_tuple(inner_key_vals, key_buf[..], key_buf.len);
        if (catch err = key_len) return false;

        Value* key_val = make_string(interp, key_buf[:key_len]);
        Value* existing = hashmap_get(key_count_map.hashmap_val, key_val);
        long next_count = existing != null && existing.tag == INT ? existing.int_val + 1 : 1;
        hashmap_set(key_count_map.hashmap_val, key_val, make_int(interp, next_count), interp);
    }
    deduce_tuple_iterator_close(&inner_it);
    inner_it_open = false;

    DeduceTupleIterator outer_it = {};
    if (!deduce_tuple_iterator_open(&outer_it, outer_rel, interp, null, 0, false, {}, {}, false)) {
        return false;
    }
    defer deduce_tuple_iterator_close(&outer_it);

    while (deduce_tuple_iterator_next(&outer_it)) {
        if (stats_out != null) stats_out.outer_rows++;
        if (!deduce_join_extract_key_values(outer_it.vals, outer_rel.col_count, outer_join_cols, outer_key_vals)) {
            return false;
        }
        char[4096] key_buf;
        usz? key_len = encode_tuple(outer_key_vals, key_buf[..], key_buf.len);
        if (catch err = key_len) return false;
        if (stats_out != null) stats_out.inner_probes++;
        Value* key_val = make_string(interp, key_buf[:key_len]);
        Value* count_val = hashmap_get(key_count_map.hashmap_val, key_val);
        long matched = count_val != null && count_val.tag == INT ? count_val.int_val : 0;
        if (matched > 0) {
            *join_count_out = *join_count_out + matched;
            if (stats_out != null) stats_out.rows_emitted += matched;
        }
    }
    if (stats_out != null) stats_out.used_hash = true;

    return true;
}

fn bool deduce_adaptive_join_count(
    Relation* outer_rel,
    Relation* inner_rel,
    usz[] outer_join_cols,
    usz[] inner_join_cols,
    Value*[] inner_static_vals,
    long fallback_probe_threshold,
    long fallback_miss_ratio_percent,
    long* join_count_out,
    DeduceJoinExecStats* stats_out,
    Interp* interp
) {
    if (join_count_out == null || interp == null) return false;
    long probe_threshold = fallback_probe_threshold > 0 ? fallback_probe_threshold : 32;
    long miss_ratio_percent = fallback_miss_ratio_percent > 0 ? fallback_miss_ratio_percent : 70;

    long index_count = 0;
    DeduceJoinExecStats index_stats = {};
    bool index_ok = deduce_index_nested_loop_join_count(
        outer_rel,
        inner_rel,
        outer_join_cols,
        inner_join_cols,
        inner_static_vals,
        &index_count,
        &index_stats,
        interp
    );

    bool should_fallback = false;
    if (!index_ok) {
        should_fallback = true;
    } else if (index_stats.inner_probes >= probe_threshold) {
        should_fallback =
            (index_stats.index_misses * 100) >= (index_stats.inner_probes * miss_ratio_percent);
    }

    if (!should_fallback) {
        *join_count_out = index_count;
        if (stats_out != null) *stats_out = index_stats;
        return index_ok;
    }

    long hash_count = 0;
    DeduceJoinExecStats hash_stats = {};
    bool hash_ok = deduce_hash_join_count(
        outer_rel,
        inner_rel,
        outer_join_cols,
        inner_join_cols,
        inner_static_vals,
        &hash_count,
        &hash_stats,
        interp
    );
    if (!hash_ok) return false;

    hash_stats.fallback_applied = true;
    *join_count_out = hash_count;
    if (stats_out != null) *stats_out = hash_stats;
    return true;
}

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

fn bool deduce_relation_ensure_column_key_values(Relation* rel, Interp* interp, usz* created_out) {
    if (created_out != null) *created_out = 0;
    if (rel == null) return false;
    if (rel.col_count == 0) return true;
    if (rel.column_key_values != null) return true;

    Value** key_values = (Value**)mem::malloc(Value*.sizeof * rel.col_count);
    if (key_values == null) return false;

    for (usz i = 0; i < rel.col_count; i++) {
        Value* key = boundary_alloc_value_in_root(interp);
        key.tag = SYMBOL;
        key.sym_val = rel.columns[i];
        key_values[i] = key;
    }

    rel.column_key_values = key_values;
    if (created_out != null) *created_out = rel.col_count;
    return true;
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
    bool has_end
) {
    bool bench_enabled = deduce_scan_range_bench_stats.enabled;
    if (bench_enabled) deduce_scan_range_bench_stats.calls++;
    usz created_key_values = 0;
    if (!deduce_relation_ensure_column_key_values(rel, interp, &created_key_values)) {
        return make_nil(interp);
    }
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
        if (bench_enabled) {
            deduce_scan_range_bench_stats.rows_materialized++;
            long materialize_end_ms = scheduler_monotonic_ms();
            if (materialize_end_ms > materialize_start_ms) {
                deduce_scan_range_bench_stats.materialize_ms += (materialize_end_ms - materialize_start_ms);
            }
        }
        result = make_cons(interp, dict, result);
    }

    return result;
}

fn Value* relation_scan_all(Relation* rel, Interp* interp) {
    return relation_scan_range(rel, interp, null, 0, false, {}, {}, false);
}
