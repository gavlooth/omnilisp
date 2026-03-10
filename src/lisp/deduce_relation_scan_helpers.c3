module lisp;

import std::core::mem;

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
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS) return make_nil(interp);
    defer mdb_txn_abort(txn);

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS) return make_nil(interp);
    defer mdb_cursor_close(cursor);

    Value* result = make_nil(interp);
    Value*[] vals = {};
    if (rel.col_count > 0) {
        vals = mem::new_array(Value*, rel.col_count);
        if (vals.ptr == null) return make_nil(interp);
    }
    defer if (vals.ptr != null) mem::free(vals.ptr);

    MdbVal key;
    MdbVal data;
    if (has_start) {
        key.mv_data = start_key;
        key.mv_size = start_key_len;
        rc = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);
    } else {
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    }

    while (rc == MDB_SUCCESS) {
        usz? count_opt = decode_tuple((char*)key.mv_data, key.mv_size, vals, interp);
        if (catch err = count_opt) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (count_opt == rel.col_count) {
            if (has_start && deduce_compare_tuples(vals, lower_bound, rel.col_count) < 0) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (has_end && deduce_compare_tuples(vals, upper_bound, rel.col_count) > 0) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
        }

        if (count_opt == rel.col_count) {
            Value* dict = make_hashmap(interp, 8);
            for (usz i = 0; i < rel.col_count; i++) {
                Value* k = interp.alloc_value();
                k.tag = SYMBOL;
                k.sym_val = rel.columns[i];
                hashmap_set(dict.hashmap_val, k, vals[i], interp);
            }
            result = make_cons(interp, dict, result);
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }

    return result;
}

fn Value* relation_scan_all(Relation* rel, Interp* interp) {
    return relation_scan_range(rel, interp, null, 0, false, {}, {}, false);
}
