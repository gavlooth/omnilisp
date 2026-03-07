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

// ============================================================
// Relation write/read operations
// ============================================================
fn Value* prim_assert_fact(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/fact-expected-relation-values", "fact!: expected (fact! relation val...)");

    DeduceTxn* txn = null;
    usz rel_index = 0;
    if (deduce_ffi_handle_name_equals(args[0], "deduce-txn")) {
        if (args.len < 2) {
            return deduce_raise(interp, "deduce/fact-expected-transaction-relation-values", "fact!: expected (fact! txn relation val...)");
        }
        if (args[0].ffi_val.lib_handle == null) {
            return deduce_raise(interp, "deduce/fact-transaction-handle-closed", "fact!: transaction handle is closed");
        }
        txn = (DeduceTxn*)args[0].ffi_val.lib_handle;
        if (!txn.open || txn.txn == null || txn.db == null || !txn.db.open) {
            return deduce_raise(interp, "deduce/fact-transaction-closed", "fact!: transaction is closed");
        }
        if (txn.read_only) {
            return deduce_raise(interp, "deduce/fact-transaction-read-only", "fact!: transaction is read-only");
        }
        rel_index = 1;
    }

    // First argument after optional txn should be a relation handle.
    Value* rel_val = args[rel_index];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/fact-relation-not-handle", "fact!: relation argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/fact-relation-handle-closed", "fact!: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/fact-relation-not-deduce-relation", "fact!: relation argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    // fault: lisp::TYPE_MISMATCH
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/fact-database-not-open", "fact!: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/fact-relation-dropped", "fact!: relation is dropped");
    if (txn != null && txn.db != rel.db) {
        return deduce_raise(interp, "deduce/fact-transaction-database-mismatch", "fact!: transaction database does not match relation database");
    }

    usz tuple_start = rel_index + 1;
    usz val_count = args.len - tuple_start;
    if (val_count != rel.col_count) {
        // fault: lisp::ARITY_MISMATCH
        return deduce_raise(interp, "deduce/fact-arity-mismatch", "fact!: wrong number of values for relation");
    }

    // Encode tuple
    char[4096] key_buf;  // buffer: C-interop (keep) — binary tuple encoding for mdb_put
    usz? key_len = encode_tuple(args[tuple_start..], key_buf[..], 4096);
    if (catch err = key_len) {
        return deduce_raise(interp, "deduce/fact-tuple-too-large", "fact!: tuple too large for buffer");
    }

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf;

    MdbVal data;
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    // Write to LMDB
    if (txn != null) {
        int rc = mdb_put(txn.txn, rel.dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/fact-put-failed", "fact!: put failed");
        }
        return make_nil(interp);
    }

    MdbTxn* write_txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &write_txn);
    if (rc != MDB_SUCCESS || write_txn == null) {
        return deduce_raise(interp, "deduce/fact-txn-begin-failed", "fact!: txn begin failed");
    }

    rc = mdb_put(write_txn, rel.dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/fact-put-failed", "fact!: put failed");
    }

    rc = mdb_txn_commit(write_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "fact!: txn commit failed");
    }
    return make_nil(interp);
}

// ============================================================
// (retract! relation val1 val2 ...) → nil
// ============================================================

fn Value* prim_retract_fact(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/retract-expected-relation-values", "retract!: expected (retract! relation val...)");

    DeduceTxn* txn = null;
    usz rel_index = 0;
    if (deduce_ffi_handle_name_equals(args[0], "deduce-txn")) {
        if (args.len < 2) {
            return deduce_raise(interp, "deduce/retract-expected-transaction-relation-values", "retract!: expected (retract! txn relation val...)");
        }
        if (args[0].ffi_val.lib_handle == null) {
            return deduce_raise(interp, "deduce/retract-transaction-handle-closed", "retract!: transaction handle is closed");
        }
        txn = (DeduceTxn*)args[0].ffi_val.lib_handle;
        if (!txn.open || txn.txn == null || txn.db == null || !txn.db.open) {
            return deduce_raise(interp, "deduce/retract-transaction-closed", "retract!: transaction is closed");
        }
        if (txn.read_only) {
            return deduce_raise(interp, "deduce/retract-transaction-read-only", "retract!: transaction is read-only");
        }
        rel_index = 1;
    }

    Value* rel_val = args[rel_index];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/retract-relation-not-handle", "retract!: relation argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/retract-relation-handle-closed", "retract!: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/retract-relation-not-deduce-relation", "retract!: relation argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    // fault: lisp::TYPE_MISMATCH
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/retract-database-not-open", "retract!: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/retract-relation-dropped", "retract!: relation is dropped");
    if (txn != null && txn.db != rel.db) {
        return deduce_raise(interp, "deduce/retract-transaction-database-mismatch", "retract!: transaction database does not match relation database");
    }

    usz tuple_start = rel_index + 1;
    usz val_count = args.len - tuple_start;
    if (val_count != rel.col_count) {
        // fault: lisp::ARITY_MISMATCH
        return deduce_raise(interp, "deduce/retract-arity-mismatch", "retract!: wrong number of values for relation");
    }

    char[4096] key_buf;  // buffer: C-interop (keep) — binary tuple encoding for mdb_del
    usz? key_len = encode_tuple(args[tuple_start..], key_buf[..], 4096);
    if (catch err = key_len) {
        return deduce_raise(interp, "deduce/retract-tuple-too-large", "retract!: tuple too large for buffer");
    }

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf;

    if (txn != null) {
        int rc = mdb_del(txn.txn, rel.dbi, &key, null);
        if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
            return deduce_raise(interp, "deduce/retract-del-failed", "retract!: delete failed");
        }
        return make_nil(interp);
    }

    MdbTxn* write_txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &write_txn);
    if (rc != MDB_SUCCESS || write_txn == null) {
        return deduce_raise(interp, "deduce/retract-txn-begin-failed", "retract!: txn begin failed");
    }

    rc = mdb_del(write_txn, rel.dbi, &key, null);
    if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/retract-del-failed", "retract!: delete failed");
    }
    rc = mdb_txn_commit(write_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "retract!: txn commit failed");
    }
    return make_nil(interp);
}

fn Value* prim_deduce_clear(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/clear-expected-relation", "deduce-clear: expected (deduce 'clear! relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/clear-relation-not-handle", "deduce-clear: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/clear-relation-handle-closed", "deduce-clear: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/clear-relation-not-deduce-relation", "deduce-clear: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/clear-database-not-open", "deduce-clear: database not open");
    }
    if (rel.dropped) return deduce_raise(interp, "deduce/clear-relation-dropped", "deduce-clear: relation is dropped");

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/clear-txn-begin-failed", "deduce-clear: txn begin failed");
    }

    rc = mdb_drop(txn, rel.dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/clear-failed", "deduce-clear: clear failed");
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-clear: txn commit failed");
    }
    return make_nil(interp);
}

fn Value* prim_deduce_drop(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/drop-expected-relation", "deduce-drop: expected (deduce 'drop! relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/drop-relation-not-handle", "deduce-drop: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/drop-relation-handle-closed", "deduce-drop: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/drop-relation-not-deduce-relation", "deduce-drop: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/drop-database-not-open", "deduce-drop: database not open");
    }
    if (rel.dropped) return make_nil(interp);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/drop-txn-begin-failed", "deduce-drop: txn begin failed");
    }

    rc = mdb_drop(txn, rel.dbi, 1);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/drop-failed", "deduce-drop: drop failed");
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-drop: txn commit failed");
    }

    rel.dropped = true;
    return make_nil(interp);
}

// ============================================================
// Scan all tuples in a relation → list of dicts
// ============================================================

fn Value* relation_scan_all(Relation* rel, Interp* interp) {
    return relation_scan_range(rel, interp, null, 0, false, {}, {}, false);
}

// ============================================================
// Count + scan operations
// ============================================================
fn Value* prim_deduce_count(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/count-expected-relation", "deduce-count: expected (deduce-count relation)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/count-relation-not-handle", "deduce-count: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/count-relation-handle-closed", "deduce-count: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/count-relation-not-deduce-relation", "deduce-count: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/count-database-not-open", "deduce-count: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/count-relation-dropped", "deduce-count: relation is dropped");

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS) return make_int(interp, 0);
    defer mdb_txn_abort(txn);

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS) return make_int(interp, 0);
    defer mdb_cursor_close(cursor);

    long count = 0;
    MdbVal key;
    MdbVal data;
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        count++;
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }

    return make_int(interp, count);
}

fn Value* prim_deduce_scan(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/scan-expected-relation", "deduce-scan: expected (deduce-scan relation)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/scan-relation-not-handle", "deduce-scan: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/scan-relation-handle-closed", "deduce-scan: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/scan-relation-not-deduce-relation", "deduce-scan: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/scan-database-not-open", "deduce-scan: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/scan-relation-dropped", "deduce-scan: relation is dropped");

    return relation_scan_all(rel, interp);
}

fn Value* prim_deduce_scan_range(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len != 3) return deduce_raise(interp, "deduce/scan-range-expected-relation-lower-upper", "deduce-scan-range: expected (deduce-scan-range relation lower-bound upper-bound)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/scan-range-relation-not-handle", "deduce-scan-range: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/scan-range-relation-handle-closed", "deduce-scan-range: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/scan-range-relation-not-deduce-relation", "deduce-scan-range: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/scan-range-database-not-open", "deduce-scan-range: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/scan-range-relation-dropped", "deduce-scan-range: relation is dropped");

    Value*[] lower_values = {};
    Value*[] upper_values = {};
    if (rel.col_count > 0) {
        lower_values = mem::new_array(Value*, rel.col_count);
        if (lower_values.ptr == null) {
            return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
        }
        upper_values = mem::new_array(Value*, rel.col_count);
        if (upper_values.ptr == null) {
            mem::free(lower_values.ptr);
            return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
        }
    }
    defer if (lower_values.ptr != null) mem::free(lower_values.ptr);
    defer if (upper_values.ptr != null) mem::free(upper_values.ptr);

    Value* lower_err = deduce_parse_bound_tuple(
        args[1],
        rel.col_count,
        lower_values,
        interp,
        "deduce/scan-range-lower-not-list",
        "deduce-scan-range: lower-bound must be a tuple list",
        "deduce/scan-range-lower-arity-mismatch",
        "deduce-scan-range: lower-bound arity must match relation columns"
    );
    if (lower_err != null) return lower_err;

    Value* upper_err = deduce_parse_bound_tuple(
        args[2],
        rel.col_count,
        upper_values,
        interp,
        "deduce/scan-range-upper-not-list",
        "deduce-scan-range: upper-bound must be a tuple list",
        "deduce/scan-range-upper-arity-mismatch",
        "deduce-scan-range: upper-bound arity must match relation columns"
    );
    if (upper_err != null) return upper_err;

    char[4096] lower_key_buf;
    usz? lower_key_len = encode_tuple(lower_values, lower_key_buf[..], 4096);
    if (catch err = lower_key_len) {
        return deduce_raise(interp, "deduce/scan-range-lower-key-too-large", "deduce-scan-range: lower-bound tuple too large for buffer");
    }

    return relation_scan_range(
        rel,
        interp,
        &lower_key_buf[0],
        lower_key_len,
        true,
        lower_values,
        upper_values,
        true
    );
}
