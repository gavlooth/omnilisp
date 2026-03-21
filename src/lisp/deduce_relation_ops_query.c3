module lisp;

import std::core::mem;

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
