module lisp;

import std::core::mem;

// ============================================================
// Relation write/read operations
// ============================================================
fn Value* prim_assert_fact(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "fact!: expected (fact! relation val...)");

    // First arg should be a Relation value (FFI_HANDLE wrapping Relation*)
    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "fact!: first argument must be a relation");
    }
    if (rel_val.ffi_val == null) return raise_error(interp, "fact!: relation handle is closed");
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    // fault: lisp::TYPE_MISMATCH
    if (rel.db == null || !rel.db.open) return raise_error(interp, "fact!: database not open");

    usz val_count = args.len - 1;
    if (val_count != rel.col_count) {
        // fault: lisp::ARITY_MISMATCH
        return raise_error(interp, "fact!: wrong number of values for relation");
    }

    // Encode tuple
    char[4096] key_buf;  // buffer: C-interop (keep) — binary tuple encoding for mdb_put
    usz? key_len = encode_tuple(args[1..], key_buf[..], 4096);
    if (catch err = key_len) {
        return raise_error(interp, "fact!: tuple too large for buffer");
    }

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf;

    MdbVal data;
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    // Write to LMDB
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    // fault: lisp::WRITE_FAILED
    if (rc != MDB_SUCCESS) return raise_error(interp, "fact!: txn begin failed");

    rc = mdb_put(txn, rel.dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        // fault: lisp::WRITE_FAILED
        return raise_error(interp, "fact!: put failed");
    }

    mdb_txn_commit(txn);
    return make_nil(interp);
}

// ============================================================
// (retract! relation val1 val2 ...) → nil
// ============================================================

fn Value* prim_retract_fact(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "retract!: expected (retract! relation val...)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "retract!: first argument must be a relation");
    }
    if (rel_val.ffi_val == null) return raise_error(interp, "retract!: relation handle is closed");
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    // fault: lisp::TYPE_MISMATCH
    if (rel.db == null || !rel.db.open) return raise_error(interp, "retract!: database not open");

    usz val_count = args.len - 1;
    if (val_count != rel.col_count) {
        // fault: lisp::ARITY_MISMATCH
        return raise_error(interp, "retract!: wrong number of values for relation");
    }

    char[4096] key_buf;  // buffer: C-interop (keep) — binary tuple encoding for mdb_del
    usz? key_len = encode_tuple(args[1..], key_buf[..], 4096);
    if (catch err = key_len) {
        return raise_error(interp, "retract!: tuple too large for buffer");
    }

    MdbVal key;
    key.mv_size = key_len;
    key.mv_data = &key_buf;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    // fault: lisp::WRITE_FAILED
    if (rc != MDB_SUCCESS) return raise_error(interp, "retract!: txn begin failed");

    rc = mdb_del(txn, rel.dbi, &key, null);
    mdb_txn_commit(txn);
    return make_nil(interp);
}

// ============================================================
// Scan all tuples in a relation → list of dicts
// ============================================================

fn Value* relation_scan_all(Relation* rel, Interp* interp) {
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS) return make_nil(interp);
    defer mdb_txn_abort(txn);

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS) return make_nil(interp);
    defer mdb_cursor_close(cursor);

    Value* result = make_nil(interp);
    Value*[MAX_RELATION_COLS] vals;

    MdbVal key;
    MdbVal data;
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        // Decode tuple
        usz? count_opt = decode_tuple((char*)key.mv_data, key.mv_size, vals[..rel.col_count], interp);
        if (catch err = count_opt) {
            // Malformed data — skip this entry
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (count_opt == rel.col_count) {
            // Build dict with column names as keys
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
// Count + scan operations
// ============================================================
fn Value* prim_deduce_count(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "deduce-count: expected (deduce-count relation)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "deduce-count: first argument must be a relation");
    }
    if (rel_val.ffi_val == null) return raise_error(interp, "deduce-count: relation handle is closed");
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;

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
    if (args.len < 1) return raise_error(interp, "deduce-scan: expected (deduce-scan relation)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "deduce-scan: first argument must be a relation");
    }
    if (rel_val.ffi_val == null) return raise_error(interp, "deduce-scan: relation handle is closed");
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;

    return relation_scan_all(rel, interp);
}
