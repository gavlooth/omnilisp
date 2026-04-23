module lisp;

fn void deduce_clear_post_commit_raise_state(Interp* interp) {
    if (interp == null) return;
    interp.flags.raise_pending = false;
    interp.raise_msg_len = 0;
    interp.raise_payload = null;
}

fn void deduce_ignore_post_commit_error(Value* err, Interp* interp) {
    if (err == null) return;
    deduce_clear_post_commit_raise_state(interp);
}

<*
@require interp != null
*>
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
        if (!txn.defer_integrity_checks) {
            Value* key_err = deduce_assert_fact_check_key_integrity(rel, txn.txn, args[tuple_start..], &key, interp);
            if (key_err != null) return key_err;
            Value* unique_err = deduce_assert_fact_check_unique_integrity(rel, txn.txn, args[tuple_start..], &key, interp);
            if (unique_err != null) return unique_err;
            Value* check_err = deduce_assert_fact_check_constraint_integrity_all(rel, args[tuple_start..], interp);
            if (check_err != null) return check_err;
            Value* ref_err = deduce_assert_fact_check_reference_integrity_all(rel, txn.txn, args[tuple_start..], interp);
            if (ref_err != null) return ref_err;
        }
        MdbVal existing_data = {};
        bool existed = mdb_get(txn.txn, rel.dbi, &key, &existing_data) == MDB_SUCCESS;
        int rc = mdb_put(txn.txn, rel.dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/fact-put-failed", "fact!: put failed");
        }
        MdbDbi extensional_dbi = 0;
        if (!deduce_open_extensional_dbi_for_relation(rel, txn.txn, true, &extensional_dbi, interp)) {
            return deduce_raise(interp, "deduce/fact-shadow-open-failed", "fact!: extensional shadow open failed");
        }
        rc = mdb_put(txn.txn, extensional_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/fact-shadow-put-failed", "fact!: extensional shadow put failed");
        }
        if (!existed && !deduce_txn_log_insert(txn, rel, &key_buf[0], key_len)) {
            txn.mutation_pending = true;
        }
        return make_void(interp);
    }

    MdbTxn* write_txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &write_txn);
    if (rc != MDB_SUCCESS || write_txn == null) {
        return deduce_raise(interp, "deduce/fact-txn-open-failed", "fact!: txn open failed");
    }

    Value* key_err = deduce_assert_fact_check_key_integrity(rel, write_txn, args[tuple_start..], &key, interp);
    if (key_err != null) {
        mdb_txn_abort(write_txn);
        return key_err;
    }
    Value* unique_err = deduce_assert_fact_check_unique_integrity(rel, write_txn, args[tuple_start..], &key, interp);
    if (unique_err != null) {
        mdb_txn_abort(write_txn);
        return unique_err;
    }
    Value* check_err = deduce_assert_fact_check_constraint_integrity_all(rel, args[tuple_start..], interp);
    if (check_err != null) {
        mdb_txn_abort(write_txn);
        return check_err;
    }
    Value* ref_err = deduce_assert_fact_check_reference_integrity_all(rel, write_txn, args[tuple_start..], interp);
    if (ref_err != null) {
        mdb_txn_abort(write_txn);
        return ref_err;
    }

    MdbVal existing_data = {};
    bool existed = mdb_get(write_txn, rel.dbi, &key, &existing_data) == MDB_SUCCESS;

    rc = mdb_put(write_txn, rel.dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/fact-put-failed", "fact!: put failed");
    }
    MdbDbi extensional_dbi = 0;
    if (!deduce_open_extensional_dbi_for_relation(rel, write_txn, true, &extensional_dbi, interp)) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/fact-shadow-open-failed", "fact!: extensional shadow open failed");
    }
    rc = mdb_put(write_txn, extensional_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/fact-shadow-put-failed", "fact!: extensional shadow put failed");
    }

    rc = mdb_txn_commit(write_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "fact!: txn commit failed");
    }
    if (!existed) {
        deduce_relation_schema_note_insert(rel);
        Value* mutation_err = deduce_db_note_insert_mutation(rel, &key_buf[0], key_len, interp);
        deduce_ignore_post_commit_error(mutation_err, interp);
    }
    return make_void(interp);
}
