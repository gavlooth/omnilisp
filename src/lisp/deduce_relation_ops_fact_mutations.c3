module lisp;

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
        if (!txn.defer_integrity_checks) {
            Value* ref_err = deduce_assert_retract_check_reference_integrity(rel, txn.txn, args[tuple_start..], &key, interp);
            if (ref_err != null) return ref_err;
        }
        int rc = mdb_del(txn.txn, rel.dbi, &key, null);
        if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
            return deduce_raise(interp, "deduce/retract-del-failed", "retract!: delete failed");
        }
        MdbDbi extensional_dbi = 0;
        if (deduce_open_extensional_dbi_for_relation(rel, txn.txn, false, &extensional_dbi, interp)) {
            rc = mdb_del(txn.txn, extensional_dbi, &key, null);
            if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
                return deduce_raise(interp, "deduce/retract-shadow-del-failed", "retract!: extensional shadow delete failed");
            }
        }
        if (rc == MDB_SUCCESS && !deduce_txn_log_delete(txn, rel)) {
            txn.mutation_pending = true;
        }
        return make_void(interp);
    }

    MdbTxn* write_txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &write_txn);
    if (rc != MDB_SUCCESS || write_txn == null) {
        return deduce_raise(interp, "deduce/retract-txn-open-failed", "retract!: txn open failed");
    }

    Value* ref_err = deduce_assert_retract_check_reference_integrity(rel, write_txn, args[tuple_start..], &key, interp);
    if (ref_err != null) {
        mdb_txn_abort(write_txn);
        return ref_err;
    }

    bool deleted = false;
    rc = mdb_del(write_txn, rel.dbi, &key, null);
    if (rc == MDB_SUCCESS) deleted = true;
    if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
        mdb_txn_abort(write_txn);
        return deduce_raise(interp, "deduce/retract-del-failed", "retract!: delete failed");
    }
    MdbDbi extensional_dbi = 0;
    if (deduce_open_extensional_dbi_for_relation(rel, write_txn, false, &extensional_dbi, interp)) {
        int shadow_rc = mdb_del(write_txn, extensional_dbi, &key, null);
        if (shadow_rc != MDB_SUCCESS && shadow_rc != MDB_NOTFOUND) {
            mdb_txn_abort(write_txn);
            return deduce_raise(interp, "deduce/retract-shadow-del-failed", "retract!: extensional shadow delete failed");
        }
    }
    rc = mdb_txn_commit(write_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "retract!: txn commit failed");
    }
    if (deleted) {
        deduce_relation_schema_note_delete(rel);
        Value* mutation_err = deduce_db_note_mutation(rel, true, interp);
        deduce_ignore_post_commit_error(mutation_err, interp);
    }
    return make_void(interp);
}
