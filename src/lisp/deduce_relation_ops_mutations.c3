module lisp;

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
        if (mutation_err != null) return mutation_err;
    }
    return make_void(interp);
}

// ============================================================
// Canonical cleanup/admin verbs:
//   retract! relation val...            → tuple delete
//   retract! txn relation val...        → tuple delete inside write txn
//   clear! relation                     → content reset, relation preserved
//   drop! relation                      → lifecycle removal, dropped handle
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
        if (mutation_err != null) return mutation_err;
    }
    return make_void(interp);
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
        return deduce_raise(interp, "deduce/clear-txn-open-failed", "deduce-clear: txn open failed");
    }

    Value* ref_err = deduce_assert_bulk_target_clear_drop_reference_integrity(rel, txn, interp);
    if (ref_err != null) {
        mdb_txn_abort(txn);
        return ref_err;
    }

    rc = mdb_drop(txn, rel.dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/clear-failed", "deduce-clear: clear failed");
    }
    MdbDbi extensional_dbi = 0;
    if (deduce_open_extensional_dbi_for_relation(rel, txn, false, &extensional_dbi, interp)) {
        rc = mdb_drop(txn, extensional_dbi, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/clear-shadow-failed", "deduce-clear: extensional shadow clear failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-clear: txn commit failed");
    }
    deduce_relation_schema_note_clear(rel);
    Value* mutation_err = deduce_db_note_mutation(rel, true, interp);
    if (mutation_err != null) return mutation_err;
    return make_void(interp);
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
    if (rel.dropped) return make_void(interp);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/drop-txn-open-failed", "deduce-drop: txn open failed");
    }

    Value* ref_err = deduce_assert_bulk_target_clear_drop_reference_integrity(rel, txn, interp);
    if (ref_err != null) {
        mdb_txn_abort(txn);
        return ref_err;
    }

    rc = mdb_drop(txn, rel.dbi, 1);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/drop-failed", "deduce-drop: drop failed");
    }
    MdbDbi extensional_dbi = 0;
    if (deduce_open_extensional_dbi_for_relation(rel, txn, false, &extensional_dbi, interp)) {
        rc = mdb_drop(txn, extensional_dbi, 1);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/drop-shadow-failed", "deduce-drop: extensional shadow drop failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-drop: txn commit failed");
    }

    deduce_relation_schema_note_clear(rel);
    Value* mutation_err = deduce_db_note_mutation(rel, true, interp);
    if (mutation_err != null) return mutation_err;
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema != null) schema.dropped = true;
    rel.dropped = true;
    Value* meta_err = deduce_relation_meta_persist_state(
        rel.db,
        rel.name,
        false,
        (SymbolId)0,
        0,
        0,
        0,
        DEDUCE_MATERIALIZED_STALE_NONE,
        true,
        interp
    );
    if (meta_err != null) return meta_err;
    return make_void(interp);
}
