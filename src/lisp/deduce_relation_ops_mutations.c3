module lisp;
// ============================================================
// Canonical cleanup/admin verbs:
//   retract! relation val...            → tuple delete
//   retract! txn relation val...        → tuple delete inside write txn
//   clear! relation                     → content reset, relation preserved
//   drop! relation                      → lifecycle removal, dropped handle
// ============================================================

<*
@require interp != null
*>
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
    deduce_ignore_post_commit_error(mutation_err, interp);
    return make_void(interp);
}

<*
@require interp != null
*>
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
    deduce_ignore_post_commit_error(mutation_err, interp);
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
    deduce_ignore_post_commit_error(meta_err, interp);
    return make_void(interp);
}
