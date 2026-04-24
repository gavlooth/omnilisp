module lisp;

import std::core::mem;

// Test seam: force deferred integrity scan termination down the storage-error path.
bool g_deduce_force_deferred_integrity_scan_end_fail = false;

fn Value* deduce_validate_relation_fact_integrity_snapshot(Relation* rel, MdbTxn* txn, Interp* interp) {
    if (rel == null || txn == null || interp == null) return null;
    if (rel.dropped || rel.columns == null || rel.col_count == 0) return null;
    if (rel.key_col == (SymbolId)0 &&
        rel.unique_col_count == 0 &&
        rel.reference_constraint_count == 0 &&
        rel.check_constraint_count == 0) {
        return null;
    }

    Value*[] decoded = mem::new_array(Value*, rel.col_count);
    if (decoded.ptr == null) {
        return deduce_raise(interp, "deduce/deferred-integrity-out-of-memory",
            "deduce-commit: deferred integrity validation ran out of memory");
    }
    defer mem::free(decoded.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
            "deduce-commit: deferred integrity validation scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
                "deduce-commit: deferred integrity tuple decode failed");
        }
        if (decoded_count != rel.col_count) {
            return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
                "deduce-commit: deferred integrity tuple arity mismatch");
        }

        Value* key_err = deduce_assert_fact_check_key_integrity(rel, txn, decoded, &existing_key, interp);
        if (key_err != null) return key_err;
        Value* unique_err = deduce_assert_fact_check_unique_integrity(rel, txn, decoded, &existing_key, interp);
        if (unique_err != null) return unique_err;
        Value* check_err = deduce_assert_fact_check_constraint_integrity_all(rel, decoded, interp);
        if (check_err != null) return check_err;
        Value* ref_err = deduce_assert_fact_check_reference_integrity_all(rel, txn, decoded, interp);
        if (ref_err != null) return ref_err;

        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
    }
    if (g_deduce_force_deferred_integrity_scan_end_fail || rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
            "deduce-commit: deferred integrity validation scan failed");
    }

    return null;
}

fn Value* deduce_validate_schema_fact_integrity_snapshot(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    MdbTxn* txn,
    MdbDbi dbi,
    Interp* interp
) {
    if (db == null || schema == null || txn == null || interp == null) return null;
    Relation rel = deduce_relation_view_from_schema(db, schema, dbi);
    return deduce_validate_relation_fact_integrity_snapshot(&rel, txn, interp);
}

fn Value* deduce_validate_relation_reference_integrity_snapshot_by_schema(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    MdbTxn* txn,
    Interp* interp
) {
    if (db == null || schema == null || txn == null || interp == null) return null;
    if (schema.dropped) return null;
    if (schema.reference_constraint_count == 0 || schema.reference_constraints == null) return null;
    if (schema.columns == null || schema.arity == 0) return null;

    Relation source_rel = {
        .name = schema.name,
        .columns = schema.columns,
        .column_key_values = null,
        .col_count = schema.arity,
        .col_capacity = schema.arity,
        .dbi = 0,
        .db = db,
        .key_col = schema.key_column,
        .unique_col = schema.unique_column,
        .unique_cols = schema.unique_columns,
        .unique_col_count = schema.unique_column_count,
        .unique_col_capacity = schema.unique_column_count,
        .reference_constraints = schema.reference_constraints,
        .reference_constraint_count = schema.reference_constraint_count,
        .reference_constraint_capacity = schema.reference_constraint_count,
        .index_col = schema.index_column,
        .dropped = false,
    };

    MdbDbi source_dbi = 0;
    if (!deduce_open_relation_dbi_by_symbol(db, txn, schema.name, &source_dbi, interp)) {
        return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
            "deduce-commit: deferred reference integrity source relation open failed");
    }
    source_rel.dbi = source_dbi;

    Value*[] decoded = mem::new_array(Value*, schema.arity);
    if (decoded.ptr == null) {
        return deduce_raise(interp, "deduce/deferred-integrity-out-of-memory",
            "deduce-commit: deferred integrity validation ran out of memory");
    }
    defer mem::free(decoded.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, source_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
            "deduce-commit: deferred integrity validation scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
                "deduce-commit: deferred reference integrity tuple decode failed");
        }
        if (decoded_count != schema.arity) {
            return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
                "deduce-commit: deferred reference integrity tuple arity mismatch");
        }

        for (usz ref_i = 0; ref_i < schema.reference_constraint_count; ref_i++) {
            Value* ref_err = deduce_assert_fact_check_reference_integrity(
                &source_rel,
                txn,
                decoded,
                &schema.reference_constraints[ref_i],
                interp
            );
            if (ref_err != null) return ref_err;
        }

        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
    }
    if (g_deduce_force_deferred_integrity_scan_end_fail || rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/deferred-integrity-scan-failed",
            "deduce-commit: deferred reference integrity validation scan failed");
    }

    return null;
}

fn Value* deduce_txn_validate_deferred_integrity(DeduceTxn* txn, Interp* interp) {
    if (txn == null || txn.txn == null || interp == null) return null;
    if (!txn.defer_integrity_checks) return null;
    if (txn.mutation_entries == null || txn.mutation_entry_count == 0) return null;

    bool has_delete = false;
    for (usz i = 0; i < txn.mutation_entry_count; i++) {
        DeduceTxnMutationEntry* entry = &txn.mutation_entries[i];
        if (entry.deleted_count > 0) has_delete = true;
        if (entry.relation == null) continue;
        if (entry.inserted_count == 0) continue;

        Value* err = deduce_validate_relation_fact_integrity_snapshot(entry.relation, txn.txn, interp);
        if (err != null) return err;
    }

    if (has_delete && txn.db != null && txn.db.relation_schemas != null) {
        for (usz i = 0; i < txn.db.relation_schema_count; i++) {
            Value* err = deduce_validate_relation_reference_integrity_snapshot_by_schema(
                txn.db,
                &txn.db.relation_schemas[i],
                txn.txn,
                interp
            );
            if (err != null) return err;
        }
    }

    return null;
}
