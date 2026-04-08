module lisp;

import std::core::mem;

fn Value* deduce_assert_retract_check_reference_integrity(
    Relation* rel,
    MdbTxn* txn,
    Value*[] tuple_vals,
    MdbVal* tuple_key,
    Interp* interp
) {
    if (rel == null || txn == null || tuple_key == null || interp == null) return null;
    if (rel.db == null || rel.columns == null || rel.col_count == 0) return null;
    if (rel.db.relation_schemas == null || rel.db.relation_schema_count == 0) return null;

    for (usz schema_i = 0; schema_i < rel.db.relation_schema_count; schema_i++) {
        DeduceRelationSchema* schema = &rel.db.relation_schemas[schema_i];
        if (schema.reference_constraint_count == 0 || schema.reference_constraints == null) continue;

        for (usz ref_i = 0; ref_i < schema.reference_constraint_count; ref_i++) {
            DeduceReferenceConstraint* ref_constraint = &schema.reference_constraints[ref_i];
            if (ref_constraint.target_relation != rel.name) continue;

            usz target_pos = 0;
            if (!deduce_find_column_position(rel.columns, rel.col_count, ref_constraint.target_column, &target_pos)) continue;
            if (target_pos >= tuple_vals.len) continue;

            usz local_pos = 0;
            if (schema.columns == null ||
                !deduce_find_column_position(schema.columns, schema.arity, ref_constraint.local_column, &local_pos)) {
                continue;
            }

            {
                Value*[] decoded = {};
                if (schema.arity > 0) {
                    decoded = mem::new_array(Value*, schema.arity);
                    if (decoded.ptr == null) {
                        return deduce_raise(interp, "deduce/retract-reference-integrity-out-of-memory",
                            "retract!: reference integrity check ran out of memory");
                    }
                }
                defer if (decoded.ptr != null) mem::free(decoded.ptr);

                MdbDbi source_dbi = 0;
                if (!deduce_open_relation_dbi_by_symbol(rel.db, txn, schema.name, &source_dbi, interp)) {
                    continue;
                }

                MdbCursor* cursor = null;
                int rc = mdb_cursor_open(txn, source_dbi, &cursor);
                if (rc != MDB_SUCCESS || cursor == null) {
                    return deduce_raise(interp, "deduce/retract-reference-integrity-scan-failed",
                        "retract!: reference integrity scan failed");
                }
                defer if (cursor != null) mdb_cursor_close(cursor);

                MdbVal existing_key = {};
                MdbVal existing_data = {};
                rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
                while (rc == MDB_SUCCESS) {
                    usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
                    if (catch err = decoded_count) {
                        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
                        continue;
                    }
                    if (decoded_count != schema.arity) {
                        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
                        continue;
                    }
                    if (local_pos >= decoded.len ||
                        deduce_compare_value(decoded[local_pos], tuple_vals[target_pos]) != 0) {
                        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
                        continue;
                    }
                    if (schema.name == rel.name && deduce_encoded_tuple_key_equals(&existing_key, tuple_key)) {
                        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
                        continue;
                    }

                    Value* data = deduce_build_reference_target_in_use_payload(
                        rel,
                        tuple_vals,
                        ref_constraint.target_column,
                        target_pos,
                        schema.name,
                        ref_constraint.local_column,
                        decoded,
                        interp
                    );
                    deduce_db_note_integrity_violation(
                        rel,
                        DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
                        "deduce/integrity-reference-target-in-use",
                        interp
                    );
                    return deduce_raise(
                        interp,
                        "deduce/integrity-reference-target-in-use",
                        "retract!: referenced target tuple is still in use",
                        data
                    );
                }
            }
        }
    }

    return null;
}

fn Value* deduce_assert_bulk_target_clear_drop_reference_integrity(
    Relation* rel,
    MdbTxn* txn,
    Interp* interp
) {
    if (rel == null || txn == null || interp == null) return null;
    if (rel.col_count == 0 || rel.columns == null) return null;

    Value*[] decoded = mem::new_array(Value*, rel.col_count);
    if (decoded.ptr == null) {
        return deduce_raise(interp, "deduce/retract-reference-integrity-out-of-memory",
            "deduce: reference integrity check ran out of memory");
    }
    defer mem::free(decoded.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/retract-reference-integrity-scan-failed",
            "deduce: reference integrity scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }

        Value* ref_err = deduce_assert_retract_check_reference_integrity(rel, txn, decoded, &existing_key, interp);
        if (ref_err != null) return ref_err;

        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
    }

    return null;
}
