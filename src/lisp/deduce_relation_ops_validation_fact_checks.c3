module lisp;

import std::core::mem;

// Test seam: force fact reference scan termination down the storage-error path.
bool g_deduce_force_fact_reference_scan_end_fail = false;

fn Value* deduce_assert_fact_check_reference_integrity(
    Relation* rel,
    MdbTxn* txn,
    Value*[] tuple_vals,
    DeduceReferenceConstraint* ref_constraint,
    Interp* interp
) {
    if (rel == null || txn == null || interp == null || ref_constraint == null) return null;

    usz local_pos = 0;
    if (!deduce_find_column_position(rel.columns, rel.col_count, ref_constraint.local_column, &local_pos)) return null;
    if (local_pos >= tuple_vals.len) return null;

    DeduceRelationSchema* target_schema = deduce_db_lookup_relation_schema(rel.db, ref_constraint.target_relation);
    if (target_schema == null) {
        Value* data = deduce_build_reference_integrity_payload(
            rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-target-missing", interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
            "deduce/integrity-reference-target-missing",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-reference-target-missing",
            "fact!: reference integrity target relation is not declared",
            data
        );
    }

    usz target_pos = 0;
    if (!deduce_find_column_position(target_schema.columns, target_schema.arity, ref_constraint.target_column, &target_pos)) {
        Value* data = deduce_build_reference_integrity_payload(
            rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-target-column-missing", interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
            "deduce/integrity-reference-target-column-missing",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-reference-target-column-missing",
            "fact!: reference integrity target column is not declared",
            data
        );
    }

    if (!deduce_relation_schema_column_is_candidate_key(target_schema, ref_constraint.target_column)) {
        Value* data = deduce_build_reference_integrity_payload(
            rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-target-not-unique", interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
            "deduce/integrity-reference-target-not-unique",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-reference-target-not-unique",
            "fact!: reference integrity target column is not a key or unique column",
            data
        );
    }

    Value*[] decoded = {};
    if (target_schema.arity > 0) {
        decoded = mem::new_array(Value*, target_schema.arity);
        if (decoded.ptr == null) {
            return deduce_raise(interp, "deduce/fact-reference-integrity-out-of-memory",
                "fact!: reference integrity check ran out of memory");
        }
    }
    defer if (decoded.ptr != null) mem::free(decoded.ptr);

    MdbDbi target_dbi = 0;
    if (!deduce_open_relation_dbi_by_symbol(rel.db, txn, ref_constraint.target_relation, &target_dbi, interp)) {
        Value* data = deduce_build_reference_integrity_payload(
            rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-target-missing", interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
            "deduce/integrity-reference-target-missing",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-reference-target-missing",
            "fact!: reference integrity target relation is not available",
            data
        );
    }

    bool found = false;
    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, target_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/fact-reference-integrity-scan-failed",
            "fact!: reference integrity scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            return deduce_raise(interp, "deduce/fact-reference-integrity-scan-failed",
                "fact!: reference integrity tuple decode failed");
        }
        if (decoded_count != target_schema.arity) {
            return deduce_raise(interp, "deduce/fact-reference-integrity-scan-failed",
                "fact!: reference integrity tuple arity mismatch");
        }
        if (target_pos < decoded.len &&
            deduce_compare_value(decoded[target_pos], tuple_vals[local_pos]) == 0) {
            found = true;
            break;
        }
        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
    }

    if (found) return null;
    if (g_deduce_force_fact_reference_scan_end_fail || rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/fact-reference-integrity-scan-failed",
            "fact!: reference integrity scan failed");
    }

    Value* data = deduce_build_reference_integrity_payload(
        rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-missing", interp);
    deduce_db_note_integrity_violation(
        rel,
        DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
        "deduce/integrity-reference-missing",
        interp
    );
    return deduce_raise(
        interp,
        "deduce/integrity-reference-missing",
        "fact!: reference integrity target tuple was not found",
        data
    );
}

fn Value* deduce_assert_fact_check_reference_integrity_all(
    Relation* rel,
    MdbTxn* txn,
    Value*[] tuple_vals,
    Interp* interp
) {
    if (rel == null || txn == null || interp == null) return null;
    if (rel.reference_constraint_count == 0 || rel.reference_constraints == null || rel.columns == null || rel.col_count == 0) return null;

    for (usz ref_i = 0; ref_i < rel.reference_constraint_count; ref_i++) {
        Value* err = deduce_assert_fact_check_reference_integrity(rel, txn, tuple_vals, &rel.reference_constraints[ref_i], interp);
        if (err != null) return err;
    }

    return null;
}

fn Value* deduce_assert_fact_check_constraint_integrity(
    Relation* rel,
    Value*[] tuple_vals,
    DeduceCheckConstraint* check_constraint,
    Interp* interp
) {
    if (rel == null || tuple_vals.ptr == null || interp == null || check_constraint == null) return null;
    if (check_constraint.local_column == (SymbolId)0 || check_constraint.predicate == (SymbolId)0) return null;

    usz local_pos = 0;
    if (!deduce_find_column_position(rel.columns, rel.col_count, check_constraint.local_column, &local_pos)) return null;
    if (local_pos >= tuple_vals.len) return null;

    Value* predicate = interp.global_env.lookup(check_constraint.predicate);
    if (predicate == null) {
        Value* data = deduce_build_check_constraint_context_payload(
            rel, tuple_vals, check_constraint.local_column, local_pos, check_constraint.predicate, null, interp);
        return deduce_raise(
            interp,
            "deduce/check-predicate-missing",
            "deduce check: predicate is not defined",
            data
        );
    }
    if (!deduce_check_constraint_callable(predicate)) {
        Value* data = deduce_build_check_constraint_context_payload(
            rel, tuple_vals, check_constraint.local_column, local_pos, check_constraint.predicate, predicate, interp);
        return deduce_raise(
            interp,
            "deduce/check-predicate-not-callable",
            "deduce check: predicate is not callable",
            data
        );
    }

    Value* result = jit_apply_value(predicate, tuple_vals[local_pos], interp);
    if (result != null && result.tag == ERROR) {
        Value* data = deduce_build_check_constraint_context_payload(
            rel, tuple_vals, check_constraint.local_column, local_pos, check_constraint.predicate, result, interp);
        return deduce_raise(
            interp,
            "deduce/check-predicate-raised",
            "deduce check: predicate evaluation failed",
            data
        );
    }
    if (!is_falsy(result, interp)) return null;

    Value* data = deduce_build_check_integrity_payload(
        rel, tuple_vals, check_constraint.local_column, local_pos, check_constraint.predicate, interp);
    deduce_db_note_integrity_violation(
        rel,
        DEDUCE_INTEGRITY_VIOLATION_CHECK,
        "deduce/integrity-check-failed",
        interp
    );
    return deduce_raise(
        interp,
        "deduce/integrity-check-failed",
        "deduce check: tuple rejected by check constraint",
        data
    );
}

fn Value* deduce_assert_fact_check_constraint_integrity_all(
    Relation* rel,
    Value*[] tuple_vals,
    Interp* interp
) {
    if (rel == null || interp == null) return null;
    if (rel.check_constraint_count == 0 || rel.check_constraints == null || rel.columns == null || rel.col_count == 0) return null;

    for (usz check_i = 0; check_i < rel.check_constraint_count; check_i++) {
        Value* err = deduce_assert_fact_check_constraint_integrity(rel, tuple_vals, &rel.check_constraints[check_i], interp);
        if (err != null) return err;
    }

    return null;
}
