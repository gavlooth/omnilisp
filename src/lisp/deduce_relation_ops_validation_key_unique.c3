module lisp;

import std::core::mem;

// Test seam: force immediate key/unique integrity scan termination down storage-error paths.
bool g_deduce_force_immediate_integrity_scan_end_fail = false;

fn Value* deduce_assert_fact_check_key_integrity(
    Relation* rel,
    MdbTxn* txn,
    Value*[] tuple_vals,
    MdbVal* tuple_key,
    Interp* interp
) {
    if (rel == null || txn == null || tuple_key == null || interp == null) return null;
    if (rel.key_col == (SymbolId)0 || rel.columns == null || rel.col_count == 0) return null;

    usz key_pos = 0;
    if (!deduce_find_column_position(rel.columns, rel.col_count, rel.key_col, &key_pos)) return null;
    if (key_pos >= tuple_vals.len) return null;

    Value*[] decoded = {};
    if (rel.col_count > 0) {
        decoded = mem::new_array(Value*, rel.col_count);
        if (decoded.ptr == null) {
            return deduce_raise(interp, "deduce/fact-key-integrity-out-of-memory",
                "fact!: keyed relation integrity check ran out of memory");
        }
    }
    defer if (decoded.ptr != null) mem::free(decoded.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/fact-key-integrity-scan-failed",
            "fact!: keyed relation integrity scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            return deduce_raise(interp, "deduce/fact-key-integrity-scan-failed",
                "fact!: keyed relation integrity tuple decode failed");
        }
        if (decoded_count != rel.col_count) {
            return deduce_raise(interp, "deduce/fact-key-integrity-scan-failed",
                "fact!: keyed relation integrity tuple arity mismatch");
        }
        if (deduce_compare_value(decoded[key_pos], tuple_vals[key_pos]) != 0) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (deduce_encoded_tuple_key_equals(&existing_key, tuple_key)) return null;

        Value* data = deduce_build_key_integrity_payload(rel, tuple_vals, decoded, key_pos, interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_KEY,
            "deduce/integrity-key-conflict",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-key-conflict",
            "fact!: keyed relation write conflicts with existing tuple",
            data
        );
    }
    if (g_deduce_force_immediate_integrity_scan_end_fail || rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/fact-key-integrity-scan-failed",
            "fact!: keyed relation integrity scan failed");
    }

    return null;
}

fn Value* deduce_assert_fact_check_unique_integrity(
    Relation* rel,
    MdbTxn* txn,
    Value*[] tuple_vals,
    MdbVal* tuple_key,
    Interp* interp
) {
    if (rel == null || txn == null || tuple_key == null || interp == null) return null;
    if (rel.unique_col_count == 0 || rel.unique_cols == null || rel.columns == null || rel.col_count == 0) return null;
    if (deduce_unique_constraint_equivalent_to_key(rel)) return null;

    usz[] unique_positions = mem::new_array(usz, rel.unique_col_count);
    if (unique_positions.ptr == null) {
        return deduce_raise(interp, "deduce/fact-unique-integrity-out-of-memory",
            "fact!: unique relation integrity check ran out of memory");
    }
    defer mem::free(unique_positions.ptr);

    for (usz i = 0; i < rel.unique_col_count; i++) {
        if (!deduce_find_column_position(rel.columns, rel.col_count, rel.unique_cols[i], &unique_positions[i])) {
            return null;
        }
        if (unique_positions[i] >= tuple_vals.len) return null;
    }

    Value*[] decoded = {};
    if (rel.col_count > 0) {
        decoded = mem::new_array(Value*, rel.col_count);
        if (decoded.ptr == null) {
            return deduce_raise(interp, "deduce/fact-unique-integrity-out-of-memory",
                "fact!: unique relation integrity check ran out of memory");
        }
    }
    defer if (decoded.ptr != null) mem::free(decoded.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/fact-unique-integrity-scan-failed",
            "fact!: unique relation integrity scan failed");
    }
    defer if (cursor != null) mdb_cursor_close(cursor);

    MdbVal existing_key = {};
    MdbVal existing_data = {};
    rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)existing_key.mv_data, existing_key.mv_size, decoded, interp);
        if (catch err = decoded_count) {
            return deduce_raise(interp, "deduce/fact-unique-integrity-scan-failed",
                "fact!: unique relation integrity tuple decode failed");
        }
        if (decoded_count != rel.col_count) {
            return deduce_raise(interp, "deduce/fact-unique-integrity-scan-failed",
                "fact!: unique relation integrity tuple arity mismatch");
        }
        if (!deduce_tuples_match_at_positions(decoded, tuple_vals, unique_positions)) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (deduce_encoded_tuple_key_equals(&existing_key, tuple_key)) return null;

        Value* data = rel.unique_col_count == 1
            ? deduce_build_unique_integrity_payload(rel, tuple_vals, decoded, unique_positions[0], interp)
            : deduce_build_composite_unique_integrity_payload(rel, tuple_vals, decoded, unique_positions, interp);
        deduce_db_note_integrity_violation(
            rel,
            DEDUCE_INTEGRITY_VIOLATION_UNIQUE,
            "deduce/integrity-unique-conflict",
            interp
        );
        return deduce_raise(
            interp,
            "deduce/integrity-unique-conflict",
            "fact!: unique relation write conflicts with existing tuple",
            data
        );
    }
    if (g_deduce_force_immediate_integrity_scan_end_fail || rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/fact-unique-integrity-scan-failed",
            "fact!: unique relation integrity scan failed");
    }

    return null;
}
