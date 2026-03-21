module lisp;

import std::core::mem;

// ============================================================
// Relation write/read operations
// ============================================================
fn bool deduce_encoded_tuple_key_equals(MdbVal* left, MdbVal* right) @inline {
    if (left == null || right == null) return false;
    if (left.mv_size != right.mv_size) return false;
    char* left_bytes = (char*)left.mv_data;
    char* right_bytes = (char*)right.mv_data;
    for (usz i = 0; i < left.mv_size; i++) {
        if (left_bytes[i] != right_bytes[i]) return false;
    }
    return true;
}

fn Value* deduce_build_tuple_list(Value*[] tuple, Interp* interp) {
    Value* rev = make_nil(interp);
    foreach (value : tuple) {
        rev = make_cons(interp, value == null ? make_nil(interp) : value, rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_build_integrity_column_list(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_cons(interp, make_symbol(interp, column), make_nil(interp));
}

fn Value* deduce_build_integrity_columns_list(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_build_tuple_projection_list(Value*[] tuple, usz[] positions, Interp* interp) {
    if (positions.len == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < positions.len; i++) {
        usz pos = positions[i];
        Value* value = pos < tuple.len ? tuple[pos] : make_nil(interp);
        rev = make_cons(interp, value == null ? make_nil(interp) : value, rev);
    }
    return deduce_reverse_list(rev, interp);
}

fn Value* deduce_build_single_column_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    SymbolId column,
    usz column_pos,
    char[] violation_kind_name,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
    explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern(violation_kind_name));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(column, interp));
    usz[1] positions = { column_pos };
    explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp));
    explain_dict_set(data, interp, "key-value",
        column_pos < attempted_tuple.len ? attempted_tuple[column_pos] : make_nil(interp));
    explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true));
    explain_dict_set(data, interp, "existing-tuple", deduce_build_tuple_list(existing_tuple, interp));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    return data;
}

fn Value* deduce_build_key_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz key_pos,
    Interp* interp
) {
    Value* data = deduce_build_single_column_integrity_payload(
        rel,
        attempted_tuple,
        existing_tuple,
        rel.key_col,
        key_pos,
        "key-conflict",
        interp
    );
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return data;
    explain_dict_set_symbol(data, interp, "key-column", rel.key_col);
    return data;
}

fn Value* deduce_build_unique_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz unique_pos,
    Interp* interp
) {
    Value* data = deduce_build_single_column_integrity_payload(
        rel,
        attempted_tuple,
        existing_tuple,
        rel.unique_col,
        unique_pos,
        "unique-conflict",
        interp
    );
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return data;
    explain_dict_set_symbol(data, interp, "unique-column", rel.unique_col);
    return data;
}

fn Value* deduce_build_composite_unique_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    Value*[] existing_tuple,
    usz[] positions,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;
    Value* constraint_values = deduce_build_tuple_projection_list(attempted_tuple, positions, interp);

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
    explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("unique-conflict"));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set(data, interp, "columns", deduce_build_integrity_columns_list(rel.unique_cols, rel.unique_col_count, interp));
    explain_dict_set(data, interp, "constraint-values", constraint_values);
    explain_dict_set(data, interp, "key-value", constraint_values);
    explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true));
    explain_dict_set(data, interp, "existing-tuple", deduce_build_tuple_list(existing_tuple, interp));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    explain_dict_set(data, interp, "unique-columns", deduce_build_integrity_columns_list(rel.unique_cols, rel.unique_col_count, interp));
    return data;
}

fn Value* deduce_build_reference_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    DeduceReferenceConstraint* ref_constraint,
    char[] violation_kind_name,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
    explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern(violation_kind_name));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(local_column, interp));
    usz[1] positions = { local_column_pos };
    explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp));
    explain_dict_set(data, interp, "key-value",
        local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp));
    explain_dict_set_symbol(data, interp, "referenced-relation", ref_constraint.target_relation);
    explain_dict_set_symbol(data, interp, "referenced-column", ref_constraint.target_column);
    explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    return data;
}

fn bool deduce_unique_constraint_equivalent_to_key(Relation* rel) @inline {
    return rel != null &&
        rel.key_col != (SymbolId)0 &&
        rel.unique_col_count == 1 &&
        rel.unique_cols != null &&
        rel.unique_cols[0] == rel.key_col;
}

fn bool deduce_tuples_match_at_positions(Value*[] left, Value*[] right, usz[] positions) @inline {
    for (usz i = 0; i < positions.len; i++) {
        usz pos = positions[i];
        if (pos >= left.len || pos >= right.len) return false;
        if (deduce_compare_value(left[pos], right[pos]) != 0) return false;
    }
    return true;
}

fn bool deduce_open_relation_dbi_by_symbol(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId relation_name,
    MdbDbi* dbi_out,
    Interp* interp
) {
    if (db == null || txn == null || dbi_out == null || interp == null || relation_name == (SymbolId)0) return false;
    char[] name_str = interp.symbols.get_name(relation_name);
    char* dbi_name = deduce_copy_c_string(name_str);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    return mdb_dbi_open(txn, dbi_name, 0, dbi_out) == MDB_SUCCESS;
}

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
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (deduce_compare_value(decoded[key_pos], tuple_vals[key_pos]) != 0) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (deduce_encoded_tuple_key_equals(&existing_key, tuple_key)) return null;

        Value* data = deduce_build_key_integrity_payload(rel, tuple_vals, decoded, key_pos, interp);
        return deduce_raise(
            interp,
            "deduce/integrity-key-conflict",
            "fact!: keyed relation write conflicts with existing tuple",
            data
        );
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
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (!deduce_tuples_match_at_positions(decoded, tuple_vals, unique_positions)) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (deduce_encoded_tuple_key_equals(&existing_key, tuple_key)) return null;

        Value* data = rel.unique_col_count == 1
            ? deduce_build_unique_integrity_payload(rel, tuple_vals, decoded, unique_positions[0], interp)
            : deduce_build_composite_unique_integrity_payload(rel, tuple_vals, decoded, unique_positions, interp);
        return deduce_raise(
            interp,
            "deduce/integrity-unique-conflict",
            "fact!: unique relation write conflicts with existing tuple",
            data
        );
    }

    return null;
}

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
        return deduce_raise(
            interp,
            "deduce/integrity-reference-target-column-missing",
            "fact!: reference integrity target column is not declared",
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
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != target_schema.arity) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (target_pos < decoded.len &&
            deduce_compare_value(decoded[target_pos], tuple_vals[local_pos]) == 0) {
            found = true;
            break;
        }
        rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
    }

    if (found) return null;

    Value* data = deduce_build_reference_integrity_payload(
        rel, tuple_vals, ref_constraint.local_column, local_pos, ref_constraint, "reference-missing", interp);
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
        Value* key_err = deduce_assert_fact_check_key_integrity(rel, txn.txn, args[tuple_start..], &key, interp);
        if (key_err != null) return key_err;
        Value* unique_err = deduce_assert_fact_check_unique_integrity(rel, txn.txn, args[tuple_start..], &key, interp);
        if (unique_err != null) return unique_err;
        Value* ref_err = deduce_assert_fact_check_reference_integrity_all(rel, txn.txn, args[tuple_start..], interp);
        if (ref_err != null) return ref_err;
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
        if (!existed && !deduce_txn_log_insert(txn, rel)) {
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
        deduce_db_note_mutation(rel, false);
    }
    return make_void(interp);
}

// ============================================================
// (retract! relation val1 val2 ...) → Void
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
        deduce_db_note_mutation(rel, true);
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
    deduce_db_note_mutation(rel, true);
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
    deduce_db_note_mutation(rel, true);
    rel.dropped = true;
    return make_void(interp);
}
