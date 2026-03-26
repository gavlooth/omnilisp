module lisp;

import std::core::mem;

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

fn Value* deduce_build_reference_target_in_use_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId target_column,
    usz target_column_pos,
    SymbolId blocking_relation,
    SymbolId blocking_column,
    Value*[] blocking_tuple,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
    explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("reference-target-in-use"));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(target_column, interp));
    usz[1] positions = { target_column_pos };
    explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp));
    explain_dict_set(data, interp, "key-value",
        target_column_pos < attempted_tuple.len ? attempted_tuple[target_column_pos] : make_nil(interp));
    explain_dict_set_symbol(data, interp, "referenced-column", target_column);
    explain_dict_set_symbol(data, interp, "blocking-relation", blocking_relation);
    explain_dict_set_symbol(data, interp, "blocking-column", blocking_column);
    explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    explain_dict_set(data, interp, "blocking-tuple", deduce_build_tuple_list(blocking_tuple, interp));
    return data;
}

fn Value* deduce_build_check_integrity_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    SymbolId predicate,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
    explain_dict_set_symbol(data, interp, "violation-kind", interp.symbols.intern("check-failed"));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set(data, interp, "columns", deduce_build_integrity_column_list(local_column, interp));
    usz[1] positions = { local_column_pos };
    explain_dict_set(data, interp, "constraint-values", deduce_build_tuple_projection_list(attempted_tuple, positions[..], interp));
    explain_dict_set(data, interp, "key-value",
        local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp));
    explain_dict_set_symbol(data, interp, "predicate", predicate);
    explain_dict_set(data, interp, "enforced", explain_bool_value(interp, true));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    return data;
}

fn Value* deduce_build_check_constraint_context_payload(
    Relation* rel,
    Value*[] attempted_tuple,
    SymbolId local_column,
    usz local_column_pos,
    SymbolId predicate,
    Value* cause,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 16);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) return null;

    explain_dict_set_symbol(data, interp, "kind", interp.symbols.intern("deduce-check-constraint"));
    explain_dict_set_symbol(data, interp, "relation", rel.name);
    explain_dict_set_symbol(data, interp, "column", local_column);
    explain_dict_set_symbol(data, interp, "predicate", predicate);
    explain_dict_set(data, interp, "value",
        local_column_pos < attempted_tuple.len ? attempted_tuple[local_column_pos] : make_nil(interp));
    explain_dict_set(data, interp, "attempted-tuple", deduce_build_tuple_list(attempted_tuple, interp));
    explain_dict_set(data, interp, "cause", cause != null ? cause : make_nil(interp));
    return data;
}

fn bool deduce_check_constraint_callable(Value* value) @inline {
    if (value == null) return false;
    switch (value.tag) {
        case CLOSURE:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
            return true;
        default:
            return false;
    }
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

fn Relation deduce_relation_view_from_schema(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    MdbDbi dbi
) @inline {
    return {
        .name = schema != null ? schema.name : (SymbolId)0,
        .columns = schema != null ? schema.columns : null,
        .column_key_values = null,
        .col_count = schema != null ? schema.arity : 0,
        .col_capacity = schema != null ? schema.arity : 0,
        .dbi = dbi,
        .db = db,
        .key_col = schema != null ? schema.key_column : (SymbolId)0,
        .unique_col = schema != null ? schema.unique_column : (SymbolId)0,
        .unique_cols = schema != null ? schema.unique_columns : null,
        .unique_col_count = schema != null ? schema.unique_column_count : 0,
        .unique_col_capacity = schema != null ? schema.unique_column_count : 0,
        .reference_constraints = schema != null ? schema.reference_constraints : null,
        .reference_constraint_count = schema != null ? schema.reference_constraint_count : 0,
        .reference_constraint_capacity = schema != null ? schema.reference_constraint_count : 0,
        .check_constraints = schema != null ? schema.check_constraints : null,
        .check_constraint_count = schema != null ? schema.check_constraint_count : 0,
        .check_constraint_capacity = schema != null ? schema.check_constraint_count : 0,
        .index_col = schema != null ? schema.index_column : (SymbolId)0,
        .materialized_view = schema != null && schema.materialized_view,
        .materialized_refresh_policy = schema != null ? schema.materialized_refresh_policy : (SymbolId)0,
        .materialized_refresh_count = schema != null ? schema.materialized_refresh_count : 0,
        .materialized_last_refresh_mutation_epoch =
            schema != null ? schema.materialized_last_refresh_mutation_epoch : 0,
        .materialized_last_stale_mutation_epoch =
            schema != null ? schema.materialized_last_stale_mutation_epoch : 0,
        .materialized_last_stale_reason =
            schema != null ? schema.materialized_last_stale_reason : DEDUCE_MATERIALIZED_STALE_NONE,
        .dropped = schema != null && schema.dropped,
    };
}

fn Value* deduce_assert_schema_write_integrity(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    MdbTxn* txn,
    MdbDbi dbi,
    Value*[] tuple_vals,
    MdbVal* encoded_key,
    Interp* interp
) {
    if (db == null || schema == null || txn == null || encoded_key == null || interp == null) return null;
    if (deduce_relation_schema_constraint_count(schema) == 0) return null;

    Relation rel = deduce_relation_view_from_schema(db, schema, dbi);

    Value* key_err = deduce_assert_fact_check_key_integrity(&rel, txn, tuple_vals, encoded_key, interp);
    if (key_err != null) return key_err;

    Value* unique_err = deduce_assert_fact_check_unique_integrity(&rel, txn, tuple_vals, encoded_key, interp);
    if (unique_err != null) return unique_err;

    Value* check_err = deduce_assert_fact_check_constraint_integrity_all(&rel, tuple_vals, interp);
    if (check_err != null) return check_err;

    return deduce_assert_fact_check_reference_integrity_all(&rel, txn, tuple_vals, interp);
}

fn Value* deduce_assert_schema_derived_write_integrity(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    MdbTxn* txn,
    MdbDbi dbi,
    Value*[] tuple_vals,
    MdbVal* encoded_key,
    Interp* interp
) {
    if (db == null || schema == null || txn == null || encoded_key == null || interp == null) return null;
    if (deduce_relation_schema_constraint_count(schema) == 0) return null;

    Relation rel = deduce_relation_view_from_schema(db, schema, dbi);

    Value* key_err = deduce_assert_fact_check_key_integrity(&rel, txn, tuple_vals, encoded_key, interp);
    if (key_err != null) return key_err;

    Value* unique_err = deduce_assert_fact_check_unique_integrity(&rel, txn, tuple_vals, encoded_key, interp);
    if (unique_err != null) return unique_err;

    return deduce_assert_fact_check_constraint_integrity_all(&rel, tuple_vals, interp);
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
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
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
        return null;
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
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
        }
        if (decoded_count != schema.arity) {
            rc = mdb_cursor_get(cursor, &existing_key, &existing_data, MDB_NEXT);
            continue;
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
