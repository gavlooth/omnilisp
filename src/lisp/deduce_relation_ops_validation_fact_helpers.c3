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
