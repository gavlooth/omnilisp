module lisp;

fn Value* deduce_relation_schema_columns_list(DeduceRelationSchema* schema, Interp* interp) {
    if (schema == null || schema.columns == null || schema.arity == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.arity; i++) {
        rev = make_cons(interp, make_symbol(interp, schema.columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_index_columns_list(
    DeduceRelationSchema* schema,
    DeduceIndexDescriptor* index,
    Interp* interp
) {
    if (schema == null || index == null || index.column_positions == null || index.column_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < index.column_count; i++) {
        usz column_pos = index.column_positions[i];
        if (column_pos >= schema.arity || schema.columns == null) {
            rev = make_cons(interp, make_nil(interp), rev);
            continue;
        }
        rev = make_cons(interp, make_symbol(interp, schema.columns[column_pos]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_schema_role_column(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_symbol(interp, column);
}

fn Value* deduce_relation_schema_role_columns(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool deduce_relation_schema_unique_equivalent_to_key(DeduceRelationSchema* schema) @inline {
    return schema != null &&
        schema.key_column != (SymbolId)0 &&
        schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == schema.key_column;
}

fn bool deduce_relation_schema_column_is_candidate_key(DeduceRelationSchema* schema, SymbolId column) @inline {
    if (schema == null || column == (SymbolId)0) return false;
    if (schema.key_column == column) return true;
    return schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == column;
}

fn long deduce_relation_schema_constraint_count(DeduceRelationSchema* schema) @inline {
    if (schema == null) return 0;
    long count = 0;
    if (schema.key_column != (SymbolId)0) count++;
    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) count++;
    count += (long)schema.reference_constraint_count;
    count += (long)schema.check_constraint_count;
    return count;
}
