module lisp;

import std::core::mem;

fn DeduceSchemaRegisterStatus deduce_db_register_relation_schema(
    DeduceDb* db,
    SymbolId name,
    SymbolId* columns,
    usz arity,
    SymbolId key_col,
    SymbolId* unique_cols,
    usz unique_col_count,
    DeduceReferenceConstraint* reference_constraints,
    usz reference_constraint_count,
    SymbolId index_col
) {
    if (db == null || name == (SymbolId)0) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (arity > 0 && columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* entry = &db.relation_schemas[i];
        if (entry.name != name) continue;
        if (entry.arity != arity) return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
        for (usz c = 0; c < arity; c++) {
            if (entry.columns == null || entry.columns[c] != columns[c]) {
                return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
            }
        }
        return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
    }

    usz needed = db.relation_schema_count + 1;
    if (!deduce_db_ensure_relation_schema_capacity(db, needed)) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }

    DeduceRelationSchema* schema = &db.relation_schemas[db.relation_schema_count];
    schema.name = name;
    schema.columns = null;
    schema.arity = arity;
    schema.key_column = key_col;
    schema.unique_column = unique_col_count == 1 ? unique_cols[0] : (SymbolId)0;
    schema.unique_columns = null;
    schema.unique_column_count = unique_col_count;
    schema.reference_constraints = null;
    schema.reference_constraint_count = reference_constraint_count;
    schema.index_column = index_col;
    schema.indexes = null;
    schema.index_count = 0;
    schema.cardinality_estimate = 0;
    schema.distinct_estimate = 0;

    if (arity > 0) {
        schema.columns = (SymbolId*)mem::malloc(SymbolId.sizeof * arity);
        if (schema.columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        for (usz i = 0; i < arity; i++) schema.columns[i] = columns[i];
    }

    if (unique_col_count > 0) {
        schema.unique_columns = (SymbolId*)mem::malloc(SymbolId.sizeof * unique_col_count);
        if (schema.unique_columns == null) {
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < unique_col_count; i++) schema.unique_columns[i] = unique_cols[i];
    }

    if (reference_constraint_count > 0) {
        schema.reference_constraints =
            (DeduceReferenceConstraint*)mem::malloc(DeduceReferenceConstraint.sizeof * reference_constraint_count);
        if (schema.reference_constraints == null) {
            if (schema.unique_columns != null) {
                mem::free(schema.unique_columns);
                schema.unique_columns = null;
            }
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < reference_constraint_count; i++) {
            schema.reference_constraints[i] = reference_constraints[i];
        }
    }

    usz max_indexes = arity + 8;
    if (max_indexes > 0) {
        schema.indexes = (DeduceIndexDescriptor*)mem::malloc(DeduceIndexDescriptor.sizeof * max_indexes);
        if (schema.indexes == null) {
            if (schema.reference_constraints != null) {
                mem::free(schema.reference_constraints);
                schema.reference_constraints = null;
            }
            if (schema.unique_columns != null) {
                mem::free(schema.unique_columns);
                schema.unique_columns = null;
            }
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < max_indexes; i++) {
            schema.indexes[i].column_positions = null;
            schema.indexes[i].column_count = 0;
            schema.indexes[i].unique = false;
        }
    }

    if (arity > 0) {
        usz[] full_positions = mem::new_array(usz, arity);
        if (full_positions.ptr == null) {
            deduce_release_index_descriptors(schema.indexes, schema.index_count);
            if (schema.indexes != null) mem::free(schema.indexes);
            if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
            if (schema.unique_columns != null) mem::free(schema.unique_columns);
            if (schema.columns != null) mem::free(schema.columns);
            schema.indexes = null;
            schema.reference_constraints = null;
            schema.unique_columns = null;
            schema.columns = null;
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        defer mem::free(full_positions.ptr);
        for (usz i = 0; i < arity; i++) full_positions[i] = i;

            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, full_positions, true)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }

        for (usz i = 0; i < arity; i++) {
            usz[1] single = { i };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, single[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz key_pos = 0;
        if (deduce_find_column_position(columns, arity, key_col, &key_pos)) {
            usz[1] key_single = { key_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_single[..], true)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        bool unique_equivalent_to_key = unique_col_count == 1 && key_col != (SymbolId)0 && unique_cols[0] == key_col;
        if (unique_col_count > 0 && !unique_equivalent_to_key) {
            usz[] unique_positions = mem::new_array(usz, unique_col_count);
            if (unique_positions.ptr == null) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
            defer mem::free(unique_positions.ptr);

            bool all_unique_found = true;
            for (usz i = 0; i < unique_col_count; i++) {
                if (!deduce_find_column_position(columns, arity, unique_cols[i], &unique_positions[i])) {
                    all_unique_found = false;
                    break;
                }
            }
            if (all_unique_found &&
                !deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, unique_positions[..], true)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz idx_pos = 0;
        if (deduce_find_column_position(columns, arity, index_col, &idx_pos)) {
            usz[1] idx_single = { idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, idx_single[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        if (deduce_find_column_position(columns, arity, key_col, &key_pos) &&
            deduce_find_column_position(columns, arity, index_col, &idx_pos) &&
            key_pos != idx_pos) {
            usz[2] key_index_pair = { key_pos, idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_index_pair[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.reference_constraints != null) mem::free(schema.reference_constraints);
                if (schema.unique_columns != null) mem::free(schema.unique_columns);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.reference_constraints = null;
                schema.unique_columns = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }
    }

    db.relation_schema_count++;
    return DEDUCE_SCHEMA_REGISTER_OK;
}
