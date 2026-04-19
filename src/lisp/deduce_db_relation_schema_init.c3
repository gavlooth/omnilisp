module lisp;

import std::core::mem;

fn DeduceSchemaRegisterStatus deduce_relation_schema_init(
    DeduceRelationSchema* schema,
    SymbolId name,
    SymbolId* columns,
    usz arity,
    SymbolId key_col,
    SymbolId* unique_cols,
    usz unique_col_count,
    DeduceReferenceConstraint* reference_constraints,
    usz reference_constraint_count,
    DeduceCheckConstraint* check_constraints,
    usz check_constraint_count,
    SymbolId index_col,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason,
    bool inferred_columns
) {
    if (schema == null || name == (SymbolId)0) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (arity > 0 && columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (unique_col_count > 0 && unique_cols == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (reference_constraint_count > 0 && reference_constraints == null) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }
    if (check_constraint_count > 0 && check_constraints == null) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }

    deduce_relation_schema_release_storage(schema);
    deduce_relation_schema_apply_runtime_defaults(
        schema,
        name,
        arity,
        inferred_columns,
        key_col,
        unique_col_count,
        reference_constraint_count,
        check_constraint_count,
        index_col,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason
    );

    if (arity > 0) {
        if (arity > usz.max / SymbolId.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        schema.columns = (SymbolId*)mem::malloc(SymbolId.sizeof * arity);
        if (schema.columns == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < arity; i++) schema.columns[i] = columns[i];
    }

    if (unique_col_count > 0) {
        if (unique_col_count > usz.max / SymbolId.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        schema.unique_columns = (SymbolId*)mem::malloc(SymbolId.sizeof * unique_col_count);
        if (schema.unique_columns == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < unique_col_count; i++) schema.unique_columns[i] = unique_cols[i];
        schema.unique_column = unique_col_count == 1 ? schema.unique_columns[0] : (SymbolId)0;
    }

    if (reference_constraint_count > 0) {
        if (reference_constraint_count > usz.max / DeduceReferenceConstraint.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        schema.reference_constraints =
            (DeduceReferenceConstraint*)mem::malloc(DeduceReferenceConstraint.sizeof * reference_constraint_count);
        if (schema.reference_constraints == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < reference_constraint_count; i++) {
            schema.reference_constraints[i] = reference_constraints[i];
        }
    }

    if (check_constraint_count > 0) {
        if (check_constraint_count > usz.max / DeduceCheckConstraint.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        schema.check_constraints =
            (DeduceCheckConstraint*)mem::malloc(DeduceCheckConstraint.sizeof * check_constraint_count);
        if (schema.check_constraints == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < check_constraint_count; i++) {
            schema.check_constraints[i] = check_constraints[i];
        }
    }

    if (arity > usz.max - (usz)8) {
        deduce_relation_schema_release_storage(schema);
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }
    usz max_indexes = arity + 8;
    if (max_indexes > 0) {
        if (max_indexes > usz.max / DeduceIndexDescriptor.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        schema.indexes = (DeduceIndexDescriptor*)mem::malloc(DeduceIndexDescriptor.sizeof * max_indexes);
        if (schema.indexes == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < max_indexes; i++) {
            schema.indexes[i].column_positions = null;
            schema.indexes[i].column_count = 0;
            schema.indexes[i].unique = false;
        }
    }

    if (arity > 0) {
        if (arity > usz.max / usz.sizeof) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        usz[] full_positions = mem::new_array(usz, arity);
        if (full_positions.ptr == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        defer mem::free(full_positions.ptr);
        for (usz i = 0; i < arity; i++) full_positions[i] = i;

        if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, full_positions, true)) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }

        for (usz i = 0; i < arity; i++) {
            usz[1] single = { i };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, single[..], false)) {
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz key_pos = 0;
        if (deduce_find_column_position(columns, arity, key_col, &key_pos)) {
            usz[1] key_single = { key_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_single[..], true)) {
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        bool unique_equivalent_to_key =
            unique_col_count == 1 && key_col != (SymbolId)0 && unique_cols[0] == key_col;
        if (unique_col_count > 0 && !unique_equivalent_to_key) {
            if (unique_col_count > usz.max / usz.sizeof) {
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
            usz[] unique_positions = mem::new_array(usz, unique_col_count);
            if (unique_positions.ptr == null) {
                deduce_relation_schema_release_storage(schema);
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
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz idx_pos = 0;
        if (deduce_find_column_position(columns, arity, index_col, &idx_pos)) {
            usz[1] idx_single = { idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, idx_single[..], false)) {
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        if (deduce_find_column_position(columns, arity, key_col, &key_pos) &&
            deduce_find_column_position(columns, arity, index_col, &idx_pos) &&
            key_pos != idx_pos) {
            usz[2] key_index_pair = { key_pos, idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_index_pair[..], false)) {
                deduce_relation_schema_release_storage(schema);
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }
    }

    return DEDUCE_SCHEMA_REGISTER_OK;
}
