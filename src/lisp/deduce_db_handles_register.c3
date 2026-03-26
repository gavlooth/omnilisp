module lisp;

import std::core::mem;

fn void deduce_relation_schema_release_storage(DeduceRelationSchema* schema) {
    if (schema == null) return;
    if (schema.indexes != null) {
        deduce_release_index_descriptors(schema.indexes, schema.index_count);
        mem::free(schema.indexes);
        schema.indexes = null;
    }
    if (schema.reference_constraints != null) {
        mem::free(schema.reference_constraints);
        schema.reference_constraints = null;
    }
    if (schema.check_constraints != null) {
        mem::free(schema.check_constraints);
        schema.check_constraints = null;
    }
    if (schema.unique_columns != null) {
        mem::free(schema.unique_columns);
        schema.unique_columns = null;
    }
    if (schema.columns != null) {
        mem::free(schema.columns);
        schema.columns = null;
    }
    schema.index_count = 0;
    schema.reference_constraint_count = 0;
    schema.check_constraint_count = 0;
    schema.unique_column_count = 0;
}

fn bool deduce_relation_schema_columns_match(
    DeduceRelationSchema* schema,
    SymbolId* columns,
    usz arity
) {
    if (schema == null) return false;
    if (schema.arity != arity) return false;
    if (arity == 0) return true;
    if (schema.columns == null || columns == null) return false;
    for (usz i = 0; i < arity; i++) {
        if (schema.columns[i] != columns[i]) return false;
    }
    return true;
}

fn SymbolId deduce_inferred_relation_column_symbol(usz index, Interp* interp) {
    if (interp == null) return (SymbolId)0;
    char[64] buf = {};
    usz cursor = 0;
    char[] prefix = "__deduce_col_";
    for (usz i = 0; i < prefix.len; i++) {
        buf[cursor++] = prefix[i];
    }
    cursor += deduce_format_usz_hex(buf[cursor..], index);
    return interp.symbols.intern(buf[..cursor]);
}

fn bool deduce_fill_inferred_relation_columns(SymbolId[] out_columns, Interp* interp) {
    if (out_columns.len == 0) return true;
    if (interp == null) return false;
    for (usz i = 0; i < out_columns.len; i++) {
        SymbolId col = deduce_inferred_relation_column_symbol(i, interp);
        if (col == (SymbolId)0) return false;
        out_columns[i] = col;
    }
    return true;
}

fn void deduce_relation_schema_apply_runtime_defaults(
    DeduceRelationSchema* schema,
    SymbolId name,
    usz arity,
    bool inferred_columns,
    SymbolId key_col,
    usz unique_col_count,
    usz reference_constraint_count,
    usz check_constraint_count,
    SymbolId index_col,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason
) {
    schema.name = name;
    schema.columns = null;
    schema.arity = arity;
    schema.inferred_columns = inferred_columns;
    schema.dropped = false;
    schema.materialized_view = materialized_view;
    schema.key_column = key_col;
    schema.unique_column = (SymbolId)0;
    schema.unique_columns = null;
    schema.unique_column_count = unique_col_count;
    schema.reference_constraints = null;
    schema.reference_constraint_count = reference_constraint_count;
    schema.check_constraints = null;
    schema.check_constraint_count = check_constraint_count;
    schema.index_column = index_col;
    schema.indexes = null;
    schema.index_count = 0;
    schema.cardinality_estimate = 0;
    schema.distinct_estimate = 0;
    schema.integrity_violation_count = 0;
    schema.key_integrity_violation_count = 0;
    schema.unique_integrity_violation_count = 0;
    schema.reference_integrity_violation_count = 0;
    schema.check_integrity_violation_count = 0;
    schema.last_integrity_violation_code = (SymbolId)0;
    schema.materialized_refresh_policy = materialized_view ? materialized_refresh_policy : (SymbolId)0;
    schema.materialized_refresh_count = materialized_view ? materialized_refresh_count : 0;
    schema.materialized_last_refresh_mutation_epoch =
        materialized_view ? materialized_last_refresh_mutation_epoch : 0;
    schema.materialized_last_stale_mutation_epoch =
        materialized_view ? materialized_last_stale_mutation_epoch : 0;
    schema.materialized_last_stale_reason =
        materialized_view ? materialized_last_stale_reason : DEDUCE_MATERIALIZED_STALE_NONE;
    schema.last_goal_directed_read_execution_path = (SymbolId)0;
    schema.last_goal_directed_read_surface = (SymbolId)0;
    schema.last_goal_directed_read_selector_rule_index = -1;
    schema.last_goal_directed_read_mutation_epoch = 0;
    schema.last_goal_directed_read_requested_bound_count = 0;
    schema.last_goal_directed_read_requested_bound_positions = null;
    schema.last_goal_directed_read_applied_bound_count = 0;
    schema.last_goal_directed_read_applied_bound_positions = null;
    schema.last_goal_directed_read_subject_keys = {};
    schema.last_goal_directed_read_subject_keys.complete = true;
    schema.last_goal_directed_read_step_counters = null;
    schema.last_goal_directed_read_step_count = 0;
    schema.integrity_violation_history_count = 0;
}

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
        schema.columns = (SymbolId*)mem::malloc(SymbolId.sizeof * arity);
        if (schema.columns == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < arity; i++) schema.columns[i] = columns[i];
    }

    if (unique_col_count > 0) {
        schema.unique_columns = (SymbolId*)mem::malloc(SymbolId.sizeof * unique_col_count);
        if (schema.unique_columns == null) {
            deduce_relation_schema_release_storage(schema);
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < unique_col_count; i++) schema.unique_columns[i] = unique_cols[i];
        schema.unique_column = unique_col_count == 1 ? schema.unique_columns[0] : (SymbolId)0;
    }

    if (reference_constraint_count > 0) {
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

    usz max_indexes = arity + 8;
    if (max_indexes > 0) {
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

fn void deduce_relation_schema_refresh_lifecycle(
    DeduceRelationSchema* schema,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason,
    bool inferred_columns
) {
    if (schema == null) return;
    schema.dropped = false;
    schema.inferred_columns = inferred_columns;
    schema.materialized_view = materialized_view;
    schema.materialized_refresh_policy = materialized_view ? materialized_refresh_policy : (SymbolId)0;
    schema.materialized_refresh_count = materialized_view ? materialized_refresh_count : 0;
    schema.materialized_last_refresh_mutation_epoch =
        materialized_view ? materialized_last_refresh_mutation_epoch : 0;
    schema.materialized_last_stale_mutation_epoch =
        materialized_view ? materialized_last_stale_mutation_epoch : 0;
    schema.materialized_last_stale_reason =
        materialized_view ? materialized_last_stale_reason : DEDUCE_MATERIALIZED_STALE_NONE;
}

fn DeduceSchemaRegisterStatus deduce_db_register_relation_schema_internal(
    DeduceDb* db,
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
    if (db == null || name == (SymbolId)0) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (arity > 0 && columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* entry = &db.relation_schemas[i];
        if (entry.name != name) continue;
        if (entry.arity != arity) return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
        if (deduce_relation_schema_columns_match(entry, columns, arity)) {
            deduce_relation_schema_refresh_lifecycle(
                entry,
                materialized_view,
                materialized_refresh_policy,
                materialized_refresh_count,
                materialized_last_refresh_mutation_epoch,
                materialized_last_stale_mutation_epoch,
                materialized_last_stale_reason,
                entry.inferred_columns && inferred_columns
            );
            return DEDUCE_SCHEMA_REGISTER_OK;
        }
        if (!entry.inferred_columns || inferred_columns) return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
        return deduce_relation_schema_init(
            entry,
            name,
            columns,
            arity,
            key_col,
            unique_cols,
            unique_col_count,
            reference_constraints,
            reference_constraint_count,
            check_constraints,
            check_constraint_count,
            index_col,
            materialized_view,
            materialized_refresh_policy,
            materialized_refresh_count,
            materialized_last_refresh_mutation_epoch,
            materialized_last_stale_mutation_epoch,
            materialized_last_stale_reason,
            false
        );
    }

    usz needed = db.relation_schema_count + 1;
    if (!deduce_db_ensure_relation_schema_capacity(db, needed)) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }

    DeduceRelationSchema* schema = &db.relation_schemas[db.relation_schema_count];
    *schema = {};
    DeduceSchemaRegisterStatus status = deduce_relation_schema_init(
        schema,
        name,
        columns,
        arity,
        key_col,
        unique_cols,
        unique_col_count,
        reference_constraints,
        reference_constraint_count,
        check_constraints,
        check_constraint_count,
        index_col,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason,
        inferred_columns
    );
    if (status != DEDUCE_SCHEMA_REGISTER_OK) return status;

    db.relation_schema_count++;
    return DEDUCE_SCHEMA_REGISTER_OK;
}

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
    DeduceCheckConstraint* check_constraints,
    usz check_constraint_count,
    SymbolId index_col,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason
) {
    return deduce_db_register_relation_schema_internal(
        db,
        name,
        columns,
        arity,
        key_col,
        unique_cols,
        unique_col_count,
        reference_constraints,
        reference_constraint_count,
        check_constraints,
        check_constraint_count,
        index_col,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason,
        false
    );
}

fn DeduceSchemaRegisterStatus deduce_db_register_inferred_relation_schema(
    DeduceDb* db,
    SymbolId name,
    usz arity,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason,
    Interp* interp
) {
    SymbolId[] inferred_columns = mem::new_array(SymbolId, arity);
    if (arity > 0 && inferred_columns.ptr == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    defer if (inferred_columns.ptr != null) mem::free(inferred_columns.ptr);

    if (!deduce_fill_inferred_relation_columns(inferred_columns, interp)) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }

    return deduce_db_register_relation_schema_internal(
        db,
        name,
        inferred_columns.ptr,
        arity,
        (SymbolId)0,
        null,
        0,
        null,
        0,
        null,
        0,
        (SymbolId)0,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason,
        true
    );
}
