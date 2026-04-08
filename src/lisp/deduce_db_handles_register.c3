module lisp;

import std::core::mem;

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
