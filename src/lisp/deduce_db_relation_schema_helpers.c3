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
foreach (item : prefix) {
        buf[cursor++] = item;
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
