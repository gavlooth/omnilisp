module lisp;

fn Value* deduce_stats_set_materialized_and_read_fields(
    Value* out,
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    DeduceMaterializedStaleReason effective_stale_reason =
        deduce_relation_schema_effective_materialized_stale_reason(schema);
    bool materialized_ready = deduce_relation_schema_is_materialized_ready(rel.db, schema);
    if (Value* err = explain_dict_set(out, interp, "materialized-view", schema.materialized_view ? make_symbol(interp, interp.sym_true) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-derived-ready", schema.materialized_view ? (materialized_ready ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false)) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-refresh-policy", schema.materialized_refresh_policy != (SymbolId)0 ? make_symbol(interp, schema.materialized_refresh_policy) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-refresh-count", make_int(interp, (long)schema.materialized_refresh_count))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-last-refresh-mutation-epoch", schema.materialized_refresh_count > 0 ? make_int(interp, (long)schema.materialized_last_refresh_mutation_epoch) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-last-stale-mutation-epoch", effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE && effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED ? make_int(interp, (long)schema.materialized_last_stale_mutation_epoch) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-last-stale-reason", deduce_materialized_stale_reason_value(effective_stale_reason, interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "materialized-stale", deduce_materialized_stale_value(rel.db, schema, interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-execution-path", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? make_symbol(interp, schema.last_goal_directed_read_execution_path) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-surface", schema.last_goal_directed_read_surface != (SymbolId)0 ? make_symbol(interp, schema.last_goal_directed_read_surface) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason", schema.last_goal_directed_read_fallback_reason != (SymbolId)0 ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index", schema.last_goal_directed_read_selector_rule_index >= 0 ? make_int(interp, schema.last_goal_directed_read_selector_rule_index) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? deduce_bound_positions_payload( schema.last_goal_directed_read_requested_bound_positions, schema.last_goal_directed_read_requested_bound_count, interp ) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions", schema.last_goal_directed_read_execution_path != (SymbolId)0 ? deduce_bound_positions_payload( schema.last_goal_directed_read_applied_bound_positions, schema.last_goal_directed_read_applied_bound_count, interp ) : make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "last-goal-directed-read-step-counters", schema.last_goal_directed_read_execution_path != (SymbolId)0 && schema.last_goal_directed_read_step_count > 0 ? deduce_explain_observed_step_counter_payload( schema.last_goal_directed_read_step_counters, schema.last_goal_directed_read_step_count, interp ) : make_nil(interp))) return err;
    return null;
}
