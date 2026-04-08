module lisp;

struct DeduceAnalyzeExecutionSummary {
    SymbolId execution_engine_sym;
    SymbolId requested_execution_engine_sym;
    SymbolId incremental_invalidation_mode;
    long     selector_rule_index;
    bool     aggregate_recursive_fallback;
    bool     naive_recursive_mixed_aggregate_routed;
    long     goal_directed_recursive_component_count;
    long     goal_directed_eligible_component_count;
    long     goal_directed_blocked_component_count;
    long     goal_directed_aggregate_blocked_component_count;
    long     goal_directed_negated_blocked_component_count;
    long     parallel_recursive_component_count;
    long     parallel_batched_component_count;
    long     parallel_batch_count;
    long     parallel_max_batch_size;
    long     iterations;
    long     derived;
    long     max_component_iterations;
    long     recursive_strata;
    long     analyze_iteration_limit;
    Value*   incremental_dirty_predicate_payload;
}

fn void deduce_analyze_finish_incremental_tracking(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    DeduceAnalyzeSelectedComponent* selected
) {
    if (db == null || scc_plan == null || selected == null) return;
    if (selected.selected_component_analysis) {
        if (!db.incremental_full_recompute_required) {
            deduce_fixpoint_clear_selected_dirty_predicates(db, scc_plan, selected.selected_components);
        }
        return;
    }
    deduce_db_reset_incremental_tracking(db);
}
