module lisp;

fn Value* deduce_analyze_execute_with_plan(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    DeduceAnalyzeSelectedComponent* selected,
    DeduceObservedRuleCounters[] observed_rules,
    bool use_seminaive_recursive,
    SymbolId execution_engine_sym,
    SymbolId requested_execution_engine_sym,
    SymbolId incremental_invalidation_mode,
    long selector_rule_index,
    bool aggregate_recursive_fallback,
    Value* incremental_dirty_predicate_payload,
    DeduceAnalyzeExecutionSummary* summary,
    Interp* interp
) {
    const usz ANALYZE_ITERATION_LIMIT = 256;
    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* err = selected.selected_component_analysis
        ? deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            db,
            scc_plan,
            selected.selected_components,
            ANALYZE_ITERATION_LIMIT,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        )
        : deduce_naive_fixpoint_evaluate_with_plan(
            db,
            scc_plan,
            ANALYZE_ITERATION_LIMIT,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        );
    if (err != null) return err;

    summary.execution_engine_sym = execution_engine_sym;
    summary.requested_execution_engine_sym = requested_execution_engine_sym;
    summary.incremental_invalidation_mode = incremental_invalidation_mode;
    summary.selector_rule_index = selector_rule_index;
    summary.aggregate_recursive_fallback = aggregate_recursive_fallback;
    summary.iterations = iterations;
    summary.derived = derived;
    summary.max_component_iterations = max_component_iterations;
    summary.analyze_iteration_limit = (long)ANALYZE_ITERATION_LIMIT;
    summary.incremental_dirty_predicate_payload = incremental_dirty_predicate_payload;
    return null;
}
