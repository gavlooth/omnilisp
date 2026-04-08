module lisp;

import std::core::mem;

fn Value* deduce_analyze_run(DeduceDb* db, Value*[] args, Interp* interp) {
    DeduceAnalyzeCatalogSummary catalog_summary = {};
    deduce_analyze_catalog_summary_init(&catalog_summary, interp);
    deduce_analyze_collect_catalog_summary(db, &catalog_summary, interp);
    SymbolId incremental_invalidation_mode =
        interp.symbols.intern(db.incremental_full_recompute_required ? "full-recompute" : "tracked");

    DeduceAnalyzeRequestedExecution requested = {};
    Value* requested_err = deduce_analyze_parse_requested_execution(args, &requested, interp);
    if (requested_err != null) return requested_err;
    bool use_seminaive_recursive = requested.use_seminaive_recursive;
    SymbolId execution_engine_sym = requested.execution_engine_sym;
    long selector_rule_index = requested.selector_rule_index;

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_err != null) return parallel_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

    SymbolId requested_execution_engine_sym = execution_engine_sym;
    DeduceAnalyzeExecutionSummary summary = {};
    deduce_analyze_collect_plan_metadata(
        db,
        &scc_plan,
        parallel_wave_ids,
        parallel_batch_sizes,
        &summary
    );
    bool aggregate_recursive_fallback = false;
    deduce_analyze_resolve_execution_engine(
        db,
        &scc_plan,
        selector_rule_index,
        requested_execution_engine_sym,
        &use_seminaive_recursive,
        &execution_engine_sym,
        &summary.naive_recursive_mixed_aggregate_routed,
        &aggregate_recursive_fallback,
        interp
    );

    DeduceAnalyzeSelectedComponent selected = {};
    Value* selected_err = deduce_analyze_prepare_selected_component(
        db,
        &scc_plan,
        selector_rule_index,
        &selected,
        interp
    );
    if (selected_err != null) return selected_err;
    defer if (selected.selected_components.ptr != null) mem::free(selected.selected_components.ptr);

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_alloc_err = deduce_analyze_allocate_observed_rules(db, &observed_rules, interp);
    if (observed_alloc_err != null) return observed_alloc_err;
    defer deduce_analyze_free_observed_rules(observed_rules);

    Value* incremental_dirty_predicate_payload = deduce_analyze_dirty_predicates_payload(db, interp);
    Value* run_err = deduce_analyze_execute_with_plan(
        db,
        &scc_plan,
        &selected,
        observed_rules,
        use_seminaive_recursive,
        execution_engine_sym,
        requested_execution_engine_sym,
        incremental_invalidation_mode,
        selector_rule_index,
        aggregate_recursive_fallback,
        incremental_dirty_predicate_payload,
        &summary,
        interp
    );
    if (run_err != null) return run_err;

    Value* out = deduce_analyze_build_payload(
        db,
        &scc_plan,
        &catalog_summary,
        &selected,
        observed_rules,
        parallel_wave_ids,
        parallel_batch_sizes,
        &summary,
        interp
    );
    if (out != null && out.tag == ERROR) return out;

    deduce_analyze_finish_incremental_tracking(db, &scc_plan, &selected);
    return out;
}
