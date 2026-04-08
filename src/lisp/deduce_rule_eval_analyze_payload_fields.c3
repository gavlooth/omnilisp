module lisp;

fn void deduce_analyze_set_result_payload_fields(
    Value* out,
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    DeduceAnalyzeCatalogSummary* catalog_summary,
    DeduceAnalyzeSelectedComponent* selected,
    usz[] parallel_wave_ids,
    usz[] parallel_batch_sizes,
    DeduceAnalyzeExecutionSummary* summary,
    Value* goal_directed_components,
    Value* goal_directed_selected_components,
    Value* goal_directed_selected_predicates,
    Interp* interp
) {
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-analyze"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "mode", interp.symbols.intern("naive-bottom-up"));
    explain_dict_set_symbol(out, interp, "goal-directed-execution-path",
        interp.symbols.intern(selected.selected_component_analysis ? "selected-component-closure" : "full-db-fixpoint"));
    explain_dict_set_symbol(out, interp, "execution-engine", summary.execution_engine_sym);
    explain_dict_set_symbol(out, interp, "requested-execution-engine", summary.requested_execution_engine_sym);
    explain_dict_set(out, interp, "goal-directed-selector-rule-index",
        selected.selected_component_analysis ? make_int(interp, summary.selector_rule_index) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-selected-component-id",
        selected.selected_component_analysis ? make_int(interp, (long)selected.selected_component_id) : make_nil(interp));
    explain_dict_set(out, interp, "recursive-aggregate-naive-fallback",
        summary.aggregate_recursive_fallback ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "naive-recursive-aggregate-mixed-routed-to-seminaive",
        summary.naive_recursive_mixed_aggregate_routed ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "version", make_int(interp, 1));
    explain_dict_set(out, interp, "rule-count", make_int(interp, catalog_summary.catalog_rule_count));
    explain_dict_set(out, interp, "strata-count", make_int(interp, (long)scc_plan.component_count));
    explain_dict_set(out, interp, "stratum-count", make_int(interp, (long)scc_plan.stratum_count));
    explain_dict_set(out, interp, "recursive-strata", make_int(interp, summary.recursive_strata));
    explain_dict_set(out, interp, "goal-directed-recursive-component-count",
        make_int(interp, summary.goal_directed_recursive_component_count));
    explain_dict_set(out, interp, "goal-directed-eligible-component-count",
        make_int(interp, summary.goal_directed_eligible_component_count));
    explain_dict_set(out, interp, "goal-directed-blocked-component-count",
        make_int(interp, summary.goal_directed_blocked_component_count));
    explain_dict_set(out, interp, "goal-directed-aggregate-blocked-component-count",
        make_int(interp, summary.goal_directed_aggregate_blocked_component_count));
    explain_dict_set(out, interp, "goal-directed-negated-blocked-component-count",
        make_int(interp, summary.goal_directed_negated_blocked_component_count));
    explain_dict_set(out, interp, "parallel-recursive-component-count",
        make_int(interp, summary.parallel_recursive_component_count));
    explain_dict_set(out, interp, "parallel-batched-component-count",
        make_int(interp, summary.parallel_batched_component_count));
    explain_dict_set(out, interp, "parallel-batch-count",
        make_int(interp, summary.parallel_batch_count));
    explain_dict_set(out, interp, "parallel-max-batch-size",
        make_int(interp, summary.parallel_max_batch_size));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            scc_plan,
            parallel_wave_ids,
            parallel_batch_sizes,
            interp
        ));
    explain_dict_set(out, interp, "goal-directed-components", goal_directed_components);
    explain_dict_set(out, interp, "goal-directed-selected-components", goal_directed_selected_components);
    explain_dict_set(out, interp, "goal-directed-selected-predicates", goal_directed_selected_predicates);
    explain_dict_set(out, interp, "iteration-limit", make_int(interp, summary.analyze_iteration_limit));
    explain_dict_set(out, interp, "max-component-iterations", make_int(interp, summary.max_component_iterations));
    explain_dict_set(out, interp, "iterations", make_int(interp, summary.iterations));
    explain_dict_set(out, interp, "derived-facts", make_int(interp, summary.derived));
    explain_dict_set(out, interp, "incremental-dependency-edges", make_int(interp, catalog_summary.dependency_edges));
    explain_dict_set(out, interp, "incremental-dirty-predicate-count", make_int(interp, catalog_summary.incremental_dirty_predicates));
    explain_dict_set(out, interp, "incremental-dirty-predicates", summary.incremental_dirty_predicate_payload);
    explain_dict_set_symbol(out, interp, "incremental-invalidation-mode", summary.incremental_invalidation_mode);
    explain_dict_set(out, interp, "keyed-relation-count", make_int(interp, catalog_summary.keyed_relation_count));
    explain_dict_set(out, interp, "unique-relation-count", make_int(interp, catalog_summary.unique_relation_count));
    explain_dict_set(out, interp, "reference-constraint-count", make_int(interp, catalog_summary.reference_constraint_count));
    explain_dict_set(out, interp, "check-constraint-count", make_int(interp, catalog_summary.check_constraint_count));
    explain_dict_set(out, interp, "resolved-reference-constraint-count", make_int(interp, catalog_summary.resolved_reference_constraint_count));
    explain_dict_set(out, interp, "unresolved-reference-constraint-count", make_int(interp, catalog_summary.unresolved_reference_constraint_count));
    explain_dict_set(out, interp, "integrity-constraint-count", make_int(interp, catalog_summary.integrity_constraint_count));
    explain_dict_set(out, interp, "materialized-view-count", make_int(interp, catalog_summary.materialized_view_count));
    explain_dict_set(out, interp, "ready-materialized-view-count", make_int(interp, catalog_summary.ready_materialized_view_count));
    explain_dict_set(out, interp, "unready-materialized-view-count", make_int(interp, catalog_summary.unready_materialized_view_count));
    explain_dict_set(out, interp, "stale-materialized-view-count", make_int(interp, catalog_summary.stale_materialized_view_count));
    explain_dict_set(out, interp, "ready-materialized-relations",
        deduce_explain_reverse_list(catalog_summary.ready_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "unready-materialized-relations",
        deduce_explain_reverse_list(catalog_summary.unready_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "stale-materialized-relations",
        deduce_explain_reverse_list(catalog_summary.stale_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, 0));
    explain_dict_set(out, interp, "integrity-violation-count", make_int(interp, (long)db.integrity_violation_count));
    explain_dict_set(out, interp, "key-integrity-violation-count", make_int(interp, (long)db.key_integrity_violation_count));
    explain_dict_set(out, interp, "unique-integrity-violation-count", make_int(interp, (long)db.unique_integrity_violation_count));
    explain_dict_set(out, interp, "reference-integrity-violation-count", make_int(interp, (long)db.reference_integrity_violation_count));
    explain_dict_set(out, interp, "check-integrity-violation-count", make_int(interp, (long)db.check_integrity_violation_count));
    explain_dict_set(out, interp, "last-integrity-violation-code",
        db.last_integrity_violation_code != (SymbolId)0
            ? make_symbol(interp, db.last_integrity_violation_code)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-integrity-violation-relation",
        db.last_integrity_violation_relation != (SymbolId)0
            ? make_symbol(interp, db.last_integrity_violation_relation)
            : make_nil(interp));
}
