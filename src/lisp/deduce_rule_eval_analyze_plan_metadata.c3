module lisp;

fn void deduce_analyze_collect_plan_metadata(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    usz[] parallel_wave_ids,
    usz[] parallel_batch_sizes,
    DeduceAnalyzeExecutionSummary* summary
) {
    if (db == null || scc_plan == null || summary == null) return;

    for (usz component_id = 0; component_id < scc_plan.component_count; component_id++) {
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[component_id];
        if (!recursive) continue;

        summary.goal_directed_recursive_component_count++;
        summary.parallel_recursive_component_count++;
        summary.recursive_strata++;

        bool has_aggregates = deduce_component_has_aggregate_rules(db, scc_plan, component_id);
        bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, scc_plan, component_id);
        bool eligible = !has_aggregates && !has_negated_body_atoms;

        if (eligible) {
            summary.goal_directed_eligible_component_count++;
        } else {
            summary.goal_directed_blocked_component_count++;
        }
        if (has_aggregates) summary.goal_directed_aggregate_blocked_component_count++;
        if (has_negated_body_atoms) summary.goal_directed_negated_blocked_component_count++;

        long batch_size =
            component_id < parallel_batch_sizes.len ? (long)parallel_batch_sizes[component_id] : 0;
        if (batch_size > 1) summary.parallel_batched_component_count++;
        if (batch_size > summary.parallel_max_batch_size) {
            summary.parallel_max_batch_size = batch_size;
        }

        bool batch_seen = false;
        if (component_id < parallel_wave_ids.len && component_id < scc_plan.component_strata.len) {
            usz stratum = scc_plan.component_strata[component_id];
            usz wave = parallel_wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    scc_plan.component_recursive.ptr != null &&
                    prev < scc_plan.component_recursive.len &&
                    scc_plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= parallel_wave_ids.len || prev >= scc_plan.component_strata.len) continue;
                if (scc_plan.component_strata[prev] != stratum) continue;
                if (parallel_wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (!batch_seen) summary.parallel_batch_count++;
    }
}

fn void deduce_analyze_resolve_execution_engine(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    long selector_rule_index,
    SymbolId requested_execution_engine_sym,
    bool* use_seminaive_recursive,
    SymbolId* execution_engine_sym,
    bool* naive_recursive_mixed_aggregate_routed,
    bool* aggregate_recursive_fallback,
    Interp* interp
) {
    if (db == null || scc_plan == null || use_seminaive_recursive == null ||
        execution_engine_sym == null || naive_recursive_mixed_aggregate_routed == null ||
        aggregate_recursive_fallback == null || interp == null) {
        return;
    }

    *naive_recursive_mixed_aggregate_routed = false;
    *aggregate_recursive_fallback = false;
    if (requested_execution_engine_sym == (SymbolId)0) return;

    if (selector_rule_index < 0 &&
        !*use_seminaive_recursive &&
        deduce_plan_has_recursive_mixed_aggregate_components(db, scc_plan)) {
        *use_seminaive_recursive = true;
        *execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        *naive_recursive_mixed_aggregate_routed = true;
    }

    *aggregate_recursive_fallback =
        selector_rule_index < 0 &&
        *use_seminaive_recursive &&
        deduce_plan_has_recursive_aggregate_fallback_components(db, scc_plan);
    if (*aggregate_recursive_fallback) {
        *execution_engine_sym = interp.symbols.intern("naive-scc");
    }
}
