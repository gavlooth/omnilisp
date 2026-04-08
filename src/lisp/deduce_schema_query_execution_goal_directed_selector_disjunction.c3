module lisp;

import std::core::mem;

fn Value* deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceQueryHeadDemandCases* head_demands,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demands == null || head_demands.count == 0 ||
        surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive goal-directed query state");
    }

    DeduceRuleHeadDemand requested_union = {};
    bool has_requested_union = false;
    Value* requested_err = deduce_query_build_union_head_demand_positions(
        rel,
        head_demands.entries,
        head_demands.count,
        &requested_union,
        &has_requested_union,
        interp
    );
    if (requested_err != null) return requested_err;
    defer deduce_rule_head_demand_release(&requested_union);

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        rel.db,
        rel.name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    DeduceQueryHeadDemandCases projected_demands = {};
    defer deduce_query_head_demand_cases_release(&projected_demands);
    bool unsupported_shape = false;
    Value* project_err = deduce_query_build_selector_ephemeral_projected_demands(
        rel,
        head_demands,
        selector_rule_index,
        use_seminaive_recursive,
        &scc_plan,
        selected_components,
        selected_component_id,
        &requested_union,
        has_requested_union,
        &projected_demands,
        &unsupported_shape,
        interp
    );
    if (project_err != null) return project_err;
    if (unsupported_shape) return null;

    DeduceRuleHeadDemand applied_union = {};
    bool has_applied_union = false;
    Value* applied_err = deduce_query_build_union_head_demand_positions(
        rel,
        projected_demands.entries,
        projected_demands.count,
        &applied_union,
        &has_applied_union,
        interp
    );
    if (applied_err != null) return applied_err;
    defer deduce_rule_head_demand_release(&applied_union);

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* filtered_rev = null;
    Value* exec_err = deduce_query_execute_selector_ephemeral_projected_demands(
        rel,
        filter_fn,
        selector_rule_index,
        use_seminaive_recursive,
        &scc_plan,
        selected_components,
        &projected_demands,
        &observed_rules,
        &filtered_rev,
        interp
    );
    if (exec_err != null) return exec_err;
    defer deduce_free_selected_rule_observed_counters(observed_rules);

    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        selector_rule_index,
        has_requested_union ? &requested_union : null,
        has_applied_union ? &applied_union : null,
        &observed_rules[(usz)selector_rule_index]
    );
    return deduce_reverse_list(filtered_rev, interp);
}
