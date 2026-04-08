module lisp;

import std::core::mem;

fn Value* deduce_scan_range_execute_goal_directed_selector_demand(
    Relation* rel,
    Value*[] lower_values,
    Value*[] upper_values,
    char* start_key,
    usz start_key_len,
    bool use_seminaive_recursive,
    long selector_rule_index,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/scan-range-invalid-state", "deduce-scan-range: invalid goal-directed scan state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* prepare_err = deduce_scan_range_prepare_goal_directed_selector_demand(
        rel.db,
        rel.name,
        selector_rule_index,
        head_demand,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (prepare_err != null) return prepare_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        return null;
    }

    return deduce_scan_range_execute_goal_directed_selector_in_txn(
        rel.db,
        rel,
        lower_values,
        upper_values,
        start_key,
        start_key_len,
        use_seminaive_recursive,
        selector_rule_index,
        head_demand,
        &scc_plan,
        selected_components,
        &projected_head_demand,
        interp
    );
}
