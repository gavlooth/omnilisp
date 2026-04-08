module lisp;

import std::core::mem;

fn Value* deduce_scan_range_execute_goal_directed_plain_demand(
    Relation* rel,
    Value*[] lower_values,
    Value*[] upper_values,
    char* start_key,
    usz start_key_len,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/scan-range-invalid-state", "deduce-scan-range: invalid plain goal-directed scan state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        return null;
    }

    return deduce_scan_range_execute_goal_directed_plain_in_txn(
        rel.db,
        rel,
        lower_values,
        upper_values,
        start_key,
        start_key_len,
        use_seminaive_recursive,
        used_out,
        head_demand,
        &scc_plan,
        selected_components,
        &projected_head_demand,
        interp
    );
}
