module lisp;

fn Value* deduce_scan_range_prepare_goal_directed_selector_demand(
    DeduceDb* db,
    SymbolId relation_name,
    long selector_rule_index,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    DeduceSccPlan* scc_plan,
    bool[]* selected_components,
    usz* selected_component_id,
    DeduceRuleHeadDemand* projected_head_demand,
    bool[]* projected_term_bound,
    Value*[]* projected_term_values,
    bool* has_projected_head_demand,
    Interp* interp
) {
    if (db == null || head_demand == null || surface == null || scc_plan == null ||
        selected_components == null || selected_component_id == null ||
        projected_head_demand == null || projected_term_bound == null ||
        projected_term_values == null || has_projected_head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/scan-range-invalid-state", "deduce-scan-range: invalid goal-directed scan state");
    }

    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        db,
        relation_name,
        selector_rule_index,
        surface,
        scc_plan,
        selected_components,
        selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    return deduce_component_project_relation_head_demand(
        db,
        scc_plan,
        *selected_component_id,
        head_demand,
        projected_head_demand,
        projected_term_bound,
        projected_term_values,
        has_projected_head_demand,
        interp
    );
}
