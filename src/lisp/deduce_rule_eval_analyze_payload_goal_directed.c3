module lisp;

fn Value* deduce_analyze_prepare_goal_directed_payloads(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    DeduceAnalyzeSelectedComponent* selected,
    Value** goal_directed_components_out,
    Value** goal_directed_selected_components_out,
    Value** goal_directed_selected_predicates_out,
    Interp* interp
) {
    if (goal_directed_components_out != null) *goal_directed_components_out = null;
    if (goal_directed_selected_components_out != null) *goal_directed_selected_components_out = null;
    if (goal_directed_selected_predicates_out != null) *goal_directed_selected_predicates_out = null;

    Value* goal_directed_components =
        deduce_explain_goal_directed_component_summaries(db, scc_plan, interp);
    if (goal_directed_components != null && goal_directed_components.tag == ERROR) {
        return goal_directed_components;
    }

    Value* goal_directed_selected_components =
        selected.selected_component_analysis
            ? deduce_explain_goal_directed_selected_components(
                scc_plan,
                selected.selected_component_id,
                true,
                interp
            )
            : make_nil(interp);
    if (goal_directed_selected_components != null && goal_directed_selected_components.tag == ERROR) {
        return goal_directed_selected_components;
    }

    Value* goal_directed_selected_predicates =
        selected.selected_component_analysis
            ? deduce_explain_goal_directed_selected_predicates(
                scc_plan,
                selected.selected_component_id,
                true,
                interp
            )
            : make_nil(interp);
    if (goal_directed_selected_predicates != null && goal_directed_selected_predicates.tag == ERROR) {
        return goal_directed_selected_predicates;
    }

    if (goal_directed_components_out != null) *goal_directed_components_out = goal_directed_components;
    if (goal_directed_selected_components_out != null) {
        *goal_directed_selected_components_out = goal_directed_selected_components;
    }
    if (goal_directed_selected_predicates_out != null) {
        *goal_directed_selected_predicates_out = goal_directed_selected_predicates;
    }
    return null;
}

fn Value* deduce_analyze_append_runtime_tail_payload(
    Value* out,
    DeduceDb* db,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    return deduce_analyze_append_read_and_rule_tail_payload(out, db, observed_rules, interp);
}
