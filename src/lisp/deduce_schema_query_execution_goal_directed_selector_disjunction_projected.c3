module lisp;

fn Value* deduce_query_build_selector_ephemeral_projected_demands(
    Relation* rel,
    DeduceQueryHeadDemandCases* head_demands,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    usz selected_component_id,
    DeduceRuleHeadDemand* requested_union,
    bool has_requested_union,
    DeduceQueryHeadDemandCases* projected_demands,
    bool* unsupported_shape,
    Interp* interp
) {
    if (unsupported_shape != null) *unsupported_shape = false;
    for (usz i = 0; i < head_demands.count; i++) {
        DeduceRuleHeadDemand projected_head_demand = {};
        bool[] projected_term_bound = {};
        Value*[] projected_term_values = {};
        bool has_projected_head_demand = false;
        Value* project_err = deduce_component_project_relation_head_demand_for_query(
            rel.db,
            scc_plan,
            selected_component_id,
            &head_demands.entries[i],
            &projected_head_demand,
            &projected_term_bound,
            &projected_term_values,
            &has_projected_head_demand,
            interp
        );
        if (project_err != null) return project_err;
        if (!has_projected_head_demand) {
            long iterations = 0;
            long derived = 0;
            long max_component_iterations = 0;
            Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
                rel.db,
                scc_plan,
                selected_components,
                256,
                use_seminaive_recursive,
                &iterations,
                &derived,
                &max_component_iterations,
                {},
                interp
            );
            if (exec_err != null) return exec_err;
            deduce_fixpoint_clear_selected_dirty_predicates(rel.db, scc_plan, selected_components);
            deduce_db_note_goal_directed_read(
                rel.db,
                rel.name,
                interp.symbols.intern("selected-component-closure"),
                interp.symbols.intern("query"),
                selector_rule_index,
                has_requested_union ? requested_union : null,
                null,
                null,
                interp.symbols.intern("unsupported-disjunction-shape")
            );
            if (unsupported_shape != null) *unsupported_shape = true;
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
                rel.db,
                scc_plan,
                selected_component_id,
                &projected_head_demand
            ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
                rel.db,
                scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
                rel.db,
                scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }

        Value* append_err = deduce_query_head_demand_cases_append_take(
            projected_demands,
            &projected_head_demand,
            interp
        );
        if (append_err != null) {
            deduce_rule_head_demand_release(&projected_head_demand);
            return append_err;
        }
    }
    return null;
}
