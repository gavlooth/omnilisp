module lisp;

import std::core::mem;

fn Value* deduce_query_execute_goal_directed_selector_ephemeral(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid goal-directed query state");
    }

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

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
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
        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            rel.db,
            &scc_plan,
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
        deduce_fixpoint_clear_selected_dirty_predicates(rel.db, &scc_plan, selected_components);
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("selected-component-closure"),
            interp.symbols.intern("query"),
            selector_rule_index,
            null,
            null,
            null,
            interp.symbols.intern("unsupported-captured-call-shape")
        );
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
            rel.db,
            &scc_plan,
            selected_component_id,
            &projected_head_demand
        )) {
        return null;
    }

    return deduce_query_execute_goal_directed_selector_ephemeral_in_txn(
        rel,
        filter_fn,
        selector_rule_index,
        use_seminaive_recursive,
        &scc_plan,
        selected_components,
        head_demand,
        &projected_head_demand,
        interp
    );
}
