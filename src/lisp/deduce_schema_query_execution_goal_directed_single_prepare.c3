module lisp;

import std::core::mem;

fn Value* deduce_query_prepare_goal_directed_plain_head_demand(
    Relation* rel,
    DeduceRuleHeadDemand* head_demand,
    DeduceSccPlan* scc_plan,
    bool[]* selected_components,
    DeduceRuleHeadDemand* projected_head_demand,
    bool[]* projected_term_bound,
    Value*[]* projected_term_values,
    bool* ready_out,
    Interp* interp
) {
    if (ready_out != null) *ready_out = false;
    if (rel == null || rel.db == null || head_demand == null || scc_plan == null ||
        selected_components == null || projected_head_demand == null || projected_term_bound == null ||
        projected_term_values == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid plain goal-directed query state");
    }

    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        scc_plan,
        selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
        rel.db,
        scc_plan,
        selected_component_id,
        head_demand,
        projected_head_demand,
        projected_term_bound,
        projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    if (!has_projected_head_demand) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
            rel.db,
            scc_plan,
            selected_component_id,
            projected_head_demand
        ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
            rel.db,
            scc_plan,
            selected_component_id,
            projected_head_demand
        )) {
        return null;
    }
    if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
            rel.db,
            scc_plan,
            selected_component_id,
            projected_head_demand
        )) {
        return null;
    }

    if (ready_out != null) *ready_out = true;
    return null;
}
