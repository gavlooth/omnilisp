module lisp;

import std::core::mem;

struct DeduceExplainGoalDirectedMetadata {
    SymbolId execution_engine_sym;
    bool     naive_recursive_mixed_aggregate_routed;
    bool     aggregate_recursive_fallback;
    bool     goal_directed_recursive;
    bool     goal_directed_has_aggregates;
    bool     goal_directed_has_negated_body_atoms;
    isz      goal_directed_component_id;
    bool     goal_directed_eligible;
    Value*   goal_directed_blockers;
    Value*   goal_directed_selected_components;
    Value*   goal_directed_selected_predicates;
    Value*   parallel_batch_components;
}

fn Value* deduce_explain_prepare_goal_directed_metadata(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    usz[] parallel_wave_ids,
    DeduceExplainRequest* request,
    DeduceExplainGoalDirectedMetadata* metadata,
    Interp* interp
) {
    if (db == null || scc_plan == null || request == null || metadata == null || interp == null) {
        return deduce_raise(interp, "deduce/explain-invalid-state",
            "deduce/explain: invalid goal-directed metadata state");
    }

    *metadata = {
        .execution_engine_sym = request.requested_execution_engine_sym,
        .goal_directed_component_id = -1,
        .goal_directed_blockers = make_nil(interp),
        .goal_directed_selected_components = make_nil(interp),
        .goal_directed_selected_predicates = make_nil(interp),
        .parallel_batch_components = make_nil(interp),
    };

    if (!request.use_seminaive_recursive &&
        request.selected_rule_index >= 0 &&
        (usz)request.selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)request.selected_rule_index];
        bool recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        if (recursive && deduce_component_has_mixed_aggregate_rules(db, scc_plan, component_id)) {
            request.use_seminaive_recursive = true;
            metadata.execution_engine_sym = interp.symbols.intern("semi-naive-scc");
            metadata.naive_recursive_mixed_aggregate_routed = true;
        }
    }

    if (request.selected_rule_index >= 0 &&
        (usz)request.selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)request.selected_rule_index];
        metadata.goal_directed_component_id = (isz)component_id;
        metadata.goal_directed_recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        metadata.goal_directed_has_aggregates = deduce_component_has_aggregate_rules(db, scc_plan, component_id);
        metadata.goal_directed_has_negated_body_atoms =
            deduce_component_has_negated_body_atoms(db, scc_plan, component_id);

        if (request.use_seminaive_recursive &&
            metadata.goal_directed_recursive &&
            metadata.goal_directed_has_aggregates &&
            (!deduce_component_can_use_seminaive_aggregates(db, scc_plan, component_id) ||
             !deduce_component_has_full_recursive_aggregate_seminaive_support(db, scc_plan, component_id))) {
            metadata.aggregate_recursive_fallback = true;
            metadata.execution_engine_sym = interp.symbols.intern("naive-scc");
        }
    }

    metadata.goal_directed_eligible =
        metadata.goal_directed_recursive &&
        !metadata.goal_directed_has_aggregates &&
        !metadata.goal_directed_has_negated_body_atoms;
    metadata.goal_directed_blockers = deduce_explain_goal_directed_blockers(
        metadata.goal_directed_recursive,
        metadata.goal_directed_has_aggregates,
        metadata.goal_directed_has_negated_body_atoms,
        interp
    );
    metadata.goal_directed_selected_components = deduce_explain_goal_directed_selected_components(
        scc_plan,
        metadata.goal_directed_component_id,
        metadata.goal_directed_eligible,
        interp
    );
    if (metadata.goal_directed_selected_components != null &&
        metadata.goal_directed_selected_components.tag == ERROR) {
        return metadata.goal_directed_selected_components;
    }
    metadata.goal_directed_selected_predicates = deduce_explain_goal_directed_selected_predicates(
        scc_plan,
        metadata.goal_directed_component_id,
        metadata.goal_directed_eligible,
        interp
    );
    if (metadata.goal_directed_selected_predicates != null &&
        metadata.goal_directed_selected_predicates.tag == ERROR) {
        return metadata.goal_directed_selected_predicates;
    }
    metadata.parallel_batch_components =
        metadata.goal_directed_component_id >= 0
            ? deduce_explain_parallel_batch_components(
                scc_plan,
                parallel_wave_ids,
                (usz)metadata.goal_directed_component_id,
                interp
            )
            : make_nil(interp);
    if (metadata.parallel_batch_components != null &&
        metadata.parallel_batch_components.tag == ERROR) {
        return metadata.parallel_batch_components;
    }

    return null;
}
