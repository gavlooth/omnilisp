module lisp;

import std::core::mem;

fn Value* deduce_explain_build_snapshot(
    DeduceDb* db,
    DeduceExplainRequest* request,
    DeduceSccPlan* scc_plan,
    usz[] parallel_wave_ids,
    usz[] parallel_batch_sizes,
    DeduceExplainGoalDirectedMetadata* metadata,
    Interp* interp
) {
    DeduceRuleSignature* sig = &db.rule_signatures[(usz)request.selected_rule_index];
    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    DeduceObservedStepCounters[] observed_steps = {};
    if (sig.body_count > 0) {
        observed_steps = mem::new_array(DeduceObservedStepCounters, sig.body_count);
        if (observed_steps.ptr == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
    }
    defer if (observed_steps.ptr != null) mem::free(observed_steps.ptr);

    DeduceObservedRuleCounters observed_rule = {
        .steps = observed_steps.ptr,
        .step_count = sig.body_count,
    };
    Value* observed_err = deduce_collect_rule_observed_counters(db, sig, &observed_rule, interp);
    if (observed_err != null) return observed_err;

    Value* steps = deduce_explain_build_plan_steps(db, sig, &observed_rule, interp);
    if (steps != null && steps.tag == ERROR) return steps;
    Value* head_projections = deduce_explain_build_head_projections(sig, interp);
    if (head_projections != null && head_projections.tag == ERROR) return head_projections;
    Value* demand_bound_head_positions = deduce_explain_goal_directed_demand_bound_head_positions(
        db,
        scc_plan,
        sig,
        metadata.goal_directed_component_id,
        metadata.goal_directed_eligible,
        interp
    );
    if (demand_bound_head_positions != null && demand_bound_head_positions.tag == ERROR) {
        return demand_bound_head_positions;
    }
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-plan"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "surface-kind", interp.symbols.intern("planner-snapshot"));
    explain_dict_set_symbol(out, interp, "execution-engine", metadata.execution_engine_sym);
    explain_dict_set_symbol(out, interp, "requested-execution-engine", request.requested_execution_engine_sym);
    explain_dict_set(out, interp, "recursive-aggregate-naive-fallback",
        metadata.aggregate_recursive_fallback ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "naive-recursive-aggregate-mixed-routed-to-seminaive",
        metadata.naive_recursive_mixed_aggregate_routed ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-component-id",
        metadata.goal_directed_component_id >= 0
            ? make_int(interp, (long)metadata.goal_directed_component_id)
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-eligible",
        metadata.goal_directed_eligible ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-shape",
        metadata.goal_directed_eligible
            ? make_symbol(interp, interp.symbols.intern("positive-recursive-closure"))
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-blockers", metadata.goal_directed_blockers);
    explain_dict_set(out, interp, "goal-directed-execution-path",
        metadata.goal_directed_eligible
            ? make_symbol(interp, interp.symbols.intern("selected-component-closure"))
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-selected-components", metadata.goal_directed_selected_components);
    explain_dict_set(out, interp, "goal-directed-selected-predicates", metadata.goal_directed_selected_predicates);
    explain_dict_set(out, interp, "goal-directed-demand-bound-head-positions", demand_bound_head_positions);
    deduce_explain_set_last_goal_directed_read_fields(out, db, sig, interp);
    explain_dict_set(out, interp, "parallel-wave",
        metadata.goal_directed_component_id >= 0 && (usz)metadata.goal_directed_component_id < parallel_wave_ids.len
            ? make_int(interp, (long)parallel_wave_ids[(usz)metadata.goal_directed_component_id])
            : make_nil(interp));
    explain_dict_set(out, interp, "parallel-batch-size",
        metadata.goal_directed_component_id >= 0 && (usz)metadata.goal_directed_component_id < parallel_batch_sizes.len
            ? make_int(interp, (long)parallel_batch_sizes[(usz)metadata.goal_directed_component_id])
            : make_nil(interp));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-components", metadata.parallel_batch_components);
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            scc_plan,
            parallel_wave_ids[..],
            parallel_batch_sizes[..],
            interp
        ));
    explain_dict_set(out, interp, "rule-index", make_int(interp, (long)request.selected_rule_index));
    explain_dict_set(out, interp, "rule-count", make_int(interp, (long)db.rule_signature_count));
    explain_dict_set(out, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(out, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(out, interp, "aggregate-projection-count", make_int(interp, (long)sig.aggregate_projection_count));
    explain_dict_set(out, interp, "head-projections", head_projections);
    explain_dict_set(out, interp, "steps", steps);
    return out;
}
