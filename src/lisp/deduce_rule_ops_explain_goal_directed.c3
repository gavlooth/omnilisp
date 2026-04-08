module lisp;

import std::core::mem;

fn Value* deduce_explain_goal_directed_selected_components(
    DeduceSccPlan* plan,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || plan == null || interp == null) return make_nil(interp);

    bool[] selected_components = mem::new_array(bool, plan.component_count);
    if (selected_components.ptr == null && plan.component_count > 0) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_fixpoint_select_dependency_components(plan, component_id, selected_components)) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < selected_components.len; i++) {
        if (!selected_components[i]) continue;
        rev = make_cons(interp, make_int(interp, (long)i), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_selected_predicates(
    DeduceSccPlan* plan,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || plan == null || interp == null) return make_nil(interp);

    bool[] selected_components = mem::new_array(bool, plan.component_count);
    if (selected_components.ptr == null && plan.component_count > 0) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_fixpoint_select_dependency_components(plan, component_id, selected_components)) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz predicate_index = 0; predicate_index < plan.predicates.len; predicate_index++) {
        if (predicate_index >= plan.predicate_component.len) continue;
        int predicate_component = plan.predicate_component[predicate_index];
        if (predicate_component < 0 || (usz)predicate_component >= selected_components.len) continue;
        if (!selected_components[(usz)predicate_component]) continue;
        rev = make_cons(interp, make_symbol(interp, plan.predicates[predicate_index]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_demand_bound_head_positions(
    DeduceDb* db,
    DeduceSccPlan* plan,
    DeduceRuleSignature* sig,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || db == null || plan == null || sig == null || interp == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (!deduce_component_position_can_use_relation_head_demand(
                db,
                plan,
                component_id,
                sig.head_predicate,
                i
            )) {
            continue;
        }
        rev = make_cons(interp, make_int(interp, (long)i), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_parallel_batch_components(
    DeduceSccPlan* plan,
    usz[] parallel_wave_ids,
    usz component_id,
    Interp* interp
) {
    if (plan == null || interp == null || component_id >= plan.component_count ||
        component_id >= plan.component_strata.len || component_id >= parallel_wave_ids.len) {
        return make_nil(interp);
    }

    usz stratum = plan.component_strata[component_id];
    usz wave = parallel_wave_ids[component_id];
    Value* rev = make_nil(interp);
    for (usz other = 0; other < plan.component_count; other++) {
        if (other >= plan.component_strata.len || other >= parallel_wave_ids.len) continue;
        if (plan.component_strata[other] != stratum) continue;
        if (parallel_wave_ids[other] != wave) continue;
        rev = make_cons(interp, make_int(interp, (long)other), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn void deduce_explain_set_last_goal_directed_read_fields(
    Value* out,
    DeduceDb* db,
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null || interp == null) return;

    DeduceRelationSchema* schema = null;
    if (db != null && sig != null) {
        schema = deduce_db_lookup_relation_schema(db, sig.head_predicate);
    }

    SymbolId execution_path = schema != null ? schema.last_goal_directed_read_execution_path : (SymbolId)0;
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        execution_path != (SymbolId)0
            ? make_symbol(interp, execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        schema != null && schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        schema != null && schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        schema != null && schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_requested_bound_positions,
                schema.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_applied_bound_positions,
                schema.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-step-counters",
        execution_path != (SymbolId)0 && schema != null && schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp));
}
