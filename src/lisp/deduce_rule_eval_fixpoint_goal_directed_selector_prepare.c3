module lisp;

import std::core::mem;

struct DeduceGoalDirectedSelectorSurface {
    char[] out_of_memory_code;
    char[] out_of_memory_message;
    char[] invalid_selector_code;
    char[] invalid_selector_message;
    char[] invalid_engine_code;
    char[] invalid_engine_message;
    char[] rule_index_out_of_range_code;
    char[] rule_index_out_of_range_message;
    char[] invalid_state_code;
    char[] invalid_state_message;
    char[] full_recompute_code;
    char[] full_recompute_message;
    char[] selector_not_eligible_code;
    char[] selector_not_eligible_message;
    char[] relation_mismatch_code;
    char[] relation_mismatch_message;
}

struct DeduceSelectedSchemaEstimateSnapshot {
    DeduceRelationSchema* schema;
    ulong                 cardinality_estimate;
    ulong                 distinct_estimate;
}

fn Value* deduce_goal_directed_selector_surface_error(
    char[] code,
    char[] message,
    long selector_rule_index,
    usz component_id,
    Value* blockers,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index));
    explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id));
    explain_dict_set(data, interp, "goal-directed-blockers",
        blockers == null ? make_nil(interp) : blockers);
    return deduce_raise(interp, code, message, data);
}

fn Value* deduce_goal_directed_relation_surface_error(
    char[] code,
    char[] message,
    long selector_rule_index,
    usz component_id,
    SymbolId relation_name,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index));
    explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id));
    explain_dict_set(data, interp, "relation",
        relation_name != (SymbolId)0 ? make_symbol(interp, relation_name) : make_nil(interp));
    return deduce_raise(interp, code, message, data);
}

fn Value* deduce_parse_goal_directed_selector_and_engine(
    Value*[] args,
    usz required_arg_count,
    DeduceGoalDirectedSelectorSurface* surface,
    long* selector_rule_index_out,
    bool* use_seminaive_recursive_out,
    Interp* interp
) {
    if (selector_rule_index_out == null || use_seminaive_recursive_out == null || surface == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid selector parse state");
    }

    *selector_rule_index_out = -1;
    *use_seminaive_recursive_out = true;
    if (args.len <= required_arg_count) return null;

    Value* selector_arg = args[required_arg_count];
    if (selector_arg == null || selector_arg.tag != INT) {
        return deduce_raise(interp, surface.invalid_selector_code, surface.invalid_selector_message);
    }
    *selector_rule_index_out = selector_arg.int_val;

    if (args.len == required_arg_count + 1) return null;

    Value* engine_arg = args[required_arg_count + 1];
    if (engine_arg == null || engine_arg.tag != SYMBOL) {
        return deduce_raise(interp, surface.invalid_engine_code, surface.invalid_engine_message);
    }
    char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
    if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
        *use_seminaive_recursive_out = false;
        return null;
    }
    if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
        *use_seminaive_recursive_out = true;
        return null;
    }
    return deduce_raise(interp, surface.invalid_engine_code, surface.invalid_engine_message);
}

fn Value* deduce_allocate_selected_rule_observed_counters(
    DeduceDb* db,
    long selector_rule_index,
    DeduceObservedRuleCounters[]* out_observed_rules,
    Interp* interp
) {
    if (out_observed_rules == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid observed counter state");
    }
    *out_observed_rules = {};
    if (db == null || selector_rule_index < 0 || (ulong)selector_rule_index >= db.rule_signature_count) {
        return null;
    }

    DeduceObservedRuleCounters[] observed_rules =
        mem::new_array(DeduceObservedRuleCounters, db.rule_signature_count);
    if (observed_rules.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }
    for (usz i = 0; i < observed_rules.len; i++) observed_rules[i] = {};

    usz step_count = db.rule_signatures[(usz)selector_rule_index].body_count;
    if (step_count > 0) {
        DeduceObservedStepCounters[] steps = mem::new_array(DeduceObservedStepCounters, step_count);
        if (steps.ptr == null) {
            mem::free(observed_rules.ptr);
            return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
        }
        observed_rules[(usz)selector_rule_index].steps = steps.ptr;
        observed_rules[(usz)selector_rule_index].step_count = step_count;
        deduce_observed_rule_counters_reset(&observed_rules[(usz)selector_rule_index]);
    }

    *out_observed_rules = observed_rules;
    return null;
}

fn void deduce_free_selected_rule_observed_counters(
    DeduceObservedRuleCounters[] observed_rules
) {
    if (observed_rules.ptr == null) return;
    for (usz i = 0; i < observed_rules.len; i++) {
        if (observed_rules[i].steps != null) {
            mem::free(observed_rules[i].steps);
            observed_rules[i].steps = null;
        }
    }
    mem::free(observed_rules.ptr);
}

fn Value* deduce_prepare_goal_directed_relation_selector_plan(
    DeduceDb* db,
    SymbolId relation_name,
    long selector_rule_index,
    DeduceGoalDirectedSelectorSurface* surface,
    DeduceSccPlan* out_plan,
    bool[]* out_selected_components,
    usz* out_selected_component_id,
    Interp* interp
) {
    if (out_plan == null || out_selected_components == null || out_selected_component_id == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed selector state");
    }
    *out_plan = {};
    *out_selected_components = {};
    *out_selected_component_id = 0;

    if (selector_rule_index < 0) return null;
    if (db == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed execution state");
    }
    if (db.incremental_full_recompute_required) {
        return deduce_goal_directed_relation_surface_error(
            surface.full_recompute_code,
            surface.full_recompute_message,
            selector_rule_index,
            0,
            relation_name,
            interp
        );
    }

    Value* plan_err = deduce_build_scc_plan(db, out_plan, interp);
    if (plan_err != null) return plan_err;

    if ((ulong)selector_rule_index >= db.rule_signature_count) {
        return deduce_raise(interp, surface.rule_index_out_of_range_code, surface.rule_index_out_of_range_message);
    }
    if (out_plan.rule_component_ids.ptr == null || (ulong)selector_rule_index >= out_plan.rule_component_ids.len) {
        return deduce_raise(interp, surface.invalid_state_code, surface.invalid_state_message);
    }

    usz selected_component_id = out_plan.rule_component_ids[(usz)selector_rule_index];
    bool recursive =
        out_plan.component_recursive.ptr != null &&
        selected_component_id < out_plan.component_recursive.len &&
        out_plan.component_recursive[selected_component_id];
    bool has_aggregates = deduce_component_has_aggregate_rules(db, out_plan, selected_component_id);
    bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, out_plan, selected_component_id);
    if (!recursive || has_aggregates || has_negated_body_atoms) {
        Value* blockers = deduce_explain_goal_directed_blockers(
            recursive,
            has_aggregates,
            has_negated_body_atoms,
            interp
        );
        return deduce_goal_directed_selector_surface_error(
            surface.selector_not_eligible_code,
            surface.selector_not_eligible_message,
            selector_rule_index,
            selected_component_id,
            blockers,
            interp
        );
    }

    bool[] selected_components = mem::new_array(bool, out_plan.component_count);
    if (selected_components.ptr == null && out_plan.component_count > 0) {
        return deduce_raise(interp, surface.out_of_memory_code, surface.out_of_memory_message);
    }

    if (!deduce_fixpoint_select_dependency_components(out_plan, selected_component_id, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_raise(interp, surface.invalid_state_code, surface.invalid_state_message);
    }
    if (!deduce_fixpoint_predicate_in_selected_components(out_plan, selected_components, relation_name)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_goal_directed_relation_surface_error(
            surface.relation_mismatch_code,
            surface.relation_mismatch_message,
            selector_rule_index,
            selected_component_id,
            relation_name,
            interp
        );
    }

    *out_selected_components = selected_components;
    *out_selected_component_id = selected_component_id;
    return null;
}
