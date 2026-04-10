module lisp;

import std::core::mem;

struct DeduceAnalyzeSelectedComponent {
    bool   selected_component_analysis;
    usz    selected_component_id;
    bool[] selected_components;
}

fn Value* deduce_analyze_goal_directed_selector_error(
    long selector_rule_index,
    usz component_id,
    Value* blockers,
    Interp* interp
) {
    Value* data = make_hashmap_checked(interp, 8, "deduce/analyze: out of memory");
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    if (Value* err = explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index))) {
        (void)err;
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    if (Value* err = explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id))) {
        (void)err;
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    if (Value* err = explain_dict_set(data, interp, "goal-directed-blockers",
            blockers == null ? make_nil(interp) : blockers)) {
        (void)err;
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    return deduce_raise(
        interp,
        "deduce/analyze-goal-directed-selector-not-eligible",
        "deduce/analyze: selected rule is not goal-directed eligible",
        data
    );
}

fn void deduce_analyze_selected_component_init(DeduceAnalyzeSelectedComponent* selected) {
    if (selected == null) return;
    *selected = {};
}

fn Value* deduce_analyze_prepare_selected_component(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    long selector_rule_index,
    DeduceAnalyzeSelectedComponent* selected,
    Interp* interp
) {
    if (db == null || scc_plan == null || selected == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid selected component state");
    }

    deduce_analyze_selected_component_init(selected);
    if (selector_rule_index < 0) return null;

    if ((ulong)selector_rule_index >= db.rule_signature_count) {
        return deduce_raise(interp, "deduce/analyze-rule-index-out-of-range",
            "deduce/analyze: rule-index is out of range");
    }
    if (scc_plan.rule_component_ids.ptr == null || (ulong)selector_rule_index >= scc_plan.rule_component_ids.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: selected rule is missing from the SCC plan");
    }

    selected.selected_component_id = scc_plan.rule_component_ids[(usz)selector_rule_index];
    bool recursive =
        scc_plan.component_recursive.ptr != null &&
        selected.selected_component_id < scc_plan.component_recursive.len &&
        scc_plan.component_recursive[selected.selected_component_id];
    bool has_aggregates = deduce_component_has_aggregate_rules(db, scc_plan, selected.selected_component_id);
    bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, scc_plan, selected.selected_component_id);
    bool eligible = recursive && !has_aggregates && !has_negated_body_atoms;
    if (!eligible) {
        Value* blockers = deduce_explain_goal_directed_blockers(
            recursive,
            has_aggregates,
            has_negated_body_atoms,
            interp
        );
        return deduce_analyze_goal_directed_selector_error(
            selector_rule_index,
            selected.selected_component_id,
            blockers,
            interp
        );
    }

    selected.selected_components = mem::new_array(bool, scc_plan.component_count);
    if (selected.selected_components.ptr == null && scc_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    if (!deduce_fixpoint_select_dependency_components(
            scc_plan,
            selected.selected_component_id,
            selected.selected_components
        )) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: failed to select goal-directed component closure");
    }

    selected.selected_component_analysis = true;
    return null;
}
