module lisp;

fn bool deduce_component_has_negated_body_atoms(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.body_negated == null) continue;
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_negated[step]) return true;
        }
    }
    return false;
}

fn Value* deduce_explain_goal_directed_blockers(
    bool recursive,
    bool has_aggregates,
    bool has_negated_body_atoms,
    Interp* interp
) {
    Value* rev = make_nil(interp);
    if (!recursive) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("non-recursive")), rev);
    }
    if (has_aggregates) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("aggregates-present")), rev);
    }
    if (has_negated_body_atoms) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("negated-body-atom")), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_component_summaries(
    DeduceDb* db,
    DeduceSccPlan* plan,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        bool recursive =
            plan.component_recursive.ptr != null &&
            component_id < plan.component_recursive.len &&
            plan.component_recursive[component_id];
        if (!recursive) continue;

        bool has_aggregates = deduce_component_has_aggregate_rules(db, plan, component_id);
        bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, plan, component_id);
        bool eligible = !has_aggregates && !has_negated_body_atoms;
        long rule_count = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            rule_count++;
        }

        Value* entry = explain_make_dict(interp, 16);

        explain_dict_set(entry, interp, "component-id", make_int(interp, (long)component_id));
        explain_dict_set(entry, interp, "stratum",
            make_int(interp, component_id < plan.component_strata.len ? (long)plan.component_strata[component_id] : 0));
        explain_dict_set(entry, interp, "rule-count", make_int(interp, rule_count));
        explain_dict_set(entry, interp, "goal-directed-eligible",
            eligible ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(entry, interp, "goal-directed-shape",
            eligible
                ? make_symbol(interp, interp.symbols.intern("positive-recursive-closure"))
                : make_nil(interp));
        explain_dict_set(entry, interp, "goal-directed-blockers",
            deduce_explain_goal_directed_blockers(
                recursive,
                has_aggregates,
                has_negated_body_atoms,
                interp
            ));
        explain_dict_set(entry, interp, "aggregate-rules-present",
            has_aggregates ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(entry, interp, "negated-body-atoms-present",
            has_negated_body_atoms ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}
