module lisp;

fn Value* deduce_explain_build_rule_execution_entry(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    usz rule_index,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (sig == null) return make_nil(interp);
    Value* entry = make_hashmap(interp, 16);
    if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    Value* steps = deduce_explain_build_plan_steps(db, sig, observed_rule_counters, interp);
    if (steps != null && steps.tag == ERROR) return steps;
    explain_dict_set(entry, interp, "rule-index", make_int(interp, (long)rule_index));
    explain_dict_set(entry, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(entry, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(entry, interp, "aggregate-projection-count", make_int(interp, (long)sig.aggregate_projection_count));
    Value* head_projections = deduce_explain_build_head_projections(sig, interp);
    if (head_projections != null && head_projections.tag == ERROR) return head_projections;
    explain_dict_set(entry, interp, "head-projections", head_projections);
    explain_dict_set(entry, interp, "steps", steps);
    return entry;
}

fn Value* deduce_explain_build_rule_execution_list(
    DeduceDb* db,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || db.rule_signature_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        Value* entry = deduce_explain_build_rule_execution_entry(
            db,
            &db.rule_signatures[i],
            i,
            observed_rule,
            interp
        );
        if (entry != null && entry.tag == ERROR) return entry;
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}
