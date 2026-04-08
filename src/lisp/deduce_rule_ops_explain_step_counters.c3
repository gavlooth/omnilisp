module lisp;

fn Value* deduce_explain_observed_step_counter_payload(
    DeduceObservedStepCounters* steps,
    usz step_count,
    Interp* interp
) {
    if (steps == null || step_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < step_count; i++) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
        explain_dict_set(entry, interp, "step-index", make_int(interp, (long)i));
        explain_dict_set(entry, interp, "rows-read", make_int(interp, steps[i].rows_read));
        explain_dict_set(entry, interp, "rows-emitted", make_int(interp, steps[i].rows_emitted));
        explain_dict_set(entry, interp, "index-hits", make_int(interp, steps[i].index_hits));
        explain_dict_set(entry, interp, "index-misses", make_int(interp, steps[i].index_misses));
        explain_dict_set(entry, interp, "join-probes", make_int(interp, steps[i].join_probes));
        explain_dict_set_symbol(entry, interp, "counter-kind", interp.symbols.intern("observed"));
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_build_step_counters(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    usz step_index,
    ulong bound_mask,
    int selected_index,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    Value* counters = make_hashmap(interp, 8);
    if (counters == null || counters.tag != HASHMAP || counters.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    if (observed_rule_counters != null && observed_rule_counters.steps != null &&
        step_index < observed_rule_counters.step_count) {
        DeduceObservedStepCounters step = observed_rule_counters.steps[step_index];
        explain_dict_set(counters, interp, "rows-read", make_int(interp, step.rows_read));
        explain_dict_set(counters, interp, "rows-emitted", make_int(interp, step.rows_emitted));
        explain_dict_set(counters, interp, "index-hits", make_int(interp, step.index_hits));
        explain_dict_set(counters, interp, "index-misses", make_int(interp, step.index_misses));
        explain_dict_set(counters, interp, "join-probes", make_int(interp, step.join_probes));
        explain_dict_set_symbol(counters, interp, "counter-kind", interp.symbols.intern("observed"));
        return counters;
    }

    DeduceRelationSchema* schema = null;
    if (db != null && sig != null && sig.body_predicates != null && step_index < sig.body_count) {
        schema = deduce_db_lookup_relation_schema(db, sig.body_predicates[step_index]);
    }

    long rows_read = deduce_explain_rows_read_estimate(schema);
    long rows_emitted = deduce_explain_rows_emitted_estimate(schema, bound_mask, selected_index);
    long index_hits = selected_index >= 0 ? rows_emitted : 0;
    long index_misses = 0;
    if (selected_index >= 0 && rows_read > index_hits) {
        index_misses = rows_read - index_hits;
    }

    explain_dict_set(counters, interp, "rows-read", make_int(interp, rows_read));
    explain_dict_set(counters, interp, "rows-emitted", make_int(interp, rows_emitted));
    explain_dict_set(counters, interp, "index-hits", make_int(interp, index_hits));
    explain_dict_set(counters, interp, "index-misses", make_int(interp, index_misses));
    explain_dict_set(counters, interp, "join-probes", make_int(interp, 0));
    explain_dict_set_symbol(counters, interp, "counter-kind", interp.symbols.intern("estimated"));
    return counters;
}
