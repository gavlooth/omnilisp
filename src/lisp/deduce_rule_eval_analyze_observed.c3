module lisp;

import std::core::mem;

fn Value* deduce_analyze_allocate_observed_rules(
    DeduceDb* db,
    DeduceObservedRuleCounters[]* observed_rules_out,
    Interp* interp
) {
    if (observed_rules_out != null) *observed_rules_out = {};
    if (db == null || observed_rules_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid observed counter state");
    }
    if (db.rule_signature_count == 0) return null;

    DeduceObservedRuleCounters[] observed_rules =
        mem::new_array(DeduceObservedRuleCounters, db.rule_signature_count);
    if (observed_rules.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    for (usz i = 0; i < observed_rules.len; i++) {
        observed_rules[i] = {};
        usz step_count = db.rule_signatures[i].body_count;
        if (step_count == 0) continue;
        DeduceObservedStepCounters[] steps = mem::new_array(DeduceObservedStepCounters, step_count);
        if (steps.ptr == null) {
            for (usz j = 0; j < i; j++) {
                if (observed_rules[j].steps != null) mem::free(observed_rules[j].steps);
            }
            mem::free(observed_rules.ptr);
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        observed_rules[i].steps = steps.ptr;
        observed_rules[i].step_count = step_count;
        deduce_observed_rule_counters_reset(&observed_rules[i]);
    }

    *observed_rules_out = observed_rules;
    return null;
}

fn void deduce_analyze_free_observed_rules(DeduceObservedRuleCounters[] observed_rules) {
    if (observed_rules.ptr == null) return;
    for (usz i = 0; i < observed_rules.len; i++) {
        if (observed_rules[i].steps != null) mem::free(observed_rules[i].steps);
    }
    mem::free(observed_rules.ptr);
}
