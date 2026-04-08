module lisp;

fn Value* deduce_seminaive_run_recursive_component_iterations_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    Interp* interp
) {
    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            DeduceObservedRuleCounters* observed_rule =
                observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
            Value* err = deduce_seminaive_evaluate_rule(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                head_demand,
                &derived,
                &changed,
                observed_rule,
                interp
            );
            if (err != null) {
                return err;
            }
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
        if (!has_delta) {
            return deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}
