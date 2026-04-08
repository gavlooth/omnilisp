module lisp;

fn Value* deduce_naive_evaluate_recursive_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass_in_txn(
            db,
            plan,
            component_id,
            txn,
            head_demand,
            &changed,
            &derived,
            observed_rules,
            interp
        );
        if (err != null) {
            return err;
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        if (!changed) {
            return deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}
