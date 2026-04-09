module lisp;

fn Value* deduce_naive_evaluate_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    usz iteration_limit,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool has_rules = false;
    for (usz i = 0; i < plan.rule_component_ids.len; i++) {
        if (plan.rule_component_ids[i] == component_id) {
            has_rules = true;
            break;
        }
    }
    if (!has_rules) return null;

    bool recursive = plan.component_recursive.ptr != null && plan.component_recursive[component_id];
    bool has_recursive_aggregates = recursive && deduce_component_has_aggregate_rules(db, plan, component_id);
    if (!recursive) {
        return deduce_naive_evaluate_nonrecursive_component_in_txn(
            db,
            plan,
            component_id,
            txn,
            head_demand,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    if (use_seminaive_recursive && !has_recursive_aggregates) {
        return deduce_seminaive_evaluate_recursive_component_in_txn(
            db,
            plan,
            component_id,
            iteration_limit,
            txn,
            head_demand,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    if (use_seminaive_recursive && has_recursive_aggregates &&
        deduce_component_can_use_seminaive_aggregates(db, plan, component_id) &&
        deduce_component_has_full_recursive_aggregate_seminaive_support(db, plan, component_id)) {
        return deduce_seminaive_evaluate_recursive_aggregate_component_in_txn(
            db,
            plan,
            component_id,
            iteration_limit,
            txn,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    return deduce_naive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        head_demand,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
}
