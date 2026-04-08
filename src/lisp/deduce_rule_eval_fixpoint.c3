module lisp;

fn Value* deduce_naive_fixpoint_evaluate_with_plan(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz max_iterations,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    long* max_component_iterations_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (iterations_out != null) *iterations_out = 0;
    if (derived_out != null) *derived_out = 0;
    if (max_component_iterations_out != null) *max_component_iterations_out = 0;
    if (db == null || !db.open || db.env == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: database is not open");
    }
    if (db.rule_signature_count == 0 || plan.component_count == 0) return null;

    usz hard_limit = max_iterations > 0 ? max_iterations : 256;
    for (usz i = 0; i < plan.component_order.len; i++) {
        usz component_id = plan.component_order[i];
        long before_iterations = iterations_out != null ? *iterations_out : 0;
        Value* err = deduce_naive_evaluate_component(
            db,
            plan,
            component_id,
            hard_limit,
            use_seminaive_recursive,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
        if (err != null) return err;

        if (max_component_iterations_out != null && iterations_out != null) {
            long component_iterations = *iterations_out - before_iterations;
            if (component_iterations > *max_component_iterations_out) {
                *max_component_iterations_out = component_iterations;
            }
        }
    }
    return null;
}
