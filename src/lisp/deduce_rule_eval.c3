module lisp;

// Validation helpers are factored to /src/lisp/deduce_rule_eval_validation.c3.

fn Value* deduce_rule_validate_install_candidate(DeduceDb* db, DeduceRuleIr* rule, Interp* interp) {
    Value* err = deduce_rule_validate_arity(db, rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_head_variable_safety(rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_negation_safety(rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_aggregate_projections(rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_aggregate_head_migration(db, rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_aggregate_recursion(db, rule, interp);
    if (err != null) return err;

    err = deduce_rule_validate_constrained_head_relation(db, rule, interp);
    if (err != null) return err;

    return deduce_rule_validate_stratification(db, rule, interp);
}
