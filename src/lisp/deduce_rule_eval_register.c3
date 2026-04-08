module lisp;

fn Value* deduce_rule_register_ir(
    DeduceDb* db,
    DeduceRuleIr* rule,
    Interp* interp
) {
    if (db == null || rule == null || interp == null) {
        return deduce_raise(interp, "deduce/rule-invalid-state", "deduce-rule!: invalid rule registration state");
    }

    DeduceRuleRegisterScratch scratch = {};
    Value* err = deduce_rule_register_alloc_scratch(rule, &scratch, interp);
    if (err != null) return err;
    defer deduce_rule_register_free_scratch(&scratch);

    err = deduce_rule_register_prepare_body_plan(
        db,
        rule,
        scratch.body_preds,
        scratch.body_negated,
        scratch.body_bound_masks,
        scratch.body_selected_indexes,
        scratch.body_filter_pushdown,
        scratch.body_projection_masks,
        scratch.body_plan,
        interp
    );
    if (err != null) return err;
    err = deduce_rule_register_prepare_head_terms(
        rule,
        scratch.head_term_kinds,
        scratch.head_term_aggregate_ops,
        scratch.head_term_is_variable,
        scratch.head_term_var_ids,
        scratch.head_term_literals,
        interp
    );
    if (err != null) return err;

    err = deduce_rule_register_prepare_body_terms(
        rule,
        scratch.body_plan,
        scratch.body_term_offsets,
        scratch.body_term_counts,
        scratch.body_term_is_variable,
        scratch.body_term_var_ids,
        scratch.body_term_literals,
        interp
    );
    if (err != null) return err;

    return deduce_rule_register_finalize_install(
        db,
        rule,
        scratch.body_preds,
        scratch.body_negated,
        scratch.body_bound_masks,
        scratch.body_selected_indexes,
        scratch.body_filter_pushdown,
        scratch.body_projection_masks,
        scratch.head_term_kinds,
        scratch.head_term_aggregate_ops,
        scratch.head_term_is_variable,
        scratch.head_term_var_ids,
        scratch.head_term_literals,
        scratch.body_term_offsets,
        scratch.body_term_counts,
        scratch.body_term_is_variable,
        scratch.body_term_var_ids,
        scratch.body_term_literals,
        interp
    );
}
