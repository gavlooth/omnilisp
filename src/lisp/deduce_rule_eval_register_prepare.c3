module lisp;

fn Value* deduce_rule_register_prepare_body_plan(
    DeduceDb* db,
    DeduceRuleIr* rule,
    SymbolId[] body_preds,
    bool[] body_negated,
    ulong[] body_bound_masks,
    int[] body_selected_indexes,
    bool[] body_filter_pushdown,
    ulong[] body_projection_masks,
    DeduceBodyPlanStep[] body_plan,
    Interp* interp
) {
    for (usz i = 0; i < rule.body_count; i++) {
        body_plan[i].logical_body_index = 0;
        body_plan[i].bound_mask = 0;
        body_plan[i].selected_index = -1;
        body_plan[i].op_kind = DEDUCE_PHYSICAL_FULL_SCAN;
    }

    Value* err = deduce_plan_conjunctive_body(
        db,
        rule.body,
        rule.body_count,
        rule.max_var_id,
        body_plan.ptr,
        body_plan.len,
        interp
    );
    if (err != null) return err;

    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_preds[i] = atom.predicate;
        body_negated[i] = atom.negated;
        body_bound_masks[i] = body_plan[i].bound_mask;
        body_selected_indexes[i] = body_plan[i].selected_index;
        body_filter_pushdown[i] = body_plan[i].bound_mask != 0 ||
            deduce_rule_atom_has_local_equality_constraint(&atom);
        body_projection_masks[i] = deduce_rule_projection_mask_for_step(rule, body_plan, i);
    }
    return null;
}

fn Value* deduce_rule_register_finalize_install(
    DeduceDb* db,
    DeduceRuleIr* rule,
    SymbolId[] body_preds,
    bool[] body_negated,
    ulong[] body_bound_masks,
    int[] body_selected_indexes,
    bool[] body_filter_pushdown,
    ulong[] body_projection_masks,
    DeduceRuleTermKind[] head_term_kinds,
    DeduceAggregateOpKind[] head_term_aggregate_ops,
    bool[] head_term_is_variable,
    uint[] head_term_var_ids,
    Value*[] head_term_literals,
    usz[] body_term_offsets,
    usz[] body_term_counts,
    bool[] body_term_is_variable,
    uint[] body_term_var_ids,
    Value*[] body_term_literals,
    Interp* interp
) {
    DeduceRuleRegisterStatus register_status =
        deduce_db_add_rule_signature(
            db,
            rule.head.predicate,
            body_preds,
            body_negated,
            body_bound_masks,
            body_selected_indexes,
            body_filter_pushdown,
            body_projection_masks,
            head_term_kinds,
            head_term_aggregate_ops,
            head_term_is_variable,
            head_term_var_ids,
            head_term_literals,
            body_term_offsets,
            body_term_counts,
            body_term_is_variable,
            body_term_var_ids,
            body_term_literals,
            rule.max_var_id
        );
    if (register_status != DEDUCE_RULE_REGISTER_OK) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    Value* materialized_err = deduce_db_note_rule_signature_install(db, interp);
    if (materialized_err != null) return materialized_err;
    Value* persist_err = deduce_db_persist_rule_catalog(db, interp);
    if (persist_err != null) return persist_err;
    Value* persist_signatures_err = deduce_db_persist_rule_signatures(db, interp);
    if (persist_signatures_err != null) return persist_signatures_err;
    return make_void(interp);
}
