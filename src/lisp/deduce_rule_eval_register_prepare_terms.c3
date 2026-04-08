module lisp;

fn Value* deduce_rule_register_prepare_head_terms(
    DeduceRuleIr* rule,
    DeduceRuleTermKind[] head_term_kinds,
    DeduceAggregateOpKind[] head_term_aggregate_ops,
    bool[] head_term_is_variable,
    uint[] head_term_var_ids,
    Value*[] head_term_literals,
    Interp* interp
) {
    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
        head_term_kinds[i] = term.kind;
        head_term_aggregate_ops[i] = term.aggregate_op;
        head_term_is_variable[i] = is_var;
        head_term_var_ids[i] =
            (term.kind == DEDUCE_RULE_TERM_VARIABLE || term.kind == DEDUCE_RULE_TERM_AGGREGATE)
                ? term.normalized_var_id
                : 0;
        if (is_var || term.kind == DEDUCE_RULE_TERM_AGGREGATE) {
            head_term_literals[i] = null;
        } else {
            head_term_literals[i] = boundary_promote_to_escape(term.literal_value, interp);
        }
    }
    return null;
}

fn Value* deduce_rule_register_prepare_body_terms(
    DeduceRuleIr* rule,
    DeduceBodyPlanStep[] body_plan,
    usz[] body_term_offsets,
    usz[] body_term_counts,
    bool[] body_term_is_variable,
    uint[] body_term_var_ids,
    Value*[] body_term_literals,
    Interp* interp
) {
    usz body_term_cursor = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_term_offsets[i] = body_term_cursor;
        body_term_counts[i] = atom.term_count;
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
            body_term_is_variable[body_term_cursor] = is_var;
            body_term_var_ids[body_term_cursor] = is_var ? term.normalized_var_id : 0;
            if (is_var) {
                body_term_literals[body_term_cursor] = null;
            } else {
                body_term_literals[body_term_cursor] = boundary_promote_to_escape(term.literal_value, interp);
            }
            body_term_cursor++;
        }
    }
    return null;
}
