module lisp;

fn ulong deduce_rule_atom_bound_mask(DeduceRuleAtom* atom, bool[] bound_vars) {
    if (atom == null) return 0;
    ulong mask = 0;
    usz width = (usz)(ulong.sizeof * 8);
    for (usz t = 0; t < atom.term_count; t++) {
        bool bound = false;
        DeduceRuleTerm term = atom.terms[t];
        if (term.kind == DEDUCE_RULE_TERM_LITERAL) {
            bound = true;
        } else if (term.kind == DEDUCE_RULE_TERM_VARIABLE &&
                   term.normalized_var_id < bound_vars.len &&
                   bound_vars[term.normalized_var_id]) {
            bound = true;
        }

        if (bound && t < width) {
            mask |= ((ulong)1 << t);
        }
    }
    return mask;
}

fn void deduce_rule_mark_atom_variables_bound(DeduceRuleAtom* atom, bool[] bound_vars) {
    if (atom == null) return;
    for (usz t = 0; t < atom.term_count; t++) {
        DeduceRuleTerm term = atom.terms[t];
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE &&
            term.kind != DEDUCE_RULE_TERM_AGGREGATE) continue;
        if (term.normalized_var_id >= bound_vars.len) continue;
        bound_vars[term.normalized_var_id] = true;
    }
}

fn int deduce_rule_bound_mask_popcount(ulong mask) {
    int count = 0;
    ulong v = mask;
    while (v != 0) {
        count += (int)(v & 1);
        v >>= 1;
    }
    return count;
}

fn DeducePhysicalOpKind deduce_rule_choose_op_kind(DeduceRuleAtom* atom, int selected_index) {
    if (atom != null && atom.negated) {
        if (selected_index >= 0) return DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE;
        return DEDUCE_PHYSICAL_NEGATED_SCAN;
    }
    if (selected_index >= 0) return DEDUCE_PHYSICAL_INDEX_SCAN;
    return DEDUCE_PHYSICAL_FULL_SCAN;
}

fn bool deduce_rule_atom_has_local_equality_constraint(DeduceRuleAtom* atom) {
    if (atom == null || atom.term_count < 2) return false;
    for (usz i = 0; i < atom.term_count; i++) {
        DeduceRuleTerm ti = atom.terms[i];
        if (ti.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        for (usz j = i + 1; j < atom.term_count; j++) {
            DeduceRuleTerm tj = atom.terms[j];
            if (tj.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (ti.normalized_var_id == tj.normalized_var_id) return true;
        }
    }
    return false;
}

fn bool deduce_rule_var_used_in_head(DeduceRuleIr* rule, uint var_id) {
    if (rule == null || var_id == 0) return false;
    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        if ((term.kind == DEDUCE_RULE_TERM_VARIABLE ||
             term.kind == DEDUCE_RULE_TERM_AGGREGATE) &&
            term.normalized_var_id == var_id) {
            return true;
        }
    }
    return false;
}

fn bool deduce_rule_var_used_in_planned_suffix(
    DeduceRuleIr* rule,
    DeduceBodyPlanStep[] plan_steps,
    usz from_step_exclusive,
    uint var_id
) {
    if (rule == null || var_id == 0) return false;
    for (usz step = from_step_exclusive + 1; step < plan_steps.len; step++) {
        usz logical_idx = plan_steps[step].logical_body_index;
        DeduceRuleAtom* atom = &rule.body[logical_idx];
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind == DEDUCE_RULE_TERM_VARIABLE && term.normalized_var_id == var_id) {
                return true;
            }
        }
    }
    return false;
}
