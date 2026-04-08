module lisp;

import std::core::mem;

fn Value* deduce_rule_validate_arity_for_atom(
    DeduceDb* db,
    DeduceRuleAtom* atom,
    Interp* interp
) {
    usz expected = 0;
    if (!deduce_db_lookup_relation_arity(db, atom.predicate, &expected)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce-rule!: predicate relation is not registered");
    }
    if (expected != atom.term_count) {
        return deduce_raise(interp, "deduce/arity-mismatch", "deduce-rule!: predicate arity mismatch");
    }
    return null;
}

fn Value* deduce_rule_validate_arity(DeduceDb* db, DeduceRuleIr* rule, Interp* interp) {
    Value* err = deduce_rule_validate_arity_for_atom(db, &rule.head, interp);
    if (err != null) return err;
    for (usz i = 0; i < rule.body_count; i++) {
        err = deduce_rule_validate_arity_for_atom(db, &rule.body[i], interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_rule_validate_head_variable_safety(DeduceRuleIr* rule, Interp* interp) {
    if (rule.max_var_id == 0) return null;

    bool[] bound_in_positive_body = mem::new_array(bool, (usz)rule.max_var_id + 1);
    if (bound_in_positive_body.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(bound_in_positive_body.ptr);

    for (usz i = 0; i < bound_in_positive_body.len; i++) bound_in_positive_body[i] = false;

    for (usz i = 0; i < rule.body_count; i++) {
        DeduceRuleAtom atom = rule.body[i];
        if (atom.negated) continue;
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (term.normalized_var_id < bound_in_positive_body.len) {
                bound_in_positive_body[term.normalized_var_id] = true;
            }
        }
    }

    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        if (term.normalized_var_id >= bound_in_positive_body.len ||
            !bound_in_positive_body[term.normalized_var_id]) {
            return deduce_raise(interp, "deduce/unsafe-rule", "deduce-rule!: head variable is not grounded by a positive body atom");
        }
    }
    return null;
}

fn Value* deduce_rule_validate_negation_safety(DeduceRuleIr* rule, Interp* interp) {
    if (rule.max_var_id == 0) return null;

    bool[] bound_so_far = mem::new_array(bool, (usz)rule.max_var_id + 1);
    if (bound_so_far.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(bound_so_far.ptr);

    for (usz i = 0; i < bound_so_far.len; i++) bound_so_far[i] = false;

    for (usz i = 0; i < rule.body_count; i++) {
        DeduceRuleAtom atom = rule.body[i];
        if (atom.negated) {
            for (usz t = 0; t < atom.term_count; t++) {
                DeduceRuleTerm term = atom.terms[t];
                if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
                if (term.normalized_var_id >= bound_so_far.len ||
                    !bound_so_far[term.normalized_var_id]) {
                    return deduce_raise(interp, "deduce/unsafe-rule", "deduce-rule!: negated atom uses an unbound variable");
                }
            }
            continue;
        }

        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (term.normalized_var_id < bound_so_far.len) {
                bound_so_far[term.normalized_var_id] = true;
            }
        }
    }
    return null;
}
