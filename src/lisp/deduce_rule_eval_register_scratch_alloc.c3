module lisp;

import std::core::mem;

fn Value* deduce_rule_register_alloc_scratch(
    DeduceRuleIr* rule,
    DeduceRuleRegisterScratch* scratch_out,
    Interp* interp
) {
    if (scratch_out != null) *scratch_out = {};
    if (rule == null || scratch_out == null || interp == null) {
        return deduce_raise(interp, "deduce/rule-invalid-state", "deduce-rule!: invalid rule scratch state");
    }

    DeduceRuleRegisterScratch scratch = {};
    scratch.body_preds = mem::new_array(SymbolId, rule.body_count);
    if (scratch.body_preds.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_negated = mem::new_array(bool, rule.body_count);
    if (scratch.body_negated.ptr == null) {
        deduce_rule_register_free_scratch(&scratch);
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_bound_masks = mem::new_array(ulong, rule.body_count);
    if (scratch.body_bound_masks.ptr == null) {
        deduce_rule_register_free_scratch(&scratch);
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_selected_indexes = mem::new_array(int, rule.body_count);
    if (scratch.body_selected_indexes.ptr == null) {
        deduce_rule_register_free_scratch(&scratch);
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_filter_pushdown = mem::new_array(bool, rule.body_count);
    if (scratch.body_filter_pushdown.ptr == null) {
        deduce_rule_register_free_scratch(&scratch);
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_projection_masks = mem::new_array(ulong, rule.body_count);
    if (scratch.body_projection_masks.ptr == null) {
        deduce_rule_register_free_scratch(&scratch);
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    Value* term_err = deduce_rule_register_alloc_term_storage(rule, &scratch, interp);
    if (term_err != null) {
        deduce_rule_register_free_scratch(&scratch);
        return term_err;
    }

    *scratch_out = scratch;
    return null;
}
