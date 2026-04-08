module lisp;

import std::core::mem;

fn Value* deduce_rule_register_alloc_term_storage(
    DeduceRuleIr* rule,
    DeduceRuleRegisterScratch* scratch,
    Interp* interp
) {
    scratch.head_term_kinds = mem::new_array(DeduceRuleTermKind, rule.head.term_count);
    if (scratch.head_term_kinds.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.head_term_aggregate_ops = mem::new_array(DeduceAggregateOpKind, rule.head.term_count);
    if (scratch.head_term_aggregate_ops.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.head_term_is_variable = mem::new_array(bool, rule.head.term_count);
    if (scratch.head_term_is_variable.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.head_term_var_ids = mem::new_array(uint, rule.head.term_count);
    if (scratch.head_term_var_ids.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.head_term_literals = mem::new_array(Value*, rule.head.term_count);
    if (scratch.head_term_literals.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_term_offsets = mem::new_array(usz, rule.body_count);
    if (scratch.body_term_offsets.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_term_counts = mem::new_array(usz, rule.body_count);
    if (scratch.body_term_counts.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    usz total_body_term_count = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        if (total_body_term_count > usz.max - rule.body[i].term_count) {
            return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
        }
        total_body_term_count += rule.body[i].term_count;
    }

    scratch.body_term_is_variable = mem::new_array(bool, total_body_term_count);
    if (scratch.body_term_is_variable.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_term_var_ids = mem::new_array(uint, total_body_term_count);
    if (scratch.body_term_var_ids.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_term_literals = mem::new_array(Value*, total_body_term_count);
    if (scratch.body_term_literals.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    scratch.body_plan = mem::new_array(DeduceBodyPlanStep, rule.body_count);
    if (scratch.body_plan.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    return null;
}
