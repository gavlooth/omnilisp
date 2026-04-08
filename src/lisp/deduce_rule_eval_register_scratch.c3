module lisp;

import std::core::mem;

struct DeduceRuleRegisterScratch {
    SymbolId[] body_preds;
    bool[] body_negated;
    ulong[] body_bound_masks;
    int[] body_selected_indexes;
    bool[] body_filter_pushdown;
    ulong[] body_projection_masks;
    DeduceRuleTermKind[] head_term_kinds;
    DeduceAggregateOpKind[] head_term_aggregate_ops;
    bool[] head_term_is_variable;
    uint[] head_term_var_ids;
    Value*[] head_term_literals;
    usz[] body_term_offsets;
    usz[] body_term_counts;
    bool[] body_term_is_variable;
    uint[] body_term_var_ids;
    Value*[] body_term_literals;
    DeduceBodyPlanStep[] body_plan;
}

fn void deduce_rule_register_free_scratch(DeduceRuleRegisterScratch* scratch) {
    if (scratch == null) return;

    if (scratch.body_preds.ptr != null) mem::free(scratch.body_preds.ptr);
    if (scratch.body_negated.ptr != null) mem::free(scratch.body_negated.ptr);
    if (scratch.body_bound_masks.ptr != null) mem::free(scratch.body_bound_masks.ptr);
    if (scratch.body_selected_indexes.ptr != null) mem::free(scratch.body_selected_indexes.ptr);
    if (scratch.body_filter_pushdown.ptr != null) mem::free(scratch.body_filter_pushdown.ptr);
    if (scratch.body_projection_masks.ptr != null) mem::free(scratch.body_projection_masks.ptr);
    if (scratch.head_term_kinds.ptr != null) mem::free(scratch.head_term_kinds.ptr);
    if (scratch.head_term_aggregate_ops.ptr != null) mem::free(scratch.head_term_aggregate_ops.ptr);
    if (scratch.head_term_is_variable.ptr != null) mem::free(scratch.head_term_is_variable.ptr);
    if (scratch.head_term_var_ids.ptr != null) mem::free(scratch.head_term_var_ids.ptr);
    if (scratch.head_term_literals.ptr != null) mem::free(scratch.head_term_literals.ptr);
    if (scratch.body_term_offsets.ptr != null) mem::free(scratch.body_term_offsets.ptr);
    if (scratch.body_term_counts.ptr != null) mem::free(scratch.body_term_counts.ptr);
    if (scratch.body_term_is_variable.ptr != null) mem::free(scratch.body_term_is_variable.ptr);
    if (scratch.body_term_var_ids.ptr != null) mem::free(scratch.body_term_var_ids.ptr);
    if (scratch.body_term_literals.ptr != null) mem::free(scratch.body_term_literals.ptr);
    if (scratch.body_plan.ptr != null) mem::free(scratch.body_plan.ptr);
    *scratch = {};
}
