module lisp;

import std::core::mem;

fn DeduceRuleRegisterStatus deduce_db_add_rule_signature(
    DeduceDb* db,
    SymbolId head_predicate,
    SymbolId[] body_predicates,
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
    uint max_var_id
) {
    if (db == null ||
        body_predicates.len != body_negated.len ||
        body_predicates.len != body_bound_masks.len ||
        body_predicates.len != body_selected_indexes.len ||
        body_predicates.len != body_filter_pushdown.len ||
        body_predicates.len != body_projection_masks.len ||
        head_term_kinds.len != head_term_aggregate_ops.len ||
        head_term_kinds.len != head_term_is_variable.len ||
        head_term_is_variable.len != head_term_var_ids.len ||
        head_term_is_variable.len != head_term_literals.len ||
        body_term_offsets.len != body_term_counts.len ||
        body_term_is_variable.len != body_term_var_ids.len ||
        body_term_is_variable.len != body_term_literals.len) {
        return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
    }
    for (usz i = 0; i < body_term_counts.len; i++) {
        if (i >= body_term_offsets.len) return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        if (body_term_offsets[i] > body_term_is_variable.len ||
            body_term_counts[i] > body_term_is_variable.len - body_term_offsets[i]) {
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
    }

    if (db.rule_signature_count == usz.max) return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
    usz needed = db.rule_signature_count + 1;
    if (!deduce_db_ensure_rule_signature_capacity(db, needed)) {
        return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
    }

    DeduceRuleSignature* dst = &db.rule_signatures[db.rule_signature_count];
    dst.head_predicate = head_predicate;
    dst.body_count = body_predicates.len;
    dst.body_predicates = null;
    dst.body_negated = null;
    dst.body_bound_masks = null;
    dst.body_selected_indexes = null;
    dst.body_filter_pushdown = null;
    dst.body_projection_masks = null;
    dst.head_term_kinds = null;
    dst.head_term_aggregate_ops = null;
    dst.head_term_is_variable = null;
    dst.head_term_var_ids = null;
    dst.head_term_literals = null;
    dst.head_term_count = head_term_kinds.len;
    dst.aggregate_projection_count = 0;
    dst.body_term_offsets = null;
    dst.body_term_counts = null;
    dst.body_term_is_variable = null;
    dst.body_term_var_ids = null;
    dst.body_term_literals = null;
    dst.body_term_total_count = body_term_is_variable.len;
    dst.max_var_id = max_var_id;

    if (body_predicates.len > 0) {
        if (body_predicates.len > usz.max / SymbolId.sizeof ||
            body_negated.len > usz.max / bool.sizeof ||
            body_bound_masks.len > usz.max / ulong.sizeof ||
            body_selected_indexes.len > usz.max / int.sizeof ||
            body_filter_pushdown.len > usz.max / bool.sizeof ||
            body_projection_masks.len > usz.max / ulong.sizeof) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_predicates = (SymbolId*)mem::malloc(SymbolId.sizeof * body_predicates.len);
        if (dst.body_predicates == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_negated = (bool*)mem::malloc(bool.sizeof * body_negated.len);
        if (dst.body_negated == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_bound_masks = (ulong*)mem::malloc(ulong.sizeof * body_bound_masks.len);
        if (dst.body_bound_masks == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_selected_indexes = (int*)mem::malloc(int.sizeof * body_selected_indexes.len);
        if (dst.body_selected_indexes == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_filter_pushdown = (bool*)mem::malloc(bool.sizeof * body_filter_pushdown.len);
        if (dst.body_filter_pushdown == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_projection_masks = (ulong*)mem::malloc(ulong.sizeof * body_projection_masks.len);
        if (dst.body_projection_masks == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_predicates.len; i++) {
            dst.body_predicates[i] = body_predicates[i];
            dst.body_negated[i] = body_negated[i];
            dst.body_bound_masks[i] = body_bound_masks[i];
            dst.body_selected_indexes[i] = body_selected_indexes[i];
            dst.body_filter_pushdown[i] = body_filter_pushdown[i];
            dst.body_projection_masks[i] = body_projection_masks[i];
        }
    }

    if (head_term_kinds.len > 0) {
        if (head_term_kinds.len > usz.max / DeduceRuleTermKind.sizeof ||
            head_term_aggregate_ops.len > usz.max / DeduceAggregateOpKind.sizeof ||
            head_term_is_variable.len > usz.max / bool.sizeof ||
            head_term_var_ids.len > usz.max / uint.sizeof ||
            head_term_literals.len > usz.max / Value*.sizeof) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_kinds = (DeduceRuleTermKind*)mem::malloc(DeduceRuleTermKind.sizeof * head_term_kinds.len);
        if (dst.head_term_kinds == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_aggregate_ops = (DeduceAggregateOpKind*)mem::malloc(DeduceAggregateOpKind.sizeof * head_term_aggregate_ops.len);
        if (dst.head_term_aggregate_ops == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_is_variable = (bool*)mem::malloc(bool.sizeof * head_term_is_variable.len);
        if (dst.head_term_is_variable == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_var_ids = (uint*)mem::malloc(uint.sizeof * head_term_var_ids.len);
        if (dst.head_term_var_ids == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_literals = (Value**)mem::malloc(Value*.sizeof * head_term_literals.len);
        if (dst.head_term_literals == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < head_term_kinds.len; i++) {
            dst.head_term_kinds[i] = head_term_kinds[i];
            dst.head_term_aggregate_ops[i] = head_term_aggregate_ops[i];
            dst.head_term_is_variable[i] = head_term_is_variable[i];
            dst.head_term_var_ids[i] = head_term_var_ids[i];
            dst.head_term_literals[i] = head_term_literals[i];
            if (head_term_kinds[i] == DEDUCE_RULE_TERM_AGGREGATE) {
                dst.aggregate_projection_count++;
            }
        }
    }

    if (body_term_counts.len > 0) {
        if (body_term_offsets.len > usz.max / usz.sizeof ||
            body_term_counts.len > usz.max / usz.sizeof) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_offsets = (usz*)mem::malloc(usz.sizeof * body_term_offsets.len);
        if (dst.body_term_offsets == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_counts = (usz*)mem::malloc(usz.sizeof * body_term_counts.len);
        if (dst.body_term_counts == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_term_counts.len; i++) {
            dst.body_term_offsets[i] = body_term_offsets[i];
            dst.body_term_counts[i] = body_term_counts[i];
        }
    }

    if (body_term_is_variable.len > 0) {
        if (body_term_is_variable.len > usz.max / bool.sizeof ||
            body_term_var_ids.len > usz.max / uint.sizeof ||
            body_term_literals.len > usz.max / Value*.sizeof) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_is_variable = (bool*)mem::malloc(bool.sizeof * body_term_is_variable.len);
        if (dst.body_term_is_variable == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_var_ids = (uint*)mem::malloc(uint.sizeof * body_term_var_ids.len);
        if (dst.body_term_var_ids == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_literals = (Value**)mem::malloc(Value*.sizeof * body_term_literals.len);
        if (dst.body_term_literals == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_term_is_variable.len; i++) {
            dst.body_term_is_variable[i] = body_term_is_variable[i];
            dst.body_term_var_ids[i] = body_term_var_ids[i];
            dst.body_term_literals[i] = body_term_literals[i];
        }
    }

    db.rule_signature_count++;
    return DEDUCE_RULE_REGISTER_OK;
}
