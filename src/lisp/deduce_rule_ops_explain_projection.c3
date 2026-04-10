module lisp;

fn Value* deduce_explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    return out;
}

fn Value* deduce_explain_build_head_projections(
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (sig == null || sig.head_term_count == 0 || sig.head_term_kinds == null) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.head_term_count; i++) {
        Value* entry = explain_make_dict(interp, 8);
        DeduceRuleTermKind kind = sig.head_term_kinds[i];
        explain_dict_set(entry, interp, "position", make_int(interp, (long)i));
        explain_dict_set_symbol(entry, interp, "projection-kind",
            deduce_explain_head_projection_symbol(interp, kind));
        if (kind == DEDUCE_RULE_TERM_LITERAL) {
            Value* literal = sig.head_term_literals != null ? sig.head_term_literals[i] : null;
            explain_dict_set(entry, interp, "literal", literal == null ? make_nil(interp) : literal);
            explain_dict_set(entry, interp, "source-var-id", make_nil(interp));
            explain_dict_set(entry, interp, "aggregate-function", make_nil(interp));
        } else {
            uint source_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[i] : 0;
            explain_dict_set(entry, interp, "literal", make_nil(interp));
            explain_dict_set(entry, interp, "source-var-id", make_int(interp, (long)source_var_id));
            if (kind == DEDUCE_RULE_TERM_AGGREGATE && sig.head_term_aggregate_ops != null) {
                explain_dict_set_symbol(entry, interp, "aggregate-function",
                    deduce_explain_aggregate_op_symbol(interp, sig.head_term_aggregate_ops[i]));
            } else {
                explain_dict_set(entry, interp, "aggregate-function", make_nil(interp));
            }
        }
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn long deduce_explain_rows_read_estimate(DeduceRelationSchema* schema) {
    if (schema == null) return 0;
    if (schema.cardinality_estimate > (ulong)long.max) return long.max;
    return (long)schema.cardinality_estimate;
}

fn long deduce_explain_rows_emitted_estimate(
    DeduceRelationSchema* schema,
    ulong bound_mask,
    int selected_index
) {
    if (schema == null) return 0;
    long rows_read = deduce_explain_rows_read_estimate(schema);
    if (rows_read <= 0) return 0;
    if (selected_index < 0) return rows_read;
    if (bound_mask == 0) return rows_read;
    if (schema.distinct_estimate == 0) return 1;

    ulong distinct = schema.distinct_estimate;
    if (distinct > schema.cardinality_estimate) distinct = schema.cardinality_estimate;
    if (distinct == 0) return 1;
    if (distinct >= schema.cardinality_estimate) return 1;

    long emitted = rows_read / (long)distinct;
    if (emitted <= 0) emitted = 1;
    if (emitted > rows_read) emitted = rows_read;
    return emitted;
}
