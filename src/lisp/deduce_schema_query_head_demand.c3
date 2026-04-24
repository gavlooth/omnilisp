module lisp;

import std::core::mem;

fn void deduce_query_append_list_to_reversed(Value* list, Value** reversed_out, Interp* interp) {
    if (reversed_out == null || interp == null) return;
    Value* current = list;
    while (current != null && current.tag == CONS) {
        *reversed_out = make_cons(interp, current.cons_val.car, *reversed_out);
        current = current.cons_val.cdr;
    }
}

fn Value* deduce_query_try_build_head_demand_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null ||
        has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_head_demand,
        has_any_bound_out,
        supported_out,
        interp
    );
    if (demand_err != null || !*supported_out || out_head_demand.term_bound == null || out_head_demand.term_values == null) {
        return demand_err;
    }
    *out_term_bound = out_head_demand.term_bound[..out_head_demand.term_count];
    *out_term_values = out_head_demand.term_values[..out_head_demand.term_count];
    return null;
}

fn Value* deduce_query_scan_relation_in_txn(
    Relation* rel,
    Value* filter_fn,
    MdbTxn* txn,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || filter_fn == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid in-transaction query state");
    }
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open_in_txn(&it, rel, interp, txn, null, 0, false, {}, {}, false)) {
        Value* open_err = deduce_tuple_iterator_error(&it);
        if (open_err != null) return open_err;
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid iterator state");
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            if (subject_keys_out != null) {
                char[4096] key_buf;
                usz? key_len = encode_tuple(it.vals, key_buf[..], key_buf.len);
                if (try encoded_row_key_len = key_len) {
                    deduce_goal_directed_read_subject_key_capture_record(
                        subject_keys_out,
                        &key_buf[0],
                        encoded_row_key_len
                    );
                } else {
                    subject_keys_out.complete = false;
                }
            }
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    Value* scan_err = deduce_tuple_iterator_error(&it);
    if (scan_err != null) return scan_err;
    return deduce_reverse_list(filtered_rev, interp);
}
