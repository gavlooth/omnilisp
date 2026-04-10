module lisp;

import std::core::mem;

fn Value* deduce_match_build_head_demand(
    Value* pattern,
    Relation* rel,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0) {
        *out_head_demand = { .predicate = rel != null ? rel.name : (SymbolId)0, .term_count = 0 };
        return null;
    }

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
    }

    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }

    Value* args = cdr(pattern);
    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        if (!is_wildcard(pattern_elem, interp) && !is_logic_var(pattern_elem, interp)) {
            term_bound[col] = true;
            term_values[col] = pattern_elem;
            *has_any_bound_out = true;
        }
        args = cdr(args);
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}

fn Value* deduce_match_scan_relation_in_txn(
    Relation* rel,
    Value* pattern,
    MdbTxn* txn,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid transaction scan state");
    }
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    Value*[] tuple_vals = {};
    if (rel.col_count > 0) {
        tuple_vals = mem::new_array(Value*, rel.col_count);
        if (tuple_vals.ptr == null) {
            return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
        }
    }
    defer if (tuple_vals.ptr != null) mem::free(tuple_vals.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    Value* result_rev = make_nil(interp);
    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        Bindings bindings = {};
        bindings_init(&bindings);
        if (catch err = match_clause_against_tuple(pattern, tuple_vals, rel, &bindings, interp)) {
            // Pattern did not match this tuple.
        } else {
            if (subject_keys_out != null) {
                deduce_goal_directed_read_subject_key_capture_record(
                    subject_keys_out,
                    key.mv_data,
                    key.mv_size
                );
            }
            Value* result_dict = build_result_dict(&bindings, null, interp);
            if (result_dict != null && result_dict.tag == ERROR) {
                bindings_deinit(&bindings);
                return result_dict;
            }
            result_rev = make_cons(interp, result_dict, result_rev);
        }
        bindings_deinit(&bindings);
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return deduce_reverse_list(result_rev, interp);
}

fn Value* deduce_match_scan_relation(
    Relation* rel,
    Value* pattern,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);
    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    Value* result_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Bindings bindings = {};
        bindings_init(&bindings);

        if (catch err = match_clause_against_tuple(pattern, it.vals, rel, &bindings, interp)) {
            // Pattern didn't match this row — skip.
        } else {
            if (subject_keys_out != null) {
                deduce_goal_directed_read_subject_key_capture_record(
                    subject_keys_out,
                    it.key.mv_data,
                    it.key.mv_size
                );
            }
            Value* result_dict = build_result_dict(&bindings, null, interp);
            if (result_dict != null && result_dict.tag == ERROR) {
                bindings_deinit(&bindings);
                return result_dict;
            }
            result_rev = make_cons(interp, result_dict, result_rev);
        }
        bindings_deinit(&bindings);
    }
    return deduce_reverse_list(result_rev, interp);
}
