module lisp;

import std::core::mem;

fn Value* deduce_build_aggregate_group_head_tuple(
    DeduceRuleSignature* sig,
    DeduceAggregateGroup* group,
    DeduceAggregateTermState* term_states,
    Value*[] head_tuple,
    Interp* interp
) {
    if (sig == null || group == null || term_states == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate tuple state");
    }
    if (sig.head_term_kinds == null || sig.head_term_aggregate_ops == null ||
        sig.head_term_literals == null || group.head_values == null || head_tuple.len != sig.head_term_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate tuple metadata");
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        switch (sig.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_VARIABLE:
                head_tuple[i] = group.head_values[i];
                break;
            case DEDUCE_RULE_TERM_LITERAL:
                head_tuple[i] = sig.head_term_literals[i];
                break;
            case DEDUCE_RULE_TERM_AGGREGATE: {
                DeduceAggregateTermState* state = &term_states[i];
                if (!state.initialized) {
                    return deduce_raise(interp, "deduce/analyze-invalid-state",
                        "deduce/analyze: aggregate finalize state is incomplete");
                }

                switch (sig.head_term_aggregate_ops[i]) {
                    case DEDUCE_AGGREGATE_COUNT:
                        head_tuple[i] = make_int(interp, state.count_value);
                        break;
                    case DEDUCE_AGGREGATE_SUM:
                        head_tuple[i] = state.sum_is_double
                            ? make_double(interp, state.sum_double_value)
                            : make_int(interp, state.sum_int_value);
                        break;
                    case DEDUCE_AGGREGATE_MIN:
                    case DEDUCE_AGGREGATE_MAX:
                        head_tuple[i] = state.extremum_value;
                        break;
                    case DEDUCE_AGGREGATE_NONE:
                        return deduce_raise(interp, "deduce/analyze-invalid-state",
                            "deduce/analyze: invalid aggregate operator");
                }
                break;
            }
        }
    }
    return null;
}

fn Value* deduce_encode_aggregate_group_tuple(
    DeduceRuleSignature* sig,
    DeduceAggregateGroup* group,
    DeduceAggregateTermState* term_states,
    Value*[] head_tuple,
    DeduceEncodedTuple* tuple_out,
    Interp* interp
) {
    if (tuple_out == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate tuple target");
    }
    *tuple_out = {};

    Value* err = deduce_build_aggregate_group_head_tuple(sig, group, term_states, head_tuple, interp);
    if (err != null) return err;

    char[4096] key_buf;
    usz? key_len = encode_tuple(head_tuple, key_buf[..], key_buf.len);
    if (catch err2 = key_len) {
        return deduce_raise(interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }

    if (key_len > 0) {
        tuple_out.key_bytes = (char*)mem::malloc(key_len);
        if (tuple_out.key_bytes == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < key_len; i++) tuple_out.key_bytes[i] = key_buf[i];
    }
    tuple_out.key_len = key_len;
    return null;
}

fn void deduce_aggregate_group_commit_materialized_state(DeduceAggregateGroup* group, DeduceRuleSignature* sig) {
    if (group == null || sig == null || group.term_states == null || group.previous_term_states == null) return;
    for (usz i = 0; i < sig.head_term_count; i++) {
        group.previous_term_states[i] = group.term_states[i];
    }
    group.previous_support_count = group.support_count;
    group.materialized = group.support_count > 0;
    group.dirty = false;
}

fn Value* deduce_naive_materialize_aggregate_groups(
    DeduceNaiveRuleExec* exec,
    DeduceEncodedTuple** tuples_out,
    usz* tuple_count_out
) {
    if (exec == null || exec.sig == null || exec.interp == null || exec.aggregate_groups == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }
    if (tuples_out == null || tuple_count_out == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate materialization target");
    }
    *tuples_out = null;
    *tuple_count_out = 0;

    usz tuple_count = exec.aggregate_groups.count;
    if (tuple_count == 0) return null;
    if (tuple_count > usz.max / DeduceEncodedTuple.sizeof) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    DeduceEncodedTuple* tuples = (DeduceEncodedTuple*)mem::malloc(DeduceEncodedTuple.sizeof * tuple_count);
    if (tuples == null) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < tuple_count; i++) tuples[i] = {};

    for (usz group_idx = 0; group_idx < tuple_count; group_idx++) {
        DeduceAggregateGroup* group = &exec.aggregate_groups.entries[group_idx];
        for (usz i = 0; i < exec.sig.head_term_count; i++) {
            switch (exec.sig.head_term_kinds[i]) {
                case DEDUCE_RULE_TERM_VARIABLE:
                    exec.head_tuple[i] = group.head_values != null ? group.head_values[i] : null;
                    break;
                case DEDUCE_RULE_TERM_LITERAL:
                    exec.head_tuple[i] = exec.sig.head_term_literals[i];
                    break;
                case DEDUCE_RULE_TERM_AGGREGATE: {
                    DeduceAggregateTermState* state =
                        group.term_states != null ? &group.term_states[i] : null;
                    if (state == null || !state.initialized) {
                        deduce_encoded_tuples_release(tuples, tuple_count);
                        return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
                            "deduce/analyze: aggregate finalize state is incomplete");
                    }

                    switch (exec.sig.head_term_aggregate_ops[i]) {
                        case DEDUCE_AGGREGATE_COUNT:
                            exec.head_tuple[i] = make_int(exec.interp, state.count_value);
                            break;
                        case DEDUCE_AGGREGATE_SUM:
                            exec.head_tuple[i] = state.sum_is_double
                                ? make_double(exec.interp, state.sum_double_value)
                                : make_int(exec.interp, state.sum_int_value);
                            break;
                        case DEDUCE_AGGREGATE_MIN:
                        case DEDUCE_AGGREGATE_MAX:
                            exec.head_tuple[i] = state.extremum_value;
                            break;
                        case DEDUCE_AGGREGATE_NONE:
                            deduce_encoded_tuples_release(tuples, tuple_count);
                            return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
                                "deduce/analyze: invalid aggregate operator");
                    }
                    break;
                }
            }
        }

        char[4096] key_buf;
        usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
        if (catch err = key_len) {
            deduce_encoded_tuples_release(tuples, tuple_count);
            return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
        }

        if (key_len > 0) {
            tuples[group_idx].key_bytes = (char*)mem::malloc(key_len);
            if (tuples[group_idx].key_bytes == null) {
                deduce_encoded_tuples_release(tuples, tuple_count);
                return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
            }
            for (usz i = 0; i < key_len; i++) tuples[group_idx].key_bytes[i] = key_buf[i];
        }
        tuples[group_idx].key_len = key_len;
    }

    *tuples_out = tuples;
    *tuple_count_out = tuple_count;
    return null;
}

fn Value* deduce_relation_count_rows(
    MdbTxn* txn,
    MdbDbi dbi,
    long* count_out,
    Interp* interp
) {
    if (txn == null || count_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid relation count state");
    }

    *count_out = 0;
    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        if (*count_out < long.max) *count_out = *count_out + 1;
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}
