module lisp;

import std::core::mem;

fn Value* deduce_naive_eval_rule_step(
    DeduceNaiveRuleExec* exec,
    usz step_idx,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (step_idx >= exec.sig.body_count) {
        return deduce_naive_emit_head_fact(exec, var_bound, var_values);
    }

    SymbolId predicate = exec.sig.body_predicates != null ? exec.sig.body_predicates[step_idx] : (SymbolId)0;
    bool negated = exec.sig.body_negated != null ? exec.sig.body_negated[step_idx] : false;

    MdbDbi body_dbi = 0;
    DeduceRelationSchema* body_schema = null;
    if (!deduce_rule_open_existing_dbi(exec.db, exec.txn, predicate, &body_dbi, &body_schema, exec.interp)) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }
    if (body_schema == null) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }
    if (deduce_rule_step_has_bound_join_input(exec.sig, step_idx, var_bound)) {
        deduce_observed_step_note_join_probe(exec.observed_rule_counters, step_idx);
    }

    Value*[] tuple_vals = {};
    if (body_schema.arity > 0) {
        tuple_vals = mem::new_array(Value*, body_schema.arity);
        if (tuple_vals.ptr == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (tuple_vals.ptr != null) mem::free(tuple_vals.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(exec.txn, body_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(exec.interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);

    if (negated) {
        bool found_match = false;
        while (rc == MDB_SUCCESS) {
            deduce_observed_step_note_rows_read(exec.observed_rule_counters, step_idx);
            usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
            if (catch err = decoded_count) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (decoded_count != body_schema.arity) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (deduce_rule_match_negated_tuple(exec.sig, step_idx, tuple_vals, var_bound, var_values)) {
                found_match = true;
                break;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        if (found_match) return null;
        deduce_observed_step_note_rows_emitted(exec.observed_rule_counters, step_idx);
        return deduce_naive_eval_rule_step(exec, step_idx + 1, var_bound, var_values);
    }

    bool[] candidate_bound = mem::new_array(bool, var_bound.len);
    if (candidate_bound.ptr == null && var_bound.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_bound.ptr != null) mem::free(candidate_bound.ptr);

    Value*[] candidate_values = mem::new_array(Value*, var_values.len);
    if (candidate_values.ptr == null && var_values.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_values.ptr != null) mem::free(candidate_values.ptr);

    while (rc == MDB_SUCCESS) {
        deduce_observed_step_note_rows_read(exec.observed_rule_counters, step_idx);
        usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (decoded_count != body_schema.arity) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        for (usz i = 0; i < var_bound.len; i++) candidate_bound[i] = var_bound[i];
        for (usz i = 0; i < var_values.len; i++) candidate_values[i] = var_values[i];

        if (deduce_rule_match_positive_tuple(exec.sig, step_idx, tuple_vals, candidate_bound, candidate_values)) {
            deduce_observed_step_note_rows_emitted(exec.observed_rule_counters, step_idx);
            Value* err = deduce_naive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}

fn Value* deduce_naive_evaluate_rule_with_groups(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceAggregateGroups* shared_aggregate_groups,
    bool finalize_aggregates,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    bool dry_run,
    Interp* interp
) {
    if (db == null || sig == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
    }
    if (sig.body_count == 0 || sig.body_predicates == null) {
        return null;
    }

    MdbDbi head_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, sig.head_predicate, &head_dbi, &head_schema, interp)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
    }

    Value*[] var_values = {};
    bool[] var_bound = {};
    usz var_capacity = (usz)sig.max_var_id + 1;
    if (var_capacity == 0) var_capacity = 1;

    var_values = mem::new_array(Value*, var_capacity);
    if (var_values.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_values.ptr);
    for (usz i = 0; i < var_values.len; i++) var_values[i] = null;

    var_bound = mem::new_array(bool, var_capacity);
    if (var_bound.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_bound.ptr);
    for (usz i = 0; i < var_bound.len; i++) var_bound[i] = false;

    Value*[] head_tuple = {};
    if (sig.head_term_count > 0) {
        head_tuple = mem::new_array(Value*, sig.head_term_count);
        if (head_tuple.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (head_tuple.ptr != null) mem::free(head_tuple.ptr);

    DeduceAggregateGroups aggregate_groups = {};
    if (shared_aggregate_groups == null) {
        defer deduce_aggregate_groups_release(&aggregate_groups);
    }

    DeduceAggregateGroups* active_aggregate_groups = null;
    if (deduce_rule_signature_has_aggregates(sig)) {
        active_aggregate_groups = shared_aggregate_groups != null ? shared_aggregate_groups : &aggregate_groups;
    }

    DeduceNaiveRuleExec exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .aggregate_groups = active_aggregate_groups,
        .observed_rule_counters = observed_rule_counters,
        .dry_run = dry_run,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };
    Value* err = deduce_naive_eval_rule_step(&exec, 0, var_bound, var_values);
    if (err != null) return err;
    if (deduce_rule_signature_has_aggregates(sig) && finalize_aggregates) {
        return deduce_naive_finalize_aggregate_groups(&exec);
    }
    return null;
}

fn Value* deduce_naive_finalize_aggregate_groups_for_signature(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceAggregateGroups* aggregate_groups,
    long* derived_out,
    bool* changed_out,
    Interp* interp
) {
    if (db == null || sig == null || txn == null || aggregate_groups == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate finalize state");
    }

    MdbDbi head_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, sig.head_predicate, &head_dbi, &head_schema, interp)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
    }

    Value*[] head_tuple = {};
    if (sig.head_term_count > 0) {
        head_tuple = mem::new_array(Value*, sig.head_term_count);
        if (head_tuple.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (head_tuple.ptr != null) mem::free(head_tuple.ptr);

    DeduceNaiveRuleExec exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .aggregate_groups = aggregate_groups,
        .observed_rule_counters = null,
        .dry_run = false,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };
    return deduce_naive_finalize_aggregate_groups(&exec);
}

fn Value* deduce_naive_evaluate_rule(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    bool dry_run,
    Interp* interp
) {
    return deduce_naive_evaluate_rule_with_groups(
        db,
        sig,
        txn,
        null,
        true,
        derived_out,
        changed_out,
        observed_rule_counters,
        dry_run,
        interp
    );
}
