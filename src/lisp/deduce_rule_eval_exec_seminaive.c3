module lisp;

import std::core::mem;
import std::collections::list;

struct DeduceSemiNaiveAggregateBatch {
    DeduceRuleSignature* sig;
    DeduceAggregateGroups groups;
}

fn Value* deduce_seminaive_build_proof_key(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values,
    Value** key_out
) {
    if (exec == null || exec.interp == null || key_out == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp,
                "deduce/analyze-invalid-state",
                "deduce/analyze: invalid proof key state");
        }
        return null;
    }

    usz proof_value_count = 1 + var_bound.len * 2;
    Value*[] proof_values = mem::new_array(Value*, proof_value_count);
    if (proof_values.ptr == null && proof_value_count > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (proof_values.ptr != null) mem::free(proof_values.ptr);

    proof_values[0] = make_symbol(
        exec.interp,
        exec.anchor_delta_is_removal
            ? exec.interp.symbols.intern("proof-remove")
            : exec.interp.symbols.intern("proof-add")
    );
    usz proof_idx = 1;
    for (usz i = 0; i < var_bound.len; i++) {
        proof_values[proof_idx++] = make_int(exec.interp, var_bound[i] ? 1 : 0);
        proof_values[proof_idx++] =
            i < var_values.len && var_bound[i] ? var_values[i] : make_nil(exec.interp);
    }

    return deduce_encode_value_vector_to_owned_string(proof_values, key_out, exec.interp);
}

fn Value* deduce_seminaive_mark_current_proof_seen(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values,
    bool* already_seen_out
) {
    if (already_seen_out == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp,
                "deduce/analyze-invalid-state",
                "deduce/analyze: invalid proof dedupe target");
        }
        return null;
    }
    *already_seen_out = false;
    if (exec == null || exec.interp == null || !exec.aggregate_component_mode) return null;

    if (exec.proof_seen_map == null) {
        exec.proof_seen_map = make_hashmap(exec.interp, 64);
        if (exec.proof_seen_map == null || exec.proof_seen_map.tag != HASHMAP || exec.proof_seen_map.hashmap_val == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }

    Value* proof_key = null;
    Value* err = deduce_seminaive_build_proof_key(exec, var_bound, var_values, &proof_key);
    if (err != null) return err;

    if (hashmap_get(exec.proof_seen_map.hashmap_val, proof_key) != null) {
        *already_seen_out = true;
        return null;
    }

    hashmap_set(
        exec.proof_seen_map.hashmap_val,
        proof_key,
        make_symbol(exec.interp, exec.interp.sym_true),
        exec.interp
    );
    return null;
}

fn Value* deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len,
    bool delta_is_removal
) {
    if (exec == null || exec.plan == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid supported head state");
        }
        return null;
    }
    if (exec.support_tables.len == 0 || !exec.aggregate_component_mode) return null;

    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.support_tables.len ||
        (usz)pred_idx >= exec.plan.predicate_component.len ||
        (usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid supported head predicate");
    }

    bool became_present = false;
    bool became_absent = false;
    long delta = delta_is_removal ? -1 : 1;
    Value* err = deduce_tuple_support_table_adjust_recursive_count(
        &exec.support_tables[(usz)pred_idx],
        key_data,
        key_len,
        delta,
        null,
        &became_present,
        &became_absent,
        exec.interp
    );
    if (err != null) return err;
    if (!became_present && !became_absent) return null;

    char empty_key = 0;
    MdbVal key = { .mv_size = key_len, .mv_data = key_len > 0 ? key_data : &empty_key };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    if (became_present) {
        MdbVal existing = {};
        if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) != MDB_SUCCESS) {
            int put_rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
            if (put_rc != MDB_SUCCESS) {
                return deduce_raise(exec.interp, "deduce/analyze-put-failed",
                    "deduce/analyze: failed to insert supported derived tuple");
            }
        }
        if (exec.changed_out != null) *exec.changed_out = true;
        if (exec.derived_out != null && *exec.derived_out < long.max) *exec.derived_out = *exec.derived_out + 1;
        if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
            exec.head_schema.cardinality_estimate++;
            if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
                exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
            }
        }
        return deduce_seminaive_record_new_delta_tuple(exec, predicate, key_data, key_len);
    }

    int del_rc = mdb_del(exec.txn, exec.head_dbi, &key, null);
    if (del_rc != MDB_SUCCESS && del_rc != MDB_NOTFOUND) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed",
            "deduce/analyze: failed to retract supported derived tuple");
    }
    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null && *exec.derived_out < long.max) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate > 0) {
        exec.head_schema.cardinality_estimate--;
        exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
    }
    return deduce_seminaive_record_removed_delta_tuple(exec, predicate, key_data, key_len);
}

fn Value* deduce_seminaive_apply_supported_encoded_head_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    return deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
        exec,
        predicate,
        key_data,
        key_len,
        exec != null && exec.anchor_delta_is_removal
    );
}

fn Value* deduce_seminaive_replay_anchor_delta_set(
    DeduceSemiNaiveRuleExec* exec,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values,
    bool[] candidate_bound,
    Value*[] candidate_values,
    DeduceDeltaTupleSet* delta_set,
    bool delta_is_removal
) {
    if (exec == null || delta_set == null) return null;
    bool saved_delta_is_removal = exec.anchor_delta_is_removal;
    exec.anchor_delta_is_removal = delta_is_removal;
    defer exec.anchor_delta_is_removal = saved_delta_is_removal;

    for (usz delta_idx = 0; delta_idx < delta_set.count; delta_idx++) {
        deduce_observed_step_note_rows_read(exec.observed_rule_counters, step_idx);
        DeduceEncodedTuple* delta_tuple = &delta_set.tuples[delta_idx];
        usz? decoded_count = decode_tuple(delta_tuple.key_bytes, delta_tuple.key_len, tuple_vals, exec.interp);
        if (catch err = decoded_count) continue;
        if (decoded_count != tuple_vals.len) continue;

        for (usz i = 0; i < var_bound.len; i++) candidate_bound[i] = var_bound[i];
        for (usz i = 0; i < var_values.len; i++) candidate_values[i] = var_values[i];

        if (deduce_rule_match_positive_tuple(exec.sig, step_idx, tuple_vals, candidate_bound, candidate_values)) {
            deduce_observed_step_note_rows_emitted(exec.observed_rule_counters, step_idx);
            Value* err = deduce_seminaive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
    }
    return null;
}

fn Value* deduce_seminaive_emit_head_fact(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (exec.sig.head_term_count != exec.head_tuple.len) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid head term metadata");
    }
    if (exec.sig.head_term_is_variable == null || exec.sig.head_term_var_ids == null || exec.sig.head_term_literals == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing head term metadata");
    }

    bool already_seen = false;
    Value* proof_err = deduce_seminaive_mark_current_proof_seen(exec, var_bound, var_values, &already_seen);
    if (proof_err != null) return proof_err;
    if (already_seen) return null;

    if (deduce_rule_signature_has_aggregates(exec.sig)) {
        if (exec.aggregate_groups == null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        DeduceAggregateGroup* group = null;
        Value* err = deduce_aggregate_group_find_or_add(
            exec.aggregate_groups,
            exec.sig,
            var_bound,
            var_values,
            &group,
            exec.interp
        );
        if (err != null) return err;
        return deduce_aggregate_group_note_binding_with_polarity(
            group,
            exec.sig,
            var_bound,
            var_values,
            exec.anchor_delta_is_removal,
            exec.interp
        );
    }

    for (usz i = 0; i < exec.sig.head_term_count; i++) {
        if (exec.sig.head_term_is_variable[i]) {
            uint var_id = exec.sig.head_term_var_ids[i];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
                return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound head variable at runtime");
            }
            exec.head_tuple[i] = var_values[var_id];
        } else {
            exec.head_tuple[i] = exec.sig.head_term_literals[i];
        }
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }
    if (exec.dry_run) {
        return null;
    }

    if (exec.aggregate_component_mode && exec.support_tables.len > 0) {
        return deduce_seminaive_apply_supported_encoded_head_tuple(
            exec,
            exec.sig.head_predicate,
            &key_buf[0],
            key_len
        );
    }

    MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }

    return deduce_seminaive_record_new_delta_tuple(exec, exec.sig.head_predicate, &key_buf[0], key_len);
}

fn Value* deduce_seminaive_eval_rule_step(
    DeduceSemiNaiveRuleExec* exec,
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
        return deduce_seminaive_emit_head_fact(exec, var_bound, var_values);
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
        return deduce_seminaive_eval_rule_step(exec, step_idx + 1, var_bound, var_values);
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

    int pred_idx = exec.anchor_active && step_idx == exec.anchor_step
        ? deduce_scc_predicate_index(exec.plan, predicate)
        : -1;
    bool use_delta_scan =
        pred_idx >= 0 &&
        (usz)pred_idx < exec.current_delta_sets.len &&
        (usz)pred_idx < exec.plan.predicate_component.len &&
        (usz)exec.plan.predicate_component[(usz)pred_idx] == exec.component_id;
    if (use_delta_scan) {
        DeduceSignedDeltaSet* delta_set = &exec.current_delta_sets[(usz)pred_idx];
        Value* err = deduce_seminaive_replay_anchor_delta_set(
            exec,
            step_idx,
            tuple_vals,
            var_bound,
            var_values,
            candidate_bound,
            candidate_values,
            &delta_set.additions,
            false
        );
        if (err != null) return err;
        if (exec.aggregate_component_mode && exec.support_tables.len > 0) {
            err = deduce_seminaive_replay_anchor_delta_set(
                exec,
                step_idx,
                tuple_vals,
                var_bound,
                var_values,
                candidate_bound,
                candidate_values,
                &delta_set.removals,
                true
            );
            if (err != null) return err;
        }
        return null;
    }

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
            Value* err = deduce_seminaive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}

fn usz deduce_seminaive_rule_anchor_count(
    DeduceRuleSignature* sig,
    DeduceSccPlan* plan,
    usz component_id
) {
    usz[] anchor_steps = {};
    return deduce_seminaive_collect_anchor_steps(sig, plan, component_id, anchor_steps);
}

fn Value* deduce_seminaive_evaluate_rule_with_groups(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    DeduceTupleSupportTable[] support_tables,
    DeduceAggregateGroups* aggregate_groups,
    bool aggregate_component_mode,
    bool allow_unanchored,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (db == null || plan == null || sig == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
    }
    if (deduce_rule_signature_has_aggregates(sig) && aggregate_groups == null) {
        return deduce_raise(interp, "deduce/unsupported-aggregate",
            "deduce/analyze: recursive aggregate rule execution is not implemented yet");
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

    usz[] anchor_steps = {};
    if (sig.body_count > 0) {
        anchor_steps = mem::new_array(usz, sig.body_count);
        if (anchor_steps.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (anchor_steps.ptr != null) mem::free(anchor_steps.ptr);

    DeduceSemiNaiveRuleExec exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .aggregate_groups = aggregate_groups,
        .plan = plan,
        .component_id = component_id,
        .current_delta_sets = current_delta_sets,
        .next_delta_sets = next_delta_sets,
        .support_tables = support_tables,
        .proof_seen_map = null,
        .anchor_active = false,
        .anchor_step = 0,
        .anchor_delta_is_removal = false,
        .aggregate_component_mode = aggregate_component_mode,
        .observed_rule_counters = observed_rule_counters,
        .dry_run = false,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };

    usz anchor_count = deduce_seminaive_collect_anchor_steps(sig, plan, component_id, anchor_steps);
    if (anchor_count == 0) {
        if (!allow_unanchored) return null;
        return deduce_seminaive_eval_rule_step(&exec, 0, var_bound, var_values);
    }

    for (usz i = 0; i < anchor_count; i++) {
        if (i >= anchor_steps.len) break;
        exec.anchor_active = true;
        exec.anchor_step = anchor_steps[i];
        Value* err = deduce_seminaive_eval_rule_step(&exec, 0, var_bound, var_values);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_seminaive_evaluate_rule(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    return deduce_seminaive_evaluate_rule_with_groups(
        db,
        plan,
        component_id,
        sig,
        txn,
        current_delta_sets,
        next_delta_sets,
        {},
        null,
        false,
        true,
        derived_out,
        changed_out,
        observed_rule_counters,
        interp
    );
}

fn Value* deduce_seminaive_finalize_aggregate_groups_for_signature(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceAggregateGroups* aggregate_groups,
    DeduceSignedDeltaSet[] next_delta_sets,
    DeduceTupleSupportTable[] support_tables,
    long* derived_out,
    bool* changed_out,
    Interp* interp
) {
    if (db == null || plan == null || sig == null || txn == null || aggregate_groups == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate finalize state");
    }

    MdbDbi head_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, sig.head_predicate, &head_dbi, &head_schema, interp)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
    }

    int pred_idx = deduce_scc_predicate_index(plan, sig.head_predicate);
    if (pred_idx < 0 || (usz)pred_idx >= next_delta_sets.len ||
        (usz)pred_idx >= support_tables.len ||
        (usz)pred_idx >= plan.predicate_component.len ||
        (usz)plan.predicate_component[(usz)pred_idx] != component_id) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate delta target is not in recursive component");
    }

    Value*[] head_tuple = {};
    if (sig.head_term_count > 0) {
        head_tuple = mem::new_array(Value*, sig.head_term_count);
        if (head_tuple.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (head_tuple.ptr != null) mem::free(head_tuple.ptr);

    DeduceSemiNaiveRuleExec support_exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .aggregate_groups = aggregate_groups,
        .plan = plan,
        .component_id = component_id,
        .current_delta_sets = {},
        .next_delta_sets = next_delta_sets,
        .support_tables = support_tables,
        .anchor_active = false,
        .anchor_step = 0,
        .anchor_delta_is_removal = false,
        .aggregate_component_mode = true,
        .observed_rule_counters = null,
        .dry_run = false,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };

    for (usz i = 0; i < aggregate_groups.count; i++) {
        DeduceAggregateGroup* group = &aggregate_groups.entries[i];
        if (!group.dirty) continue;

        DeduceEncodedTuple old_actual = {};
        DeduceEncodedTuple new_actual = {};
        defer {
            if (old_actual.key_bytes != null) mem::free(old_actual.key_bytes);
            if (new_actual.key_bytes != null) mem::free(new_actual.key_bytes);
        }

        bool had_actual = group.materialized && group.previous_support_count > 0;
        bool has_actual = group.support_count > 0;

        if (had_actual) {
            Value* err = deduce_encode_aggregate_group_tuple(
                sig,
                group,
                group.previous_term_states,
                head_tuple,
                &old_actual,
                interp
            );
            if (err != null) return err;
        }

        Value* err = null;
        if (has_actual) {
            err = deduce_encode_aggregate_group_tuple(
                sig,
                group,
                group.term_states,
                head_tuple,
                &new_actual,
                interp
            );
            if (err != null) return err;
        }

        bool actual_changed = had_actual != has_actual ||
            (had_actual && has_actual &&
                !deduce_bytes_equal(old_actual.key_bytes, old_actual.key_len, new_actual.key_bytes, new_actual.key_len));
        if (!actual_changed) {
            deduce_aggregate_group_commit_materialized_state(group, sig);
            continue;
        }

        if (had_actual) {
            err = deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
                &support_exec,
                sig.head_predicate,
                old_actual.key_bytes,
                old_actual.key_len,
                true
            );
            if (err != null) return err;
        }

        if (has_actual) {
            err = deduce_seminaive_apply_supported_encoded_head_tuple_with_polarity(
                &support_exec,
                sig.head_predicate,
                new_actual.key_bytes,
                new_actual.key_len,
                false
            );
            if (err != null) return err;
        }

        deduce_aggregate_group_commit_materialized_state(group, sig);
    }
    return null;
}

fn Value* deduce_seminaive_clear_component_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    Interp* interp
) {
    if (db == null || plan == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate clear state");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi head_dbi = 0;
        DeduceRelationSchema* head_schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &head_dbi, &head_schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
        }
        int rc = mdb_drop(txn, head_dbi, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/analyze-put-failed",
                "deduce/analyze: failed to reset recursive aggregate head relation");
        }
        if (head_schema != null) {
            head_schema.cardinality_estimate = 0;
            head_schema.distinct_estimate = 0;
        }
    }
    return null;
}

fn Value* deduce_seminaive_evaluate_recursive_aggregate_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid recursive aggregate state");
    }

    DeduceSignedDeltaSet[] current_delta_sets = mem::new_array(DeduceSignedDeltaSet, plan.predicates.len);
    if (current_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (current_delta_sets.ptr != null) {
            for (usz i = 0; i < current_delta_sets.len; i++) deduce_signed_delta_set_release(&current_delta_sets[i]);
            mem::free(current_delta_sets.ptr);
        }
    }

    DeduceSignedDeltaSet[] next_delta_sets = mem::new_array(DeduceSignedDeltaSet, plan.predicates.len);
    if (next_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (next_delta_sets.ptr != null) {
            for (usz i = 0; i < next_delta_sets.len; i++) deduce_signed_delta_set_release(&next_delta_sets[i]);
            mem::free(next_delta_sets.ptr);
        }
    }

    DeduceTupleSupportTable[] support_tables = mem::new_array(DeduceTupleSupportTable, plan.predicates.len);
    if (support_tables.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (support_tables.ptr != null) {
            for (usz i = 0; i < support_tables.len; i++) deduce_tuple_support_table_release(&support_tables[i]);
            mem::free(support_tables.ptr);
        }
    }

    isz[] aggregate_rule_batch_ids = mem::new_array(isz, db.rule_signature_count);
    if (aggregate_rule_batch_ids.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (aggregate_rule_batch_ids.ptr != null) mem::free(aggregate_rule_batch_ids.ptr);
    for (usz i = 0; i < aggregate_rule_batch_ids.len; i++) aggregate_rule_batch_ids[i] = -1;

    DeduceSemiNaiveAggregateBatch* batches = null;
    usz batch_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (!deduce_rule_signature_has_aggregates(sig)) continue;

        isz batch_id = -1;
        for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
            if (deduce_aggregate_signatures_share_head_batch(batches[batch_idx].sig, sig)) {
                batch_id = (isz)batch_idx;
                break;
            }
        }
        if (batch_id >= 0) {
            aggregate_rule_batch_ids[i] = batch_id;
            continue;
        }

        DeduceSemiNaiveAggregateBatch* next_batches =
            (DeduceSemiNaiveAggregateBatch*)mem::realloc(
                batches,
                DeduceSemiNaiveAggregateBatch.sizeof * (batch_count + 1)
            );
        if (next_batches == null) {
            for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
                deduce_aggregate_groups_release(&batches[batch_idx].groups);
            }
            if (batches != null) mem::free(batches);
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        batches = next_batches;
        batches[batch_count] = {};
        batches[batch_count].sig = sig;
        aggregate_rule_batch_ids[i] = (isz)batch_count;
        batch_count++;
    }
    defer {
        if (batches != null) {
            for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
                deduce_aggregate_groups_release(&batches[batch_idx].groups);
            }
            mem::free(batches);
        }
    }

    bool[] processed = mem::new_array(bool, db.rule_signature_count);
    if (processed.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (processed.ptr != null) mem::free(processed.ptr);

    MdbTxn* seed_txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &seed_txn);
    if (rc != MDB_SUCCESS || seed_txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    Value* err = deduce_seminaive_clear_component_predicates(db, plan, component_id, seed_txn, interp);
    if (err == null) {
        err = deduce_seminaive_seed_component_extensional_state_in_txn(
            db,
            plan,
            component_id,
            seed_txn,
            next_delta_sets,
            support_tables,
            true,
            interp
        );
    }
    if (err == null) {
        for (usz i = 0; i < processed.len; i++) processed[i] = false;
        for (usz i = 0; i < db.rule_signature_count && err == null; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            DeduceObservedRuleCounters* observed_rule =
                observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
            if (!deduce_rule_signature_has_aggregates(sig)) {
                if (deduce_seminaive_rule_anchor_count(sig, plan, component_id) != 0) continue;
                err = deduce_seminaive_evaluate_rule_with_groups(
                    db,
                    plan,
                    component_id,
                    sig,
                    seed_txn,
                    current_delta_sets,
                    next_delta_sets,
                    support_tables,
                    null,
                    true,
                    true,
                    derived_out,
                    null,
                    observed_rule,
                    interp
                );
                processed[i] = true;
                continue;
            }
            if (deduce_seminaive_rule_anchor_count(sig, plan, component_id) != 0) continue;

            DeduceAggregateGroups* groups = null;
            if (i < aggregate_rule_batch_ids.len && aggregate_rule_batch_ids[i] >= 0) {
                usz batch_idx = (usz)aggregate_rule_batch_ids[i];
                if (batch_idx < batch_count) groups = &batches[batch_idx].groups;
            }
            err = deduce_seminaive_evaluate_rule_with_groups(
                db,
                plan,
                component_id,
                sig,
                seed_txn,
                current_delta_sets,
                next_delta_sets,
                support_tables,
                groups,
                true,
                true,
                derived_out,
                null,
                observed_rule,
                interp
            );
            processed[i] = true;
            for (usz j = i + 1; err == null && j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id || processed[j]) continue;
                DeduceRuleSignature* other = &db.rule_signatures[j];
                if (!deduce_rule_signature_has_aggregates(other)) continue;
                if (j >= aggregate_rule_batch_ids.len || aggregate_rule_batch_ids[j] != aggregate_rule_batch_ids[i]) continue;
                if (deduce_seminaive_rule_anchor_count(other, plan, component_id) != 0) continue;
                DeduceObservedRuleCounters* other_observed =
                    observed_rules.ptr != null && j < observed_rules.len ? &observed_rules[j] : null;
                err = deduce_seminaive_evaluate_rule_with_groups(
                    db,
                    plan,
                    component_id,
                    other,
                    seed_txn,
                    current_delta_sets,
                    next_delta_sets,
                    support_tables,
                    groups,
                    true,
                    true,
                    derived_out,
                    null,
                    other_observed,
                    interp
                );
                processed[j] = true;
            }
        }
    }
    for (usz batch_idx = 0; batch_idx < batch_count && err == null; batch_idx++) {
        err = deduce_seminaive_finalize_aggregate_groups_for_signature(
            db,
            plan,
            component_id,
            batches[batch_idx].sig,
            seed_txn,
            &batches[batch_idx].groups,
            next_delta_sets,
            support_tables,
            derived_out,
            null,
            interp
        );
    }
    if (err != null) {
        mdb_txn_abort(seed_txn);
        return err;
    }
    rc = mdb_txn_commit(seed_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }
    if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;

    for (usz i = 0; i < plan.predicates.len; i++) {
        current_delta_sets[i] = next_delta_sets[i];
        next_delta_sets[i] = {};
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 1; iter < limit; iter++) {
        if (!deduce_seminaive_component_has_delta(plan, component_id, current_delta_sets)) return null;

        MdbTxn* txn = null;
        rc = mdb_txn_begin(db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
        }

        for (usz i = 0; i < processed.len; i++) processed[i] = false;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            DeduceObservedRuleCounters* observed_rule =
                observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
            if (!deduce_rule_signature_has_aggregates(sig)) {
                if (deduce_seminaive_rule_anchor_count(sig, plan, component_id) == 0) continue;
                err = deduce_seminaive_evaluate_rule_with_groups(
                    db,
                    plan,
                    component_id,
                    sig,
                    txn,
                    current_delta_sets,
                    next_delta_sets,
                    support_tables,
                    null,
                    true,
                    false,
                    derived_out,
                    null,
                    observed_rule,
                    interp
                );
                processed[i] = true;
                if (err != null) break;
                continue;
            }
            if (deduce_seminaive_rule_anchor_count(sig, plan, component_id) == 0) continue;

            DeduceAggregateGroups* groups = null;
            if (i < aggregate_rule_batch_ids.len && aggregate_rule_batch_ids[i] >= 0) {
                usz batch_idx = (usz)aggregate_rule_batch_ids[i];
                if (batch_idx < batch_count) groups = &batches[batch_idx].groups;
            }
            err = deduce_seminaive_evaluate_rule_with_groups(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                support_tables,
                groups,
                true,
                false,
                derived_out,
                null,
                observed_rule,
                interp
            );
            processed[i] = true;
            for (usz j = i + 1; err == null && j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id || processed[j]) continue;
                DeduceRuleSignature* other = &db.rule_signatures[j];
                if (!deduce_rule_signature_has_aggregates(other)) continue;
                if (j >= aggregate_rule_batch_ids.len || aggregate_rule_batch_ids[j] != aggregate_rule_batch_ids[i]) continue;
                if (deduce_seminaive_rule_anchor_count(other, plan, component_id) == 0) continue;
                DeduceObservedRuleCounters* other_observed =
                    observed_rules.ptr != null && j < observed_rules.len ? &observed_rules[j] : null;
                err = deduce_seminaive_evaluate_rule_with_groups(
                    db,
                    plan,
                    component_id,
                    other,
                    txn,
                    current_delta_sets,
                    next_delta_sets,
                    support_tables,
                    groups,
                    true,
                    false,
                    derived_out,
                    null,
                    other_observed,
                    interp
                );
                processed[j] = true;
            }
            if (err != null) break;
        }

        for (usz batch_idx = 0; batch_idx < batch_count && err == null; batch_idx++) {
            err = deduce_seminaive_finalize_aggregate_groups_for_signature(
                db,
                plan,
                component_id,
                batches[batch_idx].sig,
                txn,
                &batches[batch_idx].groups,
                next_delta_sets,
                support_tables,
                derived_out,
                null,
                interp
            );
        }

        if (err != null) {
            mdb_txn_abort(txn);
            return err;
        }
        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
        }
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
        if (!has_delta) return null;
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit",
        "deduce/analyze: recursive stratum iteration limit reached", data);
}
