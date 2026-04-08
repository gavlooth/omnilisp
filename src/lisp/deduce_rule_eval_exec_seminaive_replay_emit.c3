module lisp;

import std::core::mem;

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

fn Value* deduce_seminaive_replay_scratch_visible_set(
    DeduceSemiNaiveRuleExec* exec,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values,
    bool[] candidate_bound,
    Value*[] candidate_values,
    DeduceDeltaTupleSet* visible_set,
    bool use_prefix_probe,
    Value*[] prefix_probe_values,
    usz prefix_probe_len
) {
    if (exec == null || visible_set == null) return null;
    for (usz tuple_idx = 0; tuple_idx < visible_set.count; tuple_idx++) {
        deduce_observed_step_note_rows_read(exec.observed_rule_counters, step_idx);
        DeduceEncodedTuple* visible_tuple = &visible_set.tuples[tuple_idx];
        usz? decoded_count = decode_tuple(visible_tuple.key_bytes, visible_tuple.key_len, tuple_vals, exec.interp);
        if (catch err = decoded_count) continue;
        if (decoded_count != tuple_vals.len) continue;
        if (use_prefix_probe && !deduce_tuple_matches_prefix(tuple_vals, prefix_probe_values, prefix_probe_len)) continue;

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
        if (exec.aggregate_component_mode) {
            return deduce_raise(exec.interp, "deduce/parallel-worker-scratch-aggregate-unsupported",
                "deduce/parallel: worker scratch batches do not yet support aggregate recursive components");
        }

        MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
        MdbVal existing = {};
        if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
            deduce_worker_scratch_trace("emit_head_dry_run_skip_existing", exec.component_id, (long)key_len, (long)exec.sig.head_predicate);
            return null;
        }
        if (deduce_seminaive_scratch_visible_contains(exec, exec.sig.head_predicate, &key_buf[0], key_len)) {
            deduce_worker_scratch_trace("emit_head_dry_run_skip_visible", exec.component_id, (long)key_len, (long)exec.sig.head_predicate);
            return null;
        }

        if (exec.changed_out != null) *exec.changed_out = true;
        if (exec.derived_out != null && *exec.derived_out < long.max) *exec.derived_out = *exec.derived_out + 1;
        deduce_worker_scratch_trace("emit_head_dry_run_record_new", exec.component_id, (long)key_len, (long)exec.sig.head_predicate);
        return deduce_seminaive_record_new_delta_tuple(exec, exec.sig.head_predicate, &key_buf[0], key_len);
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

    Value* integrity_err = deduce_assert_schema_derived_write_integrity(
        exec.db,
        exec.head_schema,
        exec.txn,
        exec.head_dbi,
        exec.head_tuple,
        &key,
        exec.interp
    );
    if (integrity_err != null) return integrity_err;

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
