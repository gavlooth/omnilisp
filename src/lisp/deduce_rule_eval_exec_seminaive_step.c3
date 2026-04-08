module lisp;

import std::core::mem;

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

    Value*[] prefix_probe_values = {};
    usz prefix_probe_len = 0;
    char[4096] prefix_probe_key_buf;
    usz prefix_probe_key_len = 0;
    bool use_prefix_probe =
        exec.head_demand != null &&
        !negated;
    if (use_prefix_probe && body_schema.arity > 0) {
        prefix_probe_values = mem::new_array(Value*, body_schema.arity);
        if (prefix_probe_values.ptr == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (!deduce_rule_build_step_prefix_probe(exec.sig, step_idx, var_bound, var_values, prefix_probe_values, &prefix_probe_len)) {
            use_prefix_probe = false;
        } else {
            usz? encoded_prefix_len = encode_tuple(prefix_probe_values[0:prefix_probe_len], prefix_probe_key_buf[..], prefix_probe_key_buf.len);
            if (catch err = encoded_prefix_len) {
                use_prefix_probe = false;
                prefix_probe_len = 0;
            } else {
                prefix_probe_key_len = encoded_prefix_len;
            }
        }
    }
    defer if (prefix_probe_values.ptr != null) mem::free(prefix_probe_values.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(exec.txn, body_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(exec.interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    if (use_prefix_probe) {
        key.mv_data = &prefix_probe_key_buf[0];
        key.mv_size = prefix_probe_key_len;
        rc = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);
    } else {
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    }

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
            if (use_prefix_probe && !deduce_tuple_matches_prefix(tuple_vals, prefix_probe_values, prefix_probe_len)) break;
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

    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    bool use_delta_scan =
        exec.anchor_active &&
        step_idx == exec.anchor_step &&
        pred_idx >= 0 &&
        (usz)pred_idx < exec.current_delta_sets.len &&
        (usz)pred_idx < exec.plan.predicate_component.len &&
        (usz)exec.plan.predicate_component[(usz)pred_idx] == exec.component_id;
    bool use_scratch_visible_scan =
        exec.dry_run &&
        pred_idx >= 0 &&
        (usz)pred_idx < exec.scratch_visible_sets.len &&
        (usz)pred_idx < exec.plan.predicate_component.len &&
        (usz)exec.plan.predicate_component[(usz)pred_idx] == exec.component_id &&
        (!exec.anchor_active || step_idx != exec.anchor_step);
    if (use_delta_scan) {
        deduce_worker_scratch_trace(
            "replay_anchor_delta_scan",
            exec.component_id,
            (long)step_idx,
            pred_idx >= 0 && (usz)pred_idx < exec.current_delta_sets.len
                ? (long)exec.current_delta_sets[(usz)pred_idx].additions.count
                : 0
        );
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
        if (use_prefix_probe && !deduce_tuple_matches_prefix(tuple_vals, prefix_probe_values, prefix_probe_len)) break;

        for (usz i = 0; i < var_bound.len; i++) candidate_bound[i] = var_bound[i];
        for (usz i = 0; i < var_values.len; i++) candidate_values[i] = var_values[i];

        if (deduce_rule_match_positive_tuple(exec.sig, step_idx, tuple_vals, candidate_bound, candidate_values)) {
            deduce_observed_step_note_rows_emitted(exec.observed_rule_counters, step_idx);
            Value* err = deduce_seminaive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }

    if (use_scratch_visible_scan) {
        deduce_worker_scratch_trace(
            "replay_scratch_visible_scan",
            exec.component_id,
            (long)step_idx,
            pred_idx >= 0 && (usz)pred_idx < exec.scratch_visible_sets.len
                ? (long)exec.scratch_visible_sets[(usz)pred_idx].count
                : 0
        );
        Value* err = deduce_seminaive_replay_scratch_visible_set(
            exec,
            step_idx,
            tuple_vals,
            var_bound,
            var_values,
            candidate_bound,
            candidate_values,
            &exec.scratch_visible_sets[(usz)pred_idx],
            use_prefix_probe,
            prefix_probe_values,
            prefix_probe_len
        );
        if (err != null) return err;
    }
    return null;
}
