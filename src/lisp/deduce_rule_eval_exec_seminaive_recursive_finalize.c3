module lisp;

import std::core::mem;

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
        .head_demand = null,
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
