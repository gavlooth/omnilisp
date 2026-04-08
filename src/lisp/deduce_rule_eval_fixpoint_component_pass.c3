module lisp;

import std::core::mem;

fn Value* deduce_naive_evaluate_component_pass_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    bool* changed_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool changed = false;
    long derived = 0;
    bool[] processed = mem::new_array(bool, db.rule_signature_count);
    if (processed.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (processed.ptr != null) mem::free(processed.ptr);
    for (usz i = 0; i < processed.len; i++) processed[i] = false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        Value* err = null;
        if (!deduce_rule_signature_has_aggregates(sig)) {
            err = deduce_naive_evaluate_rule(
                db,
                sig,
                txn,
                head_demand,
                &derived,
                &changed,
                observed_rule,
                false,
                interp
            );
            processed[i] = true;
        } else {
            DeduceAggregateGroups aggregate_groups = {};
            defer deduce_aggregate_groups_release(&aggregate_groups);

            err = deduce_naive_evaluate_rule_with_groups(
                db,
                sig,
                txn,
                head_demand,
                &aggregate_groups,
                false,
                &derived,
                &changed,
                observed_rule,
                false,
                interp
            );
            processed[i] = true;

            for (usz j = i + 1; err == null && j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id || processed[j]) continue;
                DeduceRuleSignature* other = &db.rule_signatures[j];
                if (!deduce_aggregate_signatures_share_head_batch(sig, other)) continue;
                DeduceObservedRuleCounters* other_observed =
                    observed_rules.ptr != null && j < observed_rules.len ? &observed_rules[j] : null;
                err = deduce_naive_evaluate_rule_with_groups(
                    db,
                    other,
                    txn,
                    head_demand,
                    &aggregate_groups,
                    false,
                    &derived,
                    &changed,
                    other_observed,
                    false,
                    interp
                );
                processed[j] = true;
            }

            if (err == null) {
                err = deduce_naive_finalize_aggregate_groups_for_signature(
                    db,
                    sig,
                    txn,
                    &aggregate_groups,
                    &derived,
                    &changed,
                    interp
                );
            }
        }
        if (err != null) return err;
    }

    if (changed_out != null) *changed_out = changed;
    if (derived_out != null) *derived_out = derived;
    return null;
}

fn Value* deduce_naive_evaluate_component_pass(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    bool* changed_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    bool changed = false;
    long derived = 0;
    Value* err = deduce_naive_evaluate_component_pass_in_txn(
        db,
        plan,
        component_id,
        txn,
        null,
        &changed,
        &derived,
        observed_rules,
        interp
    );
    if (err == null) {
        err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
    }
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }

    if (changed_out != null) *changed_out = changed;
    if (derived_out != null) *derived_out = derived;
    return null;
}
