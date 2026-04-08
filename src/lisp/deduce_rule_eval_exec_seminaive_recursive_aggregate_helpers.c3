module lisp;

struct DeduceSemiNaiveAggregateBatch {
    DeduceRuleSignature* sig;
    DeduceAggregateGroups groups;
}

fn Value* deduce_seminaive_execute_component_rule_pass(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    DeduceTupleSupportTable[] support_tables,
    DeduceSemiNaiveAggregateBatch* batches,
    usz batch_count,
    isz[] aggregate_rule_batch_ids,
    bool[] processed,
    bool seedless_only,
    bool use_seeded_rules,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || txn == null || processed.ptr == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid recursive aggregate state");
    }

    Value* err = null;
    for (usz i = 0; i < db.rule_signature_count && err == null; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        bool has_aggregates = deduce_rule_signature_has_aggregates(sig);
        long anchor_count = deduce_seminaive_rule_anchor_count(sig, plan, component_id);
        if (!has_aggregates) {
            if (seedless_only ? anchor_count != 0 : anchor_count == 0) continue;
            err = deduce_seminaive_evaluate_rule_with_groups(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                {},
                support_tables,
                null,
                null,
                true,
                use_seeded_rules,
                false,
                derived_out,
                null,
                observed_rule,
                interp
            );
            processed[i] = true;
            continue;
        }
        if (seedless_only ? anchor_count != 0 : anchor_count == 0) continue;

        if (i >= aggregate_rule_batch_ids.len || aggregate_rule_batch_ids[i] < 0) continue;
        usz batch_idx = (usz)aggregate_rule_batch_ids[i];
        if (batch_idx >= batch_count) continue;
        err = deduce_seminaive_execute_component_aggregate_batch(
            db,
            plan,
            component_id,
            txn,
            current_delta_sets,
            next_delta_sets,
            support_tables,
            &batches[batch_idx],
            batch_idx,
            aggregate_rule_batch_ids,
            processed,
            seedless_only,
            use_seeded_rules,
            derived_out,
            observed_rules,
            interp
        );
    }
    return err;
}

fn Value* deduce_seminaive_execute_component_aggregate_batch(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    DeduceTupleSupportTable[] support_tables,
    DeduceSemiNaiveAggregateBatch* batch,
    usz batch_idx,
    isz[] aggregate_rule_batch_ids,
    bool[] processed,
    bool seedless_only,
    bool use_seeded_rules,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || txn == null || processed.ptr == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid recursive aggregate state");
    }
    if (batch == null) return null;

    DeduceAggregateGroups* groups = &batch.groups;
    Value* err = null;
    for (usz i = 0; i < db.rule_signature_count && err == null; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (!deduce_rule_signature_has_aggregates(sig)) continue;
        if (i >= aggregate_rule_batch_ids.len || aggregate_rule_batch_ids[i] != (isz)batch_idx) continue;

        long anchor_count = deduce_seminaive_rule_anchor_count(sig, plan, component_id);
        if (seedless_only ? anchor_count != 0 : anchor_count == 0) continue;

        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        err = deduce_seminaive_evaluate_rule_with_groups(
            db,
            plan,
            component_id,
            sig,
            txn,
            current_delta_sets,
            next_delta_sets,
            {},
            support_tables,
            null,
            groups,
            true,
            use_seeded_rules,
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
            if (j >= aggregate_rule_batch_ids.len || aggregate_rule_batch_ids[j] != (isz)batch_idx) continue;
            long other_anchor_count = deduce_seminaive_rule_anchor_count(other, plan, component_id);
            if (seedless_only ? other_anchor_count != 0 : other_anchor_count == 0) continue;
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
                {},
                support_tables,
                null,
                groups,
                true,
                use_seeded_rules,
                false,
                derived_out,
                null,
                other_observed,
                interp
            );
            processed[j] = true;
        }
    }
    return err;
}
