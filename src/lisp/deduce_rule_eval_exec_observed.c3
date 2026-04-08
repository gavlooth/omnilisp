module lisp;

struct DeduceObservedStepCounters {
    long rows_read;
    long rows_emitted;
    long index_hits;
    long index_misses;
    long join_probes;
}

struct DeduceObservedRuleCounters {
    DeduceObservedStepCounters* steps;
    usz                         step_count;
}

fn DeduceObservedStepCounters* deduce_observed_step_counters_ref(
    void* observed_rule_counters,
    usz step_idx
) @inline {
    if (observed_rule_counters == null) return null;
    DeduceObservedRuleCounters* rule = (DeduceObservedRuleCounters*)observed_rule_counters;
    if (rule.steps == null || step_idx >= rule.step_count) return null;
    return &rule.steps[step_idx];
}

fn void deduce_observed_step_note_rows_read(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.rows_read < long.max) step.rows_read++;
}

fn void deduce_observed_step_note_rows_emitted(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.rows_emitted < long.max) step.rows_emitted++;
}

fn void deduce_observed_step_note_join_probe(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.join_probes < long.max) step.join_probes++;
}

fn void deduce_observed_rule_counters_reset(DeduceObservedRuleCounters* counters) {
    if (counters == null || counters.steps == null) return;
    for (usz i = 0; i < counters.step_count; i++) {
        counters.steps[i] = {};
    }
}

fn Value* deduce_collect_rule_observed_counters(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    DeduceObservedRuleCounters* counters,
    Interp* interp
) {
    if (db == null || sig == null || counters == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid observed-counter state");
    }
    deduce_observed_rule_counters_reset(counters);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    return deduce_naive_evaluate_rule(db, sig, txn, null, null, null, counters, true, interp);
}
