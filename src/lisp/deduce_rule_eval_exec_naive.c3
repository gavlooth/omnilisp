module lisp;

import std::core::mem;

fn Value* deduce_naive_evaluate_rule_with_groups(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
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

    if (!deduce_rule_apply_head_demand(sig, head_demand, var_bound, var_values)) {
        return null;
    }

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
        .head_demand = head_demand,
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
    DeduceRuleHeadDemand* head_demand,
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
        head_demand,
        null,
        true,
        derived_out,
        changed_out,
        observed_rule_counters,
        dry_run,
        interp
    );
}
