module lisp;

import std::core::mem;

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
    DeduceDeltaTupleSet[] scratch_visible_sets,
    DeduceTupleSupportTable[] support_tables,
    DeduceRuleHeadDemand* head_demand,
    DeduceAggregateGroups* aggregate_groups,
    bool aggregate_component_mode,
    bool allow_unanchored,
    bool dry_run,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (db == null || plan == null || sig == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
    }
    if (deduce_rule_signature_has_aggregates(sig) && aggregate_groups == null) {
        return deduce_raise(
            interp,
            "deduce/analyze-invalid-state",
            "deduce/analyze: recursive aggregate evaluation requires aggregate group state"
        );
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
        .head_demand = head_demand,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .aggregate_groups = aggregate_groups,
        .plan = plan,
        .component_id = component_id,
        .current_delta_sets = current_delta_sets,
        .next_delta_sets = next_delta_sets,
        .scratch_visible_sets = scratch_visible_sets,
        .support_tables = support_tables,
        .proof_seen_map = null,
        .anchor_active = false,
        .anchor_step = 0,
        .anchor_delta_is_removal = false,
        .aggregate_component_mode = aggregate_component_mode,
        .observed_rule_counters = observed_rule_counters,
        .dry_run = dry_run,
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
    DeduceRuleHeadDemand* head_demand,
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
        {},
        head_demand,
        null,
        false,
        true,
        false,
        derived_out,
        changed_out,
        observed_rule_counters,
        interp
    );
}

fn Value* deduce_seminaive_evaluate_rule_dry_run(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] current_delta_sets,
    DeduceSignedDeltaSet[] next_delta_sets,
    DeduceDeltaTupleSet[] scratch_visible_sets,
    DeduceRuleHeadDemand* head_demand,
    long* derived_out,
    bool* changed_out,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    deduce_worker_scratch_trace("dry_run_rule_begin", component_id, (long)sig.head_predicate, 0);
    Value* err = deduce_seminaive_evaluate_rule_with_groups(
        db,
        plan,
        component_id,
        sig,
        txn,
        current_delta_sets,
        next_delta_sets,
        scratch_visible_sets,
        {},
        head_demand,
        null,
        false,
        true,
        true,
        derived_out,
        changed_out,
        observed_rule_counters,
        interp
    );
    deduce_worker_scratch_trace(
        "dry_run_rule_end",
        component_id,
        derived_out != null ? *derived_out : 0,
        changed_out != null && *changed_out ? 1 : 0
    );
    return err;
}
