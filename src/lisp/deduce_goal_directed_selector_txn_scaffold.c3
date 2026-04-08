module lisp;

import std::core::mem;

struct DeduceGoalDirectedSelectorTxnScaffold {
    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots;
    DeduceObservedRuleCounters[]           observed_rules;
    MdbTxn*                                txn;
    DeduceGoalDirectedReadSubjectKeyCapture subject_keys;
}

fn void deduce_goal_directed_selector_txn_scaffold_release(
    DeduceGoalDirectedSelectorTxnScaffold* scaffold
) {
    if (scaffold == null) return;
    deduce_goal_directed_read_subject_key_capture_release(&scaffold.subject_keys);
    if (scaffold.txn != null) mdb_txn_abort(scaffold.txn);
    deduce_free_selected_rule_observed_counters(scaffold.observed_rules);
    deduce_fixpoint_restore_selected_schema_estimates(scaffold.schema_snapshots);
    if (scaffold.schema_snapshots.ptr != null) mem::free(scaffold.schema_snapshots.ptr);
    *scaffold = {};
}

fn Value* deduce_goal_directed_selector_txn_scaffold_begin(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* projected_head_demand,
    char[] transaction_failed_code,
    char[] transaction_failed_message,
    DeduceGoalDirectedSelectorTxnScaffold* out_scaffold,
    Interp* interp
) {
    *out_scaffold = {};
    out_scaffold.subject_keys.complete = true;

    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        db,
        scc_plan,
        selected_components,
        &out_scaffold.schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;

    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        db,
        selector_rule_index,
        &out_scaffold.observed_rules,
        interp
    );
    if (observed_err != null) {
        deduce_goal_directed_selector_txn_scaffold_release(out_scaffold);
        return observed_err;
    }

    int rc = mdb_txn_begin(db.env, null, 0, &out_scaffold.txn);
    if (rc != MDB_SUCCESS || out_scaffold.txn == null) {
        deduce_goal_directed_selector_txn_scaffold_release(out_scaffold);
        return deduce_raise(interp, transaction_failed_code, transaction_failed_message);
    }

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        db,
        scc_plan,
        selected_components,
        out_scaffold.txn,
        256,
        use_seminaive_recursive,
        projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        out_scaffold.observed_rules,
        interp
    );
    if (exec_err != null) {
        deduce_goal_directed_selector_txn_scaffold_release(out_scaffold);
        return exec_err;
    }

    return null;
}

fn Value* deduce_goal_directed_selector_txn_scaffold_finish_read(
    DeduceGoalDirectedSelectorTxnScaffold* scaffold,
    DeduceDb* db,
    Relation* rel,
    long selector_rule_index,
    DeduceRuleHeadDemand* head_demand,
    DeduceRuleHeadDemand* projected_head_demand,
    char[] read_path_symbol,
    char[] read_op_symbol,
    Value* rows,
    Interp* interp
) {
    deduce_db_note_goal_directed_read(
        db,
        rel.name,
        interp.symbols.intern(read_path_symbol),
        interp.symbols.intern(read_op_symbol),
        selector_rule_index,
        head_demand,
        projected_head_demand,
        &scaffold.observed_rules[(usz)selector_rule_index]
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &scaffold.subject_keys
    );
    return rows;
}
