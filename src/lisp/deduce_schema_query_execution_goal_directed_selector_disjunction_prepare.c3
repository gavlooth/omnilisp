module lisp;

import std::core::mem;

fn Value* deduce_query_execute_selector_ephemeral_projected_demands(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    DeduceQueryHeadDemandCases* projected_demands,
    DeduceObservedRuleCounters[]* observed_rules_out,
    Value** filtered_rev_out,
    Interp* interp
) {
    if (observed_rules_out != null) *observed_rules_out = {};
    if (filtered_rev_out != null) *filtered_rev_out = make_nil(interp);

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        rel.db,
        selector_rule_index,
        &observed_rules,
        interp
    );
    if (observed_err != null) return observed_err;

    Value* filtered_rev = filtered_rev_out != null ? *filtered_rev_out : make_nil(interp);
    for (usz i = 0; i < projected_demands.count; i++) {
        if (i > 0) deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);

        MdbTxn* txn = null;
        int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            deduce_free_selected_rule_observed_counters(observed_rules);
            return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
        }

        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
            rel.db,
            scc_plan,
            selected_components,
            txn,
            256,
            use_seminaive_recursive,
            &projected_demands.entries[i],
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        );
        if (exec_err != null) {
            if (txn != null) mdb_txn_abort(txn);
            deduce_free_selected_rule_observed_counters(observed_rules);
            return exec_err;
        }

        Value* branch_rows = deduce_query_scan_relation_in_txn(rel, filter_fn, txn, interp);
        if (txn != null) {
            mdb_txn_abort(txn);
            txn = null;
        }
        if (branch_rows != null && branch_rows.tag == ERROR) {
            deduce_free_selected_rule_observed_counters(observed_rules);
            return branch_rows;
        }
        deduce_query_append_list_to_reversed(branch_rows, &filtered_rev, interp);
    }

    if (observed_rules_out != null) *observed_rules_out = observed_rules;
    if (filtered_rev_out != null) *filtered_rev_out = filtered_rev;
    return null;
}
