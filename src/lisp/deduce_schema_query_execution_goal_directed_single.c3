module lisp;

import std::core::mem;

fn Value* deduce_query_execute_goal_directed_plain_ephemeral(
    Relation* rel,
    Value* filter_fn,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || filter_fn == null || head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid plain goal-directed query state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool ready = false;
    Value* prepare_err = deduce_query_prepare_goal_directed_plain_head_demand(
        rel,
        head_demand,
        &scc_plan,
        &selected_components,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    MdbTxn* txn = null;
    Value* setup_err = deduce_goal_directed_plain_snapshot_and_execute_in_txn(
        rel.db,
        &scc_plan,
        selected_components,
        use_seminaive_recursive,
        &projected_head_demand,
        "deduce/query-transaction-failed",
        "deduce-query: transaction open failed",
        &schema_snapshots,
        &txn,
        interp
    );
    if (setup_err != null) return setup_err;
    defer deduce_goal_directed_plain_release_txn_setup(txn, schema_snapshots);

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = deduce_goal_directed_plain_subject_key_capture_start();
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        txn,
        interp,
        &subject_keys
    );
    if (used_out != null) *used_out = true;
    deduce_goal_directed_plain_note_read_and_subject_keys(
        rel.db,
        rel,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        head_demand,
        &projected_head_demand,
        &subject_keys
    );
    return rows;
}
