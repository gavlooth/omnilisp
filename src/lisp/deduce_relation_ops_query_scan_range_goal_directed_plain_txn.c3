module lisp;

fn Value* deduce_scan_range_execute_goal_directed_plain_in_txn(
    DeduceDb* db,
    Relation* rel,
    Value*[] lower_values,
    Value*[] upper_values,
    char* start_key,
    usz start_key_len,
    bool use_seminaive_recursive,
    bool* used_out,
    DeduceRuleHeadDemand* head_demand,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    DeduceRuleHeadDemand* projected_head_demand,
    Interp* interp
) {
    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    MdbTxn* txn = null;
    Value* setup_err = deduce_goal_directed_plain_snapshot_and_execute_in_txn(
        db,
        scc_plan,
        selected_components,
        use_seminaive_recursive,
        projected_head_demand,
        "deduce/scan-range-transaction-failed",
        "deduce-scan-range: transaction open failed",
        &schema_snapshots,
        &txn,
        interp
    );
    if (setup_err != null) return setup_err;
    defer deduce_goal_directed_plain_release_txn_setup(txn, schema_snapshots);

    if (used_out != null) *used_out = true;
    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = deduce_goal_directed_plain_subject_key_capture_start();
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = relation_scan_range_in_txn(
        rel,
        interp,
        txn,
        start_key,
        start_key_len,
        true,
        lower_values,
        upper_values,
        true,
        &subject_keys
    );
    deduce_goal_directed_plain_note_read_and_subject_keys(
        db,
        rel,
        interp.symbols.intern("ephemeral-head-demand-scan-range"),
        interp.symbols.intern("scan-range"),
        head_demand,
        projected_head_demand,
        &subject_keys
    );
    return rows;
}
