module lisp;

fn Value* deduce_scan_range_execute_goal_directed_selector_in_txn(
    DeduceDb* db,
    Relation* rel,
    Value*[] lower_values,
    Value*[] upper_values,
    char* start_key,
    usz start_key_len,
    bool use_seminaive_recursive,
    long selector_rule_index,
    DeduceRuleHeadDemand* head_demand,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    DeduceRuleHeadDemand* projected_head_demand,
    Interp* interp
) {
    DeduceGoalDirectedSelectorTxnScaffold scaffold = {};
    Value* scaffold_err = deduce_goal_directed_selector_txn_scaffold_begin(
        db,
        scc_plan,
        selected_components,
        selector_rule_index,
        use_seminaive_recursive,
        projected_head_demand,
        "deduce/scan-range-transaction-failed",
        "deduce-scan-range: transaction open failed",
        &scaffold,
        interp
    );
    if (scaffold_err != null) return scaffold_err;
    defer deduce_goal_directed_selector_txn_scaffold_release(&scaffold);

    Value* rows = relation_scan_range_in_txn(
        rel,
        interp,
        scaffold.txn,
        start_key,
        start_key_len,
        true,
        lower_values,
        upper_values,
        true,
        &scaffold.subject_keys
    );
    return deduce_goal_directed_selector_txn_scaffold_finish_read(
        &scaffold,
        db,
        rel,
        selector_rule_index,
        head_demand,
        projected_head_demand,
        "ephemeral-head-demand-scan-range",
        "scan-range",
        rows,
        interp
    );
}
