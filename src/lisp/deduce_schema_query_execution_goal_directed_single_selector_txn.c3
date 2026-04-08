module lisp;

fn Value* deduce_query_execute_goal_directed_selector_ephemeral_in_txn(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    DeduceRuleHeadDemand* head_demand,
    DeduceRuleHeadDemand* projected_head_demand,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || scc_plan == null ||
        head_demand == null || projected_head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state",
            "deduce-query: invalid selector goal-directed execution state");
    }

    DeduceGoalDirectedSelectorTxnScaffold scaffold = {};
    Value* scaffold_err = deduce_goal_directed_selector_txn_scaffold_begin(
        rel.db,
        scc_plan,
        selected_components,
        selector_rule_index,
        use_seminaive_recursive,
        projected_head_demand,
        "deduce/query-transaction-failed",
        "deduce-query: transaction open failed",
        &scaffold,
        interp
    );
    if (scaffold_err != null) return scaffold_err;
    defer deduce_goal_directed_selector_txn_scaffold_release(&scaffold);

    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        scaffold.txn,
        interp,
        &scaffold.subject_keys
    );
    return deduce_goal_directed_selector_txn_scaffold_finish_read(
        &scaffold,
        rel.db,
        rel,
        selector_rule_index,
        head_demand,
        projected_head_demand,
        "ephemeral-head-demand-query",
        "query",
        rows,
        interp
    );
}
