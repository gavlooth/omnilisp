module lisp;

fn Value* deduce_match_execute_goal_directed_selector_ephemeral(
    Relation* rel,
    Value* pattern,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid goal-directed match state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        rel.db,
        rel.name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            rel.db,
            &scc_plan,
            selected_components,
            256,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            {},
            interp
        );
        if (exec_err != null) return exec_err;
        deduce_fixpoint_clear_selected_dirty_predicates(rel.db, &scc_plan, selected_components);
        return deduce_match_scan_relation(rel, pattern, interp);
    }

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
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
    defer deduce_free_selected_rule_observed_counters(observed_rules);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        rel.db,
        &scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        &projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        observed_rules,
        interp
    );
    if (exec_err != null) return exec_err;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-match"),
        interp.symbols.intern("match"),
        selector_rule_index,
        head_demand,
        &projected_head_demand,
        &observed_rules[(usz)selector_rule_index]
    );
    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_match_scan_relation_in_txn(
        rel,
        pattern,
        txn,
        interp,
        &subject_keys
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}
