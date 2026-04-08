module lisp;

fn Value* deduce_match_execute_goal_directed_plain_ephemeral(
    Relation* rel,
    Value* pattern,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid plain goal-directed match state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

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
        return null;
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
        {},
        interp
    );
    if (exec_err != null) return exec_err;

    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-match"),
        interp.symbols.intern("match"),
        -1,
        head_demand,
        &projected_head_demand
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
