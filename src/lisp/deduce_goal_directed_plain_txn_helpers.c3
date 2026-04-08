module lisp;

import std::core::mem;

fn Value* deduce_goal_directed_plain_snapshot_and_execute_in_txn(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    bool[] selected_components,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* projected_head_demand,
    char[] txn_error_code,
    char[] txn_error_message,
    DeduceSelectedSchemaEstimateSnapshot[]* schema_snapshots_out,
    MdbTxn** txn_out,
    Interp* interp
) {
    if (schema_snapshots_out != null) *schema_snapshots_out = {};
    if (txn_out != null) *txn_out = null;

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        db,
        scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
        return deduce_raise(interp, txn_error_code, txn_error_message);
    }

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        db,
        scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        {},
        interp
    );
    if (exec_err != null) {
        if (txn != null) mdb_txn_abort(txn);
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
        return exec_err;
    }

    if (schema_snapshots_out != null) *schema_snapshots_out = schema_snapshots;
    if (txn_out != null) *txn_out = txn;
    return null;
}

fn void deduce_goal_directed_plain_release_txn_setup(
    MdbTxn* txn,
    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots
) {
    if (txn != null) mdb_txn_abort(txn);
    deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
    if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
}

fn DeduceGoalDirectedReadSubjectKeyCapture deduce_goal_directed_plain_subject_key_capture_start() {
    return { .complete = true };
}

fn void deduce_goal_directed_plain_note_read_and_subject_keys(
    DeduceDb* db,
    Relation* rel,
    SymbolId source_tag,
    SymbolId mode_tag,
    DeduceRuleHeadDemand* head_demand,
    DeduceRuleHeadDemand* projected_head_demand,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys
) {
    deduce_db_note_goal_directed_read(
        db,
        rel.name,
        source_tag,
        mode_tag,
        -1,
        head_demand,
        projected_head_demand
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        subject_keys
    );
}
