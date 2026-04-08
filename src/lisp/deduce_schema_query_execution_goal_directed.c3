module lisp;

import std::core::mem;

fn Value* deduce_query_execute_goal_directed_plain_ephemeral_disjunction(
    Relation* rel,
    Value* filter_fn,
    bool use_seminaive_recursive,
    DeduceQueryHeadDemandCases* head_demands,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || filter_fn == null || head_demands == null || head_demands.count == 0 || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid plain disjunctive goal-directed query state");
    }

    DeduceRuleHeadDemand requested_union = {};
    bool has_requested_union = false;
    Value* requested_err = deduce_query_build_union_head_demand_positions(
        rel,
        head_demands.entries,
        head_demands.count,
        &requested_union,
        &has_requested_union,
        interp
    );
    if (requested_err != null) return requested_err;
    defer deduce_rule_head_demand_release(&requested_union);

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

    DeduceQueryHeadDemandCases projected_demands = {};
    defer deduce_query_head_demand_cases_release(&projected_demands);
    for (usz i = 0; i < head_demands.count; i++) {
        DeduceRuleHeadDemand projected_head_demand = {};
        bool[] projected_term_bound = {};
        Value*[] projected_term_values = {};
        bool has_projected_head_demand = false;
        Value* project_err = deduce_component_project_relation_head_demand_for_query(
            rel.db,
            &scc_plan,
            selected_component_id,
            &head_demands.entries[i],
            &projected_head_demand,
            &projected_term_bound,
            &projected_term_values,
            &has_projected_head_demand,
            interp
        );
        if (project_err != null) return project_err;
        if (!has_projected_head_demand) return null;
        if (deduce_query_recursive_component_requires_projected_head_demand_relaxation(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            ) && !deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }
        if (deduce_query_recursive_component_requires_projected_head_demand_fallback(
                rel.db,
                &scc_plan,
                selected_component_id,
                &projected_head_demand
            )) {
            return null;
        }

        Value* append_err = deduce_query_head_demand_cases_append_take(
            &projected_demands,
            &projected_head_demand,
            interp
        );
        if (append_err != null) {
            deduce_rule_head_demand_release(&projected_head_demand);
            return append_err;
        }
    }

    DeduceRuleHeadDemand applied_union = {};
    bool has_applied_union = false;
    Value* applied_err = deduce_query_build_union_head_demand_positions(
        rel,
        projected_demands.entries,
        projected_demands.count,
        &applied_union,
        &has_applied_union,
        interp
    );
    if (applied_err != null) return applied_err;
    defer deduce_rule_head_demand_release(&applied_union);

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

    Value* filtered_rev = make_nil(interp);
    for (usz i = 0; i < projected_demands.count; i++) {
        if (i > 0) deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);

        MdbTxn* txn = null;
        int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
        }

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
            &projected_demands.entries[i],
            &iterations,
            &derived,
            &max_component_iterations,
            {},
            interp
        );
        if (exec_err != null) {
            if (txn != null) mdb_txn_abort(txn);
            return exec_err;
        }

        Value* branch_rows = deduce_query_scan_relation_in_txn(rel, filter_fn, txn, interp);
        if (txn != null) {
            mdb_txn_abort(txn);
            txn = null;
        }
        if (branch_rows != null && branch_rows.tag == ERROR) return branch_rows;
        deduce_query_append_list_to_reversed(branch_rows, &filtered_rev, interp);
    }

    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        -1,
        has_requested_union ? &requested_union : null,
        has_applied_union ? &applied_union : null
    );
    return deduce_reverse_list(filtered_rev, interp);
}
