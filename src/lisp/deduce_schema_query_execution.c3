module lisp;

import std::core::mem;

fn Value* deduce_query_execute_goal_directed_selector_ephemeral(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid goal-directed query state");
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
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
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
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("selected-component-closure"),
            interp.symbols.intern("query"),
            selector_rule_index,
            null,
            null,
            null,
            interp.symbols.intern("unsupported-captured-call-shape")
        );
        return null;
    }
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
        return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
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
    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        txn,
        interp,
        &subject_keys
    );
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        selector_rule_index,
        head_demand,
        &projected_head_demand,
        &observed_rules[(usz)selector_rule_index]
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}

fn SymbolId deduce_materialized_refresh_policy_value(
    DeduceRelationSchema* schema,
    Interp* interp
) @inline {
    if (schema != null && schema.materialized_refresh_policy != (SymbolId)0) {
        return schema.materialized_refresh_policy;
    }
    return interp.symbols.intern("manual");
}

fn Value* deduce_relation_maybe_refresh_on_read(
    Value* rel_handle,
    Relation* rel,
    Env* env,
    SymbolId read_surface,
    Interp* interp,
    bool* handled_out
) {
    (void)read_surface;
    if (handled_out != null) *handled_out = false;
    if (rel == null || rel.db == null || interp == null) return null;

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    SymbolId on_read_policy = interp.symbols.intern("on-read");
    if (schema == null || !schema.materialized_view || schema.materialized_refresh_policy != on_read_policy) {
        return null;
    }

    if (handled_out != null) *handled_out = true;
    if (!deduce_relation_schema_is_materialized_stale(rel.db, schema)) return null;

    if (!deduce_relation_schema_is_materialized_ready(rel.db, schema)) {
        return deduce_raise(
            interp,
            "deduce/on-read-refresh-unavailable",
            "deduce: on-read materialized relation cannot refresh because derived rules are unavailable in the current state"
        );
    }

    if (!rel.db.incremental_full_recompute_required) {
        long refreshed_stale_count = 0;
        Value* refreshed_relations = make_nil(interp);
        Value* cleared_dirty_predicates = make_nil(interp);
        return deduce_refresh_materialized_relation_targeted(
            rel.db,
            rel,
            schema,
            env,
            interp,
            &refreshed_stale_count,
            &refreshed_relations,
            &cleared_dirty_predicates,
            null
        );
    }

    if (rel_handle == null) {
        return deduce_raise(
            interp,
            "deduce/on-read-refresh-invalid-state",
            "deduce: on-read materialized relation refresh requires a live relation handle"
        );
    }

    Value*[1] refresh_args = { rel_handle };
    Value* refresh_result = prim_deduce_refresh(refresh_args[..], env, interp);
    if (refresh_result == null || refresh_result.tag == ERROR) return refresh_result;
    return null;
}

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
    Value* project_err = deduce_component_project_relation_head_demand_for_query(
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
        return deduce_raise(interp, "deduce/query-transaction-failed", "deduce-query: transaction open failed");
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

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_query_scan_relation_in_txn(
        rel,
        filter_fn,
        txn,
        interp,
        &subject_keys
    );
    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        -1,
        head_demand,
        &projected_head_demand
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}

fn Value* deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
    Relation* rel,
    Value* filter_fn,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceQueryHeadDemandCases* head_demands,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || filter_fn == null || head_demands == null || head_demands.count == 0 ||
        surface == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive goal-directed query state");
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
            deduce_db_note_goal_directed_read(
                rel.db,
                rel.name,
                interp.symbols.intern("selected-component-closure"),
                interp.symbols.intern("query"),
                selector_rule_index,
                has_requested_union ? &requested_union : null,
                null,
                null,
                interp.symbols.intern("unsupported-disjunction-shape")
            );
            return null;
        }
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

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        rel.db,
        selector_rule_index,
        &observed_rules,
        interp
    );
    if (observed_err != null) return observed_err;
    defer deduce_free_selected_rule_observed_counters(observed_rules);

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
            observed_rules,
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

    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-query"),
        interp.symbols.intern("query"),
        selector_rule_index,
        has_requested_union ? &requested_union : null,
        has_applied_union ? &applied_union : null,
        &observed_rules[(usz)selector_rule_index]
    );
    return deduce_reverse_list(filtered_rev, interp);
}

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

fn Value* prim_deduce_query(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2 || args.len > 4) {
        return deduce_raise(interp, "deduce/query-expected-relation-filter",
            "deduce-query: expected (deduce-query relation filter-fn [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/query-relation-not-handle", "deduce-query: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/query-relation-handle-closed", "deduce-query: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/query-relation-not-deduce-relation", "deduce-query: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/query-database-not-open", "deduce-query: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/query-relation-dropped", "deduce-query: relation is dropped");

    Value* filter_fn = args[1];
    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/query-out-of-memory",
        .out_of_memory_message = "deduce/query: out of memory",
        .invalid_selector_code = "deduce/query-invalid-selector",
        .invalid_selector_message = "deduce-query: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/query-invalid-engine",
        .invalid_engine_message = "deduce-query: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/query-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-query: rule-index is out of range",
        .invalid_state_code = "deduce/query-invalid-state",
        .invalid_state_message = "deduce-query: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/query-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/query: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/query-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/query: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/query-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/query: selected rule closure does not produce the requested relation",
    };
    bool use_seminaive_recursive = true;
    long selector_rule_index = -1;
    Value* parse_err = deduce_parse_goal_directed_selector_and_engine(
        args,
        2,
        &surface,
        &selector_rule_index,
        &use_seminaive_recursive,
        interp
    );
    if (parse_err != null) return parse_err;
    Value* exec_err = null;

    Value* key_values_err = deduce_relation_ensure_column_key_values(rel, interp, null);
    if (key_values_err != null) return key_values_err;

    DeduceRuleHeadDemand head_demand = {};
    bool[] term_bound = {};
    Value*[] term_values = {};
    bool has_any_bound = false;
    bool supports_head_demand = false;
    Value* demand_err = deduce_query_try_build_head_demand_from_filter(
        rel,
        filter_fn,
        &head_demand,
        &term_bound,
        &term_values,
        &has_any_bound,
        &supports_head_demand,
        interp
    );
    if (demand_err != null) return demand_err;
    defer if (term_bound.ptr != null) mem::free(term_bound.ptr);
    defer if (term_values.ptr != null) mem::free(term_values.ptr);

    if (supports_head_demand && has_any_bound) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &head_demand,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &head_demand,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    DeduceQueryHeadDemandCases disjunctive_head_demands = {};
    defer deduce_query_head_demand_cases_release(&disjunctive_head_demands);
    bool supports_disjunctive_head_demand = false;
    if (!supports_head_demand || !has_any_bound) {
        Value* disjunctive_err = deduce_query_try_build_head_demand_disjunction_from_filter(
            rel,
            filter_fn,
            &disjunctive_head_demands,
            &supports_disjunctive_head_demand,
            interp
        );
        if (disjunctive_err != null) return disjunctive_err;
    }

    if (supports_disjunctive_head_demand && disjunctive_head_demands.count > 0) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral_disjunction(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    bool selector_goal_directed_executed = false;
    if (selector_rule_index >= 0) {
        exec_err = deduce_execute_goal_directed_relation_selector(
            rel.db,
            rel.name,
            selector_rule_index,
            use_seminaive_recursive,
            interp.symbols.intern("query"),
            &surface,
            interp
        );
        if (exec_err != null) return exec_err;
        selector_goal_directed_executed = true;
        bool handled = false;
        exec_err = deduce_relation_maybe_refresh_on_read(
            rel_val,
            rel,
            env,
            interp.symbols.intern("query"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    } else {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("query"),
            env,
            interp
        );
        if (exec_err != null) return exec_err;
    }

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            deduce_goal_directed_read_subject_key_capture_record(
                &subject_keys,
                it.key.mv_data,
                it.key.mv_size
            );
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    if (selector_rule_index >= 0 && !selector_goal_directed_executed) {
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("no-op"),
            interp.symbols.intern("query"),
            selector_rule_index
        );
    }
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return deduce_reverse_list(filtered_rev, interp);
}
