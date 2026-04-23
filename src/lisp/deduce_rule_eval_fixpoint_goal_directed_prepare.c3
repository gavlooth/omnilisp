module lisp;

import std::core::mem;

fn Value* deduce_fixpoint_snapshot_selected_schema_estimates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    DeduceSelectedSchemaEstimateSnapshot[]* out_snapshots,
    Interp* interp
) {
    if (out_snapshots == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid schema snapshot state");
    }
    *out_snapshots = {};
    if (db == null || plan == null) return null;

    usz snapshot_count = 0;
    for (usz i = 0; i < plan.predicates.len; i++) {
        SymbolId predicate = plan.predicates[i];
        if (!deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        if (deduce_db_lookup_relation_schema(db, predicate) != null) snapshot_count++;
    }
    if (snapshot_count == 0) return null;

    DeduceSelectedSchemaEstimateSnapshot[] snapshots =
        mem::new_array(DeduceSelectedSchemaEstimateSnapshot, snapshot_count);
    if (snapshots.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    usz write_index = 0;
    for (usz i = 0; i < plan.predicates.len; i++) {
        SymbolId predicate = plan.predicates[i];
        if (!deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) continue;

        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
        if (schema == null) continue;

        snapshots[write_index++] = {
            .schema = schema,
            .cardinality_estimate = schema.cardinality_estimate,
            .distinct_estimate = schema.distinct_estimate,
        };
    }
    *out_snapshots = snapshots;
    return null;
}

fn void deduce_fixpoint_restore_selected_schema_estimates(
    DeduceSelectedSchemaEstimateSnapshot[] snapshots
) {
foreach (item : snapshots) {
        DeduceSelectedSchemaEstimateSnapshot snapshot = item;
        if (snapshot.schema == null) continue;
        snapshot.schema.cardinality_estimate = snapshot.cardinality_estimate;
        snapshot.schema.distinct_estimate = snapshot.distinct_estimate;
}
}

fn bool deduce_fixpoint_selected_components_have_dirty_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) {
        return false;
    }

    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        if (deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) {
            return true;
        }
    }
    return false;
}

fn Value* deduce_prepare_goal_directed_target_relation_plan_if_dirty(
    DeduceDb* db,
    SymbolId relation_name,
    DeduceSccPlan* out_plan,
    bool[]* out_selected_components,
    usz* out_selected_component_id,
    bool* out_ready,
    Interp* interp
) {
    if (out_plan == null || out_selected_components == null || out_selected_component_id == null || out_ready == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed target relation state");
    }
    *out_plan = {};
    *out_selected_components = {};
    *out_selected_component_id = 0;
    *out_ready = false;

    if (db == null || relation_name == (SymbolId)0 || interp == null) return null;
    if (db.incremental_full_recompute_required || db.incremental_dirty_count == 0) return null;

    Value* plan_err = deduce_build_scc_plan(db, out_plan, interp);
    if (plan_err != null) return plan_err;

    int predicate_index = deduce_scc_find_predicate_index(out_plan.predicates, relation_name);
    if (predicate_index < 0 || (usz)predicate_index >= out_plan.predicate_component.len) return null;

    int component_id = out_plan.predicate_component[(usz)predicate_index];
    if (component_id < 0 || (usz)component_id >= out_plan.component_count) return null;

    bool recursive =
        out_plan.component_recursive.ptr != null &&
        (usz)component_id < out_plan.component_recursive.len &&
        out_plan.component_recursive[(usz)component_id];
    if (!recursive) return null;

    bool has_aggregates = deduce_component_has_aggregate_rules(db, out_plan, (usz)component_id);
    bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, out_plan, (usz)component_id);
    if (has_aggregates || has_negated_body_atoms) return null;

    bool[] selected_components = mem::new_array(bool, out_plan.component_count);
    if (selected_components.ptr == null && out_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    if (!deduce_fixpoint_select_dependency_components(out_plan, (usz)component_id, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_raise(interp, "deduce/invalid-state",
            "deduce: failed to select automatic goal-directed component closure");
    }
    if (!deduce_fixpoint_selected_components_have_dirty_predicates(db, out_plan, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return null;
    }

    *out_selected_components = selected_components;
    *out_selected_component_id = (usz)component_id;
    *out_ready = true;
    return null;
}

fn Value* deduce_auto_execute_goal_directed_relation_if_dirty(
    DeduceDb* db,
    SymbolId relation_name,
    bool use_seminaive_recursive,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        db,
        relation_name,
        &scc_plan,
        &selected_components,
        &component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
        db,
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
    deduce_fixpoint_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    if (used_out != null) *used_out = true;
    return null;
}
