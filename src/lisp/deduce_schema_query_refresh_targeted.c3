module lisp;

import std::core::mem;

fn Value* deduce_refresh_materialized_relation_targeted(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    Env* env,
    Interp* interp,
    long* refreshed_stale_count_out,
    Value** refreshed_relations_out,
    Value** cleared_dirty_predicates_out,
    bool* used_incremental_out
) {
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = 0;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = make_nil(interp);
    if (used_incremental_out != null) *used_incremental_out = false;
    if (db == null || rel == null || target_schema == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid relation-scoped refresh state");
    }
    if (!deduce_relation_schema_is_materialized_ready(db, target_schema)) {
        return deduce_raise(interp, "deduce/refresh-materialized-relation-not-derived",
            "deduce/refresh!: materialized relation has no derived rule heads to refresh");
    }

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    int predicate_index = deduce_scc_find_predicate_index(scc_plan.predicates, rel.name);
    if (predicate_index < 0 || (usz)predicate_index >= scc_plan.predicate_component.len) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: derived materialized relation is missing from the dependency plan");
    }

    int target_component = scc_plan.predicate_component[(usz)predicate_index];
    if (target_component < 0 || (usz)target_component >= scc_plan.component_count) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid target component for materialized relation");
    }

    bool[] selected_components = mem::new_array(bool, scc_plan.component_count);
    if (selected_components.ptr == null && scc_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/refresh-out-of-memory", "deduce/refresh!: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_refresh_select_dependency_components(&scc_plan, (usz)target_component, selected_components)) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: failed to select refresh dependency closure");
    }

    bool used_incremental = false;
    Value* incremental_err = deduce_refresh_try_incremental_extensional_copy_relation(
        db,
        rel,
        target_schema,
        &used_incremental,
        interp
    );
    if (incremental_err != null) return incremental_err;

    if (!used_incremental) {
        for (usz i = 0; i < scc_plan.component_order.len; i++) {
            usz component_id = scc_plan.component_order[i];
            if (component_id >= selected_components.len || !selected_components[component_id]) continue;
            Value* err = deduce_naive_evaluate_component(
                db,
                &scc_plan,
                component_id,
                256,
                true,
                null,
                null,
                {},
                interp
            );
            if (err != null) return err;
        }
    }

    long refreshed_stale_count =
        deduce_refresh_count_selected_stale_materialized_views(db, &scc_plan, selected_components);
    Value* refreshed_relations =
        deduce_refresh_selected_stale_materialized_relations(db, &scc_plan, selected_components, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;
    Value* cleared_dirty_predicates =
        deduce_refresh_selected_dirty_predicates_payload(db, &scc_plan, selected_components, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
    deduce_refresh_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    deduce_refresh_note_selected_materialized_views(db, &scc_plan, selected_components);
    Value* persist_err = deduce_refresh_persist_selected_materialized_views(
        db,
        &scc_plan,
        selected_components,
        interp
    );
    if (persist_err != null) return persist_err;
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = refreshed_stale_count;
    if (refreshed_relations_out != null) *refreshed_relations_out = refreshed_relations;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = cleared_dirty_predicates;
    if (used_incremental_out != null) *used_incremental_out = used_incremental;
    return null;
}
