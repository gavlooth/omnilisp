module lisp;

fn bool deduce_refresh_select_dependency_components(
    DeduceSccPlan* plan,
    usz target_component_id,
    bool[] selected_components
) {
    if (plan == null || target_component_id >= plan.component_count) return false;
    if (selected_components.len < plan.component_count) return false;

    for (usz i = 0; i < selected_components.len; i++) selected_components[i] = false;
    selected_components[target_component_id] = true;

    bool changed = true;
    while (changed) {
        changed = false;
        foreach (edge : plan.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= plan.predicate_component.len || (usz)edge.to >= plan.predicate_component.len) continue;

            int from_component = plan.predicate_component[(usz)edge.from];
            int to_component = plan.predicate_component[(usz)edge.to];
            if (from_component < 0 || to_component < 0 || from_component == to_component) continue;
            if ((usz)from_component >= selected_components.len || (usz)to_component >= selected_components.len) continue;
            if (!selected_components[(usz)from_component] || selected_components[(usz)to_component]) continue;

            selected_components[(usz)to_component] = true;
            changed = true;
        }
    }
    return true;
}

fn bool deduce_refresh_predicate_in_selected_components(
    DeduceSccPlan* plan,
    bool[] selected_components,
    SymbolId predicate
) {
    if (plan == null || predicate == (SymbolId)0) return false;
    int predicate_index = deduce_scc_find_predicate_index(plan.predicates, predicate);
    if (predicate_index < 0 || (usz)predicate_index >= plan.predicate_component.len) return false;

    int component_id = plan.predicate_component[(usz)predicate_index];
    if (component_id < 0 || (usz)component_id >= selected_components.len) return false;
    return selected_components[(usz)component_id];
}

fn void deduce_refresh_clear_selected_dirty_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) return;

    usz write_index = 0;
    for (usz read_index = 0; read_index < db.incremental_dirty_count; read_index++) {
        SymbolId predicate = db.incremental_dirty_predicates[read_index];
        if (deduce_refresh_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        db.incremental_dirty_predicates[write_index++] = predicate;
    }
    db.incremental_dirty_count = write_index;
}

fn void deduce_refresh_note_selected_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null) return;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
        deduce_relation_schema_note_materialized_refresh(db, schema);
    }
}

fn Value* deduce_refresh_persist_selected_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null || interp == null) return null;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            schema.name,
            schema.materialized_view,
            schema.materialized_refresh_policy,
            schema.materialized_refresh_count,
            schema.materialized_last_refresh_mutation_epoch,
            schema.materialized_last_stale_mutation_epoch,
            schema.materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) return meta_err;
    }
    return null;
}
