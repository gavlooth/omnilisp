module lisp;

fn long deduce_refresh_count_selected_stale_materialized_views(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null) return 0;

    long refreshed_stale_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;

        if (deduce_relation_schema_is_materialized_stale(db, schema)) refreshed_stale_count++;
    }
    return refreshed_stale_count;
}

fn Value* deduce_refresh_selected_stale_materialized_relations(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, schema.name)) continue;
        if (!deduce_relation_schema_is_materialized_stale(db, schema)) continue;
        rev = make_cons(interp, make_symbol(interp, schema.name), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_refresh_stale_materialized_relations(
    DeduceDb* db,
    bool ready_only,
    Interp* interp
) {
    if (db == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (ready_only && !deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        if (!deduce_relation_schema_is_materialized_stale(db, schema)) continue;
        rev = make_cons(interp, make_symbol(interp, schema.name), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_dirty_predicates_payload(DeduceDb* db, Interp* interp) {
    if (db == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates == null) break;
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_refresh_selected_dirty_predicates_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates == null) break;
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        if (!deduce_refresh_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}
