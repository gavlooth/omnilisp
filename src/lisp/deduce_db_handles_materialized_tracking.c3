module lisp;

fn bool deduce_relation_schema_is_materialized_ready(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null) return false;
    if (!schema.materialized_view || schema.dropped) return false;
    return deduce_relation_schema_has_derived_rules(db, schema);
}

fn DeduceMaterializedStaleReason deduce_relation_schema_effective_materialized_stale_reason(
    DeduceRelationSchema* schema
) {
    if (schema == null || !schema.materialized_view) return DEDUCE_MATERIALIZED_STALE_NONE;
    if (schema.materialized_refresh_count == 0) return DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED;
    return schema.materialized_last_stale_reason;
}

fn bool deduce_relation_schema_is_materialized_stale(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || !schema.materialized_view) return false;
    if (schema.materialized_refresh_count == 0) return true;
    if (schema.materialized_last_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE) return true;
    if (db.incremental_full_recompute_required) return true;
    return deduce_db_is_incremental_dirty_predicate(db, schema.name);
}

fn bool deduce_db_has_other_stale_materialized_relation(
    DeduceDb* db,
    SymbolId relation_name
) {
    if (db == null || db.relation_schemas == null) return false;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (schema == null || schema.name == relation_name || schema.dropped) continue;
        if (!schema.materialized_view) continue;
        if (deduce_relation_schema_is_materialized_stale(db, schema)) return true;
    }
    return false;
}

fn void deduce_relation_schema_note_materialized_stale(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || schema == null || !schema.materialized_view) return;
    schema.materialized_last_stale_mutation_epoch = db.incremental_mutation_epoch;
    schema.materialized_last_stale_reason = reason;
}

fn void deduce_db_note_materialized_views_stale(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        deduce_relation_schema_note_materialized_stale(db, &db.relation_schemas[i], reason);
    }
}

fn void deduce_db_note_ready_materialized_views_stale(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!deduce_relation_schema_is_materialized_ready(db, schema)) continue;
        deduce_relation_schema_note_materialized_stale(db, schema, reason);
    }
}

fn void deduce_relation_schema_note_materialized_refresh(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || !schema.materialized_view) return;
    if (schema.materialized_refresh_count < ulong.max) schema.materialized_refresh_count++;
    schema.materialized_last_refresh_mutation_epoch = db.incremental_mutation_epoch;
    schema.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
}

fn Value* deduce_db_persist_materialized_view_states(DeduceDb* db, Interp* interp) {
    if (db == null || interp == null) return null;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
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
