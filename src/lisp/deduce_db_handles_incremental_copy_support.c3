module lisp;

fn bool deduce_db_queue_supported_incremental_insert_for_source(
    DeduceDb* db,
    SymbolId source_predicate,
    bool derived_source,
    void* key_data,
    usz key_len
) {
    if (db == null || source_predicate == (SymbolId)0 || (key_data == null && key_len > 0)) return false;
    if (db.relation_schemas == null) return true;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* target_schema = &db.relation_schemas[i];
        if (!target_schema.materialized_view) continue;
        if (!deduce_relation_schema_is_materialized_ready(db, target_schema)) continue;
        if (target_schema.materialized_refresh_count == 0) continue;

        SymbolId candidate_source = (SymbolId)0;
        bool supported = derived_source
            ? deduce_relation_schema_incremental_derived_copy_source(db, target_schema, &candidate_source)
            : deduce_relation_schema_incremental_extensional_copy_source(db, target_schema, &candidate_source);
        if (!supported) continue;
        if (candidate_source != source_predicate) continue;

        if (!deduce_delta_set_append_encoded_tuple(&target_schema.maintained_pending_insertions, key_data, key_len)) {
            return false;
        }
    }
    return true;
}

fn bool deduce_db_queue_supported_incremental_insert(
    Relation* rel,
    void* key_data,
    usz key_len
) {
    if (rel == null || rel.db == null) return false;
    return deduce_db_queue_supported_incremental_insert_for_source(rel.db, rel.name, false, key_data, key_len);
}

fn bool deduce_db_queue_supported_incremental_insert_from_materialized_source(
    DeduceDb* db,
    SymbolId source_predicate,
    void* key_data,
    usz key_len
) {
    return deduce_db_queue_supported_incremental_insert_for_source(db, source_predicate, true, key_data, key_len);
}

fn Value* deduce_db_note_insert_mutation(Relation* rel, void* key_data, usz key_len, Interp* interp) {
    deduce_db_note_committed_mutation(rel, false);
    if (rel != null && rel.db != null && !deduce_db_queue_supported_incremental_insert(rel, key_data, key_len)) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            rel.db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
    return deduce_db_persist_materialized_view_states(rel != null ? rel.db : null, interp);
}
