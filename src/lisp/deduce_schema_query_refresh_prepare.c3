module lisp;

struct DeduceRefreshRequest {
    DeduceDb*            db;
    Relation*            rel;
    DeduceRelationSchema* target_schema;
    bool                 relation_scope;
    bool                 target_was_stale;
    Value*               handle;
    long                 stale_materialized_view_count;
    long                 refreshable_stale_materialized_view_count;
}

fn Value* deduce_refresh_prepare_request(
    Value*[] args,
    DeduceRefreshRequest* refresh,
    Interp* interp
) {
    if (refresh != null) *refresh = {};
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/refresh-expected-db-or-relation",
            "deduce/refresh!: expected (deduce 'refresh! db-or-materialized-relation)");
    }

    Value* handle = args[0];
    if (!deduce_ffi_handle_name_equals(handle, "deduce-db") &&
        !deduce_ffi_handle_name_equals(handle, "deduce-relation")) {
        return deduce_raise(interp, "deduce/refresh-handle-not-deduce-db-or-relation",
            "deduce/refresh!: first argument must be a deduce database or materialized relation handle");
    }
    if (handle.ffi_val == null || handle.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/refresh-handle-closed",
            "deduce/refresh!: handle is closed");
    }

    DeduceDb* db = null;
    Relation* rel = null;
    DeduceRelationSchema* target_schema = null;
    bool relation_scope = false;
    bool target_was_stale = false;

    if (deduce_ffi_handle_name_equals(handle, "deduce-db")) {
        db = (DeduceDb*)handle.ffi_val.lib_handle;
        if (!db.open || db.env == null) {
            return deduce_raise(interp, "deduce/refresh-db-not-open",
                "deduce/refresh!: database is not open");
        }
    } else {
        relation_scope = true;
        rel = (Relation*)handle.ffi_val.lib_handle;
        if (rel.db == null || !rel.db.open || rel.db.env == null) {
            return deduce_raise(interp, "deduce/refresh-db-not-open",
                "deduce/refresh!: database is not open");
        }
        target_schema = deduce_relation_lookup_schema(rel);
        if (target_schema == null) {
            return deduce_raise(interp, "deduce/refresh-relation-not-found",
                "deduce/refresh!: relation schema is not registered");
        }
        if (rel.dropped || target_schema.dropped) {
            return deduce_raise(interp, "deduce/refresh-relation-dropped",
                "deduce/refresh!: relation is dropped");
        }
        if (!target_schema.materialized_view) {
            return deduce_raise(interp, "deduce/refresh-relation-not-materialized",
                "deduce/refresh!: relation is not a materialized view");
        }
        if (!deduce_relation_schema_is_materialized_ready(rel.db, target_schema)) {
            return deduce_raise(interp, "deduce/refresh-materialized-relation-not-derived",
                "deduce/refresh!: materialized relation has no derived rule heads to refresh");
        }
        target_was_stale = deduce_relation_schema_is_materialized_stale(rel.db, target_schema);
        db = rel.db;
    }

    long stale_materialized_view_count = 0;
    long refreshable_stale_materialized_view_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (!schema.materialized_view) continue;
        if (deduce_relation_schema_is_materialized_stale(db, schema)) {
            stale_materialized_view_count++;
            if (deduce_relation_schema_is_materialized_ready(db, schema)) {
                refreshable_stale_materialized_view_count++;
            }
        }
    }

    if (refresh != null) {
        *refresh = {
            .db = db,
            .rel = rel,
            .target_schema = target_schema,
            .relation_scope = relation_scope,
            .target_was_stale = target_was_stale,
            .handle = handle,
            .stale_materialized_view_count = stale_materialized_view_count,
            .refreshable_stale_materialized_view_count = refreshable_stale_materialized_view_count,
        };
    }
    return null;
}
