module lisp;

fn Value* prim_deduce_dematerialize(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/dematerialize-expected-relation",
            "deduce/dematerialize!: expected (deduce 'dematerialize! relation)");
    }

    Value* rel_val = args[0];
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-deduce-relation",
            "deduce/dematerialize!: first argument must be a deduce relation handle");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/dematerialize-relation-handle-closed",
            "deduce/dematerialize!: relation handle is closed");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/dematerialize-database-not-open",
            "deduce/dematerialize!: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-found",
            "deduce/dematerialize!: relation schema is not registered");
    }
    if (rel.dropped || schema.dropped) {
        return deduce_raise(interp, "deduce/dematerialize-relation-dropped",
            "deduce/dematerialize!: relation is dropped");
    }
    if (!schema.materialized_view) {
        return deduce_raise(interp, "deduce/dematerialize-relation-not-materialized",
            "deduce/dematerialize!: relation is not a materialized view");
    }

    Value* meta_err = deduce_relation_meta_persist_state(
        rel.db,
        rel.name,
        false,
        (SymbolId)0,
        0,
        0,
        0,
        DEDUCE_MATERIALIZED_STALE_NONE,
        true,
        interp
    );
    if (meta_err != null) return meta_err;

    schema.materialized_view = false;
    schema.materialized_refresh_policy = (SymbolId)0;
    schema.materialized_refresh_count = 0;
    schema.materialized_last_refresh_mutation_epoch = 0;
    schema.materialized_last_stale_mutation_epoch = 0;
    schema.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    deduce_relation_schema_clear_pending_incremental_insertions(schema);

    rel.materialized_view = false;
    rel.materialized_refresh_policy = (SymbolId)0;
    rel.materialized_refresh_count = 0;
    rel.materialized_last_refresh_mutation_epoch = 0;
    rel.materialized_last_stale_mutation_epoch = 0;
    rel.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    return make_void(interp);
}
