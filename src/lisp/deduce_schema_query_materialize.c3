module lisp;

<*
@require interp != null
*>
fn Value* prim_deduce_materialize(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/materialize-expected-relation",
            "deduce/materialize!: expected (deduce 'materialize! relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/materialize-relation-not-handle",
            "deduce/materialize!: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/materialize-relation-handle-closed",
            "deduce/materialize!: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/materialize-relation-not-deduce-relation",
            "deduce/materialize!: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/materialize-database-not-open",
            "deduce/materialize!: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/materialize-relation-not-found",
            "deduce/materialize!: relation schema is not registered");
    }
    if (rel.dropped || schema.dropped) {
        return deduce_raise(interp, "deduce/materialize-relation-dropped",
            "deduce/materialize!: relation is dropped");
    }
    if (!deduce_relation_schema_has_derived_rules(rel.db, schema)) {
        return deduce_raise(interp, "deduce/materialize-relation-not-derived",
            "deduce/materialize!: relation has no derived rule heads to materialize");
    }

    SymbolId manual_policy = interp.symbols.intern("manual");
    if (manual_policy == INVALID_SYMBOL_ID) {
        return deduce_raise(interp, "deduce/materialize-out-of-memory",
            "deduce/materialize!: failed to intern materialized refresh policy");
    }
    SymbolId materialized_policy =
        schema.materialized_view && schema.materialized_refresh_policy != (SymbolId)0
            ? schema.materialized_refresh_policy
            : manual_policy;
    Value* meta_err = deduce_relation_meta_persist_state(
        rel.db,
        rel.name,
        true,
        materialized_policy,
        schema.materialized_refresh_count,
        schema.materialized_last_refresh_mutation_epoch,
        schema.materialized_last_stale_mutation_epoch,
        schema.materialized_last_stale_reason,
        false,
        interp
    );
    if (meta_err != null) return meta_err;
    schema.materialized_view = true;
    schema.materialized_refresh_policy = materialized_policy;
    rel.materialized_view = true;
    rel.materialized_refresh_policy = materialized_policy;
    rel.materialized_refresh_count = schema.materialized_refresh_count;
    rel.materialized_last_refresh_mutation_epoch = schema.materialized_last_refresh_mutation_epoch;
    rel.materialized_last_stale_mutation_epoch = schema.materialized_last_stale_mutation_epoch;
    rel.materialized_last_stale_reason = schema.materialized_last_stale_reason;
    return make_void(interp);
}
