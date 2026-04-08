module lisp;

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
