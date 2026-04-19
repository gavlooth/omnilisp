module lisp;

fn Value* prim_deduce_refresh(Value*[] args, Env* env, Interp* interp) {
    DeduceRefreshRequest refresh = {};
    Value* prepare_err = deduce_refresh_prepare_request(args, &refresh, interp);
    if (prepare_err != null) return prepare_err;
    DeduceDb* db = refresh.db;
    Relation* rel = refresh.rel;
    DeduceRelationSchema* target_schema = refresh.target_schema;
    bool relation_scope = refresh.relation_scope;
    bool target_was_stale = refresh.target_was_stale;
    Value* handle = refresh.handle;
    long stale_materialized_view_count = refresh.stale_materialized_view_count;
    long refreshable_stale_materialized_view_count = refresh.refreshable_stale_materialized_view_count;

    if (relation_scope && !db.incremental_full_recompute_required) {
        long refreshed_stale_count = 0;
        Value* refreshed_relations = make_nil(interp);
        Value* cleared_dirty_predicates = make_nil(interp);
        bool used_incremental = false;
        Value* refresh_err = deduce_refresh_materialized_relation_targeted(
            db,
            rel,
            target_schema,
            env,
            interp,
            &refreshed_stale_count,
            &refreshed_relations,
            &cleared_dirty_predicates,
            &used_incremental
        );
        if (refresh_err != null) return refresh_err;
        if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
        Value* remaining_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
        if (remaining_dirty_predicates != null && remaining_dirty_predicates.tag == ERROR) return remaining_dirty_predicates;
        Value* remaining_stale_relations = deduce_refresh_stale_materialized_relations(db, false, interp);
        if (remaining_stale_relations != null && remaining_stale_relations.tag == ERROR) return remaining_stale_relations;

        Value* out = deduce_relation_schema_payload(rel, target_schema, interp);
        if (out == null || out.tag == ERROR || out.tag != HASHMAP || out.hashmap_val == null) return out;
        explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-refresh"));
        explain_dict_set_symbol(
            out,
            interp,
            "refresh-policy",
            deduce_materialized_refresh_policy_value(target_schema, interp)
        );
        explain_dict_set_symbol(out, interp, "refresh-scope", interp.symbols.intern("relation"));
        explain_dict_set_symbol(
            out,
            interp,
            "refresh-execution-path",
            interp.symbols.intern(used_incremental ? "incremental-targeted" : "targeted")
        );
        explain_dict_set(out, interp, "refresh-fallback-reason", make_nil(interp));
        explain_dict_set(out, interp, "requested-refresh-relation", make_symbol(interp, rel.name));
        explain_dict_set(out, interp, "refreshed-relation", make_symbol(interp, rel.name));
        explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, refreshed_stale_count));
        explain_dict_set(out, interp, "refreshed-materialized-relations", refreshed_relations);
        explain_dict_set(out, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
        explain_dict_set(out, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
        explain_dict_set(out, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
        // Refresh payload should report the target's stale state explicitly.
        explain_dict_set(
            out,
            interp,
            "materialized-stale",
            deduce_relation_schema_is_materialized_stale(db, target_schema)
                ? make_symbol(interp, interp.sym_true)
                : make_symbol(interp, interp.sym_false)
        );
        return out;
    }
    return deduce_refresh_db_wide(&refresh, env, interp);
}
