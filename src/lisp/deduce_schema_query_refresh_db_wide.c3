module lisp;

import std::core::mem;

fn Value* deduce_refresh_db_wide(
    DeduceRefreshRequest* refresh,
    Env* env,
    Interp* interp
) {
    if (refresh == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid refresh state");
    }

    DeduceDb* db = refresh.db;
    Relation* rel = refresh.rel;
    DeduceRelationSchema* target_schema = refresh.target_schema;
    bool relation_scope = refresh.relation_scope;
    bool[] stale_before = mem::new_array(bool, db != null ? db.relation_schema_count : 0);
    if (stale_before.ptr == null && db != null && db.relation_schema_count > 0) {
        return deduce_raise(interp, "deduce/refresh-out-of-memory",
            "deduce/refresh!: out of memory");
    }
    defer if (stale_before.ptr != null) mem::free(stale_before.ptr);
    if (db != null) {
        for (usz i = 0; i < db.relation_schema_count; i++) {
            stale_before[i] = deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i]) &&
                deduce_relation_schema_is_materialized_stale(db, &db.relation_schemas[i]);
        }
    }

    Value* cleared_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;

    Value* analyze_handle = refresh.handle;
    if (relation_scope) {
        analyze_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false);
        if (analyze_handle == null) {
            return deduce_raise(interp, "deduce/refresh-out-of-memory",
                "deduce/refresh!: out of memory");
        }
    }

    Value*[1] analyze_args = { analyze_handle };
    // prim_deduce_analyze does not require an environment pointer.
    Value* result = prim_deduce_analyze(analyze_args[..], env, interp);
    if (result == null || result.tag == ERROR) return result;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i])) continue;
        deduce_relation_schema_note_materialized_refresh(db, &db.relation_schemas[i]);
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            db.relation_schemas[i].name,
            db.relation_schemas[i].materialized_view,
            db.relation_schemas[i].materialized_refresh_policy,
            db.relation_schemas[i].materialized_refresh_count,
            db.relation_schemas[i].materialized_last_refresh_mutation_epoch,
            db.relation_schemas[i].materialized_last_stale_mutation_epoch,
            db.relation_schemas[i].materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) return meta_err;
    }

    long remaining_stale_materialized_view_count = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!db.relation_schemas[i].materialized_view) continue;
        if (deduce_relation_schema_is_materialized_stale(db, &db.relation_schemas[i])) {
            remaining_stale_materialized_view_count++;
        }
    }
    Value* remaining_stale_relations = deduce_refresh_stale_materialized_relations(db, false, interp);
    if (remaining_stale_relations != null && remaining_stale_relations.tag == ERROR) return remaining_stale_relations;
    Value* remaining_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
    if (remaining_dirty_predicates != null && remaining_dirty_predicates.tag == ERROR) return remaining_dirty_predicates;

    long refreshed_materialized_view_count = 0;
    Value* refreshed_rev = make_nil(interp);
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (stale_before.ptr == null || !stale_before[i]) continue;
        if (deduce_relation_schema_is_materialized_stale(db, &db.relation_schemas[i])) continue;
        refreshed_materialized_view_count++;
        refreshed_rev = make_cons(interp, make_symbol(interp, db.relation_schemas[i].name), refreshed_rev);
        if (refreshed_rev != null && refreshed_rev.tag == ERROR) return refreshed_rev;
    }
    Value* refreshed_relations = deduce_explain_reverse_list(refreshed_rev, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;

    if (!relation_scope) {
        if (result.tag == HASHMAP && result.hashmap_val != null) {
            if (Value* err = explain_dict_set_symbol(result, interp, "kind", interp.symbols.intern("deduce-refresh"))) {
                return err;
            }
            if (Value* err = explain_dict_set_symbol(result, interp, "refresh-policy", interp.symbols.intern("manual"))) {
                return err;
            }
            if (Value* err = explain_dict_set_symbol(result, interp, "refresh-scope", interp.symbols.intern("database"))) {
                return err;
            }
            if (Value* err = explain_dict_set_symbol(result, interp, "refresh-execution-path", interp.symbols.intern("db-wide"))) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "refresh-fallback-reason", make_nil(interp))) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count))) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "refreshed-materialized-view-count", make_int(interp, refreshed_materialized_view_count))) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "refreshed-materialized-relations", refreshed_relations)) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "cleared-dirty-predicates", cleared_dirty_predicates)) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "remaining-dirty-predicates", remaining_dirty_predicates)) {
                return err;
            }
            if (Value* err = explain_dict_set(result, interp, "remaining-stale-materialized-relations", remaining_stale_relations)) {
                return err;
            }
        }
        return result;
    }
    Value* out = deduce_relation_schema_payload(rel, target_schema, interp);
    if (out == null || out.tag == ERROR || out.tag != HASHMAP || out.hashmap_val == null) return out;
    if (Value* err = explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-refresh"))) {
        return err;
    }
    if (Value* err = explain_dict_set_symbol(
            out,
            interp,
            "refresh-policy",
            deduce_materialized_refresh_policy_value(target_schema, interp)
        )) {
        return err;
    }
    if (Value* err = explain_dict_set_symbol(out, interp, "refresh-scope", interp.symbols.intern("relation"))) {
        return err;
    }
    if (Value* err = explain_dict_set_symbol(out, interp, "refresh-execution-path", interp.symbols.intern("db-wide"))) {
        return err;
    }
    if (Value* err = explain_dict_set_symbol(out, interp, "refresh-fallback-reason", interp.symbols.intern("full-recompute-required"))) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "requested-refresh-relation", make_symbol(interp, rel.name))) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "refreshed-relation", make_symbol(interp, rel.name))) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, refreshed_materialized_view_count))) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count))) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "refreshed-materialized-relations", refreshed_relations)) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "cleared-dirty-predicates", cleared_dirty_predicates)) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "remaining-dirty-predicates", remaining_dirty_predicates)) {
        return err;
    }
    if (Value* err = explain_dict_set(out, interp, "remaining-stale-materialized-relations", remaining_stale_relations)) {
        return err;
    }
    return out;
}
