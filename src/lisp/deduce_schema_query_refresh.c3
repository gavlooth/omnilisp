module lisp;

import std::core::mem;

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

fn Value* deduce_refresh_try_incremental_extensional_copy_relation(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    bool* used_incremental_out,
    Interp* interp
) {
    if (used_incremental_out != null) *used_incremental_out = false;
    if (db == null || rel == null || target_schema == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid incremental refresh state");
    }
    if (db.incremental_full_recompute_required) return null;
    if (target_schema.materialized_refresh_count == 0) return null;
    if (target_schema.materialized_last_stale_reason != DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY) return null;
    if (target_schema.maintained_pending_insertions.count == 0) return null;

    SymbolId source_predicate = (SymbolId)0;
    if (!deduce_relation_schema_incremental_extensional_copy_source(db, target_schema, &source_predicate) ||
        source_predicate == (SymbolId)0) {
        source_predicate = (SymbolId)0;
        if (!deduce_relation_schema_incremental_derived_copy_source(db, target_schema, &source_predicate) ||
            source_predicate == (SymbolId)0) {
            return null;
        }
        DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
        if (source_schema == null || deduce_relation_schema_is_materialized_stale(db, source_schema)) {
            return null;
        }
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    MdbDbi target_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, rel.name, &target_dbi, &head_schema, interp) || head_schema == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: materialized relation storage is unavailable for incremental refresh");
    }

    for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
        DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
        if (tuple == null || (tuple.key_bytes == null && tuple.key_len > 0)) {
            return deduce_raise(interp, "deduce/refresh-invalid-state",
                "deduce/refresh!: invalid pending incremental tuple state");
        }

        Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
            db,
            head_schema,
            txn,
            target_dbi,
            tuple.key_bytes,
            tuple.key_len,
            interp
        );
        if (integrity_err != null) return integrity_err;

        char empty_key = 0;
        MdbVal key = {
            .mv_size = tuple.key_len,
            .mv_data = tuple.key_len > 0 ? (void*)tuple.key_bytes : &empty_key
        };
        MdbVal existing = {};
        if (mdb_get(txn, target_dbi, &key, &existing) == MDB_SUCCESS) continue;

        MdbVal data = {};
        char empty = 0;
        data.mv_size = 0;
        data.mv_data = &empty;

        rc = mdb_put(txn, target_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/refresh-put-failed",
                "deduce/refresh!: failed to insert incrementally maintained tuple");
        }

        if (head_schema.cardinality_estimate < ulong.max) {
            head_schema.cardinality_estimate++;
            if (head_schema.distinct_estimate < head_schema.cardinality_estimate) {
                head_schema.distinct_estimate = head_schema.cardinality_estimate;
            }
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/refresh!: txn commit failed");
    }
    txn = null;

    bool queued_downstream = true;
    if (target_schema.maintained_pending_insertions.count > 0) {
        for (usz i = 0; i < target_schema.maintained_pending_insertions.count; i++) {
            DeduceEncodedTuple* tuple = &target_schema.maintained_pending_insertions.tuples[i];
            if (tuple == null) continue;
            if (!deduce_db_queue_supported_incremental_insert_from_materialized_source(
                    db,
                    rel.name,
                    tuple.key_bytes,
                    tuple.key_len
                )) {
                queued_downstream = false;
                break;
            }
        }
    }
    deduce_relation_schema_clear_pending_incremental_insertions(target_schema);
    if (!queued_downstream) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
        Value* persist_err = deduce_db_persist_materialized_view_states(db, interp);
        if (persist_err != null) return persist_err;
    }
    if (used_incremental_out != null) *used_incremental_out = true;
    return null;
}

fn Value* deduce_refresh_materialized_relation_targeted(
    DeduceDb* db,
    Relation* rel,
    DeduceRelationSchema* target_schema,
    Env* env,
    Interp* interp,
    long* refreshed_stale_count_out,
    Value** refreshed_relations_out,
    Value** cleared_dirty_predicates_out,
    bool* used_incremental_out
) {
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = 0;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = make_nil(interp);
    if (used_incremental_out != null) *used_incremental_out = false;
    if (db == null || rel == null || target_schema == null || interp == null) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid relation-scoped refresh state");
    }
    if (!deduce_relation_schema_is_materialized_ready(db, target_schema)) {
        return deduce_raise(interp, "deduce/refresh-materialized-relation-not-derived",
            "deduce/refresh!: materialized relation has no derived rule heads to refresh");
    }

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    int predicate_index = deduce_scc_find_predicate_index(scc_plan.predicates, rel.name);
    if (predicate_index < 0 || (usz)predicate_index >= scc_plan.predicate_component.len) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: derived materialized relation is missing from the dependency plan");
    }

    int target_component = scc_plan.predicate_component[(usz)predicate_index];
    if (target_component < 0 || (usz)target_component >= scc_plan.component_count) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: invalid target component for materialized relation");
    }

    bool[] selected_components = mem::new_array(bool, scc_plan.component_count);
    if (selected_components.ptr == null && scc_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/refresh-out-of-memory", "deduce/refresh!: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_refresh_select_dependency_components(&scc_plan, (usz)target_component, selected_components)) {
        return deduce_raise(interp, "deduce/refresh-invalid-state",
            "deduce/refresh!: failed to select refresh dependency closure");
    }

    bool used_incremental = false;
    Value* incremental_err = deduce_refresh_try_incremental_extensional_copy_relation(
        db,
        rel,
        target_schema,
        &used_incremental,
        interp
    );
    if (incremental_err != null) return incremental_err;

    if (!used_incremental) {
        for (usz i = 0; i < scc_plan.component_order.len; i++) {
            usz component_id = scc_plan.component_order[i];
            if (component_id >= selected_components.len || !selected_components[component_id]) continue;
            Value* err = deduce_naive_evaluate_component(
                db,
                &scc_plan,
                component_id,
                256,
                true,
                null,
                null,
                {},
                interp
            );
            if (err != null) return err;
        }
    }

    long refreshed_stale_count =
        deduce_refresh_count_selected_stale_materialized_views(db, &scc_plan, selected_components);
    Value* refreshed_relations =
        deduce_refresh_selected_stale_materialized_relations(db, &scc_plan, selected_components, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;
    Value* cleared_dirty_predicates =
        deduce_refresh_selected_dirty_predicates_payload(db, &scc_plan, selected_components, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
    deduce_refresh_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    deduce_refresh_note_selected_materialized_views(db, &scc_plan, selected_components);
    Value* persist_err = deduce_refresh_persist_selected_materialized_views(
        db,
        &scc_plan,
        selected_components,
        interp
    );
    if (persist_err != null) return persist_err;
    if (refreshed_stale_count_out != null) *refreshed_stale_count_out = refreshed_stale_count;
    if (refreshed_relations_out != null) *refreshed_relations_out = refreshed_relations;
    if (cleared_dirty_predicates_out != null) *cleared_dirty_predicates_out = cleared_dirty_predicates;
    if (used_incremental_out != null) *used_incremental_out = used_incremental;
    return null;
}

fn Value* prim_deduce_refresh(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/refresh-expected-db-or-relation",
            "deduce/refresh!: expected (deduce 'refresh! db-or-materialized-relation)");
    }

    DeduceDb* db = null;
    Relation* rel = null;
    DeduceRelationSchema* target_schema = null;
    bool relation_scope = false;
    bool target_was_stale = false;
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
        return out;
    }

    Value* cleared_dirty_predicates = deduce_dirty_predicates_payload(db, interp);
    if (cleared_dirty_predicates != null && cleared_dirty_predicates.tag == ERROR) return cleared_dirty_predicates;
    Value* refreshed_relations = deduce_refresh_stale_materialized_relations(db, true, interp);
    if (refreshed_relations != null && refreshed_relations.tag == ERROR) return refreshed_relations;

    Value* analyze_handle = args[0];
    if (relation_scope) {
        analyze_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false);
        if (analyze_handle == null) {
            return deduce_raise(interp, "deduce/refresh-out-of-memory",
                "deduce/refresh!: out of memory");
        }
    }

    Value*[1] analyze_args = { analyze_handle };
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

    if (!relation_scope) {
        if (result.tag == HASHMAP && result.hashmap_val != null) {
            explain_dict_set_symbol(result, interp, "kind", interp.symbols.intern("deduce-refresh"));
            explain_dict_set_symbol(result, interp, "refresh-policy", interp.symbols.intern("manual"));
            explain_dict_set_symbol(result, interp, "refresh-scope", interp.symbols.intern("database"));
            explain_dict_set_symbol(result, interp, "refresh-execution-path", interp.symbols.intern("db-wide"));
            explain_dict_set(result, interp, "refresh-fallback-reason", make_nil(interp));
            explain_dict_set(result, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count));
            explain_dict_set(result, interp, "refreshed-materialized-view-count", make_int(interp, refreshable_stale_materialized_view_count));
            explain_dict_set(result, interp, "refreshed-materialized-relations", refreshed_relations);
            explain_dict_set(result, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
            explain_dict_set(result, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
            explain_dict_set(result, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
        }
        return result;
    }

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
    explain_dict_set_symbol(out, interp, "refresh-execution-path", interp.symbols.intern("db-wide"));
    explain_dict_set_symbol(out, interp, "refresh-fallback-reason", interp.symbols.intern("full-recompute-required"));
    explain_dict_set(out, interp, "requested-refresh-relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "refreshed-relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, refreshable_stale_materialized_view_count));
    explain_dict_set(out, interp, "stale-materialized-view-count", make_int(interp, remaining_stale_materialized_view_count));
    explain_dict_set(out, interp, "refreshed-materialized-relations", refreshed_relations);
    explain_dict_set(out, interp, "cleared-dirty-predicates", cleared_dirty_predicates);
    explain_dict_set(out, interp, "remaining-dirty-predicates", remaining_dirty_predicates);
    explain_dict_set(out, interp, "remaining-stale-materialized-relations", remaining_stale_relations);
    return out;
}
