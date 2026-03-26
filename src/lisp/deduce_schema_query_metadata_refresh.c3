module lisp;

import std::core::mem;

fn Value* deduce_relation_schema_columns_list(DeduceRelationSchema* schema, Interp* interp) {
    if (schema == null || schema.columns == null || schema.arity == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.arity; i++) {
        rev = make_cons(interp, make_symbol(interp, schema.columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_index_columns_list(
    DeduceRelationSchema* schema,
    DeduceIndexDescriptor* index,
    Interp* interp
) {
    if (schema == null || index == null || index.column_positions == null || index.column_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < index.column_count; i++) {
        usz column_pos = index.column_positions[i];
        if (column_pos >= schema.arity || schema.columns == null) {
            rev = make_cons(interp, make_nil(interp), rev);
            continue;
        }
        rev = make_cons(interp, make_symbol(interp, schema.columns[column_pos]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_schema_role_column(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_symbol(interp, column);
}

fn Value* deduce_relation_schema_role_columns(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool deduce_relation_schema_unique_equivalent_to_key(DeduceRelationSchema* schema) @inline {
    return schema != null &&
        schema.key_column != (SymbolId)0 &&
        schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == schema.key_column;
}

fn bool deduce_relation_schema_column_is_candidate_key(DeduceRelationSchema* schema, SymbolId column) @inline {
    if (schema == null || column == (SymbolId)0) return false;
    if (schema.key_column == column) return true;
    return schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == column;
}

fn long deduce_relation_schema_constraint_count(DeduceRelationSchema* schema) @inline {
    if (schema == null) return 0;
    long count = 0;
    if (schema.key_column != (SymbolId)0) count++;
    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) count++;
    count += (long)schema.reference_constraint_count;
    count += (long)schema.check_constraint_count;
    return count;
}

fn void deduce_reference_constraint_resolution(
    DeduceDb* db,
    DeduceReferenceConstraint* ref_constraint,
    bool* target_declared_out,
    bool* target_column_declared_out,
    bool* target_live_out,
    bool* target_unique_out
) {
    bool target_declared = false;
    bool target_column_declared = false;
    bool target_live = false;
    bool target_unique = false;
    if (db != null && ref_constraint != null) {
        DeduceRelationSchema* target_schema = deduce_db_lookup_relation_schema(db, ref_constraint.target_relation);
        if (target_schema != null) {
            target_declared = true;
            usz ignored = 0;
            target_column_declared =
                deduce_find_column_position(target_schema.columns, target_schema.arity, ref_constraint.target_column, &ignored);
            target_live = !target_schema.dropped;
            if (target_column_declared && target_live) {
                target_unique = deduce_relation_schema_column_is_candidate_key(target_schema, ref_constraint.target_column);
            }
        }
    }
    if (target_declared_out != null) *target_declared_out = target_declared;
    if (target_column_declared_out != null) *target_column_declared_out = target_column_declared;
    if (target_live_out != null) *target_live_out = target_live;
    if (target_unique_out != null) *target_unique_out = target_unique;
}

fn long deduce_relation_schema_resolved_reference_constraint_count(
    DeduceDb* db,
    DeduceRelationSchema* schema
) {
    if (db == null || schema == null || schema.reference_constraints == null || schema.reference_constraint_count == 0) {
        return 0;
    }
    long count = 0;
    for (usz i = 0; i < schema.reference_constraint_count; i++) {
        bool target_declared = false;
        bool target_column_declared = false;
        bool target_live = false;
        bool target_unique = false;
        deduce_reference_constraint_resolution(db, &schema.reference_constraints[i], &target_declared, &target_column_declared, &target_live, &target_unique);
        if (target_declared && target_column_declared && target_live && target_unique) count++;
    }
    return count;
}

fn Value* deduce_relation_schema_constraints_payload(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    if (schema == null) return make_nil(interp);

    Value* rev = make_nil(interp);

    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp);
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("unique"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    if (schema.reference_constraint_count > 0 && schema.reference_constraints != null) {
        for (usz i = 0; i < schema.reference_constraint_count; i++) {
            DeduceReferenceConstraint* ref = &schema.reference_constraints[i];
            Value* entry = make_hashmap(interp, 16);
            if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
                return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
            }
            bool target_declared = false;
            bool target_column_declared = false;
            bool target_live = false;
            bool target_unique = false;
            deduce_reference_constraint_resolution(db, ref, &target_declared, &target_column_declared, &target_live, &target_unique);
            Value* columns = make_cons(interp, make_symbol(interp, ref.local_column), make_nil(interp));
            explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("reference"));
            explain_dict_set(entry, interp, "columns", columns);
            explain_dict_set_symbol(entry, interp, "referenced-relation", ref.target_relation);
            explain_dict_set_symbol(entry, interp, "referenced-column", ref.target_column);
            explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
            explain_dict_set(entry, interp, "target-declared", explain_bool_value(interp, target_declared));
            explain_dict_set(entry, interp, "target-column-declared", explain_bool_value(interp, target_column_declared));
            explain_dict_set(entry, interp, "target-live", explain_bool_value(interp, target_live));
            explain_dict_set(entry, interp, "target-unique", explain_bool_value(interp, target_unique));
            explain_dict_set(entry, interp, "resolved", explain_bool_value(interp, target_declared && target_column_declared && target_live && target_unique));
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.check_constraint_count > 0 && schema.check_constraints != null) {
        for (usz i = 0; i < schema.check_constraint_count; i++) {
            DeduceCheckConstraint* check = &schema.check_constraints[i];
            Value* entry = make_hashmap(interp, 10);
            if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
                return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
            }
            Value* columns = make_cons(interp, make_symbol(interp, check.local_column), make_nil(interp));
            explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("check"));
            explain_dict_set(entry, interp, "columns", columns);
            explain_dict_set_symbol(entry, interp, "predicate", check.predicate);
            explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, false));
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.key_column != (SymbolId)0) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = make_cons(interp, make_symbol(interp, schema.key_column), make_nil(interp));
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("key"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    return rev == null ? make_nil(interp) : explain_reverse_list(rev, interp);
}

fn Value* deduce_integrity_violation_class_value(
    DeduceIntegrityViolationClass violation_class,
    Interp* interp
) {
    if (interp == null) return null;
    switch (violation_class) {
        case DEDUCE_INTEGRITY_VIOLATION_KEY:
            return make_symbol(interp, interp.symbols.intern("key"));
        case DEDUCE_INTEGRITY_VIOLATION_UNIQUE:
            return make_symbol(interp, interp.symbols.intern("unique"));
        case DEDUCE_INTEGRITY_VIOLATION_REFERENCE:
            return make_symbol(interp, interp.symbols.intern("reference"));
        case DEDUCE_INTEGRITY_VIOLATION_CHECK:
            return make_symbol(interp, interp.symbols.intern("check"));
    }
}

fn Value* deduce_integrity_violation_history_payload(
    DeduceIntegrityViolationEntry[] history,
    usz count,
    Interp* interp
) {
    if (interp == null || history.ptr == null || count == 0) return make_nil(interp);

    Value* list = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        DeduceIntegrityViolationEntry entry_data = history[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/integrity-history-out-of-memory",
                "deduce: integrity history payload out of memory");
        }
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
        explain_dict_set(entry, interp, "sequence", make_int(interp, (long)entry_data.sequence));
        explain_dict_set(entry, interp, "relation",
            entry_data.relation != (SymbolId)0
                ? make_symbol(interp, entry_data.relation)
                : make_nil(interp));
        explain_dict_set(entry, interp, "code",
            entry_data.code != (SymbolId)0
                ? make_symbol(interp, entry_data.code)
                : make_nil(interp));
        explain_dict_set(entry, interp, "violation-class",
            deduce_integrity_violation_class_value(entry_data.violation_class, interp));
        list = make_cons(interp, entry, list);
    }
    return list;
}

fn Value* deduce_bound_positions_payload(
    usz* positions,
    ulong count,
    Interp* interp
) {
    if (interp == null || positions == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < (usz)count; i++) {
        rev = make_cons(interp, make_int(interp, (long)positions[i]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_materialized_stale_reason_value(
    DeduceMaterializedStaleReason reason,
    Interp* interp
) {
    if (interp == null) return null;
    switch (reason) {
        case DEDUCE_MATERIALIZED_STALE_NONE:
            return make_nil(interp);
        case DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED:
            return make_symbol(interp, interp.symbols.intern("never-refreshed"));
        case DEDUCE_MATERIALIZED_STALE_RULE_SET_CHANGE:
            return make_symbol(interp, interp.symbols.intern("rule-set-change"));
        case DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY:
            return make_symbol(interp, interp.symbols.intern("dependency-dirty"));
        case DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED:
            return make_symbol(interp, interp.symbols.intern("full-recompute-required"));
    }
}

fn Value* deduce_relation_schema_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    Value* out = make_hashmap(interp, 32);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
    }

    long resolved_reference_constraint_count =
        deduce_relation_schema_resolved_reference_constraint_count(rel != null ? rel.db : null, schema);
    long unresolved_reference_constraint_count =
        schema != null ? (long)schema.reference_constraint_count - resolved_reference_constraint_count : 0;

    Value* constraints = deduce_relation_schema_constraints_payload(rel != null ? rel.db : null, schema, interp);
    if (constraints != null && constraints.tag == ERROR) return constraints;
    DeduceMaterializedStaleReason effective_stale_reason =
        deduce_relation_schema_effective_materialized_stale_reason(schema);

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-schema"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "columns", deduce_relation_schema_columns_list(schema, interp));
    explain_dict_set(out, interp, "arity", make_int(interp, (long)schema.arity));
    explain_dict_set(out, interp, "key-column", deduce_relation_schema_role_column(schema.key_column, interp));
    explain_dict_set(out, interp, "unique-column", deduce_relation_schema_role_column(schema.unique_column, interp));
    explain_dict_set(out, interp, "unique-columns", deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp));
    explain_dict_set(out, interp, "reference-constraint-count", make_int(interp, (long)schema.reference_constraint_count));
    explain_dict_set(out, interp, "check-constraint-count", make_int(interp, (long)schema.check_constraint_count));
    explain_dict_set(out, interp, "resolved-reference-constraint-count", make_int(interp, resolved_reference_constraint_count));
    explain_dict_set(out, interp, "unresolved-reference-constraint-count", make_int(interp, unresolved_reference_constraint_count));
    explain_dict_set(out, interp, "index-column", deduce_relation_schema_role_column(schema.index_column, interp));
    explain_dict_set(out, interp, "index-count", make_int(interp, (long)schema.index_count));
    explain_dict_set(out, interp, "constraint-count", make_int(interp, deduce_relation_schema_constraint_count(schema)));
    explain_dict_set(out, interp, "constraints", constraints);
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "materialized-view", explain_bool_value(interp, schema.materialized_view));
    explain_dict_set(out, interp, "materialized-derived-ready",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_ready(rel != null ? rel.db : null, schema)));
    explain_dict_set(out, interp, "materialized-refresh-policy",
        schema.materialized_refresh_policy != (SymbolId)0
            ? make_symbol(interp, schema.materialized_refresh_policy)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-refresh-count", make_int(interp, (long)schema.materialized_refresh_count));
    explain_dict_set(out, interp, "materialized-last-refresh-mutation-epoch",
        schema.materialized_refresh_count > 0
            ? make_int(interp, (long)schema.materialized_last_refresh_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-mutation-epoch",
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE &&
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED
            ? make_int(interp, (long)schema.materialized_last_stale_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-reason",
        deduce_materialized_stale_reason_value(effective_stale_reason, interp));
    explain_dict_set(out, interp, "materialized-stale",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_stale(rel != null ? rel.db : null, schema)));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    explain_dict_set(out, interp, "database-open", explain_bool_value(interp, rel.db != null && rel.db.open));
    return out;
}

fn Value* deduce_relation_indexes_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    (void)rel;
    if (schema == null || schema.indexes == null || schema.index_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* index = &schema.indexes[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/indexes-out-of-memory", "deduce/indexes: out of memory");
        }
        explain_dict_set(entry, interp, "index-id", make_int(interp, (long)i));
        explain_dict_set(entry, interp, "columns", deduce_relation_index_columns_list(schema, index, interp));
        explain_dict_set(entry, interp, "column-count", make_int(interp, (long)index.column_count));
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, index.unique));
        rev = make_cons(interp, entry, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* prim_deduce_schema(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/schema-expected-relation", "deduce/schema: expected (deduce/schema relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/schema-relation-not-handle", "deduce/schema: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/schema-relation-handle-closed", "deduce/schema: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/schema-relation-not-deduce-relation", "deduce/schema: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/schema-database-not-open", "deduce/schema: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/schema-relation-not-found", "deduce/schema: relation schema is not registered");
    }

    return deduce_relation_schema_payload(rel, schema, interp);
}

fn Value* prim_deduce_indexes(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/indexes-expected-relation", "deduce/indexes: expected (deduce/indexes relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/indexes-relation-not-handle", "deduce/indexes: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/indexes-relation-handle-closed", "deduce/indexes: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/indexes-relation-not-deduce-relation", "deduce/indexes: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/indexes-database-not-open", "deduce/indexes: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/indexes-relation-not-found", "deduce/indexes: relation schema is not registered");
    }

    return deduce_relation_indexes_payload(rel, schema, interp);
}

fn Value* deduce_parallel_batch_topology_counts(
    DeduceDb* db,
    long* recursive_component_count_out,
    long* batched_component_count_out,
    long* batch_count_out,
    long* max_batch_size_out,
    Interp* interp
) {
    if (recursive_component_count_out != null) *recursive_component_count_out = 0;
    if (batched_component_count_out != null) *batched_component_count_out = 0;
    if (batch_count_out != null) *batch_count_out = 0;
    if (max_batch_size_out != null) *max_batch_size_out = 0;
    if (db == null || interp == null) return null;

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_err != null) return parallel_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

    long recursive_component_count = 0;
    long batched_component_count = 0;
    long batch_count = 0;
    long max_batch_size = 0;
    for (usz component_id = 0; component_id < scc_plan.component_count; component_id++) {
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[component_id];
        if (!recursive) continue;

        recursive_component_count++;

        long batch_size =
            component_id < parallel_batch_sizes.len ? (long)parallel_batch_sizes[component_id] : 0;
        if (batch_size > 1) batched_component_count++;
        if (batch_size > max_batch_size) max_batch_size = batch_size;

        bool batch_seen = false;
        if (component_id < parallel_wave_ids.len && component_id < scc_plan.component_strata.len) {
            usz stratum = scc_plan.component_strata[component_id];
            usz wave = parallel_wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    scc_plan.component_recursive.ptr != null &&
                    prev < scc_plan.component_recursive.len &&
                    scc_plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= parallel_wave_ids.len || prev >= scc_plan.component_strata.len) continue;
                if (scc_plan.component_strata[prev] != stratum) continue;
                if (parallel_wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (!batch_seen) batch_count++;
    }

    if (recursive_component_count_out != null) *recursive_component_count_out = recursive_component_count;
    if (batched_component_count_out != null) *batched_component_count_out = batched_component_count;
    if (batch_count_out != null) *batch_count_out = batch_count;
    if (max_batch_size_out != null) *max_batch_size_out = max_batch_size;
    return null;
}

fn Value* prim_deduce_stats(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/stats-expected-relation", "deduce/stats: expected (deduce/stats relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/stats-relation-not-handle", "deduce/stats: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/stats-relation-handle-closed", "deduce/stats: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/stats-relation-not-deduce-relation", "deduce/stats: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/stats-database-not-open", "deduce/stats: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/stats-relation-not-found", "deduce/stats: relation schema is not registered");
    }

    Value* out = make_hashmap(interp, 32);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/stats-out-of-memory", "deduce/stats: out of memory");
    }

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-stats"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "dirty-predicate-count", make_int(interp, (long)rel.db.incremental_dirty_count));
    Value* dirty_predicates = deduce_dirty_predicates_payload(rel.db, interp);
    if (dirty_predicates != null && dirty_predicates.tag == ERROR) return dirty_predicates;
    explain_dict_set(out, interp, "dirty-predicates", dirty_predicates);
    explain_dict_set(out, interp, "dirty-self",
        explain_bool_value(interp, deduce_db_is_incremental_dirty_predicate(rel.db, rel.name)));
    explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)rel.db.incremental_mutation_epoch));
    explain_dict_set(out, interp, "full-recompute-required", explain_bool_value(interp, rel.db.incremental_full_recompute_required));
    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(rel.db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);
    long parallel_recursive_component_count = 0;
    long parallel_batched_component_count = 0;
    long parallel_batch_count = 0;
    long parallel_max_batch_size = 0;
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_meta_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_meta_err != null) return parallel_meta_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);
    Value* parallel_err = deduce_parallel_batch_topology_counts(
        rel.db,
        &parallel_recursive_component_count,
        &parallel_batched_component_count,
        &parallel_batch_count,
        &parallel_max_batch_size,
        interp
    );
    if (parallel_err != null) return parallel_err;
    explain_dict_set(out, interp, "parallel-recursive-component-count",
        make_int(interp, parallel_recursive_component_count));
    explain_dict_set(out, interp, "parallel-batched-component-count",
        make_int(interp, parallel_batched_component_count));
    explain_dict_set(out, interp, "parallel-batch-count",
        make_int(interp, parallel_batch_count));
    explain_dict_set(out, interp, "parallel-max-batch-size",
        make_int(interp, parallel_max_batch_size));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            &scc_plan,
            parallel_wave_ids[..],
            parallel_batch_sizes[..],
            interp
        ));
    explain_dict_set(out, interp, "integrity-violation-count", make_int(interp, (long)schema.integrity_violation_count));
    explain_dict_set(out, interp, "key-integrity-violation-count", make_int(interp, (long)schema.key_integrity_violation_count));
    explain_dict_set(out, interp, "unique-integrity-violation-count", make_int(interp, (long)schema.unique_integrity_violation_count));
    explain_dict_set(out, interp, "reference-integrity-violation-count", make_int(interp, (long)schema.reference_integrity_violation_count));
    explain_dict_set(out, interp, "check-integrity-violation-count", make_int(interp, (long)schema.check_integrity_violation_count));
    explain_dict_set(out, interp, "last-integrity-violation-code",
        schema.last_integrity_violation_code != (SymbolId)0
            ? make_symbol(interp, schema.last_integrity_violation_code)
            : make_nil(interp));
    Value* recent_integrity_violations = deduce_integrity_violation_history_payload(
        schema.integrity_violation_history[..],
        schema.integrity_violation_history_count,
        interp
    );
    if (recent_integrity_violations != null && recent_integrity_violations.tag == ERROR) {
        return recent_integrity_violations;
    }
    DeduceMaterializedStaleReason effective_stale_reason =
        deduce_relation_schema_effective_materialized_stale_reason(schema);
    explain_dict_set(out, interp, "recent-integrity-violations", recent_integrity_violations);
    explain_dict_set(out, interp, "materialized-view", explain_bool_value(interp, schema.materialized_view));
    explain_dict_set(out, interp, "materialized-derived-ready",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_ready(rel.db, schema)));
    explain_dict_set(out, interp, "materialized-refresh-policy",
        schema.materialized_refresh_policy != (SymbolId)0
            ? make_symbol(interp, schema.materialized_refresh_policy)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-refresh-count", make_int(interp, (long)schema.materialized_refresh_count));
    explain_dict_set(out, interp, "materialized-last-refresh-mutation-epoch",
        schema.materialized_refresh_count > 0
            ? make_int(interp, (long)schema.materialized_last_refresh_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-mutation-epoch",
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NONE &&
        effective_stale_reason != DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED
            ? make_int(interp, (long)schema.materialized_last_stale_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "materialized-last-stale-reason",
        deduce_materialized_stale_reason_value(effective_stale_reason, interp));
    explain_dict_set(out, interp, "materialized-stale",
        explain_bool_value(interp, deduce_relation_schema_is_materialized_stale(rel.db, schema)));
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_requested_bound_positions,
                schema.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        schema.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_applied_bound_positions,
                schema.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-step-counters",
        schema.last_goal_directed_read_execution_path != (SymbolId)0 &&
        schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    return out;
}

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

