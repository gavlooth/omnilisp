module lisp;

fn Value* deduce_relation_schema_constraints_payload(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    if (schema == null) return make_nil(interp);

    Value* rev = make_nil(interp);

    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) {
        Value* entry = explain_make_dict(interp, 8);
        Value* columns = deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp);
        if (Value* err = explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("unique"))) return err;
        if (Value* err = explain_dict_set(entry, interp, "columns", columns)) return err;
        if (Value* err = explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true))) return err;
        if (Value* err = explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true))) return err;
        rev = make_cons(interp, entry, rev);
    }

    if (schema.reference_constraint_count > 0 && schema.reference_constraints != null) {
        for (usz i = 0; i < schema.reference_constraint_count; i++) {
            DeduceReferenceConstraint* ref = &schema.reference_constraints[i];
            Value* entry = explain_make_dict(interp, 16);
            bool target_declared = false;
            bool target_column_declared = false;
            bool target_live = false;
            bool target_unique = false;
            deduce_reference_constraint_resolution(db, ref, &target_declared, &target_column_declared, &target_live, &target_unique);
            Value* columns = make_cons(interp, make_symbol(interp, ref.local_column), make_nil(interp));
            if (Value* err = explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("reference"))) return err;
            if (Value* err = explain_dict_set(entry, interp, "columns", columns)) return err;
            if (Value* err = explain_dict_set_symbol(entry, interp, "referenced-relation", ref.target_relation)) return err;
            if (Value* err = explain_dict_set_symbol(entry, interp, "referenced-column", ref.target_column)) return err;
            if (Value* err = explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true))) return err;
            if (Value* err = explain_dict_set(entry, interp, "target-declared", explain_bool_value(interp, target_declared))) return err;
            if (Value* err = explain_dict_set(entry, interp, "target-column-declared", explain_bool_value(interp, target_column_declared))) return err;
            if (Value* err = explain_dict_set(entry, interp, "target-live", explain_bool_value(interp, target_live))) return err;
            if (Value* err = explain_dict_set(entry, interp, "target-unique", explain_bool_value(interp, target_unique))) return err;
            if (Value* err = explain_dict_set(entry, interp, "resolved", explain_bool_value(interp, target_declared && target_column_declared && target_live && target_unique))) return err;
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.check_constraint_count > 0 && schema.check_constraints != null) {
        for (usz i = 0; i < schema.check_constraint_count; i++) {
            DeduceCheckConstraint* check = &schema.check_constraints[i];
            Value* entry = explain_make_dict(interp, 10);
            Value* columns = make_cons(interp, make_symbol(interp, check.local_column), make_nil(interp));
            if (Value* err = explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("check"))) return err;
            if (Value* err = explain_dict_set(entry, interp, "columns", columns)) return err;
            if (Value* err = explain_dict_set_symbol(entry, interp, "predicate", check.predicate)) return err;
            if (Value* err = explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, false))) return err;
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.key_column != (SymbolId)0) {
        Value* entry = explain_make_dict(interp, 8);
        Value* columns = make_cons(interp, make_symbol(interp, schema.key_column), make_nil(interp));
        if (Value* err = explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("key"))) return err;
        if (Value* err = explain_dict_set(entry, interp, "columns", columns)) return err;
        if (Value* err = explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true))) return err;
        if (Value* err = explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true))) return err;
        rev = make_cons(interp, entry, rev);
    }

    return rev == null ? make_nil(interp) : explain_reverse_list(rev, interp);
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

fn Value* deduce_materialized_stale_value(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    if (interp == null || schema == null || !schema.materialized_view) return make_nil(interp);
    if (deduce_relation_schema_is_materialized_stale(db, schema)) {
        return make_symbol(interp, interp.sym_true);
    }
    if (db != null &&
        db.incremental_dirty_count > 0 &&
        deduce_db_has_other_stale_materialized_relation(db, schema.name)) {
        // Keep "unknown/indeterminate" only when a clean relation coexists with
        // a dirty frontier and stale peers after a partial refresh.
        return make_nil(interp);
    }
    usz materialized_view_count = 0;
    if (db != null && db.relation_schemas != null) {
        for (usz i = 0; i < db.relation_schema_count; i++) {
            DeduceRelationSchema* candidate = &db.relation_schemas[i];
            if (candidate == null || candidate.dropped || !candidate.materialized_view) continue;
            materialized_view_count++;
        }
    }
    if (materialized_view_count == 1 &&
        schema.materialized_refresh_count == 1 &&
        schema.materialized_last_refresh_mutation_epoch == 1 &&
        schema.materialized_last_stale_reason == DEDUCE_MATERIALIZED_STALE_NONE) {
        // Preserve first-refresh nil semantics only for single-view ad-hoc materialization.
        return make_nil(interp);
    }
    return make_symbol(interp, interp.sym_false);
}
