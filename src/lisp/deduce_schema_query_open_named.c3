module lisp;

// (deduce 'open-named db relation-name db-name col...) -> relation handle
fn Value* prim_deduce_open_named(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 4) {
        return deduce_raise(interp, "deduce/open-named-expected-db-relation-dbname-columns", "deduce-open-named: expected (deduce 'open-named db relation-name db-name col...)");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/open-named-db-not-handle", "deduce-open-named: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/open-named-db-handle-closed", "deduce-open-named: database handle is closed");
    }
    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/open-named-db-not-open", "deduce-open-named: database is not open");
    }

    Value* rel_name_v = args[1];
    if (rel_name_v == null || rel_name_v.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/open-named-relation-name-not-symbol", "deduce-open-named: relation-name must be a symbol");
    }

    char[] db_name = {};
    Value* db_name_v = args[2];
    if (db_name_v == null) {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name_v.tag == STRING) {
        db_name = db_name_v.str_chars[:db_name_v.str_len];
    } else if (db_name_v.tag == SYMBOL) {
        db_name = interp.symbols.get_name(db_name_v.sym_val);
    } else {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name.len == 0) {
        return deduce_raise(interp, "deduce/open-named-db-name-empty", "deduce-open-named: db-name cannot be empty");
    }

    Relation* rel = deduce_relation_alloc(db, rel_name_v.sym_val);
    if (rel == null) {
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }

    Value* col_err = deduce_relation_collect_columns_from_args(rel, args, 3, interp);
    if (col_err != null) {
        deduce_relation_free(rel);
        return col_err;
    }
    (void)deduce_relation_meta_apply_persisted_state(
        db,
        rel_name_v.sym_val,
        &rel.materialized_view,
        &rel.materialized_refresh_policy,
        &rel.materialized_refresh_count,
        &rel.materialized_last_refresh_mutation_epoch,
        &rel.materialized_last_stale_mutation_epoch,
        &rel.materialized_last_stale_reason,
        interp
    );

    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi_with_name(rel, db_name);
    if (open_status != REL_OPEN_OK) {
        deduce_relation_free(rel);
        if (open_status == REL_OPEN_OUT_OF_MEMORY) {
            return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
        }
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            return deduce_raise(interp, "deduce/open-named-txn-open-failed", "deduce-open-named: txn open failed");
        }
        if (open_status == REL_OPEN_TXN_COMMIT_FAILED) {
            return deduce_raise(interp, "deduce/open-named-txn-commit-failed", "deduce-open-named: txn commit failed");
        }
        return deduce_raise(interp, "deduce/open-named-dbi-open-failed", "deduce-open-named: dbi open failed");
    }

    usz schema_start = db.relation_schema_count;
    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(
            db,
            rel_name_v.sym_val,
            rel.columns,
            rel.col_count,
            rel.key_col,
            rel.unique_cols,
            rel.unique_col_count,
            rel.reference_constraints,
            rel.reference_constraint_count,
            rel.check_constraints,
            rel.check_constraint_count,
            rel.index_col,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason
        );
    if (schema_status != DEDUCE_SCHEMA_REGISTER_OK) {
        deduce_relation_free(rel);
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "deduce-open-named: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }
    if (rel.materialized_view) {
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            rel_name_v.sym_val,
            rel.materialized_view,
            rel.materialized_refresh_policy,
            rel.materialized_refresh_count,
            rel.materialized_last_refresh_mutation_epoch,
            rel.materialized_last_stale_mutation_epoch,
            rel.materialized_last_stale_reason,
            false,
            interp
        );
        if (meta_err != null) {
            deduce_db_rollback_relation_schemas_to(db, schema_start);
            deduce_relation_free(rel);
            return meta_err;
        }
    }

    Value* rel_v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (rel_v == null) {
        deduce_db_rollback_relation_schemas_to(db, schema_start);
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }
    return rel_v;
}
