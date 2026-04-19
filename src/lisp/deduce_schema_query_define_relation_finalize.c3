module lisp;

fn Value* deduce_define_relation_finalize_open_relation(
    DeduceDb* db,
    SymbolId rel_name,
    Relation* rel,
    Interp* interp
) {
    usz schema_start = db.relation_schema_count;
    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(
            db,
            rel_name,
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
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "__define-relation: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    if (rel.materialized_view) {
        Value* meta_err = deduce_relation_meta_persist_state(
            db,
            rel_name,
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
            return meta_err;
        }
    }

    Value* v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (v == null) {
        deduce_db_rollback_relation_schemas_to(db, schema_start);
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    return v;
}
