module lisp;

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
    bool applied_incremental = false;
    Value* copy_err = deduce_refresh_apply_incremental_copy_pending_insertions(
        db,
        rel,
        target_schema,
        &source_predicate,
        &applied_incremental,
        interp
    );
    if (copy_err != null) return copy_err;
    if (used_incremental_out != null) {
        *used_incremental_out = applied_incremental && !db.incremental_full_recompute_required;
    }
    return null;
}
