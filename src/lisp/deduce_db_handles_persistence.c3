module lisp;

fn Value* deduce_db_ensure_restored_rule_predicate_schema(
    DeduceDb* db,
    SymbolId predicate,
    usz arity,
    Interp* interp
) {
    if (db == null || predicate == (SymbolId)0 || interp == null) return null;

    DeduceRelationSchema* existing = deduce_db_lookup_relation_schema(db, predicate);
    if (existing != null) {
        if (existing.arity != arity) {
            return deduce_raise(interp, "deduce/rule-signature-restore-schema-conflict",
                "deduce/open: restored rule signature predicate arity conflicts with registered relation schema");
        }
        return null;
    }

    bool materialized_view = false;
    SymbolId materialized_refresh_policy = (SymbolId)0;
    ulong materialized_refresh_count = 0;
    ulong materialized_last_refresh_mutation_epoch = 0;
    ulong materialized_last_stale_mutation_epoch = 0;
    DeduceMaterializedStaleReason materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    (void)deduce_relation_meta_apply_persisted_state(
        db,
        predicate,
        &materialized_view,
        &materialized_refresh_policy,
        &materialized_refresh_count,
        &materialized_last_refresh_mutation_epoch,
        &materialized_last_stale_mutation_epoch,
        &materialized_last_stale_reason,
        interp
    );

    DeduceSchemaRegisterStatus status = deduce_db_register_inferred_relation_schema(
        db,
        predicate,
        arity,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason,
        interp
    );
    if (status == DEDUCE_SCHEMA_REGISTER_OK) return null;
    if (status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
        return deduce_raise(interp, "deduce/rule-signature-restore-schema-conflict",
            "deduce/open: restored rule signature predicate arity conflicts with registered relation schema");
    }
    return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
        "deduce/open: restored rule signature schema registration ran out of memory");
}

fn Value* deduce_db_ensure_restored_rule_signature_schemas(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (db == null || sig == null || interp == null) return null;

    Value* head_err = deduce_db_ensure_restored_rule_predicate_schema(
        db,
        sig.head_predicate,
        sig.head_term_count,
        interp
    );
    if (head_err != null) return head_err;

    if (sig.body_predicates == null || sig.body_term_counts == null) return null;
    for (usz i = 0; i < sig.body_count; i++) {
        Value* body_err = deduce_db_ensure_restored_rule_predicate_schema(
            db,
            sig.body_predicates[i],
            sig.body_term_counts[i],
            interp
        );
        if (body_err != null) return body_err;
    }
    return null;
}
