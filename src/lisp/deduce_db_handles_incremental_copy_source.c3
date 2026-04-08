module lisp;

fn bool deduce_rule_signature_is_incremental_direct_copy_class(
    DeduceRuleSignature* sig,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig == null || sig.body_count != 1 || deduce_rule_signature_has_aggregates(sig)) return false;
    if (sig.body_predicates == null || sig.body_predicates[0] == (SymbolId)0) return false;
    if (sig.body_negated != null && sig.body_negated[0]) return false;
    if (sig.head_term_count == 0 || sig.head_term_kinds == null || sig.head_term_var_ids == null) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) return false;

    usz body_offset = sig.body_term_offsets[0];
    usz body_count = sig.body_term_counts[0];
    if (body_count != sig.head_term_count) return false;
    if (body_offset + body_count > sig.body_term_total_count) return false;

    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] != DEDUCE_RULE_TERM_VARIABLE) return false;
        usz body_term_index = body_offset + i;
        if (!sig.body_term_is_variable[body_term_index]) return false;
        if (sig.head_term_var_ids[i] != sig.body_term_var_ids[body_term_index]) return false;
    }

    if (source_predicate_out != null) *source_predicate_out = sig.body_predicates[0];
    return true;
}

fn bool deduce_relation_schema_incremental_extensional_copy_source(
    DeduceDb* db,
    DeduceRelationSchema* target_schema,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (db == null || target_schema == null || target_schema.name == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    SymbolId source_predicate = (SymbolId)0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != target_schema.name) continue;

        SymbolId candidate_source = (SymbolId)0;
        if (!deduce_rule_signature_is_incremental_direct_copy_class(sig, &candidate_source)) {
            return false;
        }
        if (matched != null) return false;
        matched = sig;
        source_predicate = candidate_source;
    }
    if (matched == null || source_predicate == (SymbolId)0) return false;

    DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
    if (source_schema == null || deduce_relation_schema_has_derived_rules(db, source_schema)) return false;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    return true;
}

fn bool deduce_relation_schema_incremental_derived_copy_source(
    DeduceDb* db,
    DeduceRelationSchema* target_schema,
    SymbolId* source_predicate_out
) {
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (db == null || target_schema == null || target_schema.name == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    SymbolId source_predicate = (SymbolId)0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != target_schema.name) continue;

        SymbolId candidate_source = (SymbolId)0;
        if (!deduce_rule_signature_is_incremental_direct_copy_class(sig, &candidate_source)) {
            return false;
        }
        if (matched != null) return false;
        matched = sig;
        source_predicate = candidate_source;
    }
    if (matched == null || source_predicate == (SymbolId)0) return false;

    DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, source_predicate);
    if (source_schema == null || !source_schema.materialized_view ||
        !deduce_relation_schema_is_materialized_ready(db, source_schema)) {
        return false;
    }
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    return true;
}
