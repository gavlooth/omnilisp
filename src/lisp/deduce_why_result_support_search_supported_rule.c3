module lisp;

fn bool deduce_why_result_find_supported_extensional_rule(
    DeduceDb* db,
    SymbolId head_predicate,
    long* rule_index_out,
    SymbolId* source_predicate_out,
    DeduceRuleSignature** sig_out
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig_out != null) *sig_out = null;
    if (db == null || head_predicate == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    long matched_rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    long head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != head_predicate) continue;
        head_rule_count++;
        if (head_rule_count > 1) return false;
        if (sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) return false;
        if (sig.body_predicates == null) return false;
        if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
            sig.body_term_offsets == null || sig.body_term_counts == null ||
            sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
            return false;
        }
        source_predicate = sig.body_predicates[0];
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_predicates[step] == (SymbolId)0) return false;
            if (sig.body_negated != null && sig.body_negated[step]) return false;
            DeduceRelationSchema* source_schema = deduce_db_lookup_relation_schema(db, sig.body_predicates[step]);
            if (source_schema == null || source_schema.dropped ||
                deduce_relation_schema_has_derived_rules(db, source_schema)) {
                return false;
            }
        }
        matched = sig;
        matched_rule_index = (long)i;
    }

    if (matched == null || matched_rule_index < 0 || source_predicate == (SymbolId)0) return false;

    if (rule_index_out != null) *rule_index_out = matched_rule_index;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    if (sig_out != null) *sig_out = matched;
    return true;
}

fn bool deduce_why_result_find_supported_single_rule(
    DeduceDb* db,
    SymbolId head_predicate,
    long* rule_index_out,
    SymbolId* source_predicate_out,
    DeduceRuleSignature** sig_out
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (source_predicate_out != null) *source_predicate_out = (SymbolId)0;
    if (sig_out != null) *sig_out = null;
    if (db == null || head_predicate == (SymbolId)0) return false;

    DeduceRuleSignature* matched = null;
    long matched_rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    long head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != head_predicate) continue;
        head_rule_count++;
        if (head_rule_count > 1) return false;
        if (sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) return false;
        if (sig.body_predicates == null) return false;
        if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
            sig.body_term_offsets == null || sig.body_term_counts == null ||
            sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
            return false;
        }
        source_predicate = sig.body_predicates[0];
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_predicates[step] == (SymbolId)0) return false;
            if (sig.body_negated != null && sig.body_negated[step]) return false;
        }
        matched = sig;
        matched_rule_index = (long)i;
    }

    if (matched == null || matched_rule_index < 0 || source_predicate == (SymbolId)0) return false;
    if (rule_index_out != null) *rule_index_out = matched_rule_index;
    if (source_predicate_out != null) *source_predicate_out = source_predicate;
    if (sig_out != null) *sig_out = matched;
    return true;
}

fn bool deduce_why_result_rule_has_supported_nonrecursive_shape(
    DeduceDb* db,
    DeduceRuleSignature* sig
) {
    (void)db;
    if (sig == null || sig.body_count == 0 || deduce_rule_signature_has_aggregates(sig)) {
        return false;
    }
    if (sig.body_predicates == null) return false;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null ||
        sig.body_term_offsets == null || sig.body_term_counts == null ||
        sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
        return false;
    }

    foreach (i, predicate : sig.body_predicates[..sig.body_count]) {
        if (predicate == (SymbolId)0) return false;
        if (sig.body_negated != null && sig.body_negated[i]) return false;
    }
    return true;
}
