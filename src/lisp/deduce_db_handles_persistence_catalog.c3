module lisp;

fn usz deduce_db_catalog_rule_count(DeduceDb* db) @inline {
    if (db == null) return 0;
    if (db.rule_signature_count > 0) return db.rule_signature_count;
    return db.persisted_rule_catalog_count;
}

fn void deduce_db_catalog_dependency_edge_counts(DeduceDb* db, long* total_out, long* negated_out) {
    if (total_out != null) *total_out = 0;
    if (negated_out != null) *negated_out = 0;
    if (db == null) return;
    if (db.rule_signature_count > 0) {
        deduce_db_dependency_edge_counts(db, total_out, negated_out);
        return;
    }

    long total = 0;
    long negated = 0;
    for (usz i = 0; i < db.persisted_rule_catalog_count; i++) {
        DeducePersistedRuleCatalogEntry* entry = &db.persisted_rule_catalog[i];
        for (usz b = 0; b < entry.body_count; b++) {
            total++;
            if (entry.body_negated != null && entry.body_negated[b]) negated++;
        }
    }
    if (total_out != null) *total_out = total;
    if (negated_out != null) *negated_out = negated;
}
