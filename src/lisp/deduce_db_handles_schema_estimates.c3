module lisp;

fn DeduceRelationSchema* deduce_db_lookup_relation_schema(DeduceDb* db, SymbolId name) {
    if (db == null || name == (SymbolId)0) return null;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (db.relation_schemas[i].name != name) continue;
        return &db.relation_schemas[i];
    }
    return null;
}

fn DeduceRelationSchema* deduce_relation_lookup_schema(Relation* rel) {
    if (rel == null || rel.db == null) return null;
    return deduce_db_lookup_relation_schema(rel.db, rel.name);
}

fn void deduce_relation_schema_set_estimates(Relation* rel, ulong cardinality, ulong distinct_estimate) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    schema.cardinality_estimate = cardinality;
    if (distinct_estimate > cardinality) {
        schema.distinct_estimate = cardinality;
    } else {
        schema.distinct_estimate = distinct_estimate;
    }
}

fn void deduce_relation_schema_note_insert(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate < ulong.max) schema.cardinality_estimate++;
    if (schema.distinct_estimate < schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_delete(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate > 0) schema.cardinality_estimate--;
    if (schema.distinct_estimate > schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_clear(Relation* rel) {
    deduce_relation_schema_set_estimates(rel, 0, 0);
}
