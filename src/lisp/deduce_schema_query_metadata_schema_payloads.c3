module lisp;

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
