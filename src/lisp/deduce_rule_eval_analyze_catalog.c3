module lisp;

struct DeduceAnalyzeCatalogSummary {
    long   dependency_edges;
    long   catalog_rule_count;
    long   incremental_dirty_predicates;
    long   keyed_relation_count;
    long   unique_relation_count;
    long   reference_constraint_count;
    long   resolved_reference_constraint_count;
    long   unresolved_reference_constraint_count;
    long   check_constraint_count;
    long   integrity_constraint_count;
    long   materialized_view_count;
    long   ready_materialized_view_count;
    long   unready_materialized_view_count;
    long   stale_materialized_view_count;
    Value* ready_materialized_relations_rev;
    Value* unready_materialized_relations_rev;
    Value* stale_materialized_relations_rev;
}

fn Value* deduce_analyze_dirty_predicates_payload(DeduceDb* db, Interp* interp) {
    if (db == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn void deduce_analyze_catalog_summary_init(
    DeduceAnalyzeCatalogSummary* summary,
    Interp* interp
) {
    if (summary == null) return;
    *summary = {
        .ready_materialized_relations_rev = make_nil(interp),
        .unready_materialized_relations_rev = make_nil(interp),
        .stale_materialized_relations_rev = make_nil(interp),
    };
}

fn void deduce_analyze_collect_catalog_summary(
    DeduceDb* db,
    DeduceAnalyzeCatalogSummary* summary,
    Interp* interp
) {
    if (db == null || summary == null) return;

    deduce_db_catalog_dependency_edge_counts(db, &summary.dependency_edges, null);
    summary.catalog_rule_count = (long)deduce_db_catalog_rule_count(db);
    summary.incremental_dirty_predicates = (long)db.incremental_dirty_count;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* schema = &db.relation_schemas[i];
        if (schema.key_column != (SymbolId)0) summary.keyed_relation_count++;
        if (schema.unique_column_count > 0 &&
            !deduce_relation_schema_unique_equivalent_to_key(schema)) {
            summary.unique_relation_count++;
        }
        long resolved_reference_count =
            deduce_relation_schema_resolved_reference_constraint_count(db, schema);
        summary.reference_constraint_count += (long)schema.reference_constraint_count;
        summary.check_constraint_count += (long)schema.check_constraint_count;
        summary.resolved_reference_constraint_count += resolved_reference_count;
        summary.unresolved_reference_constraint_count +=
            (long)schema.reference_constraint_count - resolved_reference_count;
        summary.integrity_constraint_count += deduce_relation_schema_constraint_count(schema);
        if (!schema.materialized_view) continue;

        summary.materialized_view_count++;
        Value* relation_symbol = make_symbol(interp, schema.name);
        if (deduce_relation_schema_is_materialized_ready(db, schema)) {
            summary.ready_materialized_view_count++;
            summary.ready_materialized_relations_rev =
                make_cons(interp, relation_symbol, summary.ready_materialized_relations_rev);
        } else {
            summary.unready_materialized_view_count++;
            summary.unready_materialized_relations_rev =
                make_cons(interp, relation_symbol, summary.unready_materialized_relations_rev);
        }
        if (deduce_relation_schema_is_materialized_stale(db, schema)) {
            summary.stale_materialized_view_count++;
            summary.stale_materialized_relations_rev =
                make_cons(interp, relation_symbol, summary.stale_materialized_relations_rev);
        }
    }
}
