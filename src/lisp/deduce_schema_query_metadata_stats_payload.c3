module lisp;

fn Value* deduce_stats_build_payload(
    Value* out,
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-stats"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "dirty-predicate-count", make_int(interp, (long)rel.db.incremental_dirty_count));
    Value* dirty_predicates = deduce_dirty_predicates_payload(rel.db, interp);
    if (dirty_predicates != null && dirty_predicates.tag == ERROR) return dirty_predicates;
    explain_dict_set(out, interp, "dirty-predicates", dirty_predicates);
    explain_dict_set(out, interp, "dirty-self",
        explain_bool_value(interp, deduce_db_is_incremental_dirty_predicate(rel.db, rel.name)));
    explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)rel.db.incremental_mutation_epoch));
    explain_dict_set(out, interp, "full-recompute-required", explain_bool_value(interp, rel.db.incremental_full_recompute_required));
    Value* parallel_err = deduce_stats_set_parallel_topology_fields(out, rel, interp);
    if (parallel_err != null) return parallel_err;
    explain_dict_set(out, interp, "integrity-violation-count", make_int(interp, (long)schema.integrity_violation_count));
    explain_dict_set(out, interp, "key-integrity-violation-count", make_int(interp, (long)schema.key_integrity_violation_count));
    explain_dict_set(out, interp, "unique-integrity-violation-count", make_int(interp, (long)schema.unique_integrity_violation_count));
    explain_dict_set(out, interp, "reference-integrity-violation-count", make_int(interp, (long)schema.reference_integrity_violation_count));
    explain_dict_set(out, interp, "check-integrity-violation-count", make_int(interp, (long)schema.check_integrity_violation_count));
    explain_dict_set(out, interp, "last-integrity-violation-code",
        schema.last_integrity_violation_code != (SymbolId)0
            ? make_symbol(interp, schema.last_integrity_violation_code)
            : make_nil(interp));
    Value* recent_integrity_violations = deduce_integrity_violation_history_payload(
        schema.integrity_violation_history[..],
        schema.integrity_violation_history_count,
        interp
    );
    if (recent_integrity_violations != null && recent_integrity_violations.tag == ERROR) {
        return recent_integrity_violations;
    }
    explain_dict_set(out, interp, "recent-integrity-violations", recent_integrity_violations);
    deduce_stats_set_materialized_and_read_fields(out, rel, schema, interp);
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    return out;
}
