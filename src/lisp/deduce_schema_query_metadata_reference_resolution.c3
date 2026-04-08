module lisp;

fn void deduce_reference_constraint_resolution(
    DeduceDb* db,
    DeduceReferenceConstraint* ref_constraint,
    bool* target_declared_out,
    bool* target_column_declared_out,
    bool* target_live_out,
    bool* target_unique_out
) {
    bool target_declared = false;
    bool target_column_declared = false;
    bool target_live = false;
    bool target_unique = false;
    if (db != null && ref_constraint != null) {
        DeduceRelationSchema* target_schema = deduce_db_lookup_relation_schema(db, ref_constraint.target_relation);
        if (target_schema != null) {
            target_declared = true;
            usz ignored = 0;
            target_column_declared =
                deduce_find_column_position(target_schema.columns, target_schema.arity, ref_constraint.target_column, &ignored);
            target_live = !target_schema.dropped;
            if (target_column_declared && target_live) {
                target_unique = deduce_relation_schema_column_is_candidate_key(target_schema, ref_constraint.target_column);
            }
        }
    }
    if (target_declared_out != null) *target_declared_out = target_declared;
    if (target_column_declared_out != null) *target_column_declared_out = target_column_declared;
    if (target_live_out != null) *target_live_out = target_live;
    if (target_unique_out != null) *target_unique_out = target_unique;
}

fn long deduce_relation_schema_resolved_reference_constraint_count(
    DeduceDb* db,
    DeduceRelationSchema* schema
) {
    if (db == null || schema == null || schema.reference_constraints == null || schema.reference_constraint_count == 0) {
        return 0;
    }
    long count = 0;
    for (usz i = 0; i < schema.reference_constraint_count; i++) {
        bool target_declared = false;
        bool target_column_declared = false;
        bool target_live = false;
        bool target_unique = false;
        deduce_reference_constraint_resolution(db, &schema.reference_constraints[i], &target_declared, &target_column_declared, &target_live, &target_unique);
        if (target_declared && target_column_declared && target_live && target_unique) count++;
    }
    return count;
}
