module lisp;

fn Value* deduce_relation_note_unique_column(Relation* rel, SymbolId col, Interp* interp) {
    if (rel == null || col == (SymbolId)0) return null;
    for (usz i = 0; i < rel.unique_col_count; i++) {
        if (rel.unique_cols != null && rel.unique_cols[i] == col) {
            rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
            return null;
        }
    }
    if (rel.unique_col_count == usz.max || !deduce_relation_ensure_unique_column_capacity(rel, rel.unique_col_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.unique_cols[rel.unique_col_count++] = col;
    rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
    return null;
}

fn Value* deduce_relation_note_reference_constraint(
    Relation* rel,
    SymbolId local_col,
    SymbolId target_relation,
    SymbolId target_column,
    Interp* interp
) {
    if (rel == null || local_col == (SymbolId)0 || target_relation == (SymbolId)0 || target_column == (SymbolId)0) {
        return null;
    }
    for (usz i = 0; i < rel.reference_constraint_count; i++) {
        if (rel.reference_constraints == null || rel.reference_constraints[i].local_column != local_col) continue;
        rel.reference_constraints[i].target_relation = target_relation;
        rel.reference_constraints[i].target_column = target_column;
        return null;
    }
    if (rel.reference_constraint_count == usz.max || !deduce_relation_ensure_reference_constraint_capacity(rel, rel.reference_constraint_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.reference_constraints[rel.reference_constraint_count++] = {
        .local_column = local_col,
        .target_relation = target_relation,
        .target_column = target_column
    };
    return null;
}

fn Value* deduce_relation_note_check_constraint(
    Relation* rel,
    SymbolId local_col,
    SymbolId predicate,
    Interp* interp
) {
    if (rel == null || local_col == (SymbolId)0 || predicate == (SymbolId)0) return null;
    for (usz i = 0; i < rel.check_constraint_count; i++) {
        if (rel.check_constraints == null || rel.check_constraints[i].local_column != local_col) continue;
        rel.check_constraints[i].predicate = predicate;
        return null;
    }
    if (rel.check_constraint_count == usz.max || !deduce_relation_ensure_check_constraint_capacity(rel, rel.check_constraint_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.check_constraints[rel.check_constraint_count++] = {
        .local_column = local_col,
        .predicate = predicate
    };
    return null;
}
