module lisp;

import std::core::mem;

fn Relation* deduce_relation_alloc(DeduceDb* db, SymbolId rel_name) {
    Relation* rel = (Relation*)mem::malloc(Relation.sizeof);
    if (rel == null) return null;
    rel.name = rel_name;
    rel.columns = null;
    rel.column_key_values = null;
    rel.db = db;
    rel.col_count = 0;
    rel.col_capacity = 0;
    rel.dbi = 0;
    rel.key_col = (SymbolId)0;
    rel.unique_col = (SymbolId)0;
    rel.unique_cols = null;
    rel.unique_col_count = 0;
    rel.unique_col_capacity = 0;
    rel.reference_constraints = null;
    rel.reference_constraint_count = 0;
    rel.reference_constraint_capacity = 0;
    rel.check_constraints = null;
    rel.check_constraint_count = 0;
    rel.check_constraint_capacity = 0;
    rel.index_col = (SymbolId)0;
    rel.materialized_view = false;
    rel.materialized_refresh_policy = (SymbolId)0;
    rel.materialized_refresh_count = 0;
    rel.materialized_last_refresh_mutation_epoch = 0;
    rel.materialized_last_stale_mutation_epoch = 0;
    rel.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    rel.dropped = false;
    return rel;
}

fn bool deduce_relation_ensure_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.col_capacity) return true;

    usz new_cap = rel.col_capacity > 0 ? rel.col_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / SymbolId.sizeof) return false;

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.columns, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.columns = new_columns;
    rel.col_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_unique_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.unique_col_capacity) return true;

    usz new_cap = rel.unique_col_capacity > 0 ? rel.unique_col_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / SymbolId.sizeof) return false;

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.unique_cols, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.unique_cols = new_columns;
    rel.unique_col_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_reference_constraint_capacity(Relation* rel, usz needed) {
    if (needed <= rel.reference_constraint_capacity) return true;

    usz new_cap = rel.reference_constraint_capacity > 0 ? rel.reference_constraint_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / DeduceReferenceConstraint.sizeof) return false;

    DeduceReferenceConstraint* next =
        (DeduceReferenceConstraint*)mem::realloc(rel.reference_constraints, DeduceReferenceConstraint.sizeof * new_cap);
    if (next == null) return false;
    rel.reference_constraints = next;
    rel.reference_constraint_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_check_constraint_capacity(Relation* rel, usz needed) {
    if (needed <= rel.check_constraint_capacity) return true;

    usz new_cap = rel.check_constraint_capacity > 0 ? rel.check_constraint_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / DeduceCheckConstraint.sizeof) return false;

    DeduceCheckConstraint* next =
        (DeduceCheckConstraint*)mem::realloc(rel.check_constraints, DeduceCheckConstraint.sizeof * new_cap);
    if (next == null) return false;
    rel.check_constraints = next;
    rel.check_constraint_capacity = new_cap;
    return true;
}
