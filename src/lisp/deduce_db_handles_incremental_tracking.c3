module lisp;

import std::core::mem;

fn bool deduce_symbol_id_slice_contains(SymbolId[] values, SymbolId target) @inline {
    if (target == (SymbolId)0) return false;
    for (usz i = 0; i < values.len; i++) {
        if (values[i] == target) return true;
    }
    return false;
}

fn bool deduce_db_ensure_incremental_dirty_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.incremental_dirty_capacity) return true;

    usz new_cap = db.incremental_dirty_capacity > 0 ? db.incremental_dirty_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / SymbolId.sizeof) return false;

    SymbolId* next = (SymbolId*)mem::realloc(db.incremental_dirty_predicates, SymbolId.sizeof * new_cap);
    if (next == null) return false;
    db.incremental_dirty_predicates = next;
    db.incremental_dirty_capacity = new_cap;
    return true;
}

fn bool deduce_db_mark_incremental_dirty_predicate(DeduceDb* db, SymbolId predicate) {
    if (db == null || predicate == (SymbolId)0) return false;
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates != null && db.incremental_dirty_predicates[i] == predicate) {
            return true;
        }
    }
    if (db.incremental_dirty_count == usz.max) return false;
    usz needed = db.incremental_dirty_count + 1;
    if (!deduce_db_ensure_incremental_dirty_capacity(db, needed)) return false;
    db.incremental_dirty_predicates[db.incremental_dirty_count] = predicate;
    db.incremental_dirty_count++;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
    deduce_relation_schema_note_materialized_stale(db, schema, DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY);
    return true;
}

fn bool deduce_db_is_incremental_dirty_predicate(DeduceDb* db, SymbolId predicate) {
    if (db == null || predicate == (SymbolId)0) return false;
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates != null && db.incremental_dirty_predicates[i] == predicate) {
            return true;
        }
    }
    return false;
}

fn bool deduce_relation_schema_has_derived_rules(DeduceDb* db, DeduceRelationSchema* schema) {
    if (db == null || schema == null || schema.name == (SymbolId)0) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (db.rule_signatures[i].head_predicate == schema.name) return true;
    }
    return false;
}

fn void deduce_relation_schema_clear_pending_incremental_insertions(DeduceRelationSchema* schema) {
    if (schema == null) return;
    deduce_delta_set_release(&schema.maintained_pending_insertions);
}

fn void deduce_db_clear_pending_incremental_insertions(DeduceDb* db) {
    if (db == null || db.relation_schemas == null) return;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        deduce_relation_schema_clear_pending_incremental_insertions(&db.relation_schemas[i]);
    }
}

fn void deduce_db_escalate_incremental_tracking_to_full_recompute(
    DeduceDb* db,
    DeduceMaterializedStaleReason reason
) {
    if (db == null) return;
    deduce_db_clear_pending_incremental_insertions(db);
    db.incremental_full_recompute_required = true;
    deduce_db_note_materialized_views_stale(db, reason);
}
