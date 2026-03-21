module lisp;

import std::core::mem;

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
    usz needed = db.incremental_dirty_count + 1;
    if (!deduce_db_ensure_incremental_dirty_capacity(db, needed)) return false;
    db.incremental_dirty_predicates[db.incremental_dirty_count] = predicate;
    db.incremental_dirty_count++;
    return true;
}

fn bool deduce_db_mark_dirty_with_rule_dependents(DeduceDb* db, SymbolId seed_predicate) {
    if (db == null || seed_predicate == (SymbolId)0) return false;
    usz queue_cap = db.rule_signature_count + db.relation_schema_count + 1;
    if (queue_cap == 0) queue_cap = 1;
    SymbolId[] queue = mem::new_array(SymbolId, queue_cap);
    if (queue.ptr == null) return false;
    defer mem::free(queue.ptr);

    usz queue_len = 1;
    usz queue_idx = 0;
    queue[0] = seed_predicate;

    while (queue_idx < queue_len) {
        SymbolId current = queue[queue_idx++];
        if (!deduce_db_mark_incremental_dirty_predicate(db, current)) return false;

        for (usz r = 0; r < db.rule_signature_count; r++) {
            DeduceRuleSignature* sig = &db.rule_signatures[r];
            bool depends_on_current = false;
            for (usz b = 0; b < sig.body_count; b++) {
                if (sig.body_predicates != null && sig.body_predicates[b] == current) {
                    depends_on_current = true;
                    break;
                }
            }
            if (!depends_on_current) continue;
            SymbolId head = sig.head_predicate;
            if (head == (SymbolId)0) continue;
            if (deduce_symbol_id_slice_contains(queue[..queue_len], head)) continue;
            if (queue_len >= queue.len) return false;
            queue[queue_len++] = head;
        }
    }
    return true;
}

fn void deduce_db_note_committed_mutation(Relation* rel, bool destructive_change) {
    if (rel == null || rel.db == null) return;
    DeduceDb* db = rel.db;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    if (destructive_change) db.incremental_full_recompute_required = true;
    if (!deduce_db_mark_dirty_with_rule_dependents(db, rel.name)) {
        db.incremental_full_recompute_required = true;
    }
}

fn void deduce_db_note_mutation(Relation* rel, bool destructive_change) {
    deduce_db_note_committed_mutation(rel, destructive_change);
}

fn void deduce_db_note_untracked_write_commit(DeduceDb* db) {
    if (db == null) return;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    db.incremental_full_recompute_required = true;
}

fn void deduce_db_reset_incremental_tracking(DeduceDb* db) {
    if (db == null) return;
    db.incremental_dirty_count = 0;
    db.incremental_full_recompute_required = false;
}

fn void deduce_db_dependency_edge_counts(DeduceDb* db, long* total_out, long* negated_out) {
    if (total_out != null) *total_out = 0;
    if (negated_out != null) *negated_out = 0;
    if (db == null) return;

    long total = 0;
    long negated = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        for (usz b = 0; b < sig.body_count; b++) {
            total++;
            if (sig.body_negated != null && sig.body_negated[b]) negated++;
        }
    }
    if (total_out != null) *total_out = total;
    if (negated_out != null) *negated_out = negated;
}

fn bool deduce_db_lookup_relation_arity(DeduceDb* db, SymbolId name, usz* arity_out) {
    if (arity_out == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, name);
    if (schema == null) return false;
    *arity_out = schema.arity;
    return true;
}

fn bool deduce_index_column_bound(ulong bound_mask, usz col_pos) @inline {
    usz width = (usz)(ulong.sizeof * 8);
    if (col_pos >= width) return false;
    return (bound_mask & ((ulong)1 << col_pos)) != 0;
}

fn int deduce_schema_select_index_for_bound_mask(DeduceRelationSchema* schema, ulong bound_mask) {
    if (schema == null || schema.indexes == null || schema.index_count == 0) return -1;

    int best_index = -1;
    int best_score = -1;
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* idx = &schema.indexes[i];
        if (idx.column_positions == null || idx.column_count == 0) continue;

        usz prefix_len = 0;
        for (usz c = 0; c < idx.column_count; c++) {
            if (!deduce_index_column_bound(bound_mask, idx.column_positions[c])) break;
            prefix_len++;
        }
        if (prefix_len == 0) continue;

        int score = (int)(prefix_len * 128);
        score -= (int)idx.column_count;
        if (idx.unique) score += 4;
        if (score > best_score) {
            best_score = score;
            best_index = (int)i;
        }
    }
    return best_index;
}
