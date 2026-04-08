module lisp;

import std::core::mem;

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
    if (destructive_change) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
    if (!deduce_db_mark_dirty_with_rule_dependents(db, rel.name)) {
        deduce_db_escalate_incremental_tracking_to_full_recompute(
            db,
            DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
        );
    }
}

fn Value* deduce_db_note_mutation(Relation* rel, bool destructive_change, Interp* interp) {
    deduce_db_note_committed_mutation(rel, destructive_change);
    return deduce_db_persist_materialized_view_states(rel != null ? rel.db : null, interp);
}

fn Value* deduce_db_note_untracked_write_commit(DeduceDb* db, Interp* interp) {
    if (db == null) return null;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    deduce_db_escalate_incremental_tracking_to_full_recompute(
        db,
        DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
    );
    return deduce_db_persist_materialized_view_states(db, interp);
}

fn Value* deduce_db_note_rule_signature_install(DeduceDb* db, Interp* interp) {
    if (db == null) return null;
    bool has_ready_materialized_views = false;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (!deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i])) continue;
        has_ready_materialized_views = true;
        break;
    }
    if (!has_ready_materialized_views) return null;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    deduce_db_clear_pending_incremental_insertions(db);
    db.incremental_full_recompute_required = true;
    deduce_db_note_ready_materialized_views_stale(db, DEDUCE_MATERIALIZED_STALE_RULE_SET_CHANGE);
    return deduce_db_persist_materialized_view_states(db, interp);
}

fn void deduce_db_reset_incremental_tracking(DeduceDb* db) {
    if (db == null) return;
    deduce_db_clear_pending_incremental_insertions(db);
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
