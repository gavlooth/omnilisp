module lisp;

import std::core::mem;

struct DeduceTxnMutationEntry {
    Relation* relation;
    ulong     inserted_count;
    ulong     deleted_count;
    bool      destructive_change;
    DeduceDeltaTupleSet inserted_tuples;
}

struct DeduceTxn {
    DeduceDb* db;
    MdbTxn*   txn;
    bool      open;
    bool      read_only;
    bool      defer_integrity_checks;
    bool      mutation_pending;
    DeduceTxnMutationEntry* mutation_entries;
    usz       mutation_entry_count;
    usz       mutation_entry_capacity;
}

// Test seam: force transaction mutation-log append/write failure paths.
bool g_deduce_force_txn_mutation_log_fail = false;

fn bool deduce_txn_ensure_mutation_capacity(DeduceTxn* txn, usz needed) {
    if (txn == null) return false;
    if (needed <= txn.mutation_entry_capacity) return true;

    usz new_cap = txn.mutation_entry_capacity > 0 ? txn.mutation_entry_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceTxnMutationEntry* next =
        (DeduceTxnMutationEntry*)mem::realloc(txn.mutation_entries, DeduceTxnMutationEntry.sizeof * new_cap);
    if (next == null) return false;
    txn.mutation_entries = next;
    txn.mutation_entry_capacity = new_cap;
    return true;
}

fn DeduceTxnMutationEntry* deduce_txn_lookup_or_add_mutation_entry(DeduceTxn* txn, Relation* rel) {
    if (txn == null || rel == null) return null;
    for (usz i = 0; i < txn.mutation_entry_count; i++) {
        DeduceTxnMutationEntry* entry = &txn.mutation_entries[i];
        if (entry.relation == rel) return entry;
    }

    usz needed = txn.mutation_entry_count + 1;
    if (!deduce_txn_ensure_mutation_capacity(txn, needed)) return null;

    DeduceTxnMutationEntry* added = &txn.mutation_entries[txn.mutation_entry_count];
    added.relation = rel;
    added.inserted_count = 0;
    added.deleted_count = 0;
    added.destructive_change = false;
    added.inserted_tuples = {};
    txn.mutation_entry_count++;
    return added;
}

fn bool deduce_txn_log_insert(DeduceTxn* txn, Relation* rel, void* key_data, usz key_len) {
    if (g_deduce_force_txn_mutation_log_fail) return false;
    DeduceTxnMutationEntry* entry = deduce_txn_lookup_or_add_mutation_entry(txn, rel);
    if (entry == null) return false;
    if (entry.inserted_count < ulong.max) entry.inserted_count++;
    return deduce_delta_set_append_encoded_tuple(&entry.inserted_tuples, key_data, key_len);
}

fn bool deduce_txn_log_delete(DeduceTxn* txn, Relation* rel) {
    if (g_deduce_force_txn_mutation_log_fail) return false;
    DeduceTxnMutationEntry* entry = deduce_txn_lookup_or_add_mutation_entry(txn, rel);
    if (entry == null) return false;
    if (entry.deleted_count < ulong.max) entry.deleted_count++;
    entry.destructive_change = true;
    return true;
}

fn void deduce_relation_schema_apply_mutation_delta(Relation* rel, ulong inserted_count, ulong deleted_count) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;

    ulong cardinality = schema.cardinality_estimate;
    if (inserted_count > 0) {
        if (ulong.max - cardinality < inserted_count) {
            cardinality = ulong.max;
        } else {
            cardinality += inserted_count;
        }
    }
    if (deleted_count > 0) {
        if (deleted_count >= cardinality) {
            cardinality = 0;
        } else {
            cardinality -= deleted_count;
        }
    }
    schema.cardinality_estimate = cardinality;
    schema.distinct_estimate = cardinality;
}

fn void deduce_txn_apply_mutation_log(DeduceTxn* txn) {
    if (txn == null || txn.mutation_entries == null || txn.mutation_entry_count == 0) return;
    for (usz i = 0; i < txn.mutation_entry_count; i++) {
        DeduceTxnMutationEntry* entry = &txn.mutation_entries[i];
        Relation* rel = entry.relation;
        if (rel == null || rel.db == null) continue;
        deduce_relation_schema_apply_mutation_delta(rel, entry.inserted_count, entry.deleted_count);
        bool destructive = entry.destructive_change || entry.deleted_count > 0;
        deduce_db_note_committed_mutation(rel, destructive);
        if (!destructive) {
            bool tracked_incremental = true;
            for (usz tuple_idx = 0; tuple_idx < entry.inserted_tuples.count; tuple_idx++) {
                DeduceEncodedTuple* tuple = &entry.inserted_tuples.tuples[tuple_idx];
                if (tuple == null) continue;
                if (!deduce_db_queue_supported_incremental_insert(rel, tuple.key_bytes, tuple.key_len)) {
                    tracked_incremental = false;
                    break;
                }
            }
            if (!tracked_incremental) {
                deduce_db_escalate_incremental_tracking_to_full_recompute(
                    rel.db,
                    DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED
                );
            }
        }
    }
}

fn void deduce_txn_clear_mutation_log(DeduceTxn* txn) {
    if (txn == null) return;
    if (txn.mutation_entries != null) {
        for (usz i = 0; i < txn.mutation_entry_count; i++) {
            deduce_delta_set_release(&txn.mutation_entries[i].inserted_tuples);
        }
        mem::free(txn.mutation_entries);
        txn.mutation_entries = null;
    }
    txn.mutation_entry_count = 0;
    txn.mutation_entry_capacity = 0;
}

fn void deduce_txn_finalize(DeduceTxn* txn) @inline {
    if (txn == null) return;
    if (txn.open && txn.txn != null) {
        mdb_txn_abort(txn.txn);
    }
    txn.txn = null;
    txn.open = false;
    txn.db = null;
    txn.defer_integrity_checks = false;
    txn.mutation_pending = false;
    deduce_txn_clear_mutation_log(txn);
}

fn void deduce_txn_finalizer(void* handle) {
    if (handle == null) return;
    DeduceTxn* txn = (DeduceTxn*)handle;
    deduce_txn_finalize(txn);
}
