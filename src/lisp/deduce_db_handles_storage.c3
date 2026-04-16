module lisp;

import std::core::mem;

fn bool deduce_value_supported_by_tuple_codec(Value* v) @inline {
    if (v == null || v.tag == NIL) return true;
    return v.tag == INT ||
        v.tag == DOUBLE ||
        v.tag == FLOAT32 ||
        v.tag == STRING ||
        v.tag == SYMBOL ||
        v.tag == TIME_POINT;
}

fn bool deduce_relation_meta_apply_persisted_state(
    DeduceDb* db,
    SymbolId relation_name,
    bool* materialized_view_io,
    SymbolId* materialized_refresh_policy_io,
    ulong* materialized_refresh_count_io,
    ulong* materialized_last_refresh_mutation_epoch_io,
    ulong* materialized_last_stale_mutation_epoch_io,
    DeduceMaterializedStaleReason* materialized_last_stale_reason_io,
    Interp* interp
) {
    if (db == null || !db.open || relation_name == (SymbolId)0 || interp == null) return false;
    if (materialized_view_io == null || materialized_refresh_policy_io == null) return false;
    if (materialized_refresh_count_io == null ||
        materialized_last_refresh_mutation_epoch_io == null ||
        materialized_last_stale_mutation_epoch_io == null ||
        materialized_last_stale_reason_io == null) {
        return false;
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) return false;
    defer mdb_txn_abort(txn);

    MdbDbi meta_dbi = 0;
    if (!deduce_open_relation_meta_dbi(db, txn, false, &meta_dbi)) return false;

    char[] relation_name_str = interp.symbols.get_name(relation_name);
    MdbVal key = {
        .mv_size = relation_name_str.len,
        .mv_data = relation_name_str.ptr
    };
    MdbVal data = {};
    rc = mdb_get(txn, meta_dbi, &key, &data);
    if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRelationMetaV1.sizeof) {
        return false;
    }

    DeducePersistedRelationMetaV1 meta_v1 = {};
    mem::copy(&meta_v1, data.mv_data, DeducePersistedRelationMetaV1.sizeof);
    if (meta_v1.version == 1) {
        if ((meta_v1.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED) == 0) return false;
        *materialized_view_io = true;
        if ((meta_v1.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH) != 0) {
            SymbolId policy = interp.symbols.intern("manual");
            if (policy == INVALID_SYMBOL_ID) return false;
            *materialized_refresh_policy_io = policy;
        }
        *materialized_refresh_count_io = 0;
        *materialized_last_refresh_mutation_epoch_io = 0;
        *materialized_last_stale_mutation_epoch_io = 0;
        *materialized_last_stale_reason_io = DEDUCE_MATERIALIZED_STALE_NONE;
        return true;
    }

    if (meta_v1.version != DEDUCE_PERSISTED_RELATION_META_VERSION ||
        data.mv_size < DeducePersistedRelationMetaV2.sizeof) {
        return false;
    }

    DeducePersistedRelationMetaV2 meta_v2 = {};
    mem::copy(&meta_v2, data.mv_data, DeducePersistedRelationMetaV2.sizeof);
    if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED) == 0) return false;

    *materialized_view_io = true;
    if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH) != 0) {
        SymbolId policy = interp.symbols.intern("manual");
        if (policy == INVALID_SYMBOL_ID) return false;
        *materialized_refresh_policy_io = policy;
    } else if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_ON_READ_REFRESH) != 0) {
        SymbolId policy = interp.symbols.intern("on-read");
        if (policy == INVALID_SYMBOL_ID) return false;
        *materialized_refresh_policy_io = policy;
    }
    *materialized_refresh_count_io = meta_v2.materialized_refresh_count;
    *materialized_last_refresh_mutation_epoch_io = meta_v2.materialized_last_refresh_mutation_epoch;
    *materialized_last_stale_mutation_epoch_io = meta_v2.materialized_last_stale_mutation_epoch;
    if (meta_v2.stale_reason <= (char)DeduceMaterializedStaleReason.DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED) {
        *materialized_last_stale_reason_io = (DeduceMaterializedStaleReason)meta_v2.stale_reason;
    } else {
        *materialized_last_stale_reason_io = DeduceMaterializedStaleReason.DEDUCE_MATERIALIZED_STALE_NONE;
    }
    return true;
}

fn Value* deduce_relation_meta_persist_state(
    DeduceDb* db,
    SymbolId relation_name,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason,
    bool delete_entry,
    Interp* interp
) {
    if (db == null || !db.open || relation_name == (SymbolId)0 || interp == null) {
        return deduce_raise(interp, "deduce/materialized-meta-invalid-state",
            "deduce: invalid relation metadata persistence state");
    }
    if (g_deduce_force_materialized_meta_persist_fail) {
        return deduce_raise(interp, "deduce/materialized-meta-forced-failed",
            "deduce: forced materialized relation metadata persistence failure");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/materialized-meta-txn-open-failed",
            "deduce: materialized relation metadata txn open failed");
    }

    MdbDbi meta_dbi = 0;
    int meta_open_rc = deduce_open_relation_meta_dbi_rc(db, txn, !delete_entry, &meta_dbi);
    if (meta_open_rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        if (delete_entry && meta_open_rc == MDB_NOTFOUND) return null;
        return deduce_raise(interp, "deduce/materialized-meta-open-failed",
            "deduce: materialized relation metadata dbi open failed");
    }

    char[] relation_name_str = interp.symbols.get_name(relation_name);
    MdbVal key = {
        .mv_size = relation_name_str.len,
        .mv_data = relation_name_str.ptr
    };
    if (delete_entry) {
        rc = mdb_del(txn, meta_dbi, &key, null);
        if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/materialized-meta-delete-failed",
                "deduce: materialized relation metadata delete failed");
        }
    } else {
        DeducePersistedRelationMetaV2 meta = {
            .version = DEDUCE_PERSISTED_RELATION_META_VERSION,
            .flags = 0,
            .stale_reason = (char)materialized_last_stale_reason,
            .reserved = 0,
            .materialized_refresh_count = materialized_refresh_count,
            .materialized_last_refresh_mutation_epoch = materialized_last_refresh_mutation_epoch,
            .materialized_last_stale_mutation_epoch = materialized_last_stale_mutation_epoch,
        };
        if (materialized_view) meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED;
        if (materialized_refresh_policy != (SymbolId)0) {
            char[] policy_name = interp.symbols.get_name(materialized_refresh_policy);
            if (deduce_name_equals(policy_name, "manual")) {
                meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH;
            } else if (deduce_name_equals(policy_name, "on-read")) {
                meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_ON_READ_REFRESH;
            }
        }
        MdbVal data = {
            .mv_size = DeducePersistedRelationMetaV2.sizeof,
            .mv_data = &meta
        };
        rc = mdb_put(txn, meta_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/materialized-meta-write-failed",
                "deduce: materialized relation metadata write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/materialized-meta-commit-failed",
            "deduce: materialized relation metadata commit failed");
    }
    return null;
}
