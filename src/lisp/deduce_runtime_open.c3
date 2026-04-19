module lisp;

import std::core::mem;

// ============================================================
// Relation — named tuple schema + LMDB backing
// ============================================================
struct Relation {
    SymbolId     name;
    SymbolId*    columns;
    Value**      column_key_values;
    usz          col_count;
    usz          col_capacity;
    MdbDbi       dbi;           // LMDB database handle for this relation
    DeduceDb*    db;            // parent database
    SymbolId     key_col;       // primary key column (0 = none)
    SymbolId     unique_col;    // unique column (0 = none)
    SymbolId*    unique_cols;   // composite unique columns (possibly length 0/1+)
    usz          unique_col_count;
    usz          unique_col_capacity;
    DeduceReferenceConstraint* reference_constraints;
    usz          reference_constraint_count;
    usz          reference_constraint_capacity;
    DeduceCheckConstraint* check_constraints;
    usz          check_constraint_count;
    usz          check_constraint_capacity;
    SymbolId     index_col;     // indexed column (0 = none)
    bool         materialized_view;
    SymbolId     materialized_refresh_policy;
    ulong        materialized_refresh_count;
    ulong        materialized_last_refresh_mutation_epoch;
    ulong        materialized_last_stale_mutation_epoch;
    DeduceMaterializedStaleReason materialized_last_stale_reason;
    bool         dropped;       // true after relation database is dropped
}

fn void deduce_relation_free(Relation* rel) @inline {
    if (rel == null) return;
    if (rel.reference_constraints != null) {
        mem::free(rel.reference_constraints);
        rel.reference_constraints = null;
    }
    if (rel.check_constraints != null) {
        mem::free(rel.check_constraints);
        rel.check_constraints = null;
    }
    if (rel.unique_cols != null) {
        mem::free(rel.unique_cols);
        rel.unique_cols = null;
    }
    if (rel.column_key_values != null) {
        mem::free(rel.column_key_values);
        rel.column_key_values = null;
    }
    if (rel.columns != null) {
        mem::free(rel.columns);
        rel.columns = null;
    }
    mem::free(rel);
}

fn void deduce_relation_finalizer(void* handle) {
    if (handle == null) return;
    Relation* rel = (Relation*)handle;
    rel.dropped = true;
    deduce_relation_free(rel);
}

// ============================================================
// (deduce-open path) → database handle
// (deduce-open 'memory) → ephemeral database
// ============================================================
fn Value* prim_deduce_open(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return deduce_raise(interp, "deduce/open-expected-path-or-memory", "deduce-open: expected path or 'memory");

    DeduceDb* db = (DeduceDb*)mem::malloc(DeduceDb.sizeof);
    if (db == null) return deduce_raise(interp, "deduce/open-out-of-memory", "deduce-open: out of memory");
    db.env = null;
    db.open = false;
    db.relation_schemas = null;
    db.relation_schema_count = 0;
    db.relation_schema_capacity = 0;
    db.rule_signatures = null;
    db.rule_signature_count = 0;
    db.rule_signature_capacity = 0;
    db.persisted_rule_catalog = null;
    db.persisted_rule_catalog_count = 0;
    db.persisted_rule_catalog_capacity = 0;
    db.incremental_dirty_predicates = null;
    db.incremental_dirty_count = 0;
    db.incremental_dirty_capacity = 0;
    db.incremental_mutation_epoch = 0;
    db.incremental_full_recompute_required = false;
    db.integrity_violation_count = 0;
    db.key_integrity_violation_count = 0;
    db.unique_integrity_violation_count = 0;
    db.reference_integrity_violation_count = 0;
    db.check_integrity_violation_count = 0;
    db.last_integrity_violation_code = (SymbolId)0;
    db.last_integrity_violation_relation = (SymbolId)0;
    db.last_goal_directed_read_execution_path = (SymbolId)0;
    db.last_goal_directed_read_surface = (SymbolId)0;
    db.last_goal_directed_read_relation = (SymbolId)0;
    db.last_goal_directed_read_fallback_reason = (SymbolId)0;
    db.last_goal_directed_read_selector_rule_index = -1;
    db.last_goal_directed_read_mutation_epoch = 0;
    db.last_goal_directed_read_requested_bound_count = 0;
    db.last_goal_directed_read_requested_bound_positions = null;
    db.last_goal_directed_read_applied_bound_count = 0;
    db.last_goal_directed_read_applied_bound_positions = null;
    db.last_goal_directed_read_step_counters = null;
    db.last_goal_directed_read_step_count = 0;
    db.integrity_violation_history_count = 0;
    db.integrity_violation_sequence = 0;

    int rc = mdb_env_create(&db.env);
    if (rc != MDB_SUCCESS) {
        mem::free(db);
        return deduce_raise(interp, "deduce/open-env-create-failed", "deduce-open: failed to create LMDB env");
    }

    mdb_env_set_mapsize(db.env, 1073741824);
    mdb_env_set_maxdbs(db.env, 64);

    char* path_buf = null;
    uint flags = 0;

    if (args[0].tag == SYMBOL) {
        char[256] memory_path;
        if (!deduce_build_memory_path(&memory_path, db)) {
            mdb_env_close(db.env);
            mem::free(db);
            return deduce_raise(interp, "deduce/open-memory-path-failed", "deduce-open: failed to build memory path");
        }
        path_buf = deduce_copy_c_string(memory_path[..]);
        if (path_buf == null) {
            mdb_env_close(db.env);
            mem::free(db);
            return deduce_raise(interp, "deduce/open-out-of-memory", "deduce-open: out of memory");
        }
        flags = MDB_NOSUBDIR;
    } else if (is_string(args[0])) {
        char[] path = args[0].str_chars[:args[0].str_len];
        path_buf = deduce_copy_c_string(path);
        if (path_buf == null) {
            mdb_env_close(db.env);
            mem::free(db);
            return deduce_raise(interp, "deduce/open-out-of-memory", "deduce-open: out of memory");
        }
        flags = MDB_NOSUBDIR;
    } else {
        mdb_env_close(db.env);
        mem::free(db);
        return deduce_raise(interp, "deduce/open-path-not-string-or-memory", "deduce-open: expected path string or 'memory");
    }
    defer if (path_buf != null) mem::free(path_buf);

    rc = mdb_env_open(db.env, path_buf, flags, 0o664);
    if (rc != MDB_SUCCESS) {
        mdb_env_close(db.env);
        mem::free(db);
        return deduce_raise(interp, "deduce/open-env-open-failed", "deduce-open: failed to open database");
    }

    db.open = true;

    Value* rule_catalog_err = deduce_db_restore_persisted_rule_catalog(db, interp);
    if (rule_catalog_err != null) {
        mdb_env_close(db.env);
        mem::free(db);
        return rule_catalog_err;
    }
    Value* rule_signature_err = deduce_db_restore_persisted_rule_signatures(db, interp);
    if (rule_signature_err != null) {
        mdb_env_close(db.env);
        mem::free(db);
        return rule_signature_err;
    }

    Value* v = make_ffi_handle_ex(interp, (void*)db, "deduce-db", &deduce_db_finalizer, true, false);
    if (v == null) {
        return deduce_raise(interp, "deduce/open-out-of-memory", "deduce-open: out of memory");
    }
    return v;
}
