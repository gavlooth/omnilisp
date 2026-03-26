module lisp;

import std::core::mem;

// ============================================================
// Deduce — Relational Datalog for Omni
//
// LMDB-backed named relations, Datalog rules, bottom-up query.
// ============================================================

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
    rel.col_count = 0;
    rel.col_capacity = 0;
    rel.unique_col_count = 0;
    rel.unique_col_capacity = 0;
    rel.reference_constraint_count = 0;
    rel.reference_constraint_capacity = 0;
    rel.check_constraint_count = 0;
    rel.check_constraint_capacity = 0;
    rel.materialized_view = false;
    rel.materialized_refresh_policy = (SymbolId)0;
    rel.materialized_refresh_count = 0;
    rel.materialized_last_refresh_mutation_epoch = 0;
    rel.materialized_last_stale_mutation_epoch = 0;
    rel.materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    rel.dropped = true;
}

// ============================================================
// (deduce-open path) → database handle
// (deduce-open 'memory) → ephemeral database
// ============================================================
fn Value* prim_deduce_open(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/open-expected-path-or-memory", "deduce-open: expected path or 'memory");

    DeduceDb* db = (DeduceDb*)mem::malloc(DeduceDb.sizeof);
    // fault: lisp::WRITE_FAILED
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
        // fault: lisp::WRITE_FAILED
        return deduce_raise(interp, "deduce/open-env-create-failed", "deduce-open: failed to create LMDB env");
    }

    mdb_env_set_mapsize(db.env, 1073741824);  // 1GB default
    mdb_env_set_maxdbs(db.env, 64);

    // Determine path
    char* path_buf = null;
    uint flags = 0;

    if (args[0].tag == SYMBOL) {
        // 'memory → use unique temp path with NOSUBDIR
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
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/open-path-not-string-or-memory", "deduce-open: expected path string or 'memory");
    }
    defer if (path_buf != null) mem::free(path_buf);

    rc = mdb_env_open(db.env, path_buf, flags, 0o664);
    if (rc != MDB_SUCCESS) {
        mdb_env_close(db.env);
        mem::free(db);
        // fault: lisp::WRITE_FAILED
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

    // Wrap database pointer as scoped FFI handle
    Value* v = make_ffi_handle_ex(interp, (void*)db, "deduce-db",
        &deduce_db_finalizer, true, false);
    if (v == null) {
        mdb_env_close(db.env);
        mem::free(db);
        return deduce_raise(interp, "deduce/open-out-of-memory", "deduce-open: out of memory");
    }
    return v;
}

// ============================================================
// (deduce 'block db ['read|'write|'write-deferred]) -> transaction handle
// (deduce 'commit txn) -> Void
// (deduce 'abort txn) -> Void
// ============================================================
fn Value* prim_deduce_block(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return deduce_raise(interp, "deduce/block-expected-database", "deduce-block: expected (deduce 'block db ['read|'write|'write-deferred])");
    }
    if (!deduce_ffi_handle_name_equals(args[0], "deduce-db")) {
        return deduce_raise(interp, "deduce/block-database-not-handle", "deduce-block: first argument must be a deduce database handle");
    }
    if (args[0].ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/block-database-handle-closed", "deduce-block: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)args[0].ffi_val.lib_handle;
    if (db.env == null || !db.open) {
        return deduce_raise(interp, "deduce/block-database-not-open", "deduce-block: database is not open");
    }

    uint flags = 0;
    bool read_only = false;
    bool defer_integrity_checks = false;
    if (args.len == 2) {
        Value* mode = args[1];
        if (mode == null || mode.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/block-mode-not-symbol", "deduce-block: mode must be 'read, 'write, or 'write-deferred");
        }
        char[] mode_name = interp.symbols.get_name(mode.sym_val);
        if (deduce_name_equals(mode_name, "read")) {
            flags = MDB_RDONLY;
            read_only = true;
        } else if (deduce_name_equals(mode_name, "write")) {
            flags = 0;
            read_only = false;
        } else if (deduce_name_equals(mode_name, "write-deferred")) {
            flags = 0;
            read_only = false;
            defer_integrity_checks = true;
        } else {
            return deduce_raise(interp, "deduce/block-invalid-mode", "deduce-block: mode must be 'read, 'write, or 'write-deferred");
        }
    }

    MdbTxn* raw_txn = null;
    int rc = mdb_txn_begin(db.env, null, flags, &raw_txn);
    if (rc != MDB_SUCCESS || raw_txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-block: txn open failed");
    }

    DeduceTxn* txn = (DeduceTxn*)mem::malloc(DeduceTxn.sizeof);
    if (txn == null) {
        mdb_txn_abort(raw_txn);
        return deduce_raise(interp, "deduce/block-out-of-memory", "deduce-block: out of memory");
    }

    txn.db = db;
    txn.txn = raw_txn;
    txn.open = true;
    txn.read_only = read_only;
    txn.defer_integrity_checks = defer_integrity_checks;
    txn.mutation_pending = false;
    txn.mutation_entries = null;
    txn.mutation_entry_count = 0;
    txn.mutation_entry_capacity = 0;

    Value* txn_v = make_ffi_handle_ex(interp, (void*)txn, "deduce-txn", &deduce_txn_finalizer, true, false);
    if (txn_v == null) {
        deduce_txn_finalize(txn);
        mem::free(txn);
        return deduce_raise(interp, "deduce/block-out-of-memory", "deduce-block: out of memory");
    }
    return txn_v;
}

fn Value* prim_deduce_commit(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/commit-expected-transaction", "deduce-commit: expected (deduce 'commit txn)");
    }
    if (!deduce_ffi_handle_name_equals(args[0], "deduce-txn")) {
        return deduce_raise(interp, "deduce/commit-transaction-not-handle", "deduce-commit: first argument must be a deduce transaction handle");
    }
    if (args[0].ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/commit-transaction-handle-closed", "deduce-commit: transaction handle is closed");
    }

    DeduceTxn* txn = (DeduceTxn*)args[0].ffi_val.lib_handle;
    if (!txn.open || txn.txn == null) {
        return deduce_raise(interp, "deduce/commit-transaction-closed", "deduce-commit: transaction is already closed");
    }
    if (txn.defer_integrity_checks) {
        Value* integrity_err = deduce_txn_validate_deferred_integrity(txn, interp);
        if (integrity_err != null) {
            deduce_txn_finalize(txn);
            return integrity_err;
        }
    }

    MdbTxn* raw_txn = txn.txn;
    DeduceDb* owner_db = txn.db;
    bool mutation_pending = txn.mutation_pending;
    txn.txn = null;
    txn.open = false;
    txn.db = null;
    int rc = mdb_txn_commit(raw_txn);
    if (rc != MDB_SUCCESS) {
        txn.mutation_pending = false;
        deduce_txn_clear_mutation_log(txn);
        return deduce_raise(interp, "deduce/txn-failed", "deduce-commit: txn commit failed");
    }
    deduce_txn_apply_mutation_log(txn);
    if (mutation_pending && owner_db != null) {
        Value* mutation_err = deduce_db_note_untracked_write_commit(owner_db, interp);
        if (mutation_err != null) {
            txn.mutation_pending = false;
            deduce_txn_clear_mutation_log(txn);
            return mutation_err;
        }
    }
    txn.mutation_pending = false;
    deduce_txn_clear_mutation_log(txn);
    return make_void(interp);
}

fn Value* prim_deduce_abort(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/abort-expected-transaction", "deduce-abort: expected (deduce 'abort txn)");
    }
    if (!deduce_ffi_handle_name_equals(args[0], "deduce-txn")) {
        return deduce_raise(interp, "deduce/abort-transaction-not-handle", "deduce-abort: first argument must be a deduce transaction handle");
    }
    if (args[0].ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/abort-transaction-handle-closed", "deduce-abort: transaction handle is closed");
    }

    DeduceTxn* txn = (DeduceTxn*)args[0].ffi_val.lib_handle;
    deduce_txn_finalize(txn);
    return make_void(interp);
}

// ============================================================
// Namespaced command aliases (deduce/<command> ...)
// ============================================================
fn Value* prim_deduce_ns_open(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_open(args, env, interp);
}

fn Value* prim_deduce_ns_open_named(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_open_named(args, env, interp);
}

fn Value* prim_deduce_ns_block(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_block(args, env, interp);
}

fn Value* prim_deduce_ns_commit(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_commit(args, env, interp);
}

fn Value* prim_deduce_ns_abort(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_abort(args, env, interp);
}

fn Value* prim_deduce_ns_scan(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_scan(args, env, interp);
}

fn Value* prim_deduce_ns_scan_range(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_scan_range(args, env, interp);
}

fn Value* prim_deduce_ns_query(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_query(args, env, interp);
}

fn Value* prim_deduce_ns_schema(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_schema(args, env, interp);
}

fn Value* prim_deduce_ns_indexes(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_indexes(args, env, interp);
}

fn Value* prim_deduce_ns_stats(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_stats(args, env, interp);
}

fn Value* prim_deduce_ns_count(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_count(args, env, interp);
}

fn Value* prim_deduce_ns_match(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_match(args, env, interp);
}

fn Value* prim_deduce_ns_fact(Value*[] args, Env* env, Interp* interp) {
    return prim_assert_fact(args, env, interp);
}

fn Value* prim_deduce_ns_retract(Value*[] args, Env* env, Interp* interp) {
    return prim_retract_fact(args, env, interp);
}

fn Value* prim_deduce_ns_clear(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_clear(args, env, interp);
}

fn Value* prim_deduce_ns_drop(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_drop(args, env, interp);
}

fn Value* prim_deduce_ns_rule(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_rule(args, env, interp);
}

fn Value* prim_deduce_ns_explain(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_explain(args, env, interp);
}

fn Value* prim_deduce_ns_why_result(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_why_result(args, env, interp);
}

fn Value* prim_deduce_ns_analyze(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_analyze(args, env, interp);
}

fn Value* prim_deduce_ns_materialize(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_materialize(args, env, interp);
}

fn Value* prim_deduce_ns_dematerialize(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_dematerialize(args, env, interp);
}

fn Value* prim_deduce_ns_refresh(Value*[] args, Env* env, Interp* interp) {
    return prim_deduce_refresh(args, env, interp);
}

// ============================================================
// Dispatch helpers
// ============================================================
fn bool deduce_cmd_matches(char[] name, char[] cmd) {
    return deduce_name_equals(name, cmd);
}

fn Value* prim_deduce_dispatch(char[] name, Value*[] rest, Env* env, Interp* interp) {
    if (deduce_cmd_matches(name, "open")) return prim_deduce_open(rest, env, interp);
    if (deduce_cmd_matches(name, "open-named")) return prim_deduce_open_named(rest, env, interp);
    if (deduce_cmd_matches(name, "block")) return prim_deduce_block(rest, env, interp);
    if (deduce_cmd_matches(name, "commit")) return prim_deduce_commit(rest, env, interp);
    if (deduce_cmd_matches(name, "abort")) return prim_deduce_abort(rest, env, interp);
    if (deduce_cmd_matches(name, "scan")) return prim_deduce_scan(rest, env, interp);
    if (deduce_cmd_matches(name, "scan-range")) return prim_deduce_scan_range(rest, env, interp);
    if (deduce_cmd_matches(name, "query")) return prim_deduce_query(rest, env, interp);
    if (deduce_cmd_matches(name, "schema")) return prim_deduce_schema(rest, env, interp);
    if (deduce_cmd_matches(name, "indexes")) return prim_deduce_indexes(rest, env, interp);
    if (deduce_cmd_matches(name, "stats")) return prim_deduce_stats(rest, env, interp);
    if (deduce_cmd_matches(name, "materialize!")) return prim_deduce_materialize(rest, env, interp);
    if (deduce_cmd_matches(name, "dematerialize!")) return prim_deduce_dematerialize(rest, env, interp);
    if (deduce_cmd_matches(name, "refresh!")) return prim_deduce_refresh(rest, env, interp);
    if (deduce_cmd_matches(name, "count")) return prim_deduce_count(rest, env, interp);
    if (deduce_cmd_matches(name, "match")) return prim_deduce_match(rest, env, interp);
    if (deduce_cmd_matches(name, "fact!")) return prim_assert_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "retract!")) return prim_retract_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "clear!")) return prim_deduce_clear(rest, env, interp);
    if (deduce_cmd_matches(name, "drop!")) return prim_deduce_drop(rest, env, interp);
    if (deduce_cmd_matches(name, "rule!")) return prim_deduce_rule(rest, env, interp);
    if (deduce_cmd_matches(name, "explain")) return prim_deduce_explain(rest, env, interp);
    if (deduce_cmd_matches(name, "why-result")) return prim_deduce_why_result(rest, env, interp);
    if (deduce_cmd_matches(name, "analyze")) return prim_deduce_analyze(rest, env, interp);
    return null;
}

// ============================================================
// (deduce 'cmd args...) — unified dispatch
//
// (deduce 'open 'memory)           → database handle
// (deduce 'open "/path")           → database handle
// (deduce 'open-named db rel-name db-name col...) → relation handle
// (deduce 'block db ['read|'write|'write-deferred]) → transaction handle
// (deduce 'commit txn)             → Void
// (deduce 'abort txn)              → Void
// (deduce 'scan relation)          → list of dicts
// (deduce 'scan-range relation lower-bound upper-bound) → list of dicts
// (deduce 'query relation fn)      → filtered list of dicts
// (deduce 'schema relation)        → relation schema payload
// (deduce 'indexes relation)       → registered logical indexes
// (deduce 'stats relation)         → relation statistics payload
// (deduce 'materialize! relation)  → Void
// (deduce 'dematerialize! relation) → Void
// (deduce 'refresh! db)            → refresh payload
// (deduce 'count relation)         → int
// (deduce 'match relation pattern) → list of dicts
// (deduce 'fact! relation vals...) → Void
// (deduce 'retract! relation vals...)  → Void
// (deduce 'retract! txn relation vals...) → Void
// (deduce 'clear! relation)        → Void
// (deduce 'drop! relation)         → Void
// (deduce 'rule! db head-atom body-atom...) → Void
// (deduce 'explain db [head-symbol|rule-index]) → deterministic plan payload
// (deduce 'why-result relation vals...) → provenance payload for one stored row
// (deduce 'analyze db)             → deterministic evaluator diagnostics payload
// ============================================================

fn Value* prim_deduce(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return deduce_raise(interp, "deduce/dispatch-expected-command", "deduce: expected (deduce 'command args...)");

    Value* cmd = args[0];
    if (cmd == null || cmd.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/dispatch-command-not-symbol", "deduce: first argument must be a quoted symbol");
    }

    char[] name = interp.symbols.get_name(cmd.sym_val);

    Value* out = prim_deduce_dispatch(name, args[1..], env, interp);
    if (out != null) return out;

    return deduce_raise(interp, "deduce/dispatch-unknown-command", "deduce: unknown command (expected 'open 'open-named 'block 'commit 'abort 'scan 'scan-range 'query 'schema 'indexes 'stats 'materialize! 'dematerialize! 'refresh! 'count 'match 'fact! 'retract! 'clear! 'drop! 'rule! 'explain 'why-result 'analyze)");
}
