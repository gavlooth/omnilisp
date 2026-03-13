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
    SymbolId     index_col;     // indexed column (0 = none)
    bool         dropped;       // true after relation database is dropped
}

fn void deduce_relation_free(Relation* rel) @inline {
    if (rel == null) return;
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
// (deduce 'block db ['read|'write]) -> transaction handle
// (deduce 'commit txn) -> Void
// (deduce 'abort txn) -> Void
// ============================================================
fn Value* prim_deduce_block(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return deduce_raise(interp, "deduce/block-expected-database", "deduce-block: expected (deduce 'block db ['read|'write])");
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
    if (args.len == 2) {
        Value* mode = args[1];
        if (mode == null || mode.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/block-mode-not-symbol", "deduce-block: mode must be 'read or 'write");
        }
        char[] mode_name = interp.symbols.get_name(mode.sym_val);
        if (deduce_name_equals(mode_name, "read")) {
            flags = MDB_RDONLY;
            read_only = true;
        } else if (deduce_name_equals(mode_name, "write")) {
            flags = 0;
            read_only = false;
        } else {
            return deduce_raise(interp, "deduce/block-invalid-mode", "deduce-block: mode must be 'read or 'write");
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

    MdbTxn* raw_txn = txn.txn;
    txn.txn = null;
    txn.open = false;
    txn.db = null;
    int rc = mdb_txn_commit(raw_txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce-commit: txn commit failed");
    }
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
    if (deduce_cmd_matches(name, "count")) return prim_deduce_count(rest, env, interp);
    if (deduce_cmd_matches(name, "match")) return prim_deduce_match(rest, env, interp);
    if (deduce_cmd_matches(name, "fact!")) return prim_assert_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "retract!")) return prim_retract_fact(rest, env, interp);
    if (deduce_cmd_matches(name, "clear!")) return prim_deduce_clear(rest, env, interp);
    if (deduce_cmd_matches(name, "drop!")) return prim_deduce_drop(rest, env, interp);
    return null;
}

// ============================================================
// (deduce 'cmd args...) — unified dispatch
//
// (deduce 'open 'memory)           → database handle
// (deduce 'open "/path")           → database handle
// (deduce 'open-named db rel-name db-name col...) → relation handle
// (deduce 'block db ['read|'write]) → transaction handle
// (deduce 'commit txn)             → Void
// (deduce 'abort txn)              → Void
// (deduce 'scan relation)          → list of dicts
// (deduce 'scan-range relation lower-bound upper-bound) → list of dicts
// (deduce 'query relation fn)      → filtered list of dicts
// (deduce 'count relation)         → int
// (deduce 'match relation pattern) → list of dicts
// (deduce 'fact! relation vals...) → Void
// (deduce 'retract! relation vals...)  → Void
// (deduce 'clear! relation)        → Void
// (deduce 'drop! relation)         → Void
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

    return deduce_raise(interp, "deduce/dispatch-unknown-command", "deduce: unknown command (expected 'open 'open-named 'block 'commit 'abort 'scan 'scan-range 'query 'count 'match 'fact! 'retract! 'clear! 'drop!)");
}
