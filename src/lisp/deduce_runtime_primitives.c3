module lisp;

// ============================================================
// (deduce 'block db ['read|'write|'write-deferred]) -> transaction handle
// (deduce 'commit txn) -> Void
// (deduce 'abort txn) -> Void
// ============================================================
<*
@require interp != null
*>
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
        return deduce_raise(interp, "deduce/block-out-of-memory", "deduce-block: out of memory");
    }
    return txn_v;
}

<*
@require interp != null
*>
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
    if (!mutation_pending) {
        deduce_txn_apply_mutation_log(txn);
    }
    if (mutation_pending && owner_db != null) {
        Value* mutation_err = deduce_db_note_untracked_write_commit(owner_db, interp);
        if (mutation_err != null) {
            txn.mutation_pending = false;
            deduce_txn_clear_mutation_log(txn);
            return mutation_err;
        }
    } else if (mutation_pending) {
        txn.mutation_pending = false;
        deduce_txn_clear_mutation_log(txn);
        return deduce_raise(
            interp,
            "deduce/commit-mutation-bookkeeping-missing-db",
            "deduce-commit: transaction committed with degraded mutation log but owner database was unavailable"
        );
    }
    txn.mutation_pending = false;
    deduce_txn_clear_mutation_log(txn);
    return make_void(interp);
}

<*
@require interp != null
*>
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
