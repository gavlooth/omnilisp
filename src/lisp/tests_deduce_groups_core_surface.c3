module lisp;

import std::io;

fn void run_deduce_open_smoke_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run("(deduce 'open 'memory)", interp);
    bool ok = !r.error.has_error &&
        r.value != null &&
        r.value.tag == FFI_HANDLE &&
        r.value.ffi_val != null;
    if (ok) {
        emit_pass_literal("[PASS] deduce 'open returns handle");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            char[] msg = r.error.message[:256];
            io::printfn("[FAIL] deduce 'open returns handle (error: %s)", (ZString)msg);
        } else {
            io::printfn("[FAIL] deduce 'open returns handle (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_namespaced_alias_smoke_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define adb (deduce/open 'memory)) "
        "  (define arel (deduce/open-named adb 'people \"people\" 'name 'age)) "
        "  (deduce/fact! arel \"Alice\" 30) "
        "  (define txr (deduce/block adb 'read)) "
        "  (deduce/abort txr) "
        "  (define txw (deduce/block adb 'write)) "
        "  (deduce/commit txw) "
        "  (define m (deduce/match arel '(people ?name ?age))) "
        "  (define q (deduce/query arel (lambda (row) true))) "
        "  (define s (deduce/scan arel)) "
        "  (define sr (deduce/scan-range arel '(\"Alice\" 30) '(\"Alice\" 30))) "
        "  (deduce/retract! arel \"Alice\" 30) "
        "  (define after-retract (deduce/count arel)) "
        "  (deduce/fact! arel \"Alice\" 30) "
        "  (deduce/clear! arel) "
        "  (define after-clear (deduce/count arel)) "
        "  (deduce/drop! arel) "
        "  (and (= (length m) 1) "
        "       (and (= (length q) 1) "
        "            (and (= (length s) 1) "
        "                 (and (= (length sr) 1) "
        "                      (and (= after-retract 0) "
        "                           (= after-clear 0)))))))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce namespaced aliases");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce namespaced aliases (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] deduce namespaced aliases (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_open_named_test(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define ndb (deduce 'open 'memory)) "
            "  (define rel-a (deduce 'open-named ndb 'nrel_a \"named_a\" 'name 'age)) "
            "  (define rel-b (deduce 'open-named ndb 'nrel_b \"named_b\" 'name 'age)) "
            "  (deduce 'fact! rel-a \"Alice\" 30) "
            "  (and (= (deduce 'count rel-a) 1) (= (deduce 'count rel-b) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named isolates db names");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named isolates db names");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define ndb2 (deduce 'open 'memory)) "
            "  (define rel-c (deduce 'open-named ndb2 'nrel_c 'named_c 'name 'age)) "
            "  (= (deduce 'count rel-c) 0))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce open-named accepts symbol db-name");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named accepts symbol db-name");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(deduce 'open-named (deduce 'open 'memory) 'bad 42 'name)",
            interp
        );
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce open-named validates db-name type");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named validates db-name type");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'open 'memory)", interp);
        bool ok = !r.error.has_error &&
            r.value != null &&
            r.value.tag == FFI_HANDLE &&
            r.value.ffi_val != null &&
            r.value.ffi_val.lib_handle != null;
        if (!ok) {
            io::printn("[FAIL] deduce open-named surfaces materialized metadata read failures (setup)");
            (*fail)++;
            return;
        }

        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        Value* db_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false, false);
        if (db_handle == null || db_handle.tag == ERROR) {
            io::printn("[FAIL] deduce open-named surfaces materialized metadata read failures (handle)");
            (*fail)++;
            return;
        }
        SymbolId rel_name = interp.symbols.intern("deduce_meta_fail_closed");
        Value* rel_name_v = make_symbol(interp, rel_name);
        Value* db_name_v = make_string(interp, "deduce_meta_fail_closed");
        Value* col_name_v = make_symbol(interp, interp.symbols.intern("name"));

        bool saved_force = g_deduce_force_materialized_meta_read_fail;
        g_deduce_force_materialized_meta_read_fail = true;

        Value*[4] open_args = { db_handle, rel_name_v, db_name_v, col_name_v };
        Value* open_err = prim_deduce_open_named(open_args[0:4], null, interp);
        Value*[3] define_args = { db_handle, rel_name_v, col_name_v };
        Value* define_err = prim_define_relation(define_args[0:3], null, interp);

        g_deduce_force_materialized_meta_read_fail = saved_force;

        bool open_ok = compiler_error_value_contains(open_err, "forced materialized relation metadata read failure");
        bool define_ok = compiler_error_value_contains(define_err, "forced materialized relation metadata read failure");
        if (open_ok && define_ok) {
            emit_pass_literal("[PASS] deduce open-named and define-relation fail closed on materialized metadata read failures");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named and define-relation fail closed on materialized metadata read failures");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'open 'memory)", interp);
        bool ok = !r.error.has_error &&
            r.value != null &&
            r.value.tag == FFI_HANDLE &&
            r.value.ffi_val != null &&
            r.value.ffi_val.lib_handle != null;
        if (!ok) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted stale reason (setup)");
            (*fail)++;
            return;
        }

        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        SymbolId rel_name = interp.symbols.intern("deduce_meta_invalid_stale_reason");
        bool saved_invalid_reason = g_deduce_force_materialized_meta_invalid_stale_reason;
        g_deduce_force_materialized_meta_invalid_stale_reason = true;
        Value* persist_err = deduce_relation_meta_persist_state(
            db,
            rel_name,
            true,
            interp.symbols.intern("manual"),
            0,
            0,
            0,
            DEDUCE_MATERIALIZED_STALE_NONE,
            false,
            interp
        );
        g_deduce_force_materialized_meta_invalid_stale_reason = saved_invalid_reason;
        if (persist_err != null && persist_err.tag == ERROR) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted stale reason (persist)");
            (*fail)++;
            return;
        }

        Value* db_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false, false);
        if (db_handle == null || db_handle.tag == ERROR) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted stale reason (handle)");
            (*fail)++;
            return;
        }
        Value* rel_name_v = make_symbol(interp, rel_name);
        Value* db_name_v = make_string(interp, "deduce_meta_invalid_stale_reason");
        Value* col_name_v = make_symbol(interp, interp.symbols.intern("name"));

        Value*[4] open_args = { db_handle, rel_name_v, db_name_v, col_name_v };
        Value* open_err = prim_deduce_open_named(open_args[0:4], null, interp);
        Value*[3] define_args = { db_handle, rel_name_v, col_name_v };
        Value* define_err = prim_define_relation(define_args[0:3], null, interp);

        bool open_ok = compiler_error_value_contains(open_err, "persisted relation metadata contains invalid stale reason");
        bool define_ok = compiler_error_value_contains(define_err, "persisted relation metadata contains invalid stale reason");
        if (open_ok && define_ok) {
            emit_pass_literal("[PASS] deduce open-named and define-relation reject invalid persisted stale reasons");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named and define-relation reject invalid persisted stale reasons");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'open 'memory)", interp);
        bool ok = !r.error.has_error &&
            r.value != null &&
            r.value.tag == FFI_HANDLE &&
            r.value.ffi_val != null &&
            r.value.ffi_val.lib_handle != null;
        if (!ok) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted refresh policy (setup)");
            (*fail)++;
            return;
        }

        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        SymbolId rel_name = interp.symbols.intern("deduce_meta_invalid_refresh_policy");
        bool saved_invalid_policy = g_deduce_force_materialized_meta_invalid_refresh_policy;
        g_deduce_force_materialized_meta_invalid_refresh_policy = true;
        Value* persist_err = deduce_relation_meta_persist_state(
            db,
            rel_name,
            true,
            interp.symbols.intern("manual"),
            0,
            0,
            0,
            DEDUCE_MATERIALIZED_STALE_NONE,
            false,
            interp
        );
        g_deduce_force_materialized_meta_invalid_refresh_policy = saved_invalid_policy;
        if (persist_err != null && persist_err.tag == ERROR) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted refresh policy (persist)");
            (*fail)++;
            return;
        }

        Value* db_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false, false);
        if (db_handle == null || db_handle.tag == ERROR) {
            io::printn("[FAIL] deduce open-named rejects invalid persisted refresh policy (handle)");
            (*fail)++;
            return;
        }
        Value* rel_name_v = make_symbol(interp, rel_name);
        Value* db_name_v = make_string(interp, "deduce_meta_invalid_refresh_policy");
        Value* col_name_v = make_symbol(interp, interp.symbols.intern("name"));

        Value*[4] open_args = { db_handle, rel_name_v, db_name_v, col_name_v };
        Value* open_err = prim_deduce_open_named(open_args[0:4], null, interp);
        Value*[3] define_args = { db_handle, rel_name_v, col_name_v };
        Value* define_err = prim_define_relation(define_args[0:3], null, interp);

        bool open_ok = compiler_error_value_contains(open_err, "persisted relation metadata contains invalid refresh policy");
        bool define_ok = compiler_error_value_contains(define_err, "persisted relation metadata contains invalid refresh policy");
        if (open_ok && define_ok) {
            emit_pass_literal("[PASS] deduce open-named and define-relation reject invalid persisted refresh policies");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named and define-relation reject invalid persisted refresh policies");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'open 'memory)", interp);
        bool ok = !r.error.has_error &&
            r.value != null &&
            r.value.tag == FFI_HANDLE &&
            r.value.ffi_val != null &&
            r.value.ffi_val.lib_handle != null;
        if (!ok) {
            io::printn("[FAIL] deduce open-named accepts legacy V1 materialized metadata without refresh policy (setup)");
            (*fail)++;
            return;
        }

        DeduceDb* db = (DeduceDb*)r.value.ffi_val.lib_handle;
        SymbolId rel_name = interp.symbols.intern("deduce_meta_v1_legacy_policy");
        char[] rel_name_str = interp.symbols.get_name(rel_name);
        MdbTxn* txn = null;
        int rc = mdb_txn_begin(db.env, null, 0, &txn);
        MdbDbi meta_dbi = 0;
        if (rc == MDB_SUCCESS) rc = deduce_open_relation_meta_dbi_rc(db, txn, true, &meta_dbi);
        DeducePersistedRelationMetaV1 meta = {
            .version = 1,
            .flags = DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED
        };
        MdbVal key = {
            .mv_size = rel_name_str.len,
            .mv_data = rel_name_str.ptr
        };
        MdbVal data = {
            .mv_size = DeducePersistedRelationMetaV1.sizeof,
            .mv_data = &meta
        };
        if (rc == MDB_SUCCESS) rc = mdb_put(txn, meta_dbi, &key, &data, 0);
        if (rc == MDB_SUCCESS) {
            rc = mdb_txn_commit(txn);
        } else if (txn != null) {
            mdb_txn_abort(txn);
        }
        if (rc != MDB_SUCCESS) {
            io::printn("[FAIL] deduce open-named accepts legacy V1 materialized metadata without refresh policy (persist)");
            (*fail)++;
            return;
        }

        Value* db_handle = make_ffi_handle_ex(interp, (void*)db, "deduce-db", null, false, false);
        Value* rel_name_v = make_symbol(interp, rel_name);
        Value* db_name_v = make_string(interp, "deduce_meta_v1_legacy_policy");
        Value* col_name_v = make_symbol(interp, interp.symbols.intern("name"));
        Value*[4] open_args = { db_handle, rel_name_v, db_name_v, col_name_v };
        Value* opened = prim_deduce_open_named(open_args[0:4], null, interp);

        bool open_ok = opened != null && opened.tag == FFI_HANDLE;
        if (open_ok) {
            emit_pass_literal("[PASS] deduce open-named accepts legacy V1 materialized metadata without refresh policy");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce open-named accepts legacy V1 materialized metadata without refresh policy");
            (*fail)++;
        }
    }

    {
        uint pid = (uint)deduce_test_getpid();
        usz seq = g_deduce_test_long_path_seq++;

        char[512] db_path_buf;
        char[] db_path;
        if (try char[] rendered = io::bprintf(
            &db_path_buf,
            "/tmp/omni-deduce-meta-restore-fail-%d-%d",
            (int)pid,
            (int)seq
        )) {
            db_path = rendered;
        } else {
            io::printn("[FAIL] deduce open restores materialized metadata read failures (db path)");
            (*fail)++;
            return;
        }
        db_path_buf[db_path.len] = 0;
        defer deduce_test_unlink(&db_path_buf[0]);

        char[640] lock_path_buf;
        if (try char[] rendered_lock = io::bprintf(&lock_path_buf, "%s-lock", (ZString)db_path)) {
            lock_path_buf[rendered_lock.len] = 0;
            defer deduce_test_unlink(&lock_path_buf[0]);
        }

        char[512] script_path_buf;
        char[] script_path;
        if (try char[] rendered = io::bprintf(
            &script_path_buf,
            "/tmp/omni-deduce-meta-restore-fail-%d-%d.omni",
            (int)pid,
            (int)seq
        )) {
            script_path = rendered;
        } else {
            io::printn("[FAIL] deduce open restores materialized metadata read failures (script path)");
            (*fail)++;
            return;
        }
        script_path_buf[script_path.len] = 0;
        defer deduce_test_unlink(&script_path_buf[0]);

        char[2048] script_buf;
        char[] script;
        if (try char[] rendered = io::bprintf(
            &script_buf,
            "(block "
            "  (define db-restore-meta-fail (deduce 'open \"%s\")) "
            "  (define [relation db-restore-meta-fail] edge_restore_meta_fail (src) (dst)) "
            "  (define [relation db-restore-meta-fail materialized] ancestor_restore_meta_fail (src) (dst)) "
            "  (deduce 'fact! edge_restore_meta_fail 1 2) "
            "  (deduce/rule! db-restore-meta-fail '(ancestor_restore_meta_fail ?x ?y) '(edge_restore_meta_fail ?x ?y)) "
            "  (deduce/materialize! ancestor_restore_meta_fail) "
            "  true)",
            (ZString)db_path
        )) {
            script = rendered;
        } else {
            io::printn("[FAIL] deduce open restores materialized metadata read failures (script render)");
            (*fail)++;
            return;
        }

        if (!write_text_file(script_path, script) || !deduce_test_run_main_script(script_path)) {
            io::printn("[FAIL] deduce open restores materialized metadata read failures (setup)");
            (*fail)++;
            return;
        }

        char[768] open_buf;
        char[] open_src;
        if (try char[] rendered = io::bprintf(&open_buf, "(deduce 'open \"%s\")", (ZString)db_path)) {
            open_src = rendered;
        } else {
            io::printn("[FAIL] deduce open restores materialized metadata read failures (open render)");
            (*fail)++;
            return;
        }

        bool saved_force = g_deduce_force_materialized_meta_read_fail;
        g_deduce_force_materialized_meta_read_fail = true;
        test_error_contains(
            interp,
            "deduce open restores materialized metadata read failures",
            open_src,
            "forced materialized relation metadata read failure",
            pass,
            fail
        );
        g_deduce_force_materialized_meta_read_fail = saved_force;
    }
}

fn void run_deduce_relation_open_commit_failure_test(Interp* interp, int* pass, int* fail) {
    bool saved_force_commit_fail = g_deduce_force_relation_open_commit_fail;
    g_deduce_force_relation_open_commit_fail = true;
    EvalResult open_named_result = run(
        "(handle (deduce 'open-named (deduce 'open 'memory) 'force_commit_fail \"force_commit_fail\" 'name) "
        "  (raise payload (String (ref payload 'code))))",
        interp
    );
    EvalResult define_relation_result = run(
        "(handle (block "
        "    (define db-force-commit-fail (deduce 'open 'memory)) "
        "    (__define-relation db-force-commit-fail 'force_commit_fail_rel 'name)) "
        "  (raise payload (String (ref payload 'code))))",
        interp
    );
    g_deduce_force_relation_open_commit_fail = saved_force_commit_fail;

    bool open_named_ok = !open_named_result.error.has_error &&
        open_named_result.value != null &&
        open_named_result.value.tag == STRING &&
        deduce_name_equals(open_named_result.value.str_chars[:open_named_result.value.str_len], "deduce/open-named-txn-commit-failed");
    bool define_relation_ok = !define_relation_result.error.has_error &&
        define_relation_result.value != null &&
        define_relation_result.value.tag == STRING &&
        deduce_name_equals(
            define_relation_result.value.str_chars[:define_relation_result.value.str_len],
            "deduce/define-relation-txn-commit-failed"
        );

    if (open_named_ok && define_relation_ok) {
        emit_pass_literal("[PASS] deduce relation-open surfaces txn commit failure for open-named and define-relation");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce relation-open surfaces txn commit failure for open-named and define-relation");
        (*fail)++;
    }
}

fn void run_deduce_non_transaction_mutation_post_commit_atomic_surface_test(Interp* interp, int* pass, int* fail) {
    EvalResult setup = run(
        "(block "
        "  (define db-post-commit-atomic (deduce 'open 'memory)) "
        "  (define rel-post-commit-atomic (deduce 'open-named db-post-commit-atomic 'post_commit_atomic \"post_commit_atomic\" 'id)) "
        "  (define [relation db-post-commit-atomic materialized] rel-post-commit-atomic-mv (id)) "
        "  true)",
        interp
    );
    if (setup.error.has_error) {
        io::printn("[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures (setup)");
        (*fail)++;
        return;
    }

    bool saved_force_meta_fail = g_deduce_force_materialized_meta_persist_fail;
    g_deduce_force_materialized_meta_persist_fail = true;
    EvalResult r = run(
        "(block "
        "  (define mutation-result (handle (deduce 'fact! rel-post-commit-atomic 7) (raise payload (String (ref payload 'code))))) "
        "  (and (= (type-of mutation-result) 'Void) (= (deduce 'count rel-post-commit-atomic) 1)))",
        interp
    );
    g_deduce_force_materialized_meta_persist_fail = saved_force_meta_fail;

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce non-transaction mutations keep success surface after post-commit metadata failures");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn(
                "[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures (error: %s)",
                (ZString)r.error.message[:256]
            );
        } else {
            io::printn("[FAIL] deduce non-transaction mutations keep success surface after post-commit metadata failures");
        }
        (*fail)++;
    }
}

fn void run_deduce_transaction_mutation_log_failure_bookkeeping_test(Interp* interp, int* pass, int* fail) {
    bool saved_force_log_fail = g_deduce_force_txn_mutation_log_fail;
    g_deduce_force_txn_mutation_log_fail = true;
    EvalResult r = run(
        "(block "
        "  (define db-txn-log-fail (deduce 'open 'memory)) "
        "  (define rel-txn-log-fail (deduce 'open-named db-txn-log-fail 'txn_log_fail \"txn_log_fail\" 'id)) "
        "  (define tx-txn-log-fail (deduce 'block db-txn-log-fail)) "
        "  (deduce 'fact! tx-txn-log-fail rel-txn-log-fail 1) "
        "  (define commit-result (handle (deduce 'commit tx-txn-log-fail) (raise payload (String (ref payload 'code))))) "
        "  (define analyzed (deduce 'analyze db-txn-log-fail)) "
        "  (and (= (type-of commit-result) 'Void) "
        "       (= (deduce 'count rel-txn-log-fail) 1) "
        "       (= analyzed.incremental-invalidation-mode 'full-recompute)))",
        interp
    );
    g_deduce_force_txn_mutation_log_fail = saved_force_log_fail;

    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce transaction commit records full-recompute bookkeeping when mutation logging fails");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn(
                "[FAIL] deduce transaction commit records full-recompute bookkeeping when mutation logging fails (error: %s)",
                (ZString)r.error.message[:256]
            );
        } else {
            io::printn("[FAIL] deduce transaction commit records full-recompute bookkeeping when mutation logging fails");
        }
        (*fail)++;
    }
}

fn void run_deduce_fact_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vfact (deduce 'open 'memory)) "
        "  (define [relation db-vfact] rel-vfact (name) (age)) "
        "  (deduce 'fact! rel-vfact \"Alice\" 30))",
        "[PASS] deduce fact! returns Void",
        "[FAIL] deduce fact! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_retract_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vretract (deduce 'open 'memory)) "
        "  (define [relation db-vretract] rel-vretract (name) (age)) "
        "  (deduce 'fact! rel-vretract \"Alice\" 30) "
        "  (deduce 'retract! rel-vretract \"Alice\" 30))",
        "[PASS] deduce retract! returns Void",
        "[FAIL] deduce retract! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_commit_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vcommit (deduce 'open 'memory)) "
        "  (define [relation db-vcommit] rel-vcommit (name) (age)) "
        "  (define tx (deduce 'block db-vcommit)) "
        "  (deduce 'fact! tx rel-vcommit \"Alice\" 30) "
        "  (deduce 'commit tx))",
        "[PASS] deduce commit returns Void",
        "[FAIL] deduce commit returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_abort_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vabort (deduce 'open 'memory)) "
        "  (define [relation db-vabort] rel-vabort (name) (age)) "
        "  (define tx (deduce 'block db-vabort 'read)) "
        "  (deduce 'abort tx))",
        "[PASS] deduce abort returns Void",
        "[FAIL] deduce abort returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_clear_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vclear (deduce 'open 'memory)) "
        "  (define [relation db-vclear] rel-vclear (name) (age)) "
        "  (deduce 'fact! rel-vclear \"Alice\" 30) "
        "  (deduce 'clear! rel-vclear))",
        "[PASS] deduce clear! returns Void",
        "[FAIL] deduce clear! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_drop_void_return_test(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_void(
        interp,
        "(block "
        "  (define db-vdrop (deduce 'open 'memory)) "
        "  (define [relation db-vdrop] rel-vdrop (name) (age)) "
        "  (deduce 'drop! rel-vdrop))",
        "[PASS] deduce drop! returns Void",
        "[FAIL] deduce drop! returns Void (tag=%d)",
        pass,
        fail
    );
}

fn void run_deduce_command_void_return_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_fact_void_return_test(interp, pass, fail);
    run_deduce_retract_void_return_test(interp, pass, fail);
    run_deduce_commit_void_return_test(interp, pass, fail);
    run_deduce_abort_void_return_test(interp, pass, fail);
    run_deduce_clear_void_return_test(interp, pass, fail);
    run_deduce_drop_void_return_test(interp, pass, fail);
}

fn void run_deduce_crud_example_flow_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (load \"examples/deduce_crud_server.omni\") "
        "  (let (create1 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) "
        "        create2 (pipeline/result->response (repo/create {'id 101 'name \"paper-dup\" 'qty 3})) "
        "        existing (repo/get-by-id 101) "
        "        missing (repo/get-by-id 9999)) "
        "    (and (= (ref create1 'status) \"ok\") "
        "         (and (= (ref create2 'status) \"error\") "
        "              (and (= (ref create2 'error) \"item already exists\") "
        "                   (and (= (ref (to-json existing) 'name) \"paper\") "
        "                        (null? missing)))))))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce CRUD example flow");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce CRUD example flow (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printfn("[FAIL] deduce CRUD example flow (tag=%d)", r.value != null ? (int)r.value.tag : -1);
        }
        (*fail)++;
    }
}

fn void run_deduce_legacy_alias_rejection_tests(Interp* interp, int* pass, int* fail) {
    test_error(interp, "deduce-open rejected", "(deduce-open 'memory)", pass, fail);
    test_error(interp, "fact! rejected", "(fact! nil)", pass, fail);
    test_error(interp, "retract! rejected", "(retract! nil)", pass, fail);
    test_error(interp, "deduce-scan rejected", "(deduce-scan nil)", pass, fail);
    test_error(interp, "deduce-query rejected", "(deduce-query nil nil)", pass, fail);
    test_error(interp, "deduce-count rejected", "(deduce-count nil)", pass, fail);
    test_error(interp, "deduce-match rejected", "(deduce-match nil nil)", pass, fail);
}
