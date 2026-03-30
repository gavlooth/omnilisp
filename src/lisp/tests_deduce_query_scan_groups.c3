module lisp;

import std::io;

fn bool deduce_bench_enabled() @inline {
    return test_c_getenv("OMNI_DEDUCE_BENCH") != null;
}

fn long deduce_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn Value* deduce_bench_make_bound_tuple(Interp* interp, long id, long score) {
    return make_cons(
        interp,
        make_int(interp, id),
        make_cons(interp, make_int(interp, score), make_nil(interp))
    );
}

fn bool deduce_scan_range_oom_probe_enabled() @inline {
    return test_c_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null;
}

fn Relation* deduce_test_prepare_relation_handle(
    Interp* interp,
    char[] source,
    char[] fail_msg,
    int* fail
) {
    EvalResult rel_run = run(source, interp);
    if (rel_run.error.has_error ||
        rel_run.value == null ||
        rel_run.value.tag != FFI_HANDLE ||
        rel_run.value.ffi_val == null ||
        rel_run.value.ffi_val.lib_handle == null) {
        io::printn((ZString)fail_msg);
        (*fail)++;
        return null;
    }
    return (Relation*)rel_run.value.ffi_val.lib_handle;
}

fn bool deduce_test_result_is_oom_code(Value* value) {
    return value != null &&
        value.tag == STRING &&
        deduce_name_equals(value.str_chars[:value.str_len], "deduce/query-out-of-memory");
}

fn void run_deduce_scan_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_int_at_least(
        interp,
        "(length (deduce 'scan person))",
        2,
        "[PASS] deduce 'fact! + 'scan",
        "[FAIL] deduce 'fact! + 'scan (got %d)",
        pass,
        fail
    );

    setup(interp, "(deduce 'retract! person \"Bob\" 25)");
    deduce_test_expect_int_at_least(
        interp,
        "(length (deduce 'scan person))",
        1,
        "[PASS] deduce 'retract!",
        "[FAIL] deduce 'retract! (got %d)",
        pass,
        fail
    );

    deduce_test_expect_string(
        interp,
        "('name (car (deduce 'scan person)))",
        "[PASS] deduce 'scan dict result",
        "[FAIL] deduce 'scan dict result",
        pass,
        fail
    );
}

fn void run_deduce_scan_range_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_scan_range_oom_probe_enabled()) {
        EvalResult rel_run = run(
            "(block "
            "  (define scan-range-db (deduce 'open 'memory)) "
            "  (define [relation scan-range-db] scan_range_person (name) (age) (email)) "
            "  scan_range_person "
            ")",
            interp
        );
        if (rel_run.error.has_error || rel_run.value == null || rel_run.value.tag != FFI_HANDLE || rel_run.value.ffi_val == null || rel_run.value.ffi_val.lib_handle == null) {
            io::printn("[FAIL] deduce 'scan-range row-dict oom probe (failed to prepare relation)");
            (*fail)++;
            return;
        }

        Relation* rel = (Relation*)rel_run.value.ffi_val.lib_handle;
        Value*[3] vals = {
            make_int(interp, 30),
            make_int(interp, 40),
            make_int(interp, 50),
        };
        Value* dict = deduce_relation_materialize_row_dict(
            rel,
            vals[..],
            interp,
            deduce_relation_row_dict_capacity(rel)
        );
        if (dict == null || dict.tag == ERROR) {
            emit_pass_literal("[PASS] deduce 'scan-range row-dict oom probe");
            (*pass)++;
            return;
        }
        io::printn("[FAIL] deduce 'scan-range row-dict oom probe (expected OOM guard)");
        (*fail)++;
        return;
    }

    if (deduce_test_prepare_relation_handle(
        interp,
        "(block "
        "  (define scan-oom-db (deduce 'open 'memory)) "
        "  (define [relation scan-oom-db] scan_oom_person (name) (age) (email)) "
        "  scan_oom_person "
        ")",
        "[FAIL] deduce 'scan column-key-value oom propagation (failed to prepare relation)",
        fail
    ) == null) return;

    bool saved_force_column_key_values_oom = g_deduce_force_column_key_values_oom;
    g_deduce_force_column_key_values_oom = true;
    EvalResult scan_oom = run(
        "(handle (deduce 'scan scan_oom_person) (raise payload (symbol->string (ref payload 'code))))",
        interp
    );
    g_deduce_force_column_key_values_oom = saved_force_column_key_values_oom;
    if (!scan_oom.error.has_error && deduce_test_result_is_oom_code(scan_oom.value)) {
        emit_pass_literal("[PASS] deduce 'scan propagates column-key-value oom");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce 'scan propagates column-key-value oom");
        (*fail)++;
    }

    Relation* range_oom_rel = deduce_test_prepare_relation_handle(
        interp,
        "(block "
        "  (define scan-range-oom-db (deduce 'open 'memory)) "
        "  (define [relation scan-range-oom-db] scan_range_oom_person (name) (age) (email)) "
        "  (deduce 'fact! scan_range_oom_person \"Bob\" 30 \"b@x\") "
        "  scan_range_oom_person "
        ")",
        "[FAIL] deduce 'scan-range row-dict oom propagation (failed to prepare relation)",
        fail
    );
    if (range_oom_rel == null) return;

    bool saved_force_row_dict_oom = g_deduce_force_row_dict_oom;
    g_deduce_force_row_dict_oom = true;
    EvalResult range_oom = run(
        "(handle (deduce 'scan-range scan_range_oom_person '(\"Bob\" 30 \"b@x\") '(\"Bob\" 30 \"b@x\")) "
        "  (raise payload (symbol->string (ref payload 'code))))",
        interp
    );
    g_deduce_force_row_dict_oom = saved_force_row_dict_oom;
    if (!range_oom.error.has_error && deduce_test_result_is_oom_code(range_oom.value)) {
        emit_pass_literal("[PASS] deduce 'scan-range propagates row-dict oom");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce 'scan-range propagates row-dict oom");
        (*fail)++;
    }

    MdbTxn* txn = null;
    int txn_rc = mdb_txn_begin(range_oom_rel.db.env, null, 0, &txn);
    if (txn_rc != MDB_SUCCESS || txn == null) {
        io::printn("[FAIL] deduce scan-range in-txn row-dict oom propagation (failed to open txn)");
        (*fail)++;
        return;
    }
    defer if (txn != null) mdb_txn_abort(txn);
    saved_force_row_dict_oom = g_deduce_force_row_dict_oom;
    g_deduce_force_row_dict_oom = true;
    Value* range_in_txn_oom = relation_scan_range_in_txn(
        range_oom_rel,
        interp,
        txn,
        null,
        0,
        false,
        {},
        {},
        false
    );
    g_deduce_force_row_dict_oom = saved_force_row_dict_oom;
    if (range_in_txn_oom != null && range_in_txn_oom.tag == ERROR) {
        emit_pass_literal("[PASS] deduce scan-range in-txn propagates row-dict oom");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce scan-range in-txn propagates row-dict oom");
        (*fail)++;
    }

    setup(interp,
        "(block "
        "  (define scan-range-db (deduce 'open 'memory)) "
        "  (define [relation scan-range-db] scan_range_person (name) (age) (email)) "
        "  (deduce 'fact! scan_range_person \"Alice\" 20 \"a@x\") "
        "  (deduce 'fact! scan_range_person \"Bob\" 30 \"b@x\") "
        "  (deduce 'fact! scan_range_person \"Carol\" 40 \"c@x\") "
        "  (deduce 'fact! scan_range_person \"Dora\" 50 \"d@x\") "
        ")");

    deduce_test_expect_int_exact(
        interp,
        "(length (deduce 'scan-range scan_range_person '(\"Bob\" 30 \"b@x\") '(\"Carol\" 40 \"c@x\")))",
        2,
        "[PASS] deduce 'scan-range bounded count",
        "[FAIL] deduce 'scan-range bounded count (got %d)",
        pass,
        fail
    );

    deduce_test_expect_int_exact(
        interp,
        "(length (deduce 'scan-range scan_range_person '(\"Zed\" 99 \"z@x\") '(\"Zoe\" 100 \"z@x\")))",
        0,
        "[PASS] deduce 'scan-range empty",
        "[FAIL] deduce 'scan-range empty (got %d)",
        pass,
        fail
    );

    deduce_test_expect_error(
        interp,
        "(deduce 'scan-range scan_range_person \"bad\" '(\"Carol\" 40 \"c@x\"))",
        "[PASS] deduce 'scan-range lower bound not list",
        "[FAIL] deduce 'scan-range lower bound not list",
        pass,
        fail
    );

    deduce_test_expect_error(
        interp,
        "(deduce 'scan-range scan_range_person '(\"Bob\" 30) '(\"Carol\" 40 \"c@x\"))",
        "[PASS] deduce 'scan-range lower bound arity",
        "[FAIL] deduce 'scan-range lower bound arity",
        pass,
        fail
    );
}

fn void run_deduce_query_filter_tests(Interp* interp, int* pass, int* fail) {
    deduce_test_expect_int_at_least(
        interp,
        "(deduce 'count person)",
        1,
        "[PASS] deduce 'count",
        "[FAIL] deduce 'count (got %d)",
        pass,
        fail
    );

    deduce_test_expect_int_at_least(
        interp,
        "(length (deduce 'query person (lambda (row) (> ('age row) 28))))",
        1,
        "[PASS] deduce 'query filter",
        "[FAIL] deduce 'query filter (got %d)",
        pass,
        fail
    );

    deduce_test_expect_int_exact(
        interp,
        "(length (deduce 'query person (lambda (row) false)))",
        0,
        "[PASS] deduce 'query false predicate",
        "[FAIL] deduce 'query false predicate (got %d)",
        pass,
        fail
    );

    deduce_test_expect_string(
        interp,
        "('name (car (deduce 'query person (lambda (row) (> ('age row) 28)))))",
        "[PASS] deduce 'query dict result",
        "[FAIL] deduce 'query dict result",
        pass,
        fail
    );
}
