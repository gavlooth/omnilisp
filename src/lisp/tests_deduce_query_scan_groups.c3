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
    return deduce_test_result_is_code(value, "deduce/query-out-of-memory");
}

fn bool deduce_test_result_is_code(Value* value, char[] expected) {
    return value != null &&
        value.tag == STRING &&
        deduce_name_equals(value.str_chars[:value.str_len], expected);
}

fn void deduce_read_failure_expect_code(
    Interp* interp,
    char[] source,
    char[] expected_code,
    char[] pass_msg,
    char[] fail_msg,
    int* pass,
    int* fail
) {
    EvalResult r = run(source, interp);
    if (!r.error.has_error && deduce_test_result_is_code(r.value, expected_code)) {
        emit_pass_literal(pass_msg);
        (*pass)++;
        return;
    }
    io::printn((ZString)fail_msg);
    (*fail)++;
}

fn void run_deduce_read_failure_propagation_tests(Interp* interp, int* pass, int* fail) {
    bool saved_txn_fail = g_deduce_force_read_txn_begin_fail;
    bool saved_cursor_open_fail = g_deduce_force_read_cursor_open_fail;
    bool saved_value_array_fail = g_deduce_force_read_value_array_alloc_fail;
    int saved_cursor_get_fail_after = g_deduce_force_read_cursor_get_fail_after;
    defer {
        g_deduce_force_read_txn_begin_fail = saved_txn_fail;
        g_deduce_force_read_cursor_open_fail = saved_cursor_open_fail;
        g_deduce_force_read_value_array_alloc_fail = saved_value_array_fail;
        g_deduce_force_read_cursor_get_fail_after = saved_cursor_get_fail_after;
    }

    g_deduce_force_read_txn_begin_fail = true;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-read-txn-fail (deduce 'open 'memory)) "
        "  (define [relation db-read-txn-fail] read_txn_fail_person (name) (age)) "
        "  (deduce 'fact! read_txn_fail_person \"Alice\" 30) "
        "  (handle (deduce 'scan read_txn_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/scan-transaction-failed",
        "[PASS] deduce scan reports read transaction failure",
        "[FAIL] deduce scan reports read transaction failure",
        pass,
        fail
    );
    g_deduce_force_read_txn_begin_fail = false;

    g_deduce_force_read_cursor_open_fail = true;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-read-cursor-open-fail (deduce 'open 'memory)) "
        "  (define [relation db-read-cursor-open-fail] read_cursor_open_fail_person (name) (age)) "
        "  (deduce 'fact! read_cursor_open_fail_person \"Alice\" 30) "
        "  (handle (deduce 'scan read_cursor_open_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/scan-cursor-open-failed",
        "[PASS] deduce scan reports cursor open failure",
        "[FAIL] deduce scan reports cursor open failure",
        pass,
        fail
    );
    g_deduce_force_read_cursor_open_fail = false;

    g_deduce_force_read_value_array_alloc_fail = true;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-read-value-array-fail (deduce 'open 'memory)) "
        "  (define [relation db-read-value-array-fail] read_value_array_fail_person (name) (age)) "
        "  (deduce 'fact! read_value_array_fail_person \"Alice\" 30) "
        "  (handle (deduce 'query read_value_array_fail_person (lambda (row) true)) "
        "    (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/scan-out-of-memory",
        "[PASS] deduce query reports iterator value allocation failure",
        "[FAIL] deduce query reports iterator value allocation failure",
        pass,
        fail
    );
    g_deduce_force_read_value_array_alloc_fail = false;

    g_deduce_force_read_cursor_get_fail_after = 1;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-read-cursor-get-fail (deduce 'open 'memory)) "
        "  (define [relation db-read-cursor-get-fail] read_cursor_get_fail_person (name) (age)) "
        "  (deduce 'fact! read_cursor_get_fail_person \"Alice\" 30) "
        "  (deduce 'fact! read_cursor_get_fail_person \"Bob\" 31) "
        "  (handle (deduce 'scan read_cursor_get_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/scan-cursor-read-failed",
        "[PASS] deduce scan reports cursor read failure",
        "[FAIL] deduce scan reports cursor read failure",
        pass,
        fail
    );
    g_deduce_force_read_cursor_get_fail_after = -1;

    g_deduce_force_read_txn_begin_fail = true;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-count-txn-fail (deduce 'open 'memory)) "
        "  (define [relation db-count-txn-fail] count_txn_fail_person (name) (age)) "
        "  (handle (deduce 'count count_txn_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/count-transaction-failed",
        "[PASS] deduce count reports read transaction failure",
        "[FAIL] deduce count reports read transaction failure",
        pass,
        fail
    );
    g_deduce_force_read_txn_begin_fail = false;

    g_deduce_force_read_cursor_open_fail = true;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-count-cursor-open-fail (deduce 'open 'memory)) "
        "  (define [relation db-count-cursor-open-fail] count_cursor_open_fail_person (name) (age)) "
        "  (handle (deduce 'count count_cursor_open_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/count-cursor-open-failed",
        "[PASS] deduce count reports cursor open failure",
        "[FAIL] deduce count reports cursor open failure",
        pass,
        fail
    );
    g_deduce_force_read_cursor_open_fail = false;

    g_deduce_force_read_cursor_get_fail_after = 1;
    deduce_read_failure_expect_code(
        interp,
        "(block "
        "  (define db-count-cursor-get-fail (deduce 'open 'memory)) "
        "  (define [relation db-count-cursor-get-fail] count_cursor_get_fail_person (name) (age)) "
        "  (deduce 'fact! count_cursor_get_fail_person \"Alice\" 30) "
        "  (deduce 'fact! count_cursor_get_fail_person \"Bob\" 31) "
        "  (handle (deduce 'count count_cursor_get_fail_person) (raise payload (String (ref payload 'code))))"
        ")",
        "deduce/count-cursor-read-failed",
        "[PASS] deduce count reports cursor read failure",
        "[FAIL] deduce count reports cursor read failure",
        pass,
        fail
    );
    g_deduce_force_read_cursor_get_fail_after = -1;

    deduce_test_expect_int_exact(
        interp,
        "(block "
        "  (define db-count-empty-ok (deduce 'open 'memory)) "
        "  (define [relation db-count-empty-ok] count_empty_ok_person (name) (age)) "
        "  (deduce 'count count_empty_ok_person))",
        0,
        "[PASS] deduce count preserves valid empty relation zero",
        "[FAIL] deduce count preserves valid empty relation zero (got %d)",
        pass,
        fail
    );
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
        "(ref (car (deduce 'scan person)) 'name)",
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

    Relation* scan_oom_rel = deduce_test_prepare_relation_handle(
        interp,
        "(block "
        "  (define scan-oom-db (deduce 'open 'memory)) "
        "  (define [relation scan-oom-db] scan_oom_person (name) (age) (email)) "
        "  scan_oom_person "
        ")",
        "[FAIL] deduce 'scan column-key-value oom propagation (failed to prepare relation)",
        fail
    );
    if (scan_oom_rel == null) return;

    bool saved_force_column_key_values_oom = g_deduce_force_column_key_values_oom;
    g_deduce_force_column_key_values_oom = true;
    EvalResult scan_oom = run(
        "(handle (deduce 'scan scan_oom_person) (raise payload (String (ref payload 'code))))",
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

    {
        int saved_force_alloc = g_boundary_force_alloc_value_fail_nth;
        usz created = 0;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* key_alloc_fail = deduce_relation_ensure_column_key_values(scan_oom_rel, interp, &created);
        g_boundary_force_alloc_value_fail_nth = saved_force_alloc;

        bool ok = key_alloc_fail == null &&
            created == scan_oom_rel.col_count &&
            scan_oom_rel.column_key_values != null &&
            scan_oom_rel.col_count == 3 &&
            scan_oom_rel.column_key_values[0] == scan_oom_rel.columns[0] &&
            scan_oom_rel.column_key_values[1] == scan_oom_rel.columns[1] &&
            scan_oom_rel.column_key_values[2] == scan_oom_rel.columns[2];
        if (ok) {
            emit_pass_literal("[PASS] deduce 'scan column-key cache avoids long-lived Value pointers");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan column-key cache avoids long-lived Value pointers");
            (*fail)++;
        }
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
        "  (raise payload (String (ref payload 'code))))",
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
        "(length (deduce 'query person (lambda (row) (> row.age 28))))",
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
        "(ref (car (deduce 'query person (lambda (row) (> row.age 28)))) 'name)",
        "[PASS] deduce 'query dict result",
        "[FAIL] deduce 'query dict result",
        pass,
        fail
    );
}
