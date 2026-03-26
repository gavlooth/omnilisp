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
            io::printn("[PASS] deduce 'scan-range row-dict oom probe");
            (*pass)++;
            return;
        }
        io::printn("[FAIL] deduce 'scan-range row-dict oom probe (expected OOM guard)");
        (*fail)++;
        return;
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
