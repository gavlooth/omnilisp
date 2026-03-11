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

fn void run_deduce_scan_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 2) {
            emit_pass_literal("[PASS] deduce 'fact! + 'scan");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'fact! + 'scan (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    setup(interp, "(deduce 'retract! person \"Bob\" 25)");
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'retract!");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'retract!");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'scan person)))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'scan dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan dict result");
            (*fail)++;
        }
    }
}

fn void run_deduce_scan_range_tests(Interp* interp, int* pass, int* fail) {
    setup(interp,
        "(block "
        "  (define scan-range-db (deduce 'open 'memory)) "
        "  (define [relation scan-range-db] scan_range_person (name) (age) (email)) "
        "  (deduce 'fact! scan_range_person \"Alice\" 20 \"a@x\") "
        "  (deduce 'fact! scan_range_person \"Bob\" 30 \"b@x\") "
        "  (deduce 'fact! scan_range_person \"Carol\" 40 \"c@x\") "
        "  (deduce 'fact! scan_range_person \"Dora\" 50 \"d@x\") "
        ")");

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Bob\" 30 \"b@x\") '(\"Carol\" 40 \"c@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 2) {
            emit_pass_literal("[PASS] deduce 'scan-range bounded count");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range bounded count (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Zed\" 99 \"z@x\") '(\"Zoe\" 100 \"z@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'scan-range empty");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range empty (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person \"bad\" '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound not list");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound not list");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person '(\"Bob\" 30) '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound arity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound arity");
            (*fail)++;
        }
    }
}

fn void run_deduce_query_filter_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(deduce 'count person)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'count");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'count");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) (> ('age row) 28))))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'query filter");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'query filter (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) false)))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'query false predicate");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query false predicate");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'query person (lambda (row) (> ('age row) 28)))))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'query dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query dict result");
            (*fail)++;
        }
    }
}
