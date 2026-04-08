module lisp;


import std::io;

fn void run_diagnostic_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Diagnostic Tests");

    // --- Dispatch diagnostics ---

    // Type mismatch - shows arg types
    test_error_contains(interp, "dispatch: type mismatch shows types",
        "(block (define (typed-fn (^Integer x)) x) (typed-fn \"hello\"))",
        "String", pass, fail);

    // Shows method name
    test_error_contains(interp, "dispatch: shows method name",
        "(block (define (typed-fn2 (^Integer x)) x) (typed-fn2 \"hello\"))",
        "typed-fn2", pass, fail);

    // Arity mismatch
    test_error_contains(interp, "dispatch: arity mismatch",
        "(block (define [type] DPt (^Integer x) (^Integer y)) (define (dadd (^DPt a) (^DPt b)) 1) (dadd (DPt 1 2)))",
        "no matching method", pass, fail);

    // --- Match exhaustiveness ---

    // Union match missing variant
    test_error_contains(interp, "match: missing union variant",
        "(block (define [union] DiagResult DiagOk (DiagErr Integer)) (match (DiagErr 42) ((DiagOk) 1)))",
        "missing variants", pass, fail);
    test_eq(interp, "match: wildcard intentionally handles non-exhaustive union match",
        "(block (define [union] DiagResultW DiagOkW (DiagErrW Integer)) (match (DiagErrW 42) ((DiagOkW) 1) (_ 2)))",
        2, pass, fail);

    // Non-union match shows type
    test_error_contains(interp, "match: non-union shows type",
        "(match 42 (\"hello\" 1))",
        "Integer", pass, fail);

    // --- Strict handlers ---

    // Strict handler catches unhandled effect
    test_error_contains(interp, "handle ^strict: catches unhandled",
        "(handle ^strict (signal my-eff 42) (other-eff v v))",
        "strict handler", pass, fail);

    // Strict handler allows handled effects
    test_eq(interp, "handle ^strict: allows handled",
        "(handle ^strict (signal my-eff 42) (my-eff v (+ v 1)))",
        43, pass, fail);

    // --- Unhandled effect diagnostics ---

    // Shows tag name
    test_error_contains(interp, "unhandled effect: shows tag name",
        "(signal unknown-eff 42)",
        "unknown-eff", pass, fail);

    // Shows arg type
    test_error_contains(interp, "unhandled effect: shows arg type",
        "(signal unknown-eff \"hello\")",
        "String", pass, fail);
}

fn void run_diagnostic_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_diagnostic_tests, pass, fail);
}


fn void run_atomic_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Atomic Tests");
    run_atomic_core_tests(interp, pass, fail);
    run_atomic_lifetime_tests(interp, pass, fail);
}

fn void run_atomic_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_atomic_tests, pass, fail);
}

fn void run_atomic_core_tests(Interp* interp, int* pass, int* fail) {
    test_tag(interp, "atomic creates handle",
        "(atomic 0)", FFI_HANDLE, pass, fail);

    {
        setup(interp, "(define a (atomic 42))");
        EvalResult r = run("(atomic-read a)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 42) {
            emit_pass_literal("[PASS] atomic-read initial");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-read initial");
            (*fail)++;
        }
    }

    {
        setup(interp, "(atomic-add! a 10)");
        EvalResult r = run("(atomic-read a)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 52) {
            emit_pass_literal("[PASS] atomic-add! + read");
            (*pass)++;
        } else {
            io::printfn("[FAIL] atomic-add! + read (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(atomic-cas! a 52 100)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == SYMBOL) {
            emit_pass_literal("[PASS] atomic-cas! success");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-cas! success");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(atomic-cas! a 52 200)", interp);
        if (!r.error.has_error && (r.value == null || r.value.tag == NIL)) {
            emit_pass_literal("[PASS] atomic-cas! fail");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic-cas! fail");
            (*fail)++;
        }
    }
}

fn void run_atomic_lifetime_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define mk-atomic (lambda (n) (atomic n))) "
            "  (atomic-read (mk-atomic 7)))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 7) {
            emit_pass_literal("[PASS] atomic handle boundary copy");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic handle boundary copy");
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define build-many (lambda (n) "
            "    (if (= n 0) 0 (block (atomic n) (build-many (- n 1)))))) "
            "  (build-many 128) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] atomic transient handle stress");
            (*pass)++;
        } else {
            io::printn("[FAIL] atomic transient handle stress");
            (*fail)++;
        }
    }
}
