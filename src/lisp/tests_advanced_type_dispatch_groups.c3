module lisp;


import std::io;

fn void run_advanced_dispatch_constructor_basic_tests(Interp* interp, int* pass, int* fail) {
    // === MULTIPLE DISPATCH TESTS (Phase 2) ===

    // Basic dispatch on types
    setup(interp, "(define (describe (^Integer n)) \"integer\")");
    setup(interp, "(define (describe (^String s)) \"string\")");
    setup(interp, "(define (describe x) \"other\")");
    test_truthy_interp(interp, "dispatch int", "(= (describe 42) \"integer\")", pass, fail);
    test_truthy_interp(interp, "dispatch string", "(= (describe \"hi\") \"string\")", pass, fail);
    test_truthy_interp(interp, "dispatch fallback", "(= (describe '(1 2)) \"other\")", pass, fail);

    // Value dispatch (Fibonacci)
    setup(interp, "(define (fib (^(Value 0) n)) 0)");
    setup(interp, "(define (fib (^(Value 1) n)) 1)");
    setup(interp, "(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))");
    test_eq_interp(interp, "dispatch fib 0", "(fib 0)", 0, pass, fail);
    test_eq_interp(interp, "dispatch fib 1", "(fib 1)", 1, pass, fail);
    test_eq_interp(interp, "dispatch fib 10", "(fib 10)", 55, pass, fail);

    // Value literal dispatch coverage (symbol + string)
    setup(interp, "(define (udp-cmd (^(Value open) op)) 1)");
    setup(interp, "(define (udp-cmd (^(Value bind) op) h host port) 2)");
    setup(interp, "(define (udp-cmd op .. rest) 99)");
    test_eq_interp(interp, "dispatch Value symbol literal open", "(udp-cmd 'open)", 1, pass, fail);
    test_eq_interp(interp, "dispatch Value symbol literal bind", "(udp-cmd 'bind 1 \"127.0.0.1\" 9000)", 2, pass, fail);

    setup(interp, "(define (fmt (^(Value \"csv\") x)) 10)");
    setup(interp, "(define (fmt (^(Value \"toml\") x)) 20)");
    setup(interp, "(define (fmt x) 99)");
    test_eq_interp(interp, "dispatch Value string literal csv", "(fmt \"csv\")", 10, pass, fail);
    test_eq_interp(interp, "dispatch Value string literal toml", "(fmt \"toml\")", 20, pass, fail);

    setup(interp, "(define (flag (^(Value true) x)) 11)");
    setup(interp, "(define (flag (^(Value false) x)) 22)");
    setup(interp, "(define (flag x) 99)");
    test_eq_interp(interp, "dispatch Value bool true literal", "(flag true)", 11, pass, fail);
    test_eq_interp(interp, "dispatch Value bool false literal", "(flag false)", 22, pass, fail);

    setup(interp, "(define (ambv (^(Value open) cmd) x) 1)");
    setup(interp, "(define (ambv x (^(Value open) cmd)) 2)");
    test_error(interp, "dispatch ambiguous equal-specificity Value literal", "(ambv 'open 'open)", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Value literal message",
        "(ambv 'open 'open)",
        "ambiguous method call", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Value literal equal-specificity detail",
        "(ambv 'open 'open)",
        "equal specificity", pass, fail);

    setup(interp, "(define (ambi-int (^Integer x) y) 10)");
    setup(interp, "(define (ambi-int x (^Integer y)) 20)");
    test_error(interp, "dispatch ambiguous equal-specificity Integer positional", "(ambi-int 1 1)", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Integer positional message",
        "(ambi-int 1 1)",
        "ambiguous method call", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Integer positional equal-specificity detail",
        "(ambi-int 1 1)",
        "equal specificity", pass, fail);
    test_truthy(interp, "dispatch ambiguous payload stable fields",
        "(block "
        "  (define (ambi-payload (^Integer x) y) 10) "
        "  (define (ambi-payload x (^Integer y)) 20) "
        "  (handle (ambi-payload 1 1) "
        "    (raise payload "
        "      (let (d (ref payload 'data)) "
        "        (and (= (symbol->string (ref payload 'code)) \"type/dispatch-ambiguous\") "
        "             (and (= (ref d 'reason) 'ambiguous-equal-specificity) "
        "                  (and (= (ref d 'method) 'ambi-payload) "
        "                       (and (= (ref d 'arg-count) 2) "
        "                            (and (= (ref d 'best-score) 101) "
        "                                 (and (= (ref d 'tie-count) 2) "
        "                                      (and (= (length (ref d 'arg-types)) 2) "
        "                                           (and (= (ref (ref d 'arg-types) 0) 'Integer) "
        "                                                (and (= (ref (ref d 'arg-types) 1) 'Integer) "
        "                                                     (and (= (length (ref d 'candidate-indices)) 2) "
        "                                                          (and (= (ref (ref d 'candidate-indices) 0) 0) "
        "                                                               (= (ref (ref d 'candidate-indices) 1) 1))))))))))))))))",
        pass, fail);
    test_truthy(interp, "dispatch ambiguous payload candidate ordering stable",
        "(block "
        "  (define (ambi-order x y) 0) "
        "  (define (ambi-order (^Integer x) y) 1) "
        "  (define (ambi-order x (^Integer y)) 2) "
        "  (handle (ambi-order 1 1) "
        "    (raise payload "
        "      (let (idx (ref (ref payload 'data) 'candidate-indices)) "
        "        (and (= (length idx) 2) "
        "             (and (= (ref idx 0) 0) "
        "                  (= (ref idx 1) 1)))))))",
        pass, fail);

    // Unified command-style facade over canonical io/udp-* operations.
    setup(interp, "(define (io/udp-open) 'opened)");
    setup(interp, "(define (io/udp-bind h host port) (list 'bound h host port))");
    setup(interp, "(define (io/udp-send h host port payload) (list 'sent h host port payload))");
    setup(interp, "(define (udp (^(Value open) op)) (io/udp-open))");
    setup(interp, "(define (udp (^(Value bind) op) h host port) (io/udp-bind h host port))");
    setup(interp, "(define (udp (^(Value send) op) h host port payload) (io/udp-send h host port payload))");
    test_truthy_interp(interp, "dispatch udp open command facade", "(= (udp 'open) 'opened)", pass, fail);
    test_truthy_interp(interp, "dispatch udp bind command facade",
        "(= (udp 'bind 7 \"127.0.0.1\" 9001) '(bound 7 \"127.0.0.1\" 9001))", pass, fail);
    test_truthy_interp(interp, "dispatch udp send command facade",
        "(= (udp 'send 7 \"127.0.0.1\" 9001 \"ping\") '(sent 7 \"127.0.0.1\" 9001 \"ping\"))", pass, fail);

    // Multi-arg dispatch
    setup(interp, "(define (add2 (^Integer a) (^Integer b)) (+ a b))");
    setup(interp, "(define (add2 (^String a) (^String b)) (string-append a b))");
    test_eq_interp(interp, "dispatch multi-arg int", "(add2 3 4)", 7, pass, fail);
    test_truthy_interp(interp, "dispatch multi-arg str", "(= (add2 \"hello\" \" world\") \"hello world\")", pass, fail);

    // Numeric dispatch without implicit widening: exact match or subtype only.
    setup(interp, "(define (num-tag (^Double x)) 10)");
    setup(interp, "(define (num-tag (^Integer x)) 20)");
    setup(interp, "(define (num-tag x) 30)");
    test_eq_interp(interp, "dispatch numeric promotion double exact", "(num-tag 3.5)", 10, pass, fail);
    test_eq_interp(interp, "dispatch numeric exact Integer method", "(num-tag 3)", 20, pass, fail);

    setup(interp, "(define (only-double (^Double x)) 11)");
    setup(interp, "(define (only-double x) 22)");
    test_eq_interp(interp, "dispatch numeric no implicit Integer->Double widening", "(only-double 7)", 22, pass, fail);
    test_eq_interp(interp, "dispatch numeric explicit constructor conversion", "(only-double (Double 7))", 11, pass, fail);

    setup(interp, "(define (only-int (^Integer x)) 33)");
    setup(interp, "(define (only-int x) 44)");
    test_eq_interp(interp, "dispatch numeric no implicit Double->Integer narrowing", "(only-int 3.9)", 44, pass, fail);
    test_eq_interp(interp, "dispatch numeric explicit Integer constructor conversion", "(only-int (Integer 3.9))", 33, pass, fail);
    test_eq_interp(interp, "dispatch numeric explicit Integer constructor conversion alias", "(only-int (Integer 3.9))", 33, pass, fail);

    // Last-arg dispatch past the older 8-arg scoring cap
    setup(interp, "(define (dispatch9 a b c d e f g h (^Integer i)) 111)");
    setup(interp, "(define (dispatch9 a b c d e f g h (^String i)) 222)");
    test_eq_interp(interp, "dispatch 9th arg int", "(dispatch9 1 2 3 4 5 6 7 8 9)", 111, pass, fail);
    test_eq_interp(interp, "dispatch 9th arg string", "(dispatch9 1 2 3 4 5 6 7 8 \"nine\")", 222, pass, fail);

    {
        const usz ARG_COUNT = 40;
        char[12288] define_int_buf;
        char[12288] define_string_buf;
        char[8192] call_int_buf;
        char[8192] call_string_buf;
        if (!advanced_build_dispatch_define(&define_int_buf, define_int_buf.len, "dispatch40", ARG_COUNT, "Integer", 111) ||
            !advanced_build_dispatch_define(&define_string_buf, define_string_buf.len, "dispatch40", ARG_COUNT, "String", 222) ||
            !advanced_build_dispatch_call(&call_int_buf, call_int_buf.len, "dispatch40", ARG_COUNT, false) ||
            !advanced_build_dispatch_call(&call_string_buf, call_string_buf.len, "dispatch40", ARG_COUNT, true)) {
            io::printn("[FAIL] wide dispatch signature regression builder");
            (*fail)++;
        } else {
            setup(interp, define_int_buf[..advanced_test_cstr_len(&define_int_buf)]);
            setup(interp, define_string_buf[..advanced_test_cstr_len(&define_string_buf)]);
            test_eq_interp(interp, "dispatch 40th arg int", call_int_buf[..advanced_test_cstr_len(&call_int_buf)], 111, pass, fail);
            test_eq_interp(interp, "dispatch 40th arg string", call_string_buf[..advanced_test_cstr_len(&call_string_buf)], 222, pass, fail);
        }
    }
}

fn void run_advanced_dispatch_constructor_constraint_tests(Interp* interp, int* pass, int* fail) {
    // === CONSTRAINED DISPATCH TESTS (Phase 7) ===

    // Define abstract type hierarchy for constraint testing
    setup(interp, "(define [abstract] (Numeric Number))");  // Numeric extends Number (but Number doesn't exist yet, no parent)

    // Actually, let's use fresh abstract type hierarchy
    setup(interp, "(define [abstract] Addable)");
    setup(interp, "(define [type] (MyInt Addable) (^Integer val))");
    setup(interp, "(define [type] (MyStr Addable) (^String val))");
    setup(interp, "(define [type] NotAddable (^Integer val))");

    // Constrained dispatch: only matches if arg type <: Addable
    setup(interp, "(define (get-val (^{'T Addable} x)) x.val)");
    test_eq_interp(interp, "constraint MyInt match", "(get-val (MyInt 42))", 42, pass, fail);
    test_truthy_interp(interp, "constraint MyStr match", "(= (get-val (MyStr \"hi\")) \"hi\")", pass, fail);

    // Constraint fallback: NotAddable doesn't satisfy Addable constraint
    // Add a fallback method that accepts anything
    setup(interp, "(define (get-val x) (- 0 1))");
    test_eq_interp(interp, "constraint fallback", "(get-val (NotAddable 99))", -1, pass, fail);

    {
        const usz CONSTRAINT_COUNT = 40;
        char[12288] define_constraint_buf;
        if (!advanced_build_constraint_define(&define_constraint_buf, define_constraint_buf.len, CONSTRAINT_COUNT, "String", 111)) {
            io::printn("[FAIL] wide dispatch constraint regression builder");
            (*fail)++;
        } else {
            setup(interp, define_constraint_buf[..advanced_test_cstr_len(&define_constraint_buf)]);
            setup(interp, "(define (constraint-wide x) 222)");
            test_eq_interp(interp, "dispatch preserves >32 constraints", "(constraint-wide 1)", 222, pass, fail);
        }
    }
}

fn void run_advanced_dispatch_constructor_unification_union_tests(Interp* interp, int* pass, int* fail) {
    // === PARAMETRIC METHOD UNIFICATION TESTS ===
    setup(interp, "(define (pair-same (^T a) (^T b)) 10)");
    setup(interp, "(define (pair-same a b) 20)");
    test_eq_interp(interp, "dispatch type-var unification same Integer", "(pair-same 1 2)", 10, pass, fail);
    test_eq_interp(interp, "dispatch type-var unification mismatch falls back", "(pair-same 1 \"x\")", 20, pass, fail);

    setup(interp, "(define (num-same (^{'T Number} a) (^{'T Number} b)) 11)");
    setup(interp, "(define (num-same a b) 22)");
    test_eq_interp(interp, "dispatch constrained type-var unification Integer/Integer", "(num-same 3 4)", 11, pass, fail);
    test_eq_interp(interp, "dispatch constrained type-var unification Integer/Double fallback", "(num-same 3 4.0)", 22, pass, fail);

    // === UNION PARTICIPATION IN DISPATCH APPLICABILITY TESTS ===
    setup(interp, "(define [union] DispatchOpt DNone (DSome Integer) (DText String))");
    setup(interp, "(define (opt-tag (^DispatchOpt v)) 100)");
    setup(interp, "(define (opt-tag (^DSome v)) 200)");
    setup(interp, "(define (opt-tag (^DText v)) 300)");
    test_eq_interp(interp, "dispatch union parent accepts nullary variant", "(opt-tag DNone)", 100, pass, fail);
    test_eq_interp(interp, "dispatch union variant beats union parent (DSome)", "(opt-tag (DSome 7))", 200, pass, fail);
    test_eq_interp(interp, "dispatch union variant beats union parent (DText)", "(opt-tag (DText \"hi\"))", 300, pass, fail);
}

fn void run_advanced_dispatch_constructor_validation_tests(Interp* interp, int* pass, int* fail) {
    // === CONSTRUCTOR FIELD TYPE VALIDATION TESTS ===

    // Correct types — should succeed
    test_truthy_interp(interp, "ctor valid Point", "(instance? (Point 3 4))", pass, fail);
    // Wrong type — string where Integer expected
    test_error(interp, "ctor type error str for int", "(Point \"x\" 4)", pass, fail);
    // Wrong type — double where Integer expected
    test_error(interp, "ctor type error dbl for int", "(Point 3.0 4)", pass, fail);
    // Subtype field — Circle has Integer field, still works
    test_truthy_interp(interp, "ctor valid Circle", "(instance? (Circle 5))", pass, fail);
    // Nested instance type — Line expects Point fields
    test_truthy_interp(interp, "ctor valid Line", "(instance? (Line (Point 1 2) (Point 3 4)))", pass, fail);
    // Parametric field — Box accepts anything (no constraint)
    test_truthy_interp(interp, "ctor parametric Box", "(instance? (Box \"anything\"))", pass, fail);
}

fn void run_advanced_dispatch_constructor_ambiguity_parametric_tests(Interp* interp, int* pass, int* fail) {
    // === DISPATCH AMBIGUITY DETECTION TESTS ===

    // Two methods with identical signatures — second replaces first (idempotent re-define)
    setup(interp, "(define (ambig-test (^Integer x) (^Integer y)) (+ x y))");
    setup(interp, "(define (ambig-test (^Integer x) (^Integer y)) (* x y))");
    test_eq_interp(interp, "dispatch redefine replaces", "(ambig-test 3 4)", 12, pass, fail);
    // Different specificity — should succeed (picks more specific)
    setup(interp, "(define (specific-test (^Integer x)) (* x 2))");
    setup(interp, "(define (specific-test x) 0)");
    test_eq_interp(interp, "dispatch no ambiguity", "(specific-test 5)", 10, pass, fail);

    // === PARAMETRIC CONSTRAINT AT CONSTRUCTION TESTS ===

    // Define constrained parametric type: T must be subtype of Number
    // Constraint expressed via dict annotation on field: ^{'T Number}
    setup(interp, "(define [type] (NumBox T) (^{'T Number} value))");
    // Integer <: Number — should succeed
    test_eq_interp(interp, "NumBox Integer ok", "(let (b (NumBox 42)) b.value)", 42, pass, fail);
    // Double <: Number — should succeed
    test_truthy_interp(interp, "NumBox Dbl ok", "(= (type-of (NumBox 3.14)) 'NumBox)", pass, fail);
    // String not <: Number — should error
    test_error(interp, "NumBox String fail", "(NumBox \"hi\")", pass, fail);
}

const usz DISPATCH_HOT_PATH_BENCH_ITERS = 32768;

fn bool dispatch_hot_path_bench_enabled() @inline {
    return test_c_getenv("OMNI_DISPATCH_BENCH") != null;
}

fn long dispatch_hot_path_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn void run_advanced_dispatch_hot_path_benchmarks(Interp* interp) {
    if (!dispatch_hot_path_bench_enabled()) return;
    if (interp == null) return;

    emit_test_section_banner("Dispatch Hot-Path Benchmarks");

    setup(interp, "(define (dispatch-bench-call (^Integer x)) (+ x 1))");
    setup(interp, "(define (dispatch-bench-call (^Double x)) (+ x 1.0))");
    setup(interp, "(define (dispatch-bench-call x) 0)");
    setup(interp, "(define (typed-lambda-bench-call (^Integer x)) ((lambda ((^Integer y)) (+ y 1)) x))");

    long expected_sum = (long)(DISPATCH_HOT_PATH_BENCH_ITERS * 8);

    long start_ms = dispatch_hot_path_bench_now_ms();
    EvalResult dispatch_result = run(
        "(let loop (i 0 acc 0) "
        "  (if (= i 32768) acc "
        "      (loop (+ i 1) (+ acc (dispatch-bench-call 7)))))",
        interp
    );
    long dispatch_done_ms = dispatch_hot_path_bench_now_ms();

    EvalResult typed_lambda_result = run(
        "(let loop (i 0 acc 0) "
        "  (if (= i 32768) acc "
        "      (loop (+ i 1) (+ acc (typed-lambda-bench-call 7)))))",
        interp
    );
    long end_ms = dispatch_hot_path_bench_now_ms();

    long dispatch_ms = dispatch_done_ms > start_ms ? dispatch_done_ms - start_ms : 0;
    long typed_lambda_ms = end_ms > dispatch_done_ms ? end_ms - dispatch_done_ms : 0;

    bool dispatch_ok =
        !dispatch_result.error.has_error &&
        is_int(dispatch_result.value) &&
        dispatch_result.value.int_val == expected_sum;
    bool typed_lambda_ok =
        !typed_lambda_result.error.has_error &&
        is_int(typed_lambda_result.value) &&
        typed_lambda_result.value.int_val == expected_sum;

    io::printfn(
        "OMNI_BENCH_SUMMARY suite=dispatch_hot_path iters=%d dispatch_ms=%d dispatch_ok=%d typed_lambda_ms=%d typed_lambda_ok=%d dispatch_sum=%d typed_lambda_sum=%d",
        (long)DISPATCH_HOT_PATH_BENCH_ITERS,
        dispatch_ms,
        dispatch_ok ? 1 : 0,
        typed_lambda_ms,
        typed_lambda_ok ? 1 : 0,
        dispatch_ok ? dispatch_result.value.int_val : 0,
        typed_lambda_ok ? typed_lambda_result.value.int_val : 0
    );
}

fn void run_advanced_dispatch_constructor_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_dispatch_constructor_basic_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_constraint_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_unification_union_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_validation_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_ambiguity_parametric_tests(interp, pass, fail);
    run_advanced_dispatch_hot_path_benchmarks(interp);
}
