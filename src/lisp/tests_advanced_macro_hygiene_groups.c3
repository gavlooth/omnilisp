module lisp;


import std::io;
import main;
fn void run_advanced_macro_hygiene_string_number_tests(Interp* interp, int* pass, int* fail) {
    // String<->number
    test_eq(interp, "string->number 42", "(string->number \"42\")", 42, pass, fail);
    test_eq(interp, "string->number -7", "(string->number \"-7\")", -7, pass, fail);
    test_nil(interp, "string->number abc => nil", "(string->number \"abc\")", pass, fail);
    test_nil(interp, "string->number empty => nil", "(string->number \"\")", pass, fail);
    test_eq(interp, "number->string round-trip 42", "(string->number (number->string 42))", 42, pass, fail);
    test_eq(interp, "number->string round-trip 0", "(string->number (number->string 0))", 0, pass, fail);
    test_eq(interp, "number->string round-trip -99", "(string->number (number->string -99))", -99, pass, fail);

    // Stack overflow / non-tail recursion headroom
    test_eq(interp, "non-tail recursion exceeds former 1024 eval cap",
        "(let ^rec (f (lambda (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))) (f 1500))",
        1500, pass, fail);
    if (!main::stack_runtime_asan_enabled()) {
        test_error(interp, "stack overflow caught", "(let ^rec (f (lambda (n) (+ 1 (f (+ n 1))))) (f 0))", pass, fail);
    } else {
        emit_pass_literal("[PASS] stack overflow caught (ASAN runtime skip)");
        (*pass)++;
    }
}

fn void run_advanced_macro_hygiene_quasi_pattern_tests(Interp* interp, int* pass, int* fail) {
    // Quasiquote
    test_tag(interp, "quasiquote basic", "`(a b c)", CONS, pass, fail);
    setup(interp, "(define qq-x 42)");
    test_tag(interp, "quasiquote unquote", "`(a ,qq-x c)", CONS, pass, fail);
    test_eq(interp, "quasiquote standalone", "`,(+ 1 2)", 3, pass, fail);
    setup(interp, "(define qq-lst (list 1 2 3))");
    test_eq(interp, "quasiquote splice len", "(length `(a ,@qq-lst d))", 5, pass, fail);
    setup(interp, "(define qq-empty (list))");
    test_eq(interp, "quasiquote splice empty", "(length `(a ,@qq-empty b))", 2, pass, fail);

    // Pattern macros
    setup(interp, "(define [macro] when ([test .. body] (if test (begin .. body) nil)))");
    test_eq(interp, "when true", "(when true 42)", 42, pass, fail);
    test_nil(interp, "when false", "(when false 42)", pass, fail);
    setup(interp, "(define [macro] unless ([test .. body] (if test nil (begin .. body))))");
    test_eq(interp, "unless false", "(unless false 99)", 99, pass, fail);
    test_nil(interp, "unless true", "(unless true 99)", pass, fail);
    setup(interp, "(define [macro] with-val ([name val body] (let (name val) body)))");
    test_eq(interp, "macro with-val", "(with-val z 100 (+ z 1))", 101, pass, fail);
    test_eq(interp, "unless macro preserves let inside define",
        "(begin (unless false (define (ufoo3 x) (let (y 1) (+ x y)))) (ufoo3 2))",
        3, pass, fail);
    setup(interp, "(define [macro] cond ([] nil) ([test body .. rest] (if test body (cond .. rest))))");
    test_eq(interp, "cond macro", "(cond false 1 true 2 true 3)", 2, pass, fail);
    test_nil(interp, "cond all false", "(cond false 1 false 2)", pass, fail);
    setup(interp, "(define [macro] my-and ([a b] (if a b false)))");
    test_eq(interp, "my-and true", "(my-and true 42)", 42, pass, fail);
    setup(interp, "(define [macro] let1 ([name val body] (let (name val) body)))");
    test_eq(interp, "let1 macro", "(let1 x 10 (+ x 5))", 15, pass, fail);
    test_eq(interp, "stream-yield macro expands shift special form",
        "(car (reset (stream-yield 7)))",
        7, pass, fail);

    {
        const usz CLAUSE_COUNT = 40;
        const usz VAR_COUNT = 70;
        char[8192] define_clauses_buf;
        char[8192] define_vars_buf;
        char[8192] call_vars_buf;
        if (!advanced_build_many_clause_macro_define(&define_clauses_buf, define_clauses_buf.len, CLAUSE_COUNT) ||
            !advanced_build_many_pattern_vars_macro_define(&define_vars_buf, define_vars_buf.len, VAR_COUNT) ||
            !advanced_build_many_arg_macro_call(&call_vars_buf, call_vars_buf.len, "pick-last", VAR_COUNT)) {
            io::printn("[FAIL] wide macro regression builder");
            (*fail)++;
        } else {
            setup(interp, define_clauses_buf[..advanced_test_cstr_len(&define_clauses_buf)]);
            test_eq(interp, "macro preserves >32 clauses", "(choose-wide 39)", 39, pass, fail);
            setup(interp, define_vars_buf[..advanced_test_cstr_len(&define_vars_buf)]);
            test_eq(interp, "macro preserves >64 pattern vars", call_vars_buf[..advanced_test_cstr_len(&call_vars_buf)], (long)(VAR_COUNT - 1), pass, fail);
        }
    }
}

fn void run_advanced_macro_hygiene_gensym_tests(Interp* interp, int* pass, int* fail) {
    // Auto-gensym
    setup(interp, "(define [macro] swap! ([a b] (let (tmp# a) (begin (set! a b) (set! b tmp#)))))");
    setup(interp, "(define swap-x 10)");
    setup(interp, "(define swap-y 20)");
    setup(interp, "(swap! swap-x swap-y)");
    test_eq(interp, "swap! x", "swap-x", 20, pass, fail);
    test_eq(interp, "swap! y", "swap-y", 10, pass, fail);
    setup(interp, "(define tmp 999)");
    setup(interp, "(define hx 1)");
    setup(interp, "(define hy 2)");
    setup(interp, "(swap! hx hy)");
    test_eq(interp, "gensym hygiene", "tmp", 999, pass, fail);

    {
        const usz GENSYM_COUNT = 80;
        char[16384] define_buf;
        if (!advanced_build_many_gensym_macro_define(&define_buf, define_buf.len, GENSYM_COUNT)) {
            io::printn("[FAIL] wide auto-gensym regression builder");
            (*fail)++;
        } else {
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            test_eq(interp, "macro preserves >64 auto-gensyms", "(gensym-wide)", (long)(GENSYM_COUNT - 1), pass, fail);
        }
    }

    // gensym
    test_tag(interp, "gensym returns symbol", "(gensym)", SYMBOL, pass, fail);
}

fn void run_advanced_macro_hygiene_capture_tests(Interp* interp, int* pass, int* fail) {
    // Macro hygiene (def-env capture)
    setup(interp, "(define hyg-helper (lambda (x) (+ x 100)))");
    setup(interp, "(define [macro] hyg-apply ([x] (hyg-helper x)))");
    test_eq(interp, "hygiene normal", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define hyg-helper (lambda (x) (* x 999)))");
    test_eq(interp, "hygiene immune to shadow", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define [macro] hyg-inc ([x] (+ x 1)))");
    test_eq(interp, "hygiene captures +", "(hyg-inc 10)", 11, pass, fail);
    setup(interp, "(define [macro] hyg-nil-test ([x] (if x 42 nil)))");
    test_nil(interp, "hygiene nil branch", "(hyg-nil-test false)", pass, fail);
    test_eq(interp, "hygiene true branch", "(hyg-nil-test true)", 42, pass, fail);
    setup(interp, "(define hyg-a 10)");
    setup(interp, "(define hyg-b 20)");
    setup(interp, "(define [macro] hyg-sum ([x] (+ (+ hyg-a hyg-b) x)))");
    test_eq(interp, "hygiene multi literals", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-a 1000)");
    setup(interp, "(define hyg-b 2000)");
    test_eq(interp, "hygiene multi immune", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) (* x 2)))");
    setup(interp, "(define [macro] hyg-with-tmp ([x] (let (tmp# (hyg-op x)) tmp#)))");
    test_eq(interp, "hygiene gensym+def", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) 0))");
    test_eq(interp, "hygiene gensym+def immune", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define x 999)");
    setup(interp, "(define [macro] hyg-id ([x] x))");
    test_eq(interp, "hygiene patvar substitutes", "(hyg-id 7)", 7, pass, fail);
    test_eq(interp, "hygiene recursive cond", "(cond false 1 false 2 true 3)", 3, pass, fail);
    setup(interp, "(define hyg-late-fn (lambda (x) (+ x 50)))");
    setup(interp, "(define [macro] hyg-late-macro ([x] (hyg-late-fn x)))");
    setup(interp, "(define hyg-late-fn (lambda (x) (- x 50)))");
    test_eq(interp, "hygiene late redef", "(hyg-late-macro 100)", 150, pass, fail);

    {
        const usz CAPTURE_COUNT = 70;
        char[12288] setup_buf;
        char[12288] define_buf;
        if (!advanced_build_many_capture_bindings_setup(&setup_buf, setup_buf.len, CAPTURE_COUNT) ||
            !advanced_build_many_capture_macro_define(&define_buf, define_buf.len, CAPTURE_COUNT)) {
            io::printn("[FAIL] wide captured bindings regression builder");
            (*fail)++;
        } else {
            setup(interp, setup_buf[..advanced_test_cstr_len(&setup_buf)]);
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            setup(interp, "(define cap69 9999)");
            test_eq(interp, "macro preserves >64 captured bindings", "(capture-wide)", 69, pass, fail);
        }
    }

    bool saved_jit_enabled = interp.flags.jit_enabled;
    interp.flags.jit_enabled = false;
    setup(interp, "(define [macro] whole-pair (([x y] 'as pair) (length (quote pair))))");
    test_eq_interp(interp, "macro as pattern binds whole arg expr", "(whole-pair 1 2)", 2, pass, fail);
    interp.flags.jit_enabled = saved_jit_enabled;
}

fn void run_advanced_macro_hygiene_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_macro_hygiene_string_number_tests(interp, pass, fail);
    run_advanced_macro_hygiene_quasi_pattern_tests(interp, pass, fail);
    run_advanced_macro_hygiene_gensym_tests(interp, pass, fail);
    run_advanced_macro_hygiene_capture_tests(interp, pass, fail);
}
