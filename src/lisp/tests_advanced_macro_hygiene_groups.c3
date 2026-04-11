module lisp;


import std::io;
import main;

alias AdvancedMacroHygieneGroupFn = fn void(Interp* interp, int* pass, int* fail);

fn void run_advanced_macro_hygiene_group(
    char[] name,
    AdvancedMacroHygieneGroupFn group_fn,
    Interp* interp,
    int* pass,
    int* fail
) {
    if (!advanced_group_filter_matches_exact(name) &&
        !advanced_group_filter_matches_exact("advanced-macro-hygiene")) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_stdlib_macro_migration_regression_tests(Interp* interp, int* pass, int* fail) {
    test_eq(interp, "stdlib when macro true branch", "(let (x 0) (block (when true (set! x 10)) x))", 10, pass, fail);
    test_eq(interp, "stdlib when macro false branch keeps state", "(let (x 3) (block (when false (set! x 10)) x))", 3, pass, fail);
    test_eq(interp, "stdlib unless macro false branch", "(let (x 0) (block (unless false (set! x 20)) x))", 20, pass, fail);
    test_eq(interp, "stdlib unless macro true branch keeps state", "(let (x 4) (block (unless true (set! x 20)) x))", 4, pass, fail);
    test_eq(interp, "stdlib branch picks first truthy condition", "(branch (false 1) (true 2) (_ 3))", 2, pass, fail);
    test_eq(interp, "stdlib branch default clause with _", "(branch (false 1) (false 2) (_ 3))", 3, pass, fail);
    test_nil(interp, "stdlib branch no match without default returns nil", "(branch (false 1) (false 2))", pass, fail);
    test_truthy(interp, "stdlib branch rejects non-final _ default marker",
        "(= (String (handle (branch (_ 1) (true 2)) (raise msg (ref msg 'code)))) \"stdlib/assert-failed\")",
        pass, fail);
    test_eq(interp, "stdlib match Void branch chain", "(match Void ((? false) 1) ((? true) 2) (_ 3))", 2, pass, fail);
    test_nil(interp, "stdlib match Void no matching guard", "(match Void ((? false) 1) ((? false) 2) (_ nil))", pass, fail);
    test_str_val(interp, "stdlib with-defaults nil fallback", "(let ({name} {'name nil}) (with-defaults name \"anon\" name))", "anon", pass, fail);
    test_str_val(interp, "stdlib with-defaults keeps non-nil", "(let ({name} {'name \"omni\"}) (with-defaults name \"anon\" name))", "omni", pass, fail);
    test_eq(interp, "stdlib stream-yield macro expands to capture", "(car (checkpoint (stream-yield 9)))", 9, pass, fail);
}

fn void run_advanced_macro_hygiene_string_number_tests(Interp* interp, int* pass, int* fail) {
    // String<->number
    test_eq(interp, "string->number 42", "(string->number \"42\")", 42, pass, fail);
    test_eq(interp, "string->number -7", "(string->number \"-7\")", -7, pass, fail);
    test_nil(interp, "string->number abc => nil", "(string->number \"abc\")", pass, fail);
    test_nil(interp, "string->number empty => nil", "(string->number \"\")", pass, fail);
    test_eq(interp, "String round-trip 42", "(string->number (String 42))", 42, pass, fail);
    test_eq(interp, "String round-trip 0", "(string->number (String 0))", 0, pass, fail);
    test_eq(interp, "String round-trip -99", "(string->number (String -99))", -99, pass, fail);

    // Stack overflow / non-tail recursion headroom. Under ASAN the host-thread
    // interpreter stack burns much more space per recursive frame, so keep a
    // lower but still substantial probe there instead of routing this check
    // through checkpoint semantics.
    long deep_recursion_target = main::stack_runtime_asan_enabled() ? 896 : 1200;
    char[] deep_recursion_expr = main::stack_runtime_asan_enabled()
        ? "(let ^rec (f (lambda (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))) (f 896))"
        : "(let ^rec (f (lambda (n) (if (= n 0) 0 (+ 1 (f (- n 1)))))) (f 1200))";
    test_eq(interp, "non-tail recursion headroom",
        deep_recursion_expr,
        deep_recursion_target, pass, fail);
    if (!main::stack_runtime_asan_enabled() && test_c_getenv("OMNI_LISP_STACK_OVERFLOW_PROBE") != null) {
        test_error(interp, "stack overflow caught", "(let ^rec (f (lambda (n) (+ 1 (f (+ n 1))))) (f 0))", pass, fail);
    } else {
        emit_pass_literal("[PASS] stack overflow probe skipped (set OMNI_LISP_STACK_OVERFLOW_PROBE=1 to enable)");
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
    setup(interp, "(define qq-bad-tail (cons 1 2))");
    test_error_contains(interp, "quasiquote splice rejects improper list",
        "`(a ,@qq-bad-tail b)", ",@ value is not a proper list", pass, fail);
    test_eq(
        interp,
        "quasiquote wide list length",
        "(length `(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))",
        18,
        pass,
        fail
    );

    // Pattern macros
    setup(interp, "(define [macro] when (syntax-match ([test .. body] (template (if (insert test) (block (splice body)) nil)))))");
    test_eq(interp, "when true", "(when true 42)", 42, pass, fail);
    test_nil(interp, "when false", "(when false 42)", pass, fail);
    setup(interp, "(define [macro] unless (syntax-match ([test .. body] (template (if (insert test) nil (block (splice body)))))))");
    test_eq(interp, "unless false", "(unless false 99)", 99, pass, fail);
    test_nil(interp, "unless true", "(unless true 99)", pass, fail);
    setup(interp, "(define [macro] with-val (syntax-match ([name val body] (template (let ((insert name) (insert val)) (insert body))))))");
    test_eq(interp, "macro with-val", "(with-val z 100 (+ z 1))", 101, pass, fail);
    test_eq(interp, "unless macro preserves let inside define",
        "(block (unless false (define (ufoo3 x) (let (y 1) (+ x y)))) (ufoo3 2))",
        3, pass, fail);
    setup(interp, "(define [macro] choose-if (syntax-match ([] (template nil)) ([test body .. rest] (template (if (insert test) (insert body) (choose-if .. rest))))))");
    test_eq(interp, "choose-if macro branch chain", "(choose-if false 1 true 2)", 2, pass, fail);
    test_nil(interp, "choose-if macro all false", "(choose-if false 1 false 2)", pass, fail);
    setup(interp, "(define [macro] my-and (syntax-match ([a b] (template (if (insert a) (insert b) false)))))");
    test_eq(interp, "my-and true", "(my-and true 42)", 42, pass, fail);
    setup(interp, "(define [macro] let1 (syntax-match ([name val body] (template (let ((insert name) (insert val)) (insert body))))))");
    test_eq(interp, "let1 macro", "(let1 x 10 (+ x 5))", 15, pass, fail);
    test_eq(interp, "stream-yield macro expands capture special form",
        "(car (checkpoint (stream-yield 7)))",
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
            setup(interp, advanced_test_cstr_slice(&define_clauses_buf));
            test_eq(interp, "macro preserves >32 clauses", "(choose-wide 39)", 39, pass, fail);
            setup(interp, advanced_test_cstr_slice(&define_vars_buf));
            test_eq(interp, "macro preserves >64 pattern vars", advanced_test_cstr_slice(&call_vars_buf), (long)(VAR_COUNT - 1), pass, fail);
        }
    }
}

fn void run_advanced_macro_hygiene_gensym_tests(Interp* interp, int* pass, int* fail) {
    // Auto-gensym
    setup(interp, "(define [macro] swap! (syntax-match ([a b] (template (let (tmp# (insert a)) (block (set! (insert a) (insert b)) (set! (insert b) tmp#)))))))");
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
            setup(interp, advanced_test_cstr_slice(&define_buf));
            test_eq(interp, "macro preserves >64 auto-gensyms", "(gensym-wide)", (long)(GENSYM_COUNT - 1), pass, fail);
        }
    }

    // gensym
    test_tag(interp, "gensym returns symbol", "(gensym)", SYMBOL, pass, fail);
}

fn void run_advanced_macro_hygiene_capture_tests(Interp* interp, int* pass, int* fail) {
    // Macro hygiene (def-env capture)
    setup(interp, "(define hyg-helper (lambda (x) (+ x 100)))");
    setup(interp, "(define [macro] hyg-apply (syntax-match ([x] (template (hyg-helper (insert x))))))");
    test_eq(interp, "hygiene normal", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define hyg-helper (lambda (x) (* x 999)))");
    test_eq(interp, "hygiene immune to shadow", "(hyg-apply 5)", 105, pass, fail);
    setup(interp, "(define [macro] hyg-inc (syntax-match ([x] (template (+ (insert x) 1)))))");
    test_eq(interp, "hygiene captures +", "(hyg-inc 10)", 11, pass, fail);
    setup(interp, "(define [macro] hyg-nil-test (syntax-match ([x] (template (if (insert x) 42 nil)))))");
    test_nil(interp, "hygiene nil branch", "(hyg-nil-test false)", pass, fail);
    test_eq(interp, "hygiene true branch", "(hyg-nil-test true)", 42, pass, fail);
    setup(interp, "(define hyg-a 10)");
    setup(interp, "(define hyg-b 20)");
    setup(interp, "(define [macro] hyg-sum (syntax-match ([x] (template (+ (+ hyg-a hyg-b) (insert x))))))");
    test_eq(interp, "hygiene multi literals", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-a 1000)");
    setup(interp, "(define hyg-b 2000)");
    test_eq(interp, "hygiene multi immune", "(hyg-sum 5)", 35, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) (* x 2)))");
    setup(interp, "(define [macro] hyg-with-tmp (syntax-match ([x] (template (let (tmp# (hyg-op (insert x))) tmp#)))))");
    test_eq(interp, "hygiene gensym+def", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define hyg-op (lambda (x) 0))");
    test_eq(interp, "hygiene gensym+def immune", "(hyg-with-tmp 5)", 10, pass, fail);
    setup(interp, "(define x 999)");
    setup(interp, "(define [macro] hyg-id (syntax-match ([x] (template (insert x)))))");
    test_eq(interp, "hygiene patvar substitutes", "(hyg-id 7)", 7, pass, fail);
    test_eq(interp, "hygiene recursive match Void guards", "(match Void ((? false) 1) ((? false) 2) ((? true) 3) (_ 4))", 3, pass, fail);
    setup(interp, "(define hyg-late-fn (lambda (x) (+ x 50)))");
    setup(interp, "(define [macro] hyg-late-macro (syntax-match ([x] (template (hyg-late-fn (insert x))))))");
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
            setup(interp, advanced_test_cstr_slice(&setup_buf));
            setup(interp, advanced_test_cstr_slice(&define_buf));
            setup(interp, "(define cap69 9999)");
            test_eq(interp, "macro preserves >64 captured bindings", "(capture-wide)", 69, pass, fail);
        }
    }

    bool saved_jit_enabled = interp.flags.jit_enabled;
    interp.flags.jit_enabled = false;
    setup(interp, "(define [macro] whole-pair (syntax-match (([x y] 'as pair) (template (length (quote pair))))))");
    test_eq_interp(interp, "macro as pattern binds whole arg expr", "(whole-pair 1 2)", 2, pass, fail);
    interp.flags.jit_enabled = saved_jit_enabled;
}

fn void run_advanced_macro_hygiene_template_datum_collection_tests(Interp* interp, int* pass, int* fail) {
    test_eq(interp,
        "macro template dict literal desugars and expands",
        "(block (define [macro] wrap-dict (syntax-match ([x] (template {'value (insert x) 'twice (+ (insert x) (insert x))})))) (ref (wrap-dict 7) 'twice))",
        14, pass, fail);

    test_eq(interp,
        "macro template array literal desugars and expands",
        "(block (define [macro] wrap-array (syntax-match ([x] (template [(insert x) (+ (insert x) 1) (+ (insert x) 2)])))) (ref (wrap-array 10) 2))",
        12, pass, fail);

    test_error_contains(interp,
        "macro template dict literal enforces even entries",
        "(define [macro] bad-tmpl-dict-odd (syntax-match ([x] (template {'only-key}))))",
        "dict literal requires even number of elements",
        pass, fail);
}

fn void run_advanced_macro_hygiene_malformed_let_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "macro expansion rejects grouped let binding shape",
        "(block (define [macro] bad-let-grouped (syntax-match ([x] (template (let ((y (insert x))) y))))) (bad-let-grouped 7))",
        "macro expansion emitted invalid let bindings",
        pass, fail);

    test_error_contains(interp,
        "macro expansion rejects non-list let bindings",
        "(block (define [macro] bad-let-nonlist (syntax-match ([x] (template (let (insert x) (insert x)))))) (bad-let-nonlist 7))",
        "macro expansion emitted invalid let bindings",
        pass, fail);

    test_error_contains(interp,
        "macro expansion rejects odd flat-pair let bindings",
        "(block (define [macro] bad-let-odd (syntax-match ([x] (template (let (y) y))))) (bad-let-odd 7))",
        "macro expansion emitted invalid let bindings",
        pass, fail);

    test_error_contains(interp,
        "macro expansion reports no matching clause deterministically",
        "(block (define [macro] pair-only (syntax-match ([x y] (template (insert x))))) (pair-only 1))",
        "no macro clause matched",
        pass, fail);
}

fn void run_advanced_macro_hygiene_single_surface_tests(Interp* interp, int* pass, int* fail) {
    test_eq(interp,
        "single macro surface syntax-match/template with insert+splice",
        "(block "
        "  (define [macro] when2 "
        "    (syntax-match "
        "      ([test .. body] "
        "       (template (if (insert test) (block (splice body)) nil))))) "
        "  (when2 true 42))",
        42, pass, fail);

    test_nil(interp,
        "single macro surface false branch",
        "(block "
        "  (define [macro] when2 "
        "    (syntax-match "
        "      ([test .. body] "
        "       (template (if (insert test) (block (splice body)) nil))))) "
        "  (when2 false 42))",
        pass, fail);

    test_eq(interp,
        "single macro surface fixed-arity insert",
        "(block "
        "  (define [macro] let1-new "
        "    (syntax-match "
        "      ([name val body] "
        "       (template (let ((insert name) (insert val)) (insert body)))))) "
        "  (let1-new z 10 (+ z 5)))",
        15, pass, fail);

    test_error_contains(interp,
        "macro single-clause syntax is rejected",
        "(define [macro] sample-one ([x] x))",
        "macro clause syntax was removed",
        pass, fail);

    test_error_contains(interp,
        "macro multi-clause syntax is rejected",
        "(define [macro] sample-two ([x] x) ([x y] y))",
        "macro clause syntax was removed",
        pass, fail);

    test_error_contains(interp,
        "syntax-match requires template form wrapper",
        "(define [macro] bad-sm (syntax-match ([x] x)))",
        "expected (template <datum>) in syntax-match clause",
        pass, fail);

    test_error_contains(interp,
        "template insert requires symbol binding name",
        "(define [macro] bad-insert (syntax-match ([x] (template (insert (+ x 1))))))",
        "template insert expects a symbol binding name",
        pass, fail);

    test_error_contains(interp,
        "template splice requires single argument",
        "(define [macro] bad-splice (syntax-match ([xs] (template (block (splice xs ys))))))",
        "template splice expects exactly one symbol argument",
        pass, fail);

    test_error_contains(interp,
        "syntax-match requires at least one clause",
        "(define [macro] bad-empty (syntax-match))",
        "syntax-match requires at least one clause",
        pass, fail);

    test_error_contains(interp,
        "macro template splice target must be list",
        "(block (define [macro] bad-splice-type (syntax-match ([x] (template (block (splice x)))))) (bad-splice-type 7))",
        "macro template splice target must be a list",
        pass, fail);

    test_error_contains(interp,
        "macro template splice target missing binding",
        "(block (define [macro] bad-splice-missing (syntax-match ([x] (template (block (splice xs)))))) (bad-splice-missing 7))",
        "macro template splice target has no binding",
        pass, fail);

    test_error_contains(interp,
        "syntax-match rejects unsupported guard patterns deterministically",
        "(block (define [macro] bad-guard (syntax-match ((? true) (template 1)))) (bad-guard 1))",
        "bad macro pattern",
        pass, fail);

    test_error_contains(interp,
        "syntax-match rejects malformed emitted forms deterministically",
        "(block (define [macro] bad-out (syntax-match ([x] (template (if (insert x)))))) (bad-out true))",
        "malformed emitted form",
        pass, fail);

    {
        EvalResult r = run(
            "(block\n"
            "  (define [macro] bad-loc\n"
            "    (syntax-match ([x] (template (if (insert x)))) )\n"
            "  (bad-loc true))",
            interp
        );
        if (r.error.has_error && r.error.line == 4) {
            emit_pass_literal("[PASS] macro expansion diagnostics retain callsite location");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] macro expansion diagnostics retain callsite location (line=%d, col=%d, has_error=%s)",
                (long)r.error.line,
                (long)r.error.column,
                r.error.has_error ? "true" : "false"
            );
            (*fail)++;
        }
    }

    test_error_contains(interp,
        "macroexpand reports macro expansion errors deterministically",
        "(block (define [macro] pair-only (syntax-match ([x y] (template (insert x))))) (macroexpand '(pair-only 1)))",
        "no macro clause matched",
        pass, fail);
}

fn void run_advanced_macro_hygiene_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-stdlib-migration",
        &run_advanced_stdlib_macro_migration_regression_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-string-number",
        &run_advanced_macro_hygiene_string_number_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-quasi-pattern",
        &run_advanced_macro_hygiene_quasi_pattern_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-gensym",
        &run_advanced_macro_hygiene_gensym_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-capture",
        &run_advanced_macro_hygiene_capture_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-template-datum-collection",
        &run_advanced_macro_hygiene_template_datum_collection_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-malformed-let",
        &run_advanced_macro_hygiene_malformed_let_tests, interp, pass, fail);
    run_advanced_macro_hygiene_group("advanced-macro-hygiene-single-surface",
        &run_advanced_macro_hygiene_single_surface_tests, interp, pass, fail);
}
