module lisp;

import std::io;

fn void run_advanced_core_semantics_match_tests(Interp* interp, int* pass, int* fail) {
    // Match patterns
    test_eq(interp, "match 1 => 10", "(match 1 (1 10) (2 20) (3 30))", 10, pass, fail);
    test_eq(interp, "match 2 => 20", "(match 2 (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match 3 => 30", "(match 3 (1 10) (2 20) (3 30))", 30, pass, fail);
    test_eq(interp, "match default => 100", "(match 99 (1 10) (2 20) (x (+ x 1)))", 100, pass, fail);
    test_eq(interp, "match computed", "(match (+ 1 1) (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match [a b] => 30", "(match (quote (10 20)) ([a b] (+ a b)))", 30, pass, fail);
    test_eq(interp, "match [x] => 42", "(match (quote (42)) ([x] x))", 42, pass, fail);
    test_eq(interp, "match rejects improper sequence input", "(match (cons 1 2) ([x] 1) (_ 0))", 0, pass, fail);
    test_eq(interp, "match rejects improper rest sequence input", "(match (cons 1 2) ([x .. tail] 1) (_ 0))", 0, pass, fail);
    test_eq(interp, "match rejects scalar empty sequence input", "(match 7 ([] 1) (_ 0))", 0, pass, fail);
    test_eq(interp, "match [a b c] product", "(match (quote (2 3 4)) ([a b c] (* a (* b c))))", 24, pass, fail);
    test_eq(interp, "match [h .. t] tail len", "(match (quote (1 2 3 4 5)) ([h .. t] (length t)))", 4, pass, fail);
    test_eq(interp, "match _ wildcard", "(match 42 (_ 0))", 0, pass, fail);
    test_eq(interp, "match nil literal pattern", "(match nil (nil 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "match false literal pattern", "(match false (false 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "match nil does not match false literal", "(match nil (false 1) (_ 0))", 0, pass, fail);
    test_eq(interp, "match Void literal pattern", "(match (Void) (Void 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "match wildcard fallthrough after nil false Void literals",
        "(match 42 (nil 1) (false 2) (Void 3) (_ 9))",
        9, pass, fail);
    test_eq(interp, "match guard sub-pattern failure skips guard evaluation",
        "(let (trace []) "
        "  (match [1 2] "
        "    ((? (block (push! trace 'g1) true) [x]) 1) "
        "    (_ (length trace))))",
        0, pass, fail);
    test_eq(interp, "match guard sees sub-pattern bindings",
        "(match [7] ((? (> x 0) [x]) x) (_ 0))",
        7, pass, fail);
    test_eq(interp, "match guard evaluation stops at first matching clause",
        "(let (trace []) "
        "  (match [7] "
        "    ((? (block (push! trace 'g1) (> x 0)) [x]) (length trace)) "
        "    ((? (block (push! trace 'g2) true) [_]) 99) "
        "    (_ 0)))",
        1, pass, fail);
    test_error_contains(interp, "match guard runtime error propagates",
        "(match 5 ((? no_such_guard) 1) (_ 0))",
        "unbound variable", pass, fail);
    test_eq(interp, "match string hello", "(match \"hello\" (\"hello\" 1) (\"world\" 2) (x 0))", 1, pass, fail);
    test_eq(interp, "match string world", "(match \"world\" (\"hello\" 1) (\"world\" 2) (x 0))", 2, pass, fail);
    test_eq(interp, "match string default", "(match \"other\" (\"hello\" 1) (\"world\" 2) (x 0))", 0, pass, fail);
    {
        const usz LONG_LIST_COUNT = 80;
        char[1024] match_buf;
        if (!advanced_build_match_last_list_expr(&match_buf, match_buf.len, LONG_LIST_COUNT)) {
            io::printn("[FAIL] long list match regression builder");
            (*fail)++;
        } else {
            test_eq(interp, "match [.. last] long list >64",
                advanced_test_cstr_slice(&match_buf),
                (long)(LONG_LIST_COUNT - 1), pass, fail);
        }
    }
    test_eq(interp, "match as pattern binds whole list",
        "(match (quote (10 20)) (([a b] 'as pair) (length pair)))",
        2, pass, fail);
    test_eq(interp, "match as pattern binds inner and whole",
        "(match (quote (1 2)) (([_ y] 'as pair) (+ y (length pair))))",
        4, pass, fail);
    test_error_contains(interp, "match bare as marker rejected",
        "(match (quote (1 2)) (([x y] as pair) pair))",
        "expected 'as in as-pattern", pass, fail);
    test_error_contains(interp, "match clause missing result rejected",
        "(match 1 (1))",
        "match clauses must be (pattern result) pairs", pass, fail);
    test_error_contains(interp, "match clause extra form rejected",
        "(match 1 (1 10 20))",
        "match clauses must be (pattern result) pairs", pass, fail);
}

fn void run_advanced_core_semantics_continuation_tests(Interp* interp, int* pass, int* fail) {
    // Continuations
    test_eq(interp, "capture discard k", "(checkpoint (+ 100 (capture k 42)))", 42, pass, fail);
    test_eq(interp, "capture k mul", "(checkpoint (* 2 (capture k (k 5))))", 10, pass, fail);
    test_eq(interp, "capture k expr", "(checkpoint (+ 1 (capture k (k (+ 2 3)))))", 6, pass, fail);
    test_eq(interp, "capture use k result", "(checkpoint (+ 1 (capture k (+ (k 10) 1000))))", 1011, pass, fail);
    test_eq(interp, "checkpoint value", "(checkpoint 42)", 42, pass, fail);
}

fn void run_advanced_core_semantics_effect_cond_tests(Interp* interp, int* pass, int* fail) {
    // Effect handlers
    test_eq(interp, "effect modify", "(handle (+ 1 (signal get 0)) (get x (resolve 100)))", 101, pass, fail);
    test_eq(interp, "effect arg double", "(handle (signal double 7) (double x (resolve (* x 2))))", 14, pass, fail);
    test_eq(interp, "effect bail", "(handle (+ 1 (+ 2 (+ 3 (signal bail 42)))) (bail x x))", 42, pass, fail);
    test_eq(interp, "handle no effect", "(handle (+ 10 20) (unused x 999))", 30, pass, fail);

    // If edge cases
    test_eq(interp, "if nil => 2", "(if nil 1 2)", 2, pass, fail);
    test_eq(interp, "if false => 2", "(if false 1 2)", 2, pass, fail);
    test_eq(interp, "if 0 => 1 (truthy)", "(if 0 1 2)", 1, pass, fail);
    test_eq(interp, "if empty-str => 1 (truthy)", "(if \"\" 1 2)", 1, pass, fail);
    test_eq(interp, "if '() => 2 (nil)", "(if (quote ()) 1 2)", 2, pass, fail);
    test_eq(interp, "if '(1) => 1 (truthy)", "(if (quote (1)) 1 2)", 1, pass, fail);
    test_eq(interp, "nested if", "(if true (if false 1 2) 3)", 2, pass, fail);
    test_eq(interp, "if with branches", "(if (= 1 1) (+ 10 20) (+ 30 40))", 30, pass, fail);
}

fn void run_advanced_core_semantics_letrec_tests(Interp* interp, int* pass, int* fail) {
    test_eq(interp, "let ^rec closure mutation preserves captured array state",
        "(block (define collect-indices (lambda (arr) (let (result []) (let ^rec (loop (lambda (i) (if (= i (length arr)) result (block (push! result i) (loop (+ i 1)))))) (loop 0))))) (length (collect-indices [10 20 30])))",
        3, pass, fail);
    test_eq(interp, "let ^rec closure mutation preserves appended values",
        "(block (define collect-indices (lambda (arr) (let (result []) (let ^rec (loop (lambda (i) (if (= i (length arr)) result (block (push! result i) (loop (+ i 1)))))) (loop 0))))) (ref (collect-indices [10 20 30]) 2))",
        2, pass, fail);
}

fn void run_advanced_core_semantics_error_tests(Interp* interp, int* pass, int* fail) {
    // Error handling
    test_error_contains(interp, "unbound variable", "nonexistent_var_xyz", "unbound variable", pass, fail);
    test_error_contains(interp, "call non-function", "(42 1)", "not a function", pass, fail);
    test_error_contains(interp, "capture outside checkpoint", "(capture k 42)", "capture outside of checkpoint", pass, fail);
}

fn void run_advanced_core_semantics_try_raise_tests(Interp* interp, int* pass, int* fail) {
    // Error-as-effect: try/catch via raise effect
    test_eq(interp, "handle catches raise",
        "(handle (/ 1 0) (raise msg 42))", 42, pass, fail);
    test_str_val(interp, "try catch div-by-zero",
        "(try (lambda (xx) (/ 1 0)) (lambda (payload) (ref payload 'message)))",
        "/: division by zero", pass, fail);
    test_str_val(interp, "try catch arity error",
        "(try (lambda (xx) (+ )) (lambda (payload) (ref payload 'message)))",
        "+: arity mismatch: expected 1 or 2 arguments, got 0", pass, fail);
    test_str_val(interp, "try catch type error",
        "(try (lambda (xx) (+ \"a\" 1)) (lambda (payload) (ref payload 'message)))",
        "+: argument 1 expected number, got \"a\"", pass, fail);
    test_str_val(interp, "try catch oob array",
        "(try (lambda (xx) (ref [1 2 3] 99)) (lambda (payload) (ref payload 'message)))",
        "ref: array index out of bounds", pass, fail);
    test_str_val(interp, "try catch oob string",
        "(try (lambda (xx) (char-at \"abc\" 99)) (lambda (payload) (ref payload 'message)))",
        "char-at: index out of range", pass, fail);
    test_str_val(interp, "try catch car nil",
        "(try (lambda (xx) (car 42)) (lambda (payload) (ref payload 'message)))",
        "car: argument 1 expected pair/list, got 42", pass, fail);
    test_str_val(interp, "try catch unbound var",
        "(try (lambda (xx) nonexistent_var_xyz_2) (lambda (payload) (ref payload 'message)))",
        "unbound variable 'nonexistent_var_xyz_2'", pass, fail);
    test_eq(interp, "try no error returns value",
        "(try (lambda (xx) 42) (lambda (msg) 0))",
        42, pass, fail);
    test_str_val(interp, "try catch set! unbound",
        "(try (lambda (xx) (set! no_such_var_xyz 1)) (lambda (payload) (ref payload 'message)))",
        "set!: unbound variable", pass, fail);
    test_str_val(interp, "try catch user raise",
        "(try (lambda (xx) (signal raise \"custom error\")) (lambda (payload) (ref payload 'message)))",
        "custom error", pass, fail);
    test_str_val(interp, "try catches string raise",
        "(try (lambda (xx) (signal raise \"custom error\")) (lambda (payload) (ref payload 'message)))",
        "custom error", pass, fail);
    test_truthy(interp, "try canonical payload shape",
        "(let (payload (try (lambda (xx) (resolve 42)) (lambda (msg) msg))) "
        "  (and (dict? payload) "
        "       (and (has? payload 'code) "
        "            (and (has? payload 'domain) "
        "                 (and (has? payload 'message) "
        "                      (has? payload 'data))))))",
        pass, fail);
    test_str_val(interp, "try canonical payload domain",
        "(try (lambda (xx) (resolve 42)) (lambda (payload) (String (ref payload 'domain))))",
        "runtime", pass, fail);
    test_str_val(interp, "try canonical payload code",
        "(try (lambda (xx) (resolve 42)) (lambda (payload) (String (ref payload 'code))))",
        "runtime/invalid-continuation", pass, fail);
    test_str_val(interp, "try canonical payload message",
        "(try (lambda (xx) (resolve 42)) (lambda (payload) (ref payload 'message)))",
        "resolve: not inside a handler clause", pass, fail);
    test_eq(interp, "nested try inner catches",
        "(try (lambda (xx) (try (lambda (yy) (/ 1 0)) (lambda (msg) 99))) (lambda (msg) 0))",
        99, pass, fail);
    test_eq(interp, "unhandled raise is error",
        "(if (pair? (quote (1))) 1 0)", 1, pass, fail);
}

fn void run_advanced_core_semantics_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_core_semantics_match_tests(interp, pass, fail);
    run_advanced_core_semantics_continuation_tests(interp, pass, fail);
    run_advanced_core_semantics_effect_cond_tests(interp, pass, fail);
    run_advanced_core_semantics_letrec_tests(interp, pass, fail);
    run_advanced_core_semantics_error_tests(interp, pass, fail);
    run_advanced_core_semantics_try_raise_tests(interp, pass, fail);
}
