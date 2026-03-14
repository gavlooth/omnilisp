module lisp;

fn void run_advanced_core_semantics_match_tests(Interp* interp, int* pass, int* fail) {
    // Match patterns
    test_eq(interp, "match 1 => 10", "(match 1 (1 10) (2 20) (3 30))", 10, pass, fail);
    test_eq(interp, "match 2 => 20", "(match 2 (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match 3 => 30", "(match 3 (1 10) (2 20) (3 30))", 30, pass, fail);
    test_eq(interp, "match default => 100", "(match 99 (1 10) (2 20) (x (+ x 1)))", 100, pass, fail);
    test_eq(interp, "match computed", "(match (+ 1 1) (1 10) (2 20) (3 30))", 20, pass, fail);
    test_eq(interp, "match [a b] => 30", "(match (quote (10 20)) ([a b] (+ a b)))", 30, pass, fail);
    test_eq(interp, "match [x] => 42", "(match (quote (42)) ([x] x))", 42, pass, fail);
    test_eq(interp, "match [a b c] product", "(match (quote (2 3 4)) ([a b c] (* a (* b c))))", 24, pass, fail);
    test_eq(interp, "match [h .. t] tail len", "(match (quote (1 2 3 4 5)) ([h .. t] (length t)))", 4, pass, fail);
    test_eq(interp, "match _ wildcard", "(match 42 (_ 0))", 0, pass, fail);
    test_eq(interp, "match string hello", "(match \"hello\" (\"hello\" 1) (\"world\" 2) (x 0))", 1, pass, fail);
    test_eq(interp, "match string world", "(match \"world\" (\"hello\" 1) (\"world\" 2) (x 0))", 2, pass, fail);
    test_eq(interp, "match string default", "(match \"other\" (\"hello\" 1) (\"world\" 2) (x 0))", 0, pass, fail);
    test_eq(interp, "match [.. last] long list >64",
        "(match (let loop (i 79 acc (quote ())) (if (< i 0) acc (loop (- i 1) (cons i acc)))) ([.. last] last))",
        79, pass, fail);
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

fn void run_advanced_core_semantics_error_tests(Interp* interp, int* pass, int* fail) {
    // Error handling
    test_error(interp, "unbound variable", "nonexistent_var_xyz", pass, fail);
    test_error(interp, "call non-function", "(42 1)", pass, fail);
    test_error(interp, "capture outside checkpoint", "(capture k 42)", pass, fail);
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
        "+: expected at least 1 argument", pass, fail);
    test_str_val(interp, "try catch type error",
        "(try (lambda (xx) (+ \"a\" 1)) (lambda (payload) (ref payload 'message)))",
        "+: expected number argument", pass, fail);
    test_str_val(interp, "try catch oob array",
        "(try (lambda (xx) (ref [1 2 3] 99)) (lambda (payload) (ref payload 'message)))",
        "ref: array index out of bounds", pass, fail);
    test_str_val(interp, "try catch oob string",
        "(try (lambda (xx) (char-at \"abc\" 99)) (lambda (payload) (ref payload 'message)))",
        "char-at: index out of range", pass, fail);
    test_str_val(interp, "try catch car nil",
        "(try (lambda (xx) (car 42)) (lambda (payload) (ref payload 'message)))",
        "car: argument must be a pair", pass, fail);
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
    test_str_val(interp, "try catches legacy string raise",
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
        "(try (lambda (xx) (resolve 42)) (lambda (payload) (symbol->string (ref payload 'domain))))",
        "runtime", pass, fail);
    test_str_val(interp, "try canonical payload code",
        "(try (lambda (xx) (resolve 42)) (lambda (payload) (symbol->string (ref payload 'code))))",
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
    run_advanced_core_semantics_error_tests(interp, pass, fail);
    run_advanced_core_semantics_try_raise_tests(interp, pass, fail);
}
