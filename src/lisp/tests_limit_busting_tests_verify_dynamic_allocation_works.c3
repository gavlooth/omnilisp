module lisp;


import std::io;
import main;
// =============================================================================
// SECTION 10b: LIMIT-BUSTING TESTS (verify dynamic allocation works)
// =============================================================================

fn void run_limit_busting_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Limit-Busting Tests ---");

    // Test 1: begin with >256 expressions
    // Build: (begin 1 2 3 ... 300 999) — should return 999
    {
        char[8192] buf;
        usz pos = 0;
        char[] prefix = "(begin ";
        for (usz i = 0; i < prefix.len; i++) buf[pos++] = prefix[i];
        for (int i = 1; i <= 299; i++) {
            char[20] num;
            char[] ns = int_to_string((long)i, &num);
            for (usz j = 0; j < ns.len; j++) buf[pos++] = ns[j];
            buf[pos++] = ' ';
        }
        char[] suffix = "999)";
        for (usz i = 0; i < suffix.len; i++) buf[pos++] = suffix[i];
        test_eq(interp, "begin >256 exprs", buf[:pos], 999, pass, fail);
    }

    // Test 2: lambda with >64 parameters (old parser used SymbolId[64] stack array)
    // Build: (define (f70 p0 p1 ... p69) p69) then call (f70 0 1 ... 69) with 15 args at a time via currying
    // Instead, test that a 70-param lambda can be defined and returns the right value
    // We test via: (let (f (lambda (p0 p1 ... p69) p69)) (f 0 1 ... 69))
    // But JIT limits multi-arg calls to 16. So instead test define + apply:
    {
        char[4096] buf;
        usz pos = 0;
        // Build: (begin (define (sum70 p0 p1 ... p69) (+ p0 p69)) (sum70 0 1 2 ... 69))
        // With 70 params, define shorthand works. But multi-arg call won't JIT.
        // Instead just test that lambda with 70 params parses and the define succeeds:
        // (begin (define (f70 p0 p1 ... p69) 42) 99)
        // This verifies the parser can handle >64 params. Result = 99 (begin returns last)
        char[] prefix = "(begin (define (f70 ";
        for (usz i = 0; i < prefix.len; i++) buf[pos++] = prefix[i];
        for (int i = 0; i < 70; i++) {
            buf[pos++] = 'p';
            char[20] num;
            char[] ns = int_to_string((long)i, &num);
            for (usz j = 0; j < ns.len; j++) buf[pos++] = ns[j];
            buf[pos++] = ' ';
        }
        // Remove trailing space, close define
        pos--;  // back over last space
        char[] suffix = ") 42) 99)";
        for (usz i = 0; i < suffix.len; i++) buf[pos++] = suffix[i];
        test_eq(interp, "lambda >64 params", buf[:pos], 99, pass, fail);
    }

    // Test 3: match with >128 clauses
    // Build: (match 200 (1 10) (2 20) ... (200 2000) (_ 0))
    {
        char[16384] buf;
        usz pos = 0;
        char[] prefix = "(match 200 ";
        for (usz i = 0; i < prefix.len; i++) buf[pos++] = prefix[i];
        for (int i = 1; i <= 200; i++) {
            buf[pos++] = '(';
            char[20] num;
            char[] ns = int_to_string((long)i, &num);
            for (usz j = 0; j < ns.len; j++) buf[pos++] = ns[j];
            buf[pos++] = ' ';
            char[20] num2;
            char[] ns2 = int_to_string((long)(i * 10), &num2);
            for (usz j = 0; j < ns2.len; j++) buf[pos++] = ns2[j];
            buf[pos++] = ')'; buf[pos++] = ' ';
        }
        char[] suffix = "(_ 0))";
        for (usz i = 0; i < suffix.len; i++) buf[pos++] = suffix[i];
        test_eq(interp, "match >128 clauses", buf[:pos], 2000, pass, fail);
    }

    // Test 4: string->list on string >512 characters
    // Build string at runtime since lexer has 63-byte string literal limit
    test_eq(interp, "string->list >512 chars",
        "(length (string->list (let loop (n 0 acc \"\") (if (>= n 600) acc (loop (+ n 1) (string-append acc \"a\"))))))",
        600, pass, fail);

    // Test 5: string split producing >256 segments
    // Build "a,a,a,...,a" at runtime (300 segments)
    test_eq(interp, "split >256 segments",
        "(let (big (let loop (n 0 acc \"\") (if (>= n 299) (string-append acc \"a\") (loop (+ n 1) (string-append acc \"a,\"))))) (length (string-split big \",\")))",
        300, pass, fail);

    // =========================================================================
    // Scope-per-call tests (Phase 4a)
    // Verify closure body temporaries are freed per call without breaking results.
    // =========================================================================

    // Recursive function — each call creates and releases its own scope
    setup(interp, "(define fib-scope (lambda (n) (if (<= n 1) n (+ (fib-scope (- n 1)) (fib-scope (- n 2))))))");
    test_eq(interp, "scope-per-call: recursive fib", "(fib-scope 10)", 55, pass, fail);

    // Higher-order function — map creates a scope per call to the passed lambda
    test_eq(interp, "scope-per-call: HOF map car",
        "(car (map (lambda (x) (* x x)) '(1 2 3 4 5)))", 1, pass, fail);
    test_eq(interp, "scope-per-call: HOF map last",
        "(car (cdr (cdr (cdr (cdr (map (lambda (x) (* x x)) '(1 2 3 4 5)))))))", 25, pass, fail);

    // Closure escaping call scope — closure created in body survives scope release
    test_eq(interp, "scope-per-call: closure escape",
        "(let (make-adder (lambda (n) (lambda (x) (+ n x)))) (let (add5 (make-adder 5)) (add5 10)))", 15, pass, fail);

    // Named-let loop — TCO iterations accumulate in one call scope, freed on return
    test_eq(interp, "scope-per-call: named-let loop",
        "(let loop (n 1000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))", 500500, pass, fail);

    // Variadic closure — rest args in scope
    test_eq(interp, "scope-per-call: variadic",
        "((lambda (.. xs) (length xs)) 1 2 3 4 5)", 5, pass, fail);

    // Multi-param closure through jit_apply_multi_args path
    test_eq(interp, "scope-per-call: multi-param",
        "((lambda (a b c) (+ a (* b c))) 1 2 3)", 7, pass, fail);

    // Zero-arg closure
    test_eq(interp, "scope-per-call: zero-arg",
        "((lambda () 42))", 42, pass, fail);

    // Nested calls — inner call scope freed before outer
    test_eq(interp, "scope-per-call: nested calls",
        "(let (f (lambda (x) (* x 2))) (let (g (lambda (y) (+ (f y) 1))) (g 10)))", 21, pass, fail);

    // Chain-apply (extra args beyond param count)
    test_eq(interp, "scope-per-call: chain-apply",
        "((lambda (x) (lambda (y) (+ x y))) 3 4)", 7, pass, fail);

    // Deep recursion — stress test scope create/release cycle
    test_eq(interp, "scope-per-call: deep recursion",
        "(let loop (n 10000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", 10000, pass, fail);
}

fn void run_tco_recycling_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- TCO Scope Recycling Tests ---");

    // 1. Named-let loop — basic regression, result correct after scope recycling
    test_eq(interp, "tco-recycle: named-let sum",
        "(let loop (n 100 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))", 5050, pass, fail);

    // 2. Named-let with closures — closures escape so RC>1, no recycle
    test_eq(interp, "tco-recycle: closure escape in loop",
        "(let (closures '()) (let loop (n 5 acc closures) (if (= n 0) (length acc) (loop (- n 1) (cons (lambda (x) (+ x n)) acc)))))", 5, pass, fail);

    // 3. Named-let with string args — strings deep-copied across bounces
    test_eq(interp, "tco-recycle: string accumulation",
        "(string-length (let loop (n 5 acc \"\") (if (= n 0) acc (loop (- n 1) (string-append acc \"x\")))))", 5, pass, fail);

    // 4. Named-let with cons args — cons chain copied correctly
    test_eq(interp, "tco-recycle: cons arg copy",
        "(let loop (n 5 lst '()) (if (= n 0) (length lst) (loop (- n 1) (cons n lst))))", 5, pass, fail);

    // 5. Multi-param named-let — all bindings preserved across recycling
    test_eq(interp, "tco-recycle: multi-param loop",
        "(let loop (a 0 b 1 c 2 n 100) (if (= n 0) (+ (+ a b) c) (loop (+ a 1) (+ b 2) (+ c 3) (- n 1))))",
        (long)(100 + 201 + 302), pass, fail);

    // 6. Variadic tail call — rest args in tail position
    setup(interp, "(define (tco-variadic-sum .. args) (let loop (lst args acc 0) (if (null? lst) acc (loop (cdr lst) (+ acc (car lst))))))");
    test_eq(interp, "tco-recycle: variadic tail call",
        "(tco-variadic-sum 1 2 3 4 5 6 7 8 9 10)", 55, pass, fail);

    // 7. Mutual tail call chain — f and g alternate, recycling applies in each
    setup(interp, "(define (tco-f n) (if (= n 0) 0 (tco-g (- n 1))))");
    setup(interp, "(define (tco-g n) (if (= n 0) 0 (tco-f (- n 1))))");
    test_eq(interp, "tco-recycle: mutual tail calls",
        "(tco-f 10000)", 0, pass, fail);

    // 8. Zero-arg closure in tail position — no env copy needed
    setup(interp, "(define (tco-zero-arg) (let (x 42) x))");
    test_eq(interp, "tco-recycle: zero-arg tail",
        "(tco-zero-arg)", 42, pass, fail);

    // 9. Large iteration count — verify memory stays flat (no OOM)
    test_eq(interp, "tco-recycle: 100K iterations",
        "(let loop (n 100000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", 100000, pass, fail);

    // 10. Named-let with effects — handle/signal interacts with TCO
    test_eq(interp, "tco-recycle: effects in loop",
        "(handle (let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1)))) (raise msg 999))", 5, pass, fail);
}

