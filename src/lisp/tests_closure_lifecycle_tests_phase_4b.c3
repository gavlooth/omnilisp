module lisp;


import std::io;
import main;
// =============================================================================
// SECTION 11: CLOSURE LIFECYCLE TESTS (Phase 4b)
// =============================================================================

fn void run_closure_lifecycle_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Closure Lifecycle Tests (Phase 4b) ---");

    // 1. Temporary closure freed — anonymous lambda passed to HOF
    test_eq(interp, "closure-lc: temp lambda in map",
        "(car (map (lambda (x) (+ x 10)) '(1 2 3)))", 11, pass, fail);

    // 2. Escaping closure persists via define
    setup(interp, "(define (clc-make-adder x) (lambda (y) (+ x y)))");
    setup(interp, "(define clc-add5 (clc-make-adder 5))");
    test_eq(interp, "closure-lc: escaping closure via define",
        "(clc-add5 10)", 15, pass, fail);

    // 3. Nested closures — inner closure captures from outer
    setup(interp, "(define (clc-nest x) (lambda (y) (lambda (z) (+ (+ x y) z))))");
    test_eq(interp, "closure-lc: nested closure capture",
        "(((clc-nest 1) 2) 3)", 6, pass, fail);

    // 4. Recursive closure (let ^rec) — weak self-reference works
    test_eq(interp, "closure-lc: recursive let ^rec",
        "(let ^rec (fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))", 55, pass, fail);

    // 5. Recursive closure + escape — define'd recursive closure persists
    setup(interp, "(define clc-fact (let ^rec (f (lambda (n acc) (if (= n 0) acc (f (- n 1) (* acc n))))) f))");
    test_eq(interp, "closure-lc: recursive closure escapes via define",
        "(clc-fact 10 1)", 3628800, pass, fail);

    // 6. Closure shared in cons — refcount handles sharing correctly
    setup(interp, "(define clc-shared-fn (lambda (x) (+ x 1)))");
    test_eq(interp, "closure-lc: closure in cons (car)",
        "((car (cons clc-shared-fn clc-shared-fn)) 5)", 6, pass, fail);
    test_eq(interp, "closure-lc: closure in cons (cdr)",
        "((cdr (cons clc-shared-fn clc-shared-fn)) 7)", 8, pass, fail);

    // 7. HOF chain — intermediate closures freed, final result correct
    test_eq(interp, "closure-lc: HOF chain map+filter",
        "(car (filter (lambda (x) (> x 5)) (map (lambda (x) (* x 2)) '(1 2 3 4 5))))", 6, pass, fail);

    // 8. TCO recycling with temporary closure — named-let creating lambdas per iteration
    test_eq(interp, "closure-lc: TCO with temp closures",
        "(let loop (n 1000 acc 0) (if (= n 0) acc (let (f (lambda (x) (+ x n))) (loop (- n 1) (+ acc (f 0))))))",
        500500, pass, fail);

    // 9. Closure over mutable variable — fresh counter per eval (avoids shared state between interp/jit)
    test_eq(interp, "closure-lc: mutable capture counter",
        "(let (counter (let (n 0) (lambda () (set! n (+ n 1)) n))) (block (counter) (counter) (counter)))", 3, pass, fail);

    // 10. Coroutine with closure thunk — closure promoted to root_scope
    test_eq(interp, "closure-lc: coroutine with closure thunk",
        "(let (f (coroutine (lambda () (yield 1) (yield 2) 3))) (+ (resume f) (+ (resume f) (resume f))))", 6, pass, fail);
}

