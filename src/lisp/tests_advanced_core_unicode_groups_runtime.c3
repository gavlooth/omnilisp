module lisp;

import main;

fn void run_advanced_runtime_control_tests(Interp* interp, int* pass, int* fail) {
    // Trampoline (JIT delegates effects to eval which handles them)
    test_eq(interp, "trampoline passthrough", "(with-trampoline (lambda (xx) 42))", 42, pass, fail);
    test_eq(interp, "trampoline single bounce", "(with-trampoline (lambda (xx) (+ 1 (signal bounce (lambda (xx) 41)))))", 42, pass, fail);
    setup(interp, "(define fib-t (lambda (n) (if (< n 2) n (+ (signal bounce (lambda (xx) (fib-t (- n 1)))) (signal bounce (lambda (xx) (fib-t (- n 2))))))))");
    test_eq(interp, "trampoline fib 7", "(with-trampoline (lambda (xx) (fib-t 7)))", 13, pass, fail);

    // Coroutine primitives (coroutine/resume/yield)
    test_eq(interp, "coroutine basic", "(resume (Coroutine (lambda () (+ 1 2))))", 3, pass, fail);
    test_eq(interp, "Coroutine basic", "(resume (Coroutine (lambda () (+ 1 2))))", 3, pass, fail);
    test_eq(interp, "coroutine yield", "(let (f (Coroutine (lambda () (yield 42) 99))) (resume f))", 42, pass, fail);
    test_eq(interp, "coroutine yield complete", "(let (f (Coroutine (lambda () (yield 42) 99))) (resume f) (resume f))", 99, pass, fail);
    test_eq(interp, "coroutine resume value", "(let (f (Coroutine (lambda () (+ 1 (yield 0))))) (resume f) (resume f 10))", 11, pass, fail);
    test_eq(
        interp,
        "coroutine implicit nil after explicit resume value",
        "(let (f (Coroutine (lambda () (block (yield 0) (if (yield 0) 99 1))))) (resume f) (resume f 10) (resume f))",
        1,
        pass,
        fail
    );
    test_eq(
        interp,
        "effect resume value does not leak into later coroutine resume",
        "(block (handle (+ 1 (signal ask 0)) (ask x (resolve 41))) (let (f (Coroutine (lambda () (if (yield 0) 99 1)))) (resume f) (resume f)))",
        1,
        pass,
        fail
    );
    test_eq(
        interp,
        "capture resume value does not leak into later coroutine resume",
        "(block (checkpoint (+ 1 (capture k (k 41)))) (let (f (Coroutine (lambda () (if (yield 0) 99 1)))) (resume f) (resume f)))",
        1,
        pass,
        fail
    );
    test_eq(interp, "coroutine nested", "(let (outer (Coroutine (lambda () (let (inner (Coroutine (lambda () 10))) (+ (resume inner) 5))))) (resume outer))", 15, pass, fail);
    test_eq(interp, "coroutine deep nest", "(let (f (Coroutine (lambda () (let (a (Coroutine (lambda () (let (b (Coroutine (lambda () 1))) (+ (resume b) 2))))) (+ (resume a) 3))))) (resume f))", 6, pass, fail);
    test_eq(interp, "coroutine multi yield", "(let (g (Coroutine (lambda () (yield 1) (yield 2) 3))) (+ (resume g) (+ (resume g) (resume g))))", 6, pass, fail);
    setup(interp, "(define (coroutine-gen n) (Coroutine (lambda () (let loop (i 0) (if (= i n) i (block (yield i) (loop (+ i 1))))))))");
    test_eq(interp, "coroutine generator", "(let (g (coroutine-gen 3)) (+ (resume g) (+ (resume g) (+ (resume g) (resume g)))))", 6, pass, fail);

    // Variadic lambdas (native JIT apply path; no interpreter fallback route)
    setup(interp, "(define collect (lambda (.. args) args))");
    test_eq(interp, "variadic collect length", "(length (collect 1 2 3))", 3, pass, fail);
    test_nil(interp, "variadic collect empty", "(collect)", pass, fail);
    setup(interp, "(define head-rest (lambda (h .. t) t))");
    test_eq(interp, "variadic rest length", "(length (head-rest 1 2 3))", 2, pass, fail);
    test_nil(interp, "variadic empty rest", "(head-rest 1)", pass, fail);
    setup(interp, "(define multi-rest (lambda (a b .. rest) rest))");
    test_eq(interp, "variadic multi rest len", "(length (multi-rest 1 2 3 4 5))", 3, pass, fail);
    test_eq(interp, "variadic head-only", "((lambda (h .. rest-ho) h) 42 99 100)", 42, pass, fail);
    setup(interp, "(define count-rest (lambda (h .. t) (length t)))");
    test_eq(interp, "variadic count-rest", "(count-rest 1 2 3 4 5)", 4, pass, fail);
}

fn void run_advanced_block_syntax_contract_matrix_tests(Interp* interp, int* pass, int* fail) {
    // Block syntax contract: accepted and removed forms.
    test_eq(interp, "block returns last", "(block 1 2 3)", 3, pass, fail);
    test_eq(interp, "block single", "(block 42)", 42, pass, fail);
    test_eq(interp, "block side effects", "(block (define block-x 10) (define block-y 20) (+ block-x block-y))", 30, pass, fail);
    test_eq(interp, "block nested", "(block (block 1 2) (block 3 4))", 4, pass, fail);
    test_eq(interp, "block with if", "(block (if true 10 20) (if false 30 40))", 40, pass, fail);
    test_error_contains(interp, "begin syntax removed", "(begin 1 2)", "begin syntax was removed; use block", pass, fail);
    test_error_contains(interp, "do syntax removed", "(do 1 2)", "do syntax was removed; use block", pass, fail);
    test_error_contains(interp, "fn syntax removed", "(fn (x) x)", "fn syntax was removed; use lambda", pass, fail);
    test_error_contains(interp, "letrec syntax removed", "(letrec ((f (lambda (x) x))) (f 1))", "letrec syntax was removed; use let ^rec", pass, fail);
    test_error_contains(interp, "reset syntax removed", "(reset 1)", "reset syntax was removed; use checkpoint", pass, fail);
    test_error_contains(interp, "shift syntax removed", "(shift k 1)", "shift syntax was removed; use capture", pass, fail);
}
