module lisp;


import main;

fn void run_advanced_lambda_syntax_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_block_syntax_contract_matrix_tests(interp, pass, fail);

    // Zero-arg lambdas
    test_eq(interp, "zero-arg thunk", "((lambda () 42))", 42, pass, fail);
    test_eq(interp, "zero-arg closure", "(let (a 10) ((lambda () a)))", 10, pass, fail);
    test_eq(interp, "zero-arg stored thunk", "(let (thunk (lambda () 99)) (thunk))", 99, pass, fail);
    test_eq(interp, "zero-arg block body", "((lambda () (block 1 2 3)))", 3, pass, fail);
    test_eq(interp, "zero-arg computation", "((lambda () (+ 10 20)))", 30, pass, fail);

    // Multi-param lambda (strict arity)
    test_eq(interp, "multi-param two", "((lambda (x y) (+ x y)) 3 4)", 7, pass, fail);
    test_eq(interp, "multi-param three", "((lambda (x y z) (+ x (+ y z))) 1 2 3)", 6, pass, fail);
    test_eq(interp, "multi-param direct call", "(let (add (lambda (x y) (+ x y))) (add 10 20))", 30, pass, fail);
    setup(interp, "(define mp-add3 (lambda (a b c) (+ a (+ b c))))");
    test_eq(interp, "multi-param defined fn", "(mp-add3 10 20 30)", 60, pass, fail);
    test_eq(interp, "multi-param three call", "(let (f (lambda (x y z) (+ x (+ y z)))) (f 1 2 3))", 6, pass, fail);

    // Arity error tests (strict enforcement)
    test_error_contains(interp, "arity err 2-param 1-arg", "((lambda (x y) (+ x y)) 5)", "arity", pass, fail);
    test_error_contains(interp, "arity err 3-param 2-arg", "((lambda (x y z) (+ x y z)) 1 2)", "arity", pass, fail);

    // Placeholder _ desugaring
    test_eq(interp, "placeholder basic", "((+ 1 _) 5)", 6, pass, fail);
    test_eq(interp, "placeholder position", "((- _ 5) 10)", 5, pass, fail);
    test_eq(interp, "placeholder multi", "((+ _ _) 3 4)", 7, pass, fail);
    test_eq(interp, "placeholder in let", "(let (f (+ _ 10)) (f 5))", 15, pass, fail);
    test_eq(interp, "placeholder nested call", "((* 2 _) ((+ 1 _) 3))", 8, pass, fail);
    test_eq(interp, "indexed placeholder basic", "((+ _1 10) 5)", 15, pass, fail);
    test_eq(interp, "indexed placeholder reuse", "((+ _1 _1) 7)", 14, pass, fail);
    test_eq(interp, "indexed placeholder reorder", "((- _2 _1) 3 10)", 7, pass, fail);
    test_eq(interp, "indexed placeholder max-arity uses highest index", "((+ _2 1) 100 5)", 6, pass, fail);
    test_eq(interp, "indexed placeholder not globally reserved", "(let (_2 41) _2)", 41, pass, fail);
    test_eq(interp, "indexed placeholder symbol in callee position", "(let (_2 (lambda (x) (+ x 1))) (_2 4))", 5, pass, fail);
    test_error_contains(interp, "indexed placeholder mixed with plain underscore", "((+ _ _1) 3 4)", "cannot mix _ and _n placeholders", pass, fail);
    test_error_contains(interp, "indexed placeholder rejects _0", "((+ _0 1) 2)", "invalid indexed placeholder", pass, fail);
    test_error_contains(interp, "indexed placeholder rejects negative form", "((+ _-1 1) 2)", "invalid indexed placeholder", pass, fail);
    test_error_contains(interp, "indexed placeholder rejects non-numeric suffix", "((+ _1x 1) 2)", "invalid indexed placeholder", pass, fail);
    test_error_contains(interp, "primitive no auto-partial", "(+ 3)", "arity", pass, fail);
    test_error_contains(interp, "primitive higher-order no auto-partial", "(map (+ 1) '(1 2 3))", "arity", pass, fail);

    // Pipe |> operator
    test_eq(interp, "pipe basic", "(|> 5 (+ 3))", 8, pass, fail);
    test_eq(interp, "pipe chain", "(|> 5 (+ 3) (* 2))", 16, pass, fail);
    test_eq(interp, "pipe placeholder", "(|> 5 (- _ 3))", 2, pass, fail);
    test_eq(interp, "pipe mixed", "(|> 10 (+ 5) (- _ 3) (* 2))", 24, pass, fail);

    // Guard patterns (? pred)
    test_eq(interp, "guard basic", "(match 5 ((? positive?) 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "guard fail", "(match -3 ((? positive?) 1) (_ 0))", 0, pass, fail);
    test_eq(interp, "guard with bind", "(match 5 ((? positive? x) x) (_ 0))", 5, pass, fail);
    test_eq(interp, "guard lambda", "(match 5 ((? (lambda (x) (> x 3))) 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "guard placeholder", "(match 5 ((? (> _ 3)) 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "guard direct condition", "(match 5 ((? (> 10 3)) 1) (_ 0))", 1, pass, fail);
    test_eq(interp, "guard direct condition with sub-pattern bind", "(match 5 ((? (> x 3) x) x) (_ 0))", 5, pass, fail);

    // Partial application
    test_eq(interp, "partial basic", "((partial + 1) 2)", 3, pass, fail);
    test_eq(interp, "partial multi", "((partial + 10) 20)", 30, pass, fail);
}

fn void run_advanced_binding_mutation_tests(Interp* interp, int* pass, int* fail) {
    // Multi-binding let
    test_eq(interp, "multi-let two", "(let (ml-x 10 ml-y 20) (+ ml-x ml-y))", 30, pass, fail);
    test_eq(interp, "multi-let three", "(let (ml-a 1 ml-b 2 ml-c 3) (+ ml-a (+ ml-b ml-c)))", 6, pass, fail);
    test_eq(interp, "multi-let sequential", "(let (ml-p 10 ml-q (+ ml-p 5)) ml-q)", 15, pass, fail);
    test_eq(interp, "multi-let with lambda", "(let (ml-f (lambda (xx) (+ xx 1)) ml-v (ml-f 10)) ml-v)", 11, pass, fail);
    test_eq(interp, "multi-let single", "(let (ml-z 42) ml-z)", 42, pass, fail);

    // Named let
    test_eq(interp, "named-let sum 1..5", "(let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))", 15, pass, fail);
    test_eq(interp, "named-let fact 10", "(let fact (n 10 acc 1) (if (= n 0) acc (fact (- n 1) (* acc n))))", 3628800, pass, fail);
    test_eq(interp, "named-let fib 10", "(let fib (n 10 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))", 55, pass, fail);
    test_eq(interp, "named-let fib 20", "(let fib (n 20 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))", 6765, pass, fail);
    test_eq(interp, "named-let countdown", "(let count (n 100) (if (= n 0) 0 (count (- n 1))))", 0, pass, fail);
    test_eq(interp, "named-let build list car", "(car (let build (n 3 acc (quote ())) (if (= n 0) acc (build (- n 1) (cons n acc)))))", 1, pass, fail);
    test_eq(interp, "named-let 10000 iters", "(let loop (n 10000 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc 1))))", 10000, pass, fail);

    // set!
    test_tag(interp, "set! returns void", "(block (define set-void-local 1) (set! set-void-local 2))", VOID, pass, fail);
    setup(interp, "(define set-test 10)");
    setup(interp, "(set! set-test 20)");
    test_eq(interp, "set! basic", "set-test", 20, pass, fail);

    // Counter tests are stateful (calling counter1 mutates), use JIT-only to avoid double mutation
    setup(interp, "(define make-counter (lambda () (let (count 0) (lambda () (block (set! count (+ count 1)) count)))))");
    setup(interp, "(define counter1 (make-counter))");
    test_eq_jit(interp, "set! counter 1", "(counter1)", 1, pass, fail);
    test_eq_jit(interp, "set! counter 2", "(counter1)", 2, pass, fail);
    test_eq_jit(interp, "set! counter 3", "(counter1)", 3, pass, fail);

    // set! on let-local in closure (JIT detects and falls back to interpreter)
    test_eq(interp, "set! parent scope", "(let (x 1) (let (f (lambda () (set! x 42))) (block (f) x)))", 42, pass, fail);
    test_error(interp, "set! unbound", "(set! undefined-var-xyz 1)", pass, fail);
    setup(interp, "(define sr-test 100)");
    setup(interp, "(set! sr-test (+ sr-test 1))");
    test_eq(interp, "set! read back", "sr-test", 101, pass, fail);
}
