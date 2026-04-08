module lisp;

fn void run_basic_let_destructure_array_tests(Interp* interp, int* pass, int* fail) {
    // Array: basic
    test_eq(interp, "let [x y] array", "(let ([x y] [10 20]) (+ x y))", 30, pass, fail);
    test_eq(interp, "let [a b c] list", "(let ([a b c] '(1 2 3)) (+ a (+ b c)))", 6, pass, fail);
    test_eq(interp, "let [a b c] array", "(let ([a b c] [1 2 3]) (+ a (+ b c)))", 6, pass, fail);

    // Array: head..tail
    test_eq(interp, "let [h .. t] list", "(let ([h .. t] '(10 20 30)) h)", 10, pass, fail);
    test_eq(interp, "let [h .. t] array", "(let ([h .. t] [10 20 30]) h)", 10, pass, fail);
    test_eq(interp, "let [h .. t] tail len", "(let ([h .. t] '(10 20 30)) (length t))", 2, pass, fail);

    // Array: ignore rest
    test_eq(interp, "let [a b ..] list", "(let ([a b ..] '(1 2 3 4 5)) (+ a b))", 3, pass, fail);
    test_eq(interp, "let [a b ..] array", "(let ([a b ..] [1 2 3 4 5]) (+ a b))", 3, pass, fail);

    // Array: [.. last]
    test_eq(interp, "let [.. last] list", "(let ([.. last] '(1 2 3 4 5)) last)", 5, pass, fail);

    // Array: wildcard
    test_eq(interp, "let [_ b _] skip", "(let ([_ b _] [10 20 30]) b)", 20, pass, fail);
}

fn void run_basic_let_destructure_dict_tests(Interp* interp, int* pass, int* fail) {
    // Dictionary: basic
    test_str(interp, "let {name} dict", "(let ({name} {'name \"Alice\" 'age 30}) name)", pass, fail);
    test_eq(interp, "let {age} dict", "(let ({age} {'name \"Alice\" 'age 30}) age)", 30, pass, fail);
    test_eq(interp, "let {x y} dict", "(let ({x y} {'x 10 'y 20}) (+ x y))", 30, pass, fail);

    // Dictionary: missing key -> nil
    test_eq(interp, "let {z} missing → nil", "(let ({z} {'x 10}) (if z 99 0))", 0, pass, fail);

    // Dictionary: 3+ keys
    test_eq(interp, "let {a b c} dict", "(let ({a b c} {'a 1 'b 2 'c 3}) (+ a (+ b c)))", 6, pass, fail);
}

fn void run_basic_let_destructure_mixed_binding_tests(Interp* interp, int* pass, int* fail) {
    // Mixed: plain + array
    test_eq(interp, "let plain + [arr]", "(let (x 1 [a b] [2 3]) (+ x (+ a b)))", 6, pass, fail);
    test_eq(interp, "let [arr] + plain", "(let ([a b] [2 3] x 1) (+ x (+ a b)))", 6, pass, fail);

    // Mixed: plain + dict
    test_eq(interp, "let plain + {dict}", "(let (x 1 {y} {'y 2}) (+ x y))", 3, pass, fail);
    test_eq(interp, "let {dict} + plain", "(let ({y} {'y 2} x 1) (+ x y))", 3, pass, fail);

    // Mixed: array + dict
    test_eq(interp, "let [arr] + {dict}", "(let ([a b] [1 2] {x} {'x 10}) (+ a (+ b x)))", 13, pass, fail);
    test_eq(interp, "let {dict} + [arr]", "(let ({x} {'x 10} [a b] [1 2]) (+ a (+ b x)))", 13, pass, fail);

    // Mixed: plain + array + dict
    test_eq(interp, "let p + [a] + {d}", "(let (z 1 [a b] [2 3] {x} {'x 4}) (+ z (+ a (+ b x))))", 10, pass, fail);

    // Mixed: two arrays
    test_eq(interp, "let [a1] [a2]", "(let ([a b] [1 2] [c d] [3 4]) (+ a (+ b (+ c d))))", 10, pass, fail);

    // Mixed: two dicts
    test_eq(interp, "let {d1} {d2}", "(let ({x} {'x 3} {y} {'y 7}) (+ x y))", 10, pass, fail);
}

fn void run_basic_let_destructure_shadow_scope_tests(Interp* interp, int* pass, int* fail) {
    // Shadowing in let: second binding shadows first
    test_eq(interp, "let shadow plain", "(let (x 1 x 2) x)", 2, pass, fail);
    test_eq(interp, "let shadow [a] [a]", "(let ([a b] [1 2] [a b] [10 20]) (+ a b))", 30, pass, fail);
    test_eq(interp, "let shadow {d} {d}", "(let ({x} {'x 1} {x} {'x 99}) x)", 99, pass, fail);

    // Nested scope: destructured bindings visible in body
    test_eq(interp, "let [a] in body", "(let ([a b] [1 2]) (let (c (+ a b)) c))", 3, pass, fail);
    test_eq(interp, "let {d} in body", "(let ({x y} {'x 3 'y 4}) (let (z (+ x y)) z))", 7, pass, fail);

    // Array destr from function result
    test_eq(interp, "let [a b] from call", "(let ([a b] (list 10 20)) (+ a b))", 30, pass, fail);

    // Dictionary destr with symbol-as-function
    test_eq(interp, "let {d} sym-as-fn", "(let ({name} {'name \"Alice\"}) (string-length name))", 5, pass, fail);
}

fn void run_basic_let_destructure_default_tests(Interp* interp, int* pass, int* fail) {
    // default function for dict optional keys
    test_eq(interp, "default nil → fallback", "(default nil 42)", 42, pass, fail);
    test_eq(interp, "default val → val", "(default 10 42)", 10, pass, fail);
    test_eq(interp, "default 0 → 0", "(default 0 42)", 0, pass, fail);
    test_eq(interp, "default with {dict}", "(let ({timeout} {'host \"x\"}) (default timeout 5000))", 5000, pass, fail);
    test_eq(interp, "default provided", "(let ({timeout} {'timeout 3000}) (default timeout 5000))", 3000, pass, fail);

    // with-defaults macro
    test_eq(interp, "with-defaults body only", "(with-defaults 42)", 42, pass, fail);
    test_eq(interp, "with-defaults nil → default", "(let (x nil) (with-defaults x 5 x))", 5, pass, fail);
    test_eq(interp, "with-defaults val → val", "(let (x 10) (with-defaults x 5 x))", 10, pass, fail);
    test_eq(interp, "with-defaults multi", "(let (x nil y nil) (with-defaults x 5 y 10 (+ x y)))", 15, pass, fail);
    test_eq(interp, "with-defaults partial", "(let (x 1 y nil) (with-defaults x 5 y 10 (+ x y)))", 11, pass, fail);
    test_eq(interp, "with-defaults + {dict}", "(let ({timeout} {'host \"x\"}) (with-defaults timeout 5000 timeout))", 5000, pass, fail);
}

fn void run_basic_let_destructure_tests(Interp* interp, int* pass, int* fail) {
    // === Let destructuring: all combinations ===
    run_basic_let_destructure_array_tests(interp, pass, fail);
    run_basic_let_destructure_dict_tests(interp, pass, fail);
    run_basic_let_destructure_mixed_binding_tests(interp, pass, fail);
    run_basic_let_destructure_shadow_scope_tests(interp, pass, fail);
    run_basic_let_destructure_default_tests(interp, pass, fail);
}

fn void run_basic_function_param_destructure_define_tests(Interp* interp, int* pass, int* fail) {
    // Single dict param
    setup(interp, "(define (d-single {name}) name)");
    test_str(interp, "define {name} param", "(d-single {'name \"Alice\" 'age 30})", pass, fail);

    // Multi-key dict param
    setup(interp, "(define (d-multi {x y}) (+ x y))");
    test_eq(interp, "define {x y} param", "(d-multi {'x 10 'y 20})", 30, pass, fail);

    // Single array param
    setup(interp, "(define (d-arr [a b]) (+ a b))");
    test_eq(interp, "define [a b] param", "(d-arr [3 4])", 7, pass, fail);

    // Array param with head..tail
    setup(interp, "(define (d-ht [h .. t]) h)");
    test_eq(interp, "define [h .. t] param", "(d-ht '(10 20 30))", 10, pass, fail);

    // Dictionary + positional
    setup(interp, "(define (d-dp {a} b) (+ a b))");
    test_eq(interp, "define {dict} + positional", "(d-dp {'a 5} 10)", 15, pass, fail);

    // Positional + dict
    setup(interp, "(define (d-pd x {y}) (+ x y))");
    test_eq(interp, "define positional + {dict}", "(d-pd 3 {'y 7})", 10, pass, fail);

    // Array + positional
    setup(interp, "(define (d-ap [a b] c) (+ a (+ b c)))");
    test_eq(interp, "define [arr] + positional", "(d-ap [1 2] 3)", 6, pass, fail);

    // Positional + array
    setup(interp, "(define (d-pa x [a b]) (+ x (+ a b)))");
    test_eq(interp, "define positional + [arr]", "(d-pa 10 [3 4])", 17, pass, fail);

    // Two dict params
    setup(interp, "(define (d-dd {x} {y}) (+ x y))");
    test_eq(interp, "define {d1} {d2}", "(d-dd {'x 3} {'y 7})", 10, pass, fail);

    // Dictionary + array
    setup(interp, "(define (d-da {x} [a b]) (+ x (+ a b)))");
    test_eq(interp, "define {dict} [arr]", "(d-da {'x 10} [1 2])", 13, pass, fail);

    // Array + dict
    setup(interp, "(define (d-ad [a b] {x}) (+ a (+ b x)))");
    test_eq(interp, "define [arr] {dict}", "(d-ad [1 2] {'x 10})", 13, pass, fail);

    // Dictionary + array + positional
    setup(interp, "(define (d-dap {x} [a b] c) (+ x (+ a (+ b c))))");
    test_eq(interp, "define {d} [a] pos", "(d-dap {'x 1} [2 3] 4)", 10, pass, fail);

    // Positional + dict + array
    setup(interp, "(define (d-pda z {x} [a b]) (+ z (+ x (+ a b))))");
    test_eq(interp, "define pos {d} [a]", "(d-pda 1 {'x 2} [3 4])", 10, pass, fail);

    // Dictionary + positional + variadic
    setup(interp, "(define (d-dpv {x} y .. rest) (+ x y))");
    test_eq(interp, "define {d} pos .. rest", "(d-dpv {'x 5} 10 20 30)", 15, pass, fail);

    // Array + variadic
    setup(interp, "(define (d-av [a b] .. rest) (+ a b))");
    test_eq(interp, "define [arr] .. rest", "(d-av [1 2] 3 4 5)", 3, pass, fail);

    // Three destructuring params
    setup(interp, "(define (d-three {x} [a b] {y}) (+ x (+ a (+ b y))))");
    test_eq(interp, "define {d} [a] {d2}", "(d-three {'x 1} [2 3] {'y 4})", 10, pass, fail);
}

fn void run_basic_function_param_destructure_lambda_typed_tests(Interp* interp, int* pass, int* fail) {
    // Lambda: dict param
    test_eq(interp, "lambda {dict}", "((lambda ({x y}) (+ x y)) {'x 3 'y 4})", 7, pass, fail);

    // Lambda: array param
    test_eq(interp, "lambda [arr]", "((lambda ([a b]) (+ a b)) [5 6])", 11, pass, fail);

    // Lambda: dict + positional
    test_eq(interp, "lambda {d} pos", "((lambda ({x} y) (+ x y)) {'x 3} 7)", 10, pass, fail);

    // Lambda: array + dict
    test_eq(interp, "lambda [a] {d}", "((lambda ([a b] {x}) (+ a (+ b x))) [1 2] {'x 10})", 13, pass, fail);

    // Typed param + dict
    setup(interp, "(define (d-typed (^Integer n) {x}) (+ n x))");
    test_eq(interp, "define typed + {dict}", "(d-typed 5 {'x 10})", 15, pass, fail);
}

fn void run_basic_function_param_destructure_shadow_missing_tests(Interp* interp, int* pass, int* fail) {
    // === Shadowing: last binding wins ===
    // Same name in two dict params: second shadows first (nested lets, inner wins)
    setup(interp, "(define (d-shadow {x} {x y}) (+ x y))");
    test_eq(interp, "shadow {x} {x y} inner wins", "(d-shadow {'x 100} {'x 1 'y 2})", 3, pass, fail);

    // Same name in array params: second shadows
    setup(interp, "(define (d-sha2 [c z] [c z]) (+ c z))");
    test_eq(interp, "shadow [c z] [c z] inner wins", "(d-sha2 [100 200] [1 2])", 3, pass, fail);

    // Dictionary key missing -> nil
    test_eq(interp, "dict missing key → nil", "((lambda ({x y}) (if y 99 0)) {'x 10})", 0, pass, fail);
}

fn void run_basic_function_param_destructure_tests(Interp* interp, int* pass, int* fail) {
    // === Destructuring in function params: all combinations ===
    run_basic_function_param_destructure_define_tests(interp, pass, fail);
    run_basic_function_param_destructure_lambda_typed_tests(interp, pass, fail);
    run_basic_function_param_destructure_shadow_missing_tests(interp, pass, fail);
}
