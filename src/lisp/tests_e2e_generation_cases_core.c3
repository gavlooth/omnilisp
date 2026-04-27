module lisp;

const E2ETestCase[] E2E_TESTS_CORE = {
    // === Literals ===
    { "int literal 42", "42" },
    { "int literal 0", "0" },
    { "int literal negative", "-5" },
    { "string literal", `"hello"` },
    { "true literal", "true" },
    { "nil literal", "nil" },
    { "quoted symbol", "(quote foo)" },
    { "quoted list", "(quote (a b c))" },
    { "quoted nested", "(quote (1 (2 3) 4))" },
    { "empty list", "(quote ())" },

    // === Arithmetic ===
    { "add", "(+ 1 2)" },
    { "add zero", "(+ 0 0)" },
    { "add negative", "(+ -5 3)" },
    { "add cancel", "(+ 100 -100)" },
    { "add large", "(+ 1000000 2000000)" },
    { "sub", "(- 10 3)" },
    { "sub negative result", "(- 3 10)" },
    { "mul", "(* 6 7)" },
    { "mul zero", "(* 100 0)" },
    { "div", "(/ 10 3)" },
    { "div exact", "(/ 100 10)" },
    { "modulo", "(% 10 3)" },
    { "modulo even", "(% 8 4)" },
    { "nested arith", "(+ (* 3 4) (- 10 5))" },

    // === Floats ===
    { "float literal", "3.14" },
    { "float add", "(+ 1.5 2.5)" },
    { "float mul", "(* 2.0 3.0)" },
    { "float div", "(/ 7.0 2.0)" },
    { "int to float add", "(+ 1 0.5)" },
    { "float integer value", "5.0" },

    // === Comparisons ===
    { "eq true", "(= 5 5)" },
    { "eq false", "(= 5 6)" },
    { "lt true", "(< 3 5)" },
    { "lt false", "(< 5 3)" },
    { "gt true", "(> 5 3)" },
    { "gt false", "(> 3 5)" },
    { "le true", "(<= 5 5)" },
    { "le false", "(<= 6 5)" },
    { "ge true", "(>= 5 5)" },
    { "ge false", "(>= 4 5)" },

    // === Variables & Bindings ===
    { "var x", "x" },
    { "let simple", "(let (y 7) y)" },
    { "let nested", "(let (a 1) (let (b 2) (+ a b)))" },
    { "let shadow", "(let (x 99) x)" },
    { "let multi", "(let (a 1 b 2 c 3) (+ a (+ b c)))" },
    { "module define shadows outer let collision", "(let (auditx 0) (block (module auditmod (export auditx) (block (define auditx 1))) (with auditmod auditx)))" },
    { "module set shadows outer let collision", "(let (auditx 0) (block (module auditmodset (export auditx) (block (define auditx 1) (set! auditx 2))) (with auditmodset auditx)))" },
    { "with inner let shadows module alias", "(let (auditx 0) (block (module auditmodinner (export auditx) (define auditx 1)) (with auditmodinner (let (auditx 3) auditx))))" },
    { "outer let restored after with module collision", "(let (auditx 7) (block (module auditmod2 (export auditx) (define auditx 1)) (with auditmod2 auditx) auditx))" },
    { "module type shadows outer let collision", "(let (AuditShadowType 0) (block (module audittypemod (export AuditShadowType) (define [abstract] AuditShadowType)) (with audittypemod AuditShadowType)))" },

    // === Lambda & Application ===
    { "lambda identity", "((lambda (x) x) 5)" },
    { "lambda const", "((lambda (x) 42) 99)" },
    { "lambda arith", "((lambda (x) (+ x 10)) 32)" },
    { "curried add", "(((lambda (x) (lambda (y) (+ x y))) 3) 4)" },
    { "closure adder5", "(adder5 10)" },

    // === If ===
    { "if true", "(if true 1 2)" },
    { "if false", "(if false 1 2)" },
    { "if nested", "(if (> 5 3) (if (< 2 1) 10 20) 30)" },

    // === And/Or ===
    { "and true true", "(and 1 42)" },
    { "and false short", "(and false 42)" },
    { "or first truthy", "(or 42 99)" },
    { "or fallback", "(or nil 99)" },

    // === Truthiness ===
    { "0 is truthy", "(if 0 42 99)" },
    { "empty string truthy", `(if "" 1 0)` },
    { "empty list truthy", "(if (quote ()) 1 0)" },

    // === Block ===
    { "block single", "(block 42)" },
    { "block multi", "(block 1 2 3)" },

    // === Cons / Car / Cdr ===
    { "car cons", "(car (cons 1 2))" },
    { "cdr cons", "(cdr (cons 1 2))" },
    { "car list", "(car (quote (10 20 30)))" },
    { "cdr list", "(cdr (quote (10 20 30)))" },
    { "cons build", "(cons 1 (cons 2 (cons 3 (quote ()))))" },
    { "null? empty", "(null? (quote ()))" },
    { "null? nonempty", "(null? (quote (1)))" },
    { "pair?", "(pair? (cons 1 2))" },
    { "pair? nil", "(pair? nil)" },

    // === Length ===
    { "length empty", "(length (quote ()))" },
    { "length 5", "(length (quote (1 2 3 4 5)))" },

    // === String operations ===
    { "string-length", `(string-length "hello world")` },
    { "string-append", `(string-append "hello" " world")` },
    // Three-arg substring coverage lives in the extended interpreter-delegated rows.
    { "String", `(String 42)` },
    { "parse-number", `(parse-number "42")` },
    { "Integer ctor double", "(Integer 3.9)" },
    { "Dict shorthand ctor type", "(type-of (Dict 'a 1))" },
    { "Float64 ctor int", "(Float64 3)" },
    { "Boolean ctor truthy", "(Boolean 0)" },
    { "Dict shorthand ref", "(ref (Dict 'a 1) 'a)" },
    { "Nil ctor nil", "(Nil nil)" },
    { "Void singleton type", "(type-of (Void))" },
    { "Void singleton truthy", "(if (Void) 1 0)" },
    { "String ctor int", "(String 3)" },
    { "Symbol ctor string", "(Symbol \"alpha\")" },
    { "Dictionary ctor type", "(type-of (Dictionary 'a 1))" },
    { "Set ctor type", "(type-of (Set 1 2 3))" },
    { "Closure ctor lambda type", "(type-of (Closure (lambda (x) x)))" },
    { "Coroutine ctor lambda type", "(type-of (Coroutine (lambda () 1)))" },
    { "Iterator ctor array car", "(car (list (Iterator [4 5 6])))" },

    // === Recursive functions ===
    { "sum-to 50", "(sum-to 50)" },
    { "fib 10", "(fib 10)" },
    { "fib 15", "(fib 15)" },
    { "factorial 5", "(fact 5)" },
    { "factorial 10", "(fact 10)" },

    // === Closure factories ===
    { "counter c1", "(c1 5)" },
    { "counter c2", "(c2 5)" },

    // === Higher-order functions ===
    { "map double car", "(car ((mymap double) (quote (1 2 3))))" },
    { "fold sum", "(((fold-helper +) 0) (quote (1 2 3 4 5)))" },
    { "map and fold", "(((fold-helper +) 0) ((mymap double) (quote (1 2 3 4 5))))" },

    // === Pattern matching ===
    { "match literal", "(match 42 (42 1) (x 0))" },
    { "match variable", "(match 5 (x x))" },
    { "match wildcard", "(match 99 (_ 1))" },
    { "match list", "(match (quote (1 2 3)) ([a b c] (+ a (+ b c))))" },
    { "match head", "(match (quote (10 20 30)) ([head .. tail] head))" },
    { "match rest sum", "(match (quote (1 2 3 4 5)) ([a b ..] (+ a b)))" },
    { "match last", "(match (quote (1 2 3 4 5)) ([.. last] last))" },
    { "match fallthrough", "(match 42 (0 100) (1 200) (x x))" },
    { "match underscore", "(match (quote (1 2 3)) ([_ second _] second))" },
    { "match nil literal pattern", "(match nil (nil 1) (_ 0))" },
    { "match false literal", "(match false (false 1) (_ 0))" },
    { "match nil does not match false literal", "(match nil (false 1) (_ 0))" },
    { "match sibling var declarations", "(match 2 (x x) (x x))" },
    // Legacy AOT match-literal parity row; runtime coverage stays elsewhere.
    { "match wildcard after nil false Void literals", "(match 42 (nil 1) (false 2) (Void 3) (_ 9))" },
    // Legacy AOT match-guard binding parity row; runtime coverage stays elsewhere.

    // === Nested let deep ===
    { "nested let 5", "(let (a 1) (let (b 2) (let (c 3) (let (d 4) (let (e 5) (+ a (+ b (+ c (+ d e)))))))))" },
    { "same-name let shadow", "(let (x 1) (block (let (x 2) x) x))" },
    { "same-name let closure capture", "(let (x 1) (let (f (let (x 2) (lambda () x))) (+ (f) x)))" },
    { "same-name mutable let capture", "(let (x 1) (block (let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x)) x))" },
    { "same-name set shadows outer mutable cell", "(let (x 0) (block (let (f (lambda () (set! x 1))) 0) (let (x 10) (set! x 20)) x))" },
    { "same-name let initializer mutates outer", "(let (x 0) (block (let (x (lambda () (set! x 1))) (x)) x))" },

    // === Let recursive ===
    { "let rec factorial", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 5))" },
    { "named let loop", "(let loop (n 10 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "same-name let rec shadow", "(let (loop 99) (block (let ^rec (loop (lambda (n) (if (= n 0) 0 (loop (- n 1))))) (loop 3)) loop))" },

    // === Effects ===
    { "checkpoint passthrough", "(checkpoint (+ 10 20))" },
    { "capture aborts", "(checkpoint (+ 1 (capture k 42)))" },
    { "capture k resumes", "(checkpoint (+ 1 (capture k (k 10))))" },
    { "mutable capture through checkpoint", "(let (x 0) (block (checkpoint (let (f (lambda () (set! x 1))) (f))) x))" },
    { "mutable capture through capture", "(let (x 0) (block (checkpoint (capture k (block (let (f (lambda () (set! x 1))) (f)) (k 0)))) x))" },
    { "checkpoint local mutable capture", "(checkpoint (let (x 0) (block (let (f (lambda () (set! x 1))) (f)) x)))" },
    { "checkpoint boxed outer restore", "(let (x 0) (block (let (g (lambda () x)) (block (set! x 2) (checkpoint (let (f (lambda () (set! x 3))) (f))) x))))" },
    { "capture continuation sees checkpoint snapshot", "(checkpoint (let (x 0) (+ (capture k (block (let (f (lambda () (set! x 10))) (f)) (k 1))) x)))" },
    { "capture continuation sees outer checkpoint snapshot", "(let (x 0) (checkpoint (+ (capture k (block (let (f (lambda () (set! x 10))) (f)) (k 1))) x)))" },
    // Legacy AOT handle parity rows; runtime/effect coverage stays in bounded slices.

    // === Not ===
    { "not true", "(not true)" },
    { "not nil", "(not nil)" },
    { "not 0", "(not 0)" },

    // === Apply ===
    { "apply add", "(apply + (quote (1 2)))" },
    { "apply list", "(apply cons (quote (1 2)))" },

    // === Arithmetic edge cases ===
    { "sub zero", "(- 0 0)" },
    { "sub neg neg", "(- -5 -3)" },
    { "mul neg pos", "(* -3 4)" },
    { "mul neg neg", "(* -3 -4)" },
    { "div exact 2", "(/ 10 2)" },
    { "div zero num", "(/ 0 5)" },
    { "div neg", "(/ -10 3)" },
    { "mod zero", "(% 9 3)" },
    { "mod small", "(% 7 10)" },
    { "nested add", "(+ 1 (+ 2 3))" },
    { "nested mul add", "(* 2 (+ 3 4))" },
    { "nested sub mul", "(- (* 5 5) (+ 10 5))" },
    { "nested div add", "(/ (+ 10 20) (- 10 4))" },

    // === Comparison edge cases ===
    { "eq zero", "(= 0 0)" },
    { "eq neg", "(= -1 -1)" },
    { "lt equal", "(< 1 1)" },
    { "lt neg zero", "(< -5 0)" },
    { "gt equal", "(> 1 1)" },
    { "le strict", "(<= 1 2)" },
    { "ge strict", "(>= 2 1)" },
    { "if lt branch", "(if (< 1 2) 10 20)" },
    { "if gt branch", "(if (> 1 2) 10 20)" },
    { "if eq computed", "(if (= (+ 2 3) 5) 1 0)" },

    // === And/Or edge cases ===
    { "and true true2", "(and true true)" },
    { "and false true", "(and false true)" },
    { "and true false", "(and true false)" },
    { "or true false", "(or true false)" },
    { "or false nil", "(or false nil)" },
    // Side-effecting short-circuit coverage lives in runtime slices.

    // === Type predicates ===
    { "string? yes", `(string? "hello")` },
    { "string? no", "(string? 42)" },
    { "int? yes", "(int? 42)" },
    { "int? no", `(int? "hello")` },
    { "symbol? yes", "(symbol? (quote foo))" },
    { "symbol? no", "(symbol? 42)" },
    { "closure? yes", "(closure? (lambda (x) x))" },
    { "closure? no", "(closure? 42)" },
    { "closure? factory product", "(let (mk (lambda () (lambda (x) x)) f (mk)) (closure? f))" },
    { "type-of factory product closure", "(let (mk (lambda () (lambda (x) x)) f (mk)) (type-of f))" },
    { "type-of variadic lambda closure", "(type-of (lambda (h .. rest) h))" },
    { "null? nil", "(null? nil)" },
    { "null? 42", "(null? 42)" },
    { "null? list", "(null? (quote (1 2)))" },
    { "pair? list", "(pair? (quote (1 2 3)))" },
    { "pair? 42", "(pair? 42)" },
    { "not false", "(not false)" },
    { "not 42", "(not 42)" },
    { "not empty-str", `(not "")` },

    // === Zero-arg lambdas ===
    { "thunk 42", "((lambda () 42))" },
    { "thunk capture", "(let (a 10) ((lambda () a)))" },
    { "thunk block", "((lambda () (block 1 2 3)))" },
    { "thunk arith", "((lambda () (+ 10 20)))" },

    // === Multi-param lambdas (strict arity) ===
    { "multi-param add", "((lambda (x y) (+ x y)) 3 4)" },
    { "multi-param 3", "((lambda (x y z) (+ x (+ y z))) 1 2 3)" },
    { "multi-param direct", "(let (add (lambda (x y) (+ x y))) (add 10 20))" },

    // === Named let patterns ===
    { "named let sum 5", "(let loop (n 5 acc 0) (if (= n 0) acc (loop (- n 1) (+ acc n))))" },
    { "named let fact 10", "(let f (n 10 acc 1) (if (= n 0) acc (f (- n 1) (* acc n))))" },
    { "named let fib 10", "(let fib (n 10 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let fib 20", "(let fib (n 20 a 0 b 1) (if (= n 0) a (fib (- n 1) b (+ a b))))" },
    { "named let count", "(let count (n 100) (if (= n 0) 0 (count (- n 1))))" },

    // === More match patterns ===
    { "match multi 1", "(match 1 (1 10) (2 20) (3 30))" },
    { "match multi 2", "(match 2 (1 10) (2 20) (3 30))" },
    { "match multi 3", "(match 3 (1 10) (2 20) (3 30))" },
    { "match default", "(match 99 (1 10) (2 20) (x (+ x 1)))" },
    { "match computed", "(match (+ 1 1) (1 10) (2 20) (3 30))" },
    { "match pair sum", "(match (quote (10 20)) ([a b] (+ a b)))" },
    { "match single", "(match (quote (42)) ([x] x))" },
    { "match product", "(match (quote (2 3 4)) ([a b c] (* a (* b c))))" },
    { "match rest len", "(match (quote (1 2 3 4 5)) ([h .. t] (length t)))" },
    { "match string", `(match "hello" ("hello" 1) ("world" 2) (x 0))` },

    // === More effects/continuations ===
    { "checkpoint large", "(checkpoint (+ 100 (capture k 42)))" },
    { "capture resume mul", "(checkpoint (* 2 (capture k (k 5))))" },
    { "capture resume add", "(checkpoint (+ 1 (capture k (k (+ 2 3)))))" },
    { "multi-shot k", "(checkpoint (+ 1 (capture k (+ (k 10) 1000))))" },
    // Legacy AOT handle parity rows; runtime/effect coverage stays in bounded slices.

    // === More list operations ===
    { "cadr", "(car (cdr (cons 1 (cons 2 (cons 3 nil)))))" },
    { "caddr", "(car (cdr (cdr (cons 1 (cons 2 (cons 3 nil))))))" },
    { "length 1", "(length (quote (1)))" },
    { "length 10", "(length (quote (1 2 3 4 5 6 7 8 9 10)))" },
    { "length cons 2", "(length (cons 1 (cons 2 nil)))" },
    { "cadr quoted", "(car (cdr (quote (10 20 30))))" },
    { "cons dotted", "(cons 1 2)" },

    // === Let shadowing ===
    { "let shadow inner", "(let (x 10) (let (x 20) x))" },
    { "let no leak", "(let (x 10) (let (y 20) x))" },

    // === Mutual recursion ===
    { "is-even 10", "(is-even 10)" },
    { "is-odd 7", "(is-odd 7)" },

    // === Composition/apply-twice ===
    { "apply-twice add3", "((apply-twice add3) 10)" },
    { "identity int", "(id 42)" },
    { "identity nil", "(id nil)" },
    { "counter c3", "(c3 5)" },

    // === String edge cases ===
    { "string-length empty", `(string-length "")` },
    { "string-length 1", `(string-length "a")` },
    { "string-append empty", `(string-append "" "abc")` },
    { "parse-number neg", `(parse-number "-7")` },
    { "String 0", `(String 0)` },

    // === Float edge cases ===
    { "float neg", "-2.5" },
    { "int+float", "(+ 1 2.0)" },
    { "float*int", "(* 2.5 4)" },
    { "float sub", "(- 5.5 2.5)" },
    { "float lt", "(< 1 2.5)" },
    { "float gt", "(> 3.5 2)" },
    { "float eq", "(= 1 1.0)" },

    // === Block edge cases ===
    { "block nested", "(block (block 1 2) (block 3 4))" },
    { "block boolean shadowing if", "(block (if true 10 20) (let (false true) (if false 30 40)))" },
    { "if boolean shadowing branches", "(if (= 1 2) (let (false 1) false) (let (false 2) false))" },
    { "closure boolean shadow capture", "(((lambda (false) (lambda () false)) 9))" },
    { "closure callable shadow capture", "(((lambda (+) (lambda (x) (+ x 2))) (lambda (a b) 88)) 1)" },

    // === Recursive let 6 ===
    { "let rec fact 6", "(let ^rec (f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) (f 6))" },
};
