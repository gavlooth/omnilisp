module lisp;

import main;

fn void run_advanced_unicode_lambda_utf8_tests(Interp* interp, int* pass, int* fail) {
    // Phase 2A: λ (lambda) syntax
    test_eq(interp, "\xce\xbb application", "((\xce\xbb (x) (* x x)) 5)", 25, pass, fail);
    test_eq(interp, "\xce\xbb multi-param", "((\xce\xbb (x y) (+ x y)) 3 7)", 10, pass, fail);
    test_eq(interp, "\xce\xbb closure", "(let (a 10) ((\xce\xbb (x) (+ x a)) 5))", 15, pass, fail);
    test_eq(interp, "\xce\xbb recursive define", "(block (define sq (\xce\xbb (x) (* x x))) (sq 6))", 36, pass, fail);

    // Phase 2B: UTF-8 string support
    test_eq(interp, "utf8 strlen ASCII", "(string-length \"hello\")", 5, pass, fail);
    test_eq(interp, "utf8 strlen h\xc3\xa9llo", "(string-length \"h\xc3\xa9llo\")", 5, pass, fail);
    test_eq(interp, "utf8 strlen CJK", "(string-length \"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\")", 3, pass, fail);
    test_eq(interp, "utf8 strlen emoji", "(string-length \"\xf0\x9f\x98\x80\")", 1, pass, fail);
    test_eq(interp, "utf8 byte-length ASCII", "(string-byte-length \"hello\")", 5, pass, fail);
    test_eq(interp, "utf8 byte-length h\xc3\xa9llo", "(string-byte-length \"h\xc3\xa9llo\")", 6, pass, fail);
    test_eq(interp, "utf8 byte-length CJK", "(string-byte-length \"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\")", 9, pass, fail);
    test_str_val(interp, "utf8 char-at ASCII", "(char-at \"hello\" 1)", "e", pass, fail);
    test_str_val(interp, "utf8 char-at caf\xc3\xa9", "(char-at \"caf\xc3\xa9\" 3)", "\xc3\xa9", pass, fail);
    test_str_val(interp, "utf8 char-at CJK", "(char-at \"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\" 1)", "\xe6\x9c\xac", pass, fail);
    test_str_val(interp, "utf8 char-at neg", "(char-at \"caf\xc3\xa9\" -1)", "\xc3\xa9", pass, fail);
    test_str_val(interp, "utf8 substring", "(substring \"caf\xc3\xa9\" 0 3)", "caf", pass, fail);
    test_str_val(interp, "utf8 substring mid", "(substring \"h\xc3\xa9llo\" 1 3)", "\xc3\xa9l", pass, fail);
    test_eq(interp, "utf8 string->list len", "(length (string->list \"h\xc3\xa9llo\"))", 5, pass, fail);
}

fn void run_advanced_callable_type_symbol_constructor_tests(Interp* interp, int* pass, int* fail) {
    test_eq(interp, "Integer ctor from double", "(Integer 3.9)", 3, pass, fail);
    test_eq(interp, "Integer ctor from string", "(Integer \"42\")", 42, pass, fail);
    test_eq(interp, "Integer shorthand ctor from double", "(Integer 3.9)", 3, pass, fail);
    test_double(interp, "Double ctor from int", "(Double 3)", 3.0, pass, fail);
    test_double(interp, "Double ctor from string", "(Double \"3.5\")", 3.5, pass, fail);
    test_str_val(interp, "String ctor from int", "(String 3)", "3", pass, fail);
    test_str_val(interp, "String ctor from symbol", "(String 'alpha)", "alpha", pass, fail);
    test_truthy(interp, "Symbol ctor from string", "(= (Symbol \"alpha\") 'alpha)", pass, fail);
    test_truthy(interp, "Symbol ctor identity", "(= (Symbol 'alpha) 'alpha)", pass, fail);
    test_truthy(interp, "Boolean ctor from truthy int", "(Boolean 0)", pass, fail);
    test_nil(interp, "Boolean ctor from nil", "(Boolean nil)", pass, fail);
    test_nil(interp, "Boolean ctor from false", "(Boolean false)", pass, fail);
    test_nil(interp, "Boolean ctor from quoted false symbol", "(Boolean 'false)", pass, fail);
    test_truthy(interp, "Boolean shorthand ctor from truthy int", "(Boolean 0)", pass, fail);
    test_truthy(interp, "Boolean ctor type", "(= (type-of (Boolean 0)) 'Boolean)", pass, fail);
    test_truthy(interp, "false collapses to nil", "(= false nil)", pass, fail);
    test_truthy(interp, "type-of false is Nil", "(= (type-of false) 'Nil)", pass, fail);
    test_nil(interp, "Nil ctor from nil", "(Nil nil)", pass, fail);
    test_nil(interp, "Nil ctor from false", "(Nil false)", pass, fail);
    test_truthy(interp, "Void singleton type", "(= (type-of (Void)) 'Void)", pass, fail);
    test_truthy(interp, "Void singleton equality", "(= (Void) (Void))", pass, fail);
    test_truthy(interp, "Void singleton truthy", "(if (Void) true nil)", pass, fail);
    test_str_val(interp, "Void print surface", "(format \"%s\" (Void))", "#<void>", pass, fail);
    test_truthy(interp, "Closure ctor identity", "(closure? (Closure (lambda (x) x)))", pass, fail);
    test_truthy(interp, "List ctor type", "(= (type-of (List 1 2 3)) 'List)", pass, fail);
    test_eq(interp, "List ctor from array len", "(length (List [1 2 3]))", 3, pass, fail);
    test_eq(interp, "List ctor from iterator len", "(length (List (Iterator [1 2 3])))", 3, pass, fail);
    test_truthy(interp, "Array ctor type", "(= (type-of (Array 1 2 3)) 'Array)", pass, fail);
    test_eq(interp, "Array ctor from list ref", "(ref (Array '(7 8 9)) 1)", 8, pass, fail);
    test_eq(interp, "Array ctor from iterator ref", "(ref (Array (Iterator [7 8 9])) 1)", 8, pass, fail);
    test_truthy(interp, "Dictionary ctor type", "(= (type-of (Dictionary 'a 1)) 'Dictionary)", pass, fail);
    test_eq(interp, "Dictionary shorthand ctor", "(ref (Dictionary 'a 1 'b 2) 'b)", 2, pass, fail);
    test_truthy(interp, "Set ctor type", "(= (type-of (Set 1 2 3)) 'Set)", pass, fail);
    test_eq(interp, "Set ctor dedup", "(set-size (Set 1 2 2 3))", 3, pass, fail);
    test_truthy(interp, "Iterator ctor from list type", "(= (type-of (Iterator '(1 2 3))) 'Iterator)", pass, fail);
    test_truthy(interp, "Coroutine ctor type",
        "(= (type-of (Coroutine (lambda () 1))) 'Coroutine)", pass, fail);
    test_tag(interp, "TimePoint ctor type", "(TimePoint 'date 2026 3 7)", TIME_POINT, pass, fail);
    setup(interp, "(define (ctor-only-array (^Array a)) 11)");
    setup(interp, "(define (ctor-only-array x) 22)");
    test_eq(interp, "ctor boundary array no implicit list->Array", "(ctor-only-array '(1 2 3))", 22, pass, fail);
    test_eq(interp, "ctor boundary array explicit Array conversion", "(ctor-only-array (Array '(1 2 3)))", 11, pass, fail);
    setup(interp, "(define (ctor-only-iter (^Iterator it)) 33)");
    setup(interp, "(define (ctor-only-iter x) 44)");
    test_eq(interp, "ctor boundary iterator no implicit array->Iterator", "(ctor-only-iter [1 2 3])", 44, pass, fail);
    test_eq(interp, "ctor boundary iterator explicit Iterator conversion", "(ctor-only-iter (Iterator [1 2 3]))", 33, pass, fail);
    test_str_val(interp,
        "Integer ctor invalid string payload code",
        "(handle (Integer \"abc\") (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Double ctor invalid string payload code",
        "(handle (Double \"abc\") (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "String ctor invalid payload code",
        "(handle (String nil) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Symbol ctor invalid payload code",
        "(handle (Symbol 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Dictionary ctor odd-arity payload code",
        "(handle (Dictionary 'a) (raise msg (symbol->string (ref msg 'code))))",
        "type/arity", pass, fail);
    test_str_val(interp,
        "Dictionary shorthand odd-arity payload code",
        "(handle (Dictionary 'a) (raise msg (symbol->string (ref msg 'code))))",
        "type/arity", pass, fail);
    test_str_val(interp,
        "Nil ctor invalid payload code",
        "(handle (Nil 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Void ctor invalid payload code",
        "(handle (Void 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Closure ctor invalid payload code",
        "(handle (Closure 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "Iterator ctor invalid payload code",
        "(handle (Iterator 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_str_val(interp,
        "__make-iterator internal helper invalid payload code",
        "(handle (__make-iterator 42) (raise msg (symbol->string (ref msg 'code))))",
        "type/arg-mismatch", pass, fail);
    test_truthy(interp,
        "Coroutine ctor invalid payload code",
        "(= (handle (Coroutine 42) (raise msg (symbol->string (ref msg 'code)))) "
        "   \"type/arg-mismatch\")",
        pass, fail);
}

fn void run_advanced_void_nil_contract_tests(Interp* interp, int* pass, int* fail) {
    // Void = command/effect completion.
    test_truthy(interp, "Void contract set! completion",
        "(= (type-of (block (define void-nil-x 1) (set! void-nil-x 2))) 'Void)", pass, fail);
    test_truthy(interp, "Void contract push! completion",
        "(= (type-of (let (a [1]) (push! a 2))) 'Void)", pass, fail);
    test_truthy(interp, "Void contract remove! completion",
        "(= (type-of (let (d {'a 1}) (remove! d 'a))) 'Void)", pass, fail);
    test_truthy(interp, "Void contract is operational truthy token",
        "(= (if (block (define void-operational-x 1) (set! void-operational-x 2)) 1 0) 1)", pass, fail);

    // Nil = absence/query miss.
    test_truthy(interp, "Nil contract ref missing",
        "(= (type-of (ref {'a 1} 'missing)) 'Nil)", pass, fail);
    test_truthy(interp, "Nil contract has? miss",
        "(= (type-of (has? {'a 1} 'missing)) 'Nil)", pass, fail);
    test_truthy(interp, "Nil contract set-contains? miss",
        "(= (type-of (set-contains? (Set 1 2) 99)) 'Nil)", pass, fail);
}

fn void run_advanced_unicode_iterator_primitives_tests(Interp* interp, int* pass, int* fail) {
    // Phase 3A: Iterator type + primitives
    test_truthy(interp, "__make-iterator internal helper creates iter",
        "(iterator? (Iterator (lambda () nil)))", pass, fail);
    test_truthy(interp, "Iterator ctor creates iter",
        "(iterator? (Iterator (lambda () nil)))", pass, fail);
    test_truthy(interp, "next on empty iter is nil",
        "(null? (next (Iterator (lambda () nil))))", pass, fail);
    // Simple iterator: returns (cons 42 empty-iter), then empty
    setup(interp, "(define empty-it (Iterator (lambda () nil)))");
    setup(interp, "(define one-it (Iterator (lambda () (cons 42 empty-it))))");
    test_eq(interp, "next returns pair car", "(car (next one-it))", 42, pass, fail);
    // Collect a hand-made 3-element iterator
    setup(interp, "(define it3 (Iterator (lambda () (cons 3 (Iterator (lambda () (cons 2 (Iterator (lambda () (cons 1 (Iterator (lambda () nil))))))))))))" );
    test_eq(interp, "list it3 car", "(car (list it3))", 3, pass, fail);
    test_eq(interp, "list it3 len", "(length (list it3))", 3, pass, fail);
    // array constructor from iterator
    test_eq(interp, "array it3 len", "(length (Array it3))", 3, pass, fail);
    test_eq(interp, "array it3 ref 0", "(ref (Array it3) 0)", 3, pass, fail);
    // type-of iterator
    test_truthy(interp, "type-of iterator",
        "(is? (Iterator (lambda () nil)) (quote Iterator))", pass, fail);
}

fn void run_advanced_unicode_iterator_lazy_pipeline_tests(Interp* interp, int* pass, int* fail) {
    // Phase 3B: iterator constructors + lazy combinators
    test_truthy(interp, "iterator dispatches list",
        "(iterator? (Iterator (quote (1 2 3))))", pass, fail);
    test_truthy(interp, "Iterator dispatches list",
        "(iterator? (Iterator (quote (1 2 3))))", pass, fail);
    test_eq(interp, "iterator list list car", "(car (list (Iterator (quote (1 2 3)))))", 1, pass, fail);
    test_eq(interp, "iterator list list len", "(length (list (Iterator (quote (1 2 3)))))", 3, pass, fail);
    test_eq(interp, "Iterator list list car", "(car (list (Iterator (quote (1 2 3)))))", 1, pass, fail);
    test_eq(interp, "iterator array list car", "(car (list (Iterator [10 20 30])))", 10, pass, fail);
    test_eq(interp, "iterator array list len", "(length (list (Iterator [10 20 30])))", 3, pass, fail);
    test_eq(interp, "iterator dict list len", "(length (list (Iterator (block (define iter-d {'a 1 'b 2}) iter-d))))", 2, pass, fail);
    test_truthy(interp, "iterator idempotent", "(iterator? (Iterator (Iterator [1 2 3])))", pass, fail);
    // Lazy map on iterator
    test_eq(interp, "map + list car", "(car (list (map (lambda (x) (* x 2)) (Iterator [1 2 3]))))", 2, pass, fail);
    test_eq(interp, "map + list len", "(length (list (map (lambda (x) (* x 2)) (Iterator [1 2 3]))))", 3, pass, fail);
    // Lazy filter on iterator
    test_truthy(interp, "filter iterator returns iterator",
        "(iterator? (filter even? (Iterator [1 2 3 4])))", pass, fail);
    test_eq(interp, "filter even car", "(car (list (filter even? (Iterator [1 2 3 4 5]))))", 2, pass, fail);
    test_eq(interp, "filter even len", "(length (list (filter even? (Iterator [1 2 3 4 5]))))", 2, pass, fail);
    // Lazy take on iterator
    test_eq(interp, "take 2 len", "(length (list (take 2 (Iterator [10 20 30]))))", 2, pass, fail);
    test_eq(interp, "take 2 car", "(car (list (take 2 (Iterator [10 20 30]))))", 10, pass, fail);
    // Infinite + take
    test_eq(interp, "range-from + take", "(car (list (take 5 (range-from 0))))", 0, pass, fail);
    test_eq(interp, "range-from + take len", "(length (list (take 5 (range-from 0))))", 5, pass, fail);
    // map + filter pipeline
    test_eq(interp, "lazy pipeline len",
        "(length (list (take 3 (filter even? (map (lambda (x) (* x 2)) (range-from 1))))))", 3, pass, fail);
    // array constructor from iterator
    test_eq(interp, "array from iter", "(ref (Array (Iterator [5 6 7])) 1)", 6, pass, fail);
    // foldl on iterator
    test_eq(interp, "foldl iterator sum", "(foldl (lambda (a x) (+ a x)) 0 (Iterator [1 2 3 4 5]))", 15, pass, fail);
    // unified infinite sources
    test_eq(interp, "repeat + take len", "(length (list (take 4 (repeat 7))))", 4, pass, fail);
    test_eq(interp, "cycle + take first", "(car (list (take 4 (cycle [9 8]))))", 9, pass, fail);
    // array-dispatched sequence ops
    test_eq(interp, "take array len", "(length (take 2 [10 20 30]))", 2, pass, fail);
    test_eq(interp, "take array ref 1", "(ref (take 2 [10 20 30]) 1)", 20, pass, fail);
    test_eq(interp, "drop array len", "(length (drop 1 [10 20 30]))", 2, pass, fail);
    test_eq(interp, "drop array ref 0", "(ref (drop 1 [10 20 30]) 0)", 20, pass, fail);
    test_eq(interp, "zip array len", "(length (zip [1 2 3] [4 5]))", 2, pass, fail);
    test_eq(interp, "zip array first-left", "(car (ref (zip [1 2] [4 5]) 0))", 1, pass, fail);
    test_eq(interp, "foldl array sum", "(foldl (lambda (a x) (+ a x)) 0 [1 2 3 4])", 10, pass, fail);
    // foldr should exist for finite collections
    test_eq(interp, "foldr list minus", "(foldr - 0 (quote (1 2 3)))", 2, pass, fail);
    test_eq(interp, "foldr array minus", "(foldr - 0 [1 2 3])", 2, pass, fail);
}

fn void run_advanced_unicode_iterator_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_unicode_lambda_utf8_tests(interp, pass, fail);
    run_advanced_callable_type_symbol_constructor_tests(interp, pass, fail);
    run_advanced_void_nil_contract_tests(interp, pass, fail);
    run_advanced_unicode_iterator_primitives_tests(interp, pass, fail);
    run_advanced_unicode_iterator_lazy_pipeline_tests(interp, pass, fail);
}

fn void run_advanced_logic_tco_tests(Interp* interp, int* pass, int* fail) {
    // And/or advanced
    test_truthy(interp, "variadic and empty identity", "(and)", pass, fail);
    test_nil(interp, "variadic or empty identity", "(or)", pass, fail);
    test_eq(interp, "variadic and single arg", "(and 7)", 7, pass, fail);
    test_eq(interp, "variadic or single arg", "(or 7)", 7, pass, fail);
    test_eq(interp, "variadic and multiple args", "(and 1 2 3 4)", 4, pass, fail);
    test_eq(interp, "variadic or multiple args", "(or nil false 0 5)", 0, pass, fail);
    test_nil(interp, "variadic and short-circuits falsy", "(and 1 nil 3 4)", pass, fail);
    setup(interp, "(define [macro] all3 (syntax-match ([a b c] (template (and (insert a) (insert b) (insert c))))))");
    test_eq(interp, "variadic and macro emission", "(all3 1 2 3)", 3, pass, fail);
    test_eq(interp, "nested and/or", "(and (or nil 5) (or false 10))", 10, pass, fail);
    test_eq(interp, "nested or/and", "(or (and nil 5) (and 1 10))", 10, pass, fail);
    test_eq(interp, "and closure truthy", "(and (lambda (x) x) 42)", 42, pass, fail);

    // Define/redefinition
    setup(interp, "(define redef-test 10)");
    test_eq(interp, "redef before", "redef-test", 10, pass, fail);
    setup(interp, "(define redef-test 20)");
    test_eq(interp, "redef after", "redef-test", 20, pass, fail);
    setup(interp, "(define computed-val (+ (* 3 4) (- 10 5)))");
    test_eq(interp, "define complex expr", "computed-val", 17, pass, fail);

    // Path access
    setup(interp, "(define ptest {'x 10 'y 20})");
    test_eq(interp, "ptest.x => 10", "ptest.x", 10, pass, fail);
    test_eq(interp, "ptest.y => 20", "ptest.y", 20, pass, fail);

    // TCO (JIT-only mode: no interpreter TCO, bounded by real stack headroom)
    setup(interp, "(define tco-loop (lambda (n) (if (= n 0) (quote done) (tco-loop (- n 1)))))");
    test_truthy(interp, "TCO loop 4000", "(tco-loop 4000)", pass, fail);
    setup(interp, "(define sum-acc (lambda (n) (lambda (acc) (if (= n 0) acc ((sum-acc (- n 1)) (+ acc n))))))");
    test_eq(interp, "TCO sum-acc 1000 => 500500", "((sum-acc 1000) 0)", 500500, pass, fail);
    setup(interp, "(define tco-even (lambda (n) (if (= n 0) true (tco-odd (- n 1)))))");
    setup(interp, "(define tco-odd (lambda (n) (if (= n 0) nil (tco-even (- n 1)))))");
    test_truthy(interp, "TCO mutual recursion 1000", "(tco-even 1000)", pass, fail);
    setup(interp, "(define count-down (lambda (n) (match n (0 (quote done)) (n (count-down (- n 1))))))");
    test_truthy(interp, "TCO match tail call 4000", "(count-down 4000)", pass, fail);
    setup(interp, "(define let-loop (lambda (n) (let (m (- n 1)) (if (= m 0) (quote done) (let-loop m)))))");
    test_truthy(interp, "TCO let body 4000", "(let-loop 4000)", pass, fail);
}

fn void run_advanced_effect_continuation_tests(Interp* interp, int* pass, int* fail) {
    // Multi-perform
    test_eq(interp, "multi-perform 2 sum", "(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x (resolve x)))", 30, pass, fail);
    test_eq(interp, "multi-perform 3 sum", "(handle (+ (signal bounce 1) (+ (signal bounce 2) (signal bounce 3))) (bounce x (resolve x)))", 6, pass, fail);
    test_eq(interp, "multi-perform doubles", "(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x (resolve (* x 2))))", 60, pass, fail);
    test_eq(interp, "multi-perform abort", "(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x x))", 10, pass, fail);

    // signal/resolve syntax (new match-style clauses)
    test_eq(interp, "signal/resolve basic", "(handle (+ 1 (signal read nil)) (read x (resolve 41)))", 42, pass, fail);
    test_eq(interp, "signal abort", "(handle (+ 1 (signal bail 5)) (bail x x))", 5, pass, fail);
    test_eq(interp, "signal abort skips post-signal body effects",
        "(let (x 0) (block (handle (block (signal bail 1) (set! x 99) 7) (bail v 5)) x))", 0, pass, fail);
    test_eq(interp, "signal/resolve double", "(handle (signal double 7) (double x (resolve (* x 2))))", 14, pass, fail);
    test_eq(interp, "resolve is terminal in clause",
        "(let (x 0) (block (handle (signal once 10) (once v (block (resolve (+ v 1)) (set! x 99) 0))) x))", 0, pass, fail);
    test_eq(interp, "signal no trigger", "(handle (+ 10 20) (unused x 999))", 30, pass, fail);
    test_eq(interp, "signal multi-resume", "(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x (resolve x)))", 30, pass, fail);
    test_error_contains(interp, "resolve outside handler", "(resolve 42)", "resolve", pass, fail);
    test_tag(interp, "resolve outside handler payload tag", "(handle (resolve 42) (raise msg msg))", HASHMAP, pass, fail);
    test_str_val(interp,
        "resolve outside handler payload domain",
        "(handle (resolve 42) (raise msg (symbol->string (ref msg 'domain))))",
        "runtime", pass, fail);
    test_str_val(interp,
        "resolve outside handler payload code",
        "(handle (resolve 42) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/invalid-continuation", pass, fail);
    test_str_val(interp,
        "capture outside checkpoint payload code",
        "(handle (capture k 1) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/capture-outside-checkpoint", pass, fail);
    test_str_val(interp,
        "effect arg mismatch payload code",
        "(handle (signal io/read-file 42) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/effect-arg-mismatch", pass, fail);
    test_str_val(interp,
        "unhandled effect payload code",
        "(handle (signal no-handler 1) (raise msg (symbol->string (ref msg 'code))))",
        "runtime/unhandled-effect", pass, fail);
    test_str_val(interp,
        "scheduler offload payload domain",
        "(handle (offload 'nope 1) (raise msg (symbol->string (ref msg 'domain))))",
        "scheduler", pass, fail);
    test_str_val(interp,
        "scheduler offload payload code",
        "(handle (offload 'nope 1) (raise msg (symbol->string (ref msg 'code))))",
        "scheduler/offload-unsupported-op", pass, fail);
    test_str_val(interp,
        "scheduler offload cpu-only payload code",
        "(handle (offload 'http-get \"https://example.com\") (raise msg (symbol->string (ref msg 'code))))",
        "scheduler/offload-op-cpu-only", pass, fail);
    test_str_val(interp,
        "scheduler join-timeout payload code",
        "(handle (block (define t-sched-timeout (thread-spawn 'sleep-ms 20)) (thread-join-timeout t-sched-timeout 0)) (raise msg (symbol->string (ref msg 'code))))",
        "scheduler/thread-join-timeout", pass, fail);
    test_str_val(interp,
        "io tcp-connect payload domain",
        "(handle (tcp-connect 42 80) (raise msg (symbol->string (ref msg 'domain))))",
        "io", pass, fail);
    test_str_val(interp,
        "io tcp-connect payload code",
        "(handle (tcp-connect 42 80) (raise msg (symbol->string (ref msg 'code))))",
        "io/tcp-connect-host-not-string", pass, fail);
    test_str_val(interp,
        "io tcp-listen payload code",
        "(handle (tcp-listen \"bad-host\" 0) (raise msg (symbol->string (ref msg 'code))))",
        "io/tcp-listen-fiber-required", pass, fail);
    test_str_val(interp,
        "io dns-resolve payload code",
        "(await (spawn (lambda () (handle (dns-resolve \"bad host\") (raise msg (symbol->string (ref msg 'code)))))))",
        "io/dns-resolve-resolution-failed", pass, fail);
    test_str_val(interp,
        "deduce dispatch payload domain",
        "(handle (deduce 123) (raise msg (symbol->string (ref msg 'domain))))",
        "deduce", pass, fail);
    test_str_val(interp,
        "deduce dispatch payload code",
        "(handle (deduce 123) (raise msg (symbol->string (ref msg 'code))))",
        "deduce/dispatch-command-not-symbol", pass, fail);
    test_str_val(interp,
        "deduce open payload code",
        "(handle (deduce 'open 42) (raise msg (symbol->string (ref msg 'code))))",
        "deduce/open-path-not-string-or-memory", pass, fail);
    test_str_val(interp,
        "deduce query payload code",
        "(handle (deduce 'query) (raise msg (symbol->string (ref msg 'code))))",
        "deduce/query-expected-relation-filter", pass, fail);
    test_str_val(interp,
        "regex re-match payload domain",
        "(handle (re-match 42 \"x\") (raise msg (symbol->string (ref msg 'domain))))",
        "regex", pass, fail);
    test_str_val(interp,
        "regex re-match payload code",
        "(handle (re-match 42 \"x\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-match-pattern-not-string", pass, fail);
    test_str_val(interp,
        "parser pika-parse payload domain",
        "(handle (pika/parse 42 \"x\") (raise msg (symbol->string (ref msg 'domain))))",
        "parser", pass, fail);
    test_str_val(interp,
        "parser pika-parse payload code",
        "(handle (pika/parse 42 \"x\") (raise msg (symbol->string (ref msg 'code))))",
        "parser/parse-grammar-not-symbol", pass, fail);
    test_str_val(interp,
        "parser pika-grammar payload code",
        "(handle (pika/grammar 'g-parser-bad '(rule start (nope \"a\"))) (raise msg (symbol->string (ref msg 'code))))",
        "parser/grammar-unknown-operator", pass, fail);

    test_eq(interp, "multi-capture sum", "(checkpoint (+ (capture k1 (k1 10)) (capture k2 (k2 20))))", 30, pass, fail);

    if (!main::stack_runtime_asan_enabled()) {
        // Multi-shot continuations
        test_eq(interp, "multi-shot k twice", "(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))", 32, pass, fail);
        test_eq(interp, "multi-shot k thrice", "(checkpoint (* 2 (capture k (+ (k 3) (+ (k 5) (k 7))))))", 30, pass, fail);
        test_eq(interp, "multi-shot let bindings", "(checkpoint (+ 10 (capture k (let (a (k 1) b (k 2)) (+ a b)))))", 23, pass, fail);
        test_eq(interp, "multi-shot conditional", "(checkpoint (+ 5 (capture k (if true (k 10) (k 20)))))", 15, pass, fail);
        test_eq(
            interp,
            "multi-shot replay set! in resumed segment",
            "(block "
            "  (define replay-set-counter 0) "
            "  (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) "
            "  (+ (* 10 replay-set-r) replay-set-counter))",
            52,
            pass,
            fail
        );
        test_eq_interp(
            interp,
            "multi-shot replay deduce fact writes",
            "(block "
            "  (define replay-db (deduce 'open 'memory)) "
            "  (define replay-rel (deduce 'open-named replay-db 'replay_rel 'replay_rel 'name 'age)) "
            "  (define replay-deduce-id 0) "
            "  (define replay-deduce-r "
            "    (checkpoint (+ (capture k (+ (k 1) (k 1))) "
            "                   (block (set! replay-deduce-id (+ replay-deduce-id 1)) "
            "                          (deduce 'fact! replay-rel \"u\" replay-deduce-id) "
            "                          (deduce 'count replay-rel))))) "
            "  (+ (* 10 replay-deduce-r) replay-deduce-id))",
            52,
            pass,
            fail
        );
        test_eq_interp(
            interp,
            "multi-shot replay handled io effect",
            "(block "
            "  (define replay-io-counter 0) "
            "  (define replay-io-r "
            "    (checkpoint "
            "      (+ (capture k (+ (k 1) (k 1))) "
            "         (block "
            "           (handle (signal io/println \"replay-io\") (io/println x (resolve 0))) "
            "           (set! replay-io-counter (+ replay-io-counter 1)) "
            "           replay-io-counter)))) "
            "  (+ (* 10 replay-io-r) replay-io-counter))",
            52,
            pass,
            fail
        );
        test_eq(interp, "stream-yield continuation resumes beyond two yields",
            "(block "
            "  (define (gen-s3) (checkpoint (block (stream-yield 1) (stream-yield 2) (stream-yield 3) nil))) "
            "  (car (drop 2 (stream-take 3 (gen-s3)))))",
            3, pass, fail);
        test_error_contains(
            interp,
        "nested handle clause rejected",
            "(handle (+ 1 (signal dup 0)) ((dup k x) (+ (k 10) (k 20))))",
        "handle clause syntax was removed",
            pass,
            fail);
        test_eq(
            interp,
            "multi-shot effect",
            "(handle (+ 1 (signal dup 0)) (dup x (with-continuation k (+ (k 10) (k 20)))))",
            32,
            pass,
            fail);

        // with-continuation (multi-shot escape hatch in handler clauses)
        test_eq(interp, "with-continuation basic", "(handle (+ 1 (signal choose 0)) (choose x (with-continuation k (+ (k 10) (k 20)))))", 32, pass, fail);
        test_eq(interp, "with-continuation single", "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))", 42, pass, fail);
        test_error_contains(
            interp,
            "with-continuation outside handle rejected",
            "(with-continuation k (k 1))",
            "with-continuation is only valid inside handle clauses",
            pass,
            fail);
    }

    // TCO inside checkpoint/capture
    setup(interp, "(define checkpoint-loop (lambda (n acc) (if (= n 0) acc (checkpoint-loop (- n 1) (+ acc 1)))))");
    test_eq(interp, "TCO in checkpoint body", "(checkpoint (checkpoint-loop 5000 0))", 5000, pass, fail);
    setup(interp, "(define capture-loop (lambda (n acc) (if (= n 0) acc (capture-loop (- n 1) (+ acc 1)))))");
    test_eq(interp, "TCO in capture body", "(checkpoint (+ 1 (capture k (capture-loop 5000 0))))", 5000, pass, fail);
    test_eq(interp, "TCO block+if in checkpoint", "(checkpoint (block (+ 1 2) (if true (+ 10 20) 99)))", 30, pass, fail);
    test_eq(interp, "TCO let chains in capture", "(checkpoint (+ 1 (capture k (let (x 10) (let (y 20) (+ x y))))))", 30, pass, fail);
    setup(interp, "(define cont-use-loop (lambda (k n) (if (= n 0) (k 42) (cont-use-loop k (- n 1)))))");
    test_eq(interp, "TCO tail-recursive k use", "(checkpoint (+ 1 (capture k (cont-use-loop k 1000))))", 43, pass, fail);
    test_eq(interp, "TCO nested checkpoint", "(checkpoint (+ (checkpoint (if true 10 20)) (capture k (k 5))))", 15, pass, fail);
    setup(interp, "(define handle-loop (lambda (n) (if (= n 0) 0 (+ 1 (handle-loop (- n 1))))))");
    test_eq(interp, "TCO handler body", "(handle (signal count 0) (count x (handle-loop 100)))", 100, pass, fail);
}
