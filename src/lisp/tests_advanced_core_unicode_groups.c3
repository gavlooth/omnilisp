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
    test_eq(interp, "Int shorthand ctor from double", "(Int 3.9)", 3, pass, fail);
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
    test_truthy(interp, "Bool shorthand ctor from truthy int", "(Bool 0)", pass, fail);
    test_truthy(interp, "Boolean ctor type", "(= (type-of (Boolean 0)) 'Boolean)", pass, fail);
    test_truthy(interp, "false collapses to nil", "(= false nil)", pass, fail);
    test_truthy(interp, "type-of false is Nil", "(= (type-of false) 'Nil)", pass, fail);
    test_nil(interp, "Nil ctor from nil", "(Nil nil)", pass, fail);
    test_nil(interp, "Nil ctor from false", "(Nil false)", pass, fail);
    test_truthy(interp, "Void ctor type", "(= (type-of (Void)) 'Void)", pass, fail);
    test_truthy(interp, "Void ctor equality", "(= (Void) (Void))", pass, fail);
    test_truthy(interp, "Void ctor truthy", "(if (Void) true nil)", pass, fail);
    test_truthy(interp, "Closure ctor identity", "(closure? (Closure (lambda (x) x)))", pass, fail);
    test_truthy(interp, "List ctor type", "(= (type-of (List 1 2 3)) 'List)", pass, fail);
    test_eq(interp, "List ctor from array len", "(length (List [1 2 3]))", 3, pass, fail);
    test_eq(interp, "List ctor from iterator len", "(length (List (Iterator [1 2 3])))", 3, pass, fail);
    test_truthy(interp, "Array ctor type", "(= (type-of (Array 1 2 3)) 'Array)", pass, fail);
    test_eq(interp, "Array ctor from list ref", "(ref (Array '(7 8 9)) 1)", 8, pass, fail);
    test_eq(interp, "Array ctor from iterator ref", "(ref (Array (Iterator [7 8 9])) 1)", 8, pass, fail);
    test_truthy(interp, "Dictionary ctor type", "(= (type-of (Dictionary 'a 1)) 'Dictionary)", pass, fail);
    test_eq(interp, "Dict shorthand ctor", "(ref (Dict 'a 1 'b 2) 'b)", 2, pass, fail);
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
        "Dict shorthand odd-arity payload code",
        "(handle (Dict 'a) (raise msg (symbol->string (ref msg 'code))))",
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
        "make-iterator invalid payload code",
        "(handle (make-iterator 42) (raise msg (symbol->string (ref msg 'code))))",
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
    test_truthy(interp, "make-iterator creates iter",
        "(iterator? (make-iterator (lambda () nil)))", pass, fail);
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
    test_eq(interp, "array it3 len", "(length (array it3))", 3, pass, fail);
    test_eq(interp, "array it3 ref 0", "(ref (array it3) 0)", 3, pass, fail);
    // type-of iterator
    test_truthy(interp, "type-of iterator",
        "(is? (Iterator (lambda () nil)) (quote Iterator))", pass, fail);
}

fn void run_advanced_unicode_iterator_lazy_pipeline_tests(Interp* interp, int* pass, int* fail) {
    // Phase 3B: iterator constructors + lazy combinators
    test_truthy(interp, "iterator dispatches list",
        "(iterator? (iterator (quote (1 2 3))))", pass, fail);
    test_truthy(interp, "Iterator dispatches list",
        "(iterator? (Iterator (quote (1 2 3))))", pass, fail);
    test_eq(interp, "iterator list list car", "(car (list (iterator (quote (1 2 3)))))", 1, pass, fail);
    test_eq(interp, "iterator list list len", "(length (list (iterator (quote (1 2 3)))))", 3, pass, fail);
    test_eq(interp, "Iterator list list car", "(car (list (Iterator (quote (1 2 3)))))", 1, pass, fail);
    test_eq(interp, "iterator array list car", "(car (list (iterator [10 20 30])))", 10, pass, fail);
    test_eq(interp, "iterator array list len", "(length (list (iterator [10 20 30])))", 3, pass, fail);
    test_eq(interp, "iterator dict list len", "(length (list (iterator (block (define iter-d {'a 1 'b 2}) iter-d))))", 2, pass, fail);
    test_truthy(interp, "iterator idempotent", "(iterator? (iterator (iterator [1 2 3])))", pass, fail);
    // Lazy map on iterator
    test_eq(interp, "map + list car", "(car (list (map (lambda (x) (* x 2)) (iterator [1 2 3]))))", 2, pass, fail);
    test_eq(interp, "map + list len", "(length (list (map (lambda (x) (* x 2)) (iterator [1 2 3]))))", 3, pass, fail);
    // Lazy filter on iterator
    test_truthy(interp, "filter iterator returns iterator",
        "(iterator? (filter even? (iterator [1 2 3 4])))", pass, fail);
    test_eq(interp, "filter even car", "(car (list (filter even? (iterator [1 2 3 4 5]))))", 2, pass, fail);
    test_eq(interp, "filter even len", "(length (list (filter even? (iterator [1 2 3 4 5]))))", 2, pass, fail);
    // Lazy take on iterator
    test_eq(interp, "take 2 len", "(length (list (take 2 (iterator [10 20 30]))))", 2, pass, fail);
    test_eq(interp, "take 2 car", "(car (list (take 2 (iterator [10 20 30]))))", 10, pass, fail);
    // Infinite + take
    test_eq(interp, "range-from + take", "(car (list (take 5 (range-from 0))))", 0, pass, fail);
    test_eq(interp, "range-from + take len", "(length (list (take 5 (range-from 0))))", 5, pass, fail);
    // map + filter pipeline
    test_eq(interp, "lazy pipeline len",
        "(length (list (take 3 (filter even? (map (lambda (x) (* x 2)) (range-from 1))))))", 3, pass, fail);
    // array constructor from iterator
    test_eq(interp, "array from iter", "(ref (array (iterator [5 6 7])) 1)", 6, pass, fail);
    // foldl on iterator
    test_eq(interp, "foldl iterator sum", "(foldl (lambda (a x) (+ a x)) 0 (iterator [1 2 3 4 5]))", 15, pass, fail);
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
    setup(interp, "(define ptest (cons (cons (quote x) 10) (cons (cons (quote y) 20) (quote ()))))");
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
            "legacy nested handle clause rejected",
            "(handle (+ 1 (signal dup 0)) ((dup k x) (+ (k 10) (k 20))))",
            "legacy handle clause syntax was removed",
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

fn void run_advanced_runtime_control_tests(Interp* interp, int* pass, int* fail) {
    // Trampoline (JIT delegates effects to eval which handles them)
    test_eq(interp, "trampoline passthrough", "(with-trampoline (lambda (xx) 42))", 42, pass, fail);
    test_eq(interp, "trampoline single bounce", "(with-trampoline (lambda (xx) (+ 1 (signal bounce (lambda (xx) 41)))))", 42, pass, fail);
    setup(interp, "(define fib-t (lambda (n) (if (< n 2) n (+ (signal bounce (lambda (xx) (fib-t (- n 1)))) (signal bounce (lambda (xx) (fib-t (- n 2))))))))");
    test_eq(interp, "trampoline fib 7", "(with-trampoline (lambda (xx) (fib-t 7)))", 13, pass, fail);

    // Coroutine primitives (coroutine/resume/yield)
    test_eq(interp, "coroutine basic", "(resume (coroutine (lambda () (+ 1 2))))", 3, pass, fail);
    test_eq(interp, "Coroutine basic", "(resume (Coroutine (lambda () (+ 1 2))))", 3, pass, fail);
    test_eq(interp, "coroutine yield", "(let (f (coroutine (lambda () (yield 42) 99))) (resume f))", 42, pass, fail);
    test_eq(interp, "coroutine yield complete", "(let (f (coroutine (lambda () (yield 42) 99))) (resume f) (resume f))", 99, pass, fail);
    test_eq(interp, "coroutine resume value", "(let (f (coroutine (lambda () (+ 1 (yield 0))))) (resume f) (resume f 10))", 11, pass, fail);
    test_eq(interp, "coroutine nested", "(let (outer (coroutine (lambda () (let (inner (coroutine (lambda () 10))) (+ (resume inner) 5))))) (resume outer))", 15, pass, fail);
    test_eq(interp, "coroutine deep nest", "(let (f (coroutine (lambda () (let (a (coroutine (lambda () (let (b (coroutine (lambda () 1))) (+ (resume b) 2))))) (+ (resume a) 3))))) (resume f))", 6, pass, fail);
    test_eq(interp, "coroutine multi yield", "(let (g (coroutine (lambda () (yield 1) (yield 2) 3))) (+ (resume g) (+ (resume g) (resume g))))", 6, pass, fail);
    setup(interp, "(define (coroutine-gen n) (coroutine (lambda () (let loop (i 0) (if (= i n) i (block (yield i) (loop (+ i 1))))))))");
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
