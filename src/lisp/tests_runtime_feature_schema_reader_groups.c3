module lisp;


import std::io;

fn void run_schema_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Schema/Contract Tests");

    // Define a schema
    setup(interp, "(define [schema] age-schema (and int (> 0)))");

    // validate with inline schema
    test_tag(interp, "validate int passes",
        "(validate 'int 42)", SYMBOL, pass, fail);
    test_tag(interp, "validate int fails on string",
        "(validate 'int \"hello\")", NIL, pass, fail);
    test_tag(interp, "validate string passes",
        "(validate 'string \"hello\")", SYMBOL, pass, fail);
    test_tag(interp, "validate any passes",
        "(validate 'any 42)", SYMBOL, pass, fail);
    test_tag(interp, "validate regex clause treats invalid regex as failed validation",
        "(validate '(re \"(\") \"x\")", NIL, pass, fail);

    // validate with composite schema
    test_tag(interp, "validate (and int (> 0)) passes",
        "(validate '(and int (> 0)) 5)", SYMBOL, pass, fail);
    test_tag(interp, "validate (and int (> 0)) fails on negative",
        "(validate '(and int (> 0)) -1)", NIL, pass, fail);
    test_tag(interp, "validate (or string int) passes int",
        "(validate '(or string int) 42)", SYMBOL, pass, fail);
    test_tag(interp, "validate (or string int) passes string",
        "(validate '(or string int) \"hi\")", SYMBOL, pass, fail);

    // validate with named schema
    test_tag(interp, "validate named schema passes",
        "(validate 'age-schema 5)", SYMBOL, pass, fail);
    test_tag(interp, "validate named schema fails",
        "(validate 'age-schema -1)", NIL, pass, fail);

    // maybe
    test_tag(interp, "validate (maybe int) on nil",
        "(validate '(maybe int) nil)", SYMBOL, pass, fail);
    test_tag(interp, "validate (maybe int) on int",
        "(validate '(maybe int) 42)", SYMBOL, pass, fail);

    // schema-explain surface
    test_tag(interp, "schema-explain valid returns nil",
        "(schema-explain 'int 42)", NIL, pass, fail);

    // explain selector surface
    test_truthy(interp, "explain dispatch selector accepted",
        "(= (ref (explain 'dispatch (+ 1 2)) 'kind) 'dispatch)", pass, fail);
    test_truthy(interp, "explain effect selector accepted",
        "(= (ref (explain 'effect (signal io/print \"x\")) 'kind) 'effect)", pass, fail);
    test_truthy(interp, "explain dispatch top-level schema keys",
        "(let (r (explain 'dispatch (+ 1 2))) (and (has? r 'kind) (and (has? r 'status) (and (has? r 'input) (and (has? r 'decision) (and (has? r 'candidates) (and (has? r 'trace) (has? r 'debug_message))))))))", pass, fail);
    test_truthy(interp, "explain effect top-level schema keys",
        "(let (r (explain 'effect (signal io/print \"x\"))) (and (has? r 'kind) (and (has? r 'status) (and (has? r 'input) (and (has? r 'decision) (and (has? r 'candidates) (and (has? r 'trace) (has? r 'debug_message))))))))", pass, fail);
    test_truthy(interp, "explain dispatch trace stable shape",
        "(let (t (ref (explain 'dispatch (+ 1 2)) 'trace)) (and (has? t 'arg-types) (and (has? t 'arg-count) (and (has? t 'target-type) (and (has? t 'method-name) (and (has? t 'method-count) (has? t 'debug_message)))))))", pass, fail);
    test_truthy(interp, "explain effect trace stable shape",
        "(let (t (ref (explain 'effect (resolve 42)) 'trace)) (and (has? t 'effect-tag) (and (has? t 'arg-type) (and (has? t 'handler-count) (and (has? t 'searched-handlers) (and (has? t 'stack-context-active) (and (has? t 'has-continuation-binding) (and (has? t 'effect-continuation) (has? t 'debug_message)))))))))", pass, fail);
    test_truthy(interp, "explain effect handler match reason",
        "(= (handle (ref (ref (explain 'effect (signal explain-eff 7)) 'decision) 'reason) (explain-eff x (resolve x))) 'handler-match)", pass, fail);
    test_eq(interp, "explain effect search order nearest-first",
        "(handle (handle (ref (ref (explain 'effect (signal explain-nested 1)) 'decision) 'search-order) (other x (resolve x))) (explain-nested x (resolve x)))", 1, pass, fail);
    test_truthy(interp, "explain effect skipped handler miss reason",
        "(= (handle (handle (ref (ref (ref (explain 'effect (signal explain-nested 1)) 'candidates) 0) 'miss-reason) (other x (resolve x))) (explain-nested x (resolve x))) 'tag-miss)", pass, fail);
    test_truthy(interp, "explain effect strict boundary reason",
        "(= (ref (ref (handle ^strict (explain 'effect (signal explain-strict 1)) (other x (resolve x))) 'decision) 'reason) 'strict-unhandled-effect)", pass, fail);
    test_truthy(interp, "explain effect unhandled status",
        "(= (ref (explain 'effect (signal explain-unhandled 9)) 'status) 'unhandled)", pass, fail);
    test_truthy(interp, "explain effect unhandled reason",
        "(= (ref (ref (explain 'effect (signal explain-unhandled 9)) 'decision) 'reason) 'unhandled-effect)", pass, fail);
    test_truthy(interp, "explain effect fast-path reason",
        "(= (ref (ref (explain 'effect (signal io/print \"x\")) 'decision) 'reason) 'fast-path)", pass, fail);
    test_truthy(interp, "explain effect resolve outcome",
        "(= (handle (ref (ref (explain 'effect (signal explain-outcome 1)) 'decision) 'outcome) (explain-outcome x (resolve x))) 'resolve)", pass, fail);
    test_truthy(interp, "explain effect resolve outcome inside with module",
        "(= (handle (ref (ref (explain 'effect (signal explain-with-module 1)) 'decision) 'outcome) (explain-with-module x (with math (resolve x)))) 'resolve)", pass, fail);
    test_truthy(interp, "explain effect abort outcome",
        "(= (handle (ref (ref (explain 'effect (signal explain-abort 1)) 'decision) 'outcome) (explain-abort x x)) 'abort)", pass, fail);
    test_truthy(interp, "explain effect resolve invalid continuation reason",
        "(= (ref (ref (explain 'effect (resolve 42)) 'decision) 'reason) 'resolve-invalid-continuation)", pass, fail);
    test_truthy(interp, "explain effect resolve continuation reason",
        "(= (handle (signal explain-resolve nil) (explain-resolve x (ref (ref (explain 'effect (resolve x)) 'decision) 'reason))) 'resolve-effect-continuation)", pass, fail);
    test_truthy(interp, "explain effect decision stable shape",
        "(let (d (ref (explain 'effect (resolve 42)) 'decision)) (and (has? d 'reason) (and (has? d 'outcome) (and (has? d 'handler-index) (and (has? d 'search-order) (and (has? d 'clause-index) (and (has? d 'match-tag) (and (has? d 'strict-handler-index) (and (has? d 'fast-path-index) (and (has? d 'fast-path-primitive) (and (has? d 'arg-name) (and (has? d 'continuation-name) (has? d 'debug_message)))))))))))))", pass, fail);
    setup(interp, "(define (explain-score (^Integer x) (^Integer y)) (+ x y))");
    setup(interp, "(define (explain-score (^Float64 x) (^Float64 y)) (+ x y))");
    test_truthy(interp, "explain dispatch status method-match",
        "(= (ref (ref (explain 'dispatch (explain-score 1 2)) 'decision) 'reason) 'method-match)", pass, fail);
    test_eq(interp, "explain dispatch candidate count",
        "(length (ref (explain 'dispatch (explain-score 1 2)) 'candidates))", 2, pass, fail);
    test_eq(interp, "explain dispatch best-score includes exact match",
        "(ref (ref (explain 'dispatch (explain-score 1 2)) 'decision) 'best-score)", 200, pass, fail);
    test_eq(interp, "explain dispatch winner score exact component",
        "(ref (ref (ref (ref (explain 'dispatch (explain-score 1 2)) 'candidates) 0) 'score-components) 'exact)", 2, pass, fail);
    test_truthy(interp, "explain dispatch decision stable shape",
        "(let (d (ref (explain 'dispatch (explain-score 1 2)) 'decision)) (and (has? d 'reason) (and (has? d 'winner-index) (and (has? d 'best-score) (and (has? d 'tie-count) (and (has? d 'fallback-source) (and (has? d 'outcome) (has? d 'debug_message))))))))", pass, fail);
    test_eq(interp, "explain keeps target form unevaluated",
        "(let (x 0) (block (explain 'dispatch (set! x 1)) x))", 0, pass, fail);
    setup(interp, "(define (explain-amb (^Integer x) y) 'a)");
    setup(interp, "(define (explain-amb x (^Integer y)) 'b)");
    test_truthy(interp, "explain dispatch ambiguity status",
        "(= (ref (explain 'dispatch (explain-amb 1 1)) 'status) 'ambiguous)", pass, fail);
    test_truthy(interp, "explain dispatch ambiguity reason",
        "(= (ref (ref (explain 'dispatch (explain-amb 1 1)) 'decision) 'reason) 'ambiguous-equal-specificity)", pass, fail);
    setup(interp, "(define (explain-src (^Integer x)) x)");
    test_gt(interp, "explain dispatch source line captured",
        "(ref (ref (ref (ref (explain 'dispatch (explain-src 1)) 'candidates) 0) 'source) 'line)", 0, pass, fail);
    test_error_contains(interp, "explain unknown selector rejected",
        "(explain 'schema (+ 1 2))", "unknown selector", pass, fail);
    test_error_contains(interp, "explain non-symbol selector rejected",
        "(explain 1 (+ 1 2))", "selector must be quoted symbol", pass, fail);
}

fn void run_schema_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_schema_tests, pass, fail);
}

fn void run_reader_dispatch_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Reader Dispatch Tests");

    // Regex literal #r""
    test_str(interp, "regex literal #r",
        "(re-match #r\"hello\" \"say hello world\")", pass, fail);

    // Block comment
    test_eq(interp, "block comment",
        "(+ 1 #| ignored |# 2)", 3, pass, fail);

    // Form comment #_
    test_eq(interp, "#_ skips one form",
        "(+ 1 #_ 999 2)", 3, pass, fail);

    // Multi-form comment #N_
    test_eq(interp, "#2_ skips two forms",
        "(+ #2_ 10 20 1 2)", 3, pass, fail);

    // Nested block comment
    test_eq(interp, "nested block comment",
        "(+ 1 #| outer #| inner |# still |# 2)", 3, pass, fail);
    test_error_contains(interp, "unterminated nested block comment errors",
        "(+ 1 #| outer #| inner |# 2", "unterminated block comment", pass, fail);

    // `run` is intentionally multi-form; strict trailing-token rejection is covered
    // by compiler fail-closed tests through the single-expression parser entrypoint.
    test_eq_interp(interp, "run evaluates trailing top-level form",
        "(+ 1 2) 3", 3, pass, fail);
    test_error_contains(interp, "ellipsis token fails closed",
        "...", "unexpected '...'", pass, fail);

    // Reader tags rewrite to one-argument calls. Function handlers are ordinary
    // definitions; [reader tag] is the canonical macro-tag declaration surface.
    test_eq(interp, "reader tag function call shorthand",
        "(block (define (reader-inc x) (+ x 1)) #reader-inc 41)", 42, pass, fail);
    test_eq(interp, "reader tag supports slash names",
        "(block (define (debug/spy x) x) #debug/spy 7)", 7, pass, fail);
    test_eq(interp, "reader tag supports lowercase x radix-looking names",
        "(block (define (xface x) (+ x 1)) #xface 41)", 42, pass, fail);
    test_eq(interp, "reader tag supports binary-looking names with digits",
        "(block (define (b101tag x) (+ x 2)) #b101tag 40)", 42, pass, fail);
    test_eq(interp, "reader tag supports octal-looking names with digits",
        "(block (define (o777tag x) (+ x 3)) #o777tag 39)", 42, pass, fail);
    test_eq(interp, "reader tag macro declaration",
        "(block (define [reader tag] reader-double (syntax-match ([x] (template (+ (insert x) (insert x)))))) #reader-double 21)",
        42, pass, fail);
    test_error_contains(interp, "reader tag malformed define rejected",
        "(define [reader tag] bad 1)", "macro definition requires", pass, fail);

    // Non-symbol hash dispatch should still fail closed.
    test_error_contains(interp, "numeric # dispatch rejected",
        "(+ 1 #1x 2)", "unknown # dispatch sequence", pass, fail);
    test_error_contains(interp, "bare # dispatch rejected",
        "(+ 1 # 2)", "unknown # dispatch sequence", pass, fail);

    test_error_contains(interp, "quoted symbol is not callable on dict",
        "('name (Dictionary 'name \"Alice\" 'age 30))", "not a function", pass, fail);
    test_error_contains(interp, "quoted symbol is not callable on dict int case",
        "('age (Dictionary 'name \"Alice\" 'age 30))", "not a function", pass, fail);
}

fn void run_reader_dispatch_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_reader_dispatch_tests, pass, fail);
}
