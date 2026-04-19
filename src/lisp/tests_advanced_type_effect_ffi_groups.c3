module lisp;


fn void run_advanced_index_mutation_tests(Interp* interp, int* pass, int* fail) {
    // Array/dict indexing (Phase 3)
    test_eq(interp, "array index", "(let (v (Array 10 20 30)) v.[0])", 10, pass, fail);
    test_eq(interp, "array index 2", "(let (v (Array 10 20 30)) v.[2])", 30, pass, fail);

    // Struct field mutation (Phase 3)
    test_eq_interp(interp, "set! struct field", "(let (p (Point 1 2)) (block (set! p.x 99) p.x))", 99, pass, fail);
    test_eq_interp(interp, "set! struct field y", "(let (p (Point 1 2)) (block (set! p.y 50) p.y))", 50, pass, fail);
    test_eq_interp(interp, "set! nested field", "(let (ln (Line (Point 0 0) (Point 10 10))) (block (set! ln.start.x 5) ln.start.x))", 5, pass, fail);
    test_eq_interp(interp, "set! preserves other", "(let (p (Point 3 4)) (block (set! p.x 99) p.y))", 4, pass, fail);
    test_error_contains(interp, "set! unbound path root error",
        "(set! missing-point.x 1)",
        "set!: unbound path root", pass, fail);
    test_error_contains(interp, "set! path intermediate non-instance error",
        "(let (p (Point 1 2)) (set! p.x.y.z 7))",
        "set!: path segment is not an instance or cons", pass, fail);
    test_error_contains(interp, "set! path missing intermediate field error",
        "(let (ln (Line (Point 0 0) (Point 10 10))) (set! ln.nope.x 7))",
        "set!: field not found in path", pass, fail);
    test_error_contains(interp, "set! final missing instance field error",
        "(let (p (Point 1 2)) (set! p.nope 7))",
        "set!: field not found", pass, fail);
    test_error_contains(interp, "set! cons path segment restriction",
        "(let (p (cons 1 2)) (set! p.foo 7))",
        "set!: cons only supports .car and .cdr", pass, fail);
    test_error_contains(interp, "set! target type restriction",
        "(let (x 1) (set! x.y 7))",
        "set!: target is not an instance or cons", pass, fail);
    test_error_contains(interp, "set! generic target restriction",
        "(set! 42 0 7)",
        "set!: generic form expects array or dict target", pass, fail);
    test_eq_interp(interp, "instance field ffi survives scope return",
        "(block "
        "  (define [type] EscBox h) "
        "  (define (mk-esc-box) (let (a (atomic 7)) (EscBox a))) "
        "  (define b (mk-esc-box)) "
        "  (atomic-add! b.h 1) "
        "  (atomic-read b.h))", 8, pass, fail);
    test_eq_interp(interp, "closure capture instance ffi field",
        "(block "
        "  (define [type] EscBoxFn h) "
        "  (define (mk-esc-fn) "
        "    (let (a (atomic 3) b (EscBoxFn a)) "
        "      (lambda () (atomic-read b.h)))) "
        "  (define f (mk-esc-fn)) "
        "  (f))", 3, pass, fail);

    // Hashmap indexing (Phase 3)
    test_eq_interp(interp, "dict index", "(let (m {'a 1 'b 2}) m.['a])", 1, pass, fail);
}

fn void run_advanced_consolidation_primitive_hierarchy_tests(Interp* interp, int* pass, int* fail) {
    // === PRIMITIVE CONSOLIDATION TESTS (Phase 4) ===
    // Overload + for Point type (primitive becomes fallback)
    setup(interp, "(define (add-points (^Point a) (^Point b)) (Point (+ a.x b.x) (+ a.y b.y)))");
    test_eq_interp(interp, "typed dispatch Point+", "(let (r (add-points (Point 1 2) (Point 3 4))) r.x)", 4, pass, fail);
    test_eq_interp(interp, "typed dispatch Point+ y", "(let (r (add-points (Point 1 2) (Point 3 4))) r.y)", 6, pass, fail);
    // Built-in + still works (dispatch preserves primitive fallback)
    test_eq_interp(interp, "builtin + after dispatch", "(+ 10 20)", 30, pass, fail);
    // Overload length for custom type
    setup(interp, "(define (my-len (^Point p)) 2)");
    test_eq_interp(interp, "typed dispatch custom len", "(my-len (Point 5 6))", 2, pass, fail);

    // === ABSTRACT TYPE HIERARCHY TESTS ===
    // Number hierarchy
    test_truthy_interp(interp, "is? int Number", "(is? 42 'Number)", pass, fail);
    test_truthy_interp(interp, "is? Float64 Number", "(is? 3.14 'Number)", pass, fail);
    test_nil_interp(interp, "is? string not Number", "(is? \"hi\" 'Number)", pass, fail);
    test_nil_interp(interp, "is? nil not Number", "(is? nil 'Number)", pass, fail);

    // Collection hierarchy
    test_truthy_interp(interp, "is? list Collection", "(is? '(1 2) 'Collection)", pass, fail);
    test_truthy_interp(interp, "is? array Collection", "(is? [1 2] 'Collection)", pass, fail);
    test_truthy_interp(interp, "is? dict Collection", "(is? {'a 1} 'Collection)", pass, fail);
    test_nil_interp(interp, "is? int not Collection", "(is? 42 'Collection)", pass, fail);
    test_nil_interp(interp, "is? string not Collection", "(is? \"hi\" 'Collection)", pass, fail);

    // number? uses abstract hierarchy
    test_truthy_interp(interp, "number? int", "(number? 42)", pass, fail);
    test_truthy_interp(interp, "number? Float64", "(number? 3.14)", pass, fail);
    test_nil_interp(interp, "number? string", "(number? \"hi\")", pass, fail);
}

fn void run_advanced_consolidation_predicate_equivalence_tests(Interp* interp, int* pass, int* fail) {
    // === PREDICATE EQUIVALENCE TESTS ===
    test_truthy_interp(interp, "int? positive", "(int? 42)", pass, fail);
    test_nil_interp(interp, "int? negative", "(int? 3.14)", pass, fail);
    test_truthy_interp(interp, "float64? positive", "(float64? 3.14)", pass, fail);
    test_nil_interp(interp, "float64? negative", "(float64? 42)", pass, fail);
    test_truthy_interp(interp, "string? positive", "(string? \"hi\")", pass, fail);
    test_nil_interp(interp, "string? negative", "(string? 42)", pass, fail);
    test_truthy_interp(interp, "symbol? positive", "(symbol? 'foo)", pass, fail);
    test_nil_interp(interp, "symbol? negative", "(symbol? 42)", pass, fail);
    test_truthy_interp(interp, "boolean? true", "(boolean? true)", pass, fail);
    test_truthy_interp(interp, "boolean? false", "(boolean? false)", pass, fail);
    test_nil_interp(interp, "boolean? int", "(boolean? 42)", pass, fail);
    test_truthy_interp(interp, "list? positive", "(list? '(1 2))", pass, fail);
    test_truthy_interp(interp, "list? nil", "(list? nil)", pass, fail);
    test_nil_interp(interp, "list? int", "(list? 42)", pass, fail);
    test_nil_interp(interp, "list? improper list", "(list? (cons 1 2))", pass, fail);
    test_truthy_interp(interp, "closure? positive", "(closure? (lambda (x) x))", pass, fail);
    test_nil_interp(interp, "closure? int", "(closure? 42)", pass, fail);
    test_truthy_interp(interp, "array? positive", "(array? [1 2])", pass, fail);
    test_nil_interp(interp, "array? int", "(array? 42)", pass, fail);
    test_truthy_interp(interp, "dict? positive", "(dict? {'a 1})", pass, fail);
    test_nil_interp(interp, "dict? int", "(dict? 42)", pass, fail);
    test_truthy_interp(interp, "zero? yes", "(zero? 0)", pass, fail);
    test_nil_interp(interp, "zero? no", "(zero? 1)", pass, fail);
    test_truthy_interp(interp, "positive? yes", "(positive? 5)", pass, fail);
    test_nil_interp(interp, "positive? no", "(positive? -1)", pass, fail);
    test_truthy_interp(interp, "negative? yes", "(negative? -3)", pass, fail);
    test_nil_interp(interp, "negative? no", "(negative? 0)", pass, fail);
    test_truthy_interp(interp, "even? yes", "(even? 4)", pass, fail);
    test_nil_interp(interp, "even? no", "(even? 3)", pass, fail);
    test_truthy_interp(interp, "odd? yes", "(odd? 3)", pass, fail);
    test_nil_interp(interp, "odd? no", "(odd? 4)", pass, fail);
}

fn void run_advanced_consolidation_dispatch_extensibility_tests(Interp* interp, int* pass, int* fail) {
    // === DISPATCH EXTENSIBILITY TESTS ===
    setup(interp, "(define [type] Vec2 (^Float64 vx) (^Float64 vy))");
    setup(interp, "(define (+ (^Vec2 a) (^Vec2 b)) (Vec2 (+ a.vx b.vx) (+ a.vy b.vy)))");
    test_truthy_interp(interp, "Vec2 + dispatch x",
        "(= (let (v (+ (Vec2 1.0 2.0) (Vec2 3.0 4.0))) v.vx) 4.0)", pass, fail);
    test_truthy_interp(interp, "Vec2 + dispatch y",
        "(= (let (v (+ (Vec2 1.0 2.0) (Vec2 3.0 4.0))) v.vy) 6.0)", pass, fail);
    // Built-in + still works after Vec2 extension
    test_eq_interp(interp, "builtin + after Vec2", "(+ 1 2)", 3, pass, fail);
    // Extend length for Vec2
    setup(interp, "(define (length (^Vec2 v)) (sqrt (+ (* v.vx v.vx) (* v.vy v.vy))))");
    test_truthy_interp(interp, "Vec2 length dispatch",
        "(= (length (Vec2 3.0 4.0)) 5.0)", pass, fail);
    // Built-in length still works
    test_eq_interp(interp, "builtin length after Vec2", "(length '(1 2 3))", 3, pass, fail);
    // procedure? recognizes method tables
    test_truthy_interp(interp, "procedure? method table", "(procedure? +)", pass, fail);
}

fn void run_advanced_consolidation_predicate_dispatch_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_consolidation_primitive_hierarchy_tests(interp, pass, fail);
    run_advanced_consolidation_predicate_equivalence_tests(interp, pass, fail);
    run_advanced_consolidation_dispatch_extensibility_tests(interp, pass, fail);
}

fn void run_advanced_mutation_hierarchy_dispatch_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_index_mutation_tests(interp, pass, fail);
    run_advanced_consolidation_predicate_dispatch_tests(interp, pass, fail);
}
