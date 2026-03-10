module lisp;


import std::io;

fn void run_advanced_type_parametric_core_hierarchy_tests(Interp* interp, int* pass, int* fail) {
    // === TYPE SYSTEM TESTS (Phase 1) ===

    // type-of primitive
    test_truthy(interp, "type-of int", "(= (type-of 42) 'Int)", pass, fail);
    test_truthy(interp, "type-of double", "(= (type-of 3.14) 'Double)", pass, fail);
    test_truthy(interp, "type-of string", "(= (type-of \"hello\") 'String)", pass, fail);
    test_truthy(interp, "type-of symbol", "(= (type-of 'foo) 'Symbol)", pass, fail);
    test_truthy(interp, "type-of nil", "(= (type-of nil) 'Nil)", pass, fail);
    test_truthy(interp, "type-of list", "(= (type-of '(1 2 3)) 'List)", pass, fail);
    test_truthy(interp, "type-of array", "(= (type-of (array 1 2 3)) 'Array)", pass, fail);
    test_truthy(interp, "type-of closure", "(= (type-of (lambda (x) x)) 'Closure)", pass, fail);
    test_truthy(interp, "type-of true", "(= (type-of true) 'Bool)", pass, fail);

    // is? primitive
    test_truthy(interp, "is? int", "(is? 42 'Int)", pass, fail);
    test_truthy(interp, "is? string", "(is? \"hello\" 'String)", pass, fail);
    test_nil(interp, "is? mismatch", "(is? 42 'String)", pass, fail);
    test_truthy(interp, "is? Any", "(is? 42 'Any)", pass, fail);

    // define [type] — simple struct (interp-only: JIT doesn't know user types yet)
    setup(interp, "(define [type] Point (^Int x) (^Int y))");
    test_truthy_interp(interp, "type constructor", "(instance? (Point 3 4))", pass, fail);
    test_truthy_interp(interp, "type-of instance", "(= (type-of (Point 3 4)) 'Point)", pass, fail);
    test_eq_interp(interp, "type field access x", "(let (p (Point 3 4)) p.x)", 3, pass, fail);
    test_eq_interp(interp, "type field access y", "(let (p (Point 3 4)) p.y)", 4, pass, fail);
    test_truthy_interp(interp, "is? instance type", "(is? (Point 3 4) 'Point)", pass, fail);

    // define [struct] — alias for [type]
    setup(interp, "(define [struct] Vec2 (^Int x) (^Int y))");
    test_truthy_interp(interp, "struct alias constructor", "(instance? (Vec2 9 8))", pass, fail);
    test_eq_interp(interp, "struct alias field access", "(let (v (Vec2 9 8)) v.x)", 9, pass, fail);

    // define [abstract] — abstract type
    setup(interp, "(define [abstract] Shape)");
    test_truthy(interp, "abstract type defined", "(= 'Shape 'Shape)", pass, fail);

    // define [type] with parent
    setup(interp, "(define [type] (Circle Shape) (^Int radius))");
    test_truthy_interp(interp, "subtype constructor", "(instance? (Circle 5))", pass, fail);
    test_truthy_interp(interp, "is? subtype", "(is? (Circle 5) 'Circle)", pass, fail);
    test_truthy_interp(interp, "is? parent type", "(is? (Circle 5) 'Shape)", pass, fail);
    test_nil_interp(interp, "is? wrong parent", "(is? (Point 1 2) 'Shape)", pass, fail);
    test_eq_interp(interp, "subtype field access", "(let (c (Circle 5)) c.radius)", 5, pass, fail);

    // Nested type access
    setup(interp, "(define [type] Line (^Point start) (^Point end))");
    test_eq_interp(interp, "nested type access", "(let (l (Line (Point 1 2) (Point 3 4))) l.start.x)", 1, pass, fail);
    test_eq_interp(interp, "nested type access 2", "(let (l (Line (Point 1 2) (Point 3 4))) l.end.y)", 4, pass, fail);

    // define [union] — union types
    setup(interp, "(define [union] (Option T) None (Some T))");
    test_truthy_interp(interp, "union nullary", "(instance? None)", pass, fail);
    test_truthy_interp(interp, "union constructor", "(instance? (Some 42))", pass, fail);
    test_truthy_interp(interp, "is? union variant", "(is? None 'Option)", pass, fail);
    test_truthy_interp(interp, "is? union Some", "(is? (Some 42) 'Option)", pass, fail);
    test_truthy_interp(interp, "is? union Some exact", "(is? (Some 42) 'Some)", pass, fail);

    {
        const usz FIELD_COUNT = 80;
        char[8192] define_buf;
        char[8192] access_buf;
        if (!advanced_build_wide_type_define(&define_buf, define_buf.len, FIELD_COUNT) ||
            !advanced_build_wide_type_access(&access_buf, access_buf.len, FIELD_COUNT)) {
            io::printn("[FAIL] wide type field regression builder");
            (*fail)++;
        } else {
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            test_eq_interp(interp, "wide type preserves >64 fields", access_buf[..advanced_test_cstr_len(&access_buf)], (long)(FIELD_COUNT - 1), pass, fail);
        }
    }

    {
        const usz VARIANT_COUNT = 24;
        char[8192] define_buf;
        char[8192] match_buf;
        if (!advanced_build_wide_union_define(&define_buf, define_buf.len, VARIANT_COUNT) ||
            !advanced_build_wide_union_match(&match_buf, match_buf.len, VARIANT_COUNT)) {
            io::printn("[FAIL] wide union regression builder");
            (*fail)++;
        } else {
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            test_eq_interp(interp, "wide union preserves >16 variants", match_buf[..advanced_test_cstr_len(&match_buf)], (long)(VARIANT_COUNT - 1), pass, fail);
        }
    }

    // define [alias]
    setup(interp, "(define [alias] Num Int)");
    test_truthy(interp, "alias defined", "(= 'Num 'Num)", pass, fail);

    // instance? primitive
    test_nil(interp, "instance? non-instance", "(instance? 42)", pass, fail);
}

fn void run_advanced_type_parametric_type_args_tests(Interp* interp, int* pass, int* fail) {
    // === PARAMETRIC TYPE TESTS (Phase 7) ===

    // Parametric type with single type param
    setup(interp, "(define [type] (Box T) (^T value))");
    test_truthy_interp(interp, "Box constructor", "(instance? (Box 42))", pass, fail);
    test_eq_interp(interp, "Box field access", "(let (b (Box 42)) b.value)", 42, pass, fail);
    test_truthy_interp(interp, "Box type-of", "(= (type-of (Box 42)) 'Box)", pass, fail);

    // type-args primitive — infer T=Int from (Box 42)
    test_truthy_interp(interp, "type-args Box Int", "(= (car (type-args (Box 42))) 'Int)", pass, fail);
    test_truthy_interp(interp, "type-args Box String", "(= (car (type-args (Box \"hi\"))) 'String)", pass, fail);

    // Parametric type with two type params
    setup(interp, "(define [type] (Pair A B) (^A first) (^B second))");
    test_eq_interp(interp, "Pair field first", "(let (p (Pair 1 \"two\")) p.first)", 1, pass, fail);
    test_truthy_interp(interp, "Pair field second", "(= (let (p (Pair 1 \"two\")) p.second) \"two\")", pass, fail);
    test_truthy_interp(interp, "type-args Pair A", "(= (car (type-args (Pair 1 \"two\"))) 'Int)", pass, fail);
    test_truthy_interp(interp, "type-args Pair B", "(= (car (cdr (type-args (Pair 1 \"two\")))) 'String)", pass, fail);

    {
        const usz PARAM_COUNT = 40;
        char[8192] define_buf;
        if (!advanced_build_many_type_params_define(&define_buf, define_buf.len, PARAM_COUNT)) {
            io::printn("[FAIL] wide type param regression builder");
            (*fail)++;
        } else {
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            test_eq_interp(interp, "deftype preserves >32 type params", "(let (p (ParamTower 42)) p.tail)", 42, pass, fail);
        }
    }

    {
        const usz META_COUNT = 20;
        char[8192] define_buf;
        if (!advanced_build_many_meta_entries_define(&define_buf, define_buf.len, META_COUNT)) {
            io::printn("[FAIL] wide metadata regression builder");
            (*fail)++;
        } else {
            setup(interp, define_buf[..advanced_test_cstr_len(&define_buf)]);
            test_eq_interp(interp, "type annotation preserves >16 metadata entries", "(let (m (MetaTower 42)) m.value)", 42, pass, fail);
        }
    }

    // type-args on non-parametric type returns nil
    test_nil_interp(interp, "type-args non-param", "(type-args (Point 1 2))", pass, fail);
    // type-args on non-instance returns nil
    test_nil_interp(interp, "type-args int", "(type-args 42)", pass, fail);
}

fn void run_advanced_type_parametric_ctor_annotation_tests(Interp* interp, int* pass, int* fail) {
    test_truthy_interp(interp, "ctor annotation Box Int", "(instance? (^(Box Int) (Box 42)))", pass, fail);
    test_truthy_interp(interp, "ctor annotation Pair Int/String",
        "(instance? (^(Pair Int String) (Pair 1 \"two\")))", pass, fail);

    test_str_val(interp, "ctor annotation mismatch payload code",
        "(try (lambda (x) (^(Box String) (Box 42))) "
        "     (lambda (payload) (symbol->string (ref payload 'code))))",
        "type/ctor-type-arg-mismatch", pass, fail);

    test_str_val(interp, "ctor annotation arity payload code",
        "(try (lambda (x) (^(Pair Int) (Pair 1 \"two\"))) "
        "     (lambda (payload) (symbol->string (ref payload 'code))))",
        "type/ctor-arity-mismatch", pass, fail);

    test_eq_interp(interp, "ctor annotation mismatch arg-index",
        "(try (lambda (x) (^(Pair Int String) (Pair 1 2))) "
        "     (lambda (payload) (ref (ref payload 'data) 'arg-index)))",
        1, pass, fail);

    test_eq_interp(interp, "ctor annotation mismatch arg-path head",
        "(try (lambda (x) (^(Pair Int String) (Pair 1 2))) "
        "     (lambda (payload) (car (ref (ref payload 'data) 'arg-path))))",
        1, pass, fail);
    test_truthy_interp(interp, "ctor annotation mismatch payload ctor field",
        "(try (lambda (x) (^(Pair Int String) (Pair 1 2))) "
        "     (lambda (payload) (= (ref (ref payload 'data) 'ctor) 'Pair)))",
        pass, fail);
    test_eq_interp(interp, "ctor annotation mismatch payload expected-args len",
        "(try (lambda (x) (^(Pair Int String) (Pair 1 2))) "
        "     (lambda (payload) (length (ref (ref payload 'data) 'expected-args))))",
        2, pass, fail);
    test_eq_interp(interp, "ctor annotation mismatch payload actual-args len",
        "(try (lambda (x) (^(Pair Int String) (Pair 1 2))) "
        "     (lambda (payload) (length (ref (ref payload 'data) 'actual-args))))",
        2, pass, fail);

    // Regression: unconstrained constructor calls continue to work unchanged.
    test_truthy_interp(interp, "ctor unconstrained call unchanged", "(instance? (Box \"still-ok\"))", pass, fail);

    setup(interp, "(define [type] (Inner T) (^T value))");
    setup(interp, "(define [type] (Outer T) (^T item))");
    test_truthy_interp(interp, "ctor annotation nested user type",
        "(instance? (^(Outer (Inner Int)) (Outer (Inner 7))))", pass, fail);
    test_eq_interp(interp, "ctor annotation nested mismatch arg-path depth",
        "(try (lambda (x) (^(Outer (Inner String)) (Outer (Inner 7)))) "
        "     (lambda (payload) (length (ref (ref payload 'data) 'arg-path))))",
        2, pass, fail);
}

fn void run_advanced_type_lambda_call_boundary_tests(Interp* interp, int* pass, int* fail) {
    test_eq_interp(interp, "typed lambda call boundary positive",
        "((lambda ((^Int x)) (+ x 1)) 41)", 42, pass, fail);

    test_str_val(interp, "typed lambda call boundary mismatch payload code",
        "(try (lambda (x) ((lambda ((^Int n)) n) \"nope\")) "
        "     (lambda (payload) (symbol->string (ref payload 'code))))",
        "type/arg-mismatch", pass, fail);

    test_eq_interp(interp, "typed lambda call boundary mismatch param-index",
        "(try (lambda (x) ((lambda ((^Int n)) n) \"nope\")) "
        "     (lambda (payload) (ref (ref payload 'data) 'param-index)))",
        0, pass, fail);
    test_str_val(interp, "typed lambda call boundary mismatch failure kind",
        "(try (lambda (x) ((lambda ((^Int n)) n) \"nope\")) "
        "     (lambda (payload) (symbol->string (ref (ref payload 'data) 'failure))))",
        "type", pass, fail);
    test_str_val(interp, "typed lambda call boundary mismatch expected type",
        "(try (lambda (x) ((lambda ((^Int n)) n) \"nope\")) "
        "     (lambda (payload) (symbol->string (ref (ref payload 'data) 'expected))))",
        "Int", pass, fail);
    test_str_val(interp, "typed lambda call boundary mismatch actual type",
        "(try (lambda (x) ((lambda ((^Int n)) n) \"nope\")) "
        "     (lambda (payload) (symbol->string (ref (ref payload 'data) 'actual))))",
        "String", pass, fail);

    test_eq_interp(interp, "typed variadic lambda fixed-prefix mismatch param-index",
        "(try (lambda (x) ((lambda ((^Int a) .. rest) a) \"bad\" 2)) "
        "     (lambda (payload) (ref (ref payload 'data) 'param-index)))",
        0, pass, fail);

    test_str_val(interp, "Lambda annotation gates closure-valued args",
        "(try (lambda (x) ((lambda ((^(Lambda Int Int) f)) 1) 42)) "
        "     (lambda (payload) (symbol->string (ref payload 'code))))",
        "type/arg-mismatch", pass, fail);

    test_eq_interp(interp, "Lambda annotation accepts closure args",
        "((lambda ((^(Lambda Int Int) f)) (f 41)) "
        " (lambda ((^Int x)) (+ x 1)))",
        42, pass, fail);

    // Return-type compatibility is currently a deliberate non-goal for lambda annotations.
    test_truthy_interp(interp, "Lambda annotation return type currently unchecked",
        "(= ((lambda ((^(Lambda Int Int) f)) (f 1)) "
        "    (lambda ((^Int x)) \"ok\")) "
        "   \"ok\")",
        pass, fail);

    // Cross-test: numeric widening contract is shared with dispatch checks.
    test_truthy_interp(interp, "typed lambda numeric widening Int->Double",
        "(= ((lambda ((^Double x)) x) 7) 7.0)", pass, fail);

    // Cross-test: typed lambda boundary checks compose with dispatch-selected methods.
    setup(interp, "(define (l3-dispatch-lam (^Int x)) ((lambda ((^Int y)) (+ y 5)) x))");
    setup(interp, "(define (l3-dispatch-lam x) (- 0 1))");
    test_eq_interp(interp, "typed lambda inside dispatch-selected Int method", "(l3-dispatch-lam 2)", 7, pass, fail);
    test_eq_interp(interp, "typed lambda dispatch fallback unaffected", "(l3-dispatch-lam \"no\")", -1, pass, fail);

    // Cross-test: union subtyping applies at lambda call boundary.
    setup(interp, "(define [union] L3Opt L3None (L3Some Int))");
    test_eq_interp(interp, "typed lambda union accepts variant subtype",
        "((lambda ((^L3Opt v)) 1) (L3Some 9))", 1, pass, fail);
    test_str_val(interp, "typed lambda union mismatch payload code",
        "(try (lambda (x) ((lambda ((^L3Opt v)) 1) 42)) "
        "     (lambda (payload) (symbol->string (ref payload 'code))))",
        "type/arg-mismatch", pass, fail);
}

fn void run_advanced_type_parametric_disambiguation_tests(Interp* interp, int* pass, int* fail) {
    // Disambiguation: (Circle Shape) still works as parent, not type param
    // Circle was defined above with parent Shape — verify it still works
    test_truthy_interp(interp, "disambig parent still works", "(is? (Circle 5) 'Shape)", pass, fail);
}

fn void run_advanced_parser_type_def_edge_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "parser deftype missing name",
        "(define [type])",
        "expected type name after [type]",
        pass, fail);

    test_error_contains(interp,
        "parser deftype compound missing name",
        "(define [type] () (^Int x))",
        "expected type name",
        pass, fail);

    test_error_contains(interp,
        "parser deftype missing field name",
        "(define [type] Broken (^Int))",
        "expected field name",
        pass, fail);

    test_error_contains(interp,
        "parser defunion missing name",
        "(define [union])",
        "expected union name after [union]",
        pass, fail);

    test_error_contains(interp,
        "parser defunion compound missing name",
        "(define [union] () (Some T))",
        "expected union name",
        pass, fail);

    test_error_contains(interp,
        "parser defunion variant missing name",
        "(define [union] U ())",
        "expected variant name",
        pass, fail);

    test_error_contains(interp,
        "parser deftype compound missing close",
        "(define [type] (Box T (^Int x))",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser defunion compound missing close",
        "(define [union] (Maybe T (Some T) None)",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser defunion variant missing close",
        "(define [union] Maybe (Some T)",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser deftype compound non-symbol type param",
        "(define [type] (Box T 123) (^Int x))",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser defunion compound non-symbol type param",
        "(define [union] (Maybe T 123) None (Some T))",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser defunion variant non-symbol name",
        "(define [union] Maybe (123 T))",
        "expected variant name",
        pass, fail);

    test_error_contains(interp,
        "parser deftype covariant marker unsupported",
        "(define [type] (Box +T) (^+T value))",
        "type parameters are invariant",
        pass, fail);

    test_error_contains(interp,
        "parser defunion contravariant marker unsupported",
        "(define [union] (Sink -T) End (Push -T))",
        "type parameters are invariant",
        pass, fail);
}

fn void run_advanced_type_parametric_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_type_parametric_core_hierarchy_tests(interp, pass, fail);
    run_advanced_type_parametric_type_args_tests(interp, pass, fail);
    run_advanced_type_parametric_ctor_annotation_tests(interp, pass, fail);
    run_advanced_type_lambda_call_boundary_tests(interp, pass, fail);
    run_advanced_type_parametric_disambiguation_tests(interp, pass, fail);
    run_advanced_parser_type_def_edge_tests(interp, pass, fail);
}

fn void run_advanced_dispatch_constructor_basic_tests(Interp* interp, int* pass, int* fail) {
    // === MULTIPLE DISPATCH TESTS (Phase 2) ===

    // Basic dispatch on types
    setup(interp, "(define (describe (^Int n)) \"integer\")");
    setup(interp, "(define (describe (^String s)) \"string\")");
    setup(interp, "(define (describe x) \"other\")");
    test_truthy_interp(interp, "dispatch int", "(= (describe 42) \"integer\")", pass, fail);
    test_truthy_interp(interp, "dispatch string", "(= (describe \"hi\") \"string\")", pass, fail);
    test_truthy_interp(interp, "dispatch fallback", "(= (describe '(1 2)) \"other\")", pass, fail);

    // Val dispatch (Fibonacci)
    setup(interp, "(define (fib (^(Val 0) n)) 0)");
    setup(interp, "(define (fib (^(Val 1) n)) 1)");
    setup(interp, "(define (fib (^Int n)) (+ (fib (- n 1)) (fib (- n 2))))");
    test_eq_interp(interp, "dispatch fib 0", "(fib 0)", 0, pass, fail);
    test_eq_interp(interp, "dispatch fib 1", "(fib 1)", 1, pass, fail);
    test_eq_interp(interp, "dispatch fib 10", "(fib 10)", 55, pass, fail);

    // Value/Val literal dispatch parity (symbol + string)
    setup(interp, "(define (udp-cmd (^(Value open) op)) 1)");
    setup(interp, "(define (udp-cmd (^(Val bind) op) h host port) 2)");
    setup(interp, "(define (udp-cmd op .. rest) 99)");
    test_eq_interp(interp, "dispatch Value symbol literal", "(udp-cmd 'open)", 1, pass, fail);
    test_eq_interp(interp, "dispatch Val symbol literal", "(udp-cmd 'bind 1 \"127.0.0.1\" 9000)", 2, pass, fail);

    setup(interp, "(define (fmt (^(Value \"csv\") x)) 10)");
    setup(interp, "(define (fmt (^(Val \"toml\") x)) 20)");
    setup(interp, "(define (fmt x) 99)");
    test_eq_interp(interp, "dispatch Value string literal", "(fmt \"csv\")", 10, pass, fail);
    test_eq_interp(interp, "dispatch Val string literal", "(fmt \"toml\")", 20, pass, fail);

    setup(interp, "(define (flag (^(Value true) x)) 11)");
    setup(interp, "(define (flag (^(Val false) x)) 22)");
    setup(interp, "(define (flag x) 99)");
    test_eq_interp(interp, "dispatch Value bool true literal", "(flag true)", 11, pass, fail);
    test_eq_interp(interp, "dispatch Val bool false literal", "(flag false)", 22, pass, fail);

    setup(interp, "(define (ambv (^(Value open) cmd) x) 1)");
    setup(interp, "(define (ambv x (^(Value open) cmd)) 2)");
    test_error(interp, "dispatch ambiguous equal-specificity Value literal", "(ambv 'open 'open)", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Value literal message",
        "(ambv 'open 'open)",
        "ambiguous method call", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Value literal equal-specificity detail",
        "(ambv 'open 'open)",
        "equal specificity", pass, fail);

    setup(interp, "(define (ambi-int (^Int x) y) 10)");
    setup(interp, "(define (ambi-int x (^Int y)) 20)");
    test_error(interp, "dispatch ambiguous equal-specificity Int positional", "(ambi-int 1 1)", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Int positional message",
        "(ambi-int 1 1)",
        "ambiguous method call", pass, fail);
    test_error_contains(interp, "dispatch ambiguous Int positional equal-specificity detail",
        "(ambi-int 1 1)",
        "equal specificity", pass, fail);

    // Unified command-style facade over canonical io/udp-* operations.
    setup(interp, "(define (io/udp-open) 'opened)");
    setup(interp, "(define (io/udp-bind h host port) (list 'bound h host port))");
    setup(interp, "(define (io/udp-send h host port payload) (list 'sent h host port payload))");
    setup(interp, "(define (udp (^(Value open) op)) (io/udp-open))");
    setup(interp, "(define (udp (^(Value bind) op) h host port) (io/udp-bind h host port))");
    setup(interp, "(define (udp (^(Value send) op) h host port payload) (io/udp-send h host port payload))");
    test_truthy_interp(interp, "dispatch udp open command facade", "(= (udp 'open) 'opened)", pass, fail);
    test_truthy_interp(interp, "dispatch udp bind command facade",
        "(= (udp 'bind 7 \"127.0.0.1\" 9001) '(bound 7 \"127.0.0.1\" 9001))", pass, fail);
    test_truthy_interp(interp, "dispatch udp send command facade",
        "(= (udp 'send 7 \"127.0.0.1\" 9001 \"ping\") '(sent 7 \"127.0.0.1\" 9001 \"ping\"))", pass, fail);

    // Multi-arg dispatch
    setup(interp, "(define (add2 (^Int a) (^Int b)) (+ a b))");
    setup(interp, "(define (add2 (^String a) (^String b)) (string-append a b))");
    test_eq_interp(interp, "dispatch multi-arg int", "(add2 3 4)", 7, pass, fail);
    test_truthy_interp(interp, "dispatch multi-arg str", "(= (add2 \"hello\" \" world\") \"hello world\")", pass, fail);

    // Numeric promotion in dispatch: Int can satisfy ^Double via widening.
    setup(interp, "(define (num-tag (^Double x)) 10)");
    setup(interp, "(define (num-tag (^Int x)) 20)");
    setup(interp, "(define (num-tag x) 30)");
    test_eq_interp(interp, "dispatch numeric promotion double exact", "(num-tag 3.5)", 10, pass, fail);
    test_eq_interp(interp, "dispatch numeric promotion int exact beats widening", "(num-tag 3)", 20, pass, fail);

    setup(interp, "(define (only-double (^Double x)) 11)");
    setup(interp, "(define (only-double x) 22)");
    test_eq_interp(interp, "dispatch numeric promotion int to double", "(only-double 7)", 11, pass, fail);

    // Last-arg dispatch past legacy 8-arg scoring cap
    setup(interp, "(define (dispatch9 a b c d e f g h (^Int i)) 111)");
    setup(interp, "(define (dispatch9 a b c d e f g h (^String i)) 222)");
    test_eq_interp(interp, "dispatch 9th arg int", "(dispatch9 1 2 3 4 5 6 7 8 9)", 111, pass, fail);
    test_eq_interp(interp, "dispatch 9th arg string", "(dispatch9 1 2 3 4 5 6 7 8 \"nine\")", 222, pass, fail);

    {
        const usz ARG_COUNT = 40;
        char[12288] define_int_buf;
        char[12288] define_string_buf;
        char[8192] call_int_buf;
        char[8192] call_string_buf;
        if (!advanced_build_dispatch_define(&define_int_buf, define_int_buf.len, "dispatch40", ARG_COUNT, "Int", 111) ||
            !advanced_build_dispatch_define(&define_string_buf, define_string_buf.len, "dispatch40", ARG_COUNT, "String", 222) ||
            !advanced_build_dispatch_call(&call_int_buf, call_int_buf.len, "dispatch40", ARG_COUNT, false) ||
            !advanced_build_dispatch_call(&call_string_buf, call_string_buf.len, "dispatch40", ARG_COUNT, true)) {
            io::printn("[FAIL] wide dispatch signature regression builder");
            (*fail)++;
        } else {
            setup(interp, define_int_buf[..advanced_test_cstr_len(&define_int_buf)]);
            setup(interp, define_string_buf[..advanced_test_cstr_len(&define_string_buf)]);
            test_eq_interp(interp, "dispatch 40th arg int", call_int_buf[..advanced_test_cstr_len(&call_int_buf)], 111, pass, fail);
            test_eq_interp(interp, "dispatch 40th arg string", call_string_buf[..advanced_test_cstr_len(&call_string_buf)], 222, pass, fail);
        }
    }
}

fn void run_advanced_dispatch_constructor_constraint_tests(Interp* interp, int* pass, int* fail) {
    // === CONSTRAINED DISPATCH TESTS (Phase 7) ===

    // Define abstract type hierarchy for constraint testing
    setup(interp, "(define [abstract] (Numeric Number))");  // Numeric extends Number (but Number doesn't exist yet, no parent)

    // Actually, let's use fresh abstract type hierarchy
    setup(interp, "(define [abstract] Addable)");
    setup(interp, "(define [type] (MyInt Addable) (^Int val))");
    setup(interp, "(define [type] (MyStr Addable) (^String val))");
    setup(interp, "(define [type] NotAddable (^Int val))");

    // Constrained dispatch: only matches if arg type <: Addable
    setup(interp, "(define (get-val (^{'T Addable} x)) x.val)");
    test_eq_interp(interp, "constraint MyInt match", "(get-val (MyInt 42))", 42, pass, fail);
    test_truthy_interp(interp, "constraint MyStr match", "(= (get-val (MyStr \"hi\")) \"hi\")", pass, fail);

    // Constraint fallback: NotAddable doesn't satisfy Addable constraint
    // Add a fallback method that accepts anything
    setup(interp, "(define (get-val x) (- 0 1))");
    test_eq_interp(interp, "constraint fallback", "(get-val (NotAddable 99))", -1, pass, fail);

    {
        const usz CONSTRAINT_COUNT = 40;
        char[12288] define_constraint_buf;
        if (!advanced_build_constraint_define(&define_constraint_buf, define_constraint_buf.len, CONSTRAINT_COUNT, "String", 111)) {
            io::printn("[FAIL] wide dispatch constraint regression builder");
            (*fail)++;
        } else {
            setup(interp, define_constraint_buf[..advanced_test_cstr_len(&define_constraint_buf)]);
            setup(interp, "(define (constraint-wide x) 222)");
            test_eq_interp(interp, "dispatch preserves >32 constraints", "(constraint-wide 1)", 222, pass, fail);
        }
    }
}

fn void run_advanced_dispatch_constructor_unification_union_tests(Interp* interp, int* pass, int* fail) {
    // === PARAMETRIC METHOD UNIFICATION TESTS ===
    setup(interp, "(define (pair-same (^T a) (^T b)) 10)");
    setup(interp, "(define (pair-same a b) 20)");
    test_eq_interp(interp, "dispatch type-var unification same Int", "(pair-same 1 2)", 10, pass, fail);
    test_eq_interp(interp, "dispatch type-var unification mismatch falls back", "(pair-same 1 \"x\")", 20, pass, fail);

    setup(interp, "(define (num-same (^{'T Number} a) (^{'T Number} b)) 11)");
    setup(interp, "(define (num-same a b) 22)");
    test_eq_interp(interp, "dispatch constrained type-var unification Int/Int", "(num-same 3 4)", 11, pass, fail);
    test_eq_interp(interp, "dispatch constrained type-var unification Int/Double fallback", "(num-same 3 4.0)", 22, pass, fail);

    // === UNION PARTICIPATION IN DISPATCH APPLICABILITY TESTS ===
    setup(interp, "(define [union] DispatchOpt DNone (DSome Int) (DText String))");
    setup(interp, "(define (opt-tag (^DispatchOpt v)) 100)");
    setup(interp, "(define (opt-tag (^DSome v)) 200)");
    setup(interp, "(define (opt-tag (^DText v)) 300)");
    test_eq_interp(interp, "dispatch union parent accepts nullary variant", "(opt-tag DNone)", 100, pass, fail);
    test_eq_interp(interp, "dispatch union variant beats union parent (DSome)", "(opt-tag (DSome 7))", 200, pass, fail);
    test_eq_interp(interp, "dispatch union variant beats union parent (DText)", "(opt-tag (DText \"hi\"))", 300, pass, fail);
}

fn void run_advanced_dispatch_constructor_validation_tests(Interp* interp, int* pass, int* fail) {
    // === CONSTRUCTOR FIELD TYPE VALIDATION TESTS ===

    // Correct types — should succeed
    test_truthy_interp(interp, "ctor valid Point", "(instance? (Point 3 4))", pass, fail);
    // Wrong type — string where Int expected
    test_error(interp, "ctor type error str for int", "(Point \"x\" 4)", pass, fail);
    // Wrong type — double where Int expected
    test_error(interp, "ctor type error dbl for int", "(Point 3.0 4)", pass, fail);
    // Subtype field — Circle has Int field, still works
    test_truthy_interp(interp, "ctor valid Circle", "(instance? (Circle 5))", pass, fail);
    // Nested instance type — Line expects Point fields
    test_truthy_interp(interp, "ctor valid Line", "(instance? (Line (Point 1 2) (Point 3 4)))", pass, fail);
    // Parametric field — Box accepts anything (no constraint)
    test_truthy_interp(interp, "ctor parametric Box", "(instance? (Box \"anything\"))", pass, fail);
}

fn void run_advanced_dispatch_constructor_ambiguity_parametric_tests(Interp* interp, int* pass, int* fail) {
    // === DISPATCH AMBIGUITY DETECTION TESTS ===

    // Two methods with identical signatures — second replaces first (idempotent re-define)
    setup(interp, "(define (ambig-test (^Int x) (^Int y)) (+ x y))");
    setup(interp, "(define (ambig-test (^Int x) (^Int y)) (* x y))");
    test_eq_interp(interp, "dispatch redefine replaces", "(ambig-test 3 4)", 12, pass, fail);
    // Different specificity — should succeed (picks more specific)
    setup(interp, "(define (specific-test (^Int x)) (* x 2))");
    setup(interp, "(define (specific-test x) 0)");
    test_eq_interp(interp, "dispatch no ambiguity", "(specific-test 5)", 10, pass, fail);

    // === PARAMETRIC CONSTRAINT AT CONSTRUCTION TESTS ===

    // Define constrained parametric type: T must be subtype of Number
    // Constraint expressed via dict annotation on field: ^{'T Number}
    setup(interp, "(define [type] (NumBox T) (^{'T Number} value))");
    // Int <: Number — should succeed
    test_eq_interp(interp, "NumBox Int ok", "(let (b (NumBox 42)) b.value)", 42, pass, fail);
    // Double <: Number — should succeed
    test_truthy_interp(interp, "NumBox Dbl ok", "(= (type-of (NumBox 3.14)) 'NumBox)", pass, fail);
    // String not <: Number — should error
    test_error(interp, "NumBox String fail", "(NumBox \"hi\")", pass, fail);
}

fn void run_advanced_dispatch_constructor_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_dispatch_constructor_basic_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_constraint_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_unification_union_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_validation_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_ambiguity_parametric_tests(interp, pass, fail);
}

fn void run_advanced_index_mutation_tests(Interp* interp, int* pass, int* fail) {
    // Array/dict indexing (Phase 3)
    test_eq(interp, "array index", "(let (v (array 10 20 30)) v.[0])", 10, pass, fail);
    test_eq(interp, "array index 2", "(let (v (array 10 20 30)) v.[2])", 30, pass, fail);

    // Struct field mutation (Phase 3)
    test_eq_interp(interp, "set! struct field", "(let (p (Point 1 2)) (begin (set! p.x 99) p.x))", 99, pass, fail);
    test_eq_interp(interp, "set! struct field y", "(let (p (Point 1 2)) (begin (set! p.y 50) p.y))", 50, pass, fail);
    test_eq_interp(interp, "set! nested field", "(let (ln (Line (Point 0 0) (Point 10 10))) (begin (set! ln.start.x 5) ln.start.x))", 5, pass, fail);
    test_eq_interp(interp, "set! preserves other", "(let (p (Point 3 4)) (begin (set! p.x 99) p.y))", 4, pass, fail);
    test_eq_interp(interp, "instance field ffi survives scope return",
        "(begin "
        "  (define [type] EscBox h) "
        "  (define (mk-esc-box) (let (a (atomic 7)) (EscBox a))) "
        "  (define b (mk-esc-box)) "
        "  (atomic-add! b.h 1) "
        "  (atomic-read b.h))", 8, pass, fail);
    test_eq_interp(interp, "closure capture instance ffi field",
        "(begin "
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
    test_truthy_interp(interp, "is? double Number", "(is? 3.14 'Number)", pass, fail);
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
    test_truthy_interp(interp, "number? double", "(number? 3.14)", pass, fail);
    test_nil_interp(interp, "number? string", "(number? \"hi\")", pass, fail);
}

fn void run_advanced_consolidation_predicate_equivalence_tests(Interp* interp, int* pass, int* fail) {
    // === PREDICATE EQUIVALENCE TESTS ===
    test_truthy_interp(interp, "int? positive", "(int? 42)", pass, fail);
    test_nil_interp(interp, "int? negative", "(int? 3.14)", pass, fail);
    test_truthy_interp(interp, "double? positive", "(double? 3.14)", pass, fail);
    test_nil_interp(interp, "double? negative", "(double? 42)", pass, fail);
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
    setup(interp, "(define [type] Vec2 (^Double vx) (^Double vy))");
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
