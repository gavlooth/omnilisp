module lisp;

import std::io;

fn void run_advanced_type_parametric_core_hierarchy_tests(Interp* interp, int* pass, int* fail) {
    // === TYPE SYSTEM TESTS (Phase 1) ===

    // type-of primitive
    test_truthy(interp, "type-of int", "(= (type-of 42) 'Integer)", pass, fail);
    test_truthy(interp, "type-of double", "(= (type-of 3.14) 'Double)", pass, fail);
    test_truthy(interp, "type-of string", "(= (type-of \"hello\") 'String)", pass, fail);
    test_truthy(interp, "type-of symbol", "(= (type-of 'foo) 'Symbol)", pass, fail);
    test_truthy(interp, "type-of nil", "(= (type-of nil) 'Nil)", pass, fail);
    test_truthy(interp, "type-of list", "(= (type-of '(1 2 3)) 'List)", pass, fail);
    test_truthy(interp, "type-of array", "(= (type-of (Array 1 2 3)) 'Array)", pass, fail);
    test_truthy(interp, "type-of closure", "(= (type-of (lambda (x) x)) 'Closure)", pass, fail);
    test_truthy(interp, "type-of true", "(= (type-of true) 'Boolean)", pass, fail);

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

    // type-args primitive — infer T=Integer from (Box 42)
    test_truthy_interp(interp, "type-args Box Integer", "(= (car (type-args (Box 42))) 'Integer)", pass, fail);
    test_truthy_interp(interp, "type-args Box String", "(= (car (type-args (Box \"hi\"))) 'String)", pass, fail);

    // Parametric type with two type params
    setup(interp, "(define [type] (Pair A B) (^A first) (^B second))");
    test_eq_interp(interp, "Pair field first", "(let (p (Pair 1 \"two\")) p.first)", 1, pass, fail);
    test_truthy_interp(interp, "Pair field second", "(= (let (p (Pair 1 \"two\")) p.second) \"two\")", pass, fail);
    test_truthy_interp(interp, "type-args Pair A", "(= (car (type-args (Pair 1 \"two\"))) 'Integer)", pass, fail);
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

    // Cross-test: typed lambda boundaries follow exact-or-subtype semantics (no implicit widening).
    test_error(interp, "typed lambda numeric no implicit Int->Double widening",
        "((lambda ((^Double x)) x) 7)", pass, fail);
    test_truthy_interp(interp, "typed lambda numeric explicit constructor conversion",
        "(= ((lambda ((^Double x)) x) (Double 7)) 7.0)", pass, fail);

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

fn void run_advanced_type_value_alias_deprecation_matrix_tests(Interp* interp, int* pass, int* fail) {
    // Canonical constructor form should remain accepted.
    test_eq_interp(interp,
        "parser matrix accepts Value int constructor",
        "((lambda ((^(Value 0) n)) n) 0)",
        0,
        pass,
        fail);

    test_eq_interp(interp,
        "parser matrix accepts Value symbol constructor",
        "((lambda ((^(Value open) cmd)) 88) 'open)",
        88,
        pass,
        fail);

    test_str_val(interp,
        "parser matrix accepts Value string constructor",
        "((lambda ((^(Value \"x\") x)) x) \"x\")",
        "x",
        pass,
        fail);

    test_eq_interp(interp,
        "parser matrix accepts Value bool constructor",
        "((lambda ((^(Value true) ok)) 99) true)",
        99,
        pass,
        fail);

    // Legacy alias should be rejected at parse time with explicit canonical message.
    test_error_contains(interp,
        "parser matrix rejects Val int constructor",
        "((lambda ((^(Val 0) n)) n) 0)",
        "Val constructor was removed; use Value",
        pass,
        fail);

    test_error_contains(interp,
        "parser matrix rejects Val symbol constructor",
        "((lambda ((^(Val open) n)) n) 'open)",
        "Val constructor was removed; use Value",
        pass,
        fail);

    test_error_contains(interp,
        "parser matrix rejects Val string constructor",
        "((lambda ((^(Val \"x\") n)) n) \"x\")",
        "Val constructor was removed; use Value",
        pass,
        fail);
}

fn void run_advanced_type_meta_symbols_type_only_tests(Interp* interp, int* pass, int* fail) {
    // Meta/abstract symbols remain non-callable constructors. They remain valid
    // type symbols and descriptor values...
    test_truthy(interp,
        "Any type symbol participates in is?",
        "(is? 7 'Any)",
        pass,
        fail);
    test_str_val(interp,
        "Any type descriptor rendering",
        "(format \"%s\" Any)",
        "#<type Any>",
        pass,
        fail);
    test_str_val(interp,
        "Number type descriptor rendering",
        "(format \"%s\" Number)",
        "#<type Number>",
        pass,
        fail);
    test_str_val(interp,
        "Collection type descriptor rendering",
        "(format \"%s\" Collection)",
        "#<type Collection>",
        pass,
        fail);

    test_eq_interp(interp,
        "Number annotation accepts int",
        "((lambda ((^Number x)) x) 7)",
        7,
        pass,
        fail);

    test_eq_interp(interp,
        "Collection annotation accepts array",
        "(length ((lambda ((^Collection xs)) xs) (Array 1 2 3)))",
        3,
        pass,
        fail);

    // ...but they are not callable constructor/coercion symbols in value position.
    test_error(interp, "Any value-position constructor rejected", "(Any 1)", pass, fail);
    test_error(interp, "Value value-position constructor rejected", "(Value 1)", pass, fail);
    test_error(interp, "Number value-position constructor rejected", "(Number 1)", pass, fail);
    test_error(interp, "Collection value-position constructor rejected", "(Collection (Array 1))", pass, fail);
}

fn void run_advanced_type_parametric_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_type_parametric_core_hierarchy_tests(interp, pass, fail);
    run_advanced_type_parametric_type_args_tests(interp, pass, fail);
    run_advanced_type_parametric_ctor_annotation_tests(interp, pass, fail);
    run_advanced_type_lambda_call_boundary_tests(interp, pass, fail);
    run_advanced_type_parametric_disambiguation_tests(interp, pass, fail);
    run_advanced_parser_type_def_edge_tests(interp, pass, fail);
    run_advanced_type_value_alias_deprecation_matrix_tests(interp, pass, fail);
    run_advanced_type_meta_symbols_type_only_tests(interp, pass, fail);
}
