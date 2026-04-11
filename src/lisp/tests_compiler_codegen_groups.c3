module lisp;


import std::io;
import main;
fn void run_compiler_group_multiarg_tco_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 1: Multi-arg calls (inline currying)
    // =========================================================================

    // 41. Multi-arg call uses inline currying (invoke_once + invoke)
    {
        char[] code = compile_to_c3("(+ 1 2)", interp);
        bool ok = compiler_code_contains_all(code, {"invoke"}) &&
                  compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: multi-arg uses inline currying",
            "[FAIL] Compiler: multi-arg uses inline currying",
            &pass,
            &fail
        );
    }

    // 42. Multi-arg uses invoke_once for intermediate args
    {
        char[] code = compile_to_c3("(f 1 2 3)", interp);
        bool ok = str_contains(code, "invoke_once") || str_contains(code, "apply_multi");
        compiler_test_finish_bool(
            ok,
            "Compiler: multi-arg uses invoke_once or apply_multi lowering",
            "[FAIL] Compiler: multi-arg uses invoke_once or apply_multi lowering",
            &pass,
            &fail
        );
    }

    // 43. 3-arg call uses apply_multi
    {
        char[] code = compile_to_c3("(substring \"hello\" 1 3)", interp);
        bool ok = str_contains(code, "apply_multi");
        compiler_test_finish_bool(
            ok,
            "Compiler: 3-arg apply_multi",
            "[FAIL] Compiler: 3-arg apply_multi",
            &pass,
            &fail
        );
    }

    // 43b. >32 arg call still compiles all args and uses apply_multi
    {
        char[] code = compile_to_c3(
            "(f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)",
            interp
        );
        bool ok = compiler_code_contains_all(code, {"apply_multi", "make_int(40)"}) &&
                  compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: >32 arg call emits all temps",
            "[FAIL] Compiler: >32 arg call emits all temps",
            &pass,
            &fail
        );
    }

    // 43c. >32 arg list build still compiles all args
    {
        char[] code = compile_to_c3(
            "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40)",
            interp
        );
        bool ok = compiler_code_contains_all(code, {"make_int(40)", "aot::cons"}) &&
                  compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: >32 arg list emits all temps",
            "[FAIL] Compiler: >32 arg list emits all temps",
            &pass,
            &fail
        );
    }

    // 44. Zero-arg call via invoke
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = str_contains(code, "invoke") && str_contains(code, "make_nil()");
        compiler_test_finish_bool(
            ok,
            "Compiler: zero-arg invoke",
            "[FAIL] Compiler: zero-arg invoke",
            &pass,
            &fail
        );
    }

    // 44b. Callable match guards use their scanned lambda, not a prelude fallback slot.
    {
        char[] code = compile_to_c3("(match 1 ((? (lambda (v) v)) 7) (_ 0))", interp);
        bool ok = compiler_match_guard_uses_scanned_lambda(code)
            && compiler_code_contains_all(code, {"match_guard_eval"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: callable match guard uses scanned lambda",
            "[FAIL] Compiler: callable match guard uses scanned lambda",
            &pass,
            &fail
        );
    }

    // 44c. Guard lambdas capture clause-local pattern bindings during scan/codegen.
    {
        char[] code = compile_to_c3("(match 5 ((? (lambda (v) (> x 3)) x) x) (_ 0))", interp);
        bool ok = compiler_code_contains_all(code, {"captured_x", "captured_x = x;", "match_guard_eval"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: callable match guard captures clause-local bindings",
            "[FAIL] Compiler: callable match guard captures clause-local bindings",
            &pass,
            &fail
        );
    }

    // 44d. Registered primitives in stdlib/user lambdas are looked up, not captured as C3 locals.
    {
        char[] code = compile_to_c3("(let (f (lambda (x) (error? x))) f)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"error?\")"})
            && compiler_code_contains_none(code, {"captured_error", "captured_error_p", " = error;", " = error_p;"});
        compiler_test_finish_bool(
            ok,
            "Compiler: runtime primitives are not closure captures",
            "[FAIL] Compiler: runtime primitives are not closure captures",
            &pass,
            &fail
        );
    }

    // 44e. Cons patterns lower to explicit cons/car/cdr checks instead of matching unconditionally.
    {
        char[] code = compile_to_c3("(match (cons 1 2) ((a . b) a) (_ 0))", interp);
        bool ok = compiler_code_contains_all(code, {"aot::is_cons(", "aot::car(", "aot::cdr("})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: cons pattern lowers directly",
            "[FAIL] Compiler: cons pattern lowers directly",
            &pass,
            &fail
        );
    }

    // 44f. Sequence patterns lower through recursive seq helpers, not shallow list-only checks.
    {
        char[] code = compile_to_c3("(match (Array 1 2) ([1 x] x) (_ 0))", interp);
        bool ok = compiler_code_contains_all(code, {"aot::seq_length(", "aot::seq_nth(", "aot::values_equal("})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: sequence pattern lowers recursively with array parity",
            "[FAIL] Compiler: sequence pattern lowers recursively with array parity",
            &pass,
            &fail
        );
    }

    // 44g. Dict patterns lower through explicit dict helpers and symbol-key binding.
    {
        char[] code = compile_to_c3("(match (Dictionary 'name 7) ({name} name) (_ 0))", interp);
        bool ok = compiler_code_contains_all(code, {"aot::is_dict(", "aot::dict_get_symbol("})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: dict pattern lowers directly",
            "[FAIL] Compiler: dict pattern lowers directly",
            &pass,
            &fail
        );
    }

    // 44h. Constructor/nullary-constructor patterns lower through typed instance helpers.
    {
        char[] code = compile_to_c3(
            "(define [union] Maybe None (Some Integer)) (match (Some 1) ((Some x) x) (None 0))",
            interp
        );
        bool ok = compiler_code_contains_all(code, {"aot::match_constructor(", "aot::instance_field(", "aot::match_var_pattern("})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: constructor patterns lower directly",
            "[FAIL] Compiler: constructor patterns lower directly",
            &pass,
            &fail
        );
    }

    // =========================================================================
    // Phase 2: TCO via trampoline
    // =========================================================================

    // 45. Tail calls in lambda body emit apply_multi_tail
    {
        char[] code = compile_to_c3("(define (f x) (f (+ x 1)))", interp);
        bool ok = str_contains(code, "apply_multi_tail");
        compiler_test_finish_bool(
            ok,
            "Compiler: tail call emits apply_multi_tail",
            "[FAIL] Compiler: tail call emits apply_multi_tail",
            &pass,
            &fail
        );
    }

    // 45b. >32 arg tail call still compiles all args
    {
        char[] code = compile_to_c3(
            "(define (f) (g 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 "
            "21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40))",
            interp
        );
        bool ok = compiler_code_contains_all(code, {"apply_multi_tail", "make_int(40)"}) &&
                  compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: >32 arg tail call emits all temps",
            "[FAIL] Compiler: >32 arg tail call emits all temps",
            &pass,
            &fail
        );
    }

    // 46. Non-tail calls use apply_multi (not tail variant)
    {
        char[] code = compile_to_c3("(define (f x) (+ (f x) 1))", interp);
        // The (f x) is not in tail position (it's an argument to +)
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(
            ok,
            "Compiler: non-tail compiles OK",
            "[FAIL] Compiler: non-tail compiles OK",
            &pass,
            &fail
        );
    }

    // 47. If branches propagate tail position
    {
        char[] code = compile_to_c3("(define (f x) (if (= x 0) 1 (f (- x 1))))", interp);
        bool ok = str_contains(code, "apply_multi_tail");
        compiler_test_finish_bool(
            ok,
            "Compiler: if branch tail position",
            "[FAIL] Compiler: if branch tail position",
            &pass,
            &fail
        );
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_quasiquote_macro_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 3: Quasiquote, macros, modules
    // =========================================================================

    // 48. Quasiquote compiles natively with aot::cons
    {
        char[] code = compile_to_c3("(let (x 5) `(a ,x))", interp);
        bool ok = str_contains(code, "aot::cons") && str_contains(code, "make_symbol");
        compiler_test_finish_bool(
            ok,
            "Compiler: quasiquote compiles natively",
            "[FAIL] Compiler: quasiquote compiles natively",
            &pass,
            &fail
        );
    }

    // 49. Quasiquote with unquote uses make_symbol for literal parts
    {
        char[] code = compile_to_c3("(let (x 5) `(a ,x))", interp);
        bool ok = str_contains(code, "make_symbol(\"a\")");
        compiler_test_finish_bool(
            ok,
            "Compiler: quasiquote uses make_symbol",
            "[FAIL] Compiler: quasiquote uses make_symbol",
            &pass,
            &fail
        );
    }

    {
        char[] code = compile_to_c3("`(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)", interp);
        bool ok = str_contains(code, "aot::cons") && !str_contains(code, "unsupported");
        compiler_test_finish_bool(
            ok,
            "Compiler: wide quasiquote compiles natively",
            "[FAIL] Compiler: wide quasiquote compiles natively",
            &pass,
            &fail
        );
    }

    // 50. Defmacro delegates to eval_source
    {
        char[] code = compile_to_c3(
            "(define [macro] double (syntax-match ([x] (template (+ (insert x) (insert x))))))",
            interp
        );
        bool ok = str_contains(code, "make_nil") && !str_contains(code, "unsupported");
        compiler_test_finish_bool(
            ok,
            "Compiler: defmacro compiles as no-op",
            "[FAIL] Compiler: defmacro compiles as no-op",
            &pass,
            &fail
        );
    }

    // 51. Stdlib functions (when, unless) compiled from STDLIB_PRELUDE
    {
        char[] code = compile_to_c3("42", interp);
        bool ok = str_contains(code, "when") && str_contains(code, "unless");
        compiler_test_finish_bool(
            ok,
            "Compiler: stdlib functions compiled",
            "[FAIL] Compiler: stdlib functions compiled",
            &pass,
            &fail
        );
    }

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_missing_numeric_symbol_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 4: Numeric parse primitives
    // =========================================================================

    // 52. parse-number compiles
    {
        char[] code = compile_to_c3("(parse-number \"42\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"parse-number\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: parse-number", "[FAIL] Compiler: parse-number", pass, fail);
    }

    // 53. String compiles
    {
        char[] code = compile_to_c3("(String 42)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"String\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: String", "[FAIL] Compiler: String", pass, fail);
    }

    // 54. gensym compiles
    {
        char[] code = compile_to_c3("(gensym)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"gensym\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: gensym", "[FAIL] Compiler: gensym", pass, fail);
    }

    // 55. apply compiles
    {
        char[] code = compile_to_c3("(apply + (list 1 2))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"apply\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: apply", "[FAIL] Compiler: apply", pass, fail);
    }

    // 56. equal? compiles
    {
        char[] code = compile_to_c3("(equal? 1 2)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"equal?\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: equal?", "[FAIL] Compiler: equal?", pass, fail);
    }
}

fn void run_compiler_group_missing_io_ref_tests(Interp* interp, usz* pass, usz* fail) {
    // 57. display compiles
    {
        char[] code = compile_to_c3("(display \"hello\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"display\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: display", "[FAIL] Compiler: display", pass, fail);
    }

    // 58. ref compiles
    {
        char[] code = compile_to_c3("(ref (Dictionary) 'a)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ref\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ref", "[FAIL] Compiler: ref", pass, fail);
    }

    // 58aa. List constructor compiles
    {
        char[] code = compile_to_c3("(List 1 2 3)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"List\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: List", "[FAIL] Compiler: List", pass, fail);
    }
    // 58ab. Dictionary constructors compile through normal callable dispatch.
    {
        char[] code = compile_to_c3("(Dictionary 'a 1 'b 2)", interp);
        bool ok = compiler_code_contains_all(code, {"aot::apply_multi("})
            && compiler_code_contains_none(code, {"aot::dict_from_args(", "unsupported"});
        compiler_test_finish_bool(ok, "Compiler: Dictionary", "[FAIL] Compiler: Dictionary", pass, fail);
    }
    {
        char[] code = compile_to_c3("(Dict 'a 1 'b 2)", interp);
        bool ok = compiler_code_contains_all(code, {"aot::apply_multi("})
            && compiler_code_contains_none(code, {"aot::dict_from_args(", "unsupported"});
        compiler_test_finish_bool(ok, "Compiler: Dict", "[FAIL] Compiler: Dict", pass, fail);
    }
    // 58bb. TimePoint constructor compiles
    {
        char[] code = compile_to_c3("(TimePoint 'date 2026 3 7)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"TimePoint\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: TimePoint", "[FAIL] Compiler: TimePoint", pass, fail);
    }
    // 58c. TimePoint datetime-tz shorthand kind compiles
    {
        char[] code = compile_to_c3("(TimePoint 'datetimetz 2026 3 7 8 15 30 123456 -420)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"TimePoint\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: TimePoint datetimetz", "[FAIL] Compiler: TimePoint datetimetz", pass, fail);
    }
    // 58d. json-get compiles
    {
        char[] code = compile_to_c3("(json-get (json-parse \"{\\\"a\\\":1}\") \"/a\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"json-get\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: json-get", "[FAIL] Compiler: json-get", pass, fail);
    }

    // 59. load compiles
    {
        char[] code = compile_to_c3("(load \"test.omni\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"load\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: load", "[FAIL] Compiler: load", pass, fail);
    }
}

fn void run_compiler_group_missing_primitive_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_missing_numeric_symbol_tests(interp, &pass, &fail);
    run_compiler_group_missing_io_ref_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_qq_macro_primitive_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_quasiquote_macro_tests(interp, pass_out, fail_out);
    run_compiler_group_missing_primitive_tests(interp, pass_out, fail_out);
}
