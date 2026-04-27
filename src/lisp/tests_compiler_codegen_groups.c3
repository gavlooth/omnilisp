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
        bool ok = compiler_code_contains_all(code, {"captured_x", "_capture_source_", "captured_x = _capture_source_", "match_guard_eval"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: callable match guard captures clause-local bindings",
            "[FAIL] Compiler: callable match guard captures clause-local bindings",
            &pass,
            &fail
        );
    }

    // 44d. Unknown internal pattern tags must not compile as catch-all matches.
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.compile_pattern_check_to_bool(&invalid, "_value", "_match_ok");
        char[] code = compiler.get_output();
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in AOT match lowering") &&
            str_contains(code, "_match_ok = false") &&
            !str_contains(code, "_match_ok = true");
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT match rejects unknown pattern tags",
            "[FAIL] Compiler: AOT match rejects unknown pattern tags",
            &pass,
            &fail
        );
    }

    // 44e. Unknown internal pattern tags must not silently skip binding lowering.
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        compiler.compile_pattern_bindings(&invalid, "_value");
        char[] code = compiler.get_output();
        bool ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in AOT match binding lowering") &&
            code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT match binding rejects unknown pattern tags",
            "[FAIL] Compiler: AOT match binding rejects unknown pattern tags",
            &pass,
            &fail
        );
    }

    // 44f. Unknown internal pattern tags must not silently skip guard scan/lowering.
    {
        Pattern invalid = {};
        char* raw = (char*)&invalid;
        raw[0] = (char)127;

        Compiler scanner;
        scanner.init(interp);
        defer scanner.free();
        bool has_guard = scanner.pattern_has_guard(&invalid);
        bool scan_ok = scanner.has_error &&
            !has_guard &&
            str_contains(scanner.error_message, "unknown pattern tag in AOT match guard scan");

        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();
        compiler.compile_pattern_guards(&invalid, "_value", 0, "_matched", "_ready");
        char[] code = compiler.get_output();
        bool lower_ok = compiler.has_error &&
            str_contains(compiler.error_message, "unknown pattern tag in AOT match guard lowering") &&
            code.len == 0;

        compiler_test_finish_bool(
            scan_ok && lower_ok,
            "Compiler: AOT match guards reject unknown pattern tags",
            "[FAIL] Compiler: AOT match guards reject unknown pattern tags",
            &pass,
            &fail
        );
    }

    // 44d. Syntactic resolve exits handler clauses before generated parent work.
    {
        char[] code = compile_to_c3("(handle (signal e 0) (e v (+ (resolve 1) 2)))", interp);
        bool ok = compiler_code_contains_ordered(code, {
                "aot::compiled_resolve(",
                "aot::sequence_should_stop(",
                "return _r"
            }) &&
            compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: resolve terminal guard in call lowering",
            "[FAIL] Compiler: resolve terminal guard in call lowering",
            &pass,
            &fail
        );
    }

    {
        char[] code = compile_to_c3("(handle (signal e 0) (e v (match 0 (0 (resolve 42)))))", interp);
        bool ok = compiler_code_contains_ordered(code, {
                "aot::compiled_resolve(",
                "aot::sequence_should_stop(",
                "return _r"
            }) &&
            compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: resolve terminal guard in match lowering",
            "[FAIL] Compiler: resolve terminal guard in match lowering",
            &pass,
            &fail
        );
    }

    // 44d. Registered primitives in stdlib/user lambdas are looked up, not captured as C3 locals.
    {
        char[] code = compile_to_c3("(let (f (lambda (x) (block (error? x) Dict json-parse sin string-byte-length sort-by))) f)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"error?\")", "lookup_prim(\"Dict\")", "lookup_prim(\"json-parse\")", "lookup_prim(\"sin\")", "lookup_prim(\"string-byte-length\")", "lookup_prim(\"sort-by\")"})
            && compiler_code_contains_none(code, {"captured_error", "captured_error_p", "captured_Dict", "captured_json_parse", "captured_sin", "captured_string_byte_length", "captured_sort_by", " = error;", " = error_p;", " = Dict;", " = json_parse;", " = sin;", " = string_byte_length;", " = sort_by;"});
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

    // 45a. Tail calls remain in tail position when a body is implicitly wrapped.
    {
        char[] code = compile_to_c3("(define (f x) (+ x 0) (f (+ x 1)))", interp);
        bool ok = str_contains(code, "apply_multi_tail");
        compiler_test_finish_bool(
            ok,
            "Compiler: implicit block tail call emits apply_multi_tail",
            "[FAIL] Compiler: implicit block tail call emits apply_multi_tail",
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

    {
        char[96] source;
        for (usz i = 0; i < 66; i++) source[i] = '`';
        source[66] = 'x';
        source[67] = 0;
        char[] error = "";
        char[] code = compile_to_c3(source[:67], interp, &error);
        bool ok = code.len == 0 &&
            str_contains(error, "quasiquote nesting too deep (max 64)") &&
            !str_contains(code, "_r184467");
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT quasiquote nesting depth fails closed",
            "[FAIL] Compiler: AOT quasiquote nesting depth fails closed",
            &pass,
            &fail
        );
    }

    {
        char[96] source;
        source[0] = '`';
        source[1] = '(';
        for (usz i = 0; i < 66; i++) source[i + 2] = '`';
        source[68] = 'x';
        source[69] = ' ';
        source[70] = 'y';
        source[71] = ' ';
        source[72] = 'z';
        source[73] = ')';
        source[74] = 0;
        char[] error = "";
        char[] code = compile_to_c3(source[:74], interp, &error);
        bool ok = code.len == 0 &&
            str_contains(error, "quasiquote nesting too deep (max 64)") &&
            !str_contains(code, "_r184467");
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT quasiquote call nesting propagates failure",
            "[FAIL] Compiler: AOT quasiquote call nesting propagates failure",
            &pass,
            &fail
        );
    }

    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr expr = {};
        expr.tag = E_UNQUOTE_SPLICING;

        usz result = compiler.compile_qq_flat(&expr, 0);
        bool ok = result == usz.max &&
            compiler.has_error &&
            str_contains(compiler.error_message, "unquote-splicing is only valid in quasiquote list context");
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT quasiquote standalone splice fails closed",
            "[FAIL] Compiler: AOT quasiquote standalone splice fails closed",
            &pass,
            &fail
        );
    }

    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        Expr expr = {};
        expr.tag = E_IF;

        usz result = compiler.compile_qq_flat(&expr, 0);
        bool ok = result == usz.max &&
            compiler.has_error &&
            str_contains(compiler.error_message, "unsupported quasiquote template expression");
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT quasiquote unsupported template fails closed",
            "[FAIL] Compiler: AOT quasiquote unsupported template fails closed",
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

    // 51. Stdlib when/unless macro surface accepts variadic bodies.
    {
        char[] code = compile_to_c3("(block (when true 1 2) (unless false 3 4))", interp);
        bool ok = compiler_code_contains_all(code, {"make_int(2)", "make_int(4)"}) &&
                  compiler_code_contains_none(code, {"unsupported", "called value is Integer"});
        compiler_test_finish_bool(
            ok,
            "Compiler: stdlib when/unless macros compile variadic bodies",
            "[FAIL] Compiler: stdlib when/unless macros compile variadic bodies",
            &pass,
            &fail
        );
    }

    // 51a. Stdlib macro parity expands before AOT analysis/lowering.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (branch (false 1) (true 2) (_ 3)) "
            "  (let ({name} {'name nil}) (with-defaults name \"anon\" name)) "
            "  (checkpoint (stream-yield 9)))",
            interp
        );
        bool ok = compiler_code_contains_all(code, {"make_int(2)", "make_int(9)"}) &&
                  compiler_code_contains_none(code, {"branch(", "with_defaults", "stream_yield", "unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: stdlib branch/default/stream macros expand before lowering",
            "[FAIL] Compiler: stdlib branch/default/stream macros expand before lowering",
            &pass,
            &fail
        );
    }

    // 51b. Compiler macro expansion does not publish compile-only macros.
    {
        SymbolId temp_macro = interp.symbols.intern("aot-temp-macro");
        usz before_idx = lookup_macro_index(temp_macro, interp);
        char[] code = compile_to_c3(
            "(define [macro] aot-temp-macro (syntax-match ([] (template 42)))) (aot-temp-macro)",
            interp
        );
        usz after_idx = lookup_macro_index(temp_macro, interp);
        bool ok = before_idx == after_idx &&
                  compiler_code_contains_all(code, {"make_int(42)"}) &&
                  compiler_code_contains_none(code, {"aot_temp_macro", "unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: AOT macro expansion state is isolated",
            "[FAIL] Compiler: AOT macro expansion state is isolated",
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

    // 56a. Data reader primitives resolve through the AOT primitive table.
    {
        char[] code = compile_to_c3("(block (hex \"ff0a\") (uuid \"550e8400-e29b-41d4-a716-446655440000\"))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"hex\")", "lookup_prim(\"uuid\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: hex/uuid data primitive AOT lookup", "[FAIL] Compiler: hex/uuid data primitive AOT lookup", pass, fail);
    }

    // 56b. Reader data tags remain ordinary calls before AOT primitive lookup.
    {
        char[] code = compile_to_c3("(block #hex \"ff0a\" #uuid \"550e8400-e29b-41d4-a716-446655440000\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"hex\")", "lookup_prim(\"uuid\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: #hex/#uuid reader tags use AOT primitive lookup", "[FAIL] Compiler: #hex/#uuid reader tags use AOT primitive lookup", pass, fail);
    }

    // 56c. Runtime primitive families remain in AOT primitive resolver parity.
    {
        char[] code = compile_to_c3(
            "(block "
            "  foreign-release "
            "  ffi-callback "
            "  runtime-memory-stats "
            "  deduce/query "
            "  __raw-http-get "
            "  __raw-tcp-read "
            "  __raw-tls-connect "
            "  __iterator-range-from "
            "  __ui-ftxui-session-read-event)",
            interp
        );
        bool ok = compiler_code_contains_all(code, {
                "lookup_prim(\"foreign-release\")",
                "lookup_prim(\"ffi-callback\")",
                "lookup_prim(\"runtime-memory-stats\")",
                "lookup_prim(\"deduce/query\")",
                "lookup_prim(\"__raw-http-get\")",
                "lookup_prim(\"__raw-tcp-read\")",
                "lookup_prim(\"__raw-tls-connect\")",
                "lookup_prim(\"__iterator-range-from\")",
                "lookup_prim(\"__ui-ftxui-session-read-event\")"
            }) &&
            compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: runtime primitive family AOT resolver parity",
            "[FAIL] Compiler: runtime primitive family AOT resolver parity",
            pass,
            fail
        );
    }

    // 56d. Pure-Lisp stdlib functions are present in AOT prelude globals.
    {
        char[] code = compile_to_c3(
            "(block "
            "  (find (lambda (x) (= x 2)) (list 1 2 3)) "
            "  (partition (lambda (x) (> x 1)) (list 1 2 3)) "
            "  (default nil 9) "
            "  (force (delay (lambda (ignored) 42))) "
            "  (stream-take 2 (checkpoint (block (stream-yield 1) (stream-yield 2) nil))))",
            interp
        );
        bool ok = compiler_code_contains_all(code, {
                "Value* find;",
                "Value* partition;",
                "Value* _omni_sym_default_",
                "Value* delay;",
                "Value* force;",
                "Value* _omni_sym_stream_take_"
            }) &&
            compiler_code_contains_none(code, {
                "unsupported",
                "find(",
                "partition(",
                "default(",
                "delay(",
                "force(",
                "stream_take("
            });
        compiler_test_finish_bool(
            ok,
            "Compiler: pure-Lisp stdlib function parity globals",
            "[FAIL] Compiler: pure-Lisp stdlib function parity globals",
            pass,
            fail
        );
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
    {
        char[] code = compile_to_c3("(ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 0.1) (Dictionary) (Dictionary) (Dictionary))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/optimizer-step\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/optimizer-step", "[FAIL] Compiler: ml/optimizer-step", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/clip-gradients (Dictionary) 1.0)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/clip-gradients\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/clip-gradients", "[FAIL] Compiler: ml/clip-gradients", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/plot [1 2 3])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/plot\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/plot", "[FAIL] Compiler: ml/plot", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/loss-curve [1.0 0.5 0.25])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/loss-curve\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/loss-curve", "[FAIL] Compiler: ml/loss-curve", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/scatter [[0 1] [1 3]])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/scatter\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/scatter", "[FAIL] Compiler: ml/scatter", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/bar-chart [1 2 3])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/bar-chart\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/bar-chart", "[FAIL] Compiler: ml/bar-chart", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/histogram [1 2 3])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/histogram\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/histogram", "[FAIL] Compiler: ml/histogram", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/heatmap [[1 2] [3 4]])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/heatmap\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/heatmap", "[FAIL] Compiler: ml/heatmap", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/roc-curve [[0.0 0.0] [1.0 1.0]])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/roc-curve\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/roc-curve", "[FAIL] Compiler: ml/roc-curve", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/pr-curve [[0.0 1.0] [1.0 0.5]])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/pr-curve\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/pr-curve", "[FAIL] Compiler: ml/pr-curve", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/tensor-summary (Tensor Float32 [2] [1 2]))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/tensor-summary\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/tensor-summary", "[FAIL] Compiler: ml/tensor-summary", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/confusion-matrix [0 1] [0 1])", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/confusion-matrix\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/confusion-matrix", "[FAIL] Compiler: ml/confusion-matrix", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/export-image (Tensor Float32 [1 1] [1]) \"/tmp/omni_compiler_export.ppm\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/export-image\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/export-image", "[FAIL] Compiler: ml/export-image", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/save-optimizer (Dictionary 'kind 'sgd 'learning-rate 0.1) (Dictionary))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/save-optimizer\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/save-optimizer", "[FAIL] Compiler: ml/save-optimizer", pass, fail);
    }
    {
        char[] code = compile_to_c3("(ml/load-optimizer \"{}\")", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"ml/load-optimizer\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: ml/load-optimizer", "[FAIL] Compiler: ml/load-optimizer", pass, fail);
    }
    {
        char[] code = compile_to_c3("(nn/forward model input)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"nn/forward\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: nn/forward", "[FAIL] Compiler: nn/forward", pass, fail);
    }
    {
        char[] code = compile_to_c3("(nn/grad model input targets)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"nn/grad\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: nn/grad", "[FAIL] Compiler: nn/grad", pass, fail);
    }
    {
        char[] code = compile_to_c3("(nn/train-step model input targets opt state)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"nn/train-step\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: nn/train-step", "[FAIL] Compiler: nn/train-step", pass, fail);
    }
    {
        char[] code = compile_to_c3("(nn/adam 0.001 (Dictionary 'beta1 0.9))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"nn/adam\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: nn/adam", "[FAIL] Compiler: nn/adam", pass, fail);
    }
    {
        char[] code = compile_to_c3("(Kernel (Dictionary 'backend 'vulkan 'operation 'elementwise 'inputs (Array) 'outputs (Array (Dictionary 'name 'out 'dtype 'Float32 'shape (Array 'n)))))", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"Kernel\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: Kernel", "[FAIL] Compiler: Kernel", pass, fail);
    }
    {
        char[] code = compile_to_c3("(kernel/run k inputs push)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"kernel/run\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: kernel/run", "[FAIL] Compiler: kernel/run", pass, fail);
    }
    {
        char[] code = compile_to_c3("(kernel/capture k inputs push)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"kernel/capture\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: kernel/capture", "[FAIL] Compiler: kernel/capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(tensor/capture source)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"tensor/capture\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: tensor/capture", "[FAIL] Compiler: tensor/capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(tensor/run graph)", interp);
        bool ok = compiler_code_contains_all(code, {"lookup_prim(\"tensor/run\")"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: tensor/run", "[FAIL] Compiler: tensor/run", pass, fail);
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
