module lisp;


import std::collections::list;
import std::io;
import main;
import lisp::aot;

fn bool compiler_str_eq(char[] left, char[] right) {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool compiler_serializes_to_roundtrip(Interp* interp, char[] source, char[] expected) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);
    Expr* expr = p.parse_expr();
    if (expr == null || p.has_error) {
        io::printfn("[DEBUG] serializer source parse failed: %s", (ZString)source);
        return false;
    }

    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();

    List{char} buf;
    defer buf.free();
    compiler.serialize_expr_to_buf(expr, &buf);

    char[] actual = "";
    if (buf.len() > 0) {
        actual = buf.entries[:buf.len()];
    }
    if (!compiler_str_eq(actual, expected)) {
        io::print("[DEBUG] serializer expected: ");
        io::printn(expected);
        io::print("[DEBUG] serializer actual:   ");
        io::printn(actual);
        return false;
    }

    Expr* reparsed = parse(actual, interp);
    if (reparsed == null) {
        io::printfn("[DEBUG] serializer reparse failed: %s", (ZString)actual);
        return false;
    }
    return true;
}

fn Value* compiler_aot_test_unary_invoke(void* data, Value* arg) {
    return arg != null ? arg : aot::make_nil();
}

fn Value* compiler_aot_test_variadic_invoke(void* data, Value* arg_list) {
    return arg_list != null ? arg_list : aot::make_nil();
}

fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BLOCK tests ---
    {
        char[] code = compile_to_c3("(block 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block 42)", "[FAIL] Compiler: (block 42)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        compiler_test_finish_bool(ok, "Compiler: (block 1 2 3)", "[FAIL] Compiler: (block 1 2 3)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block (define x 1) x)", "[FAIL] Compiler: (block (define x 1) x)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(lambda (x) (block x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (lambda (x) (block x x))", "[FAIL] Compiler: (lambda (x) (block x x))", pass, fail);
    }

    // --- Zero-arg lambda tests ---
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        compiler_test_finish_bool(ok, "Compiler: ((lambda () 42))", "[FAIL] Compiler: ((lambda () 42))", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg define + call", "[FAIL] Compiler: zero-arg define + call", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (f (lambda () 10)) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg in let", "[FAIL] Compiler: zero-arg in let", pass, fail);
    }
}

fn void run_compiler_group_syntax_multiarg_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Multi-param + multi-binding (parser desugar) ---
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-param lambda", "[FAIL] Compiler: multi-param lambda", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 1 y 2) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-binding let", "[FAIL] Compiler: multi-binding let", pass, fail);
    }
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: 3-param lambda", "[FAIL] Compiler: 3-param lambda", pass, fail);
    }
}

fn void run_compiler_group_syntax_basics(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_syntax_begin_zero_tests(interp, &pass, &fail);
    run_compiler_group_syntax_multiarg_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_stdlib_availability_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Stdlib availability ---
    {
        char[] code = compile_to_c3("(map (lambda (x) (+ x 1)) (list 1))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: map", "[FAIL] Compiler: map", pass, fail);
    }
    {
        char[] code = compile_to_c3("(filter (lambda (x) (> x 2)) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: filter", "[FAIL] Compiler: filter", pass, fail);
    }
    {
        char[] code = compile_to_c3("(foldl + 0 (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: foldl", "[FAIL] Compiler: foldl", pass, fail);
    }
}


fn void run_compiler_group_existing_feature_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Existing features still work ---
    {
        char[] code = compile_to_c3("(match (list 1 2 3) ([a b c] (+ a c)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: match", "[FAIL] Compiler: match", pass, fail);
    }
    {
        char[] code = compile_to_c3("(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: handle/perform", "[FAIL] Compiler: handle/perform", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 42) (let (f (lambda () x)) (f)))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg closure capture", "[FAIL] Compiler: zero-arg closure capture", pass, fail);
    }
    {
        char[] code = compile_to_c3("(id 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: id", "[FAIL] Compiler: id", pass, fail);
    }
    {
        char[] code = compile_to_c3("(range 5)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: range", "[FAIL] Compiler: range", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define [ffi lib] libc \"libc.so.6\")", interp);
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\")")
            && !str_contains(code, "not supported by AOT lowering");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lib lowers through AOT runtime bridge",
            "[FAIL] Compiler: declarative ffi lib lowers through AOT runtime bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define [ffi λ libc] (strlen (^String s)) ^Integer)", interp);
        bool ok = code.len > 0
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && !str_contains(code, "not supported by AOT lowering");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi function lowers through AOT runtime bridge",
            "[FAIL] Compiler: declarative ffi function lowers through AOT runtime bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (strlen (^String s)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "// Startup FFI preload")
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && str_contains(code, ", true, true);");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi preload emits explicit load policy",
            "[FAIL] Compiler: declarative ffi preload emits explicit load policy",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (strlen (^String s)) ^Integer) "
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^Pointer) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Double x)) ^Double)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "const char[] __omni_ffi_contract_json = `")
            && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
            && str_contains(code, "\"libraries\": [")
            && str_contains(code, "\"functions\": [")
            && str_contains(code, "\"binding\": \"libc\"")
            && str_contains(code, "\"soname\": \"libc.so.6\"")
            && str_contains(code, "\"eager_load\": true")
            && str_contains(code, "\"required_library\": true")
            && str_contains(code, "\"symbol\": \"strlen\"")
            && str_contains(code, "\"eager_resolve\": true")
            && str_contains(code, "\"required_symbol\": true")
            && str_contains(code, "\"params\": [")
            && str_contains(code, "\"String\"")
            && str_contains(code, "\"Pointer\"")
            && str_contains(code, "\"Boolean\"")
            && str_contains(code, "\"Double\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            "[FAIL] Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^Pointer) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Double x)) ^Double)",
            interp
        );
        bool ok = code.len > 0
            && !str_contains(code, "unsupported")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strchr\", \"strchr\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"isalpha\", \"isalpha\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"cos\", \"cos\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lowers pointer/bool/double ABI tags",
            "[FAIL] Compiler: declarative ffi lowers pointer/bool/double ABI tags",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define soname \"libc.so.6\") (define [ffi lib] libc soname)", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lib fails closed for non-literal soname in AOT mode",
            "[FAIL] Compiler: declarative ffi lib fails closed for non-literal soname in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(define [ffi λ libc variadic] (printf (^String fmt)) ^Integer)", interp);
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative variadic ffi function is rejected in AOT mode",
            "[FAIL] Compiler: declarative variadic ffi function is rejected in AOT mode",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(signal ui.open 1)",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified signal lowers directly",
            "[FAIL] Compiler: module-qualified signal lowers directly",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(module ui (export open) (define [effect] (open (^Any payload)))) "
            "(import ui) "
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            interp
        );
        bool ok = str_contains(code, "aot::compiled_signal(")
            && str_contains(code, "aot::compiled_handle(")
            && !str_contains(code, "aot::eval_serialized_expr")
            && !str_contains(code, "unsupported expr type");
        compiler_test_finish_bool(
            ok,
            "Compiler: module-qualified handle lowers directly",
            "[FAIL] Compiler: module-qualified handle lowers directly",
            pass,
            fail
        );
    }
}


fn void run_compiler_group_serializer_roundtrip_tests(Interp* interp, usz* pass, usz* fail) {
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))",
            "(import \"pkg/math.omni\" ((sum 'as plus) diff))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: import serializer preserves path and alias syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: import serializer preserves path and alias syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (+ 1 (signal ask 0)) (ask x (resolve x)))",
            "(handle (+ 1 (perform ask 0)) (ask x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: handle serializer emits parser-compatible clause syntax"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: handle serializer emits parser-compatible clause syntax"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))",
            "(match (list 1 2) ([a b] (+ a b)) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves clause parens"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves clause parens"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (cons 1 2) ((a . b) a) (_ 0))",
            "(match (cons 1 2) ((a . b) a) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves cons patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves cons patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Dictionary 'name 7) ({name} name) (_ 0))",
            "(match (Dictionary 'name 7) ({name} name) (_ 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves dict patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves dict patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(match (Some 1) ((Some x) x) (None 0))",
            "(match (Some 1) ((Some x) x) (None 0))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: match serializer preserves constructor patterns"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: match serializer preserves constructor patterns"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(signal ui.open 1)",
            "(perform ui.open 1)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified signal tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified signal tag paths"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(handle (signal ui.open 1) (ui.open x (resolve x)))",
            "(handle (perform ui.open 1) (ui.open x (resolve x)))"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves module-qualified handle tag paths"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves module-qualified handle tag paths"); }
    }
}

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_allocation_failure_seam_tests(interp, &pass, &fail);
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    run_compiler_group_serializer_roundtrip_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
