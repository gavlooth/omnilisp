module lisp;


import std::collections::list;
import std::core::mem;
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

fn Value* compiler_aot_test_exact_pair_sum(void* data, Value* arg_list) {
    usz arg_count = 0;
    Value* count_error = aot::count_arg_list(arg_list, &arg_count);
    if (count_error != null) return count_error;
    if (arg_count < 2) return aot::make_arity_error(2, arg_count);

    Value* curr = arg_list;
    Value* x = aot::car(curr);
    curr = aot::cdr(curr);
    Value* y = aot::car(curr);
    curr = aot::cdr(curr);

    long x_val = x != null && x.tag == ValueTag.INT ? x.int_val : 0;
    long y_val = y != null && y.tag == ValueTag.INT ? y.int_val : 0;
    Value* result = aot::make_int(x_val + y_val);
    if (arg_count > 2) return aot::apply_multi(result, curr, (long)(arg_count - 2));
    return result;
}

struct CompilerAotSumData {
    Value* first;
}

fn bool compiler_error_value_contains(Value* v, char[] needle) {
    return v != null && v.tag == ValueTag.ERROR && str_contains(v.str_chars[:v.str_len], needle);
}

fn Value* compiler_aot_test_sum_second(void* data, Value* arg) {
    CompilerAotSumData* sum_data = (CompilerAotSumData*)data;
    long first = 0;
    if (sum_data != null && sum_data.first != null && sum_data.first.tag == ValueTag.INT) {
        first = sum_data.first.int_val;
    }
    long second = arg != null && arg.tag == ValueTag.INT ? arg.int_val : 0;
    if (sum_data != null) mem::free(sum_data);
    return aot::make_int(first + second);
}

fn Value* compiler_aot_test_capture_first(void* data, Value* arg) {
    CompilerAotSumData* sum_data = (CompilerAotSumData*)mem::malloc(CompilerAotSumData.sizeof);
    if (sum_data == null) return aot::make_error("compiler test: failed to allocate sum payload");
    sum_data.first = arg;
    return aot::make_closure(sum_data, &compiler_aot_test_sum_second);
}

fn Value* compiler_aot_test_signal_my_eff(void* data, Value* arg) {
    return aot::compiled_signal(aot::make_symbol("my-eff"), aot::make_int(42));
}

fn Value* compiler_aot_test_other_eff_handler(void* data, Value* pair) {
    if (pair != null && pair.tag == ValueTag.CONS) return pair.cons_val.cdr;
    return aot::make_nil();
}

fn Value* compiler_aot_test_constant_zero(void* data, Value* arg) {
    return aot::make_int(0);
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
            "(define [ffi module] libc \"libc.so.6\" "
            "  (strlen (^String s)) ^Integer "
            "  (abs (^Integer n)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "// Startup FFI preload")
            && str_contains(code, "aot::ffi_declare_lib(\"libc\", \"libc.so.6\", true, true)")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strlen\", \"strlen\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"abs\", \"abs\"")
            && str_contains(code, "\"format\": \"omni.ffi-contract/v1\"")
            && str_contains(code, "\"symbol\": \"strlen\"")
            && str_contains(code, "\"symbol\": \"abs\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: grouped ffi module lowers through declarative FFI AOT path",
            "[FAIL] Compiler: grouped ffi module lowers through declarative FFI AOT path",
            pass,
            fail
        );
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(define [ffi module] libc \"libc.so.6\" "
                "  (strlen (^String s)) ^Integer "
                "  (abs (^Integer n)) ^Integer)",
                "grouped-ffi.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"schema\": \"omni-ffi-manifest-v1\"")
                        && str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"soname\": \"libc.so.6\"")
                        && str_contains(generated, "\"binding\": \"strlen\"")
                        && str_contains(generated, "\"binding\": \"abs\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: grouped ffi module emits compile sidecar manifest",
                "[FAIL] Compiler: grouped ffi module emits compile sidecar manifest",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: grouped ffi module compose sidecar manifest path");
        }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest_nested-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(block "
                "  (define [ffi module] libc \"libc.so.6\" "
                "    (strlen (^String s)) ^Integer "
                "    (abs (^Integer n)) ^Integer))",
                "grouped-ffi-nested.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"schema\": \"omni-ffi-manifest-v1\"")
                        && str_contains(generated, "\"binding\": \"libc\"")
                        && str_contains(generated, "\"soname\": \"libc.so.6\"")
                        && str_contains(generated, "\"binding\": \"strlen\"")
                        && str_contains(generated, "\"binding\": \"abs\"");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: nested grouped ffi module emits compile sidecar manifest",
                "[FAIL] Compiler: nested grouped ffi module emits compile sidecar manifest",
                pass,
                fail
            );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: nested grouped ffi module compose sidecar manifest path");
    }
}
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_grouped_ffi_manifest_escape-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }
            bool wrote_manifest = main::write_compile_ffi_manifest(
                "(define [ffi module] libc \"libc.so.6\" (strlen (^String s)) ^Integer)",
                "manifest-\b-\f-\v-source.omni",
                manifest_output_path,
                interp,
                &manifest_path,
                &manifest_write_kind
            );
            bool manifest_ok = false;
            if (wrote_manifest && manifest_path.len > 0) {
                if (try generated = io::file::load_temp((String)manifest_path)) {
                    manifest_ok = str_contains(generated, "\"source\": \"manifest-\\b-\\f-\\u000b-source.omni\"")
                        && str_contains(generated, "\"generated_for\": ");
                }
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: grouped ffi manifest escapes control characters in JSON fields",
                "[FAIL] Compiler: grouped ffi manifest escapes control characters in JSON fields",
                pass,
                fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose escaped manifest output path");
        }
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi lib] libc \"libc.so.6\") "
            "(define [ffi λ libc] (strlen (^String s)) ^Integer) "
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Float64 x)) ^Float64)",
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
            && str_contains(code, "\"ForeignHandle\"")
            && str_contains(code, "\"Boolean\"")
            && str_contains(code, "\"Float64\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            "[FAIL] Compiler: declarative ffi emits manifest contract with loading policy and ABI tags",
            pass,
            fail
        );
    }
    {
        Compiler compiler;
        compiler.init(interp);
        defer compiler.free();

        char[4] json_string = { 'a', (char)8, (char)1, '`' };
        compiler.emit_json_string(json_string[..]);
        char[] code = compiler.get_output();
        bool ok = compiler_str_eq(code, "\"a\\b\\u0001\\u0060\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: ffi manifest JSON string emitter escapes control characters",
            "[FAIL] Compiler: ffi manifest JSON string emitter escapes control characters",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle) "
            "(define [ffi λ libc] (isalpha (^Integer c)) ^Boolean) "
            "(define [ffi λ libc] (cos (^Float64 x)) ^Float64)",
            interp
        );
        bool ok = code.len > 0
            && !str_contains(code, "unsupported")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"strchr\", \"strchr\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"isalpha\", \"isalpha\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"cos\", \"cos\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lowers foreign-handle/bool/double ABI tags",
            "[FAIL] Compiler: declarative ffi lowers foreign-handle/bool/double ABI tags",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose}) "
            "(define [ffi λ libc] (fflush (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)",
            interp
        );
        bool ok = code.len > 0
            && str_contains(code, "lisp::aot::AotFfiHandlePolicySpec")
            && str_contains(code, ".name = \"File\";")
            && str_contains(code, ".ownership = 1;")
            && str_contains(code, ".nullability = 1;")
            && str_contains(code, ".finalizer = \"fclose\";")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"fopen\", \"fopen\"")
            && str_contains(code, "aot::ffi_declare_fn(\"libc\", \"fflush\", \"fflush\"")
            && str_contains(code, ", true, true, ")
            && str_contains(code, "&_ffi_return_policy_");
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi lowers ForeignHandle metadata policy descriptors into AOT bridge",
            "[FAIL] Compiler: declarative ffi lowers ForeignHandle metadata policy descriptors into AOT bridge",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3(
            "(define [ffi λ libc] (bad (^{'name File 'ownership owned 'finalizer fclose} file)) ^Integer)",
            interp
        );
        bool ok = code.len == 0;
        compiler_test_finish_bool(
            ok,
            "Compiler: declarative ffi rejects owned ForeignHandle parameter policy in AOT lowering",
            "[FAIL] Compiler: declarative ffi rejects owned ForeignHandle parameter policy in AOT lowering",
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

fn void run_compiler_group_aot_runtime_parity_tests(Interp* interp, usz* pass, usz* fail) {
    bool ok = aot::aot_init();
    if (!ok) {
        (*fail)++;
        io::printn("[FAIL] Compiler: AOT runtime parity test bootstrap");
        return;
    }
    defer aot::aot_shutdown();

    {
        aot::AotMetaSpec[1] meta;
        meta[0].key = "T";
        meta[0].sym_value = "Number";
        meta[0].int_value = 0;
        meta[0].is_int = false;

        aot::AotTypeAnnotationSpec spec = {};
        spec.has_annotation = true;
        spec.is_dict = true;
        spec.base_type = "";
        spec.meta = &meta[0];
        spec.meta_count = 1;

        TypeAnnotation ann;
        bool init_ok = aot::aot_type_annotation_init_from_spec(&ann, &spec);
        bool parity_ok =
            init_ok &&
            ann.has_annotation &&
            ann.is_dict &&
            ann.base_type == (SymbolId)0 &&
            ann.meta_count == 1 &&
            ann.meta != null &&
            ann.meta[0].key != INVALID_SYMBOL_ID &&
            ann.meta[0].sym_value != INVALID_SYMBOL_ID;
        aot::aot_type_annotation_destroy(&ann);

        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT dict metadata annotation materializes without a base type"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT dict metadata annotation materializes without a base type"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20 22)", interp);
        Value* closure = aot::make_closure(null, &compiler_aot_test_capture_first);
        Value* arg_list = aot::cons(aot::make_int(20), aot::cons(aot::make_int(22), aot::make_nil()));
        Value* aot_result = aot::apply_multi(closure, arg_list, 2);
        bool parity_ok =
            !jit_result.error.has_error &&
            jit_result.value != null &&
            jit_result.value.tag == ValueTag.INT &&
            jit_result.value.int_val == 42 &&
            aot_result != null &&
            aot_result.tag == ValueTag.INT &&
            aot_result.int_val == 42;
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT multi-arg closure apply matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT multi-arg closure apply matches JIT"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20)", interp);
        Value* closure = aot::make_variadic_closure(null, &compiler_aot_test_exact_pair_sum);
        Value* aot_result = aot::invoke(closure, aot::make_int(20));
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "arity mismatch: expected 2 argument(s), got 1") &&
            compiler_error_value_contains(aot_result, "arity mismatch: expected 2 argument(s), got 1");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT fixed multi-arg under-application matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT fixed multi-arg under-application matches JIT"); }
    }

    {
        EvalResult jit_result = run("((lambda (x y) (+ x y)) 20 22 1)", interp);
        Value* closure = aot::make_variadic_closure(null, &compiler_aot_test_exact_pair_sum);
        Value* arg_list = aot::cons(aot::make_int(20), aot::cons(aot::make_int(22), aot::cons(aot::make_int(1), aot::make_nil())));
        Value* aot_result = aot::apply_multi(closure, arg_list, 3);
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "not a function") &&
            compiler_error_value_contains(aot_result, "not a function");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT fixed multi-arg over-application matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT fixed multi-arg over-application matches JIT"); }
    }

    {
        EvalResult jit_result = run("(block (define cfg (Dictionary 'tag 42)) (signal cfg.tag 1))", interp);
        Value* aot_result = aot::compiled_signal(aot::make_int(42), aot::make_int(1));
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "effect tag must resolve to symbol") &&
            compiler_error_value_contains(aot_result, "effect tag must resolve to symbol");
        if (parity_ok) { (*pass)++; compiler_print_pass("Compiler: AOT signal invalid-tag path matches JIT"); }
        else           { (*fail)++; io::printn("[FAIL] Compiler: AOT signal invalid-tag path matches JIT"); }
    }

    {
        EvalResult jit_result = run("(handle ^strict (signal my-eff 42) (other-eff v v))", interp);
        Value* strict_tag = aot::make_symbol("other-eff");
        Value* strict_handler = aot::make_closure(null, &compiler_aot_test_other_eff_handler);
        Value* strict_body = aot::make_closure(null, &compiler_aot_test_signal_my_eff);
        Value*[1] tags = { strict_tag };
        Value*[1] handlers = { strict_handler };
        Value* aot_result = aot::compiled_handle(&tags[0], &handlers[0], 1, strict_body, true);
        bool parity_ok =
            jit_result.error.has_error &&
            str_contains(jit_result.error.message[..], "strict handler") &&
            compiler_error_value_contains(aot_result, "strict handler");
        if (parity_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT strict handle boundary matches JIT");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT strict handle boundary matches JIT");
            if (jit_result.error.has_error) io::printfn("  JIT error: %s", (ZString)jit_result.error.message[..]);
            if (aot_result != null && aot_result.tag == ValueTag.ERROR) {
                io::printfn("  AOT error: %s", (ZString)aot_result.str_chars[:aot_result.str_len]);
            } else if (aot_result != null) {
                io::printfn("  AOT result tag: %d", (int)aot_result.tag);
            } else {
                io::printn("  AOT result: nil");
            }
        }
    }

    {
        Value* invalid_tag = aot::make_int(7);
        Value* handler = aot::make_closure(null, &compiler_aot_test_other_eff_handler);
        Value* body = aot::make_closure(null, &compiler_aot_test_constant_zero);
        Value*[1] tags = { invalid_tag };
        Value*[1] handlers = { handler };
        Value* aot_result = aot::compiled_handle(&tags[0], &handlers[0], 1, body, false);
        bool ok_invalid_tag = compiler_error_value_contains(aot_result, "effect tag must resolve to symbol");
        if (ok_invalid_tag) { (*pass)++; compiler_print_pass("Compiler: AOT handle invalid tag fails closed"); }
        else                { (*fail)++; io::printn("[FAIL] Compiler: AOT handle invalid tag fails closed"); }
    }

    {
        Value* libc = aot::ffi_declare_lib("libc_aot_policy", "libc.so.6");
        Interp* aot_interp = aot::aot_interp();

        EvalResult libc_meta = run(
            "(let (m (foreign-describe libc_aot_policy))"
            "  (if (and (= (ref m 'type) 'ForeignHandle)"
            "           (= (ref m 'runtime) 'c-abi)"
            "           (= (ref m 'kind) 'library)"
            "           (= (ref m 'name) \"libc.so.6\")"
            "           (array? (ref m 'capabilities))"
            "           (= (length (ref m 'capabilities)) 3)"
            "           (= (ref (ref m 'capabilities) 0) 'load)"
            "           (= (ref (ref m 'capabilities) 1) 'resolve)"
            "           (= (ref (ref m 'capabilities) 2) 'reflect))"
            "      1"
            "      0))",
            aot_interp);
        bool lib_meta_ok =
            aot_interp != null &&
            !libc_meta.error.has_error &&
            libc_meta.value != null &&
            libc_meta.value.tag == ValueTag.INT &&
            libc_meta.value.int_val == 1;
        if (lib_meta_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ForeignHandle library descriptor exposes load/resolve/reflect only");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ForeignHandle library descriptor exposes load/resolve/reflect only");
        }

        int[2] fopen_tags = { (int)FfiTypeTag.FFI_TYPE_STRING, (int)FfiTypeTag.FFI_TYPE_STRING };
        aot::AotFfiHandlePolicySpec fopen_return_policy;
        fopen_return_policy.is_foreign_handle = true;
        fopen_return_policy.name = "File";
        fopen_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_OWNED;
        fopen_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        fopen_return_policy.finalizer = "fclose";
        Value* fopen_prim = aot::ffi_declare_fn(
            "libc_aot_policy", "fopen_aot_policy", "fopen",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_PTR,
            true, true, null, &fopen_return_policy
        );
        Value* file = null;
        if (fopen_prim != null && fopen_prim.tag == PRIMITIVE) {
            Value* fopen_args = aot::cons(
                aot::make_string("/dev/null"),
                aot::cons(aot::make_string("r"), aot::make_nil())
            );
            file = aot::apply_multi(fopen_prim, fopen_args, 2);
        }
        bool owned_return_ok =
            file != null
            && file.tag == FFI_HANDLE
            && file.ffi_val != null
            && file.ffi_val.finalizer != null
            && compiler_str_eq(file.ffi_val.lib_name[:file.ffi_val.name_len], "File");
        if (owned_return_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle owned return resolves finalizer"); }
        else                 { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle owned return resolves finalizer"); }

        aot::AotFfiHandlePolicySpec bad_return_policy;
        bad_return_policy.is_foreign_handle = true;
        bad_return_policy.name = "File";
        bad_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_MANUAL;
        bad_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        bad_return_policy.finalizer = "fclose";
        Value* bad_manual_return = aot::ffi_declare_fn(
            "libc_aot_policy", "fclose_bad_policy_aot", "fclose",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, null, &bad_return_policy
        );
        bool manual_with_finalizer_rejected = compiler_error_value_contains(
            bad_manual_return,
            "manual ForeignHandle return cannot specify a finalizer"
        );
        if (manual_with_finalizer_rejected) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ffi rejects manual ownership with finalizer in policy");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ffi rejects manual ownership with finalizer in policy");
        }

        aot::AotFfiHandlePolicySpec borrowed_return_policy;
        borrowed_return_policy.is_foreign_handle = true;
        borrowed_return_policy.name = "File";
        borrowed_return_policy.ownership = (int)FfiHandleOwnership.FFI_HANDLE_BORROWED;
        borrowed_return_policy.nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        borrowed_return_policy.finalizer = "fclose";
        Value* borrowed_with_finalizer_rejected = aot::ffi_declare_fn(
            "libc_aot_policy", "fclose_bad_policy_aot_borrowed", "fclose",
            &fopen_tags[0], 2, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, null, &borrowed_return_policy
        );
        bool borrowed_with_finalizer_rejected_ok = compiler_error_value_contains(
            borrowed_with_finalizer_rejected,
            "finalizer-bearing ForeignHandle return must use owned ownership"
        );
        if (borrowed_with_finalizer_rejected_ok) {
            (*pass)++;
            compiler_print_pass("Compiler: AOT ffi rejects borrowed ownership with finalizer in policy");
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: AOT ffi rejects borrowed ownership with finalizer in policy");
        }

        int[1] fflush_tags = { (int)FfiTypeTag.FFI_TYPE_PTR };
        aot::AotFfiHandlePolicySpec[1] fflush_param_policies;
        fflush_param_policies[0].is_foreign_handle = true;
        fflush_param_policies[0].name = "File";
        fflush_param_policies[0].ownership = (int)FfiHandleOwnership.FFI_HANDLE_BORROWED;
        fflush_param_policies[0].nullability = (int)FfiHandleNullability.FFI_HANDLE_NON_NULL;
        fflush_param_policies[0].finalizer = "";
        Value* fflush_prim = aot::ffi_declare_fn(
            "libc_aot_policy", "fflush_aot_policy", "fflush",
            &fflush_tags[0], 1, true, (int)FfiTypeTag.FFI_TYPE_INT,
            true, true, &fflush_param_policies[0], null
        );

        Value* nil_result = null;
        Value* wrong_family_result = null;
        if (fflush_prim != null && fflush_prim.tag == PRIMITIVE) {
            nil_result = aot::apply_multi(fflush_prim, aot::cons(aot::make_nil(), aot::make_nil()), 1);
            wrong_family_result = aot::apply_multi(fflush_prim, aot::cons(libc, aot::make_nil()), 1);
        }
        bool non_null_ok = compiler_error_value_contains(nil_result, "expected non-null ForeignHandle");
        if (non_null_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle non-null policy rejects nil"); }
        else             { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle non-null policy rejects nil"); }

        bool wrong_family_ok = compiler_error_value_contains(wrong_family_result, "expected ForeignHandle File");
        if (wrong_family_ok) { (*pass)++; compiler_print_pass("Compiler: AOT ffi ForeignHandle family policy rejects wrong handle"); }
        else                 { (*fail)++; io::printn("[FAIL] Compiler: AOT ffi ForeignHandle family policy rejects wrong handle"); }
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

fn void run_compiler_group_foreign_handle_metadata_dict_tests(Interp* interp, usz* pass, usz* fail) {
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})",
            "(define [ffi λ libc] (fopen (^String path) (^String mode)) ^{'type ForeignHandle 'name File 'ownership owned 'finalizer fclose})"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves canonical ForeignHandle metadata dict return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)",
            "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^ForeignHandle)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves standalone ForeignHandle return annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves standalone ForeignHandle return annotations"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)",
            "(define [ffi λ libc] (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict parameters"); }
    }
    {
        bool ok = compiler_serializes_to_roundtrip(
            interp,
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)",
            "(define [ffi λ libc] (strtol (^String s) (^{'name File 'ownership borrowed 'nullability non-null} endptr) (^Integer base)) ^Integer)"
        );
        if (ok) { (*pass)++; compiler_print_pass("Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
        else    { (*fail)++; io::printn("[FAIL] Compiler: serializer preserves ForeignHandle metadata dict optional-output annotations"); }
    }
    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_compile_manifest_atomic-%d-%d.c3",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            char[] manifest_path = "";
            main::EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer {
                if (manifest_path.len > 0) {
                    (void)io::file::delete((String)manifest_path);
                    mem::free(manifest_path.ptr);
                }
            }

            char[512] final_manifest_path_buf;
            char[] final_manifest_path;
            if (try final_rendered = io::bprintf(
                &final_manifest_path_buf,
                "%s.ffi-manifest.json",
                (ZString)manifest_output_path
            )) {
                final_manifest_path = final_rendered;
                defer (void)io::file::delete((String)final_manifest_path);

                if (try file = io::file::open((String)final_manifest_path, "w")) {
                    defer (void)file.close();
                    (void)file.write("old manifest");
                }

                main::g_compile_manifest_force_fail_before_rename = true;
                bool wrote_manifest = main::write_compile_ffi_manifest(
                    "(define [ffi module] libc \"libc.so.6\" (strlen (^String s)) ^Integer)",
                    "manifest-\b-\f-\v-source.omni",
                    manifest_output_path,
                    interp,
                    &manifest_path,
                    &manifest_write_kind
                );
                main::g_compile_manifest_force_fail_before_rename = false;

                bool final_manifest_untouched = false;
                if (try generated = io::file::load_temp((String)final_manifest_path)) {
                    final_manifest_untouched = compiler_str_eq(generated, "old manifest");
                }
                compiler_test_finish_bool(
                    !wrote_manifest && final_manifest_untouched,
                    "Compiler: grouped ffi manifest write preserves existing final file on pre-rename failure",
                    "[FAIL] Compiler: grouped ffi manifest write preserves existing final file on pre-rename failure",
                    pass,
                    fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: compose atomic manifest final path");
            }
        } else {
            fail++;
            io::printn("[FAIL] Compiler: compose atomic manifest output path");
        }
    }
}

fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_allocation_failure_seam_tests(interp, &pass, &fail);
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_aot_runtime_parity_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    run_compiler_group_serializer_roundtrip_tests(interp, &pass, &fail);
    run_compiler_group_foreign_handle_metadata_dict_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
