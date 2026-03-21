module lisp;

import std::io;
import std::core::mem;
import main;

extern fn int compiler_test_getpid() @extern("getpid");
usz g_compiler_bindgen_temp_seq = 0;

fn void run_compiler_group_mutable_capture_codegen_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 5: Mutable capture boxing
    // =========================================================================

    // 60. Mutable capture detected and uses aot::define_var
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        // Mutable capture test - checks generated code for interpreter env usage
        bool ok = str_contains(code, "aot::define_var") && str_contains(code, "aot::set_var");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture uses aot::define_var/aot::set_var",
            "[FAIL] Compiler: mutable capture uses aot::define_var/aot::set_var",
            pass,
            fail
        );
    }

    // 61. Mutable capture reads use aot::lookup_var
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        bool ok = str_contains(code, "aot::lookup_var");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture reads use aot::lookup_var",
            "[FAIL] Compiler: mutable capture reads use aot::lookup_var",
            pass,
            fail
        );
    }

    // 62. Non-mutable let uses C3 local variable (Value* x =...)
    {
        char[] code = compile_to_c3("(let (x 42) x)", interp);
        // The main fn body should contain "Value* x =" for a non-mutable let
        bool ok = str_contains(code, "Value* x =");
        compiler_test_finish_bool(
            ok,
            "Compiler: non-mutable let uses C3 local",
            "[FAIL] Compiler: non-mutable let uses C3 local",
            pass,
            fail
        );
    }
}

fn void run_compiler_group_feature_integration_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Integration: Verify all features work together
    // =========================================================================

    // 63. Complex program with multiple features compiles
    {
        char[] code = compile_to_c3(
            "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))) (factorial 10)", interp);
        bool ok = compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(ok, "Compiler: factorial compiles", "[FAIL] Compiler: factorial compiles", pass, fail);
    }

    // 64. Let-rec with tail call emits apply_multi_tail
    {
        char[] code = compile_to_c3(
            "(let ^rec (loop (lambda (n) (if (= n 0) 'done (loop (- n 1))))) (loop 100))", interp);
        bool ok = compiler_code_contains_all(code, {"apply_multi_tail"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: let-rec tail call emits apply_multi_tail",
            "[FAIL] Compiler: let-rec tail call emits apply_multi_tail",
            pass,
            fail
        );
    }

    // 65. Nested multi-arg calls compile correctly
    {
        char[] code = compile_to_c3("(+ (* 2 3) (* 4 5))", interp);
        bool ok = compiler_code_contains_all(code, {"invoke"})
            && compiler_code_contains_none(code, {"unsupported"});
        compiler_test_finish_bool(
            ok,
            "Compiler: nested multi-arg calls",
            "[FAIL] Compiler: nested multi-arg calls",
            pass,
            fail
        );
    }
}

fn void run_compiler_group_mutable_integration_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_mutable_capture_codegen_tests(interp, &pass, &fail);
    run_compiler_group_feature_integration_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_iterative_stdlib_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;

    // =========================================================================
    // Phase 6: STDLIB_PRELUDE iterative map/filter
    // =========================================================================

    // 66. STDLIB_PRELUDE map uses iterative loop pattern
    {
        char[] code = compile_to_c3("(map (lambda (x) x) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        compiler_test_finish_bool(
            ok,
            "Compiler: STDLIB map uses loop pattern",
            "[FAIL] Compiler: STDLIB map uses loop pattern",
            &pass,
            &fail
        );
    }

    // 67. STDLIB_PRELUDE filter uses iterative loop pattern
    {
        char[] code = compile_to_c3("(filter (lambda (x) x) (list 1))", interp);
        bool ok = str_contains(code, "loop");
        compiler_test_finish_bool(
            ok,
            "Compiler: STDLIB filter uses loop pattern",
            "[FAIL] Compiler: STDLIB filter uses loop pattern",
            &pass,
            &fail
        );
    }

    run_compiler_group_bindgen_canonical_type_tests(interp, &pass, &fail);

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_mutable_integration_iterative_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_mutable_integration_tests(interp, pass_out, fail_out);
    run_compiler_group_iterative_stdlib_tests(interp, pass_out, fail_out);
}

fn bool compiler_write_text_file(char[] path, char[] text) {
    if (try file = io::file::open((String)path, "w")) {
        defer (void)file.close();
        file.write(text)!!;
        return true;
    }
    return false;
}

fn void run_compiler_group_bindgen_canonical_type_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    LibclangHandle clang;
    if (!libclang_init(&clang)) {
        pass++;
        compiler_print_pass("Compiler: bindgen canonical types skipped (libclang unavailable)");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer libclang_destroy(&clang);

    char[512] header_path_buf;
    char[512] output_path_buf;
    int pid = compiler_test_getpid();
    usz seq = g_compiler_bindgen_temp_seq++;
    char[] header_path = io::bprintf(
        &header_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d.h",
        pid,
        (int)seq
    )!!;
    char[] output_path = io::bprintf(
        &output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d.omni",
        pid,
        (int)seq
    )!!;
    char[] header_source =
        "int omni_bindgen_int(int x);\n"
        "void omni_bindgen_void_ptr(void* p);\n"
        "void* omni_bindgen_ret_ptr(void);\n"
        "void omni_bindgen_void_ret(void);\n";

    if (!compiler_write_text_file(header_path, header_source)) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types write header");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer (void)io::file::delete((String)header_path);
    defer (void)io::file::delete((String)output_path);

    const usz MAX_FUNCS = 32;
    ParsedFunc* funcs = (ParsedFunc*)mem::malloc(ParsedFunc.sizeof * MAX_FUNCS);
    if (funcs == null) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types alloc funcs");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer mem::free(funcs);
    for (usz i = 0; i < MAX_FUNCS; i++) {
        funcs[i].params = null;
        funcs[i].param_count = 0;
        funcs[i].param_capacity = 0;
        funcs[i].is_variadic = false;
    }
    defer {
        for (usz i = 0; i < MAX_FUNCS; i++) {
            if (funcs[i].params != null) mem::free(funcs[i].params);
        }
    }

    usz func_count = parse_header(&clang, header_path, funcs, MAX_FUNCS, null, 0, false);
    bool parsed = func_count >= 4;
    if (!parsed) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types parse header");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    bool generated_ok = generate_ffi_module("bindgen-canonical", "bindgen", funcs, func_count, output_path);
    if (!generated_ok) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types generate module");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    if (try generated = io::file::load_temp((String)output_path)) {
        bool has_integer = str_contains(generated, "(^Integer arg0)");
        bool has_pointer = str_contains(generated, "(^Pointer arg0)");
        bool has_void = str_contains(generated, ") ^Void)");
        bool has_ret_pointer = str_contains(generated, ") ^Pointer)");
        bool ok = has_integer && has_pointer && has_void && has_ret_pointer;
        if (ok) {
            pass++;
            compiler_print_pass("Compiler: bindgen canonical Integer/Pointer/Void annotations");
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen canonical Integer/Pointer/Void annotations");
        }
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types load generated module");
    }

    *pass_out += pass;
    *fail_out += fail;
}
