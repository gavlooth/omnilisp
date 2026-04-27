module lisp;

import std::io;
import std::core::mem;
import main;

extern fn int compiler_test_getpid() @extern("getpid");
extern fn int compiler_test_mkdir(char* path, uint mode) @extern("mkdir");
extern fn int compiler_test_rmdir(char* path) @extern("rmdir");
usz g_compiler_bindgen_temp_seq = 0;

fn void run_compiler_group_mutable_capture_codegen_tests(Interp* interp, usz* pass, usz* fail) {
    // =========================================================================
    // Phase 5: Mutable capture boxing
    // =========================================================================

    // 60. Mutable capture detected and uses root-owned AOT cells
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        bool ok = compiler_code_contains_all(code, {
            "aot::AotMutableCell* _aot_cell_",
            "aot::make_mutable_cell_checked(",
            "aot::mutable_cell_set_checked(",
            "aot::mutable_cell_get("
        }) && compiler_code_contains_none(code, {
            "aot::define_var(\"x\"",
            "aot::lookup_var(\"x\"",
            "aot::set_var(\"x\""
        });
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture uses AOT mutable cells",
            "[FAIL] Compiler: mutable capture uses AOT mutable cells",
            pass,
            fail
        );
    }

    // 61. Mutable capture reads use the captured cell, not source-name lookup
    {
        char[] code = compile_to_c3("(let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x))", interp);
        bool ok = str_contains(code, "aot::mutable_cell_get(")
            && !str_contains(code, "aot::lookup_var(\"x\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture reads use mutable cells",
            "[FAIL] Compiler: mutable capture reads use mutable cells",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(let (x 0) (let (make (lambda () (handle (+ 1 (signal ask 0)) (ask v (lambda () (set! x (+ x 1))))))) (make) x))", interp);
        bool ok = str_contains(code, "aot::make_mutable_cell_checked(")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && !str_contains(code, "aot::define_var(\"x\"")
            && !str_contains(code, "aot::set_var(\"x\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture through handle body uses boxing",
            "[FAIL] Compiler: mutable capture through handle body uses boxing",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(let (x 0) (let (make (lambda () (handle (+ 1 (signal ask 0)) (ask v (resolve (lambda () (set! x (+ x 1)))))))) (make) x))", interp);
        bool ok = str_contains(code, "aot::make_mutable_cell_checked(")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && !str_contains(code, "aot::define_var(\"x\"")
            && !str_contains(code, "aot::set_var(\"x\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture through resolve value uses boxing",
            "[FAIL] Compiler: mutable capture through resolve value uses boxing",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(let (x 0) (block (checkpoint (let (f (lambda () (set! x (+ x 1)))) (f))) x))", interp);
        bool ok = str_contains(code, "lisp::Value* captured_x")
            && !str_contains(code, "aot::AotMutableCell* captured_x")
            && !str_contains(code, "aot::define_var(\"x\"")
            && !str_contains(code, "aot::set_var(\"x\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture through checkpoint body keeps outer direct",
            "[FAIL] Compiler: mutable capture through checkpoint body keeps outer direct",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(let (x 0) (block (checkpoint (capture k (block (let (f (lambda () (set! x (+ x 1)))) (f)) (k 0)))) x))", interp);
        bool ok = str_contains(code, "lisp::Value* captured_x")
            && !str_contains(code, "aot::AotMutableCell* captured_x")
            && !str_contains(code, "aot::define_var(\"x\"")
            && !str_contains(code, "aot::set_var(\"x\"");
        compiler_test_finish_bool(
            ok,
            "Compiler: mutable capture through capture body keeps outer direct",
            "[FAIL] Compiler: mutable capture through capture body keeps outer direct",
            pass,
            fail
        );
    }
    {
        char[] code = compile_to_c3("(checkpoint (let (x 0) (block (let (f (lambda () (set! x (+ x 1)))) (f)) x)))", interp);
        bool ok = str_contains(code, "aot::make_mutable_cell_checked(")
            && str_contains(code, "aot::mutable_cell_set_checked(")
            && str_contains(code, "aot::mutable_cell_get(");
        compiler_test_finish_bool(
            ok,
            "Compiler: checkpoint-local mutable capture still uses boxing",
            "[FAIL] Compiler: checkpoint-local mutable capture still uses boxing",
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
    run_compiler_group_bindgen_parse_guard_tests(interp, &pass, &fail);
    run_compiler_group_bindgen_path_guard_tests(interp, &pass, &fail);
    run_compiler_group_bindgen_overflow_guard_tests(interp, &pass, &fail);

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_mutable_integration_iterative_tests(Interp* interp, usz* pass_out, usz* fail_out) {
    run_compiler_group_mutable_integration_tests(interp, pass_out, fail_out);
    run_compiler_group_iterative_stdlib_tests(interp, pass_out, fail_out);
}

fn bool compiler_write_text_file(char[] path, char[] text) {
    if (try file = io::file::open((String)path, "w")) {
        char[] remaining = text;
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        return write_ok;
    }
    return false;
}

fn void compiler_fill_bindgen_name(char[128]* name_buf, usz idx) {
    char[] prefix = "overflow_bindgen_function_name_";
    usz pos = 0;
    for (usz i = 0; i < prefix.len && pos < name_buf.len - 1; i++) {
        (*name_buf)[pos++] = prefix[i];
    }

    usz digit_count = 0;
    usz value = idx;
    if (value == 0) {
        digit_count = 1;
    } else {
        usz tmp = value;
        while (tmp > 0) {
            digit_count++;
            tmp /= 10;
        }
    }

    for (usz pad = pos + digit_count; pad < name_buf.len - 1; pad++) {
        (*name_buf)[pos++] = 'x';
    }

    if (digit_count == 1 && value == 0) {
        (*name_buf)[pos++] = '0';
    } else {
        char[32] digits;
        usz dpos = 0;
        while (value > 0 && dpos < digits.len) {
            digits[dpos++] = (char)('0' + (value % 10));
            value /= 10;
        }
        while (dpos > 0 && pos < name_buf.len - 1) {
            (*name_buf)[pos++] = digits[--dpos];
        }
    }

    (*name_buf)[pos] = 0;
}

fn void compiler_fill_bindgen_return_type(char[16]* type_buf) {
    char[] text = "Integer";
    usz pos = 0;
    for (; pos < text.len && pos < type_buf.len - 1; pos++) {
        (*type_buf)[pos] = text[pos];
    }
    (*type_buf)[pos] = 0;
}

fn void run_compiler_group_bindgen_canonical_type_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    if (!compiler_bindgen_toml_surface_tests(&pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    if (!compiler_bindgen_toml_rejection_tests(&pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    if (!compiler_bindgen_manifest_and_helper_tests(&pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

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
    char[512] raw_output_path_buf;
    char[512] grouped_raw_output_path_buf;
    char[512] stripped_raw_output_path_buf;
    char[512] output_path_buf;
    char[512] stripped_output_path_buf;
    char[512] invalid_strip_raw_output_path_buf;
    char[512] invalid_strip_output_path_buf;
    char[512] collision_strip_raw_output_path_buf;
    char[512] collision_strip_output_path_buf;
    int pid = compiler_test_getpid();
    usz seq = g_compiler_bindgen_temp_seq++;
    char[] header_path;
    if (try rendered = io::bprintf(
        &header_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d.h",
        pid,
        (int)seq
    )) {
        header_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose header path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] output_path;
    if (try rendered = io::bprintf(
        &output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d.omni",
        pid,
        (int)seq
    )) {
        output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] raw_output_path;
    if (try rendered = io::bprintf(
        &raw_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_raw.omni",
        pid,
        (int)seq
    )) {
        raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] grouped_raw_output_path;
    if (try rendered = io::bprintf(
        &grouped_raw_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_grouped_raw.omni",
        pid,
        (int)seq
    )) {
        grouped_raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose grouped raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] stripped_raw_output_path;
    if (try rendered = io::bprintf(
        &stripped_raw_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_stripped_raw.omni",
        pid,
        (int)seq
    )) {
        stripped_raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose stripped raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] stripped_output_path;
    if (try rendered = io::bprintf(
        &stripped_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_stripped.omni",
        pid,
        (int)seq
    )) {
        stripped_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose stripped facade output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] invalid_strip_raw_output_path;
    if (try rendered = io::bprintf(
        &invalid_strip_raw_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_invalid_strip_raw.omni",
        pid,
        (int)seq
    )) {
        invalid_strip_raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose invalid strip raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] invalid_strip_output_path;
    if (try rendered = io::bprintf(
        &invalid_strip_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_invalid_strip.omni",
        pid,
        (int)seq
    )) {
        invalid_strip_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose invalid strip facade output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] collision_strip_raw_output_path;
    if (try rendered = io::bprintf(
        &collision_strip_raw_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_collision_strip_raw.omni",
        pid,
        (int)seq
    )) {
        collision_strip_raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose collision strip raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] collision_strip_output_path;
    if (try rendered = io::bprintf(
        &collision_strip_output_path_buf,
        "/tmp/omni_bindgen_canonical_types-%d-%d_collision_strip.omni",
        pid,
        (int)seq
    )) {
        collision_strip_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types compose collision strip facade output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] header_source =
        "int omni_bindgen_int(int count);\n"
        "int omni_bindgen_const_string(const char* name);\n"
        "int omni_bindgen_const_string_suffix(char const* name);\n"
        "char* omni_bindgen_pointer_const_buffer(char* const buffer, int buffer_len);\n"
        "void omni_bindgen_string_list(const char** items);\n"
        "void omni_bindgen_mut_string_list(char** items);\n"
        "char* omni_bindgen_buffer(char* buffer, int buffer_len);\n"
        "char* omni_bindgen_inout_named_buffer(char* inout_buffer, int inout_buffer_len);\n"
        "void omni_bindgen_void_ptr(void* opaque_handle);\n"
        "void omni_bindgen_callback(void (*callback)(int));\n"
        "void omni_bindgen_char_callback(char (*callback)(int));\n"
        "int omni_bindgen_unnamed(int);\n"
        "void* omni_bindgen_ret_ptr(void);\n"
        "const char* omni_bindgen_ret_string(void);\n"
        "const char** omni_bindgen_ret_string_list(void);\n"
        "const unsigned char* omni_bindgen_ret_bytes(void);\n"
        "const signed char* omni_bindgen_ret_signed_bytes(void);\n"
        "void omni_bindgen_bytes(const unsigned char* bytes);\n"
        "void omni_bindgen_signed_bytes(const signed char* bytes);\n"
        "void omni_bindgen_mut_bytes(unsigned char* bytes, int bytes_len);\n"
        "void omni_bindgen_mut_signed_bytes(signed char* bytes, int bytes_len);\n"
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
    defer (void)io::file::delete((String)raw_output_path);
    defer (void)io::file::delete((String)grouped_raw_output_path);
    defer (void)io::file::delete((String)stripped_raw_output_path);
    defer (void)io::file::delete((String)stripped_output_path);
    defer (void)io::file::delete((String)invalid_strip_raw_output_path);
    defer (void)io::file::delete((String)invalid_strip_output_path);
    defer (void)io::file::delete((String)collision_strip_raw_output_path);
    defer (void)io::file::delete((String)collision_strip_output_path);

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
    bool parsed = func_count >= 8;
    if (!parsed) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types parse header");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    if (!compiler_bindgen_raw_and_grouped_module_tests(funcs, func_count, raw_output_path, output_path, grouped_raw_output_path, &pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    if (!compiler_bindgen_strip_prefix_module_tests(funcs, func_count, stripped_raw_output_path, stripped_output_path, invalid_strip_raw_output_path, invalid_strip_output_path, collision_strip_raw_output_path, collision_strip_output_path, &pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    if (!compiler_bindgen_facade_failure_tests(funcs, func_count, pid, seq, &pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    if (!compiler_bindgen_facade_content_tests(funcs, func_count, output_path, raw_output_path, &pass, &fail)) {
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    *pass_out += pass;
    *fail_out += fail;
}
