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
    run_compiler_group_bindgen_parse_guard_tests(interp, &pass, &fail);
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
    char[512] output_path_buf;
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
    char[] header_source =
        "int omni_bindgen_int(int count);\n"
        "int omni_bindgen_const_string(const char* name);\n"
        "char* omni_bindgen_buffer(char* buffer, int buffer_len);\n"
        "void omni_bindgen_void_ptr(void* opaque_handle);\n"
        "void omni_bindgen_callback(void (*callback)(int));\n"
        "int omni_bindgen_unnamed(int);\n"
        "void* omni_bindgen_ret_ptr(void);\n"
        "const char* omni_bindgen_ret_string(void);\n"
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

    bool generated_ok = generate_ffi_module_pair("bindgen-canonical", "bindgen", funcs, func_count, raw_output_path, output_path);
    if (!generated_ok) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types generate raw/facade modules");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    bool raw_ok = false;
    if (try generated = io::file::load_temp((String)raw_output_path)) {
        bool has_raw_module = str_contains(generated, "(module ffi-bindgen-canonical-raw");
        bool has_integer = str_contains(generated, "(^Integer count)");
        bool has_const_string_param = str_contains(generated, "(^String name)");
        bool has_buffer_pointer = str_contains(generated, "(^Pointer buffer)");
        bool has_buffer_size_param = str_contains(generated, "(^Integer buffer_len)");
        bool has_string_input = str_contains(generated, "param name c-type=const char * role=string-input ownership=borrowed nullability=unknown teardown=none");
        bool has_string_buffer = str_contains(generated, "param buffer c-type=char * role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
        bool has_pointer = str_contains(generated, "(^Pointer opaque_handle)");
        bool has_pointer_meta = str_contains(generated, "param opaque_handle c-type=void * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool has_callback_meta = str_contains(generated, "param callback c-type=void (*)(int) role=callback ownership=borrowed nullability=unknown teardown=callback-unregister");
        bool has_function_shape = str_contains(generated, "bindgen-meta-record: function c-symbol=omni_bindgen_buffer omni-name=omni-bindgen-buffer");
        bool has_param_shape = str_contains(generated, "bindgen-meta-record: param name=buffer c-type=char * ffi-type='ptr omni-type=Pointer role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
        bool has_return_shape = str_contains(generated, "bindgen-meta-record: return c-type=const char * ffi-type='string omni-type=String role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
        bool has_fallback = str_contains(generated, "(define [ffi lambda _lib] (omni-bindgen-unnamed (^Integer arg0)) ^Integer)");
        bool has_void = str_contains(generated, ") ^Void)");
        bool has_ret_pointer = str_contains(generated, ") ^Pointer)");
        bool has_ret_string_meta = str_contains(generated, "return c-type=const char * role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
        raw_ok = has_raw_module && has_integer && has_const_string_param && has_buffer_pointer &&
            has_buffer_size_param &&
            has_string_input && has_string_buffer &&
            has_pointer && has_pointer_meta && has_callback_meta && has_fallback &&
            has_function_shape && has_param_shape && has_return_shape &&
            has_void && has_ret_pointer && has_ret_string_meta;
        compiler_test_finish_bool(
            raw_ok,
            "Compiler: bindgen raw module preserves canonical names and metadata annotations",
            "[FAIL] Compiler: bindgen raw module preserves canonical names and metadata annotations",
            &pass,
            &fail
        );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types load generated raw module");
    }

    if (try generated = io::file::load_temp((String)output_path)) {
        bool has_facade_module = str_contains(generated, "(module ffi-bindgen-canonical (export");
        bool imports_raw_file = str_contains(generated, "(import \"bindgen-canonical_raw.omni\")");
        bool aliases_raw = str_contains(generated, "(import ffi-bindgen-canonical-raw (");
        bool defines_wrapper = str_contains(generated, "(define omni-bindgen-int raw-omni-bindgen-int)");
        bool defines_const_string_alias = str_contains(generated, "(define omni-bindgen-const-string raw-omni-bindgen-const-string)");
        bool skips_const_string_wrapper = !str_contains(generated, "REVIEW(bindgen): review omni-bindgen-const-string");
        bool has_buffer_size_validator = str_contains(generated, "(define (bindgen-validate-string-buffer-size buffer-size direction)");
        bool has_buffer_helper = str_contains(generated, "(define (bindgen-prepare-string-buffer buffer buffer-size role ownership nullability teardown direction)");
        bool has_buffer_manual_helper = str_contains(generated, "(define (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)");
        bool has_buffer_nil_guard = str_contains(generated, "(if (nil? buffer)") &&
            str_contains(generated, "(raise \"bindgen/string-buffer-nil\")");
        bool has_buffer_unsupported_teardown_guard = str_contains(generated, "(raise \"bindgen/string-buffer-unsupported-teardown\")");
        bool has_buffer_role_guard = str_contains(generated, "(raise \"bindgen/string-buffer-role-mismatch\")");
        bool has_buffer_ownership_guard = str_contains(generated, "(raise \"bindgen/string-buffer-ownership-mismatch\")");
        bool has_buffer_size_required_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-required\")");
        bool has_buffer_size_type_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-type-mismatch\")");
        bool has_buffer_size_positive_guard = str_contains(generated, "(raise \"bindgen/string-buffer-size-nonpositive\")");
        bool has_buffer_direction_guard = str_contains(generated, "(raise \"bindgen/string-buffer-direction-mismatch\")");
        bool has_callback_helper = str_contains(generated, "(define (bindgen-register-callback callback role ownership nullability teardown)");
        bool has_callback_cleanup_helper = str_contains(generated, "(define (bindgen-unregister-callback callback-handle role ownership nullability teardown)");
        bool has_callback_nil_guard = str_contains(generated, "(raise \"bindgen/callback-nil\")");
        bool has_callback_role_guard = str_contains(generated, "(raise \"bindgen/callback-role-mismatch\")");
        bool has_callback_ownership_guard = str_contains(generated, "(raise \"bindgen/callback-ownership-mismatch\")");
        bool has_callback_unsupported_teardown_guard = str_contains(generated, "(raise \"bindgen/callback-unsupported-teardown\")");
        bool registers_callback_via_uv_shim = str_contains(generated, "(__raw-uv-timer-callback-handle callback)");
        bool unregisters_callback_via_uv_shim = str_contains(generated, "(__raw-uv-timer-callback-unhandle callback-handle)");
        bool has_opaque_arg_helper = str_contains(generated, "(define (bindgen-wrap-opaque-arg handle role ownership nullability teardown)");
        bool has_string_return_borrowed_helper = str_contains(generated, "(define (bindgen-wrap-string-return-borrowed text role ownership nullability teardown)");
        bool has_string_return_unknown_helper = str_contains(generated, "(define (bindgen-wrap-string-return-unknown text role ownership nullability teardown)");
        bool has_opaque_return_manual_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-manual-review handle role ownership nullability teardown)");
        bool has_opaque_return_none_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-none handle role ownership nullability teardown)");
        bool flags_buffer_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-buffer — string return ownership/nullability is not automatic; mutable char* buffer direction/size must be wrapped explicitly.");
        bool flags_pointer_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-void-ptr — opaque pointer ownership/lifetime must be reviewed.");
        bool flags_callback_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-callback — callback registration and teardown must be wrapped explicitly.");
        bool wraps_buffer = str_contains(generated, "(define (omni-bindgen-buffer (^Pointer buffer) (^Integer buffer_len))");
        bool wraps_pointer = str_contains(generated, "(define (omni-bindgen-void-ptr (^Pointer opaque_handle))");
        bool wraps_callback = str_contains(generated, "(define (omni-bindgen-callback (^Pointer callback))");
        bool wraps_string_return = str_contains(generated, "(define (omni-bindgen-ret-string)");
        bool wraps_pointer_return = str_contains(generated, "(define (omni-bindgen-ret-ptr)");
        bool wraps_buffer_result = str_contains(
            generated,
            "(let (buffer_input (bindgen-prepare-string-buffer buffer buffer_len \"string-buffer\" \"borrowed\" \"unknown\" \"manual-review\" \"inout\")"
        ) && str_contains(
            generated,
            "result (raw-omni-bindgen-buffer buffer_input buffer_len)"
        ) && str_contains(
            generated,
            "result_text result"
        );
        bool wraps_return_result = str_contains(generated, "(let (result (raw-omni-bindgen-ret-string)") &&
            str_contains(generated, "result_text result");
        bool wraps_pointer_result = str_contains(generated, "(let (result (raw-omni-bindgen-ret-ptr)") &&
            str_contains(generated, "result_handle result");
        bool binds_callback_handle = str_contains(generated, "(let (callback_handle (bindgen-register-callback callback \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\"))\n");
        bool binds_pointer_handle = str_contains(generated, "(let (opaque_handle_handle (bindgen-wrap-opaque-arg opaque_handle \"opaque-pointer\" \"borrowed\" \"unknown\" \"none\"))\n");
        bool has_buffer_body_note = str_contains(generated, "REVIEW: mutable char* buffers need explicit allocation, size, and output semantics.");
        bool has_callback_body_note = str_contains(generated, "REVIEW: confirm callback-handle registration/unregister policy matches the target subsystem contract.");
        bool has_pointer_return_note = str_contains(generated, "REVIEW: wrap opaque returns in a handle/finalizer policy before exposing them broadly.");
        bool forwards_callback = str_contains(generated, "(raw-omni-bindgen-callback callback_handle)");
        bool wraps_borrowed_string_return_via_helper = str_contains(
            generated,
            "(bindgen-wrap-string-return-borrowed result_text \"string-return\" \"borrowed\" \"unknown\" \"manual-review\")"
        );
        bool wraps_unknown_string_return_via_helper = str_contains(
            generated,
            "(bindgen-wrap-string-return-unknown result_text \"string-return\" \"unknown\" \"unknown\" \"manual-review\")"
        );
        bool wraps_opaque_return_via_helper = str_contains(
            generated,
            "(bindgen-wrap-opaque-return-manual-review result_handle \"opaque-pointer\" \"unknown\" \"unknown\" \"manual-review\")"
        );
        bool routes_buffer_through_helper = str_contains(generated, "(bindgen-prepare-string-buffer buffer buffer_len \"string-buffer\" \"borrowed\" \"unknown\" \"manual-review\" \"inout\")");
        bool routes_callback_through_helper = str_contains(generated, "(bindgen-register-callback callback \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\")");
        bool routes_opaque_through_helper = str_contains(generated, "(bindgen-wrap-opaque-arg opaque_handle \"opaque-pointer\" \"borrowed\" \"unknown\" \"none\")");
        bool emits_callback_cleanup = str_contains(generated, "(bindgen-unregister-callback callback_handle \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\")");
        compiler_test_finish_bool(
            has_facade_module && imports_raw_file && aliases_raw && defines_wrapper &&
                defines_const_string_alias && skips_const_string_wrapper &&
                has_buffer_size_validator &&
                has_buffer_helper && has_buffer_manual_helper &&
                has_buffer_nil_guard && has_buffer_unsupported_teardown_guard &&
                has_buffer_role_guard && has_buffer_ownership_guard &&
                has_buffer_size_required_guard && has_buffer_size_type_guard &&
                has_buffer_size_positive_guard && has_buffer_direction_guard &&
                has_callback_helper && has_callback_cleanup_helper && has_opaque_arg_helper &&
                has_callback_nil_guard && has_callback_role_guard &&
                has_callback_ownership_guard && has_callback_unsupported_teardown_guard &&
                registers_callback_via_uv_shim && unregisters_callback_via_uv_shim &&
                has_string_return_borrowed_helper && has_string_return_unknown_helper &&
                has_opaque_return_manual_helper && has_opaque_return_none_helper &&
                flags_buffer_review && flags_pointer_review && flags_callback_review && wraps_buffer && wraps_pointer &&
                wraps_callback && wraps_string_return && has_buffer_body_note &&
                has_callback_body_note && forwards_callback &&
                wraps_buffer_result && wraps_return_result &&
                wraps_pointer_return && wraps_pointer_result && has_pointer_return_note &&
                binds_callback_handle && binds_pointer_handle &&
                routes_buffer_through_helper && routes_callback_through_helper && routes_opaque_through_helper &&
                wraps_borrowed_string_return_via_helper &&
                wraps_unknown_string_return_via_helper &&
                wraps_opaque_return_via_helper &&
                emits_callback_cleanup,
            "Compiler: bindgen facade module imports raw, scaffolds wrappers, and flags manual review",
            "[FAIL] Compiler: bindgen facade module imports raw, scaffolds wrappers, and flags manual review",
            &pass,
            &fail
        );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types load generated facade module");
    }

    if (!compiler_write_text_file(output_path, ";; user facade\n")) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen facade preservation write sentinel");
    } else {
        bool preserved = generate_ffi_module_pair("bindgen-canonical", "bindgen", funcs, func_count, raw_output_path, output_path);
        if (try generated = io::file::load_temp((String)output_path)) {
            compiler_test_finish_bool(
                preserved && str_contains(generated, ";; user facade"),
                "Compiler: bindgen preserves existing facade file",
                "[FAIL] Compiler: bindgen preserves existing facade file",
                &pass,
                &fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen preserve existing facade load");
        }
    }

    *pass_out += pass;
    *fail_out += fail;
}


fn void run_compiler_group_bindgen_parse_guard_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    char[640] long_header_buf;
    usz long_len = 0;
    char[] prefix = "/tmp/";
    for (usz i = 0; i < prefix.len; i++) {
        long_header_buf[long_len++] = prefix[i];
    }
    while (long_len < 520) {
        long_header_buf[long_len++] = 'a';
    }

    LibclangHandle dummy = {};
    ParseHeaderResult long_path_result = parse_header_checked(&dummy, long_header_buf[..long_len], null, 0, null, 0, false);
    compiler_test_finish_bool(
        long_path_result.path_too_long &&
            long_path_result.count == 0 &&
            !long_path_result.output_limit_hit &&
            !long_path_result.parse_failed,
        "Compiler: bindgen parse guards reject overlong header paths",
        "[FAIL] Compiler: bindgen parse guards reject overlong header paths",
        &pass,
        &fail
    );

    LibclangHandle clang;
    if (!libclang_init(&clang)) {
        pass++;
        compiler_print_pass("Compiler: bindgen parse guards function limit skipped (libclang unavailable)");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer libclang_destroy(&clang);

    char[512] header_path_buf;
    int pid = compiler_test_getpid();
    usz seq = g_compiler_bindgen_temp_seq++;
    char[] header_path;
    if (try rendered = io::bprintf(
        &header_path_buf,
        "/tmp/omni_bindgen_parse_guard-%d-%d.h",
        pid,
        (int)seq
    )) {
        header_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen parse guards compose header path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer (void)io::file::delete((String)header_path);

    char[] header_source =
        "int omni_bindgen_limit_a(int x);\n"
        "int omni_bindgen_limit_b(void);\n"
        "int omni_bindgen_limit_c(void);\n";
    if (!compiler_write_text_file(header_path, header_source)) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen parse guards write header");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    const usz MAX_FUNCS = 2;
    ParsedFunc* funcs = (ParsedFunc*)mem::malloc(ParsedFunc.sizeof * MAX_FUNCS);
    if (funcs == null) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen parse guards alloc funcs");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer mem::free(funcs);
    for (usz i = 0; i < MAX_FUNCS; i++) {
        funcs[i] = {};
    }
    defer {
        for (usz i = 0; i < MAX_FUNCS; i++) {
            if (funcs[i].params != null) mem::free(funcs[i].params);
        }
    }

    ParseHeaderResult limit_result = parse_header_checked(&clang, header_path, funcs, MAX_FUNCS, null, 0, false);
    compiler_test_finish_bool(
        limit_result.output_limit_hit &&
            limit_result.count == MAX_FUNCS &&
            !limit_result.path_too_long &&
            !limit_result.param_allocation_failed &&
            !limit_result.unsupported_type &&
            !limit_result.parse_failed,
        "Compiler: bindgen parse guards reject partial header capture at function limit",
        "[FAIL] Compiler: bindgen parse guards reject partial header capture at function limit",
        &pass,
        &fail
    );

    g_bind_force_param_alloc_fail = true;
    ParseHeaderResult alloc_fail_result = parse_header_checked(&clang, header_path, funcs, 4, null, 0, false);
    g_bind_force_param_alloc_fail = false;
    compiler_test_finish_bool(
        alloc_fail_result.param_allocation_failed &&
            alloc_fail_result.count == 0 &&
            !alloc_fail_result.path_too_long &&
            !alloc_fail_result.output_limit_hit &&
            !alloc_fail_result.unsupported_type &&
            !alloc_fail_result.parse_failed,
        "Compiler: bindgen parse guards reject parameter allocation failure",
        "[FAIL] Compiler: bindgen parse guards reject parameter allocation failure",
        &pass,
        &fail
    );

    char[512] unsupported_path_buf;
    char[] unsupported_path;
    if (try rendered = io::bprintf(
        &unsupported_path_buf,
        "/tmp/omni_bindgen_unsupported_guard-%d-%d.h",
        pid,
        (int)(seq + 1)
    )) {
        unsupported_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen parse guards compose unsupported header path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer (void)io::file::delete((String)unsupported_path);

    char[] unsupported_header_source =
        "struct omni_bindgen_struct { int x; };\n"
        "void omni_bindgen_bad(struct omni_bindgen_struct value);\n";
    if (!compiler_write_text_file(unsupported_path, unsupported_header_source)) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen parse guards write unsupported header");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }

    ParseHeaderResult unsupported_result = parse_header_checked(&clang, unsupported_path, funcs, 4, null, 0, false);
    compiler_test_finish_bool(
        unsupported_result.unsupported_type &&
            unsupported_result.count == 0 &&
            !unsupported_result.path_too_long &&
            !unsupported_result.output_limit_hit &&
            !unsupported_result.param_allocation_failed &&
            !unsupported_result.parse_failed,
        "Compiler: bindgen parse guards reject unsupported C types",
        "[FAIL] Compiler: bindgen parse guards reject unsupported C types",
        &pass,
        &fail
    );

    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_bindgen_overflow_guard_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    const usz OVERFLOW_FUNCS = 256;
    ParsedFunc* funcs = (ParsedFunc*)mem::malloc(ParsedFunc.sizeof * OVERFLOW_FUNCS);
    if (funcs == null) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen overflow guard alloc funcs");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer mem::free(funcs);

    for (usz i = 0; i < OVERFLOW_FUNCS; i++) {
        funcs[i] = {};
        funcs[i].params = null;
        funcs[i].param_count = 0;
        funcs[i].param_capacity = 0;
        funcs[i].is_variadic = false;
        compiler_fill_bindgen_name(&funcs[i].name, i);
        compiler_fill_bindgen_return_type(&funcs[i].return_omni_type);
    }

    char[512] output_path_buf;
    char[512] raw_output_path_buf;
    int pid = compiler_test_getpid();
    usz seq = g_compiler_bindgen_temp_seq++;
    char[] output_path;
    if (try rendered = io::bprintf(
        &output_path_buf,
        "/tmp/omni_bindgen_overflow_guard-%d-%d.omni",
        pid,
        (int)seq
    )) {
        output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen overflow guard compose output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    char[] raw_output_path;
    if (try rendered = io::bprintf(
        &raw_output_path_buf,
        "/tmp/omni_bindgen_overflow_guard-%d-%d_raw.omni",
        pid,
        (int)seq
    )) {
        raw_output_path = rendered;
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen overflow guard compose raw output path");
        *pass_out += pass;
        *fail_out += fail;
        return;
    }
    defer (void)io::file::delete((String)output_path);
    defer (void)io::file::delete((String)raw_output_path);

    bool generated_ok = generate_ffi_module_pair("bindgen-overflow", "bindgen", funcs, OVERFLOW_FUNCS, raw_output_path, output_path);
    bool wrote_file = false;
    if (try file = io::file::open((String)output_path, "r")) {
        wrote_file = true;
        (void)file.close();
    }
    if (try file = io::file::open((String)raw_output_path, "r")) {
        wrote_file = true;
        (void)file.close();
    }

    compiler_test_finish_bool(
        !generated_ok && !wrote_file,
        "Compiler: bindgen overflow guard rejects truncated module emission",
        "[FAIL] Compiler: bindgen overflow guard rejects truncated module emission",
        &pass,
        &fail
    );

    *pass_out += pass;
    *fail_out += fail;
}
