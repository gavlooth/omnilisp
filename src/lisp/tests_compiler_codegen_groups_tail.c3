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

    {
        TomlFfiDep dep = {};
        BindgenRawSyntax syntax;
        toml_set_default(&dep.raw_syntax, 16, "grouped");
        bool accepts_grouped = bindgen_raw_syntax_from_dep(&dep, &syntax) &&
            syntax == BINDGEN_RAW_SYNTAX_GROUPED;
        toml_set_default(&dep.raw_syntax, 16, "legacy");
        bool accepts_legacy = bindgen_raw_syntax_from_dep(&dep, &syntax) &&
            syntax == BINDGEN_RAW_SYNTAX_LEGACY;
        toml_set_default(&dep.raw_syntax, 16, "invalid");
        bool rejects_invalid = !bindgen_raw_syntax_from_dep(&dep, &syntax);
        compiler_test_finish_bool(
            accepts_grouped && accepts_legacy && rejects_invalid,
            "Compiler: bindgen raw-syntax option accepts only legacy or grouped",
            "[FAIL] Compiler: bindgen raw-syntax option accepts only legacy or grouped",
            &pass,
            &fail
        );
    }

    {
        TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
        if (config == null) {
            fail++;
            io::printn("[FAIL] Compiler: bindgen strip-prefixes toml config alloc");
        } else {
            defer mem::free(config);
            char[] source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "strip-prefixes = [\"sqlite3_\", \"omni_\"]\n";
            parse_toml(source, config);
            TomlFfiDep* dep = &config.ffi_deps[0];
            bool parses_strip_prefixes =
                config.ffi_dep_count == 1 &&
                !dep.name_invalid &&
                !dep.library_invalid &&
                !dep.raw_syntax_invalid &&
                !dep.headers_invalid &&
                !dep.functions_invalid &&
                !dep.strip_prefixes_invalid &&
                !dep.unknown_key_invalid &&
                dep.strip_prefix_count == 2 &&
                dep.library_seen &&
                dep.headers_seen &&
                dep.strip_prefixes_seen &&
                !dep.raw_syntax_seen &&
                !dep.functions_seen &&
                bindgen_cstr_eq(&dep.strip_prefixes[0], "sqlite3_") &&
                bindgen_cstr_eq(&dep.strip_prefixes[1], "omni_");
            compiler_test_finish_bool(
                parses_strip_prefixes,
                "Compiler: bindgen strip-prefixes toml option parses string arrays",
                "[FAIL] Compiler: bindgen strip-prefixes toml option parses string arrays",
                &pass,
                &fail
            );

            char[] exclude_source =
                "[dependencies.ffi.math]\n"
                "library = \"m\"\n"
                "headers = [\"/tmp/math.h\"]\n"
                "functions = [\"cos\", \"sin\", \"tan\"]\n"
                "exclude-functions = [\"sin\", \"tan\"]\n";
            parse_toml(exclude_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    !dep.library_invalid &&
                    !dep.headers_invalid &&
                    !dep.functions_invalid &&
                    !dep.exclude_functions_invalid &&
                    !dep.unknown_key_invalid &&
                    dep.functions_seen &&
                    dep.exclude_functions_seen &&
                    dep.function_count == 3 &&
                    dep.exclude_function_count == 2 &&
                    bindgen_cstr_eq(&dep.exclude_functions[0], "sin") &&
                    bindgen_cstr_eq(&dep.exclude_functions[1], "tan"),
                "Compiler: bindgen exclude-functions toml option parses string arrays",
                "[FAIL] Compiler: bindgen exclude-functions toml option parses string arrays",
                &pass,
                &fail
            );

            char[] inline_comment_source =
                "[dependencies.ffi.hash-lib]\n"
                "library = \"sha#core\" # comment after quoted string\n"
                "raw-syntax = \"grouped\" # comment after raw syntax\n"
                "headers = [\"/tmp/hash#core.h\"] # comment after array\n"
                "functions = [\"hash#update\"] # comment after functions\n"
                "strip-prefixes = [\"hash#\"] # comment after prefixes\n";
            parse_toml(inline_comment_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    !dep.library_invalid &&
                    !dep.raw_syntax_invalid &&
                    !dep.headers_invalid &&
                    !dep.functions_invalid &&
                    !dep.strip_prefixes_invalid &&
                    !dep.unknown_key_invalid &&
                    bindgen_cstr_eq(&dep.library, "sha#core") &&
                    bindgen_cstr_eq(&dep.raw_syntax, "grouped") &&
                    bindgen_cstr_eq(&dep.headers[0], "/tmp/hash#core.h") &&
                    bindgen_cstr_eq(&dep.functions[0], "hash#update") &&
                    bindgen_cstr_eq(&dep.strip_prefixes[0], "hash#"),
                "Compiler: bindgen toml keeps quoted hash values and strips inline comments",
                "[FAIL] Compiler: bindgen toml keeps quoted hash values and strips inline comments",
                &pass,
                &fail
            );

            char[] escaped_comment_source =
                `[dependencies.ffi.win]
library = "C:\\tmp\\" # comment after path
headers = ["C:\\include\\hash.h"] # comment after array
functions = ["hash\"update"] # comment after escaped quote
strip-prefixes = ["hash\\"] # comment after prefix
`;
            parse_toml(escaped_comment_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    !dep.library_invalid &&
                    !dep.headers_invalid &&
                    !dep.functions_invalid &&
                    !dep.strip_prefixes_invalid &&
                    !dep.unknown_key_invalid &&
                    bindgen_cstr_eq(&dep.library, `C:\tmp\`) &&
                    bindgen_cstr_eq(&dep.headers[0], `C:\include\hash.h`) &&
                    bindgen_cstr_eq(&dep.functions[0], `hash"update`) &&
                    bindgen_cstr_eq(&dep.strip_prefixes[0], `hash\`),
                "Compiler: bindgen toml honors escaped backslashes before inline comments",
                "[FAIL] Compiler: bindgen toml honors escaped backslashes before inline comments",
                &pass,
                &fail
            );

            {
                int control_pid = compiler_test_getpid();
                usz control_seq = g_compiler_bindgen_temp_seq++;
                char[512] control_project_path_buf;
                char[512] control_project_lib_path_buf;
                char[512] control_project_ffi_path_buf;
                char[512] control_raw_path_buf;
                char[512] control_facade_path_buf;
                char[512] control_manifest_path_buf;
                char[] control_project_path;
                char[] control_project_lib_path;
                char[] control_project_ffi_path;
                char[] control_raw_path;
                char[] control_facade_path;
                char[] control_manifest_path;
                if (try rendered = io::bprintf(
                    &control_project_path_buf,
                    "/tmp/omni_bindgen_control_control-%d-%d",
                    control_pid,
                    (int)control_seq
                )) {
                    control_project_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char project path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }
                if (try rendered = io::bprintf(&control_project_lib_path_buf, "%s/lib", (ZString)&control_project_path_buf[0])) {
                    control_project_lib_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char lib path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }
                if (try rendered = io::bprintf(&control_project_ffi_path_buf, "%s/lib/ffi", (ZString)&control_project_path_buf[0])) {
                    control_project_ffi_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char ffi path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }
                if (try rendered = io::bprintf(&control_raw_path_buf, "%s/lib/ffi/control_raw.omni", (ZString)&control_project_path_buf[0])) {
                    control_raw_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char raw path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }
                if (try rendered = io::bprintf(&control_facade_path_buf, "%s/lib/ffi/control.omni", (ZString)&control_project_path_buf[0])) {
                    control_facade_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char facade path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }
                if (try rendered = io::bprintf(&control_manifest_path_buf, "%s/lib/ffi/control_manifest.toml", (ZString)&control_project_path_buf[0])) {
                    control_manifest_path = rendered;
                } else {
                    fail++;
                    io::printn("[FAIL] Compiler: bindgen control-char manifest path");
                    *pass_out += pass;
                    *fail_out += fail;
                    return;
                }

                (void)compiler_test_mkdir(&control_project_path_buf[0], 0o755);
                (void)compiler_test_mkdir(&control_project_lib_path_buf[0], 0o755);
                (void)compiler_test_mkdir(&control_project_ffi_path_buf[0], 0o755);
                defer (void)compiler_test_rmdir(&control_project_ffi_path_buf[0]);
                defer (void)compiler_test_rmdir(&control_project_lib_path_buf[0]);
                defer (void)compiler_test_rmdir(&control_project_path_buf[0]);

                char[] control_tab_source =
                    "[dependencies.ffi.control]\n"
                    "library = \"sqlite\t3\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n";
                parse_toml(control_tab_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] control_funcs;
                usz control_bindings = 0;
                usz control_libs = 0;
                bool rejects_control_char = dep.library_invalid && !main::process_bind_dependency(
                    control_project_path,
                    control_project_path.len,
                    null,
                    dep,
                    &control_funcs[0],
                    1,
                    &control_bindings,
                    &control_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        rejects_control_char &&
                        !bindgen_output_file_exists(control_raw_path) &&
                        !bindgen_output_file_exists(control_facade_path) &&
                        !bindgen_output_file_exists(control_manifest_path),
                    "Compiler: bindgen toml rejects raw control bytes in quoted dependency metadata and writes no outputs",
                    "[FAIL] Compiler: bindgen toml rejects raw control bytes in quoted dependency metadata and writes no outputs",
                    &pass,
                    &fail
                );

                char[] control_escape_nul_source =
                    "[dependencies.ffi.control]\n"
                    "library = \"m\\u0000evil\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n";
                parse_toml(control_escape_nul_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] control_escape_nul_funcs;
                usz control_escape_nul_bindings = 0;
                usz control_escape_nul_libs = 0;
                bool rejects_escape_nul = dep.library_invalid && !main::process_bind_dependency(
                    control_project_path,
                    control_project_path.len,
                    null,
                    dep,
                    &control_escape_nul_funcs[0],
                    1,
                    &control_escape_nul_bindings,
                    &control_escape_nul_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        rejects_escape_nul &&
                        !bindgen_output_file_exists(control_raw_path) &&
                        !bindgen_output_file_exists(control_facade_path) &&
                        !bindgen_output_file_exists(control_manifest_path),
                    "Compiler: bindgen toml rejects escaped NUL in quoted dependency metadata and writes no outputs",
                    "[FAIL] Compiler: bindgen toml rejects escaped NUL in quoted dependency metadata and writes no outputs",
                    &pass,
                    &fail
                );
            }

            char[] trailing_unicode_escape_source =
                `[dependencies.ffi.trailing]
library = "m"
headers = ["/tmp/omni\u002fsqlite3.h",]
strip-prefixes = ["omni\f",]
`;
            parse_toml(trailing_unicode_escape_source, config);
            dep = &config.ffi_deps[0];
            char* prefix = (char*)&dep.strip_prefixes[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    !dep.library_invalid &&
                    !dep.headers_invalid &&
                    !dep.strip_prefixes_invalid &&
                    !dep.unknown_key_invalid &&
                    dep.header_count == 1 &&
                    dep.strip_prefix_count == 1 &&
                    bindgen_cstr_eq(&dep.headers[0], "/tmp/omni/sqlite3.h") &&
                    prefix[0] == 'o' &&
                    prefix[1] == 'm' &&
                    prefix[2] == 'n' &&
                    prefix[3] == 'i' &&
                    prefix[4] == (char)12 &&
                    prefix[5] == 0,
                "Compiler: bindgen toml accepts trailing array commas and basic string escapes",
                "[FAIL] Compiler: bindgen toml accepts trailing array commas and basic string escapes",
                &pass,
                &fail
            );

            {
                char[] section_comment_source =
                    "[dependencies.ffi.sqlite]\n"
                    "library = \"sqlite3\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n"
                    "[dependencies.ffi.math] # inline section comment\n"
                    "library = \"m\"\n"
                    "headers = [\"/usr/include/math.h\"]\n";
                parse_toml(section_comment_source, config);
                compiler_test_finish_bool(
                    config.ffi_dep_count == 2 &&
                        bindgen_cstr_eq(&config.ffi_deps[0].library, "sqlite3") &&
                        bindgen_cstr_eq(&config.ffi_deps[1].name, "math") &&
                        bindgen_cstr_eq(&config.ffi_deps[1].library, "m") &&
                        config.ffi_deps[1].header_count == 1,
                    "Compiler: bindgen toml accepts inline comments after section headers",
                    "[FAIL] Compiler: bindgen toml accepts inline comments after section headers",
                    &pass,
                    &fail
                );
            }

            {
                char[] malformed_section_source =
                    "[dependencies.ffi.sqlite]\n"
                    "library = \"sqlite3\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n"
                    "[dependencies.ffi.math\n"
                    "library = \"../mutated\"\n"
                    "headers = [\"/usr/include/math.h\"]\n";
                parse_toml(malformed_section_source, config);
                dep = &config.ffi_deps[0];
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        dep.unknown_key_invalid &&
                        bindgen_cstr_eq(&dep.library, "sqlite3") &&
                        dep.header_count == 1,
                    "Compiler: bindgen toml malformed section headers fail closed",
                    "[FAIL] Compiler: bindgen toml malformed section headers fail closed",
                    &pass,
                    &fail
                );
            }

            {
                char[] unknown_key_source =
                    "[dependencies.ffi.sqlite]\n"
                    "library = \"sqlite3\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n"
                    "function = [\"sqlite3_open\"]\n";
                parse_toml(unknown_key_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] unknown_key_funcs;
                usz unknown_key_bindings = 0;
                usz unknown_key_libs = 0;
                bool rejects_unknown_key = !main::process_bind_dependency(
                    "."[..],
                    1,
                    null,
                    dep,
                    &unknown_key_funcs[0],
                    1,
                    &unknown_key_bindings,
                    &unknown_key_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        dep.unknown_key_invalid &&
                        rejects_unknown_key &&
                        unknown_key_bindings == 0 &&
                        unknown_key_libs == 0,
                    "Compiler: bindgen toml rejects unknown dependency keys",
                    "[FAIL] Compiler: bindgen toml rejects unknown dependency keys",
                    &pass,
                    &fail
                );
            }

            char[] duplicate_library_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "library = \"m\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(duplicate_library_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.library_invalid,
                "Compiler: bindgen toml rejects duplicate library keys",
                "[FAIL] Compiler: bindgen toml rejects duplicate library keys",
                &pass,
                &fail
            );

            char[] duplicate_raw_syntax_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "raw-syntax = \"legacy\"\n"
                "raw-syntax = \"grouped\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(duplicate_raw_syntax_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.raw_syntax_invalid,
                "Compiler: bindgen toml rejects duplicate raw-syntax keys",
                "[FAIL] Compiler: bindgen toml rejects duplicate raw-syntax keys",
                &pass,
                &fail
            );

            char[] duplicate_headers_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "headers = [\"/usr/include/math.h\"]\n";
            parse_toml(duplicate_headers_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.headers_invalid,
                "Compiler: bindgen toml rejects duplicate headers keys",
                "[FAIL] Compiler: bindgen toml rejects duplicate headers keys",
                &pass,
                &fail
            );

            char[] duplicate_functions_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "functions = [\"sqlite3_open\"]\n"
                "functions = [\"sqlite3_close\"]\n";
            parse_toml(duplicate_functions_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.functions_invalid,
                "Compiler: bindgen toml rejects duplicate functions keys",
                "[FAIL] Compiler: bindgen toml rejects duplicate functions keys",
                &pass,
                &fail
            );

            char[] duplicate_strip_prefixes_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "strip-prefixes = [\"sqlite3_\"]\n"
                "strip-prefixes = [\"omni_\"]\n";
            parse_toml(duplicate_strip_prefixes_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.strip_prefixes_invalid,
                "Compiler: bindgen toml rejects duplicate strip-prefixes keys",
                "[FAIL] Compiler: bindgen toml rejects duplicate strip-prefixes keys",
                &pass,
                &fail
            );

            char[] unquoted_raw_syntax_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "raw-syntax = grouped\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(unquoted_raw_syntax_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.raw_syntax_invalid,
                "Compiler: bindgen toml rejects unquoted raw-syntax values",
                "[FAIL] Compiler: bindgen toml rejects unquoted raw-syntax values",
                &pass,
                &fail
            );

            char[] malformed_library_tail_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\" \"m\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(malformed_library_tail_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.library_invalid,
                "Compiler: bindgen toml rejects malformed quoted library tails",
                "[FAIL] Compiler: bindgen toml rejects malformed quoted library tails",
                &pass,
                &fail
            );

            char[] empty_library_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(empty_library_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.library_invalid,
                "Compiler: bindgen toml rejects empty library names",
                "[FAIL] Compiler: bindgen toml rejects empty library names",
                &pass,
                &fail
            );

            {
                char[] unsafe_library_source =
                    "[dependencies.ffi.sqlite]\n"
                    "library = \"../sqlite3\"\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n";
                parse_toml(unsafe_library_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] unsafe_library_funcs;
                usz unsafe_library_bindings = 0;
                usz unsafe_library_libs = 0;
                bool rejects_unsafe_library = !main::process_bind_dependency(
                    "."[..],
                    1,
                    null,
                    dep,
                    &unsafe_library_funcs[0],
                    1,
                    &unsafe_library_bindings,
                    &unsafe_library_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        !dep.library_invalid &&
                        rejects_unsafe_library &&
                        unsafe_library_bindings == 0 &&
                        unsafe_library_libs == 0,
                    "Compiler: bindgen rejects unsafe library stems before header parsing",
                    "[FAIL] Compiler: bindgen rejects unsafe library stems before header parsing",
                    &pass,
                    &fail
                );
            }

            {
                char[] missing_library_source =
                    "[dependencies.ffi.sqlite]\n"
                    "headers = [\"/usr/include/sqlite3.h\"]\n";
                parse_toml(missing_library_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] missing_library_funcs;
                usz missing_library_bindings = 0;
                usz missing_library_libs = 0;
                bool rejects_missing_library = !main::process_bind_dependency(
                    "."[..],
                    1,
                    null,
                    dep,
                    &missing_library_funcs[0],
                    1,
                    &missing_library_bindings,
                    &missing_library_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        rejects_missing_library &&
                        missing_library_bindings == 0 &&
                        missing_library_libs == 0,
                    "Compiler: bindgen rejects missing library fields before header parsing",
                    "[FAIL] Compiler: bindgen rejects missing library fields before header parsing",
                    &pass,
                    &fail
                );
            }

            char[] malformed_headers_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [, \"/usr/include/sqlite3.h\"]\n";
            parse_toml(malformed_headers_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.headers_invalid &&
                    dep.header_count == 0,
                "Compiler: bindgen toml rejects malformed header arrays",
                "[FAIL] Compiler: bindgen toml rejects malformed header arrays",
                &pass,
                &fail
            );

            {
                char[] missing_headers_source =
                    "[dependencies.ffi.sqlite]\n"
                    "library = \"sqlite3\"\n";
                parse_toml(missing_headers_source, config);
                dep = &config.ffi_deps[0];
                ParsedFunc[1] missing_headers_funcs;
                usz missing_headers_bindings = 0;
                usz missing_headers_libs = 0;
                bool rejects_missing_headers = !main::process_bind_dependency(
                    "."[..],
                    1,
                    null,
                    dep,
                    &missing_headers_funcs[0],
                    1,
                    &missing_headers_bindings,
                    &missing_headers_libs
                );
                compiler_test_finish_bool(
                    config.ffi_dep_count == 1 &&
                        rejects_missing_headers &&
                        missing_headers_bindings == 0 &&
                        missing_headers_libs == 0,
                    "Compiler: bindgen rejects missing header fields before header parsing",
                    "[FAIL] Compiler: bindgen rejects missing header fields before header parsing",
                    &pass,
                    &fail
                );
            }

            char[] prefixed_headers_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = garbage [\"/usr/include/sqlite3.h\"]\n";
            parse_toml(prefixed_headers_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.headers_invalid &&
                    dep.header_count == 0,
                "Compiler: bindgen toml rejects prefixed header arrays",
                "[FAIL] Compiler: bindgen toml rejects prefixed header arrays",
                &pass,
                &fail
            );

            char[] overlong_function_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "functions = [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]\n";
            parse_toml(overlong_function_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.functions_invalid &&
                    dep.function_count == 0,
                "Compiler: bindgen toml rejects overlong function filters",
                "[FAIL] Compiler: bindgen toml rejects overlong function filters",
                &pass,
                &fail
            );

            char[] overlong_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "strip-prefixes = [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]\n";
            parse_toml(overlong_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.strip_prefixes_invalid &&
                    dep.strip_prefix_count == 0,
                "Compiler: bindgen strip-prefixes rejects overlong toml entries instead of truncating",
                "[FAIL] Compiler: bindgen strip-prefixes rejects overlong toml entries instead of truncating",
                &pass,
                &fail
            );

            char[] malformed_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "strip-prefixes = [, \"sqlite3_\"]\n";
            parse_toml(malformed_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.strip_prefixes_invalid &&
                    dep.strip_prefix_count == 0,
                "Compiler: bindgen strip-prefixes rejects malformed toml arrays",
                "[FAIL] Compiler: bindgen strip-prefixes rejects malformed toml arrays",
                &pass,
                &fail
            );

            char[] empty_entry_source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "strip-prefixes = [\"\"]\n";
            parse_toml(empty_entry_source, config);
            dep = &config.ffi_deps[0];
            compiler_test_finish_bool(
                config.ffi_dep_count == 1 &&
                    dep.strip_prefixes_invalid &&
                    dep.strip_prefix_count == 0,
                "Compiler: bindgen strip-prefixes rejects empty toml entries",
                "[FAIL] Compiler: bindgen strip-prefixes rejects empty toml entries",
                &pass,
                &fail
            );
        }
    }

    {
        char[512] manifest_output_path_buf;
        int manifest_pid = compiler_test_getpid();
        usz manifest_seq = g_compiler_bindgen_temp_seq++;
        char[] manifest_output_path;
        if (try rendered = io::bprintf(
            &manifest_output_path_buf,
            "/tmp/omni_bindgen_manifest-%d-%d.toml",
            manifest_pid,
            (int)manifest_seq
        )) {
            manifest_output_path = rendered;
            defer (void)io::file::delete((String)manifest_output_path);

            char[64][TOML_MAX_STRIP_PREFIXES] manifest_prefixes;
            for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
                toml_clear_buf(&manifest_prefixes[i], 64);
            }
            toml_set_default(&manifest_prefixes[0], 64, "sqlite3_");
            toml_set_default(&manifest_prefixes[1], 64, "omni_");

            bool wrote_manifest = generate_ffi_bindgen_manifest(
                "sqlite",
                "sqlite3",
                "lib/ffi/sqlite_raw.omni",
                "lib/ffi/sqlite.omni",
                manifest_output_path,
                BINDGEN_RAW_SYNTAX_GROUPED,
                null,
                0,
                &manifest_prefixes,
                2
            );
            bool manifest_ok = false;
            if (try generated = io::file::load_temp((String)manifest_output_path)) {
                manifest_ok =
                    str_contains(generated, "format-version = \"1\"") &&
                    str_contains(generated, "dependency = \"sqlite\"") &&
                    str_contains(generated, "library = \"sqlite3\"") &&
                    str_contains(generated, "raw-syntax = \"grouped\"") &&
                    str_contains(generated, "raw-output = \"lib/ffi/sqlite_raw.omni\"") &&
                    str_contains(generated, "facade-output = \"lib/ffi/sqlite.omni\"") &&
                    str_contains(generated, "exclude-functions = []") &&
                    str_contains(generated, "strip-prefixes = [\"sqlite3_\", \"omni_\"]");
            }
            compiler_test_finish_bool(
                wrote_manifest && manifest_ok,
                "Compiler: bindgen manifest records effective generated config",
                "[FAIL] Compiler: bindgen manifest records effective generated config",
                &pass,
                &fail
            );

            char[512] escaped_manifest_output_path_buf;
            char[] escaped_manifest_output_path;
            if (try escaped_rendered = io::bprintf(
                &escaped_manifest_output_path_buf,
                "/tmp/omni_bindgen_manifest_escaped-%d-%d.toml",
                manifest_pid,
                (int)manifest_seq
            )) {
                escaped_manifest_output_path = escaped_rendered;
                defer (void)io::file::delete((String)escaped_manifest_output_path);

                char[64][TOML_MAX_STRIP_PREFIXES] escaped_manifest_prefixes;
                for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
                    toml_clear_buf(&escaped_manifest_prefixes[i], 64);
                }
                toml_set_default(&escaped_manifest_prefixes[0], 64, "prefix\\with\"quotes");
                escaped_manifest_prefixes[0][18] = (char)1;
                char[32] escaped_raw_path = {};
                toml_set_default(&escaped_raw_path, 32, "raw-path\"quoted.omni");
                escaped_raw_path[3] = (char)1;

                bool escaped_wrote_manifest = generate_ffi_bindgen_manifest(
                    "sqlite",
                    "sqlite3",
                    escaped_raw_path[:20],
                    "facade\\path\"quoted.omni",
                    escaped_manifest_output_path,
                    BINDGEN_RAW_SYNTAX_GROUPED,
                    null,
                    0,
                    &escaped_manifest_prefixes,
                    1
                );
                bool escaped_manifest_ok = false;
                if (try escaped_generated = io::file::load_temp((String)escaped_manifest_output_path)) {
                    escaped_manifest_ok =
                        str_contains(escaped_generated, "dependency = \"sqlite\"") &&
                        str_contains(escaped_generated, "library = \"sqlite3\"") &&
                        str_contains(escaped_generated, "raw-output = \"raw\\u0001path\\\"quoted.omni\"") &&
                        str_contains(escaped_generated, "facade-output = \"facade\\\\path\\\"quoted.omni\"") &&
                        str_contains(escaped_generated, "exclude-functions = []") &&
                        str_contains(escaped_generated, "strip-prefixes = [\"prefix\\\\with\\\"quotes\\u0001\"]");
                }
                compiler_test_finish_bool(
                    escaped_wrote_manifest && escaped_manifest_ok,
                    "Compiler: bindgen manifest escapes quoted dependency, path, and control strings",
                    "[FAIL] Compiler: bindgen manifest escapes quoted dependency, path, and control strings",
                    &pass,
                    &fail
                );
            } else {
                fail++;
                io::printn("[FAIL] Compiler: bindgen manifest compose escaped output path");
            }

            char[512] atomic_fail_path_buf;
            char[512] atomic_fail_tmp_path_buf;
            char[] atomic_fail_path;
            char[] atomic_fail_tmp_path;
            if (try atomic_fail_path_rendered = io::bprintf(
                &atomic_fail_path_buf,
                "/tmp/omni_bindgen_atomic_write_fail-%d-%d.toml",
                manifest_pid,
                (int)manifest_seq
            )) {
                atomic_fail_path = atomic_fail_path_rendered;
            } else {
                fail++;
                io::printn("[FAIL] Compiler: bindgen compose atomic write failure path");
                *pass_out += pass;
                *fail_out += fail;
                return;
            }
            if (try atomic_fail_tmp_path_rendered = io::bprintf(
                &atomic_fail_tmp_path_buf,
                "%s.tmp.%d",
                (ZString)&atomic_fail_path_buf[0],
                manifest_pid
            )) {
                atomic_fail_tmp_path = atomic_fail_tmp_path_rendered;
            } else {
                fail++;
                io::printn("[FAIL] Compiler: bindgen compose atomic write failure temp path");
                *pass_out += pass;
                *fail_out += fail;
                return;
            }
            (void)compiler_test_mkdir(&atomic_fail_path_buf[0], 0o755);
            defer (void)io::file::delete((String)atomic_fail_tmp_path);
            defer (void)compiler_test_rmdir(&atomic_fail_path_buf[0]);

            bool rejects_atomic_write_failure = !bindgen_write_text_file(atomic_fail_path, "format-version = \"1\"\n");
            compiler_test_finish_bool(
                rejects_atomic_write_failure &&
                    !bindgen_output_file_exists(atomic_fail_tmp_path),
                "Compiler: bindgen writer cleans atomic temp output when final rename fails",
                "[FAIL] Compiler: bindgen writer cleans atomic temp output when final rename fails",
                &pass,
                &fail
            );
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen manifest compose output path");
        }
    }

    {
        ParsedFunc[1] helper_funcs;
        ParsedParam[2] helper_params;
        helper_funcs[0] = {};
        helper_params[0] = {};
        helper_params[1] = {};

        toml_set_default(&helper_funcs[0].name, 128, "omni_bindgen_buffer_none");
        toml_set_default(&helper_funcs[0].return_c_type, 64, "void");
        toml_set_default(&helper_funcs[0].return_ffi_type, 16, "'void");
        toml_set_default(&helper_funcs[0].return_omni_type, 16, "Void");
        helper_funcs[0].params = &helper_params[0];
        helper_funcs[0].param_count = 2;
        helper_funcs[0].param_capacity = 2;
        helper_funcs[0].return_role = BINDGEN_ROLE_VALUE;
        helper_funcs[0].return_ownership = BINDGEN_OWNERSHIP_NONE;
        helper_funcs[0].return_nullability = BINDGEN_NULLABILITY_NA;
        helper_funcs[0].return_teardown = BINDGEN_TEARDOWN_NONE;

        toml_set_default(&helper_params[0].name, 64, "buffer");
        toml_set_default(&helper_params[0].c_type, 64, "char *");
        toml_set_default(&helper_params[0].ffi_type, 16, "'ptr");
        toml_set_default(&helper_params[0].omni_type, 16, "ForeignHandle");
        helper_params[0].role = BINDGEN_ROLE_STRING_BUFFER;
        helper_params[0].ownership = BINDGEN_OWNERSHIP_BORROWED;
        helper_params[0].nullability = BINDGEN_NULLABILITY_UNKNOWN;
        helper_params[0].buffer_direction = BINDGEN_BUFFER_DIRECTION_OUT;
        helper_params[0].buffer_size_param_index = 1;
        helper_params[0].teardown = BINDGEN_TEARDOWN_NONE;

        toml_set_default(&helper_params[1].name, 64, "buffer_len");
        toml_set_default(&helper_params[1].c_type, 64, "int");
        toml_set_default(&helper_params[1].ffi_type, 16, "'int");
        toml_set_default(&helper_params[1].omni_type, 16, "Integer");
        helper_params[1].role = BINDGEN_ROLE_VALUE;
        helper_params[1].ownership = BINDGEN_OWNERSHIP_NONE;
        helper_params[1].nullability = BINDGEN_NULLABILITY_NA;
        helper_params[1].buffer_direction = BINDGEN_BUFFER_DIRECTION_NA;
        helper_params[1].buffer_size_param_index = -1;
        helper_params[1].teardown = BINDGEN_TEARDOWN_NONE;

        char[4096] helper_buf;
        bool helper_ok = true;
        usz helper_pos = bindgen_append_facade_helper_stubs(helper_buf[..], 0, &helper_funcs[0], 1, &helper_ok);
        char[] generated_helpers = helper_buf[:helper_pos];
        bool none_helper_validates_policy = helper_ok && str_contains(
            generated_helpers,
            "(define (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n    ;; none teardown keeps pointer ownership with the caller after size/direction validation.\n    (if (== role \"string-buffer\")\n        nil\n        (raise \"bindgen/string-buffer-role-mismatch\"))\n    (if (== ownership \"borrowed\")\n        nil\n        (raise \"bindgen/string-buffer-ownership-mismatch\"))"
        );
        compiler_test_finish_bool(
            none_helper_validates_policy,
            "Compiler: bindgen none-teardown string-buffer helper validates role and ownership",
            "[FAIL] Compiler: bindgen none-teardown string-buffer helper validates role and ownership",
            &pass,
            &fail
        );
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
        bool has_const_string_suffix_param = str_contains(generated, "(omni-bindgen-const-string-suffix (^String name)) ^Integer") &&
            str_contains(generated, "bindgen-policy: parameter name=name c-type=const char * role=string-input ownership=borrowed nullability=unknown teardown=none");
        bool pointer_const_char_buffer_is_mutable = str_contains(generated, "(omni-bindgen-pointer-const-buffer (^ForeignHandle buffer) (^Integer buffer_len)) ^String") &&
            str_contains(generated, "bindgen-policy: parameter name=buffer c-type=char *const role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
        bool char_pointer_pointer_param_is_handle = str_contains(generated, "(omni-bindgen-string-list (^ForeignHandle items)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=items c-type=const char ** role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool mutable_char_pointer_pointer_param_is_handle = str_contains(generated, "(omni-bindgen-mut-string-list (^ForeignHandle items)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=items c-type=char ** role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool has_buffer_foreign_handle = str_contains(generated, "(^ForeignHandle buffer)");
        bool has_buffer_size_param = str_contains(generated, "(^Integer buffer_len)");
        bool has_string_input = str_contains(generated, "bindgen-policy: parameter name=name c-type=const char * role=string-input ownership=borrowed nullability=unknown teardown=none");
        bool has_string_buffer = str_contains(generated, "bindgen-policy: parameter name=buffer c-type=char * role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
        bool named_inout_buffer_direction = str_contains(generated, "(omni-bindgen-inout-named-buffer (^ForeignHandle inout_buffer) (^Integer inout_buffer_len)) ^String") &&
            str_contains(generated, "bindgen-policy: parameter name=inout_buffer c-type=char * role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=inout_buffer_len");
        bool has_foreign_handle = str_contains(generated, "(^ForeignHandle opaque_handle)");
        bool has_foreign_handle_meta = str_contains(generated, "bindgen-policy: parameter name=opaque_handle c-type=void * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool has_callback_meta = str_contains(generated, "bindgen-policy: parameter name=callback c-type=void (*)(int) role=callback ownership=borrowed nullability=unknown teardown=callback-unregister");
        bool char_return_callback_is_handle = str_contains(generated, "(omni-bindgen-char-callback (^ForeignHandle callback)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=callback c-type=char (*)(int) role=callback ownership=borrowed nullability=unknown teardown=callback-unregister");
        bool has_function_shape = str_contains(generated, "bindgen-descriptor: callable type=ForeignCallable runtime=c-abi kind=function c-name=omni_bindgen_buffer name=omni-bindgen-buffer");
        bool has_param_shape = str_contains(generated, "bindgen-descriptor: parameter name=buffer c-type=char * abi-type='ptr type=ForeignHandle role=string-buffer ownership=borrowed nullability=unknown teardown=manual-review buffer-direction=inout size-param=buffer_len");
        bool has_return_shape = str_contains(generated, "bindgen-descriptor: returns c-type=const char * abi-type='string type=String role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
        bool has_fallback = str_contains(generated, "(define [ffi lambda _lib] (omni-bindgen-unnamed (^Integer arg0)) ^Integer)");
        bool has_void = str_contains(generated, ") ^Void)");
        bool has_ret_foreign_handle = str_contains(generated, ") ^ForeignHandle)");
        bool has_ret_string_meta = str_contains(generated, "bindgen-policy: returns c-type=const char * role=string-return ownership=borrowed nullability=unknown teardown=manual-review");
        bool char_pointer_pointer_return_is_handle = str_contains(generated, "(omni-bindgen-ret-string-list) ^ForeignHandle") &&
            str_contains(generated, "bindgen-policy: returns c-type=const char ** role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
        bool unsigned_char_param_is_handle = str_contains(generated, "(omni-bindgen-bytes (^ForeignHandle bytes)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=bytes c-type=const unsigned char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool unsigned_char_return_is_handle = str_contains(generated, "(omni-bindgen-ret-bytes) ^ForeignHandle") &&
            str_contains(generated, "bindgen-policy: returns c-type=const unsigned char * role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
        bool mutable_unsigned_char_param_is_handle = str_contains(generated, "(omni-bindgen-mut-bytes (^ForeignHandle bytes) (^Integer bytes_len)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=bytes c-type=unsigned char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool signed_char_param_is_handle = str_contains(generated, "(omni-bindgen-signed-bytes (^ForeignHandle bytes)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=bytes c-type=const signed char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool signed_char_return_is_handle = str_contains(generated, "(omni-bindgen-ret-signed-bytes) ^ForeignHandle") &&
            str_contains(generated, "bindgen-policy: returns c-type=const signed char * role=opaque-pointer ownership=unknown nullability=unknown teardown=manual-review");
        bool mutable_signed_char_param_is_handle = str_contains(generated, "(omni-bindgen-mut-signed-bytes (^ForeignHandle bytes) (^Integer bytes_len)) ^Void") &&
            str_contains(generated, "bindgen-policy: parameter name=bytes c-type=signed char * role=opaque-pointer ownership=borrowed nullability=unknown teardown=none");
        bool unsigned_char_not_string = !str_contains(generated, "(omni-bindgen-bytes (^String bytes))") &&
            !str_contains(generated, "(omni-bindgen-ret-bytes) ^String") &&
            !str_contains(generated, "c-type=unsigned char * role=string-buffer");
        bool signed_char_not_string = !str_contains(generated, "(omni-bindgen-signed-bytes (^String bytes))") &&
            !str_contains(generated, "(omni-bindgen-ret-signed-bytes) ^String") &&
            !str_contains(generated, "c-type=signed char * role=string-buffer");
        raw_ok = has_raw_module && has_integer && has_const_string_param && has_buffer_foreign_handle &&
            has_buffer_size_param &&
            has_string_input && has_string_buffer &&
            has_foreign_handle && has_foreign_handle_meta && has_callback_meta && has_fallback &&
            char_return_callback_is_handle && has_function_shape && has_param_shape && has_return_shape &&
            has_void && has_ret_foreign_handle && has_ret_string_meta &&
            has_const_string_suffix_param && pointer_const_char_buffer_is_mutable &&
            char_pointer_pointer_param_is_handle && mutable_char_pointer_pointer_param_is_handle &&
            char_pointer_pointer_return_is_handle && named_inout_buffer_direction;
        compiler_test_finish_bool(
            raw_ok,
            "Compiler: bindgen raw module preserves canonical foreign-handle names and metadata annotations",
            "[FAIL] Compiler: bindgen raw module preserves canonical foreign-handle names and metadata annotations",
            &pass,
            &fail
        );
        compiler_test_finish_bool(
            unsigned_char_param_is_handle && unsigned_char_return_is_handle &&
                mutable_unsigned_char_param_is_handle && signed_char_param_is_handle &&
                signed_char_return_is_handle && mutable_signed_char_param_is_handle &&
                unsigned_char_not_string && signed_char_not_string,
            "Compiler: bindgen keeps signed/unsigned char pointers opaque instead of string-shaped",
            "[FAIL] Compiler: bindgen keeps signed/unsigned char pointers opaque instead of string-shaped",
            &pass,
            &fail
        );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types load generated raw module");
    }

    bool grouped_generated_ok = generate_ffi_module_pair_with_syntax(
        "bindgen-canonical",
        "bindgen",
        funcs,
        func_count,
        grouped_raw_output_path,
        output_path,
        BINDGEN_RAW_SYNTAX_GROUPED
    );
    if (!grouped_generated_ok) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types generate grouped raw module");
    } else if (try generated = io::file::load_temp((String)grouped_raw_output_path)) {
        bool has_grouped_module = str_contains(generated, "(define [ffi module] _lib \"libbindgen.so\"");
        bool has_grouped_syntax_note = str_contains(generated, ";; Raw syntax: grouped");
        bool has_grouped_int = str_contains(generated, "(omni-bindgen-int (^Integer count)) ^Integer");
        bool has_grouped_buffer = str_contains(generated, "(omni-bindgen-buffer (^ForeignHandle buffer) (^Integer buffer_len)) ^String");
        bool has_grouped_fallback = str_contains(generated, "(omni-bindgen-unnamed (^Integer arg0)) ^Integer");
        bool has_grouped_meta = str_contains(generated, "bindgen-descriptor: callable type=ForeignCallable runtime=c-abi kind=function c-name=omni_bindgen_buffer name=omni-bindgen-buffer");
        bool omits_legacy_lambda = !str_contains(generated, "(define [ffi lambda _lib]");
        compiler_test_finish_bool(
            has_grouped_module && has_grouped_syntax_note && has_grouped_int && has_grouped_buffer &&
                has_grouped_fallback && has_grouped_meta && omits_legacy_lambda,
            "Compiler: bindgen can emit grouped ffi module raw bindings",
            "[FAIL] Compiler: bindgen can emit grouped ffi module raw bindings",
            &pass,
            &fail
        );
    } else {
        fail++;
        io::printn("[FAIL] Compiler: bindgen canonical types load grouped raw module");
    }

    char[64][TOML_MAX_STRIP_PREFIXES] strip_prefixes;
    for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
        toml_clear_buf(&strip_prefixes[i], 64);
    }
    toml_set_default(&strip_prefixes[0], 64, "omni_bindgen_");
    bool stripped_generated_ok = generate_ffi_module_pair_with_options(
        "bindgen-canonical-strip",
        "bindgen",
        funcs,
        func_count,
        stripped_raw_output_path,
        stripped_output_path,
        BINDGEN_RAW_SYNTAX_GROUPED,
        &strip_prefixes,
        1
    );
    if (!stripped_generated_ok) {
        fail++;
        io::printn("[FAIL] Compiler: bindgen strip-prefixes generate grouped raw/facade modules");
    } else {
        bool stripped_raw_ok = false;
        bool stripped_facade_ok = false;
        if (try generated = io::file::load_temp((String)stripped_raw_output_path)) {
            stripped_raw_ok =
                str_contains(generated, "(module ffi-bindgen-canonical-strip-raw (export omni-bindgen-int") &&
                str_contains(generated, "(omni-bindgen-int (^Integer count)) ^Integer") &&
                str_contains(generated, "c-name=omni_bindgen_buffer name=omni-bindgen-buffer") &&
                !str_contains(generated, "(module ffi-bindgen-canonical-strip-raw (export int ");
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen strip-prefixes load grouped raw module");
        }
        if (try generated = io::file::load_temp((String)stripped_output_path)) {
            stripped_facade_ok =
                str_contains(generated, "(module ffi-bindgen-canonical-strip (export") &&
                str_contains(generated, "(omni-bindgen-int 'as raw-int)") &&
                str_contains(generated, "(omni-bindgen-const-string-suffix 'as raw-const-string-suffix)") &&
                str_contains(generated, "(omni-bindgen-pointer-const-buffer 'as raw-pointer-const-buffer)") &&
                str_contains(generated, "(define int raw-int)") &&
                str_contains(generated, "(define const-string-suffix raw-const-string-suffix)") &&
                str_contains(generated, "(define (buffer (^ForeignHandle buffer) (^Integer buffer_len))") &&
                str_contains(generated, "(define (pointer-const-buffer (^ForeignHandle buffer) (^Integer buffer_len))") &&
                str_contains(generated, "(raw-buffer buffer_input buffer_len)") &&
                str_contains(generated, "(raw-pointer-const-buffer buffer_input buffer_len)") &&
                !str_contains(generated, "(define omni-bindgen-int raw-omni-bindgen-int)");
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen strip-prefixes load facade module");
        }
        compiler_test_finish_bool(
            stripped_raw_ok && stripped_facade_ok,
            "Compiler: bindgen strip-prefixes preserve raw C symbols and strip facade names",
            "[FAIL] Compiler: bindgen strip-prefixes preserve raw C symbols and strip facade names",
            &pass,
            &fail
        );
    }

    {
        ParsedFunc[1] invalid_strip_funcs;
        invalid_strip_funcs[0] = {};
        toml_set_default(&invalid_strip_funcs[0].name, 128, "sqlite3_3d_distance");
        toml_set_default(&invalid_strip_funcs[0].return_c_type, 64, "int");
        toml_set_default(&invalid_strip_funcs[0].return_ffi_type, 16, "'int");
        toml_set_default(&invalid_strip_funcs[0].return_omni_type, 16, "Integer");
        invalid_strip_funcs[0].params = null;
        invalid_strip_funcs[0].param_count = 0;
        invalid_strip_funcs[0].param_capacity = 0;
        invalid_strip_funcs[0].is_variadic = false;
        invalid_strip_funcs[0].return_role = BINDGEN_ROLE_VALUE;
        invalid_strip_funcs[0].return_ownership = BINDGEN_OWNERSHIP_NONE;
        invalid_strip_funcs[0].return_nullability = BINDGEN_NULLABILITY_NA;
        invalid_strip_funcs[0].return_teardown = BINDGEN_TEARDOWN_NONE;

        char[64][TOML_MAX_STRIP_PREFIXES] invalid_strip_prefixes;
        for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
            toml_clear_buf(&invalid_strip_prefixes[i], 64);
        }
        toml_set_default(&invalid_strip_prefixes[0], 64, "sqlite3_");

        bool rejects_digit_leading_strip = !generate_ffi_module_pair_with_options(
            "bindgen-invalid-strip",
            "bindgen",
            &invalid_strip_funcs[0],
            1,
            invalid_strip_raw_output_path,
            invalid_strip_output_path,
            BINDGEN_RAW_SYNTAX_GROUPED,
            &invalid_strip_prefixes,
            1
        );
        bool invalid_raw_not_written = !bindgen_output_file_exists(invalid_strip_raw_output_path);
        bool invalid_facade_not_written = !bindgen_output_file_exists(invalid_strip_output_path);
        compiler_test_finish_bool(
            rejects_digit_leading_strip && invalid_raw_not_written && invalid_facade_not_written,
            "Compiler: bindgen strip-prefixes reject digit-leading emitted names before writing",
            "[FAIL] Compiler: bindgen strip-prefixes reject digit-leading emitted names before writing",
            &pass,
            &fail
        );
    }

    {
        ParsedFunc[2] collision_strip_funcs;
        collision_strip_funcs[0] = {};
        collision_strip_funcs[1] = {};
        toml_set_default(&collision_strip_funcs[0].name, 128, "sqlite3_open");
        toml_set_default(&collision_strip_funcs[1].name, 128, "open");
        for (usz i = 0; i < 2; i++) {
            toml_set_default(&collision_strip_funcs[i].return_c_type, 64, "int");
            toml_set_default(&collision_strip_funcs[i].return_ffi_type, 16, "'int");
            toml_set_default(&collision_strip_funcs[i].return_omni_type, 16, "Integer");
            collision_strip_funcs[i].params = null;
            collision_strip_funcs[i].param_count = 0;
            collision_strip_funcs[i].param_capacity = 0;
            collision_strip_funcs[i].is_variadic = false;
            collision_strip_funcs[i].return_role = BINDGEN_ROLE_VALUE;
            collision_strip_funcs[i].return_ownership = BINDGEN_OWNERSHIP_NONE;
            collision_strip_funcs[i].return_nullability = BINDGEN_NULLABILITY_NA;
            collision_strip_funcs[i].return_teardown = BINDGEN_TEARDOWN_NONE;
        }

        char[64][TOML_MAX_STRIP_PREFIXES] collision_strip_prefixes;
        for (usz i = 0; i < TOML_MAX_STRIP_PREFIXES; i++) {
            toml_clear_buf(&collision_strip_prefixes[i], 64);
        }
        toml_set_default(&collision_strip_prefixes[0], 64, "sqlite3_");

        bool rejects_emitted_name_collision = !generate_ffi_module_pair_with_options(
            "bindgen-collision-strip",
            "bindgen",
            &collision_strip_funcs[0],
            2,
            collision_strip_raw_output_path,
            collision_strip_output_path,
            BINDGEN_RAW_SYNTAX_GROUPED,
            &collision_strip_prefixes,
            1
        );
        bool collision_raw_not_written = !bindgen_output_file_exists(collision_strip_raw_output_path);
        bool collision_facade_not_written = !bindgen_output_file_exists(collision_strip_output_path);
        compiler_test_finish_bool(
            rejects_emitted_name_collision && collision_raw_not_written && collision_facade_not_written,
            "Compiler: bindgen strip-prefixes reject emitted name collisions before writing",
            "[FAIL] Compiler: bindgen strip-prefixes reject emitted name collisions before writing",
            &pass,
            &fail
        );
    }

    {
        char[512] facade_fail_raw_path_buf;
        char[512] facade_fail_path_buf;
        char[] facade_fail_raw_path;
        char[] facade_fail_path;
        if (try rendered = io::bprintf(
            &facade_fail_raw_path_buf,
            "/tmp/omni_bindgen_facade_fail-%d-%d_raw.omni",
            pid,
            (int)seq
        )) {
            facade_fail_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose facade-failure raw output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &facade_fail_path_buf,
            "/tmp/omni_bindgen_missing_facade_dir-%d-%d/facade.omni",
            pid,
            (int)seq
        )) {
            facade_fail_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose facade-failure output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)facade_fail_raw_path);
        defer (void)io::file::delete((String)facade_fail_path);

        bool rejects_facade_write_failure = !generate_ffi_module_pair(
            "bindgen-facade-fail",
            "bindgen",
            funcs,
            func_count,
            facade_fail_raw_path,
            facade_fail_path
        );
        compiler_test_finish_bool(
            rejects_facade_write_failure &&
                !bindgen_output_file_exists(facade_fail_raw_path) &&
                !bindgen_output_file_exists(facade_fail_path),
            "Compiler: bindgen cleans new raw output when facade write fails",
            "[FAIL] Compiler: bindgen cleans new raw output when facade write fails",
            &pass,
            &fail
        );
    }

    {
        char[512] facade_fail_rerun_raw_path_buf;
        char[512] facade_fail_rerun_path_buf;
        char[] facade_fail_rerun_raw_path;
        char[] facade_fail_rerun_path;
        if (try rendered = io::bprintf(
            &facade_fail_rerun_raw_path_buf,
            "/tmp/omni_bindgen_facade_fail_rerun-%d-%d_raw.omni",
            pid,
            (int)seq
        )) {
            facade_fail_rerun_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose facade-fail rerun raw output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &facade_fail_rerun_path_buf,
            "/tmp/omni_bindgen_missing_facade_dir_rerun-%d-%d/facade.omni",
            pid,
            (int)seq
        )) {
            facade_fail_rerun_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose facade-fail rerun facade output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)facade_fail_rerun_raw_path);
        defer (void)io::file::delete((String)facade_fail_rerun_path);
        if (!compiler_write_text_file(facade_fail_rerun_raw_path, ";; existing raw artifact\n")) {
            fail++;
            io::printn("[FAIL] Compiler: bindgen seed rerun facade-failure raw artifact");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }

        bool rejects_facade_write_failure_on_existing_raw = !generate_ffi_module_pair(
            "bindgen-facade-fail-rerun",
            "bindgen",
            funcs,
            func_count,
            facade_fail_rerun_raw_path,
            facade_fail_rerun_path
        );
        compiler_test_finish_bool(
            rejects_facade_write_failure_on_existing_raw &&
                bindgen_output_file_exists(facade_fail_rerun_raw_path) &&
                !bindgen_output_file_exists(facade_fail_rerun_path),
            "Compiler: bindgen preserves existing raw output on rerun facade-write failure",
            "[FAIL] Compiler: bindgen preserves existing raw output on rerun facade-write failure",
            &pass,
            &fail
        );
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
        bool has_buffer_manual_review_guard = str_contains(generated, "(raise \"bindgen/string-buffer-manual-review\")");
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
        bool registration_fails_closed_for_generic_callback = str_contains(generated, "(raise \"bindgen/callback-registration-requires-subsystem-shim\")");
        bool unregister_fails_closed_for_generic_callback = str_contains(generated, "(raise \"bindgen/callback-unregister-requires-subsystem-shim\")");
        bool unregister_nil_cleanup_is_idempotent = str_contains(
            generated,
            "(if (nil? callback-handle)\n        nil\n        (if (== role \"callback\")\n            (if (== ownership \"borrowed\")\n                (if (== teardown \"callback-unregister\")\n                    (raise \"bindgen/callback-unregister-requires-subsystem-shim\")"
        );
        bool does_not_register_callback_via_uv_shim = !str_contains(generated, "(__raw-uv-timer-callback-handle callback)");
        bool does_not_unregister_callback_via_uv_shim = !str_contains(generated, "(__raw-uv-timer-callback-unhandle callback-handle)");
        bool has_opaque_arg_helper = str_contains(generated, "(define (bindgen-wrap-opaque-arg handle role ownership nullability teardown)");
        bool has_string_return_borrowed_helper = str_contains(generated, "(define (bindgen-wrap-string-return-borrowed text role ownership nullability teardown)");
        bool has_string_return_unknown_helper = str_contains(generated, "(define (bindgen-wrap-string-return-unknown text role ownership nullability teardown)");
        bool has_opaque_return_manual_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-manual-review handle role ownership nullability teardown)");
        bool has_opaque_return_none_helper = str_contains(generated, "(define (bindgen-wrap-opaque-return-none handle role ownership nullability teardown)");
        bool flags_buffer_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-buffer — string return ownership/nullability is not automatic; mutable char* buffer direction/size must be wrapped explicitly.");
        bool flags_foreign_handle_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-void-ptr — opaque pointer ownership/lifetime must be reviewed.");
        bool flags_callback_review = str_contains(generated, ";; REVIEW(bindgen): review omni-bindgen-callback — callback registration and teardown must be wrapped explicitly.");
        bool wraps_buffer = str_contains(generated, "(define (omni-bindgen-buffer (^ForeignHandle buffer) (^Integer buffer_len))");
        bool wraps_foreign_handle = str_contains(generated, "(define (omni-bindgen-void-ptr (^ForeignHandle opaque_handle))");
        bool wraps_callback = str_contains(generated, "(define (omni-bindgen-callback (^ForeignHandle callback))");
        bool wraps_string_return = str_contains(generated, "(define (omni-bindgen-ret-string)");
        bool wraps_foreign_handle_return = str_contains(generated, "(define (omni-bindgen-ret-ptr)");
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
        bool wraps_foreign_handle_result = str_contains(generated, "(let (result (raw-omni-bindgen-ret-ptr)") &&
            str_contains(generated, "result_handle result");
        bool binds_callback_handle = str_contains(generated, "(let (callback_handle (bindgen-register-callback callback \"callback\" \"borrowed\" \"unknown\" \"callback-unregister\"))\n");
        bool binds_foreign_handle_handle = str_contains(generated, "(let (opaque_handle_handle (bindgen-wrap-opaque-arg opaque_handle \"opaque-pointer\" \"borrowed\" \"unknown\" \"none\"))\n");
        bool has_buffer_body_note = str_contains(generated, "REVIEW: mutable char* buffers need explicit allocation, size, and output semantics.");
        bool has_callback_body_note = str_contains(generated, "REVIEW: confirm callback-handle registration/unregister policy matches the target subsystem contract.");
        bool has_foreign_handle_return_note = str_contains(generated, "REVIEW: wrap opaque returns in a handle/finalizer policy before exposing them broadly.");
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
                has_buffer_role_guard && has_buffer_ownership_guard && has_buffer_manual_review_guard &&
                has_buffer_size_required_guard && has_buffer_size_type_guard &&
                has_buffer_size_positive_guard && has_buffer_direction_guard &&
                has_callback_helper && has_callback_cleanup_helper && has_opaque_arg_helper &&
                has_callback_nil_guard && has_callback_role_guard &&
                has_callback_ownership_guard && has_callback_unsupported_teardown_guard &&
                registration_fails_closed_for_generic_callback &&
                unregister_fails_closed_for_generic_callback &&
                unregister_nil_cleanup_is_idempotent &&
                does_not_register_callback_via_uv_shim &&
                does_not_unregister_callback_via_uv_shim &&
                has_string_return_borrowed_helper && has_string_return_unknown_helper &&
                has_opaque_return_manual_helper && has_opaque_return_none_helper &&
                flags_buffer_review && flags_foreign_handle_review && flags_callback_review && wraps_buffer && wraps_foreign_handle &&
                wraps_callback && wraps_string_return && has_buffer_body_note &&
                has_callback_body_note && forwards_callback &&
                wraps_buffer_result && wraps_return_result &&
                wraps_foreign_handle_return && wraps_foreign_handle_result && has_foreign_handle_return_note &&
                binds_callback_handle && binds_foreign_handle_handle &&
                routes_buffer_through_helper && routes_callback_through_helper && routes_opaque_through_helper &&
                wraps_borrowed_string_return_via_helper &&
                wraps_unknown_string_return_via_helper &&
                wraps_opaque_return_via_helper &&
                emits_callback_cleanup,
            "Compiler: bindgen facade module imports raw, scaffolds foreign-handle wrappers, and flags manual review",
            "[FAIL] Compiler: bindgen facade module imports raw, scaffolds foreign-handle wrappers, and flags manual review",
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


fn void run_compiler_group_bindgen_path_guard_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    char[256] out_path;
    usz path_len = 0;
    char[32] project_dir;
    char[64] safe_dep;
    char[64] traversal_dep;
    char[64] nested_dep;
    char[64] windows_nested_dep;
    char[64] dotted_dep;
    char[64] spaced_dep;
    char[64] paren_dep;
    char[64] empty_dep;
    toml_set_default(&project_dir, 32, "/tmp/omni-bind-project");
    toml_set_default(&safe_dep, 64, "math-core_1");
    toml_set_default(&traversal_dep, 64, "../escape");
    toml_set_default(&nested_dep, 64, "nested/escape");
    toml_set_default(&windows_nested_dep, 64, "nested\\escape");
    toml_set_default(&dotted_dep, 64, "nested.escape");
    toml_set_default(&spaced_dep, 64, "bad name");
    toml_set_default(&paren_dep, 64, "bad)name");
    toml_clear_buf(&empty_dep, 64);

    bool safe_ok = main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &safe_dep) &&
        str_contains(out_path[:path_len], "/tmp/omni-bind-project/lib/ffi/math-core_1.omni");
    bool rejects_unsafe =
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &traversal_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &nested_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &windows_nested_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &dotted_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &spaced_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &paren_dep) &&
        !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &empty_dep);

    compiler_test_finish_bool(
        safe_ok && rejects_unsafe,
        "Compiler: bindgen output paths reject unsafe dependency names",
        "[FAIL] Compiler: bindgen output paths reject unsafe dependency names",
        &pass,
        &fail
    );

    {
        TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
        if (config == null) {
            fail++;
            io::printn("[FAIL] Compiler: bindgen rejects overlong dependency section names");
        } else {
            defer mem::free(config);
            char[] source =
                "[dependencies.ffi.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl]\n"
                "library = \"m\"\n"
                "headers = [\"/usr/include/math.h\"]\n";
            parse_toml(source, config);
            bool overlong_rejected =
                config.ffi_dep_count == 1 &&
                config.ffi_deps[0].name_invalid &&
                config.ffi_deps[0].name[0] == 0 &&
                !main::build_bind_output_path(out_path[..], &path_len, &project_dir, 22, &config.ffi_deps[0].name);
            compiler_test_finish_bool(
                overlong_rejected,
                "Compiler: bindgen rejects overlong dependency section names",
                "[FAIL] Compiler: bindgen rejects overlong dependency section names",
                &pass,
                &fail
            );
        }
    }

    {
        TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
        if (config == null) {
            fail++;
            io::printn("[FAIL] Compiler: bindgen rejects duplicate dependency section names");
        } else {
            defer mem::free(config);
            char[] source =
                "[dependencies.ffi.sqlite]\n"
                "library = \"sqlite3\"\n"
                "headers = [\"/usr/include/sqlite3.h\"]\n"
                "[dependencies.ffi.sqlite]\n"
                "library = \"m\"\n"
                "headers = [\"/usr/include/math.h\"]\n";
            parse_toml(source, config);
            bool duplicate_rejected =
                config.ffi_dep_count == 2 &&
                config.ffi_deps[0].name_invalid &&
                config.ffi_deps[1].name_invalid;
            compiler_test_finish_bool(
                duplicate_rejected,
                "Compiler: bindgen rejects duplicate dependency section names",
                "[FAIL] Compiler: bindgen rejects duplicate dependency section names",
                &pass,
                &fail
            );
        }
    }

    {
        TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
        if (config == null) {
            fail++;
            io::printn("[FAIL] Compiler: bindgen rejects dependency section count overflow");
        } else {
            defer mem::free(config);
            char[] source =
                "[dependencies.ffi.dep00]\nlibrary = \"m\"\nheaders = [\"/tmp/dep00.h\"]\n"
                "[dependencies.ffi.dep01]\nlibrary = \"m\"\nheaders = [\"/tmp/dep01.h\"]\n"
                "[dependencies.ffi.dep02]\nlibrary = \"m\"\nheaders = [\"/tmp/dep02.h\"]\n"
                "[dependencies.ffi.dep03]\nlibrary = \"m\"\nheaders = [\"/tmp/dep03.h\"]\n"
                "[dependencies.ffi.dep04]\nlibrary = \"m\"\nheaders = [\"/tmp/dep04.h\"]\n"
                "[dependencies.ffi.dep05]\nlibrary = \"m\"\nheaders = [\"/tmp/dep05.h\"]\n"
                "[dependencies.ffi.dep06]\nlibrary = \"m\"\nheaders = [\"/tmp/dep06.h\"]\n"
                "[dependencies.ffi.dep07]\nlibrary = \"m\"\nheaders = [\"/tmp/dep07.h\"]\n"
                "[dependencies.ffi.dep08]\nlibrary = \"m\"\nheaders = [\"/tmp/dep08.h\"]\n"
                "[dependencies.ffi.dep09]\nlibrary = \"m\"\nheaders = [\"/tmp/dep09.h\"]\n"
                "[dependencies.ffi.dep10]\nlibrary = \"m\"\nheaders = [\"/tmp/dep10.h\"]\n"
                "[dependencies.ffi.dep11]\nlibrary = \"m\"\nheaders = [\"/tmp/dep11.h\"]\n"
                "[dependencies.ffi.dep12]\nlibrary = \"m\"\nheaders = [\"/tmp/dep12.h\"]\n"
                "[dependencies.ffi.dep13]\nlibrary = \"m\"\nheaders = [\"/tmp/dep13.h\"]\n"
                "[dependencies.ffi.dep14]\nlibrary = \"m\"\nheaders = [\"/tmp/dep14.h\"]\n"
                "[dependencies.ffi.dep15]\nlibrary = \"m\"\nheaders = [\"/tmp/dep15.h\"]\n"
                "[dependencies.ffi.dep16]\nlibrary = \"m\"\nheaders = [\"/tmp/dep16.h\"]\n";
            parse_toml(source, config);
            bool overflow_rejected =
                config.ffi_dep_count == TOML_MAX_DEPS &&
                config.ffi_dep_limit_hit &&
                bindgen_cstr_eq(&config.ffi_deps[TOML_MAX_DEPS - 1].name, "dep15") &&
                bindgen_cstr_eq(&config.ffi_deps[TOML_MAX_DEPS - 1].library, "m");
            compiler_test_finish_bool(
                overflow_rejected,
                "Compiler: bindgen rejects dependency section count overflow",
                "[FAIL] Compiler: bindgen rejects dependency section count overflow",
                &pass,
                &fail
            );
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
    ParseHeaderResult long_path_result = parse_header_checked(&dummy, long_header_buf[..long_len], null, 0, null, 0, false, null, 0, false);
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

    ParseHeaderResult limit_result = parse_header_checked(&clang, header_path, funcs, MAX_FUNCS, null, 0, false, null, 0, false);
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
    ParseHeaderResult alloc_fail_result = parse_header_checked(&clang, header_path, funcs, 4, null, 0, false, null, 0, false);
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

    {
        TomlFfiDep filtered_dep = {};
        toml_set_default(&filtered_dep.name, 64, "filtered");
        toml_set_default(&filtered_dep.library, 128, "m");
        toml_set_default(&filtered_dep.raw_syntax, 16, "legacy");
        toml_set_default(&filtered_dep.headers[0], TOML_MAX_STR, header_path);
        filtered_dep.header_count = 1;
        filtered_dep.filter_functions = true;
        filtered_dep.function_count = 2;
        toml_set_default(&filtered_dep.functions[0], 64, "omni_bindgen_limit_a");
        toml_set_default(&filtered_dep.functions[1], 64, "omni_bindgen_limit_missing");
        ParsedFunc[4] filtered_funcs;
        for (usz i = 0; i < 4; i++) {
            filtered_funcs[i] = {};
        }
        usz filtered_bindings = 0;
        usz filtered_libs = 0;
        bool rejects_missing_filter = !main::process_bind_dependency(
            "."[..],
            1,
            &clang,
            &filtered_dep,
            &filtered_funcs[0],
            4,
            &filtered_bindings,
            &filtered_libs
        );
        compiler_test_finish_bool(
            rejects_missing_filter &&
                filtered_bindings == 0 &&
                filtered_libs == 0,
            "Compiler: bindgen rejects missing explicit function filters",
            "[FAIL] Compiler: bindgen rejects missing explicit function filters",
            &pass,
            &fail
        );
    }

    {
        char[512] project_path_buf;
        char[512] project_lib_path_buf;
        char[512] project_ffi_path_buf;
        char[512] raw_path_buf;
        char[512] facade_path_buf;
        char[512] manifest_path_buf;
        char[] project_path;
        char[] project_lib_path;
        char[] project_ffi_path;
        char[] raw_path;
        char[] facade_path;
        char[] manifest_path;
        if (try rendered = io::bprintf(
            &project_path_buf,
            "/tmp/omni_bindgen_exclude_functions-%d-%d",
            pid,
            (int)(seq + 2)
        )) {
            project_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose project path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&project_lib_path_buf, "%s/lib", (ZString)&project_path_buf[0])) {
            project_lib_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose lib path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&project_ffi_path_buf, "%s/lib/ffi", (ZString)&project_path_buf[0])) {
            project_ffi_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose ffi path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&raw_path_buf, "%s/lib/ffi/excludeok_raw.omni", (ZString)&project_path_buf[0])) {
            raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose raw path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&facade_path_buf, "%s/lib/ffi/excludeok.omni", (ZString)&project_path_buf[0])) {
            facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose facade path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&manifest_path_buf, "%s/lib/ffi/excludeok_manifest.toml", (ZString)&project_path_buf[0])) {
            manifest_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen exclude-functions compose manifest path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }

        (void)compiler_test_mkdir(&project_path_buf[0], 0o755);
        (void)compiler_test_mkdir(&project_lib_path_buf[0], 0o755);
        (void)compiler_test_mkdir(&project_ffi_path_buf[0], 0o755);
        defer (void)io::file::delete((String)manifest_path);
        defer (void)io::file::delete((String)facade_path);
        defer (void)io::file::delete((String)raw_path);
        defer (void)compiler_test_rmdir(&project_ffi_path_buf[0]);
        defer (void)compiler_test_rmdir(&project_lib_path_buf[0]);
        defer (void)compiler_test_rmdir(&project_path_buf[0]);

        TomlFfiDep exclude_dep = {};
        toml_set_default(&exclude_dep.name, 64, "excludeok");
        toml_set_default(&exclude_dep.library, 128, "m");
        toml_set_default(&exclude_dep.raw_syntax, 16, "grouped");
        toml_set_default(&exclude_dep.headers[0], TOML_MAX_STR, header_path);
        exclude_dep.header_count = 1;
        exclude_dep.function_count = 3;
        exclude_dep.filter_functions = true;
        toml_set_default(&exclude_dep.functions[0], 64, "omni_bindgen_limit_a");
        toml_set_default(&exclude_dep.functions[1], 64, "omni_bindgen_limit_b");
        toml_set_default(&exclude_dep.functions[2], 64, "omni_bindgen_limit_c");
        exclude_dep.exclude_function_count = 1;
        exclude_dep.exclude_functions_seen = true;
        toml_set_default(&exclude_dep.exclude_functions[0], 64, "omni_bindgen_limit_b");

        ParsedFunc[4] exclude_funcs;
        for (usz i = 0; i < 4; i++) {
            exclude_funcs[i] = {};
        }
        usz exclude_bindings = 0;
        usz exclude_libs = 0;
        bool exclude_bind_ok = main::process_bind_dependency(
            project_path,
            project_path.len,
            &clang,
            &exclude_dep,
            &exclude_funcs[0],
            4,
            &exclude_bindings,
            &exclude_libs
        );
        bool exclude_outputs_ok = false;
        if (exclude_bind_ok) {
            if (try raw_generated = io::file::load_temp((String)raw_path)) {
                if (try facade_generated = io::file::load_temp((String)facade_path)) {
                    if (try manifest_generated = io::file::load_temp((String)manifest_path)) {
                        exclude_outputs_ok =
                            str_contains(raw_generated, "omni-bindgen-limit-a") &&
                            !str_contains(raw_generated, "omni-bindgen-limit-b") &&
                            str_contains(raw_generated, "omni-bindgen-limit-c") &&
                            str_contains(facade_generated, "omni-bindgen-limit-a") &&
                            !str_contains(facade_generated, "omni-bindgen-limit-b") &&
                            str_contains(facade_generated, "omni-bindgen-limit-c") &&
                            str_contains(manifest_generated, "exclude-functions = [\"omni_bindgen_limit_b\"]");
                    }
                }
            }
        }
        compiler_test_finish_bool(
            exclude_bind_ok &&
                exclude_outputs_ok &&
                exclude_bindings == 2 &&
                exclude_libs == 1,
            "Compiler: bindgen exclude-functions omits denied functions from outputs",
            "[FAIL] Compiler: bindgen exclude-functions omits denied functions from outputs",
            &pass,
            &fail
        );
    }

    {
        char[512] missing_header_path_buf;
        char[] missing_header_path;
        if (try rendered = io::bprintf(
            &missing_header_path_buf,
            "/tmp/omni_bindgen_parse_guard_missing-%d-%d.h",
            pid,
            (int)(seq + 1)
        )) {
            missing_header_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen parse guards compose missing header path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)missing_header_path);

        TomlFfiDep partial_dep = {};
        toml_set_default(&partial_dep.name, 64, "partial-parse");
        toml_set_default(&partial_dep.library, 128, "m");
        toml_set_default(&partial_dep.raw_syntax, 16, "legacy");
        toml_set_default(&partial_dep.headers[0], TOML_MAX_STR, header_path);
        toml_set_default(&partial_dep.headers[1], TOML_MAX_STR, missing_header_path);
        partial_dep.header_count = 2;

        ParsedFunc[4] partial_funcs;
        for (usz i = 0; i < 4; i++) {
            partial_funcs[i] = {};
        }
        usz partial_bindings = 0;
        usz partial_libs = 0;
        bool rejects_partial_parse = !main::process_bind_dependency(
            "."[..],
            1,
            &clang,
            &partial_dep,
            &partial_funcs[0],
            4,
            &partial_bindings,
            &partial_libs
        );
        compiler_test_finish_bool(
            rejects_partial_parse &&
                partial_bindings == 0 &&
                partial_libs == 0 &&
                partial_funcs[0].params == null,
            "Compiler: bindgen frees partial header parse state on later parse failure",
            "[FAIL] Compiler: bindgen frees partial header parse state on later parse failure",
            &pass,
            &fail
        );
    }

    {
        char[512] project_path_buf;
        char[512] project_lib_path_buf;
        char[512] project_ffi_path_buf;
        char[512] manifest_dir_path_buf;
        char[512] raw_path_buf;
        char[512] facade_path_buf;
        char[] project_path;
        char[] project_lib_path;
        char[] project_ffi_path;
        char[] manifest_dir_path;
        char[] raw_path;
        char[] facade_path;
        if (try rendered = io::bprintf(
            &project_path_buf,
            "/tmp/omni_bindgen_manifest_fail-%d-%d",
            pid,
            (int)seq
        )) {
            project_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure project path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&project_lib_path_buf, "%s/lib", (ZString)&project_path_buf[0])) {
            project_lib_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure lib path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&project_ffi_path_buf, "%s/lib/ffi", (ZString)&project_path_buf[0])) {
            project_ffi_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure ffi path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&manifest_dir_path_buf, "%s/lib/ffi/manifestfail_manifest.toml", (ZString)&project_path_buf[0])) {
            manifest_dir_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure manifest path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&raw_path_buf, "%s/lib/ffi/manifestfail_raw.omni", (ZString)&project_path_buf[0])) {
            raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure raw path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(&facade_path_buf, "%s/lib/ffi/manifestfail.omni", (ZString)&project_path_buf[0])) {
            facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose manifest-failure facade path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }

        (void)compiler_test_mkdir(&project_path_buf[0], 0o755);
        (void)compiler_test_mkdir(&project_lib_path_buf[0], 0o755);
        (void)compiler_test_mkdir(&project_ffi_path_buf[0], 0o755);
        (void)compiler_test_mkdir(&manifest_dir_path_buf[0], 0o755);
        defer (void)io::file::delete((String)raw_path);
        defer (void)io::file::delete((String)facade_path);
        defer (void)compiler_test_rmdir(&manifest_dir_path_buf[0]);
        defer (void)compiler_test_rmdir(&project_ffi_path_buf[0]);
        defer (void)compiler_test_rmdir(&project_lib_path_buf[0]);
        defer (void)compiler_test_rmdir(&project_path_buf[0]);

        TomlFfiDep manifest_fail_dep = {};
        toml_set_default(&manifest_fail_dep.name, 64, "manifestfail");
        toml_set_default(&manifest_fail_dep.library, 128, "m");
        toml_set_default(&manifest_fail_dep.raw_syntax, 16, "legacy");
        toml_set_default(&manifest_fail_dep.headers[0], TOML_MAX_STR, header_path);
        manifest_fail_dep.header_count = 1;

        ParsedFunc[4] manifest_fail_funcs;
        for (usz i = 0; i < 4; i++) {
            manifest_fail_funcs[i] = {};
        }
        usz manifest_fail_bindings = 0;
        usz manifest_fail_libs = 0;
        bool rejects_manifest_write_failure = !main::process_bind_dependency(
            project_path,
            project_path.len,
            &clang,
            &manifest_fail_dep,
            &manifest_fail_funcs[0],
            4,
            &manifest_fail_bindings,
            &manifest_fail_libs
        );
        compiler_test_finish_bool(
            rejects_manifest_write_failure &&
                manifest_fail_bindings == 0 &&
                manifest_fail_libs == 0 &&
                !bindgen_output_file_exists(raw_path) &&
                !bindgen_output_file_exists(facade_path),
            "Compiler: bindgen cleans new raw/facade outputs when manifest write fails",
            "[FAIL] Compiler: bindgen cleans new raw/facade outputs when manifest write fails",
            &pass,
            &fail
        );
    }

    char[512] unsupported_path_buf;
    char[] unsupported_path;
    if (try rendered = io::bprintf(
        &unsupported_path_buf,
        "/tmp/omni_bindgen_unsupported_guard-%d-%d.h",
        pid,
        (int)(seq + 2)
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

    ParseHeaderResult unsupported_result = parse_header_checked(&clang, unsupported_path, funcs, 4, null, 0, false, null, 0, false);
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

    char[64] unsupported_exclude_name;
    toml_set_default(&unsupported_exclude_name, 64, "omni_bindgen_bad");
    ParseHeaderResult excluded_unsupported_result = parse_header_checked(
        &clang,
        unsupported_path,
        funcs,
        4,
        null,
        0,
        false,
        &unsupported_exclude_name,
        1,
        true
    );
    compiler_test_finish_bool(
        excluded_unsupported_result.count == 0 &&
            !excluded_unsupported_result.path_too_long &&
            !excluded_unsupported_result.output_limit_hit &&
            !excluded_unsupported_result.param_allocation_failed &&
            !excluded_unsupported_result.unsupported_type &&
            !excluded_unsupported_result.parse_failed,
        "Compiler: bindgen parse guards skip excluded unsupported C types",
        "[FAIL] Compiler: bindgen parse guards skip excluded unsupported C types",
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

    {
        ParsedParam fallback_param = {};
        visitor_set_param_name(&fallback_param, 123);
        compiler_test_finish_bool(
            cstr_eq(&fallback_param.name, "arg123"),
            "Compiler: bindgen anonymous parameter fallback names support three digits",
            "[FAIL] Compiler: bindgen anonymous parameter fallback names support three digits",
            &pass,
            &fail
        );
    }

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

    {
        ParsedFunc[1] unsafe_meta_funcs;
        ParsedParam[1] unsafe_meta_params;
        unsafe_meta_funcs[0] = {};
        unsafe_meta_params[0] = {};

        toml_set_default(&unsafe_meta_funcs[0].name, 128, "bindgen_unsafe_meta");
        toml_set_default(&unsafe_meta_funcs[0].return_c_type, 64, "void");
        toml_set_default(&unsafe_meta_funcs[0].return_ffi_type, 16, "'void");
        toml_set_default(&unsafe_meta_funcs[0].return_omni_type, 16, "Void");
        unsafe_meta_funcs[0].params = &unsafe_meta_params[0];
        unsafe_meta_funcs[0].param_count = 1;
        unsafe_meta_funcs[0].param_capacity = 1;
        unsafe_meta_funcs[0].is_variadic = false;
        unsafe_meta_funcs[0].return_role = BINDGEN_ROLE_STRING_RETURN;
        unsafe_meta_funcs[0].return_ownership = BINDGEN_OWNERSHIP_UNKNOWN;
        unsafe_meta_funcs[0].return_nullability = BINDGEN_NULLABILITY_UNKNOWN;
        unsafe_meta_funcs[0].return_teardown = BINDGEN_TEARDOWN_MANUAL_REVIEW;

        toml_set_default(&unsafe_meta_params[0].name, 64, "text");
        toml_set_default(&unsafe_meta_params[0].c_type, 64, "char*\n");
        toml_set_default(&unsafe_meta_params[0].ffi_type, 16, "'ptr");
        toml_set_default(&unsafe_meta_params[0].omni_type, 16, "ForeignHandle");
        unsafe_meta_params[0].role = BINDGEN_ROLE_OPAQUE_POINTER;
        unsafe_meta_params[0].ownership = BINDGEN_OWNERSHIP_BORROWED;
        unsafe_meta_params[0].nullability = BINDGEN_NULLABILITY_UNKNOWN;
        unsafe_meta_params[0].teardown = BINDGEN_TEARDOWN_NONE;

        char[512] unsafe_raw_path_buf;
        char[512] unsafe_facade_path_buf;
        char[] unsafe_raw_path;
        char[] unsafe_facade_path;
        if (try rendered = io::bprintf(
            &unsafe_raw_path_buf,
            "/tmp/omni_bindgen_overflow_guard_unsafe_meta-%d-%d_raw.omni",
            pid,
            (int)seq
        )) {
            unsafe_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe metadata raw output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &unsafe_facade_path_buf,
            "/tmp/omni_bindgen_overflow_guard_unsafe_meta-%d-%d.omni",
            pid,
            (int)seq
        )) {
            unsafe_facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe metadata facade output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)unsafe_raw_path);
        defer (void)io::file::delete((String)unsafe_facade_path);

        bool rejects_unsafe_metadata =
            !generate_ffi_module_pair_with_options(
                "bindgen-overflow-unsafe-meta",
                "bindgen",
                &unsafe_meta_funcs[0],
                1,
                unsafe_raw_path,
                unsafe_facade_path,
                BINDGEN_RAW_SYNTAX_GROUPED,
                null,
                0
            );
        bool unsafe_metadata_not_written =
            !bindgen_output_file_exists(unsafe_raw_path) &&
            !bindgen_output_file_exists(unsafe_facade_path);
        compiler_test_finish_bool(
            rejects_unsafe_metadata && unsafe_metadata_not_written,
            "Compiler: bindgen rejects metadata fields containing newlines",
            "[FAIL] Compiler: bindgen rejects metadata fields containing newlines",
            &pass,
            &fail
        );

        toml_set_default(&unsafe_meta_funcs[0].name, 128, "bindgen_unsafe_meta_tab");
        toml_set_default(&unsafe_meta_funcs[0].return_omni_type, 16, "Void\t");
        bool rejects_tab_metadata =
            !generate_ffi_module_pair_with_options(
                "bindgen-overflow-unsafe-meta-tab",
                "bindgen",
                &unsafe_meta_funcs[0],
                1,
                unsafe_raw_path,
                unsafe_facade_path,
                BINDGEN_RAW_SYNTAX_GROUPED,
                null,
                0
            );
        bool unsafe_tab_metadata_not_written =
            !bindgen_output_file_exists(unsafe_raw_path) &&
            !bindgen_output_file_exists(unsafe_facade_path);
        compiler_test_finish_bool(
            rejects_tab_metadata && unsafe_tab_metadata_not_written,
            "Compiler: bindgen rejects metadata fields containing tab control characters",
            "[FAIL] Compiler: bindgen rejects metadata fields containing tab control characters",
            &pass,
            &fail
        );
    }

    {
        ParsedFunc[2] mixed_return_type_funcs;
        mixed_return_type_funcs[0] = {};
        mixed_return_type_funcs[1] = {};

        toml_set_default(&mixed_return_type_funcs[0].name, 128, "bindgen_partial_ok");
        toml_set_default(&mixed_return_type_funcs[0].return_c_type, 64, "int");
        toml_set_default(&mixed_return_type_funcs[0].return_ffi_type, 16, "'int");
        toml_set_default(&mixed_return_type_funcs[0].return_omni_type, 16, "Integer");
        mixed_return_type_funcs[0].params = null;
        mixed_return_type_funcs[0].param_count = 0;
        mixed_return_type_funcs[0].param_capacity = 0;
        mixed_return_type_funcs[0].is_variadic = false;
        mixed_return_type_funcs[0].return_role = BINDGEN_ROLE_VALUE;
        mixed_return_type_funcs[0].return_ownership = BINDGEN_OWNERSHIP_NONE;
        mixed_return_type_funcs[0].return_nullability = BINDGEN_NULLABILITY_NA;
        mixed_return_type_funcs[0].return_teardown = BINDGEN_TEARDOWN_NONE;

        toml_set_default(&mixed_return_type_funcs[1].name, 128, "bindgen_unsafe_return_type");
        toml_set_default(&mixed_return_type_funcs[1].return_c_type, 64, "void");
        toml_set_default(&mixed_return_type_funcs[1].return_ffi_type, 16, "'void");
        toml_set_default(&mixed_return_type_funcs[1].return_omni_type, 16, "Integer\n");
        mixed_return_type_funcs[1].params = null;
        mixed_return_type_funcs[1].param_count = 0;
        mixed_return_type_funcs[1].param_capacity = 0;
        mixed_return_type_funcs[1].is_variadic = false;
        mixed_return_type_funcs[1].return_role = BINDGEN_ROLE_VALUE;
        mixed_return_type_funcs[1].return_ownership = BINDGEN_OWNERSHIP_NONE;
        mixed_return_type_funcs[1].return_nullability = BINDGEN_NULLABILITY_NA;
        mixed_return_type_funcs[1].return_teardown = BINDGEN_TEARDOWN_NONE;

        char[512] mixed_return_type_raw_path_buf;
        char[512] mixed_return_type_facade_path_buf;
        char[] mixed_return_type_raw_path;
        char[] mixed_return_type_facade_path;
        if (try rendered = io::bprintf(
            &mixed_return_type_raw_path_buf,
            "/tmp/omni_bindgen_overflow_guard_unsafe_return_%d_%d_raw.omni",
            pid,
            (int)seq
        )) {
            mixed_return_type_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe return-type raw output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &mixed_return_type_facade_path_buf,
            "/tmp/omni_bindgen_overflow_guard_unsafe_return_%d_%d.omni",
            pid,
            (int)seq
        )) {
            mixed_return_type_facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe return-type facade output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)mixed_return_type_raw_path);
        defer (void)io::file::delete((String)mixed_return_type_facade_path);

        bool rejects_unsafe_return_type =
            !generate_ffi_module_pair_with_options(
                "bindgen-overflow-unsafe-return-type",
                "bindgen",
                &mixed_return_type_funcs[0],
                2,
                mixed_return_type_raw_path,
                mixed_return_type_facade_path,
                BINDGEN_RAW_SYNTAX_GROUPED,
                null,
                0
            );
        bool unsafe_return_type_not_written =
            !bindgen_output_file_exists(mixed_return_type_raw_path) &&
            !bindgen_output_file_exists(mixed_return_type_facade_path);
        compiler_test_finish_bool(
            rejects_unsafe_return_type && unsafe_return_type_not_written,
            "Compiler: bindgen rejects unsafe return type metadata and fails closed",
            "[FAIL] Compiler: bindgen rejects unsafe return type metadata and fails closed",
            &pass,
            &fail
        );
    }

    {
        char[512] module_fail_raw_path_buf;
        char[512] module_fail_facade_path_buf;
        char[] module_fail_raw_path;
        char[] module_fail_facade_path;
        if (try rendered = io::bprintf(
            &module_fail_raw_path_buf,
            "/tmp/omni_bindgen_overflow_guard_module_name-%d-%d_raw.omni",
            pid,
            (int)seq
        )) {
            module_fail_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe module-name raw output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &module_fail_facade_path_buf,
            "/tmp/omni_bindgen_overflow_guard_module_name-%d-%d.omni",
            pid,
            (int)seq
        )) {
            module_fail_facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe module-name facade output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)module_fail_raw_path);
        defer (void)io::file::delete((String)module_fail_facade_path);

        bool rejects_unsafe_module_name =
            !generate_ffi_module_pair("bindgen_unsafe\nname", "bindgen", funcs, 1, module_fail_raw_path, module_fail_facade_path);
        bool module_name_not_written =
            !bindgen_output_file_exists(module_fail_raw_path) &&
            !bindgen_output_file_exists(module_fail_facade_path);
        compiler_test_finish_bool(
            rejects_unsafe_module_name && module_name_not_written,
            "Compiler: bindgen rejects unsafe emitted module names",
            "[FAIL] Compiler: bindgen rejects unsafe emitted module names",
            &pass,
            &fail
        );
    }

    {
        ParsedFunc[1] unsafe_variadic_name_funcs;
        unsafe_variadic_name_funcs[0] = {};
        toml_set_default(&unsafe_variadic_name_funcs[0].name, 128, "bindgen_unsafe\nvariadic");
        toml_set_default(&unsafe_variadic_name_funcs[0].return_c_type, 64, "int");
        toml_set_default(&unsafe_variadic_name_funcs[0].return_ffi_type, 16, "'int");
        toml_set_default(&unsafe_variadic_name_funcs[0].return_omni_type, 16, "Int");
        unsafe_variadic_name_funcs[0].params = null;
        unsafe_variadic_name_funcs[0].param_count = 0;
        unsafe_variadic_name_funcs[0].param_capacity = 0;
        unsafe_variadic_name_funcs[0].is_variadic = true;
        unsafe_variadic_name_funcs[0].return_role = BINDGEN_ROLE_VALUE;
        unsafe_variadic_name_funcs[0].return_ownership = BINDGEN_OWNERSHIP_NONE;
        unsafe_variadic_name_funcs[0].return_nullability = BINDGEN_NULLABILITY_NA;
        unsafe_variadic_name_funcs[0].return_teardown = BINDGEN_TEARDOWN_NONE;

        char[512] variadic_raw_path_buf;
        char[512] variadic_facade_path_buf;
        char[] variadic_raw_path;
        char[] variadic_facade_path;
        if (try rendered = io::bprintf(
            &variadic_raw_path_buf,
            "/tmp/omni_bindgen_overflow_guard_variadic_%d_%d_raw.omni",
            pid,
            (int)seq
        )) {
            variadic_raw_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe variadic skip output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        if (try rendered = io::bprintf(
            &variadic_facade_path_buf,
            "/tmp/omni_bindgen_overflow_guard_variadic_%d_%d.omni",
            pid,
            (int)seq
        )) {
            variadic_facade_path = rendered;
        } else {
            fail++;
            io::printn("[FAIL] Compiler: bindgen compose unsafe variadic skip output path");
            *pass_out += pass;
            *fail_out += fail;
            return;
        }
        defer (void)io::file::delete((String)variadic_raw_path);
        defer (void)io::file::delete((String)variadic_facade_path);

        bool rejects_unsafe_variadic_name =
            !generate_ffi_module_pair_with_options(
                "bindgen-overflow-variadic",
                "bindgen",
                &unsafe_variadic_name_funcs[0],
                1,
                variadic_raw_path,
                variadic_facade_path,
                BINDGEN_RAW_SYNTAX_GROUPED,
                null,
                0
            );
        bool variadic_path_not_written =
            !bindgen_output_file_exists(variadic_raw_path) &&
            !bindgen_output_file_exists(variadic_facade_path);
        compiler_test_finish_bool(
            rejects_unsafe_variadic_name && variadic_path_not_written,
            "Compiler: bindgen rejects unsafe variadic names in generated comments",
            "[FAIL] Compiler: bindgen rejects unsafe variadic names in generated comments",
            &pass,
            &fail
        );
    }

    *pass_out += pass;
    *fail_out += fail;
}
