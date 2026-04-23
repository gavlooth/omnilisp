module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_compiler_group_bindgen_parse_guard_tests(Interp* _interp, usz* pass_out, usz* fail_out) {
    (void)_interp;
    usz pass = 0;
    usz fail = 0;

    char[640] long_header_buf;
    usz long_len = 0;
    char[] prefix = "/tmp/";
foreach (item : prefix) {
        long_header_buf[long_len++] = item;
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
