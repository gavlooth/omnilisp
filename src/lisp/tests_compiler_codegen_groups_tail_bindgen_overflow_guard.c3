module lisp;

import std::io;
import std::core::mem;
import main;

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
