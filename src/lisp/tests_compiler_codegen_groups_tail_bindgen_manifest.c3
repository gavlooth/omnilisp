module lisp;

import std::io;
import std::core::mem;
import main;

fn bool compiler_bindgen_manifest_and_helper_tests(usz* pass, usz* fail) {
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
            pass,
            fail
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
                pass,
                fail
            );
        } else {
            (*fail)++;
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
            (*fail)++;
            io::printn("[FAIL] Compiler: bindgen compose atomic write failure path");
                return false;
        }
        if (try atomic_fail_tmp_path_rendered = io::bprintf(
            &atomic_fail_tmp_path_buf,
            "%s.tmp.%d",
            (ZString)&atomic_fail_path_buf[0],
            manifest_pid
        )) {
            atomic_fail_tmp_path = atomic_fail_tmp_path_rendered;
        } else {
            (*fail)++;
            io::printn("[FAIL] Compiler: bindgen compose atomic write failure temp path");
                return false;
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
            pass,
            fail
        );
    } else {
        (*fail)++;
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
        pass,
        fail
    );
}

    return true;
}
