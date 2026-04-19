module lisp;

import std::io;
import std::core::mem;
import main;

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
