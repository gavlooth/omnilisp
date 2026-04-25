module lisp;

import std::io;
import std::core::mem;
import main;

fn bool compiler_bindgen_toml_surface_tests(usz* pass, usz* fail) {
{
    TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
    if (config == null) {
        (*fail)++;
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
            pass,
            fail
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
            pass,
            fail
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
            pass,
            fail
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
            pass,
            fail
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
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char project path");
                    return false;
            }
            if (try rendered = io::bprintf(&control_project_lib_path_buf, "%s/lib", (ZString)&control_project_path_buf[0])) {
                control_project_lib_path = rendered;
            } else {
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char lib path");
                    return false;
            }
            if (try rendered = io::bprintf(&control_project_ffi_path_buf, "%s/lib/ffi", (ZString)&control_project_path_buf[0])) {
                control_project_ffi_path = rendered;
            } else {
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char ffi path");
                    return false;
            }
            if (try rendered = io::bprintf(&control_raw_path_buf, "%s/lib/ffi/control_raw.omni", (ZString)&control_project_path_buf[0])) {
                control_raw_path = rendered;
            } else {
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char raw path");
                    return false;
            }
            if (try rendered = io::bprintf(&control_facade_path_buf, "%s/lib/ffi/control.omni", (ZString)&control_project_path_buf[0])) {
                control_facade_path = rendered;
            } else {
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char facade path");
                    return false;
            }
            if (try rendered = io::bprintf(&control_manifest_path_buf, "%s/lib/ffi/control_manifest.toml", (ZString)&control_project_path_buf[0])) {
                control_manifest_path = rendered;
            } else {
                (*fail)++;
                io::printn("[FAIL] Compiler: bindgen control-char manifest path");
                    return false;
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
                pass,
                fail
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
                pass,
                fail
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
            pass,
            fail
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
                pass,
                fail
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
                pass,
                fail
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
                pass,
                fail
            );
        }
    }
}

    return true;
}
