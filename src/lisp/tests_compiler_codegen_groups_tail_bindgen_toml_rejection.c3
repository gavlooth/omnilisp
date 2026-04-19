module lisp;

import std::io;
import std::core::mem;
import main;

fn bool compiler_bindgen_toml_rejection_tests(usz* pass, usz* fail) {
    TomlConfig* config = (TomlConfig*)mem::malloc(TomlConfig.sizeof);
    if (config == null) {
        (*fail)++;
        io::printn("[FAIL] Compiler: bindgen rejection toml config alloc");
        return true;
    }
    defer mem::free(config);
    TomlFfiDep* dep;

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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
            pass,
            fail
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
            pass,
            fail
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
        pass,
        fail
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
            pass,
            fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
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
        pass,
        fail
    );

    return true;
}
