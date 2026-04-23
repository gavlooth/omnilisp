module main;

import std::io;
import std::core::mem;
import lisp;

fn bool bind_filter_function_was_parsed(lisp::ParsedFunc* funcs, usz func_count, char* filter_name) {
    for (usz i = 0; i < func_count; i++) {
        if (lisp::cstr_eq(&funcs[i].name, filter_name)) return true;
    }
    return false;
}

fn bool bind_filter_function_was_requested(lisp::TomlFfiDep* dep, char* filter_name) {
    if (!dep.filter_functions) return true;
    for (usz i = 0; i < dep.function_count; i++) {
        if (lisp::cstr_eq(&dep.functions[i], filter_name)) return true;
    }
    return false;
}

fn bool bind_filter_function_was_excluded(lisp::TomlFfiDep* dep, char* filter_name) {
    for (usz i = 0; i < dep.exclude_function_count; i++) {
        if (lisp::cstr_eq(&dep.exclude_functions[i], filter_name)) return true;
    }
    return false;
}

fn bool bind_all_filter_functions_were_parsed(lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz func_count, char** missing_name) {
    if (!dep.filter_functions) return true;
    for (usz i = 0; i < dep.function_count; i++) {
        char* filter_name = &dep.functions[i];
        if (bind_filter_function_was_excluded(dep, filter_name)) continue;
        if (!bind_filter_function_was_parsed(funcs, func_count, filter_name)) {
            if (missing_name != null) *missing_name = filter_name;
            return false;
        }
    }
    return true;
}

fn bool bind_all_exclude_functions_were_parsed(lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz func_count, char** missing_name) {
    if (dep.exclude_function_count == 0) return true;
    for (usz i = 0; i < dep.exclude_function_count; i++) {
        char* filter_name = &dep.exclude_functions[i];
        if (!bind_filter_function_was_parsed(funcs, func_count, filter_name)) {
            if (missing_name != null) *missing_name = filter_name;
            return false;
        }
    }
    return true;
}

fn usz bind_count_visible_functions(lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz func_count) {
    usz visible_count = 0;
    for (usz i = 0; i < func_count; i++) {
        char* func_name = &funcs[i].name;
        if (!bind_filter_function_was_requested(dep, func_name)) continue;
        if (bind_filter_function_was_excluded(dep, func_name)) continue;
        visible_count++;
    }
    return visible_count;
}

fn void bind_mark_hidden_functions(lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz func_count) {
    for (usz i = 0; i < func_count; i++) {
        char* func_name = &funcs[i].name;
        funcs[i].filtered_out = !bind_filter_function_was_requested(dep, func_name) ||
            bind_filter_function_was_excluded(dep, func_name);
    }
}

fn bool process_bind_dependency(char[] proj_dir, usz proj_dir_len, lisp::LibclangHandle* clang,
    lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz max_funcs, usz* total_bindings, usz* total_libs) {
    // Get null-terminated name and library
    char* dep_name = &dep.name;
    char* dep_lib = &dep.library;
    lisp::BindgenRawSyntax raw_syntax;
    if (dep.name_invalid || dep_name[0] == 0) {
        io::printfn(
            "  Error: invalid bind dependency name '%s' (expected a unique non-empty output-safe section name shorter than 64 bytes)",
            (ZString)dep_name
        );
        return false;
    }

    if (dep.raw_syntax_invalid) {
        io::printfn(
            "  Error: invalid bind raw-syntax for '%s' (expected \"legacy\" or \"grouped\")",
            (ZString)dep_name
        );
        return false;
    }

    if (!lisp::bindgen_raw_syntax_from_dep(dep, &raw_syntax)) {
        io::printfn(
            "  Error: unsupported bind raw-syntax for '%s': %s (expected legacy or grouped)",
            (ZString)dep_name,
            (ZString)&dep.raw_syntax
        );
        return false;
    }

    if (dep.library_invalid) {
        io::printfn(
            "  Error: invalid bind library for '%s' (expected a non-empty quoted string shorter than 128 bytes)",
            (ZString)dep_name
        );
        return false;
    }
    if (dep_lib[0] == 0) {
        io::printfn(
            "  Error: missing bind library for '%s' (expected a non-empty quoted string shorter than 128 bytes)",
            (ZString)dep_name
        );
        return false;
    }
    if (!lisp::bindgen_library_name_is_safe(dep_lib)) {
        io::printfn(
            "  Error: unsafe bind library for '%s' (expected a library stem without slashes, quotes, whitespace, or control characters)",
            (ZString)dep_name
        );
        return false;
    }

    if (dep.headers_invalid) {
        io::printfn(
            "  Error: invalid bind headers for '%s' (expected a non-empty array of non-empty quoted strings shorter than 256 bytes)",
            (ZString)dep_name
        );
        return false;
    }
    if (dep.header_count == 0) {
        io::printfn(
            "  Error: missing bind headers for '%s' (expected a non-empty array of non-empty quoted strings shorter than 256 bytes)",
            (ZString)dep_name
        );
        return false;
    }

    if (dep.functions_invalid) {
        io::printfn(
            "  Error: invalid bind functions for '%s' (expected a non-empty array of non-empty quoted strings shorter than 64 bytes)",
            (ZString)dep_name
        );
        return false;
    }

    if (dep.exclude_functions_invalid) {
        io::printfn(
            "  Error: invalid bind exclude-functions for '%s' (expected a non-empty array of non-empty quoted strings shorter than 64 bytes)",
            (ZString)dep_name
        );
        return false;
    }

    if (dep.strip_prefixes_invalid) {
        io::printfn(
            "  Error: invalid bind strip-prefixes for '%s' (expected non-empty quoted strings shorter than 64 bytes)",
            (ZString)dep_name
        );
        return false;
    }
    if (dep.unknown_key_invalid) {
        io::printfn(
            "  Error: invalid bind dependency key for '%s' (unsupported FFI dependency option)",
            (ZString)dep_name
        );
        return false;
    }

    io::printfn("Parsing headers for '%s' (lib%s)...", (ZString)dep_name, (ZString)dep_lib);

    void* include_filter = dep.filter_functions ? (void*)&dep.functions : null;
    usz include_filter_count = dep.filter_functions ? dep.function_count : 0;

    BindHeaderParseResult parse_result = parse_ffi_dep_headers(
        clang,
        dep,
        funcs,
        max_funcs,
        include_filter,
        include_filter_count,
        dep.filter_functions,
        &dep.exclude_functions,
        dep.exclude_function_count,
        dep.exclude_function_count > 0
    );
    usz func_count = parse_result.func_count;

    if (parse_result.path_too_long) {
        io::printfn("  Error: header path too long for '%s': %s", (ZString)dep_name, (ZString)parse_result.failed_header);
        free_parsed_funcs(funcs, func_count);
        return false;
    }
    if (parse_result.function_limit_hit) {
        io::printfn(
            "  Error: parsed headers exceed bind function limit (%d) for '%s'; refusing partial bindings at %s",
            (int)max_funcs,
            (ZString)dep_name,
            (ZString)parse_result.failed_header
        );
        free_parsed_funcs(funcs, func_count);
        return false;
    }
    if (parse_result.param_allocation_failed) {
        io::printfn(
            "  Error: out of memory while parsing parameter metadata for '%s': %s",
            (ZString)dep_name,
            (ZString)parse_result.failed_header
        );
        free_parsed_funcs(funcs, func_count);
        return false;
    }
    if (parse_result.unsupported_type) {
        io::printfn(
            "  Error: unsupported C type for '%s' in %s: %s",
            (ZString)dep_name,
            (ZString)parse_result.failed_header,
            (ZString)&parse_result.unsupported_type_name
        );
        free_parsed_funcs(funcs, func_count);
        return false;
    }
    if (parse_result.parse_failed) {
        io::printfn("  Error: failed to parse header for '%s': %s", (ZString)dep_name, (ZString)parse_result.failed_header);
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    char* missing_filter = null;
    if (!bind_all_filter_functions_were_parsed(dep, funcs, func_count, &missing_filter)) {
        io::printfn(
            "  Error: bind function filter '%s' was not found for '%s'",
            (ZString)missing_filter,
            (ZString)dep_name
        );
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    if (dep.exclude_function_count > 0) {
        usz exclude_parse_cap = max_funcs;
        if (dep.exclude_function_count > exclude_parse_cap) exclude_parse_cap = dep.exclude_function_count;
        lisp::ParsedFunc* exclude_funcs = (lisp::ParsedFunc*)mem::malloc(lisp::ParsedFunc.sizeof * exclude_parse_cap);
        if (exclude_funcs == null) {
            io::printfn("  Error: out of memory while validating bind exclude-functions for '%s'", (ZString)dep_name);
            free_parsed_funcs(funcs, func_count);
            return false;
        }
        for (usz i = 0; i < exclude_parse_cap; i++) exclude_funcs[i] = {};

        BindHeaderParseResult exclude_parse_result = parse_ffi_dep_headers(
            clang,
            dep,
            exclude_funcs,
            exclude_parse_cap,
            &dep.exclude_functions,
            dep.exclude_function_count,
            true,
            null,
            0,
            false
        );
        usz exclude_func_count = exclude_parse_result.func_count;
        if (exclude_parse_result.path_too_long) {
            io::printfn("  Error: header path too long while validating exclude-functions for '%s': %s",
                (ZString)dep_name,
                (ZString)exclude_parse_result.failed_header);
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }
        if (exclude_parse_result.function_limit_hit) {
            io::printfn(
                "  Error: parsed headers exceed exclude-function validation limit (%d) for '%s'; refusing partial validation at %s",
                (int)exclude_parse_cap,
                (ZString)dep_name,
                (ZString)exclude_parse_result.failed_header
            );
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }
        if (exclude_parse_result.param_allocation_failed) {
            io::printfn(
                "  Error: out of memory while parsing exclude-function metadata for '%s': %s",
                (ZString)dep_name,
                (ZString)exclude_parse_result.failed_header
            );
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }
        if (exclude_parse_result.unsupported_type) {
            io::printfn(
                "  Error: unsupported C type while validating exclude-functions for '%s' in %s: %s",
                (ZString)dep_name,
                (ZString)exclude_parse_result.failed_header,
                (ZString)&exclude_parse_result.unsupported_type_name
            );
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }
        if (exclude_parse_result.parse_failed) {
            io::printfn("  Error: failed to parse header while validating exclude-functions for '%s': %s",
                (ZString)dep_name,
                (ZString)exclude_parse_result.failed_header);
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }

        char* missing_exclude = null;
        if (!bind_all_exclude_functions_were_parsed(dep, exclude_funcs, exclude_func_count, &missing_exclude)) {
            io::printfn(
                "  Error: bind exclude function '%s' was not found for '%s'",
                (ZString)missing_exclude,
                (ZString)dep_name
            );
            free_parsed_funcs(exclude_funcs, exclude_func_count);
            mem::free(exclude_funcs);
            free_parsed_funcs(funcs, func_count);
            return false;
        }

        free_parsed_funcs(exclude_funcs, exclude_func_count);
        mem::free(exclude_funcs);
    }

    bind_mark_hidden_functions(dep, funcs, func_count);
    usz visible_count = bind_count_visible_functions(dep, funcs, func_count);

    if (visible_count == 0) {
        io::printfn("  Warning: no functions found for '%s'", (ZString)dep_name);
        free_parsed_funcs(funcs, func_count);
        return true;
    }

    char[512] raw_out_path;
    usz raw_olen = 0;
    if (!build_bind_raw_output_path(raw_out_path[..], &raw_olen, &proj_dir[0], proj_dir_len, dep_name)) {
        io::printfn("  Error: bind raw output path too long or unsafe for dependency name '%s'", (ZString)dep_name);
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    char[512] facade_out_path;
    usz facade_olen = 0;
    if (!build_bind_output_path(facade_out_path[..], &facade_olen, &proj_dir[0], proj_dir_len, dep_name)) {
        io::printfn("  Error: bind output path too long or unsafe for dependency name '%s'", (ZString)dep_name);
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    char[512] manifest_out_path;
    usz manifest_olen = 0;
    if (!build_bind_manifest_output_path(manifest_out_path[..], &manifest_olen, &proj_dir[0], proj_dir_len, dep_name)) {
        io::printfn("  Error: bind manifest output path too long or unsafe for dependency name '%s'", (ZString)dep_name);
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    usz dep_name_len = 0;
    while (dep_name_len < 256 && dep_name[dep_name_len] != 0) dep_name_len++;
    usz dep_lib_len = 0;
    while (dep_lib_len < 512 && dep_lib[dep_lib_len] != 0) dep_lib_len++;

    bool raw_existed = lisp::bindgen_output_file_exists(raw_out_path[:raw_olen]);
    bool facade_existed = lisp::bindgen_output_file_exists(facade_out_path[:facade_olen]);
    bool manifest_existed = lisp::bindgen_output_file_exists(manifest_out_path[:manifest_olen]);

    bool pair_ok = lisp::generate_ffi_module_pair_with_options(dep_name[:dep_name_len], dep_lib[:dep_lib_len],
        funcs, func_count, raw_out_path[:raw_olen], facade_out_path[:facade_olen],
        raw_syntax, &dep.strip_prefixes, dep.strip_prefix_count);
    bool manifest_ok = false;
    if (pair_ok) {
        manifest_ok = lisp::generate_ffi_bindgen_manifest(dep_name[:dep_name_len], dep_lib[:dep_lib_len],
            raw_out_path[:raw_olen], facade_out_path[:facade_olen], manifest_out_path[:manifest_olen],
            raw_syntax, &dep.exclude_functions, dep.exclude_function_count, &dep.strip_prefixes, dep.strip_prefix_count);
        if (!manifest_ok) {
            if (!raw_existed) lisp::bindgen_delete_output_file_if_exists(raw_out_path[:raw_olen]);
            if (!facade_existed) lisp::bindgen_delete_output_file_if_exists(facade_out_path[:facade_olen]);
            if (!manifest_existed) lisp::bindgen_delete_output_file_if_exists(manifest_out_path[:manifest_olen]);
        }
    }

    if (pair_ok && manifest_ok) {
        if (facade_existed) {
            io::printfn(
                "  Generated %s and %s; preserved existing %s (%d functions)",
                (ZString)&raw_out_path[0],
                (ZString)&manifest_out_path[0],
                (ZString)&facade_out_path[0],
                (int)visible_count
            );
        } else {
            io::printfn(
                "  Generated %s, %s, and %s (%d functions)",
                (ZString)&raw_out_path[0],
                (ZString)&facade_out_path[0],
                (ZString)&manifest_out_path[0],
                (int)visible_count
            );
        }
        *total_bindings += visible_count;
        *total_libs += 1;
        free_parsed_funcs(funcs, func_count);
        return true;
    } else {
        io::printfn(
            "  Error: failed to write bind outputs for '%s' (%s, %s, %s)",
            (ZString)dep_name,
            (ZString)&raw_out_path[0],
            (ZString)&facade_out_path[0],
            (ZString)&manifest_out_path[0]
        );
        free_parsed_funcs(funcs, func_count);
        return false;
    }
}
