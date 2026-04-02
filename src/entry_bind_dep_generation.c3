module main;

import std::io;
import lisp;

fn bool process_bind_dependency(char[] proj_dir, usz proj_dir_len, lisp::LibclangHandle* clang,
    lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz max_funcs, usz* total_bindings, usz* total_libs) {
    // Get null-terminated name and library
    char* dep_name = &dep.name;
    char* dep_lib = &dep.library;

    io::printfn("Parsing headers for '%s' (lib%s)...", (ZString)dep_name, (ZString)dep_lib);

    BindHeaderParseResult parse_result = parse_ffi_dep_headers(clang, dep, funcs, max_funcs);
    usz func_count = parse_result.func_count;

    if (parse_result.path_too_long) {
        io::printfn("  Error: header path too long for '%s': %s", (ZString)dep_name, (ZString)parse_result.failed_header);
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
        return false;
    }

    if (func_count == 0) {
        io::printfn("  Warning: no functions found for '%s'", (ZString)dep_name);
        return true;
    }

    char[512] out_path;
    usz olen = 0;
    if (!build_bind_output_path(out_path[..], &olen, &proj_dir[0], proj_dir_len, dep_name)) {
        io::printfn("  Error: bind output path too long for '%s'", (ZString)dep_name);
        free_parsed_funcs(funcs, func_count);
        return false;
    }

    usz dep_name_len = 0;
    while (dep_name[dep_name_len] != 0) dep_name_len++;
    usz dep_lib_len = 0;
    while (dep_lib[dep_lib_len] != 0) dep_lib_len++;

    if (lisp::generate_ffi_module(dep_name[:dep_name_len], dep_lib[:dep_lib_len],
        funcs, func_count, out_path[:olen])) {
        io::printfn("  Generated %s (%d functions)", (ZString)&out_path[0], (int)func_count);
        *total_bindings += func_count;
        *total_libs += 1;
        free_parsed_funcs(funcs, func_count);
        return true;
    } else {
        io::printfn("  Error: failed to write %s", (ZString)&out_path[0]);
        free_parsed_funcs(funcs, func_count);
        return false;
    }
}
