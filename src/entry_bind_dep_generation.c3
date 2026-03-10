module main;

import std::io;
import lisp;

fn void process_bind_dependency(char[] proj_dir, usz proj_dir_len, lisp::LibclangHandle* clang,
    lisp::TomlFfiDep* dep, lisp::ParsedFunc* funcs, usz max_funcs, usz* total_bindings, usz* total_libs) {
    // Get null-terminated name and library
    char* dep_name = &dep.name;
    char* dep_lib = &dep.library;

    io::printfn("Parsing headers for '%s' (lib%s)...", (ZString)dep_name, (ZString)dep_lib);

    usz func_count = parse_ffi_dep_headers(clang, dep, funcs, max_funcs);

    if (func_count == 0) {
        io::printfn("  Warning: no functions found for '%s'", (ZString)dep_name);
        return;
    }

    char[512] out_path;
    usz olen = build_bind_output_path(out_path[..], proj_dir[..proj_dir_len], dep_name);

    usz dep_name_len = 0;
    while (dep_name[dep_name_len] != 0) dep_name_len++;
    usz dep_lib_len = 0;
    while (dep_lib[dep_lib_len] != 0) dep_lib_len++;

    if (lisp::generate_ffi_module(dep_name[:dep_name_len], dep_lib[:dep_lib_len],
        funcs, func_count, out_path[:olen])) {
        io::printfn("  Generated %s (%d functions)", (ZString)&out_path[0], (int)func_count);
        *total_bindings += func_count;
        *total_libs += 1;
    } else {
        io::printfn("  Error: failed to write %s", (ZString)&out_path[0]);
    }
    free_parsed_funcs(funcs, func_count);
}
