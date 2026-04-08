module main;

import std::io;
import lisp;

/**
 * Generate FFI bindings from omni.toml using libclang.
 * Usage: omni --bind [project-dir]
 */
fn int run_bind(int argc, char** argv, int bind_idx) {
    char* project_arg = bind_idx + 1 < argc ? argv[bind_idx + 1] : null;
    if (cli_arg_starts_with_dash(project_arg)) {
        io::printfn("Error: unexpected option after --bind: %s", (ZString)project_arg);
        return 1;
    }
    char* extra = find_extra_mode_arg(argc, argv, bind_idx, 1);
    if (extra != null) {
        return print_unexpected_mode_arg("--bind", extra);
    }

    char[512] proj_dir;
    usz proj_dir_len = 0;
    if (!resolve_bind_project_dir(argc, argv, bind_idx, proj_dir[..], &proj_dir_len)) {
        io::printn("Error: bind project path too long");
        return 1;
    }

    // Load omni.toml
    char[512] toml_path;
    usz tlen = 0;
    if (!build_bind_toml_path(toml_path[..], &tlen, &proj_dir[0], proj_dir_len)) {
        io::printn("Error: bind omni.toml path too long");
        return 1;
    }

    lisp::TomlConfig* config = (lisp::TomlConfig*)mem::malloc(lisp::TomlConfig.sizeof);
    if (config == null) {
        io::printn("Error: failed to allocate TOML config");
        return 1;
    }
    defer mem::free(config);

    if (!lisp::toml_load(toml_path[:tlen], config)) {
        io::printfn("Error: cannot read %s", (ZString)&toml_path[0]);
        return 1;
    }

    if (config.ffi_dep_count == 0) {
        io::printn("No FFI dependencies found in omni.toml");
        io::printn("Add a [dependencies.ffi.NAME] section to generate bindings.");
        return 0;
    }

    // Initialize libclang
    lisp::LibclangHandle clang;
    if (!bind_init_clang_or_report(&clang)) return 1;
    defer lisp::libclang_destroy(&clang);

    // Allocate parsed functions buffer
    const usz MAX_FUNCS = 256;
    lisp::ParsedFunc* funcs = alloc_bind_func_buffer(MAX_FUNCS);
    if (funcs == null) {
        io::printn("Error: failed to allocate bind function buffer");
        return 1;
    }
    defer mem::free(funcs);

    usz total_bindings = 0;
    usz total_libs = 0;
    bool had_errors = false;

    for (usz dep_idx = 0; dep_idx < config.ffi_dep_count; dep_idx++) {
        if (!process_bind_dependency(proj_dir[..], proj_dir_len, &clang, &config.ffi_deps[dep_idx],
                funcs, MAX_FUNCS, &total_bindings, &total_libs)) {
            had_errors = true;
        }
    }

    io::printfn("\nGenerated %d bindings from %d libraries.", (int)total_bindings, (int)total_libs);
    if (had_errors) {
        io::printfn("Error: one or more FFI bindings failed for '%s'", (ZString)&proj_dir[0]);
        return 1;
    }
    return 0;
}
