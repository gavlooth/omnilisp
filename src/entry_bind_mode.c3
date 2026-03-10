module main;

import std::io;
import lisp;

/**
 * Generate FFI bindings from omni.toml using libclang.
 * Usage: ./main --bind [project-dir]
 */
fn int run_bind(int argc, char** argv, int bind_idx) {
    char[512] proj_dir;
    usz proj_dir_len = resolve_bind_project_dir(argc, argv, bind_idx, proj_dir[..]);

    // Load omni.toml
    char[512] toml_path;
    usz tlen = build_bind_toml_path(toml_path[..], proj_dir[..proj_dir_len]);

    lisp::TomlConfig* config = (lisp::TomlConfig*)mem::malloc(lisp::TomlConfig.sizeof);
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
    defer mem::free(funcs);

    usz total_bindings = 0;
    usz total_libs = 0;

    for (usz dep_idx = 0; dep_idx < config.ffi_dep_count; dep_idx++) {
        process_bind_dependency(proj_dir[..], proj_dir_len, &clang, &config.ffi_deps[dep_idx],
            funcs, MAX_FUNCS, &total_bindings, &total_libs);
    }

    io::printfn("\nGenerated %d bindings from %d libraries.", (int)total_bindings, (int)total_libs);
    return 0;
}
