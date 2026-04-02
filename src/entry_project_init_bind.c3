module main;

import std::io;

fn void print_init_summary(char* name) {
    io::printfn("Created project '%s'", (ZString)name);
    io::printn("");
    io::printfn("  %s/", (ZString)name);
    io::printn("    omni.toml       # Project config");
    io::printn("    src/main.omni   # Entry point");
    io::printn("    lib/ffi/        # Auto-generated FFI bindings");
    io::printn("    include/        # C headers");
    io::printn("    build/          # Build artifacts (contains project.json for c3c)");
    io::printn("");
    io::printn("Next steps:");
    io::printfn("  cd %s", (ZString)name);
    io::printn("  # Edit omni.toml to configure build and add FFI dependencies");
    io::printn("  # Run: omni --bind to generate bindings");
}

/**
 * Scaffold a new Omni project directory.
 * Usage: omni --init <project-name>
 */
fn int run_init(int argc, char** argv, int init_idx) {
    if (init_idx + 1 >= argc) {
        io::printn("Usage: omni --init <project-name>");
        return 1;
    }

    char* name = argv[init_idx + 1];
    if (cli_arg_starts_with_dash(name)) {
        io::printfn("Error: unexpected option after --init: %s", (ZString)name);
        return 1;
    }
    char* extra = find_extra_mode_arg(argc, argv, init_idx, 1);
    if (extra != null) {
        return print_unexpected_mode_arg("--init", extra);
    }
    usz name_len = cstr_len(name);

    bool project_root_created = false;
    bool success = false;
    defer if (project_root_created && !success) cleanup_init_project(name, name_len);

    if (!init_project_root_is_fresh(name, name_len)) return 1;
    if (!mkdir_project_path(name, name_len)) return 1;
    project_root_created = true;
    if (!mkdir_project_path(name, name_len, "src")) return 1;
    if (!mkdir_project_path(name, name_len, "lib")) return 1;
    if (!mkdir_project_path(name, name_len, "lib/ffi")) return 1;
    if (!mkdir_project_path(name, name_len, "include")) return 1;
    if (!mkdir_project_path(name, name_len, "build")) return 1;

    if (!write_init_toml(name, name_len)) return 1;
    if (!write_init_project_json(name, name_len)) return 1;
    if (!write_init_main_omni(name, name_len)) return 1;

    success = true;
    print_init_summary(name);
    return 0;
}
