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
 * Usage: ./main --init myproject
 */
fn int run_init(int argc, char** argv, int init_idx) {
    if (init_idx + 1 >= argc) {
        io::printn("Usage: ./main --init <project-name>");
        return 1;
    }

    char* name = argv[init_idx + 1];
    usz name_len = cstr_len(name);

    mkdir_project_path(name, name_len);
    mkdir_project_path(name, name_len, "src");
    mkdir_project_path(name, name_len, "lib");
    mkdir_project_path(name, name_len, "lib/ffi");
    mkdir_project_path(name, name_len, "include");
    mkdir_project_path(name, name_len, "build");

    write_init_toml(name, name_len);
    write_init_project_json(name, name_len);
    write_init_main_omni(name, name_len);
    print_init_summary(name);
    return 0;
}
