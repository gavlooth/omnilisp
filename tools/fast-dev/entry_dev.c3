module main;

import std::io;

extern fn int system(char* command) @extern("system");

/** Fast dev entry point: keep normal runtime modes but omit test-suite wiring. */
fn int main(int argc, char** argv) {
    int idx = find_flag_index(argc, argv, "--help", "-h", "-help");
    if (idx >= 0) return print_help();

    idx = find_flag_index(argc, argv, "--version", "-v");
    if (idx >= 0) return print_version();

    idx = find_flag_index(argc, argv, "--language-ref", "--lang-ref", "--manual");
    if (idx >= 0) {
        io::printn("main-dev does not include --language-ref support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--eval");
    if (idx >= 0) return run_eval_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--init");
    if (idx >= 0) {
        io::printn("main-dev does not include --init support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--bind");
    if (idx >= 0) {
        io::printn("main-dev does not include --bind support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--check");
    if (idx >= 0) return run_check_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--build");
    if (idx >= 0) {
        io::printn("main-dev does not include --build support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--compile", "-compile");
    if (idx >= 0) {
        io::printn("main-dev does not include --compile support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--gen-e2e");
    if (idx >= 0) {
        io::printn("main-dev does not include --gen-e2e support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--stack-affinity-probe");
    if (idx >= 0) {
        io::printn("main-dev does not include --stack-affinity-probe support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--test-suite");
    if (idx >= 0) {
        io::printn("main-dev does not include --test-suite support; use ./build/main");
        return 2;
    }

    idx = find_flag_index(argc, argv, "--repl", "-repl");
    if (idx >= 0) return run_repl_mode(argc, argv, idx);

    if (argc > 1) return run_script_mode(argv[1]);
    return run_repl_mode(argc, argv, argc);
}
