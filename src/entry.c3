module main;

extern fn int system(char* command) @extern("system");

/** Main entry point. */
fn int main(int argc, char** argv) {
    int idx = find_flag_index(argc, argv, "--help", "-h", "-help");
    if (idx >= 0) return print_help();

    idx = find_flag_index(argc, argv, "--version", "-v");
    if (idx >= 0) return print_version();

    idx = find_flag_index(argc, argv, "--language-ref", "--lang-ref", "--manual");
    if (idx >= 0) return print_language_reference();

    idx = find_flag_index(argc, argv, "--describe");
    if (idx >= 0) return run_describe_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--eval");
    if (idx >= 0) return run_eval_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--init");
    if (idx >= 0) return run_init(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--bind");
    if (idx >= 0) return run_bind(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--fmt");
    if (idx >= 0) return run_fmt_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--check");
    if (idx >= 0) return run_check_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--build");
    if (idx >= 0) return run_build(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--compile", "-compile");
    if (idx >= 0) {
        char* input_file = idx + 1 < argc ? argv[idx + 1] : null;
        char* output_file = idx + 2 < argc ? argv[idx + 2] : null;
        return run_compile_mode(input_file, output_file);
    }

    idx = find_flag_index(argc, argv, "--gen-e2e");
    if (idx >= 0) return run_gen_e2e_mode();

    idx = find_flag_index(argc, argv, "--stack-affinity-probe");
    if (idx >= 0) return run_stack_affinity_probe_mode();

    idx = find_flag_index(argc, argv, "--test-suite");
    if (idx >= 0) {
        char* suite_name = idx + 1 < argc ? argv[idx + 1] : null;
        bool json = find_flag_index(argc, argv, "--json") >= 0;
        return run_test_mode_with_self_for_suite(argv[0], suite_name, json);
    }

    idx = find_flag_index(argc, argv, "--repl", "-repl");
    if (idx >= 0) return run_repl_mode(argc, argv, idx);

    if (argc > 1) return run_script_mode(argv[1]);
    return run_repl_mode(argc, argv, argc);
}
