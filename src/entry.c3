module main;

extern fn int system(char* command) @extern("system");

/** Main entry point. */
fn int main(int argc, char** argv) {
    int idx = find_flag_index(argc, argv, "--help", "-h");
    if (idx >= 0) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, idx);
        if (unexpected != null) {
            return print_unexpected_mode_arg("--help", unexpected);
        }
        return print_help();
    }

    idx = find_flag_index(argc, argv, "--version", "-v");
    if (idx >= 0) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, idx);
        if (unexpected != null) {
            return print_unexpected_mode_arg("--version", unexpected);
        }
        return print_version();
    }

    idx = find_flag_index(argc, argv, "--language-ref", "--lang-ref", "--manual");
    if (idx >= 0) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, idx);
        if (unexpected != null) {
            return print_unexpected_mode_arg("--language-ref", unexpected);
        }
        return print_language_reference();
    }

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

    idx = find_flag_index(argc, argv, "--compile");
    if (idx >= 0) {
        char* extra = find_extra_mode_arg(argc, argv, idx, 2);
        if (extra != null) {
            return print_unexpected_mode_arg("--compile", extra);
        }
        char* input_file = idx + 1 < argc ? argv[idx + 1] : null;
        char* output_file = idx + 2 < argc ? argv[idx + 2] : null;
        return run_compile_mode(input_file, output_file);
    }

    idx = find_flag_index(argc, argv, "--gen-e2e");
    if (idx >= 0) return run_gen_e2e_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--stack-affinity-probe");
    if (idx >= 0) return run_stack_affinity_probe_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--test-suite");
    if (idx >= 0) {
        char* extra = find_extra_mode_arg(argc, argv, idx, 1, true);
        bool json = find_flag_index(argc, argv, "--json") >= 0;
        if (extra != null) {
            if (json) {
                print_test_suite_unexpected_arg_json(extra);
                return 1;
            }
            return print_unexpected_mode_arg("--test-suite", extra);
        }
        char* suite_name = find_test_suite_arg(argc, argv, idx);
        if (suite_name != null && cli_arg_starts_with_dash(suite_name)) {
            if (json) {
                print_test_suite_unexpected_arg_json(suite_name);
                return 1;
            }
            return print_unexpected_mode_arg("--test-suite", suite_name);
        }
        return run_test_mode_with_self_for_suite(argv[0], suite_name, json);
    }

    idx = find_flag_index(argc, argv, "--repl-server");
    if (idx >= 0) return run_repl_server_mode(argc, argv, idx);

    idx = find_flag_index(argc, argv, "--repl");
    if (idx >= 0) return run_repl_mode(argc, argv, idx);

    if (argc > 1 && cli_arg_starts_with_dash(argv[1])) {
        return print_unknown_option(argv[1]);
    }

    if (argc > 1) return run_script_mode(argc, argv, 1);
    return run_repl_mode(argc, argv, argc);
}
