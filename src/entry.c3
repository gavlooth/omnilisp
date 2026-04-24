module main;

import lisp;

extern fn int system(char* command) @extern("system");

fn int finish_main(int rc) {
    if (!lisp::scheduler_shutdown() && rc == 0) return 1;
    return rc;
}

/** Main entry point. */
fn int main(int argc, char** argv) {
    char* mode_arg = cli_first_mode_arg(argc, argv);
    if (mode_arg == null) return finish_main(run_repl_mode(argc, argv, argc));

    if (cli_first_arg_matches(argc, argv, "--help", "-h")) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, 1);
        if (unexpected != null) {
            return finish_main(print_unexpected_mode_arg("--help", unexpected));
        }
        return finish_main(print_help());
    }

    if (cli_first_arg_matches(argc, argv, "--version", "-v")) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, 1);
        if (unexpected != null) {
            return finish_main(print_unexpected_mode_arg("--version", unexpected));
        }
        return finish_main(print_version());
    }

    if (cli_first_arg_matches(argc, argv, "--language-ref", "--lang-ref", "--manual")) {
        char* unexpected = find_unexpected_mode_arg(argc, argv, 1);
        if (unexpected != null) {
            return finish_main(print_unexpected_mode_arg("--language-ref", unexpected));
        }
        return finish_main(print_language_reference());
    }

    if (cli_first_arg_matches(argc, argv, "--describe")) return finish_main(run_describe_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--eval")) return finish_main(run_eval_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--init")) return finish_main(run_init(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--bind")) return finish_main(run_bind(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--fmt")) return finish_main(run_fmt_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--check")) return finish_main(run_check_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--build")) return finish_main(run_build(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--compile")) {
        char* extra = find_extra_mode_arg(argc, argv, 1, 2);
        if (extra != null) {
            return finish_main(print_unexpected_mode_arg("--compile", extra));
        }
        char* input_file = argc > 2 ? argv[2] : null;
        char* output_file = argc > 3 ? argv[3] : null;
        return finish_main(run_compile_mode(input_file, output_file));
    }

    if (cli_first_arg_matches(argc, argv, "--gen-e2e")) return finish_main(run_gen_e2e_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--stack-affinity-probe")) return finish_main(run_stack_affinity_probe_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--test-suite")) {
        char* extra = find_extra_mode_arg(argc, argv, 1, 1, true);
        bool json = find_flag_index(argc, argv, "--json") >= 0;
        if (extra != null) {
            if (json) {
                print_test_suite_unexpected_arg_json(extra);
                return finish_main(1);
            }
            return finish_main(print_unexpected_mode_arg("--test-suite", extra));
        }
        char* suite_name = find_test_suite_arg(argc, argv, 1);
        if (suite_name != null && cli_arg_starts_with_dash(suite_name)) {
            if (json) {
                print_test_suite_unexpected_arg_json(suite_name);
                return finish_main(1);
            }
            return finish_main(print_unexpected_mode_arg("--test-suite", suite_name));
        }
        return finish_main(run_test_mode_with_self_for_suite(argv[0], suite_name, json));
    }

    if (cli_first_arg_matches(argc, argv, "--repl-server")) return finish_main(run_repl_server_mode(argc, argv, 1));

    if (cli_first_arg_matches(argc, argv, "--repl")) return finish_main(run_repl_mode(argc, argv, 1));

    if (cli_arg_starts_with_dash(mode_arg)) {
        return finish_main(print_unknown_option(mode_arg));
    }

    return finish_main(run_script_mode(argc, argv, 1));
}
