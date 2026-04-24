module main;

import std::io;
import lisp;

struct ReplModeOptions {
    bool json;
    bool load_project;
    bool load_file;
    char* project_dir;
    char* load_path;
    char* unexpected;
}

fn ReplModeOptions parse_repl_mode_options(int argc, char** argv, int repl_idx) {
    ReplModeOptions options = {};
    for (int i = repl_idx + 1; i < argc; i++) {
        char* arg = argv[i];
        if (str_eq(arg, "--json")) {
            options.json = true;
            continue;
        }
        if (str_eq(arg, "--project")) {
            if (options.load_project) {
                options.unexpected = arg;
                return options;
            }
            options.load_project = true;
            if (i + 1 < argc && !cli_arg_starts_with_dash(argv[i + 1])) {
                options.project_dir = argv[i + 1];
                i++;
            }
            continue;
        }
        if (str_eq(arg, "--load")) {
            if (options.load_file || i + 1 >= argc || cli_arg_starts_with_dash(argv[i + 1])) {
                options.unexpected = arg;
                return options;
            }
            options.load_file = true;
            options.load_path = argv[i + 1];
            i++;
            continue;
        }
        options.unexpected = arg;
        return options;
    }
    return options;
}

fn bool repl_load_file_into_interp(lisp::Interp* interp, char[] load_path, bool json, char[] label) {
    if (try source = io::file::load_temp((String)load_path)) {
        if (!lisp::push_source_dir(load_path, interp)) {
            io::printfn("Error: failed to allocate source directory context for '%s'", (ZString)load_path.ptr);
            return false;
        }
        defer lisp::pop_source_dir(interp);

        lisp::EvalResult result = lisp::run_program(source, interp);
        if (result.error.has_error) {
            if (result.error.line > 0) {
                io::printfn(
                    "Error: failed to load %s '%s' at line %d, column %d: %s",
                    (ZString)label.ptr,
                    (ZString)load_path.ptr,
                    (int)result.error.line,
                    (int)result.error.column,
                    (ZString)&result.error.message
                );
            } else {
                io::printfn(
                    "Error: failed to load %s '%s': %s",
                    (ZString)label.ptr,
                    (ZString)load_path.ptr,
                    (ZString)&result.error.message
                );
            }
            return false;
        }

        if (!json) {
            io::printfn("Loaded %s: %s", (ZString)label.ptr, (ZString)load_path.ptr);
        }
        return true;
    }

    EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
    if (!classify_file_read_path(load_path, &read_kind)) {
        print_file_read_error_text(label, load_path, read_kind);
        return false;
    }

    print_file_read_error_text(label, load_path, ENTRY_FILE_READ_GENERIC_FAILURE);
    return false;
}

fn bool repl_load_project_into_interp(lisp::Interp* interp, char* project_dir, bool json) {
    char[REPL_PROJECT_PATH_BUFFER_CAP] main_path_buf = {};
    usz main_path_len = 0;
    if (!repl_resolve_project_main_path(project_dir, main_path_buf[..], &main_path_len)) {
        io::printn("Error: no Omni project found or project path too long");
        return false;
    }

    if (!repl_load_file_into_interp(interp, main_path_buf[:main_path_len], json, "project entry")) {
        return false;
    }
    return true;
}

fn bool repl_load_requested_source(lisp::Interp* interp, ReplModeOptions* options) {
    if (options.load_project) {
        return repl_load_project_into_interp(interp, options.project_dir, options.json);
    }
    if (options.load_file) {
        char[] load_path;
        if (!cli_cstr_slice(options.load_path, &load_path)) {
            (void)print_cli_cstr_too_long("--repl --load file", options.json);
            return false;
        }
        return repl_load_file_into_interp(interp, load_path, options.json, "REPL preload");
    }
    return false;
}

fn int run_gen_e2e_mode(int argc, char** argv, int gen_idx) {
    char* unexpected = find_unexpected_mode_arg(argc, argv, gen_idx);
    if (unexpected != null) {
        return print_unexpected_mode_arg("--gen-e2e", unexpected);
    }

    lisp::Interp* interp = create_runtime_interp();
    if (interp == null) {
        io::printn("Error: failed to create runtime interpreter for --gen-e2e");
        return 1;
    }
    defer destroy_runtime_interp(interp);

    return lisp::generate_e2e_tests(interp) ? 0 : 1;
}

fn int run_repl_mode(int argc, char** argv, int repl_idx) {
    ReplModeOptions options = parse_repl_mode_options(argc, argv, repl_idx);
    if (options.unexpected != null) {
        if (options.json) {
            print_cli_unexpected_arg_json("--repl", options.unexpected);
            return 1;
        }
        return print_unexpected_mode_arg("--repl", options.unexpected);
    }
    if (options.load_project && options.load_file) {
        if (options.json) {
            print_cli_usage_json("choose either --repl --project or --repl --load <file>");
            return 1;
        }
        io::printn("Error: choose either --repl --project or --repl --load <file>");
        return 1;
    }
    if (options.json && (options.load_project || options.load_file)) {
        print_cli_usage_json("REPL preload is not supported with --json");
        return 1;
    }

    if (!options.json) {
        if (options.load_project || options.load_file) {
            io::printn("Starting Omni Lisp REPL with preload...");
        } else {
            io::printn("Starting Omni Lisp REPL...");
        }
    }

    lisp::Interp* interp = create_runtime_interp();
    if (interp == null) {
        if (options.json) {
            print_cli_error_json("runtime/bootstrap-failed", "failed to create runtime interpreter for --repl");
            return 1;
        }
        io::printn("Error: failed to create runtime interpreter for --repl");
        return 1;
    }
    defer destroy_runtime_interp(interp);

    if (options.json) interp.flags.console_to_stderr = true;

    if ((options.load_project || options.load_file) && !repl_load_requested_source(interp, &options)) {
        return 1;
    }

    if (options.json) {
        lisp::repl_json(interp);
    } else {
        lisp::repl(interp);
    }
    return 0;
}
