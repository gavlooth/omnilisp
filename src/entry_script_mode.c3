module main;

import std::io;
import lisp;

fn int run_script_mode(int argc, char** argv, int script_idx) {
    char* unexpected = find_unexpected_mode_arg(argc, argv, script_idx);
    if (unexpected != null) {
        return print_unexpected_mode_arg("script", unexpected);
    }

    char* script_file = argv[script_idx];
    char[] script_path;
    if (!cli_cstr_slice(script_file, &script_path)) return print_cli_cstr_too_long("script path");

    EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
    if (!classify_file_read_path(script_path, &read_kind)) {
        print_script_read_error(script_path, read_kind);
        return 1;
    }

    // Read script file
    if (try source = io::file::load_temp((String)script_path)) {
        lisp::Interp* interp = create_runtime_interp();
        if (interp == null) {
            io::printfn("Error: failed to create runtime interpreter for script '%s'", (ZString)script_file);
            return 1;
        }
        defer destroy_runtime_interp(interp);

        // Push script directory for relative import resolution
        if (!lisp::push_source_dir(script_path, interp)) {
            io::printfn("Error: failed to allocate source directory context for '%s'", (ZString)script_file);
            return 1;
        }

        // Run the script (multiple expressions)
        char[] src = source;
        lisp::EvalResult r = lisp::run_program(src, interp);

        if (r.error.has_error) {
            print_script_eval_error(r);
            return 1;
        }

        print_script_result_if_non_nil(r, interp);
        return 0;
    } else {
        print_script_read_error(script_path, ENTRY_FILE_READ_GENERIC_FAILURE);
        return 1;
    }
}
