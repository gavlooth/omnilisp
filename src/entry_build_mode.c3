module main;

import std::io;
import lisp;

extern fn int c_chdir(char* path) @extern("chdir");

/**
 * AOT build: compile Omni source to a standalone binary.
 * Usage: omni --build <file> [-o output]
 */
fn int run_build(int argc, char** argv, int build_idx) {
    char* input_file = build_idx + 1 < argc ? argv[build_idx + 1] : null;
    bool output_missing = false;
    char* output_binary = find_build_output_arg(argc, argv, build_idx, &output_missing);
    if (input_file == null || cli_arg_starts_with_dash(input_file)) {
        if (input_file != null) {
            io::printfn("Error: missing input file before option %s", (ZString)input_file);
        }
        io::printn("Usage: omni --build <file> [-o output]");
        return 1;
    }
    if (output_missing) {
        io::printn("Error: missing output path after -o");
        io::printn("Usage: omni --build <file> [-o output]");
        return 1;
    }
    char* extra = find_extra_mode_arg(
        argc, argv, build_idx, 1,
        false, false, false,
        true, true, true);
    if (extra != null) {
        return print_unexpected_mode_arg("--build", extra);
    }

    char[BUILD_PATH_BUFFER_CAP] cwd_buf;
    if (c_getcwd(&cwd_buf[0], cwd_buf.len) == null) {
        io::printn("Error: failed to read current working directory");
        return 1;
    }

    char[BUILD_PATH_BUFFER_CAP] input_abs_buf;
    if (input_file[0] != '/') {
        if (try input_abs = io::bprintf(&input_abs_buf, "%s/%s", (ZString)&cwd_buf[0], (ZString)input_file)) {
            input_file = &input_abs[0];
        } else {
            io::printn("Error: input path too long");
            return 1;
        }
    }

    char[BUILD_PATH_BUFFER_CAP] output_abs_buf;
    if (output_binary != null && output_binary[0] != '/') {
        if (try output_abs = io::bprintf(&output_abs_buf, "%s/%s", (ZString)&cwd_buf[0], (ZString)output_binary)) {
            output_binary = &output_abs[0];
        } else {
            io::printn("Error: output path too long");
            return 1;
        }
    }

    char[BUILD_PATH_BUFFER_CAP] repo_root_buf;
    usz repo_root_len = 0;
    if (!resolve_build_repo_root(argv[0], repo_root_buf[..], &repo_root_len)) {
        io::printn("Error: failed to resolve repo root for --build");
        io::printn("Hint: run --build from a full Omni checkout, or set OMNI_REPO_ROOT to that checkout, or use the example repo's local ./omni wrapper.");
        return 1;
    }
    if (repo_root_len == 0 || c_chdir(&repo_root_buf[0]) != 0) {
        io::printn("Error: failed to chdir to repo root for --build");
        return 1;
    }
    defer (void)c_chdir(&cwd_buf[0]);

    char[BUILD_PATH_BUFFER_CAP] default_out;
    if (output_binary == null) {
        output_binary = derive_default_output_name(input_file, default_out[..]);
        if (output_binary == null) {
            io::printn("Error: derived default output path is too long");
            return 1;
        }
    }

    char[] input_path;
    if (!cli_cstr_slice(input_file, &input_path)) return print_cli_cstr_too_long("--build input file");
    char[] output_path;
    if (!cli_cstr_slice(output_binary, &output_path)) return print_cli_cstr_too_long("--build output binary");

    EntryFileWriteKind output_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    if (!validate_output_target_path(output_binary, &output_kind)) {
        print_file_write_error_text("output binary", output_path, output_kind);
        return 1;
    }

    char[] source;
    EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
    if (!load_build_source(input_file, &source, &read_kind)) {
        print_file_read_error_text("input file", input_path, read_kind);
        return 1;
    }

    bool print_last;
    bool print_all;
    parse_build_print_flags(argc, argv, &print_last, &print_all);

    // Step 1: Compile Lisp → C3
    io::printfn("Compiling %s...", (ZString)input_file);
    thread_registry_init();
    defer thread_registry_shutdown();

    lisp::Interp* interp = create_plain_interp();
    if (interp == null) {
        io::printn("Error: failed to allocate compiler interpreter");
        return 1;
    }
    defer destroy_plain_interp(interp);

    char[] compile_error = "";
    char[] c3_code = lisp::compile_to_c3_ext(source, interp, print_last, print_all, &compile_error);
    if (c3_code.len == 0) {
        if (compile_error.len > 0) print_compile_error_message(compile_error);
        return 1;
    }

    // Step 2: Write temp C3 file
    char[256] temp_c3_path;
    usz temp_c3_path_len = 0;
    if (!write_aot_temp_file(c3_code, temp_c3_path[..], &temp_c3_path_len)) return 1;
    defer (void)io::file::delete((String)temp_c3_path[..temp_c3_path_len]);

    // Step 3: Invoke c3c to compile
    io::printfn("Building %s...", (ZString)output_binary);
    int exit_code = run_aot_backend_compile(&temp_c3_path[0], output_binary);
    if (exit_code != 0) return 1;

    io::printfn("Built: %s", (ZString)output_binary);
    return 0;
}
