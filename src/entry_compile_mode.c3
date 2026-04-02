module main;

import std::io;
import lisp;

fn int run_compile_mode(char* input_file, char* output_file) {
    if (input_file == null || cli_arg_starts_with_dash(input_file)) {
        if (input_file != null) {
            io::printfn("Error: missing compile input file before option %s", (ZString)input_file);
        }
        return print_compile_usage();
    }
    if (output_file == null || cli_arg_starts_with_dash(output_file)) {
        if (output_file != null) {
            io::printfn("Error: missing compile output file before option %s", (ZString)output_file);
        }
        return print_compile_usage();
    }

    // Read input file
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
        EntryFileWriteKind output_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        if (!validate_output_target_path(output_file, &output_kind)) {
            print_compile_write_error(output_file[:cstr_len(output_file)], output_kind);
            return 1;
        }

        print_compile_start(input_file, output_file);

        // Initialize thread-local registry before interpreter
        thread_registry_init();

        // Initialize interpreter for symbol table (heap-allocated due to size)
        lisp::Interp* interp = create_plain_interp();
        if (interp == null) {
            print_compile_alloc_error();
            thread_registry_shutdown();
            return 1;
        }

        // Compile to C3
        char[] compile_error = "";
        char[] c3_code = lisp::compile_to_c3(source, interp, &compile_error);

        if (c3_code.len == 0) {
            if (compile_error.len > 0) print_compile_error_message(compile_error);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        // Write output file
        char[] output_path = output_file[:cstr_len(output_file)];
        if (try file = io::file::open((String)output_path, "w")) {
            char[] remaining = c3_code;
            bool write_ok = true;
            EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            while (write_ok && remaining.len > 0) {
                usz? written_opt = file.write(remaining);
                if (catch err = written_opt) {
                    write_kind = classify_file_write_errno();
                    write_ok = false;
                    break;
                }
                if (written_opt == 0) {
                    write_ok = false;
                    break;
                }
                remaining = remaining[written_opt..];
            }
            if (catch err = file.close()) {
                write_kind = classify_file_write_errno();
                write_ok = false;
            }
            if (!write_ok) {
                print_compile_write_error(output_path, write_kind);
                destroy_plain_interp(interp);
                thread_registry_shutdown();
                return 1;
            }
            print_compile_success(output_file);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 0;
        } else {
            print_compile_write_error(output_path, classify_file_write_errno());
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }
    } else {
        EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
        if (!classify_file_read_path(input_path, &read_kind)) {
            return print_compile_read_error(input_path, read_kind);
        }
        return print_compile_read_error(input_path, ENTRY_FILE_READ_GENERIC_FAILURE);
    }
}
