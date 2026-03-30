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

    print_compile_start(input_file, output_file);

    // Read input file
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
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
        char[] c3_code = lisp::compile_to_c3(source, interp);

        if (c3_code.len == 0) {
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        // Write output file
        char[] output_path = output_file[:cstr_len(output_file)];
        if (try file = io::file::open((String)output_path, "w")) {
            char[] remaining = c3_code;
            bool write_ok = true;
            while (write_ok && remaining.len > 0) {
                usz? written_opt = file.write(remaining);
                if (catch err = written_opt) {
                    write_ok = false;
                    break;
                }
                if (written_opt == 0) {
                    write_ok = false;
                    break;
                }
                remaining = remaining[written_opt..];
            }
            if (catch err = file.close()) write_ok = false;
            if (!write_ok) {
                print_compile_write_error(output_file);
                destroy_plain_interp(interp);
                thread_registry_shutdown();
                return 1;
            }
            print_compile_success(output_file);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 0;
        } else {
            print_compile_write_error(output_file);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }
    } else {
        return print_compile_read_error(input_file);
    }
}
