module main;

import std::io;
import lisp;

fn int run_compile_mode(char* input_file, char* output_file) {
    if (input_file == null || output_file == null) {
        return print_compile_usage();
    }

    print_compile_start(input_file, output_file);

    // Read input file
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
        // Initialize thread-local registry before interpreter
        thread_registry_init();

        // Initialize interpreter for symbol table (heap-allocated due to size)
        lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
        interp.init();

        // Compile to C3
        char[] c3_code = lisp::compile_to_c3(source, interp);

        if (c3_code.len == 0) {
            interp.destroy();
            mem::free(interp);
            thread_registry_shutdown();
            return 1;
        }

        // Write output file
        char[] output_path = output_file[:cstr_len(output_file)];
        if (try file = io::file::open((String)output_path, "w")) {
            defer (void)file.close();
            file.write(c3_code)!!;
            print_compile_success(output_file);
            interp.destroy();
            mem::free(interp);
            thread_registry_shutdown();
            return 0;
        } else {
            print_compile_write_error(output_file);
            interp.destroy();
            mem::free(interp);
            thread_registry_shutdown();
            return 1;
        }
    } else {
        return print_compile_read_error(input_file);
    }
}
