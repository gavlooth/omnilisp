module main;

import std::io;
import lisp;

/**
 * AOT build: compile Lisp source to standalone binary.
 * Usage: ./main --build input.lisp -o output_binary
 */
fn int run_build(int argc, char** argv, int build_idx) {
    char* input_file = build_idx + 1 < argc ? argv[build_idx + 1] : null;
    char* output_binary = find_build_output_arg(argc, argv, build_idx);
    if (input_file == null) {
        io::printn("Usage: ./main --build input.lisp [-o output]");
        return 1;
    }

    char[512] default_out;
    if (output_binary == null) output_binary = derive_default_output_name(input_file, default_out[..]);

    char[] source;
    if (!load_build_source(input_file, &source)) return 1;

    bool print_last;
    bool print_all;
    parse_build_print_flags(argc, argv, &print_last, &print_all);

    // Step 1: Compile Lisp → C3
    io::printfn("Compiling %s...", (ZString)input_file);
    thread_registry_init();
    defer thread_registry_shutdown();

    lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    defer mem::free(interp);
    interp.init();
    defer interp.destroy();

    char[] c3_code = lisp::compile_to_c3_ext(source, interp, print_last, print_all);
    if (c3_code.len == 0) return 1;

    // Step 2: Write temp C3 file
    if (!write_aot_temp_file(c3_code)) return 1;

    // Step 3: Invoke c3c to compile
    io::printfn("Building %s...", (ZString)output_binary);
    int exit_code = run_aot_backend_compile(output_binary);
    if (exit_code != 0) {
        io::printn("Error: c3c compilation failed");
        return 1;
    }

    io::printfn("Built: %s", (ZString)output_binary);
    return 0;
}
