module main;

import std::io;
import lisp;

fn int run_script_mode(char* script_file) {
    char[] script_path = script_file[:cstr_len(script_file)];

    // Read script file
    if (try source = io::file::load_temp((String)script_path)) {
        // Initialize thread-local registry
        thread_registry_init();

        // Initialize interpreter
        lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
        interp.init();
        lisp::register_primitives(interp);
        lisp::register_stdlib(interp);
        interp.flags.jit_enabled = true;

        // Push script directory for relative import resolution
        if (!lisp::push_source_dir(script_path, interp)) {
            io::printn("Error: failed to allocate source directory context");
            interp.destroy();
            mem::free(interp);
            thread_registry_shutdown();
            return 1;
        }

        // Run the script (multiple expressions)
        char[] src = source;
        lisp::EvalResult r = lisp::run_program(src, interp);

        if (r.error.has_error) {
            print_script_eval_error(r);
            interp.destroy();
            mem::free(interp);
            thread_registry_shutdown();
            return 1;
        }

        print_script_result_if_non_nil(r, interp);

        interp.destroy();
        mem::free(interp);
        thread_registry_shutdown();
        return 0;
    } else {
        print_script_read_error(script_file);
        return 1;
    }
}
