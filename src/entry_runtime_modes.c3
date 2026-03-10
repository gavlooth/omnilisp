module main;

import std::io;
import lisp;

fn int run_gen_e2e_mode() {
    thread_registry_init();
    lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    interp.init();
    lisp::register_primitives(interp);
    lisp::register_stdlib(interp);
    interp.flags.jit_enabled = true;

    lisp::generate_e2e_tests(interp);

    interp.destroy();
    mem::free(interp);
    thread_registry_shutdown();
    return 0;
}

fn int run_repl_mode() {
    io::printn("Starting Lisp REPL...");

    // Initialize thread-local registry first
    thread_registry_init();

    // Initialize interpreter
    lisp::Interp* interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    interp.init();
    lisp::register_primitives(interp);
    lisp::register_stdlib(interp);
    interp.flags.jit_enabled = true;

    // Run REPL
    lisp::repl(interp);

    // Clean up
    interp.destroy();
    mem::free(interp);
    thread_registry_shutdown();
    return 0;
}
