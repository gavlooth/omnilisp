module main;

import std::io;
import lisp;

fn void run_lisp_test_suite_with_registry() {
    // Retained setup hook while entry modes converge on post-region runtime init.
    thread_registry_init();
    io::printfn("Thread-local runtime setup initialized.");

    // Run Lisp interpreter tests
    lisp::run_lisp_tests();

    if (mem_stats_enabled()) {
        lisp::print_memory_promotion_stats();
        scope_print_memory_stats();
    }

    thread_registry_shutdown();
    io::printfn("System ready.");
}
