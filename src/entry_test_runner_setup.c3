module main;

import std::io;
import lisp;

fn int run_lisp_test_suite_with_registry(int* out_pass, int* out_fail) {
    // Retained setup hook while entry modes converge on post-region runtime init.
    thread_registry_init();
    io::printfn("Thread-local runtime setup initialized.");

    // Run Lisp interpreter tests
    int pass = 0;
    int fail = 0;
    bool ok = lisp::run_lisp_tests(&pass, &fail);

    if (mem_stats_enabled()) {
        lisp::print_memory_promotion_stats();
        scope_print_memory_stats();
    }

    thread_registry_shutdown();
    io::printfn("System ready.");
    if (out_pass != null) *out_pass = pass;
    if (out_fail != null) *out_fail = fail;
    return ok ? 0 : 1;
}
