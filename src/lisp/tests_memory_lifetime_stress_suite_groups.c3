module lisp;

import std::io;

fn void run_memory_lifetime_soak_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Lifetime Soak Tests ---");

    // Soak includes the full smoke suite plus stress coverage.
    run_memory_lifetime_smoke_tests(interp, pass, fail);
    run_memory_lifetime_boundary_negative_stress_tests(interp, pass, fail);
    run_memory_lifetime_hot_budget_tests(interp, pass, fail);
    run_memory_lifetime_promotion_context_tests(interp, pass, fail);
}
