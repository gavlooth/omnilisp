module lisp;

import std::io;

fn void run_memory_lifetime_benchmark_tests(Interp* interp, int* pass, int* fail) {
    (void)pass;
    (void)fail;
    emit_test_section_banner("Memory Lifetime Benchmark Tests");

    // Keep benchmark-heavy passes out of the soak slice so one process does not
    // accumulate both stress graphs and benchmark allocations together.
    run_memory_lifetime_boundary_benchmarks(interp);
}

fn void run_allocator_benchmark_tests(Interp* interp, int* pass, int* fail) {
    (void)pass;
    (void)fail;
    emit_test_section_banner("Allocator Benchmark Tests");
    run_memory_lifetime_ast_arena_benchmark(interp);
}
