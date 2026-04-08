module main;

import std::io;

/**
 * Run all stack engine tests.
 */
fn void run_stack_engine_tests(int* out_pass, int* out_fail) {
    g_stack_quiet_output =
        getenv("OMNI_TEST_QUIET") != null ||
        (stack_summary_enabled() && getenv("OMNI_TEST_VERBOSE") == null);

    if (!stack_quiet_enabled()) {
        io::printfn("\n========================================");
        io::printfn("STACK ENGINE TESTS");
        io::printfn("========================================\n");
    }

    int pass = 0;
    int fail = 0;
    stack_defer_metrics_reset();

    if (test_stack_region()) { stack_print_pass("stack region alloc/free"); pass++; }
    else { io::printfn("  FAIL: stack region alloc/free"); fail++; }

    if (test_pool_lifecycle()) { stack_print_pass("pool create/recycle"); pass++; }
    else { io::printfn("  FAIL: pool create/recycle"); fail++; }

    if (test_pool_double_destroy_idempotent()) { stack_print_pass("pool double-destroy idempotent"); pass++; }
    else { io::printfn("  FAIL: pool double-destroy idempotent"); fail++; }

    if (test_stack_ctx_thread_affinity_state()) { stack_print_pass("stack thread-affinity ownership"); pass++; }
    else { io::printfn("  FAIL: stack thread-affinity ownership"); fail++; }

    if (test_basic_coro()) { stack_print_pass("basic context run + complete"); pass++; }
    else { io::printfn("  FAIL: basic context run + complete"); fail++; }

    if (test_stack_ctx_second()) { stack_print_pass("second context entry"); pass++; }
    else { io::printfn("  FAIL: second context entry"); fail++; }

    if (test_suspend_resume()) { stack_print_pass("suspend/resume (generator)"); pass++; }
    else { io::printfn("  FAIL: suspend/resume (generator)"); fail++; }

    if (test_stack_ctx_recursion()) { stack_print_pass("recursion on separate stack"); pass++; }
    else { io::printfn("  FAIL: recursion on separate stack"); fail++; }

    if (test_two_coros_interleaved()) { stack_print_pass("two contexts interleaved"); pass++; }
    else { io::printfn("  FAIL: two contexts interleaved"); fail++; }

    if (test_fpu_preservation()) { stack_print_pass("FPU state preserved across switch"); pass++; }
    else { io::printfn("  FAIL: FPU state preserved across switch"); fail++; }

    if (stack_runtime_asan_enabled()) {
        if (!stack_quiet_enabled()) {
            io::printfn("  SKIP: stack overflow recovery (ASAN runtime)");
        }
    } else if (test_stack_overflow_recovery()) { stack_print_pass("stack overflow recovery"); pass++; }
    else { io::printfn("  FAIL: stack overflow recovery"); fail++; }

    if (test_stack_ctx_clone_multishot()) { stack_print_pass("clone/resume multi-shot"); pass++; }
    else { io::printfn("  FAIL: clone/resume multi-shot"); fail++; }

    if (test_stack_ctx_scope_create_in_context()) { stack_print_pass("scope create inside stack context"); pass++; }
    else { io::printfn("  FAIL: scope create inside stack context"); fail++; }

    if (test_stack_ctx_fiber_temp_clone_discard_stress()) { stack_print_pass("fiber-temp clone/discard stress"); pass++; }
    else { io::printfn("  FAIL: fiber-temp clone/discard stress"); fail++; }

    if (test_stack_ctx_fiber_temp_retention_guard()) { stack_print_pass("fiber-temp retention guard"); pass++; }
    else { io::printfn("  FAIL: fiber-temp retention guard"); fail++; }

    if (test_stack_ctx_defer_destroy_lifo()) { stack_print_pass("defer destroy order (LIFO)"); pass++; }
    else { io::printfn("  FAIL: defer destroy order (LIFO)"); fail++; }

    if (test_stack_ctx_defer_undefer_pop()) { stack_print_pass("defer undefer pop"); pass++; }
    else { io::printfn("  FAIL: defer undefer pop"); fail++; }

    if (test_stack_ctx_defer_clone_hook()) { stack_print_pass("defer clone hook"); pass++; }
    else { io::printfn("  FAIL: defer clone hook"); fail++; }

    if (test_stack_ctx_defer_update_arg()) { stack_print_pass("defer update arg"); pass++; }
    else { io::printfn("  FAIL: defer update arg"); fail++; }

    if (test_stack_ctx_defer_update_arg_clone_isolation()) { stack_print_pass("defer update arg clone isolation"); pass++; }
    else { io::printfn("  FAIL: defer update arg clone isolation"); fail++; }

    if (test_stack_ctx_lifecycle_destroy_isolation()) { stack_print_pass("lifecycle destroy isolation"); pass++; }
    else { io::printfn("  FAIL: lifecycle destroy isolation"); fail++; }

    if (test_stack_ctx_lifecycle_clone_hook()) { stack_print_pass("lifecycle clone hook"); pass++; }
    else { io::printfn("  FAIL: lifecycle clone hook"); fail++; }

    if (test_stack_pool_shutdown_defer_cross_ctx_order()) { stack_print_pass("pool shutdown defer cross-context order"); pass++; }
    else { io::printfn("  FAIL: pool shutdown defer cross-context order"); fail++; }

    if (!stack_quiet_enabled()) {
        io::printfn("\nStack engine: %d passed, %d failed", pass, fail);
    }
    emit_stack_defer_summary();
    emit_stack_summary(pass, fail);
    if (!stack_quiet_enabled()) {
        io::printfn("========================================\n");
    }

    if (out_pass != null) *out_pass = pass;
    if (out_fail != null) *out_fail = fail;
}
