module lisp;

import std::io;

fn void run_memory_lifetime_smoke_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Memory Lifetime Smoke Tests ---");

    if (!run_memory_lifetime_result_and_coroutine_tests(interp, pass, fail)) return;
    run_memory_lifetime_finalize_parity_tests(interp, pass, fail);
    run_memory_lifetime_lane_barrier_tests(interp, pass, fail);
    run_memory_lifetime_root_fallback_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_restore_tests(interp, pass, fail);
    run_memory_lifetime_boundary_copy_fault_tests(interp, pass, fail);
    run_memory_lifetime_boundary_invariant_policy_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_interleaving_test(interp, pass, fail);
    run_memory_lifetime_boundary_transition_regression_tests(interp, pass, fail);
    run_memory_lifetime_boundary_return_classification_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_tests(interp, pass, fail);
    run_memory_lifetime_boundary_graph_audit_tests(interp, pass, fail);
    run_memory_lifetime_boundary_txn_protocol_tests(interp, pass, fail);
    run_memory_lifetime_run_program_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_run_program_error_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(interp, pass, fail);
    run_memory_lifetime_env_copy_escape_mode_tests(interp, pass, fail);
}
