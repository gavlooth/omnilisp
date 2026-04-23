module lisp;

import std::io;

fn void run_memory_lifetime_smoke_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Memory Lifetime Smoke Tests");

    if (!run_memory_lifetime_result_and_coroutine_tests(interp, pass, fail)) return;
    run_memory_lifetime_finalize_parity_tests(interp, pass, fail);
    run_memory_lifetime_lane_barrier_tests(interp, pass, fail);
    run_memory_lifetime_root_fallback_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_restore_tests(interp, pass, fail);
    run_memory_lifetime_boundary_copy_fault_tests(interp, pass, fail);
    run_memory_lifetime_boundary_invariant_policy_tests(interp, pass, fail);
    run_memory_lifetime_boundary_scope_scan_budget_tests(interp, pass, fail);
    run_memory_lifetime_boundary_zero_hint_scope_cache_test(interp, pass, fail);
    run_memory_lifetime_boundary_explicit_target_scope_cache_test(interp, pass, fail);
    run_memory_lifetime_boundary_scope_interleaving_test(interp, pass, fail);
    run_memory_lifetime_boundary_nested_effect_payload_test(interp, pass, fail);
    run_memory_lifetime_boundary_wide_scalar_payload_test(interp, pass, fail);
    run_memory_lifetime_boundary_shared_composite_cycle_payload_test(interp, pass, fail);
    run_memory_lifetime_boundary_instance_payload_alias_test(interp, pass, fail);
    run_memory_lifetime_boundary_list_child_scope_alias_test(interp, pass, fail);
    run_memory_lifetime_boundary_transition_regression_tests(interp, pass, fail);
    run_memory_lifetime_boundary_return_classification_tests(interp, pass, fail);
    run_memory_lifetime_boundary_commit_escape_tests(interp, pass, fail);
    run_memory_lifetime_boundary_graph_audit_tests(interp, pass, fail);
    run_memory_lifetime_boundary_txn_protocol_tests(interp, pass, fail);
    run_memory_lifetime_run_program_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_run_program_error_boundary_state_test(interp, pass, fail);
    run_memory_lifetime_tco_string_argument_list_promotion_test(interp, pass, fail);
    run_memory_lifetime_tco_env_copy_failure_fail_closed_test(interp, pass, fail);
    run_memory_lifetime_tco_recycle_env_copy_failure_restore_test(interp, pass, fail);
    run_memory_lifetime_tco_nested_temp_graph_recycle_guard_test(interp, pass, fail);
    run_memory_lifetime_tco_closure_payload_temp_recycle_guard_test(interp, pass, fail);
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(interp, pass, fail);
    run_memory_lifetime_target_chain_shared_wrapper_payload_alias_test(interp, pass, fail);
    run_memory_lifetime_method_table_abort_cleanup_test(interp, pass, fail);
    run_memory_lifetime_shared_wrapper_partial_cleanup_test(interp, pass, fail);
    run_memory_lifetime_env_copy_escape_mode_tests(interp, pass, fail);
    run_memory_lifetime_runtime_helper_alloc_tests(interp, pass, fail);
}
