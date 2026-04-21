module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_escape_env_lane_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: unified one-scope escape-env mode allocates env frames in ESCAPE lane.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool saved_escape_mode = interp.escape_env_mode;

        main::ScopeRegion* s = main::scope_create(saved_scope);
        interp.current_scope = s;
        interp.escape_env_mode = true;

        Env* e = make_env(interp, null);
        bool ok = e != null &&
                  e.scope_gen == s.escape_generation &&
                  main::is_in_scope((void*)e, s);

        interp.escape_env_mode = saved_escape_mode;
        interp.current_scope = saved_scope;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: unified escape-env uses current ESCAPE lane");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: unified escape-env uses current ESCAPE lane");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_escape_mode_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_copy_parent_closure_env_scope_retain_test(interp, pass, fail);
    run_memory_lifetime_copy_parent_closure_env_scope_parent_detach_test(interp, pass, fail);
    run_memory_lifetime_copy_parent_closure_wrapper_alloc_fail_test(interp, pass, fail);
    run_memory_lifetime_env_copy_nested_closure_retain_test(interp, pass, fail);
    run_memory_lifetime_env_copy_closure_wrapper_alloc_fail_test(interp, pass, fail);
    run_memory_lifetime_env_copy_closure_retain_stress_test(interp, pass, fail);
    run_memory_lifetime_env_copy_memo_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_reuses_target_owned_frame_test(interp, pass, fail);
    run_memory_lifetime_env_copy_persistent_parent_rewrite_test(interp, pass, fail);
    run_memory_lifetime_env_copy_mixed_chain_rewrite_test(interp, pass, fail);
    run_memory_lifetime_env_copy_primitive_policy_test(interp, pass, fail);
    run_memory_lifetime_env_copy_shared_wrapper_policy_test(interp, pass, fail);
    run_memory_lifetime_escape_env_lane_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_fault_surface_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_invalid_persistent_in_place_guard_test(interp, pass, fail);
    run_memory_lifetime_env_copy_stale_generation_persistent_parent_rewrite_test(interp, pass, fail);
    run_memory_lifetime_env_copy_rejects_undelimited_closure_alias_test(interp, pass, fail);
    run_memory_lifetime_env_copy_rejects_cross_scope_undelimited_alias_test(interp, pass, fail);
    run_memory_lifetime_env_copy_clones_global_iterator_closure_test(interp, pass, fail);
    run_memory_lifetime_env_copy_memoizes_iterator_closure_payload_clone_test(interp, pass, fail);
    run_memory_lifetime_env_copy_self_referential_closure_payload_test(interp, pass, fail);
    run_memory_lifetime_env_copy_transactional_binding_rollback_test(interp, pass, fail);
    run_memory_lifetime_env_copy_failure_invalidates_promotion_ctx_test(interp, pass, fail);
    run_memory_lifetime_env_copy_rollback_preserves_shared_iterator_payload_test(interp, pass, fail);
    run_memory_lifetime_env_copy_iterator_wrapper_failure_rollback_test(interp, pass, fail);
    run_memory_lifetime_env_copy_continuation_rollback_test(interp, pass, fail);
    run_memory_lifetime_env_copy_nested_cons_rollback_test(interp, pass, fail);
    run_memory_lifetime_env_copy_nested_array_rollback_test(interp, pass, fail);
    run_memory_lifetime_env_copy_nested_cons_wrapper_alias_test(interp, pass, fail);
}
