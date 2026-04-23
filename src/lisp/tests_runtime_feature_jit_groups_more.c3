module lisp;


import std::io;
import std::core::mem;
import main;

fn bool jit_policy_str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return true;
    if (needle.len > haystack.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

alias JitPolicyTestFn = fn void(Interp* interp, int* pass, int* fail);

fn bool jit_policy_filter_name_exact(char* filter, char[] name) {
    usz len = 0;
    while (filter[len] != 0) len++;
    if (len != name.len) return false;
    for (usz i = 0; i < len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return true;
}

fn bool jit_policy_filter_matches(char[] name) {
    char* filter = test_c_getenv("OMNI_JIT_POLICY_FILTER");
    if (filter == null) return true;
    usz start = 0;
    while (true) {
        usz end = start;
        while (filter[end] != 0 && filter[end] != ',') end++;
        usz len = end - start;
        if (len == name.len) {
            bool same = true;
            for (usz i = 0; i < len; i++) {
                if (filter[start + i] != name[i]) {
                    same = false;
                    break;
                }
            }
            if (same) return true;
        }
        if (filter[end] == 0) break;
        start = end + 1;
    }
    return false;
}

fn void run_jit_policy_case(char[] name, JitPolicyTestFn test_fn, Interp* interp, int* pass, int* fail) {
    if (!jit_policy_filter_matches(name)) {
        return;
    }
    test_fn(interp, pass, fail);
}

fn void run_jit_policy_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("JIT Policy Tests");

    bool saved_jit_enabled = interp.flags.jit_enabled;
    bool run_stdlib_teardown_redefinition_only =
        test_c_getenv("OMNI_JIT_POLICY_STDLIB_TEARDOWN_REDEFINITION") != null;
    if (run_stdlib_teardown_redefinition_only) {
        run_jit_policy_stdlib_teardown_redefinition_test(interp, pass, fail);
        interp.flags.jit_enabled = saved_jit_enabled;
        jit_cache_clear();
        return;
    }

    bool parsed = true;
    if (jit_policy_filter_matches("warm-cache")) {
        parsed = run_jit_policy_warm_cache_tests(interp, pass, fail);
    }
    if (parsed) {
        run_jit_policy_case("cache-overwrite-count", &run_jit_policy_cache_overwrite_count_test, interp, pass, fail);
        run_jit_policy_case("cache-probe-saturation-recovery", &run_jit_policy_cache_probe_saturation_recovery_test, interp, pass, fail);
        run_jit_policy_case("retired-code-tombstone-table", &run_jit_policy_retired_code_tombstone_table_test, interp, pass, fail);
        run_jit_policy_case("gc-safe-point", &run_jit_policy_gc_safe_point_test, interp, pass, fail);
        run_jit_policy_case("stale-compiled-fn-guard", &run_jit_policy_stale_compiled_fn_guard_test, interp, pass, fail);
        run_jit_policy_case("foreign-compiled-fn-owner", &run_jit_policy_foreign_compiled_fn_owner_test, interp, pass, fail);
        run_jit_policy_case("boundary-reset", &run_jit_policy_boundary_reset_test, interp, pass, fail);
        run_jit_policy_case("stale-raise-scrub", &run_jit_policy_stale_raise_scrub_test, interp, pass, fail);
        run_jit_policy_case("variadic-no-fallback", &run_jit_policy_variadic_no_fallback_test, interp, pass, fail);
        run_jit_policy_case("match-binding-growth-failure", &run_jit_policy_match_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-binding-growth-failure", &run_jit_policy_multi_arg_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("tail-constructor-escape-opcode", &run_jit_policy_tail_constructor_escape_opcode_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-primitive-buffer-failure", &run_jit_policy_multi_arg_primitive_buffer_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-method-table-buffer-failure", &run_jit_policy_multi_arg_method_table_buffer_failure_test, interp, pass, fail);
        run_jit_policy_case("match-guard-binding-growth-failure", &run_jit_policy_match_guard_binding_growth_failure_test, interp, pass, fail);
        run_jit_policy_case("multi-interp-lifetime", &run_jit_policy_multi_interp_lifetime_test, interp, pass, fail);
        run_jit_policy_case("continuation-teardown", &run_jit_policy_continuation_teardown_test, interp, pass, fail);
        run_jit_policy_case("shared-handle-state-teardown", &run_jit_policy_shared_handle_state_teardown_test, interp, pass, fail);
        run_jit_policy_case("cross-interp-continuation-guard", &run_jit_policy_cross_interp_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("escaped-handle-continuation-guard", &run_jit_policy_escaped_handle_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("side-effect-escaped-handle-continuation-guard", &run_jit_policy_side_effect_escaped_handle_continuation_guard_test, interp, pass, fail);
        run_jit_policy_case("multishot-capture-scope-guard-clone", &run_jit_policy_multishot_capture_scope_guard_clone_test, interp, pass, fail);
        run_jit_policy_case("shadowed-primitive-call", &run_jit_policy_shadowed_primitive_call_test, interp, pass, fail);
        run_jit_policy_case("capture-boundary-reset", &run_jit_policy_capture_boundary_reset_test, interp, pass, fail);
        run_jit_policy_case("capture-discard-recycles-stack-ctx", &run_jit_policy_capture_discard_recycles_stack_ctx_test, interp, pass, fail);
        run_jit_policy_case("tco-stale-generation-moved-binding-copy", &run_jit_policy_tco_stale_generation_moved_binding_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-persistent-parent-root-rewrite", &run_jit_policy_tco_persistent_parent_root_rewrite_test, interp, pass, fail);
        run_jit_policy_case("tco-persistent-parent-stale-generation-rewrite", &run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test, interp, pass, fail);
        run_jit_policy_case("root-box-extend-copies-cross-scope-closure-value", &run_jit_policy_root_box_extend_copies_cross_scope_closure_value_test, interp, pass, fail);
        run_jit_policy_case("closure-persistent-env-copy-scope-contract", &run_jit_policy_closure_persistent_env_copy_scope_contract_test, interp, pass, fail);
        run_jit_policy_case("cross-scope-alias-policy", &run_jit_policy_cross_scope_alias_policy_test, interp, pass, fail);
        run_jit_policy_case("invalid-primitive-state-fails-closed", &run_jit_policy_invalid_primitive_state_fails_closed_test, interp, pass, fail);
        run_jit_policy_case("pending-raise-string-alloc-failure", &run_jit_policy_pending_raise_string_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("pending-raise-payload-alloc-failure", &run_jit_policy_pending_raise_payload_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("dispatch-payload-alloc-failure-does-not-preseed-raise", &run_jit_policy_dispatch_payload_alloc_failure_does_not_preseed_raise_test, interp, pass, fail);
        run_jit_policy_case("handle-continuation-alloc-failure", &run_jit_policy_handle_continuation_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("handler-arg-list-alloc-failure", &run_jit_policy_handler_arg_list_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("handle-clause-bind-failure-releases-continuation", &run_jit_policy_handle_clause_bind_failure_releases_continuation_test, interp, pass, fail);
        run_jit_policy_case("multi-arg-malformed-list-fails-closed", &run_jit_policy_multi_arg_malformed_list_fails_closed_test, interp, pass, fail);
        run_jit_policy_case("quasiquote-cons-alloc-failure", &run_jit_policy_quasiquote_cons_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("variadic-rest-list-alloc-failure", &run_jit_policy_variadic_rest_list_alloc_failure_test, interp, pass, fail);
        run_jit_policy_case("iterator-array-thunk-boundary", &run_jit_policy_iterator_array_thunk_boundary_test, interp, pass, fail);
        run_jit_policy_case("tco-cons-iterator-edge-copy", &run_jit_policy_tco_cons_iterator_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-cons-releasing-scalar-edge-copy", &run_jit_policy_tco_cons_releasing_scalar_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-partial-shared-wrapper-edge-copy", &run_jit_policy_tco_partial_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-iterator-shared-wrapper-edge-copy", &run_jit_policy_tco_iterator_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-foreign-partial-shared-wrapper-edge-copy", &run_jit_policy_tco_foreign_partial_shared_wrapper_edge_copy_test, interp, pass, fail);
        run_jit_policy_case("tco-foreign-shared-wrapper-copy", &run_jit_policy_tco_foreign_shared_wrapper_copy_test, interp, pass, fail);
        run_jit_policy_case("closure-method-iterator-boundary", &run_jit_policy_closure_method_iterator_boundary_test, interp, pass, fail);
        run_jit_policy_case("rec-closure-type-sig-copy-failure", &run_jit_policy_rec_closure_type_sig_copy_failure_test, interp, pass, fail);
        run_jit_policy_case("stdlib-teardown-redefinition", &run_jit_policy_stdlib_teardown_redefinition_test, interp, pass, fail);
        run_jit_policy_case("mutable-local-null-box-helpers-fail-closed", &run_jit_policy_mutable_local_null_box_helpers_fail_closed_test, interp, pass, fail);
    }

    interp.flags.jit_enabled = saved_jit_enabled;
    jit_cache_clear();
}

fn void run_jit_policy_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_jit_policy_tests, pass, fail);
}
