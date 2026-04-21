module lisp;

import std::io;
import std::core::mem;

alias AdvancedFfiSystemGroupFn = fn void(Interp* interp, int* pass, int* fail);

int g_double_release_finalizer_calls = 0;

fn void ffi_double_release_test_finalizer(void* payload) {
    g_double_release_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn bool ffi_test_error_contains(Value* value, char[] needle) {
    if (value == null || value.tag != ERROR || value.str_chars == null) return false;
    char[] msg = value.str_chars[:value.str_len];
    if (needle.len > msg.len) return false;
    for (usz i = 0; i <= msg.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (msg[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn bool advanced_group_filter_matches_selected_exact(char[] name) {
    char* filter = test_c_getenv("OMNI_ADVANCED_GROUP_FILTER");
    if (filter == null) return false;
    return advanced_group_filter_name_exact(filter, name);
}

fn void run_advanced_ffi_system_group(
    char[] name,
    AdvancedFfiSystemGroupFn group_fn,
    Interp* interp,
    int* pass,
    int* fail
) {
    if (!advanced_group_filter_matches_exact(name) &&
        !advanced_group_filter_matches_exact("advanced-ffi-system")) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_ffi_system_selected_exact_group(
    char[] name,
    AdvancedFfiSystemGroupFn group_fn,
    Interp* interp,
    int* pass,
    int* fail
) {
    if (!advanced_group_filter_matches_selected_exact(name)) {
        return;
    }
    emit_advanced_group_trace("begin", name);
    group_fn(interp, pass, fail);
    emit_advanced_group_trace("end", name);
}

fn void run_advanced_process_wait_event_driven_test(Interp* interp, int* pass, int* fail) {
    run_advanced_process_wait_event_driven_test_impl(interp, pass, fail);
}

fn void run_advanced_process_handle_concurrent_reuse_test(Interp* interp, int* pass, int* fail) {
    run_advanced_process_handle_concurrent_reuse_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_burst_delivery_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_burst_delivery_test_impl(interp, pass, fail);
}

fn void run_advanced_sigint_interrupt_routing_test(Interp* interp, int* pass, int* fail) {
    run_advanced_sigint_interrupt_routing_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_handle_registry_scaling_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_handle_registry_scaling_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_dispatch_owner_interp_isolation_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_dispatch_owner_interp_isolation_test_impl(interp, pass, fail);
}

fn void run_advanced_signal_callback_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_signal_callback_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_register_invoke_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_register_invoke_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_unhandle_idempotence_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_unhandle_idempotence_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_wrapper_failure_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_wrapper_failure_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_finalizer_scope_release_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_finalizer_scope_release_test_impl(interp, pass, fail);
}

fn void run_advanced_uv_timer_callback_handle_isolation_test(Interp* interp, int* pass, int* fail) {
    run_advanced_uv_timer_callback_handle_isolation_test_impl(interp, pass, fail);
}

fn void run_advanced_union_memory_limit_tests(Interp* interp, int* pass, int* fail) {
    // === UNION PATTERN MATCHING TESTS (Phase 6) ===
    // Define Option locally so this isolated group does not depend on other suites.
    setup(interp, "(define [union] (Option T) None (Some T))");

    // Nullary constructor match
    test_eq_interp(interp, "match None", "(match None (None 0) ((Some x) x))", 0, pass, fail);

    // Constructor with field match
    test_eq_interp(interp, "match Some", "(match (Some 42) (None 0) ((Some x) x))", 42, pass, fail);

    // Result type
    setup(interp, "(define [union] (Result T E) (Ok T) (Err E))");
    test_eq_interp(interp, "match Ok", "(match (Ok 1) ((Ok v) v) ((Err e) (- 0 1)))", 1, pass, fail);
    test_eq_interp(interp, "match Err", "(match (Err 99) ((Ok v) v) ((Err e) e))", 99, pass, fail);

    // Nested constructor match
    test_eq_interp(interp, "match nested Some", "(match (Some (Some 7)) ((Some (Some x)) x) (_ 0))", 7, pass, fail);

    // Wildcard in constructor
    test_eq_interp(interp, "match ctor wildcard", "(match (Ok 42) ((Ok _) 1) ((Err _) 2))", 1, pass, fail);

    // === MEMORY RECLAMATION TESTS ===
    run_advanced_memory_reclamation_tests(interp, pass, fail);

    // === RAISED LIMITS TESTS ===
    // Match with 17 clauses (was max 16, now 32)
    test_eq(interp, "match 17 clauses",
        "(match 17 (1 101) (2 102) (3 103) (4 104) (5 105) (6 106) (7 107) (8 108) (9 109) (10 110) (11 111) (12 112) (13 113) (14 114) (15 115) (16 116) (17 117))",
        117, pass, fail);
    // Handle with 9 effect clauses (was max 8, now 16)
    test_eq(interp, "handle 9 effects",
        "(handle (signal e9 42) (e1 v (resolve v)) (e2 v (resolve v)) (e3 v (resolve v)) (e4 v (resolve v)) (e5 v (resolve v)) (e6 v (resolve v)) (e7 v (resolve v)) (e8 v (resolve v)) (e9 v (+ v 1)))",
        43, pass, fail);
    // Large method table (>32 methods, was max 32, now 64)
    setup(interp, "(define [type] TestMT33 (^Integer val))");
    setup(interp, "(define (mt33-f (^(Value 1) n)) 1) (define (mt33-f (^(Value 2) n)) 2) (define (mt33-f (^(Value 3) n)) 3) (define (mt33-f (^(Value 4) n)) 4) (define (mt33-f (^(Value 5) n)) 5) (define (mt33-f (^(Value 6) n)) 6) (define (mt33-f (^(Value 7) n)) 7) (define (mt33-f (^(Value 8) n)) 8)");
    setup(interp, "(define (mt33-f (^(Value 9) n)) 9) (define (mt33-f (^(Value 10) n)) 10) (define (mt33-f (^(Value 11) n)) 11) (define (mt33-f (^(Value 12) n)) 12) (define (mt33-f (^(Value 13) n)) 13) (define (mt33-f (^(Value 14) n)) 14) (define (mt33-f (^(Value 15) n)) 15) (define (mt33-f (^(Value 16) n)) 16)");
    setup(interp, "(define (mt33-f (^(Value 17) n)) 17) (define (mt33-f (^(Value 18) n)) 18) (define (mt33-f (^(Value 19) n)) 19) (define (mt33-f (^(Value 20) n)) 20) (define (mt33-f (^(Value 21) n)) 21) (define (mt33-f (^(Value 22) n)) 22) (define (mt33-f (^(Value 23) n)) 23) (define (mt33-f (^(Value 24) n)) 24)");
    setup(interp, "(define (mt33-f (^(Value 25) n)) 25) (define (mt33-f (^(Value 26) n)) 26) (define (mt33-f (^(Value 27) n)) 27) (define (mt33-f (^(Value 28) n)) 28) (define (mt33-f (^(Value 29) n)) 29) (define (mt33-f (^(Value 30) n)) 30) (define (mt33-f (^(Value 31) n)) 31) (define (mt33-f (^(Value 32) n)) 32)");
    setup(interp, "(define (mt33-f (^(Value 33) n)) 33)");
    test_eq(interp, "method table >32", "(mt33-f 33)", 33, pass, fail);
}

fn void run_advanced_effect_union_limit_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_io_typed_effect_tests(interp, pass, fail);
    run_advanced_union_memory_limit_tests(interp, pass, fail);
}

fn void run_advanced_ffi_system_long_symbol_tests(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    setup(interp, "(define [ffi λ libc] (ffi_superlong_symbol_name_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_tailmarkerzz (^Integer n)) ^Integer)");
    test_error_contains(interp,
        "ffi λ long symbol name preserves tail",
        "(ffi_superlong_symbol_name_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_tailmarkerzz 7)",
        "tailmarkerzz",
        pass, fail);
}

fn void run_advanced_ffi_system_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_ffi_system_group("advanced-ffi-system-surface",
        &run_advanced_ffi_system_surface_tests, interp, pass, fail);
    run_advanced_ffi_system_group("advanced-ffi-system-foreign-handle-metadata-dict",
        &run_advanced_ffi_system_foreign_handle_metadata_dict_tests, interp, pass, fail);
    run_advanced_ffi_system_selected_exact_group("advanced-ffi-system-long-symbol",
        &run_advanced_ffi_system_long_symbol_tests, interp, pass, fail);
}
