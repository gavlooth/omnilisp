module lisp;


import std::io;
import main;
import pika;
// =============================================================================
// SECTION 10: TESTS
// =============================================================================

fn void emit_test_summary(char[] suite, int pass, int fail) {
    if (!test_summary_enabled()) return;
    io::printfn("OMNI_TEST_SUMMARY suite=%s pass=%d fail=%d", (ZString)suite, pass, fail);
}

fn void emit_lisp_slice_memory_flag(char* active_slice) {
    bool high_mem = lisp_slice_high_memory(active_slice);
    if (active_slice == null || lisp_slice_matches(active_slice, "all")) {
        io::printfn(
            "OMNI_TEST_SUMMARY suite=lisp_slice slice=all high_memory=%d",
            high_mem ? 1 : 0
        );
        if (high_mem && !test_quiet_requested()) {
            io::printn("[WARN] lisp slice 'all' includes high-memory groups: memory-lifetime-soak, memory-lifetime-bench, allocator-bench, memory-stress");
        }
        return;
    }

    io::printfn(
        "OMNI_TEST_SUMMARY suite=lisp_slice slice=%s high_memory=%d",
        (ZString)active_slice,
        high_mem ? 1 : 0
    );
    if (high_mem && !test_quiet_requested()) {
        io::printfn("[WARN] high-memory lisp slice selected: %s", (ZString)active_slice);
    }
}

fn void emit_boundary_decision_summary() {
    if (!test_summary_enabled()) return;

    BoundaryDecisionStats stats = boundary_decision_stats_snapshot();
    usz splice_fail_total = stats.splice_attempted >= stats.splice_succeeded
        ? stats.splice_attempted - stats.splice_succeeded
        : 0;

    io::printfn(
        "OMNI_TEST_SUMMARY suite=boundary_decisions splice_attempted=%d splice_succeeded=%d splice_fail_total=%d "
        "splice_fail_null_parent=%d splice_fail_null_child=%d splice_fail_same_scope=%d splice_fail_not_immediate_child=%d splice_fail_refcount=%d splice_fail_owner_thread=%d splice_fail_parent_lane=%d splice_fail_child_temp_lane=%d splice_fail_child_escape_lane=%d "
        "promotion_attempted=%d promotion_aborted_budget=%d promotion_aborted_pre_aborted=%d "
        "copy_fallback_total=%d planned_stable_publish=%d planned_stable_materialize=%d planned_transplant=%d "
        "selected_stable_publish=%d selected_stable_materialize=%d selected_transplant=%d selected_fail_closed=%d "
        "route_fail_transplant_rejected=%d materialization_node_count=%d materialization_copy_bytes=%d "
        "selected_stable_materialize_cons=%d selected_stable_materialize_array=%d selected_stable_materialize_hashmap=%d selected_stable_materialize_set=%d selected_stable_materialize_closure=%d selected_stable_materialize_big_integer=%d "
        "materialization_copy_bytes_cons=%d materialization_copy_bytes_array=%d materialization_copy_bytes_hashmap=%d materialization_copy_bytes_set=%d materialization_copy_bytes_closure=%d materialization_copy_bytes_big_integer=%d",
        (long)stats.splice_attempted,
        (long)stats.splice_succeeded,
        (long)splice_fail_total,
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_NULL_PARENT],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_NULL_CHILD],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID],
        (long)stats.splice_failed_by_reason[(usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID],
        (long)stats.promotion_attempted,
        (long)stats.promotion_aborted_by_reason[(usz)BoundaryPromotionAbortReason.BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED],
        (long)stats.promotion_aborted_by_reason[(usz)BoundaryPromotionAbortReason.BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX],
        (long)stats.copy_fallback_total,
        (long)stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE],
        (long)stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
        (long)stats.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
        (long)stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE],
        (long)stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
        (long)stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
        (long)stats.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_FAIL_CLOSED],
        (long)stats.route_failure_by_reason[(usz)BoundaryPlanReason.BOUNDARY_PLAN_REASON_TRANSPLANT_REJECTED],
        (long)stats.stable_materialization_node_count_total,
        (long)stats.stable_materialization_copy_bytes_total,
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CONS),
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, ARRAY),
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, HASHMAP),
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, SET),
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, CLOSURE),
        (long)boundary_decision_selected_route_tag_count(stats, BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, BIG_INTEGER),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, CONS),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, ARRAY),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, HASHMAP),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, SET),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, CLOSURE),
        (long)boundary_decision_materialization_copy_bytes_for_tag(stats, BIG_INTEGER)
    );

    // Optional verbose dump used by boundary hardening diagnostics.
    boundary_dump_decision_stats_verbose("tests");
}

fn bool traversal_summary_enabled() @inline {
    return test_summary_enabled() && test_c_getenv("OMNI_BOUNDARY_TRAVERSAL_SUMMARY") != null;
}

fn void emit_boundary_traversal_summary() {
    if (!traversal_summary_enabled()) return;

    CopyToParentStats copy_stats = copy_to_parent_stats_snapshot();
    BoundaryEscapeShapeStats shape_stats = boundary_escape_shape_stats_snapshot();
    BoundaryDecisionStats boundary_stats = boundary_decision_stats_snapshot();
    usz cons_avg_len = copy_stats.cons_spine_copy_samples > 0
        ? copy_stats.cons_spine_copy_total_len / copy_stats.cons_spine_copy_samples
        : 0;

    io::printfn(
        "OMNI_TEST_SUMMARY suite=boundary_traversal "
        "copy_total=%d copy_fast_reuse=%d copy_defensive=%d "
        "copy_tag_cons=%d copy_tag_closure=%d copy_tag_other=%d "
        "copy_site_run_jit=%d copy_site_jit_single=%d copy_site_tco=%d "
        "promoted_then_spliced=%d promoted_then_fallback_copied=%d "
        "cons_spine_samples=%d cons_spine_avg_len=%d cons_spine_peak_len=%d",
        (long)copy_stats.total_calls,
        (long)copy_stats.fast_reuse_hits,
        (long)copy_stats.defensive_copies,
        (long)copy_stats.cons_calls,
        (long)copy_stats.closure_calls,
        (long)copy_stats.other_calls,
        (long)copy_stats.site_calls[COPY_SITE_RUN_JIT_RESULT],
        (long)copy_stats.site_calls[COPY_SITE_JIT_SINGLE_SCOPE_RETURN],
        (long)copy_stats.site_calls[COPY_SITE_JIT_TCO_FRAME_COPY],
        (long)boundary_stats.splice_succeeded,
        (long)boundary_stats.copy_fallback_total,
        (long)copy_stats.cons_spine_copy_samples,
        (long)cons_avg_len,
        (long)copy_stats.cons_spine_copy_peak_len
    );
    io::printfn(
        "OMNI_TEST_SUMMARY suite=boundary_escape_shape candidate_roots=%d graph_roots=%d unsafe_roots=%d cons_roots=%d array_roots=%d hashmap_roots=%d set_roots=%d closure_roots=%d partial_roots=%d iterator_roots=%d tensor_roots=%d cons_spine_samples=%d cons_spine_peak_len=%d",
        (long)shape_stats.candidate_roots,
        (long)shape_stats.graph_roots,
        (long)shape_stats.unsafe_roots,
        (long)shape_stats.cons_roots,
        (long)shape_stats.array_roots,
        (long)shape_stats.hashmap_roots,
        (long)shape_stats.set_roots,
        (long)shape_stats.closure_roots,
        (long)shape_stats.partial_roots,
        (long)shape_stats.iterator_roots,
        (long)shape_stats.tensor_roots,
        (long)shape_stats.cons_spine_samples,
        (long)shape_stats.cons_spine_peak_len
    );
}

fn usz jit_fallback_count(ExprTag tag) @inline {
    return jit_fallback_audit_tag_count(tag);
}

fn void emit_jit_fallback_summary() {
    if (!test_summary_enabled()) return;

    JitFallbackAuditStats stats = jit_fallback_audit_snapshot();
    usz core_total =
        jit_fallback_count(E_LIT) +
        jit_fallback_count(E_VAR) +
        jit_fallback_count(E_LAMBDA) +
        jit_fallback_count(E_APP) +
        jit_fallback_count(E_IF) +
        jit_fallback_count(E_LET) +
        jit_fallback_count(E_DEFINE) +
        jit_fallback_count(E_AND) +
        jit_fallback_count(E_OR) +
        jit_fallback_count(E_CALL) +
        jit_fallback_count(E_BLOCK) +
        jit_fallback_count(E_SET);
    usz effects_total =
        jit_fallback_count(E_QUOTE) +
        jit_fallback_count(E_RESET) +
        jit_fallback_count(E_SHIFT) +
        jit_fallback_count(E_PERFORM) +
        jit_fallback_count(E_HANDLE) +
        jit_fallback_count(E_RESOLVE) +
        jit_fallback_count(E_INDEX) +
        jit_fallback_count(E_PATH) +
        jit_fallback_count(E_MATCH) +
        jit_fallback_count(E_QUASIQUOTE) +
        jit_fallback_count(E_DEFMACRO) +
        jit_fallback_count(E_MODULE) +
        jit_fallback_count(E_IMPORT);
    usz type_ffi_total =
        jit_fallback_count(E_DEFTYPE) +
        jit_fallback_count(E_DEFABSTRACT) +
        jit_fallback_count(E_DEFUNION) +
        jit_fallback_count(E_DEFALIAS) +
        jit_fallback_count(E_DEFEFFECT) +
        jit_fallback_count(E_EXPORT_FROM) +
        jit_fallback_count(E_FFI_LIB) +
        jit_fallback_count(E_FFI_FN);

    io::printfn(
        "OMNI_TEST_SUMMARY suite=jit_fallback "
        "total=%d core_group=%d effects_group=%d type_ffi_group=%d "
        "unquote=%d unquote_splicing=%d unknown=%d last_tag=%d",
        (long)stats.total,
        (long)core_total,
        (long)effects_total,
        (long)type_ffi_total,
        (long)jit_fallback_count(E_UNQUOTE),
        (long)jit_fallback_count(E_UNQUOTE_SPLICING),
        (long)stats.by_tag[JIT_FALLBACK_TAG_UNKNOWN],
        (long)(isz)stats.last_tag
    );
}

fn void emit_pass(char[] name) @inline {
    if (test_quiet_enabled()) return;
    io::printfn("[PASS] %s", (ZString)name);
}

fn void emit_pass_literal(char[] msg) @inline {
    if (test_quiet_enabled()) return;
    io::printfn("%s", (ZString)msg);
}

fn void emit_test_section_banner(char[] name) @inline {
    if (test_quiet_enabled()) return;
    io::printfn("\n--- %s ---", (ZString)name);
}

alias IsolatedTestGroupFn = fn void(Interp* interp, int* pass, int* fail);

fn bool advanced_group_trace_enabled() @inline {
    return test_c_getenv("OMNI_ADVANCED_GROUP_TRACE") != null;
}

fn bool advanced_group_filter_name_exact(char* filter, char[] name) {
    usz len = 0;
    while (filter[len] != 0) len++;
    if (len != name.len) return false;
    for (usz i = 0; i < len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return true;
}

fn bool advanced_group_filter_is_descendant_of(char* filter, char[] name) {
    usz filter_len = 0;
    while (filter[filter_len] != 0) filter_len++;
    if (filter_len <= name.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return filter[name.len] == '-';
}

fn bool advanced_group_filter_matches_exact(char[] name) {
    char* filter = test_c_getenv("OMNI_ADVANCED_GROUP_FILTER");
    if (filter == null) return true;
    return advanced_group_filter_name_exact(filter, name);
}

fn bool advanced_group_filter_matches_or_descends(char[] name) {
    char* filter = test_c_getenv("OMNI_ADVANCED_GROUP_FILTER");
    if (filter == null) return true;
    return advanced_group_filter_name_exact(filter, name) ||
        advanced_group_filter_is_descendant_of(filter, name);
}

fn void emit_advanced_group_trace(char[] phase, char[] name) {
    if (!advanced_group_trace_enabled()) return;
    io::printfn("OMNI_ADVANCED_GROUP_TRACE %s %s", (ZString)phase, (ZString)name);
}

fn void run_test_global_boundary_reset() {
    // Keep test groups independent from prior JIT/effect/promotion global state.
    if (g_jit_state_count > 0) {
        g_jit_gc_needed = true;
        jit_gc();
    } else {
        g_jit_gc_needed = false;
    }
    jit_cache_clear();
    g_jit_pool_warned = false;
    g_tensor_force_expr_edge_copy_fail_nth = 0;
    g_tensor_force_expr_edge_escape_fail_nth = 0;
    pika::regex_cache_reset();
}

fn void run_group_isolated(IsolatedTestGroupFn group_fn, int* pass, int* fail) {
    run_test_global_boundary_reset();

    Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
    if (tmp == null) {
        io::printn("[FAIL] test group bootstrap allocation");
        (*fail)++;
        return;
    }
    tmp.init();
    if (!bootstrap_runtime_interp(tmp)) {
        io::printn("[FAIL] test group bootstrap runtime surface");
        (*fail)++;
        tmp.destroy();
        mem::free(tmp);
        return;
    }
    group_fn(tmp, pass, fail);
    tmp.destroy();
    mem::free(tmp);

    run_test_global_boundary_reset();
}

fn bool run_named_group_isolated(char[] name, IsolatedTestGroupFn group_fn, int* pass, int* fail) {
    if (!advanced_group_filter_matches_or_descends(name)) return false;
    emit_advanced_group_trace("begin", name);
    run_group_isolated(group_fn, pass, fail);
    emit_advanced_group_trace("end", name);
    return true;
}

fn void run_advanced_tests_isolated(int* pass, int* fail) {
    // Keep advanced subgroup definitions from accumulating in one long-lived interpreter.
    bool matched_any = false;
    matched_any = run_named_group_isolated("advanced-core-semantics", &run_advanced_core_semantics_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-unicode-iterator", &run_advanced_unicode_iterator_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-logic-tco", &run_advanced_logic_tco_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-effect-continuation", &run_advanced_effect_continuation_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-runtime-control", &run_advanced_runtime_control_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-lambda-syntax", &run_advanced_lambda_syntax_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-binding-mutation", &run_advanced_binding_mutation_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-macro-hygiene", &run_advanced_macro_hygiene_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-collections-module", &run_advanced_collections_module_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-stdlib-numeric", &run_advanced_stdlib_numeric_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-type-dispatch-mutation-chain", &run_advanced_type_dispatch_mutation_chain_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-effect-union-limit", &run_advanced_effect_union_limit_tests, pass, fail) || matched_any;
    matched_any = run_named_group_isolated("advanced-ffi-system", &run_advanced_ffi_system_tests, pass, fail) || matched_any;
    char* filter = test_c_getenv("OMNI_ADVANCED_GROUP_FILTER");
    if (filter != null && !matched_any) {
        io::printfn("[FAIL] advanced group filter matched no tests: %s", (ZString)filter);
        (*fail)++;
    }
}

fn void run_advanced_type_dispatch_mutation_chain_tests(Interp* interp, int* pass, int* fail) {
    // These suites share type definitions (Point/Circle/Line/Box) and must run in one interpreter.
    run_advanced_type_parametric_tests(interp, pass, fail);
    run_advanced_dispatch_constructor_tests(interp, pass, fail);
    run_advanced_mutation_hierarchy_dispatch_tests(interp, pass, fail);
}

// ---------------------------------------------------------------------------
// Unified Test Helpers — each runs BOTH interpreter AND JIT on the same expr
// ---------------------------------------------------------------------------

fn void run_test_group_boundary_reset(Interp* interp) {
    run_test_global_boundary_reset();

    interp.flags.raise_pending = false;
    interp.raise_msg_len = 0;
    interp.raise_payload = null;
    interp.last_call_name = (SymbolId)0;
    interp.jit_env = null;
    interp.match_env = null;
    interp.flags.jit_tco_bounce = false;
    interp.jit_tco_expr = null;
    interp.jit_tco_env = null;
    interp.tco_recycle_scope = null;
    interp.tco_scope_defer_slot = 0;
    interp.tco_scope_defer_active = false;
    interp.releasing_scope = null;
    interp.escape_env_mode = false;
    interp.active_promotion_ctx = null;
    interp.resume_value = null;
    interp.yield_value = null;
    interp.yield_scope = null;
    interp.current_reset_state = null;
    if (interp.current_scope != interp.root_scope) {
        interp.current_scope = interp.root_scope;
    }
}

fn bool run_lisp_tests(int* out_pass, int* out_fail) {
    char* active_slice = resolved_lisp_slice();
    reject_deprecated_slice_aliases(active_slice);
    enforce_container_only_slice_execution(active_slice);
    enforce_container_only_high_memory(active_slice);
    assert(lisp_slice_known(active_slice), "unknown OMNI_LISP_TEST_SLICE");

    g_test_quiet_output = test_quiet_requested();

    if (test_summary_enabled()) {
        emit_lisp_slice_memory_flag(active_slice);
    }

    if (!test_quiet_enabled()) {
        if (jit_checks_enabled()) {
            io::printn("=== Unified Tests (Interpreter + JIT) ===");
        } else {
            io::printn("=== Unified Tests (Interpreter; JIT checks disabled) ===");
        }
    }

    Interp* interp = (Interp*)mem::malloc(Interp.sizeof);
    if (interp == null) {
        io::printn("[FAIL] unified test bootstrap allocation");
        if (out_pass != null) *out_pass = 0;
        if (out_fail != null) *out_fail = 1;
        return false;
    }
    interp.init();
    if (!bootstrap_runtime_interp(interp)) {
        io::printn("[FAIL] unified test bootstrap runtime surface");
        interp.destroy();
        mem::free(interp);
        if (out_pass != null) *out_pass = 0;
        if (out_fail != null) *out_fail = 1;
        return false;
    }

    int pass = 0;
    int fail = 0;
    bool ok = true;
    bool run_teardown_regression = test_c_getenv("OMNI_LISP_TEARDOWN_REGRESSION") != null;
    boundary_reset_decision_stats();
    reset_memory_promotion_stats();
    reset_boundary_escape_shape_stats();
    stable_escape_store_reset(interp);
    stable_escape_store_reset_publication_hook_count();
    stable_escape_store_reset_prepared_publication_hook_count();
    stable_escape_store_reset_publication_outcome_counts();
    jit_fallback_audit_reset();

    bool run_basic = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "basic");
    bool run_memory_lifetime_smoke =
        lisp_slice_any(active_slice) ||
        lisp_slice_matches(active_slice, "memory-lifetime") ||
        lisp_slice_matches(active_slice, "memory-lifetime-smoke");
    bool run_memory_lifetime_soak = lisp_slice_matches(active_slice, "memory-lifetime-soak");
    bool run_memory_lifetime_bench = lisp_slice_matches(active_slice, "memory-lifetime-bench");
    bool run_memory_lifetime_policy =
        lisp_slice_any(active_slice) ||
        lisp_slice_matches(active_slice, "memory-lifetime-policy");
    bool run_allocator_validation =
        lisp_slice_any(active_slice) ||
        lisp_slice_matches(active_slice, "allocator-validation");
    bool run_memory_stress = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "memory-stress");
    bool run_allocator_bench =
        lisp_slice_matches(active_slice, "allocator-bench");
    bool run_list_closure = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "list-closure");
    bool run_arithmetic_comparison = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "arithmetic-comparison");
    bool run_string_type = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "string-type");
    bool run_diagnostics = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "diagnostics");
    bool run_jit_policy = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "jit-policy");
    bool run_advanced = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "advanced");
    bool run_escape_scope = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "escape-scope");
    bool run_limit_busting = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "limit-busting");
    bool run_tco_recycling = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "tco-recycling");
    bool run_closure_lifecycle = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "closure-lifecycle");
    bool run_pika = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "pika");
    bool run_unicode = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "unicode");
    bool run_compression = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "compression");
    bool run_data_format = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "data-format");
    bool run_json = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "json");
    bool run_async = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "async");
    bool run_reader_dispatch = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "reader-dispatch");
    bool run_schema = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "schema");
    bool run_deduce = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "deduce");
    bool run_scheduler = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "scheduler");
    bool run_http = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "http");
    bool run_http_crud = lisp_slice_matches(active_slice, "http-crud");
    bool run_atomic = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "atomic");
    bool run_compiler = lisp_slice_any(active_slice) || lisp_slice_matches(active_slice, "compiler");
    bool run_any_unified =
        run_teardown_regression ||
        run_basic || run_memory_lifetime_smoke || run_memory_lifetime_soak || run_memory_lifetime_bench || run_allocator_bench || run_memory_stress ||
        run_memory_lifetime_policy || run_allocator_validation ||
        run_list_closure ||
        run_arithmetic_comparison || run_string_type || run_diagnostics || run_jit_policy ||
        run_advanced || run_escape_scope || run_limit_busting || run_tco_recycling ||
        run_closure_lifecycle || run_pika || run_unicode || run_compression ||
        run_data_format || run_json || run_async || run_reader_dispatch ||
        run_schema || run_deduce || run_scheduler || run_http || run_http_crud || run_atomic;

    if (run_teardown_regression) {
        emit_pass_literal("[PASS] teardown regression loads stdlib and reaches deterministic interpreter teardown");
        pass++;
    } else {
        if (run_basic) run_group_isolated(&run_basic_tests, &pass, &fail);
        if (run_memory_lifetime_soak) {
            run_group_isolated(&run_memory_lifetime_soak_tests, &pass, &fail);
        } else if (run_memory_lifetime_bench) {
            run_group_isolated(&run_memory_lifetime_benchmark_tests, &pass, &fail);
        } else if (run_memory_lifetime_smoke) {
            run_group_isolated(&run_memory_lifetime_smoke_tests, &pass, &fail);
        }
        if (run_memory_lifetime_policy) {
            run_group_isolated(&run_memory_lifetime_boundary_policy_parse_tests, &pass, &fail);
        }
        if (run_allocator_validation) {
            run_group_isolated(&run_memory_lifetime_ast_arena_validation_tests, &pass, &fail);
        }
        if (run_allocator_bench) {
            run_group_isolated(&run_allocator_benchmark_tests, &pass, &fail);
        }
        if (run_memory_stress) run_group_isolated(&run_memory_stress_tests, &pass, &fail);
        if (run_list_closure) run_group_isolated(&run_list_closure_tests, &pass, &fail);
        if (run_arithmetic_comparison) run_group_isolated(&run_arithmetic_comparison_tests, &pass, &fail);
        if (run_string_type) run_group_isolated(&run_string_type_tests, &pass, &fail);
        if (run_diagnostics) run_diagnostic_tests_isolated(&pass, &fail);
        if (run_jit_policy) run_jit_policy_tests_isolated(&pass, &fail);
        if (run_advanced) run_advanced_tests_isolated(&pass, &fail);
        if (run_escape_scope) run_group_isolated(&run_escape_scope_tests, &pass, &fail);
        if (run_limit_busting) run_group_isolated(&run_limit_busting_tests, &pass, &fail);
        if (run_tco_recycling) run_group_isolated(&run_tco_recycling_tests, &pass, &fail);
        if (run_closure_lifecycle) run_group_isolated(&run_closure_lifecycle_tests, &pass, &fail);
        if (run_pika) run_pika_tests_isolated(&pass, &fail);
        if (run_unicode) run_unicode_tests_isolated(&pass, &fail);
        if (run_compression) run_compression_tests_isolated(&pass, &fail);
        if (run_data_format) run_data_format_tests_isolated(&pass, &fail);
        if (run_json) run_json_tests_isolated(&pass, &fail);
        if (run_async) run_async_tests_isolated(&pass, &fail);
        if (run_reader_dispatch) run_reader_dispatch_tests_isolated(&pass, &fail);
        if (run_schema) run_schema_tests_isolated(&pass, &fail);
        if (run_deduce) run_deduce_tests(interp, &pass, &fail);
        if (run_scheduler) run_scheduler_tests_isolated(&pass, &fail);
        if (run_http) run_http_tests_isolated(&pass, &fail);
        if (run_http_crud) run_http_crud_concurrency_tests_isolated(&pass, &fail);
        if (run_atomic) run_atomic_tests_isolated(&pass, &fail);
    }

    if (run_any_unified) {
        if (!test_quiet_enabled()) {
            io::printfn("\n=== Unified Tests: %d passed, %d failed ===", pass, fail);
        }
        emit_test_summary("unified", pass, fail);
        emit_boundary_decision_summary();
        emit_boundary_traversal_summary();
        emit_jit_fallback_summary();
        if (fail != 0) ok = false;
    }

    if (run_compiler) {
        run_test_group_boundary_reset(interp);
        int compiler_pass = 0;
        int compiler_fail = 0;
        if (!run_compiler_tests(interp, &compiler_pass, &compiler_fail)) ok = false;
        if (!run_any_unified) {
            pass = compiler_pass;
            fail = compiler_fail;
        }
    }

    // Final JIT teardown keeps ASAN leak checks deterministic.
    jit_global_shutdown();
    interp.destroy();
    mem::free(interp);
    if (out_pass != null) *out_pass = pass;
    if (out_fail != null) *out_fail = fail;
    return ok;
}
