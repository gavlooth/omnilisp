module lisp;

import std::io;

struct BoundaryDecisionStats {
    usz splice_attempted;
    usz splice_succeeded;
    usz[BOUNDARY_SCOPE_TRANSFER_REASON_COUNT] splice_failed_by_reason;
    usz promotion_attempted;
    usz[BOUNDARY_PROMOTION_ABORT_REASON_COUNT] promotion_aborted_by_reason;
    usz copy_fallback_total;
    usz[BOUNDARY_PLAN_ROUTE_COUNT] planned_route_by_route;
    usz[BOUNDARY_PLAN_ROUTE_COUNT] selected_route_by_route;
    usz[BOUNDARY_PLAN_ROUTE_COUNT * BOUNDARY_VALUE_TAG_COUNT] planned_route_by_tag;
    usz[BOUNDARY_PLAN_ROUTE_COUNT * BOUNDARY_VALUE_TAG_COUNT] selected_route_by_tag;
    usz[BOUNDARY_PLAN_REASON_COUNT] route_failure_by_reason;
    usz stable_materialization_success;
    usz stable_materialization_node_count_total;
    usz stable_materialization_copy_bytes_total;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_success_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_node_count_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_copy_bytes_by_tag;
    usz scope_chain_scan_total;
    usz scope_chain_scan_with_hint;
    usz scope_chain_scan_fallback;
    usz scope_chain_scan_suppressed;
    usz graph_audit_invoked;
    usz graph_audit_skipped_rate;
    usz graph_audit_skipped_max_roots;
}

BoundaryDecisionStats g_boundary_decision_stats;

struct BoundaryValueShapeStats {
    usz array_construct_count;
    usz array_capacity_total;
    usz array_capacity_peak;
    usz array_growth_count;
    usz array_growth_old_capacity_total;
    usz array_growth_new_capacity_total;
    usz array_growth_new_capacity_peak;
    usz hashmap_construct_count;
    usz hashmap_capacity_total;
    usz hashmap_capacity_peak;
    usz hashmap_growth_count;
    usz hashmap_growth_old_capacity_total;
    usz hashmap_growth_new_capacity_total;
    usz hashmap_growth_new_capacity_peak;
    usz set_construct_count;
    usz set_capacity_total;
    usz set_capacity_peak;
    usz set_growth_count;
    usz set_growth_old_capacity_total;
    usz set_growth_new_capacity_total;
    usz set_growth_new_capacity_peak;
    usz closure_env_copy_frame_count;
    usz closure_env_copy_depth_total;
    usz closure_env_copy_depth_peak;
    usz closure_env_copy_binding_count_total;
    usz closure_env_copy_binding_count_peak;
    usz string_payload_count;
    usz string_payload_bytes_total;
    usz string_payload_bytes_peak;
    usz error_payload_count;
    usz error_payload_bytes_total;
    usz error_payload_bytes_peak;
    usz big_integer_wrapper_count;
    usz big_integer_decimal_input_count;
    usz big_integer_decimal_input_bytes_total;
    usz big_integer_decimal_input_bytes_peak;
    usz tensor_construct_count;
    usz tensor_payload_bytes_total;
    usz tensor_payload_bytes_peak;
    usz ffi_handle_wrapper_count;
    usz ffi_handle_borrowed_count;
    usz ffi_handle_owned_count;
    usz ffi_handle_manual_count;
    usz ffi_handle_releasable_count;
    usz ffi_handle_buffer_count;
    usz ffi_handle_library_count;
    usz stable_passport_valid_count;
    usz stable_passport_owner_scope_invalid_count;
    usz stable_passport_owner_escape_changed_count;
    usz stable_passport_root_not_in_owner_chain_count;
    usz stable_passport_prepared_graph_stale_count;
}

BoundaryValueShapeStats g_boundary_value_shape_stats;

// Instrumentation policy:
// - macros are used only for telemetry/trace scaffolding
// - ownership transitions and boundary control flow stay in functions
enum BoundaryInstrumentationMode : char {
    BOUNDARY_INSTRUMENTATION_OFF,
    BOUNDARY_INSTRUMENTATION_COUNTERS,
    BOUNDARY_INSTRUMENTATION_TRACE,
    BOUNDARY_INSTRUMENTATION_BENCHMARK,
}

// Compile-time instrumentation mode gate:
// - OFF: zero-overhead counters/trace path
// - COUNTERS: lightweight counters only
// - TRACE: counters + human-readable trace events (env-gated)
// - BENCHMARK: counters + compact benchmark event stream (env-gated)
// Feature flags:
// - OMNI_BOUNDARY_INSTR_COUNTERS
// - OMNI_BOUNDARY_INSTR_TRACE
// - OMNI_BOUNDARY_INSTR_BENCHMARK
// If none are set, instrumentation defaults to OFF.
fn BoundaryInstrumentationMode boundary_instrumentation_mode() @inline {
    $if $feature(OMNI_BOUNDARY_INSTR_TRACE):
        return BOUNDARY_INSTRUMENTATION_TRACE;
    $else
        $if $feature(OMNI_BOUNDARY_INSTR_BENCHMARK):
            return BOUNDARY_INSTRUMENTATION_BENCHMARK;
        $else
            $if $feature(OMNI_BOUNDARY_INSTR_COUNTERS):
                return BOUNDARY_INSTRUMENTATION_COUNTERS;
            $else
                return BOUNDARY_INSTRUMENTATION_OFF;
            $endif
        $endif
    $endif
}

fn bool boundary_telemetry_enabled() @inline {
    return boundary_instrumentation_mode() != BOUNDARY_INSTRUMENTATION_OFF;
}

fn bool boundary_trace_mode_enabled() @inline {
    return boundary_instrumentation_mode() == BOUNDARY_INSTRUMENTATION_TRACE;
}

fn bool boundary_benchmark_mode_enabled() @inline {
    return boundary_instrumentation_mode() == BOUNDARY_INSTRUMENTATION_BENCHMARK;
}

fn bool boundary_trace_runtime_enabled() @inline {
    return boundary_trace_mode_enabled() && boundary_policy_trace_runtime_enabled();
}

fn bool boundary_benchmark_runtime_enabled() @inline {
    return boundary_benchmark_mode_enabled() && boundary_policy_benchmark_trace_enabled();
}

fn void boundary_value_shape_note_array_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.array_construct_count++;
    g_boundary_value_shape_stats.array_capacity_total += capacity;
    if (capacity > g_boundary_value_shape_stats.array_capacity_peak) g_boundary_value_shape_stats.array_capacity_peak = capacity;
}

fn void boundary_value_shape_note_array_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.array_growth_count++;
    g_boundary_value_shape_stats.array_growth_old_capacity_total += old_capacity;
    g_boundary_value_shape_stats.array_growth_new_capacity_total += new_capacity;
    if (new_capacity > g_boundary_value_shape_stats.array_growth_new_capacity_peak) g_boundary_value_shape_stats.array_growth_new_capacity_peak = new_capacity;
}

fn void boundary_value_shape_note_hashmap_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.hashmap_construct_count++;
    g_boundary_value_shape_stats.hashmap_capacity_total += capacity;
    if (capacity > g_boundary_value_shape_stats.hashmap_capacity_peak) g_boundary_value_shape_stats.hashmap_capacity_peak = capacity;
}

fn void boundary_value_shape_note_hashmap_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.hashmap_growth_count++;
    g_boundary_value_shape_stats.hashmap_growth_old_capacity_total += old_capacity;
    g_boundary_value_shape_stats.hashmap_growth_new_capacity_total += new_capacity;
    if (new_capacity > g_boundary_value_shape_stats.hashmap_growth_new_capacity_peak) g_boundary_value_shape_stats.hashmap_growth_new_capacity_peak = new_capacity;
}

fn void boundary_value_shape_note_set_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.set_construct_count++;
    g_boundary_value_shape_stats.set_capacity_total += capacity;
    if (capacity > g_boundary_value_shape_stats.set_capacity_peak) g_boundary_value_shape_stats.set_capacity_peak = capacity;
}

fn void boundary_value_shape_note_set_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.set_growth_count++;
    g_boundary_value_shape_stats.set_growth_old_capacity_total += old_capacity;
    g_boundary_value_shape_stats.set_growth_new_capacity_total += new_capacity;
    if (new_capacity > g_boundary_value_shape_stats.set_growth_new_capacity_peak) g_boundary_value_shape_stats.set_growth_new_capacity_peak = new_capacity;
}

fn void boundary_value_shape_note_env_copy_frame(usz depth, usz binding_count) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.closure_env_copy_frame_count++;
    g_boundary_value_shape_stats.closure_env_copy_depth_total += depth;
    g_boundary_value_shape_stats.closure_env_copy_binding_count_total += binding_count;
    if (depth > g_boundary_value_shape_stats.closure_env_copy_depth_peak) g_boundary_value_shape_stats.closure_env_copy_depth_peak = depth;
    if (binding_count > g_boundary_value_shape_stats.closure_env_copy_binding_count_peak) g_boundary_value_shape_stats.closure_env_copy_binding_count_peak = binding_count;
}

fn void boundary_value_shape_note_string_payload(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.string_payload_count++;
    g_boundary_value_shape_stats.string_payload_bytes_total += len;
    if (len > g_boundary_value_shape_stats.string_payload_bytes_peak) g_boundary_value_shape_stats.string_payload_bytes_peak = len;
}

fn void boundary_value_shape_note_error_payload(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.error_payload_count++;
    g_boundary_value_shape_stats.error_payload_bytes_total += len;
    if (len > g_boundary_value_shape_stats.error_payload_bytes_peak) g_boundary_value_shape_stats.error_payload_bytes_peak = len;
}

fn void boundary_value_shape_note_big_integer_wrapper() @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.big_integer_wrapper_count++;
}

fn void boundary_value_shape_note_big_integer_decimal_input(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.big_integer_decimal_input_count++;
    g_boundary_value_shape_stats.big_integer_decimal_input_bytes_total += len;
    if (len > g_boundary_value_shape_stats.big_integer_decimal_input_bytes_peak) g_boundary_value_shape_stats.big_integer_decimal_input_bytes_peak = len;
}

fn void boundary_value_shape_note_tensor_construct(usz byte_len) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.tensor_construct_count++;
    g_boundary_value_shape_stats.tensor_payload_bytes_total += byte_len;
    if (byte_len > g_boundary_value_shape_stats.tensor_payload_bytes_peak) g_boundary_value_shape_stats.tensor_payload_bytes_peak = byte_len;
}

fn void boundary_value_shape_note_ffi_handle(FfiHandleOwnership ownership, ForeignHandleKind handle_kind, bool releasable) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_value_shape_stats.ffi_handle_wrapper_count++;
    switch (ownership) {
        case FFI_HANDLE_BORROWED:
            g_boundary_value_shape_stats.ffi_handle_borrowed_count++;
        case FFI_HANDLE_OWNED:
            g_boundary_value_shape_stats.ffi_handle_owned_count++;
        case FFI_HANDLE_MANUAL:
            g_boundary_value_shape_stats.ffi_handle_manual_count++;
    }
    if (releasable) g_boundary_value_shape_stats.ffi_handle_releasable_count++;
    switch (handle_kind) {
        case FOREIGN_HANDLE_BUFFER:
            g_boundary_value_shape_stats.ffi_handle_buffer_count++;
        case FOREIGN_HANDLE_LIBRARY:
            g_boundary_value_shape_stats.ffi_handle_library_count++;
        default:
    }
}

fn void boundary_value_shape_note_stable_passport_reason(StableGraphPassportInvalidationReason reason) @inline {
    if (!boundary_telemetry_enabled()) return;
    switch (reason) {
        case STABLE_GRAPH_PASSPORT_VALID:
            g_boundary_value_shape_stats.stable_passport_valid_count++;
        case STABLE_GRAPH_PASSPORT_OWNER_SCOPE_INVALID:
            g_boundary_value_shape_stats.stable_passport_owner_scope_invalid_count++;
        case STABLE_GRAPH_PASSPORT_OWNER_ESCAPE_CHANGED:
            g_boundary_value_shape_stats.stable_passport_owner_escape_changed_count++;
        case STABLE_GRAPH_PASSPORT_ROOT_NOT_IN_OWNER_CHAIN:
            g_boundary_value_shape_stats.stable_passport_root_not_in_owner_chain_count++;
        case STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE:
            g_boundary_value_shape_stats.stable_passport_prepared_graph_stale_count++;
    }
}

macro boundary_telemetry_inc_splice_attempted() {
    if (boundary_telemetry_enabled()) g_boundary_decision_stats.splice_attempted++;
}

macro boundary_telemetry_inc_splice_succeeded() {
    if (boundary_telemetry_enabled()) g_boundary_decision_stats.splice_succeeded++;
}

macro boundary_telemetry_inc_splice_failed(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT) {
            g_boundary_decision_stats.splice_failed_by_reason[reason_idx]++;
        }
    }
}

macro boundary_telemetry_inc_promotion_attempted() {
    if (boundary_telemetry_enabled()) g_boundary_decision_stats.promotion_attempted++;
}

macro boundary_telemetry_inc_promotion_aborted(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_PROMOTION_ABORT_REASON_COUNT) {
            g_boundary_decision_stats.promotion_aborted_by_reason[reason_idx]++;
        }
    }
}

macro boundary_telemetry_inc_scope_chain_scan() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.scope_chain_scan_total++;
    }
}

macro boundary_telemetry_inc_scope_chain_scan_with_hint() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.scope_chain_scan_with_hint++;
    }
}

macro boundary_telemetry_inc_scope_chain_scan_fallback() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.scope_chain_scan_fallback++;
    }
}

macro boundary_telemetry_inc_scope_chain_scan_suppressed() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.scope_chain_scan_suppressed++;
    }
}

macro boundary_telemetry_inc_graph_audit_invoked() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.graph_audit_invoked++;
    }
}

macro boundary_telemetry_inc_graph_audit_skipped_rate() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.graph_audit_skipped_rate++;
    }
}

macro boundary_telemetry_inc_graph_audit_skipped_max_roots() {
    if (boundary_telemetry_enabled()) {
        g_boundary_decision_stats.graph_audit_skipped_max_roots++;
    }
}

macro boundary_telemetry_inc_planned_route(route) {
    if (boundary_telemetry_enabled()) {
        usz route_idx = (usz)(route);
        if (route_idx < BOUNDARY_PLAN_ROUTE_COUNT) {
            g_boundary_decision_stats.planned_route_by_route[route_idx]++;
        }
    }
}

macro boundary_telemetry_inc_selected_route(route) {
    if (boundary_telemetry_enabled()) {
        usz route_idx = (usz)(route);
        if (route_idx < BOUNDARY_PLAN_ROUTE_COUNT) {
            g_boundary_decision_stats.selected_route_by_route[route_idx]++;
        }
    }
}

fn usz boundary_decision_route_tag_index(BoundaryPlanRoute route, ValueTag tag) @inline {
    return (usz)route * BOUNDARY_VALUE_TAG_COUNT + (usz)tag;
}

fn usz boundary_decision_planned_route_tag_count(
    BoundaryDecisionStats stats,
    BoundaryPlanRoute route,
    ValueTag tag
) @inline {
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.planned_route_by_tag[boundary_decision_route_tag_index(route, tag)];
}

fn usz boundary_decision_selected_route_tag_count(
    BoundaryDecisionStats stats,
    BoundaryPlanRoute route,
    ValueTag tag
) @inline {
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.selected_route_by_tag[boundary_decision_route_tag_index(route, tag)];
}

fn usz boundary_decision_materialization_success_tag_count(
    BoundaryDecisionStats stats,
    ValueTag tag
) @inline {
    usz tag_idx = (usz)tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.stable_materialization_success_by_tag[tag_idx];
}

fn usz boundary_decision_materialization_node_count_for_tag(
    BoundaryDecisionStats stats,
    ValueTag tag
) @inline {
    usz tag_idx = (usz)tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.stable_materialization_node_count_by_tag[tag_idx];
}

fn usz boundary_decision_materialization_copy_bytes_for_tag(
    BoundaryDecisionStats stats,
    ValueTag tag
) @inline {
    usz tag_idx = (usz)tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.stable_materialization_copy_bytes_by_tag[tag_idx];
}

fn void boundary_telemetry_inc_planned_route_tag(BoundaryPlanRoute route, ValueTag tag) @inline {
    if (!boundary_telemetry_enabled()) return;
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    g_boundary_decision_stats.planned_route_by_tag[boundary_decision_route_tag_index(route, tag)]++;
}

fn void boundary_telemetry_inc_selected_route_tag(BoundaryPlanRoute route, ValueTag tag) @inline {
    if (!boundary_telemetry_enabled()) return;
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    g_boundary_decision_stats.selected_route_by_tag[boundary_decision_route_tag_index(route, tag)]++;
}

macro boundary_telemetry_inc_route_failure(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_PLAN_REASON_COUNT) {
            g_boundary_decision_stats.route_failure_by_reason[reason_idx]++;
        }
    }
}

macro boundary_trace_splice_event(allowed, reason) {
    if (boundary_trace_runtime_enabled()) {
        io::eprintfn(
            "[boundary][trace] splice allowed=%d reason=%s",
            allowed ? 1 : 0,
            boundary_scope_transfer_reason_name(reason)
        );
    }
}

macro boundary_benchmark_splice_event(allowed, reason) {
    if (boundary_benchmark_runtime_enabled()) {
        io::eprintfn(
            "[boundary][bench] splice allowed=%d reason=%d",
            allowed ? 1 : 0,
            (int)(reason)
        );
    }
}

macro boundary_trace_promotion_abort(reason) {
    if (boundary_trace_runtime_enabled()) {
        io::eprintfn(
            "[boundary][trace] promotion abort reason=%s",
            boundary_promotion_abort_reason_name(reason)
        );
    }
}

macro boundary_benchmark_promotion_abort(reason) {
    if (boundary_benchmark_runtime_enabled()) {
        io::eprintfn(
            "[boundary][bench] promotion abort reason=%d",
            (int)(reason)
        );
    }
}

fn void boundary_reset_decision_stats() {
    g_boundary_decision_stats = {};
}

fn void boundary_restore_decision_stats(BoundaryDecisionStats snapshot) @inline {
    g_boundary_decision_stats = snapshot;
}

fn BoundaryDecisionStats boundary_decision_stats_snapshot() @inline {
    return g_boundary_decision_stats;
}

fn BoundaryValueShapeStats boundary_value_shape_stats_snapshot() @inline {
    return g_boundary_value_shape_stats;
}

fn void boundary_note_splice_result(BoundaryScopeTransferResult result) @inline {
    boundary_telemetry_inc_splice_attempted();
    boundary_trace_splice_event(result.allowed, result.reason);
    boundary_benchmark_splice_event(result.allowed, result.reason);
    if (result.allowed) {
        boundary_telemetry_inc_splice_succeeded();
        return;
    }
    boundary_telemetry_inc_splice_failed(result.reason);
}

fn void boundary_note_promotion_attempt() @inline {
    boundary_telemetry_inc_promotion_attempted();
}

fn void boundary_note_promotion_abort(BoundaryPromotionAbortReason reason) @inline {
    boundary_trace_promotion_abort(reason);
    boundary_benchmark_promotion_abort(reason);
    boundary_telemetry_inc_promotion_aborted(reason);
}

fn void boundary_note_materialization_copy_debt(usz node_count, usz estimated_copy_bytes) @inline {
    if (!boundary_telemetry_enabled()) return;
    g_boundary_decision_stats.stable_materialization_success++;
    g_boundary_decision_stats.stable_materialization_node_count_total += node_count;
    g_boundary_decision_stats.stable_materialization_copy_bytes_total += estimated_copy_bytes;
}

fn void boundary_note_materialization_copy_debt_by_tag(
    ValueTag root_tag,
    usz node_count,
    usz estimated_copy_bytes
) @inline {
    if (!boundary_telemetry_enabled()) return;
    boundary_note_materialization_copy_debt(node_count, estimated_copy_bytes);
    usz tag_idx = (usz)root_tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    g_boundary_decision_stats.stable_materialization_success_by_tag[tag_idx]++;
    g_boundary_decision_stats.stable_materialization_node_count_by_tag[tag_idx] += node_count;
    g_boundary_decision_stats.stable_materialization_copy_bytes_by_tag[tag_idx] += estimated_copy_bytes;
}

fn void boundary_note_commit_decision(BoundaryCommitEscapeResult committed) @inline {
    boundary_telemetry_inc_planned_route(committed.planned_route);
    boundary_telemetry_inc_selected_route(committed.selected_route);
    boundary_telemetry_inc_planned_route_tag(committed.planned_route, committed.planned_value_tag);
    boundary_telemetry_inc_selected_route_tag(committed.selected_route, committed.selected_value_tag);
    if (committed.selected_route == BOUNDARY_PLAN_ROUTE_FAIL_CLOSED) {
        boundary_telemetry_inc_route_failure(committed.selected_reason);
    }
    if (committed.selected_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION ||
        committed.selected_route == BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION) {
        if (boundary_telemetry_enabled()) g_boundary_decision_stats.copy_fallback_total++;
    }
}

macro boundary_note_commit_decision_enabled(committed) {
    $if $feature(OMNI_BOUNDARY_INSTR_TRACE):
        boundary_note_commit_decision(committed);
    $else
        $if $feature(OMNI_BOUNDARY_INSTR_BENCHMARK):
            boundary_note_commit_decision(committed);
        $else
            $if $feature(OMNI_BOUNDARY_INSTR_COUNTERS):
                boundary_note_commit_decision(committed);
            $endif
        $endif
    $endif
}

macro boundary_note_materialization_copy_debt_enabled(root_tag, node_count, estimated_copy_bytes) {
    $if $feature(OMNI_BOUNDARY_INSTR_TRACE):
        boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes);
    $else
        $if $feature(OMNI_BOUNDARY_INSTR_BENCHMARK):
            boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes);
        $else
            $if $feature(OMNI_BOUNDARY_INSTR_COUNTERS):
                boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes);
            $endif
        $endif
    $endif
}
