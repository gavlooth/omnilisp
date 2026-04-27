module lisp;

import std::atomic;
import std::core::mem;
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
    usz forced_no_splice_materialization_success;
    usz forced_no_splice_materialization_node_count_total;
    usz forced_no_splice_materialization_copy_bytes_total;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_success_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_node_count_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] stable_materialization_copy_bytes_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] forced_no_splice_materialization_success_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] forced_no_splice_materialization_node_count_by_tag;
    usz[BOUNDARY_VALUE_TAG_COUNT] forced_no_splice_materialization_copy_bytes_by_tag;
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

fn usz boundary_telemetry_load(usz* counter) @inline {
    return @atomic_load(*counter, RELAXED);
}

fn void boundary_telemetry_store(usz* counter, usz value) @inline {
    @atomic_store(*counter, value, RELAXED);
}

fn usz boundary_telemetry_saturating_add(usz* counter, usz amount) @inline {
    if (amount == 0) return boundary_telemetry_load(counter);
    while (true) {
        usz old_value = boundary_telemetry_load(counter);
        if (old_value == usz.max) return usz.max;
        usz new_value = amount > usz.max - old_value ? usz.max : old_value + amount;
        usz previous = mem::compare_exchange(counter, old_value, new_value);
        if (previous == old_value) return new_value;
    }
}

fn usz boundary_telemetry_inc(usz* counter) @inline {
    return boundary_telemetry_saturating_add(counter, (usz)1);
}

fn void boundary_telemetry_max(usz* counter, usz value) @inline {
    (void)atomic::fetch_max(counter, value, RELAXED);
}

fn void boundary_value_shape_note_array_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.array_construct_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.array_capacity_total, capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.array_capacity_peak, capacity);
}

fn void boundary_value_shape_note_array_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.array_growth_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.array_growth_old_capacity_total, old_capacity);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.array_growth_new_capacity_total, new_capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.array_growth_new_capacity_peak, new_capacity);
}

fn void boundary_value_shape_note_hashmap_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.hashmap_construct_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.hashmap_capacity_total, capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.hashmap_capacity_peak, capacity);
}

fn void boundary_value_shape_note_hashmap_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.hashmap_growth_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.hashmap_growth_old_capacity_total, old_capacity);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_total, new_capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_peak, new_capacity);
}

fn void boundary_value_shape_note_set_construct(usz capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.set_construct_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.set_capacity_total, capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.set_capacity_peak, capacity);
}

fn void boundary_value_shape_note_set_growth(usz old_capacity, usz new_capacity) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.set_growth_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.set_growth_old_capacity_total, old_capacity);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.set_growth_new_capacity_total, new_capacity);
    boundary_telemetry_max(&g_boundary_value_shape_stats.set_growth_new_capacity_peak, new_capacity);
}

fn void boundary_value_shape_note_env_copy_frame(usz depth, usz binding_count) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.closure_env_copy_frame_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.closure_env_copy_depth_total, depth);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.closure_env_copy_binding_count_total, binding_count);
    boundary_telemetry_max(&g_boundary_value_shape_stats.closure_env_copy_depth_peak, depth);
    boundary_telemetry_max(&g_boundary_value_shape_stats.closure_env_copy_binding_count_peak, binding_count);
}

fn void boundary_value_shape_note_string_payload(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.string_payload_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.string_payload_bytes_total, len);
    boundary_telemetry_max(&g_boundary_value_shape_stats.string_payload_bytes_peak, len);
}

fn void boundary_value_shape_note_error_payload(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.error_payload_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.error_payload_bytes_total, len);
    boundary_telemetry_max(&g_boundary_value_shape_stats.error_payload_bytes_peak, len);
}

fn void boundary_value_shape_note_big_integer_wrapper() @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.big_integer_wrapper_count);
}

fn void boundary_value_shape_note_big_integer_decimal_input(usz len) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.big_integer_decimal_input_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_total, len);
    boundary_telemetry_max(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_peak, len);
}

fn void boundary_value_shape_note_tensor_construct(usz byte_len) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.tensor_construct_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_value_shape_stats.tensor_payload_bytes_total, byte_len);
    boundary_telemetry_max(&g_boundary_value_shape_stats.tensor_payload_bytes_peak, byte_len);
}

fn void boundary_value_shape_note_ffi_handle(FfiHandleOwnership ownership, ForeignHandleKind handle_kind, bool releasable) @inline {
    if (!boundary_telemetry_enabled()) return;
    (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_wrapper_count);
    switch (ownership) {
        case FFI_HANDLE_BORROWED:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_borrowed_count);
        case FFI_HANDLE_OWNED:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_owned_count);
        case FFI_HANDLE_MANUAL:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_manual_count);
    }
    if (releasable) (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_releasable_count);
    switch (handle_kind) {
        case FOREIGN_HANDLE_BUFFER:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_buffer_count);
        case FOREIGN_HANDLE_LIBRARY:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.ffi_handle_library_count);
        default:
    }
}

fn void boundary_value_shape_note_stable_passport_reason(StableGraphPassportInvalidationReason reason) @inline {
    if (!boundary_telemetry_enabled()) return;
    switch (reason) {
        case STABLE_GRAPH_PASSPORT_VALID:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.stable_passport_valid_count);
        case STABLE_GRAPH_PASSPORT_OWNER_SCOPE_INVALID:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.stable_passport_owner_scope_invalid_count);
        case STABLE_GRAPH_PASSPORT_OWNER_ESCAPE_CHANGED:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.stable_passport_owner_escape_changed_count);
        case STABLE_GRAPH_PASSPORT_ROOT_NOT_IN_OWNER_CHAIN:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.stable_passport_root_not_in_owner_chain_count);
        case STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE:
            (void)boundary_telemetry_inc(&g_boundary_value_shape_stats.stable_passport_prepared_graph_stale_count);
    }
}

macro boundary_telemetry_inc_splice_attempted() {
    if (boundary_telemetry_enabled()) (void)boundary_telemetry_inc(&g_boundary_decision_stats.splice_attempted);
}

macro boundary_telemetry_inc_splice_succeeded() {
    if (boundary_telemetry_enabled()) (void)boundary_telemetry_inc(&g_boundary_decision_stats.splice_succeeded);
}

macro boundary_telemetry_inc_splice_failed(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT) {
            (void)boundary_telemetry_inc(&g_boundary_decision_stats.splice_failed_by_reason[reason_idx]);
        }
    }
}

macro boundary_telemetry_inc_promotion_attempted() {
    if (boundary_telemetry_enabled()) (void)boundary_telemetry_inc(&g_boundary_decision_stats.promotion_attempted);
}

macro boundary_telemetry_inc_promotion_aborted(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_PROMOTION_ABORT_REASON_COUNT) {
            (void)boundary_telemetry_inc(&g_boundary_decision_stats.promotion_aborted_by_reason[reason_idx]);
        }
    }
}

macro boundary_telemetry_inc_scope_chain_scan() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.scope_chain_scan_total);
    }
}

macro boundary_telemetry_inc_scope_chain_scan_with_hint() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.scope_chain_scan_with_hint);
    }
}

macro boundary_telemetry_inc_scope_chain_scan_fallback() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.scope_chain_scan_fallback);
    }
}

macro boundary_telemetry_inc_scope_chain_scan_suppressed() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.scope_chain_scan_suppressed);
    }
}

macro boundary_telemetry_inc_graph_audit_invoked() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.graph_audit_invoked);
    }
}

fn usz boundary_telemetry_note_graph_audit_invoked() @inline {
    if (!boundary_telemetry_enabled()) return boundary_telemetry_load(&g_boundary_decision_stats.graph_audit_invoked);
    return boundary_telemetry_inc(&g_boundary_decision_stats.graph_audit_invoked);
}

macro boundary_telemetry_inc_graph_audit_skipped_rate() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.graph_audit_skipped_rate);
    }
}

macro boundary_telemetry_inc_graph_audit_skipped_max_roots() {
    if (boundary_telemetry_enabled()) {
        (void)boundary_telemetry_inc(&g_boundary_decision_stats.graph_audit_skipped_max_roots);
    }
}

macro boundary_telemetry_inc_planned_route(route) {
    if (boundary_telemetry_enabled()) {
        usz route_idx = (usz)(route);
        if (route_idx < BOUNDARY_PLAN_ROUTE_COUNT) {
            (void)boundary_telemetry_inc(&g_boundary_decision_stats.planned_route_by_route[route_idx]);
        }
    }
}

macro boundary_telemetry_inc_selected_route(route) {
    if (boundary_telemetry_enabled()) {
        usz route_idx = (usz)(route);
        if (route_idx < BOUNDARY_PLAN_ROUTE_COUNT) {
            (void)boundary_telemetry_inc(&g_boundary_decision_stats.selected_route_by_route[route_idx]);
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

fn usz boundary_decision_forced_no_splice_materialization_copy_bytes_for_tag(
    BoundaryDecisionStats stats,
    ValueTag tag
) @inline {
    usz tag_idx = (usz)tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return 0;
    return stats.forced_no_splice_materialization_copy_bytes_by_tag[tag_idx];
}

fn usz boundary_decision_optimizer_materialization_copy_bytes(BoundaryDecisionStats stats) @inline {
    if (stats.stable_materialization_copy_bytes_total <= stats.forced_no_splice_materialization_copy_bytes_total) {
        return 0;
    }
    return stats.stable_materialization_copy_bytes_total -
        stats.forced_no_splice_materialization_copy_bytes_total;
}

fn usz boundary_decision_optimizer_materialization_copy_bytes_for_tag(
    BoundaryDecisionStats stats,
    ValueTag tag
) @inline {
    usz total = boundary_decision_materialization_copy_bytes_for_tag(stats, tag);
    usz forced = boundary_decision_forced_no_splice_materialization_copy_bytes_for_tag(stats, tag);
    return total <= forced ? 0 : total - forced;
}

fn void boundary_telemetry_inc_planned_route_tag(BoundaryPlanRoute route, ValueTag tag) @inline {
    if (!boundary_telemetry_enabled()) return;
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.planned_route_by_tag[boundary_decision_route_tag_index(route, tag)]);
}

fn void boundary_telemetry_inc_selected_route_tag(BoundaryPlanRoute route, ValueTag tag) @inline {
    if (!boundary_telemetry_enabled()) return;
    usz route_idx = (usz)route;
    usz tag_idx = (usz)tag;
    if (route_idx >= BOUNDARY_PLAN_ROUTE_COUNT || tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.selected_route_by_tag[boundary_decision_route_tag_index(route, tag)]);
}

macro boundary_telemetry_inc_route_failure(reason) {
    if (boundary_telemetry_enabled()) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_PLAN_REASON_COUNT) {
            (void)boundary_telemetry_inc(&g_boundary_decision_stats.route_failure_by_reason[reason_idx]);
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
    BoundaryDecisionStats stats = {};
    boundary_restore_decision_stats(stats);
}

fn void boundary_restore_decision_stats(BoundaryDecisionStats snapshot) @inline {
    boundary_telemetry_store(&g_boundary_decision_stats.splice_attempted, snapshot.splice_attempted);
    boundary_telemetry_store(&g_boundary_decision_stats.splice_succeeded, snapshot.splice_succeeded);
    for (usz i = 0; i < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.splice_failed_by_reason[i], snapshot.splice_failed_by_reason[i]);
    }
    boundary_telemetry_store(&g_boundary_decision_stats.promotion_attempted, snapshot.promotion_attempted);
    for (usz i = 0; i < BOUNDARY_PROMOTION_ABORT_REASON_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.promotion_aborted_by_reason[i], snapshot.promotion_aborted_by_reason[i]);
    }
    boundary_telemetry_store(&g_boundary_decision_stats.copy_fallback_total, snapshot.copy_fallback_total);
    for (usz i = 0; i < BOUNDARY_PLAN_ROUTE_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.planned_route_by_route[i], snapshot.planned_route_by_route[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.selected_route_by_route[i], snapshot.selected_route_by_route[i]);
    }
    for (usz i = 0; i < BOUNDARY_PLAN_ROUTE_COUNT * BOUNDARY_VALUE_TAG_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.planned_route_by_tag[i], snapshot.planned_route_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.selected_route_by_tag[i], snapshot.selected_route_by_tag[i]);
    }
    for (usz i = 0; i < BOUNDARY_PLAN_REASON_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.route_failure_by_reason[i], snapshot.route_failure_by_reason[i]);
    }
    boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_success, snapshot.stable_materialization_success);
    boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_node_count_total, snapshot.stable_materialization_node_count_total);
    boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_copy_bytes_total, snapshot.stable_materialization_copy_bytes_total);
    boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_success, snapshot.forced_no_splice_materialization_success);
    boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_total, snapshot.forced_no_splice_materialization_node_count_total);
    boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_total, snapshot.forced_no_splice_materialization_copy_bytes_total);
    for (usz i = 0; i < BOUNDARY_VALUE_TAG_COUNT; i++) {
        boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_success_by_tag[i], snapshot.stable_materialization_success_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_node_count_by_tag[i], snapshot.stable_materialization_node_count_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.stable_materialization_copy_bytes_by_tag[i], snapshot.stable_materialization_copy_bytes_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_success_by_tag[i], snapshot.forced_no_splice_materialization_success_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_by_tag[i], snapshot.forced_no_splice_materialization_node_count_by_tag[i]);
        boundary_telemetry_store(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_by_tag[i], snapshot.forced_no_splice_materialization_copy_bytes_by_tag[i]);
    }
    boundary_telemetry_store(&g_boundary_decision_stats.scope_chain_scan_total, snapshot.scope_chain_scan_total);
    boundary_telemetry_store(&g_boundary_decision_stats.scope_chain_scan_with_hint, snapshot.scope_chain_scan_with_hint);
    boundary_telemetry_store(&g_boundary_decision_stats.scope_chain_scan_fallback, snapshot.scope_chain_scan_fallback);
    boundary_telemetry_store(&g_boundary_decision_stats.scope_chain_scan_suppressed, snapshot.scope_chain_scan_suppressed);
    boundary_telemetry_store(&g_boundary_decision_stats.graph_audit_invoked, snapshot.graph_audit_invoked);
    boundary_telemetry_store(&g_boundary_decision_stats.graph_audit_skipped_rate, snapshot.graph_audit_skipped_rate);
    boundary_telemetry_store(&g_boundary_decision_stats.graph_audit_skipped_max_roots, snapshot.graph_audit_skipped_max_roots);
}

fn BoundaryDecisionStats boundary_decision_stats_snapshot() @inline {
    BoundaryDecisionStats stats = {};
    stats.splice_attempted = boundary_telemetry_load(&g_boundary_decision_stats.splice_attempted);
    stats.splice_succeeded = boundary_telemetry_load(&g_boundary_decision_stats.splice_succeeded);
    for (usz i = 0; i < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT; i++) {
        stats.splice_failed_by_reason[i] = boundary_telemetry_load(&g_boundary_decision_stats.splice_failed_by_reason[i]);
    }
    stats.promotion_attempted = boundary_telemetry_load(&g_boundary_decision_stats.promotion_attempted);
    for (usz i = 0; i < BOUNDARY_PROMOTION_ABORT_REASON_COUNT; i++) {
        stats.promotion_aborted_by_reason[i] = boundary_telemetry_load(&g_boundary_decision_stats.promotion_aborted_by_reason[i]);
    }
    stats.copy_fallback_total = boundary_telemetry_load(&g_boundary_decision_stats.copy_fallback_total);
    for (usz i = 0; i < BOUNDARY_PLAN_ROUTE_COUNT; i++) {
        stats.planned_route_by_route[i] = boundary_telemetry_load(&g_boundary_decision_stats.planned_route_by_route[i]);
        stats.selected_route_by_route[i] = boundary_telemetry_load(&g_boundary_decision_stats.selected_route_by_route[i]);
    }
    for (usz i = 0; i < BOUNDARY_PLAN_ROUTE_COUNT * BOUNDARY_VALUE_TAG_COUNT; i++) {
        stats.planned_route_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.planned_route_by_tag[i]);
        stats.selected_route_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.selected_route_by_tag[i]);
    }
    for (usz i = 0; i < BOUNDARY_PLAN_REASON_COUNT; i++) {
        stats.route_failure_by_reason[i] = boundary_telemetry_load(&g_boundary_decision_stats.route_failure_by_reason[i]);
    }
    stats.stable_materialization_success = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_success);
    stats.stable_materialization_node_count_total = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_node_count_total);
    stats.stable_materialization_copy_bytes_total = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_copy_bytes_total);
    stats.forced_no_splice_materialization_success = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_success);
    stats.forced_no_splice_materialization_node_count_total = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_total);
    stats.forced_no_splice_materialization_copy_bytes_total = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_total);
    for (usz i = 0; i < BOUNDARY_VALUE_TAG_COUNT; i++) {
        stats.stable_materialization_success_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_success_by_tag[i]);
        stats.stable_materialization_node_count_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_node_count_by_tag[i]);
        stats.stable_materialization_copy_bytes_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.stable_materialization_copy_bytes_by_tag[i]);
        stats.forced_no_splice_materialization_success_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_success_by_tag[i]);
        stats.forced_no_splice_materialization_node_count_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_by_tag[i]);
        stats.forced_no_splice_materialization_copy_bytes_by_tag[i] = boundary_telemetry_load(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_by_tag[i]);
    }
    stats.scope_chain_scan_total = boundary_telemetry_load(&g_boundary_decision_stats.scope_chain_scan_total);
    stats.scope_chain_scan_with_hint = boundary_telemetry_load(&g_boundary_decision_stats.scope_chain_scan_with_hint);
    stats.scope_chain_scan_fallback = boundary_telemetry_load(&g_boundary_decision_stats.scope_chain_scan_fallback);
    stats.scope_chain_scan_suppressed = boundary_telemetry_load(&g_boundary_decision_stats.scope_chain_scan_suppressed);
    stats.graph_audit_invoked = boundary_telemetry_load(&g_boundary_decision_stats.graph_audit_invoked);
    stats.graph_audit_skipped_rate = boundary_telemetry_load(&g_boundary_decision_stats.graph_audit_skipped_rate);
    stats.graph_audit_skipped_max_roots = boundary_telemetry_load(&g_boundary_decision_stats.graph_audit_skipped_max_roots);
    return stats;
}

fn void boundary_restore_value_shape_stats(BoundaryValueShapeStats snapshot) @inline {
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_construct_count, snapshot.array_construct_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_capacity_total, snapshot.array_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_capacity_peak, snapshot.array_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_growth_count, snapshot.array_growth_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_growth_old_capacity_total, snapshot.array_growth_old_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_growth_new_capacity_total, snapshot.array_growth_new_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.array_growth_new_capacity_peak, snapshot.array_growth_new_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_construct_count, snapshot.hashmap_construct_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_capacity_total, snapshot.hashmap_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_capacity_peak, snapshot.hashmap_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_growth_count, snapshot.hashmap_growth_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_growth_old_capacity_total, snapshot.hashmap_growth_old_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_total, snapshot.hashmap_growth_new_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_peak, snapshot.hashmap_growth_new_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_construct_count, snapshot.set_construct_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_capacity_total, snapshot.set_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_capacity_peak, snapshot.set_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_growth_count, snapshot.set_growth_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_growth_old_capacity_total, snapshot.set_growth_old_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_growth_new_capacity_total, snapshot.set_growth_new_capacity_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.set_growth_new_capacity_peak, snapshot.set_growth_new_capacity_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.closure_env_copy_frame_count, snapshot.closure_env_copy_frame_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.closure_env_copy_depth_total, snapshot.closure_env_copy_depth_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.closure_env_copy_depth_peak, snapshot.closure_env_copy_depth_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.closure_env_copy_binding_count_total, snapshot.closure_env_copy_binding_count_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.closure_env_copy_binding_count_peak, snapshot.closure_env_copy_binding_count_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.string_payload_count, snapshot.string_payload_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.string_payload_bytes_total, snapshot.string_payload_bytes_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.string_payload_bytes_peak, snapshot.string_payload_bytes_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.error_payload_count, snapshot.error_payload_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.error_payload_bytes_total, snapshot.error_payload_bytes_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.error_payload_bytes_peak, snapshot.error_payload_bytes_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.big_integer_wrapper_count, snapshot.big_integer_wrapper_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.big_integer_decimal_input_count, snapshot.big_integer_decimal_input_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_total, snapshot.big_integer_decimal_input_bytes_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_peak, snapshot.big_integer_decimal_input_bytes_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.tensor_construct_count, snapshot.tensor_construct_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.tensor_payload_bytes_total, snapshot.tensor_payload_bytes_total);
    boundary_telemetry_store(&g_boundary_value_shape_stats.tensor_payload_bytes_peak, snapshot.tensor_payload_bytes_peak);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_wrapper_count, snapshot.ffi_handle_wrapper_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_borrowed_count, snapshot.ffi_handle_borrowed_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_owned_count, snapshot.ffi_handle_owned_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_manual_count, snapshot.ffi_handle_manual_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_releasable_count, snapshot.ffi_handle_releasable_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_buffer_count, snapshot.ffi_handle_buffer_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.ffi_handle_library_count, snapshot.ffi_handle_library_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.stable_passport_valid_count, snapshot.stable_passport_valid_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.stable_passport_owner_scope_invalid_count, snapshot.stable_passport_owner_scope_invalid_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.stable_passport_owner_escape_changed_count, snapshot.stable_passport_owner_escape_changed_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.stable_passport_root_not_in_owner_chain_count, snapshot.stable_passport_root_not_in_owner_chain_count);
    boundary_telemetry_store(&g_boundary_value_shape_stats.stable_passport_prepared_graph_stale_count, snapshot.stable_passport_prepared_graph_stale_count);
}

fn BoundaryValueShapeStats boundary_value_shape_stats_snapshot() @inline {
    BoundaryValueShapeStats stats = {};
    stats.array_construct_count = boundary_telemetry_load(&g_boundary_value_shape_stats.array_construct_count);
    stats.array_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.array_capacity_total);
    stats.array_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.array_capacity_peak);
    stats.array_growth_count = boundary_telemetry_load(&g_boundary_value_shape_stats.array_growth_count);
    stats.array_growth_old_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.array_growth_old_capacity_total);
    stats.array_growth_new_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.array_growth_new_capacity_total);
    stats.array_growth_new_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.array_growth_new_capacity_peak);
    stats.hashmap_construct_count = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_construct_count);
    stats.hashmap_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_capacity_total);
    stats.hashmap_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_capacity_peak);
    stats.hashmap_growth_count = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_growth_count);
    stats.hashmap_growth_old_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_growth_old_capacity_total);
    stats.hashmap_growth_new_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_total);
    stats.hashmap_growth_new_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.hashmap_growth_new_capacity_peak);
    stats.set_construct_count = boundary_telemetry_load(&g_boundary_value_shape_stats.set_construct_count);
    stats.set_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.set_capacity_total);
    stats.set_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.set_capacity_peak);
    stats.set_growth_count = boundary_telemetry_load(&g_boundary_value_shape_stats.set_growth_count);
    stats.set_growth_old_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.set_growth_old_capacity_total);
    stats.set_growth_new_capacity_total = boundary_telemetry_load(&g_boundary_value_shape_stats.set_growth_new_capacity_total);
    stats.set_growth_new_capacity_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.set_growth_new_capacity_peak);
    stats.closure_env_copy_frame_count = boundary_telemetry_load(&g_boundary_value_shape_stats.closure_env_copy_frame_count);
    stats.closure_env_copy_depth_total = boundary_telemetry_load(&g_boundary_value_shape_stats.closure_env_copy_depth_total);
    stats.closure_env_copy_depth_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.closure_env_copy_depth_peak);
    stats.closure_env_copy_binding_count_total = boundary_telemetry_load(&g_boundary_value_shape_stats.closure_env_copy_binding_count_total);
    stats.closure_env_copy_binding_count_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.closure_env_copy_binding_count_peak);
    stats.string_payload_count = boundary_telemetry_load(&g_boundary_value_shape_stats.string_payload_count);
    stats.string_payload_bytes_total = boundary_telemetry_load(&g_boundary_value_shape_stats.string_payload_bytes_total);
    stats.string_payload_bytes_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.string_payload_bytes_peak);
    stats.error_payload_count = boundary_telemetry_load(&g_boundary_value_shape_stats.error_payload_count);
    stats.error_payload_bytes_total = boundary_telemetry_load(&g_boundary_value_shape_stats.error_payload_bytes_total);
    stats.error_payload_bytes_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.error_payload_bytes_peak);
    stats.big_integer_wrapper_count = boundary_telemetry_load(&g_boundary_value_shape_stats.big_integer_wrapper_count);
    stats.big_integer_decimal_input_count = boundary_telemetry_load(&g_boundary_value_shape_stats.big_integer_decimal_input_count);
    stats.big_integer_decimal_input_bytes_total = boundary_telemetry_load(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_total);
    stats.big_integer_decimal_input_bytes_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.big_integer_decimal_input_bytes_peak);
    stats.tensor_construct_count = boundary_telemetry_load(&g_boundary_value_shape_stats.tensor_construct_count);
    stats.tensor_payload_bytes_total = boundary_telemetry_load(&g_boundary_value_shape_stats.tensor_payload_bytes_total);
    stats.tensor_payload_bytes_peak = boundary_telemetry_load(&g_boundary_value_shape_stats.tensor_payload_bytes_peak);
    stats.ffi_handle_wrapper_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_wrapper_count);
    stats.ffi_handle_borrowed_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_borrowed_count);
    stats.ffi_handle_owned_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_owned_count);
    stats.ffi_handle_manual_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_manual_count);
    stats.ffi_handle_releasable_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_releasable_count);
    stats.ffi_handle_buffer_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_buffer_count);
    stats.ffi_handle_library_count = boundary_telemetry_load(&g_boundary_value_shape_stats.ffi_handle_library_count);
    stats.stable_passport_valid_count = boundary_telemetry_load(&g_boundary_value_shape_stats.stable_passport_valid_count);
    stats.stable_passport_owner_scope_invalid_count = boundary_telemetry_load(&g_boundary_value_shape_stats.stable_passport_owner_scope_invalid_count);
    stats.stable_passport_owner_escape_changed_count = boundary_telemetry_load(&g_boundary_value_shape_stats.stable_passport_owner_escape_changed_count);
    stats.stable_passport_root_not_in_owner_chain_count = boundary_telemetry_load(&g_boundary_value_shape_stats.stable_passport_root_not_in_owner_chain_count);
    stats.stable_passport_prepared_graph_stale_count = boundary_telemetry_load(&g_boundary_value_shape_stats.stable_passport_prepared_graph_stale_count);
    return stats;
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
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.stable_materialization_success);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.stable_materialization_node_count_total, node_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.stable_materialization_copy_bytes_total, estimated_copy_bytes);
}

fn void boundary_note_materialization_copy_debt_by_tag(
    ValueTag root_tag,
    usz node_count,
    usz estimated_copy_bytes,
    bool forced_no_splice
) @inline {
    if (!boundary_telemetry_enabled()) return;
    boundary_note_materialization_copy_debt(node_count, estimated_copy_bytes);
    usz tag_idx = (usz)root_tag;
    if (tag_idx >= BOUNDARY_VALUE_TAG_COUNT) return;
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.stable_materialization_success_by_tag[tag_idx]);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.stable_materialization_node_count_by_tag[tag_idx], node_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.stable_materialization_copy_bytes_by_tag[tag_idx], estimated_copy_bytes);
    if (!forced_no_splice) return;
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.forced_no_splice_materialization_success);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_total, node_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_total, estimated_copy_bytes);
    (void)boundary_telemetry_inc(&g_boundary_decision_stats.forced_no_splice_materialization_success_by_tag[tag_idx]);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.forced_no_splice_materialization_node_count_by_tag[tag_idx], node_count);
    (void)boundary_telemetry_saturating_add(&g_boundary_decision_stats.forced_no_splice_materialization_copy_bytes_by_tag[tag_idx], estimated_copy_bytes);
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
        if (boundary_telemetry_enabled()) (void)boundary_telemetry_inc(&g_boundary_decision_stats.copy_fallback_total);
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

macro boundary_note_materialization_copy_debt_enabled(root_tag, node_count, estimated_copy_bytes, forced_no_splice) {
    $if $feature(OMNI_BOUNDARY_INSTR_TRACE):
        boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes, forced_no_splice);
    $else
        $if $feature(OMNI_BOUNDARY_INSTR_BENCHMARK):
            boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes, forced_no_splice);
        $else
            $if $feature(OMNI_BOUNDARY_INSTR_COUNTERS):
                boundary_note_materialization_copy_debt_by_tag(root_tag, node_count, estimated_copy_bytes, forced_no_splice);
            $endif
        $endif
    $endif
}
