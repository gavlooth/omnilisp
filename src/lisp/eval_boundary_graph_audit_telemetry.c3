module lisp;

import std::io;

fn bool boundary_verbose_telemetry_enabled() @inline {
    return boundary_policy_verbose_telemetry_enabled();
}

fn void boundary_dump_decision_stats_verbose(char[] label = "runtime") @noinline {
    if (!boundary_verbose_telemetry_enabled()) return;
    boundary_policy_emit_snapshot_once(label);
    BoundaryDecisionStats stats = boundary_decision_stats_snapshot();

    io::eprintfn(
        "[boundary][telemetry][%s] splice_attempted=%d splice_succeeded=%d promotion_attempted=%d copy_fallback_total=%d materialization_success=%d materialization_nodes=%d materialization_copy_bytes=%d scope_chain_scan_total=%d scope_chain_scan_with_hint=%d scope_chain_scan_fallback=%d scope_chain_scan_suppressed=%d graph_audit_invoked=%d graph_audit_skipped_rate=%d graph_audit_skipped_max_roots=%d",
        (ZString)label,
        (long)stats.splice_attempted,
        (long)stats.splice_succeeded,
        (long)stats.promotion_attempted,
        (long)stats.copy_fallback_total,
        (long)stats.stable_materialization_success,
        (long)stats.stable_materialization_node_count_total,
        (long)stats.stable_materialization_copy_bytes_total,
        (long)stats.scope_chain_scan_total,
        (long)stats.scope_chain_scan_with_hint,
        (long)stats.scope_chain_scan_fallback,
        (long)stats.scope_chain_scan_suppressed,
        (long)stats.graph_audit_invoked,
        (long)stats.graph_audit_skipped_rate,
        (long)stats.graph_audit_skipped_max_roots
    );

    for (usz i = 0; i < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] splice_fail reason=%s count=%d",
            (ZString)label,
            boundary_scope_transfer_reason_name((BoundaryScopeTransferReason)i),
            (long)stats.splice_failed_by_reason[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_PROMOTION_ABORT_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] promotion_abort reason=%s count=%d",
            (ZString)label,
            boundary_promotion_abort_reason_name((BoundaryPromotionAbortReason)i),
            (long)stats.promotion_aborted_by_reason[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_PLAN_ROUTE_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] route route=%s planned=%d selected=%d",
            (ZString)label,
            boundary_plan_route_name((BoundaryPlanRoute)i),
            (long)stats.planned_route_by_route[i],
            (long)stats.selected_route_by_route[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_PLAN_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] route_failure reason=%s count=%d",
            (ZString)label,
            boundary_plan_reason_name((BoundaryPlanReason)i),
            (long)stats.route_failure_by_reason[i]
        );
    }
}
