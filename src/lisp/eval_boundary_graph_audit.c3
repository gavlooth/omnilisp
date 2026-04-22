module lisp;

import main;
import std::io;

fn bool boundary_graph_audit_escape_reachability(Value* root, main::ScopeRegion* target_scope) @noinline {
    return boundary_graph_audit_escape_reachability_result(root, target_scope).ok;
}

fn void boundary_debug_graph_audit_committed_escape_root(
    BoundaryCommitEscapeResult committed,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope = null,
    Env* global_env = null
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    if (committed.value == null || target_scope == null) return;
    if (committed.value.scope_gen != target_scope.escape_generation) return;

    boundary_telemetry_inc_graph_audit_invoked();

    usz rate = boundary_graph_audit_rate();
    usz max_roots = boundary_graph_audit_max_roots();
    usz audit_index = g_boundary_decision_stats.graph_audit_invoked;

    if (rate == 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return;
    }

    if (max_roots > 0 && audit_index > max_roots) {
        boundary_telemetry_inc_graph_audit_skipped_max_roots();
        return;
    }

    if (rate > 1 && (audit_index - 1) % rate != 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return;
    }

    main::ScopeRegion* forbidden_scope = committed.releasing_scope_spliced ? null : releasing_scope;
    BoundaryGraphAuditResult audit =
        boundary_graph_audit_escape_reachability_result(committed.value, target_scope, true, forbidden_scope, global_env);
    if (!audit.ok) {
        boundary_graph_audit_log_reason(audit);
        boundary_graph_audit_log_commit_context(committed, audit);
        if (boundary_verbose_telemetry_enabled()) {
            BoundaryGraphAuditReason reason = audit.reason;
            io::eprintfn(
                "[boundary][graph-audit] committed escape root reachability violation outcome=%d reason=%s root_tag=%d violating_edge_tag=%d",
                (int)committed.outcome,
                (ZString)boundary_graph_audit_reason_name(reason),
                (long)audit.root_tag,
                (long)audit.violating_edge_tag
            );
        }
    }
}

fn bool boundary_debug_graph_audit_pre_splice_escape_root(
    Value* root,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    Env* global_env = null
) @noinline {
    if (!boundary_graph_audit_enabled()) return true;
    if (root == null || target_scope == null || releasing_scope == null) return true;

    boundary_telemetry_inc_graph_audit_invoked();

    usz rate = boundary_graph_audit_rate();
    usz max_roots = boundary_graph_audit_max_roots();
    usz audit_index = g_boundary_decision_stats.graph_audit_invoked;

    if (rate == 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return true;
    }

    if (max_roots > 0 && audit_index > max_roots) {
        boundary_telemetry_inc_graph_audit_skipped_max_roots();
        return true;
    }

    if (rate > 1 && (audit_index - 1) % rate != 0) {
        boundary_telemetry_inc_graph_audit_skipped_rate();
        return true;
    }

    BoundaryGraphAuditResult audit =
        boundary_graph_audit_escape_reachability_result(root, target_scope, true, releasing_scope, global_env);
    if (audit.ok) return true;

    if (boundary_verbose_telemetry_enabled()) {
        io::eprintfn(
            "[boundary][graph-audit] pre-splice escape root reachability violation reason=%s root_tag=%d violating_edge_tag=%d",
            (ZString)boundary_graph_audit_reason_name(audit.reason),
            (long)audit.root_tag,
            (long)audit.violating_edge_tag
        );
    }
    return false;
}
