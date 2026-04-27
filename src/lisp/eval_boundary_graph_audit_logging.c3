module lisp;

import std::io;

fn void boundary_graph_audit_log_reason(BoundaryGraphAuditResult result) @inline {
    switch (result.reason) {
        case BOUNDARY_GRAPH_AUDIT_OK:
            return;
        case BOUNDARY_GRAPH_AUDIT_INVALID_TARGET:
            io::eprintfn("[boundary][graph-audit] invalid target scope for reachability audit");
            return;
        case BOUNDARY_GRAPH_AUDIT_ALLOCATION_FAILED:
            io::eprintfn(
                "[boundary][graph-audit] allocation failure during reachability audit target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_SEED_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] seed overflow while queuing root target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_VALUE_SET_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] value visit overflow while validating escape reachability target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_ENV_SET_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] env visit overflow while validating escape reachability target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] value push overflow while traversing graph target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW:
            io::eprintfn(
                "[boundary][graph-audit] env push overflow while traversing graph target_escape_gen=%ld",
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE:
            io::eprintfn(
                "[boundary][graph-audit] violation: reachable Omni edge enters TEMP root_tag=%ld violating_edge_tag=%ld root_scope_gen=%ld violating_scope_gen=%ld target_scope_gen=%ld target_escape_gen=%ld",
                (long)result.root_tag,
                (long)result.violating_edge_tag,
                (long)result.root_scope_gen,
                (long)result.violating_scope_gen,
                (long)result.target_scope_gen,
                (long)result.target_escape_gen
            );
            return;
        case BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV:
            io::eprintfn(
                "[boundary][graph-audit] violation: reachable Omni edge enters TEMP root_tag=%ld violating_edge_tag=%ld root_scope_gen=%ld violating_scope_gen=%ld target_scope_gen=%ld target_escape_gen=%ld",
                (long)result.root_tag,
                (long)result.violating_edge_tag,
                (long)result.root_scope_gen,
                (long)result.violating_scope_gen,
                (long)result.target_scope_gen,
                (long)result.target_escape_gen
            );
            return;
        default:
            io::eprintfn(
                "[boundary][graph-audit] unknown failure reason=%d target_escape_gen=%ld",
                (int)result.reason,
                (long)result.target_escape_gen
            );
            return;
    }
}

fn void boundary_graph_audit_log_commit_context(
    BoundaryCommitEscapeResult committed,
    BoundaryGraphAuditResult audit
) @inline {
    io::eprintfn(
        "[boundary][graph-audit] commit-context outcome=%s planned_route=%s selected_route=%s selected_reason=%s stable_publication=%s stable_materialization=%s compatibility_path=%s commit_value_tag=%ld commit_scope_gen=%ld target_scope_gen=%ld target_escape_gen=%ld reason=%s root_tag=%ld violating_edge_tag=%ld violating_scope_gen=%ld",
        (ZString)boundary_commit_outcome_name(committed.outcome),
        (ZString)boundary_plan_route_name(committed.planned_route),
        (ZString)boundary_plan_route_name(committed.selected_route),
        (ZString)boundary_plan_reason_name(committed.selected_reason),
        committed.stable_publication_attempted ? "yes" : "no",
        committed.stable_materialization_used ? "yes" : "no",
        committed.compatibility_path_used ? "yes" : "no",
        (long)committed.value.tag,
        (long)committed.value.scope_gen,
        (long)audit.target_scope_gen,
        (long)audit.target_escape_gen,
        (ZString)boundary_graph_audit_reason_name(audit.reason),
        (long)audit.root_tag,
        (long)audit.violating_edge_tag,
        (long)audit.violating_scope_gen
    );
}
