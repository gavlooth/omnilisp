module lisp;

import main;
import std::io;

fn void boundary_debug_graph_audit_report_scope_transfer(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    bool parent_non_null = parent != null;
    bool child_non_null = child != null;
    bool same_scope = parent_non_null && child_non_null && parent == child;
    bool immediate_child = parent_non_null && child_non_null && child.parent == parent;
    bool refcount_one = child_non_null && child.refcount == 1;
    bool owner_thread_match = parent_non_null &&
        child_non_null &&
        parent.owner_thread_token != null &&
        child.owner_thread_token != null &&
        parent.owner_thread_token == child.owner_thread_token;
    bool parent_escape_lane_valid = parent_non_null &&
        boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks);
    bool parent_escape_chunks_non_null = parent_non_null && parent.escape_chunks != null;
    bool parent_escape_bump_non_null = parent_non_null && parent.escape_bump != null;
    bool parent_escape_limit_non_null = parent_non_null && parent.escape_limit != null;
    bool child_temp_lane_valid = child_non_null &&
        boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks);
    bool child_escape_lane_valid = child_non_null &&
        boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks);

    io::eprintfn(
        "[boundary][graph-audit] splice-reject reason=%s parent_non_null=%d child_non_null=%d same_scope=%d immediate_child=%d refcount_one=%d owner_thread_match=%d parent_escape_lane_valid=%d parent_escape_chunks_non_null=%d parent_escape_bump_non_null=%d parent_escape_limit_non_null=%d child_temp_lane_valid=%d child_escape_lane_valid=%d",
        boundary_scope_transfer_reason_name(reason),
        parent_non_null ? 1 : 0,
        child_non_null ? 1 : 0,
        same_scope ? 1 : 0,
        immediate_child ? 1 : 0,
        refcount_one ? 1 : 0,
        owner_thread_match ? 1 : 0,
        parent_escape_lane_valid ? 1 : 0,
        parent_escape_chunks_non_null ? 1 : 0,
        parent_escape_bump_non_null ? 1 : 0,
        parent_escape_limit_non_null ? 1 : 0,
        child_temp_lane_valid ? 1 : 0,
        child_escape_lane_valid ? 1 : 0
    );
}

fn BoundaryScopeTransferResult boundary_scope_transfer_reject(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @inline {
    if (boundary_graph_audit_enabled()) {
        boundary_debug_graph_audit_report_scope_transfer(parent, child, reason);
    }
    return { .allowed = false, .reason = reason };
}

fn BoundaryTransplantProof boundary_build_transplant_proof(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    bool parent_non_null = parent != null;
    bool child_non_null = child != null;
    bool same_scope = parent_non_null && child_non_null && parent == child;
    bool immediate_child = parent_non_null && child_non_null && child.parent == parent;
    bool child_refcount_one = child_non_null && child.refcount == 1;
    bool owner_thread_match = parent_non_null &&
        child_non_null &&
        parent.owner_thread_token != null &&
        child.owner_thread_token != null &&
        parent.owner_thread_token == child.owner_thread_token;
    bool parent_escape_lane_valid = parent_non_null &&
        boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks);
    bool child_temp_lane_valid = child_non_null &&
        boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks);
    bool child_escape_lane_valid = child_non_null &&
        boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks);

    BoundaryScopeTransferReason reason = BOUNDARY_SCOPE_TRANSFER_OK;
    if (!parent_non_null) {
        reason = BOUNDARY_SCOPE_TRANSFER_NULL_PARENT;
    } else if (!child_non_null) {
        reason = BOUNDARY_SCOPE_TRANSFER_NULL_CHILD;
    } else if (same_scope) {
        reason = BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE;
    } else if (!immediate_child) {
        reason = BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
    } else if (!child_refcount_one) {
        reason = BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
    } else if (!owner_thread_match) {
        reason = BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH;
    } else if (!parent_escape_lane_valid) {
        reason = BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID;
    } else if (!child_temp_lane_valid) {
        reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
    } else if (!child_escape_lane_valid) {
        reason = BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID;
    }

    bool allowed = reason == BOUNDARY_SCOPE_TRANSFER_OK;
    if (!allowed && boundary_graph_audit_enabled()) {
        boundary_debug_graph_audit_report_scope_transfer(parent, child, reason);
    }

    return {
        .transfer = { .allowed = allowed, .reason = reason },
        .parent_non_null = parent_non_null,
        .child_non_null = child_non_null,
        .same_scope = same_scope,
        .immediate_child = immediate_child,
        .child_refcount_one = child_refcount_one,
        .owner_thread_match = owner_thread_match,
        .parent_escape_lane_valid = parent_escape_lane_valid,
        .child_temp_lane_valid = child_temp_lane_valid,
        .child_escape_lane_valid = child_escape_lane_valid,
        .root_graph_required = false,
        .root_non_null = false,
        .root_owned_by_child_escape = true,
        .source_escape_graph_safe = true,
        .reachable_temp_edges_safe = true,
        .closure_env_edges_safe = true,
        .ffi_native_traversal_required = false,
        .ffi_native_edges_safe = true,
        .mutation_epoch_required = false,
        .mutation_epoch_stable = true,
        .stamp_rewrite_required = false,
        .stamp_rewrite_safe = true
    };
}

fn BoundaryScopeTransferResult boundary_check_scope_transfer(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    return boundary_build_transplant_proof(parent, child).transfer;
}

fn void boundary_invariant_fail(char[] domain, char[] msg) @noinline {
    io::eprintfn(
        "[boundary][invariant][%s] %s",
        (ZString)domain,
        (ZString)msg
    );
    unreachable("boundary invariant failure");
}

fn bool boundary_invariant_checks_enabled() @inline {
    if (boundary_env_flag_enabled("OMNI_DISABLE_BOUNDARY_INVARIANTS")) return false;
    return true;
}

macro boundary_invariant_require(condition, domain, msg) {
    if (boundary_invariant_checks_enabled() && !(condition)) {
        boundary_invariant_fail(domain, msg);
    }
}

fn void boundary_assert_interp_scope_chain(Interp* interp) @inline {
    if (interp == null) return;
    boundary_invariant_require(
        interp.root_scope != null,
        "scope-chain",
        "root_scope must not be null"
    );
    boundary_invariant_require(
        interp.current_scope != null,
        "scope-chain",
        "current_scope must not be null"
    );
}

fn void boundary_assert_saved_state(BoundaryInterpState saved) @inline {
    boundary_invariant_require(
        saved.current_scope != null,
        "saved-state",
        "saved current_scope must not be null"
    );
}

fn void boundary_assert_session_state(BoundarySession* session) @inline {
    boundary_invariant_require(
        session != null,
        "session",
        "session pointer must not be null"
    );
    boundary_invariant_require(
        session.current_scope != null,
        "session",
        "saved current_scope must not be null"
    );
}

fn void boundary_assert_scope_swap_state(BoundaryScopeSwap* swap) @inline {
    boundary_invariant_require(
        swap != null,
        "scope-swap",
        "scope swap pointer must not be null"
    );
    boundary_invariant_require(
        swap.saved_scope != null,
        "scope-swap",
        "saved scope must not be null"
    );
}
