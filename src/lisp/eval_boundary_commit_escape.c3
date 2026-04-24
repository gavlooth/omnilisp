module lisp;

import main;

<*
@require interp != null : "boundary_commit_escape: null interp"
@require target_scope != null : "boundary_commit_escape: null target_scope"
@require releasing_scope != null : "boundary_commit_escape: null releasing_scope"
*>
fn BoundaryCommitEscapeResult boundary_commit_escape(
    Value* result,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null,
    bool allow_scope_splice = true
) {
    BoundaryCommitEscapeResult committed = {
        .value = result,
        .outcome = BOUNDARY_COMMIT_NULL_RESULT,
        .fault_code = BOUNDARY_COMMIT_FAULT_NONE,
        .releasing_scope_spliced = false,
        .reused_in_target_chain = false,
        .stable_publication_attempted = false,
        .stable_materialization_used = false,
        .compatibility_path_used = false,
        .planned_route = BOUNDARY_PLAN_ROUTE_NULL_RESULT,
        .planned_reason = BOUNDARY_PLAN_REASON_NULL_RESULT,
        .selected_route = BOUNDARY_PLAN_ROUTE_NULL_RESULT,
        .selected_reason = BOUNDARY_PLAN_REASON_NULL_RESULT,
    };
    if (result == null) {
        boundary_note_commit_decision_enabled(committed);
        return committed;
    }
    committed.outcome = BOUNDARY_COMMIT_PASSTHROUGH;
    committed.planned_route = BOUNDARY_PLAN_ROUTE_PASSTHROUGH;
    committed.planned_reason = BOUNDARY_PLAN_REASON_PASSTHROUGH;
    committed.selected_route = BOUNDARY_PLAN_ROUTE_PASSTHROUGH;
    committed.selected_reason = BOUNDARY_PLAN_REASON_PASSTHROUGH;
    defer boundary_debug_graph_audit_committed_escape_root(committed, target_scope, releasing_scope, interp.global_env);

    BoundaryTxn txn = boundary_txn_begin(interp);
    defer boundary_txn_close(interp, &txn);
    BoundaryCommitClosureNormalizeStage normalize_stage = {};
    bool commit_succeeded = false;
    defer {
        if (commit_succeeded) {
            if (normalize_stage.active && committed.value != normalize_stage.closure) {
                boundary_commit_normalized_closure_rollback(&normalize_stage);
            } else {
                boundary_commit_normalized_closure_finalize(&normalize_stage);
            }
        } else {
            boundary_commit_normalized_closure_rollback(&normalize_stage);
        }
    }
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    Value* normalized_result = boundary_commit_normalize_closure_env(result, interp, ctx, &normalize_stage);
    if (result != null &&
        result.tag == CLOSURE &&
        normalized_result != null &&
        normalized_result.tag == ERROR) {
        committed.value = normalized_result;
        committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
        committed.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION;
        committed.planned_route = BOUNDARY_PLAN_ROUTE_FAIL_CLOSED;
        committed.planned_reason = BOUNDARY_PLAN_REASON_NORMALIZED_CLOSURE_ERROR;
        committed.selected_route = BOUNDARY_PLAN_ROUTE_FAIL_CLOSED;
        committed.selected_reason = BOUNDARY_PLAN_REASON_NORMALIZED_CLOSURE_ERROR;
        boundary_txn_abort(&txn);
        boundary_note_commit_decision_enabled(committed);
        return committed;
    }
    result = normalized_result;

    BoundaryReturnClassification classification =
        boundary_classify_return_value(result, interp, target_scope, releasing_scope);
    BoundaryPlanDecision plan = boundary_plan_commit_escape(
        result,
        classification,
        allow_scope_splice,
        releasing_scope
    );
    boundary_commit_apply_initial_plan(&committed, plan);

    switch (classification.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            boundary_commit_escape_commit_reused_in_target_chain(
                &txn,
                &committed,
                interp,
                result,
                &commit_succeeded
            );
            boundary_note_commit_decision_enabled(committed);
            return committed;
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP:
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE:
            boundary_commit_escape_handle_releasing_scope(
                &committed,
                &result,
                interp,
                target_scope,
                releasing_scope,
                ctx,
                allow_scope_splice,
                &txn,
                &commit_succeeded,
                &classification
            );
            boundary_note_commit_decision_enabled(committed);
            return committed;
        case BOUNDARY_RETURN_MIXED_UNCERTAIN:
            boundary_commit_escape_handle_mixed_destination(
                &committed,
                result,
                interp,
                target_scope,
                ctx,
                &txn,
                &commit_succeeded
            );
            boundary_note_commit_decision_enabled(committed);
            return committed;
    }
}
