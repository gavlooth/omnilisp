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
    };
    if (result == null) return committed;
    committed.outcome = BOUNDARY_COMMIT_PASSTHROUGH;
    defer boundary_debug_graph_audit_committed_escape_root(committed, target_scope);

    BoundaryTxn txn = boundary_txn_begin(interp);
    defer boundary_txn_close(interp, &txn);
    BoundaryCommitClosureNormalizeStage normalize_stage = {};
    bool commit_succeeded = false;
    defer {
        if (commit_succeeded) {
            boundary_commit_normalized_closure_finalize(&normalize_stage);
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
        boundary_txn_abort(&txn);
        return committed;
    }
    result = normalized_result;

    BoundaryReturnClassification classification =
        boundary_classify_return_value(result, interp, target_scope, releasing_scope);

    switch (classification.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            boundary_commit_escape_commit_reused_in_target_chain(
                &txn,
                &committed,
                result,
                &commit_succeeded
            );
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
            return committed;
    }
}
