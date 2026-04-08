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
            committed.value = result;
            committed.outcome = BOUNDARY_COMMIT_REUSED_IN_TARGET_CHAIN;
            committed.reused_in_target_chain = true;
            boundary_txn_commit(&txn);
            commit_succeeded = true;
            return committed;
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP:
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE:
        case BOUNDARY_RETURN_MIXED_UNCERTAIN:
            break;
    }

    if (classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP ||
        classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE) {
        PromotionContext local_ctx = {};
        PromotionContext* active_ctx = ctx;
        bool owns_ctx = false;
        if (active_ctx == null) {
            boundary_promotion_context_begin(interp, &local_ctx);
            active_ctx = &local_ctx;
            owns_ctx = true;
        } else if (active_ctx.aborted) {
            boundary_promotion_context_begin(interp, &local_ctx, PROMOTION_CTX_BUDGET_RETURN);
            active_ctx = &local_ctx;
            owns_ctx = true;
        }
        defer {
            if (owns_ctx) boundary_promotion_context_end(interp, active_ctx);
        }

        bool splice_rejected_for_commit = false;
        BoundaryScopeTransferReason splice_reject_reason = BOUNDARY_SCOPE_TRANSFER_OK;
        BoundaryCommitFault fallback_fault = BOUNDARY_COMMIT_FAULT_NONE;
        bool requires_escape_promotion =
            classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;

        if (requires_escape_promotion) {
            BoundaryCommitDestinationPromotion destination_promotion =
                boundary_commit_try_destination_promotion(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx,
                    true
                );
            if (destination_promotion.committed) {
                committed.value = destination_promotion.value;
                committed.outcome = destination_promotion.outcome;
                boundary_txn_commit(&txn);
                commit_succeeded = true;
                return committed;
            }
            if (destination_promotion.fault_code != BOUNDARY_COMMIT_FAULT_NONE) {
                fallback_fault = destination_promotion.fault_code;
            }
        }

        if (releasing_scope.refcount == 1) {
            bool pre_aborted_ctx = active_ctx.aborted;
            if (requires_escape_promotion) {
                boundary_note_promotion_attempt();
                BoundaryScopeSwap promote_scope = boundary_scope_swap_begin(interp, releasing_scope);
                defer boundary_scope_swap_end(interp, &promote_scope);
                result = boundary_promote_to_escape(result, interp);
                classification = boundary_classify_return_value(result, interp, target_scope, releasing_scope);
            }

            if (active_ctx.aborted) {
                if (allow_scope_splice &&
                    classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE) {
                    if (boundary_commit_can_splice_escape_root(result, releasing_scope)) {
                        BoundaryScopeTransferResult splice_result =
                            boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                        if (splice_result.allowed) {
                            committed.value = result;
                            committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                            committed.releasing_scope_spliced = true;
                            boundary_txn_commit(&txn);
                            commit_succeeded = true;
                            return committed;
                        }
                        splice_rejected_for_commit = true;
                        splice_reject_reason = splice_result.reason;
                    } else {
                        splice_rejected_for_commit = true;
                        splice_reject_reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
                    }
                }
                BoundaryPromotionAbortReason abort_reason =
                    BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED;
                if (pre_aborted_ctx || active_ctx.abort_reason != PROMOTION_ABORT_BUDGET_EXHAUSTED) {
                    abort_reason = BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX;
                }
                boundary_note_promotion_abort(abort_reason);
                committed.fault_code = BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED;
                committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
                committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
                boundary_txn_abort(&txn);
                return committed;
            }

            if (allow_scope_splice &&
                classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                boundary_should_promote(result, active_ctx, releasing_scope)) {
                if (boundary_commit_can_splice_escape_root(result, releasing_scope)) {
                    BoundaryScopeTransferResult splice_result =
                        boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                    if (splice_result.allowed) {
                        committed.value = result;
                        committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                        committed.releasing_scope_spliced = true;
                        boundary_txn_commit(&txn);
                        commit_succeeded = true;
                        return committed;
                    }
                    splice_rejected_for_commit = true;
                    splice_reject_reason = splice_result.reason;
                } else {
                    splice_rejected_for_commit = true;
                    splice_reject_reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
                }
            }
        }

        if (releasing_scope.refcount != 1 &&
            boundary_nonunique_destination_retry_allowed(result)) {
            Value* destination_direct = boundary_promote_to_escape(result, interp);
            if (!active_ctx.aborted &&
                destination_direct != null &&
                destination_direct != result &&
                destination_direct.scope_gen == target_scope.escape_generation) {
                committed.value = destination_direct;
                committed.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
                boundary_txn_commit(&txn);
                commit_succeeded = true;
                return committed;
            }
        }

        if (classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE) {
            BoundaryCommitDestinationPromotion destination_promotion =
                boundary_commit_try_destination_promotion(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx,
                    false
                );
            if (destination_promotion.committed) {
                committed.value = destination_promotion.value;
                committed.outcome = destination_promotion.outcome;
                boundary_txn_commit(&txn);
                commit_succeeded = true;
                return committed;
            }
            if (destination_promotion.fault_code != BOUNDARY_COMMIT_FAULT_NONE) {
                fallback_fault = destination_promotion.fault_code;
            }
        }

        (void)splice_rejected_for_commit;
        (void)splice_reject_reason;
        if (fallback_fault == BOUNDARY_COMMIT_FAULT_NONE && splice_rejected_for_commit) {
            fallback_fault = BOUNDARY_COMMIT_FAULT_SPLICE_REJECTED;
        }
        if (fallback_fault == BOUNDARY_COMMIT_FAULT_NONE) {
            fallback_fault = BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED;
        }
        committed.fault_code = fallback_fault;
        committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
        committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
        boundary_txn_abort(&txn);
        return committed;
    }

    PromotionContext mixed_local_ctx = {};
    PromotionContext* mixed_ctx = ctx;
    bool owns_mixed_ctx = false;
    if (mixed_ctx == null) {
        boundary_promotion_context_begin(interp, &mixed_local_ctx);
        mixed_ctx = &mixed_local_ctx;
        owns_mixed_ctx = true;
    }
    defer {
        if (owns_mixed_ctx) boundary_promotion_context_end(interp, mixed_ctx);
    }

    Value* mixed_destination = boundary_promote_to_escape(result, interp);
    if (!mixed_ctx.aborted &&
        mixed_destination != null &&
        mixed_destination != result &&
        mixed_destination.scope_gen == target_scope.escape_generation) {
        committed.value = mixed_destination;
        committed.outcome = BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED;
        boundary_txn_commit(&txn);
        commit_succeeded = true;
        return committed;
    }

    committed.fault_code = BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION;
    committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
    committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
    boundary_txn_abort(&txn);
    return committed;
}
