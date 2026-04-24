module lisp;

import main;

fn void boundary_commit_escape_commit_reused_in_target_chain(
    BoundaryTxn* txn,
    BoundaryCommitEscapeResult* committed,
    Interp* interp,
    Value* result,
    bool* commit_succeeded
) @inline {
    committed.value = stable_escape_prepare_and_publish_value(interp, result);
    committed.outcome = BOUNDARY_COMMIT_REUSED_IN_TARGET_CHAIN;
    committed.reused_in_target_chain = true;
    committed.stable_publication_attempted = true;
    boundary_commit_set_selected_plan(
        committed,
        BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE,
        BOUNDARY_PLAN_REASON_REUSABLE_IN_TARGET_CHAIN
    );
    boundary_txn_commit(txn);
    if (commit_succeeded != null) *commit_succeeded = true;
}

fn BoundaryPlanReason boundary_commit_escape_failure_reason(BoundaryCommitFault fault_code) @inline {
    if (fault_code == BOUNDARY_COMMIT_FAULT_SPLICE_REJECTED) {
        return BOUNDARY_PLAN_REASON_TRANSPLANT_REJECTED;
    }
    if (fault_code == BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION) {
        return BOUNDARY_PLAN_REASON_MIXED_DESTINATION_FAILED;
    }
    if (fault_code == BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED) {
        return BOUNDARY_PLAN_REASON_FALLBACK_DISALLOWED;
    }
    if (fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED) {
        return BOUNDARY_PLAN_REASON_PROMOTION_ABORTED;
    }
    return BOUNDARY_PLAN_REASON_DESTINATION_FAILED;
}

fn void boundary_commit_escape_fail_closed(
    BoundaryCommitEscapeResult* committed,
    Interp* interp,
    BoundaryTxn* txn,
    BoundaryCommitFault fault_code
) @inline {
    committed.fault_code = fault_code;
    committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
    committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
    boundary_commit_set_selected_plan(
        committed,
        BOUNDARY_PLAN_ROUTE_FAIL_CLOSED,
        boundary_commit_escape_failure_reason(fault_code)
    );
    boundary_txn_abort(txn);
}

fn bool boundary_commit_cons_transplant_budget_available(
    Value* result,
    PromotionContext* active_ctx,
    Interp* interp,
    usz* out_prepared_node_count
) {
    if (result == null || result.tag != CONS || interp == null) return false;
    StableEscapePreparedGraphScratch scratch = {};
    scratch.global_env = interp.global_env;
    usz root_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    if (!stable_escape_prepare_graph_node(&scratch, result, &root_index)) return false;
    if (root_index == STABLE_ESCAPE_PREPARED_INVALID_INDEX || root_index >= scratch.node_count) return false;
    if (out_prepared_node_count != null) *out_prepared_node_count = scratch.node_count;
    if (active_ctx == null || active_ctx.budget == usz.max) return true;
    if (active_ctx.aborted) return false;
    return active_ctx.budget >= scratch.node_count;
}

fn bool boundary_commit_try_temp_cons_transplant(
    BoundaryCommitEscapeResult* committed,
    Value** result_io,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* active_ctx,
    BoundaryTxn* txn,
    bool* commit_succeeded,
    BoundaryReturnClassification* classification_io,
    BoundaryPlanDecision plan
) {
    if (committed == null || result_io == null || *result_io == null) return false;
    if (interp == null || target_scope == null || releasing_scope == null || txn == null) return false;
    Value* result = *result_io;
    if (plan.route != BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION ||
        !plan.transplant_candidate ||
        plan.provenance != BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP ||
        result.tag != CONS) {
        return false;
    }

    usz prepared_node_count = 0;
    if (!boundary_commit_cons_transplant_budget_available(result, active_ctx, interp, &prepared_node_count)) {
        return false;
    }
    (void)prepared_node_count;

    bool pre_aborted_ctx = active_ctx != null && active_ctx.aborted;
    boundary_note_promotion_attempt();
    BoundaryScopeSwap promote_scope = boundary_scope_swap_begin(interp, releasing_scope);
    Value* promoted = boundary_promote_to_escape_ctx(result, interp, active_ctx);
    boundary_scope_swap_end(interp, &promote_scope);
    if (classification_io != null) {
        *classification_io = boundary_classify_return_value(promoted, interp, target_scope, releasing_scope);
    }
    *result_io = promoted;

    if (active_ctx != null && active_ctx.aborted) {
        BoundaryPromotionAbortReason abort_reason = BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED;
        if (pre_aborted_ctx || active_ctx.abort_reason != PROMOTION_ABORT_BUDGET_EXHAUSTED) {
            abort_reason = BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX;
        }
        boundary_note_promotion_abort(abort_reason);
        boundary_commit_escape_fail_closed(
            committed,
            interp,
            txn,
            BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED
        );
        return true;
    }

    if (promoted == null || promoted == result || promoted.tag == ERROR) return false;
    if (classification_io == null ||
        classification_io.provenance != BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE ||
        !boundary_should_promote(promoted, active_ctx, releasing_scope)) {
        return false;
    }

    BoundaryTransplantProof splice_proof =
        boundary_commit_build_splice_escape_root_proof(promoted, target_scope, releasing_scope, interp.global_env);
    if (!splice_proof.transfer.allowed) return false;
    BoundaryScopeTransferResult splice_result =
        boundary_try_scope_splice_escapes_root_proof_result(target_scope, releasing_scope, promoted, &splice_proof);
    if (!splice_result.allowed) return false;

    committed.value = promoted;
    committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
    committed.releasing_scope_spliced = true;
    boundary_commit_set_selected_plan(
        committed,
        BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT,
        BOUNDARY_PLAN_REASON_TRANSPLANT_COMMITTED
    );
    boundary_txn_commit(txn);
    if (commit_succeeded != null) *commit_succeeded = true;
    return true;
}

fn void boundary_commit_escape_handle_releasing_scope(
    BoundaryCommitEscapeResult* committed,
    Value** result_io,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx,
    bool allow_scope_splice,
    BoundaryTxn* txn,
    bool* commit_succeeded,
    BoundaryReturnClassification* classification_io,
    BoundaryPlanDecision plan
) {
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

    Value* result = *result_io;
    bool requires_escape_promotion =
        plan.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;

    if (boundary_commit_try_temp_cons_transplant(
            committed,
            result_io,
            interp,
            target_scope,
            releasing_scope,
            active_ctx,
            txn,
            commit_succeeded,
            classification_io,
            plan
        )) {
        return;
    }
    result = *result_io;

    if (plan.route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION ||
        plan.route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION) {
        bool allow_direct_destination =
            requires_escape_promotion ||
            (plan.route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                releasing_scope.refcount != 1 &&
                boundary_nonunique_destination_retry_allowed(result));
        BoundaryCommitDestinationPromotion destination_promotion =
            boundary_commit_try_destination_promotion(
                result,
                interp,
                target_scope,
                releasing_scope,
                active_ctx,
                plan.route,
                allow_direct_destination
            );
        if (destination_promotion.committed) {
            committed.value = destination_promotion.value;
            committed.outcome = destination_promotion.outcome;
            committed.stable_materialization_used = destination_promotion.stable_materialization_used;
            committed.compatibility_path_used = !destination_promotion.stable_materialization_used;
            boundary_commit_set_selected_plan(
                committed,
                plan.route,
                plan.route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION
                    ? BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_STABLE
                    : BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY
            );
            boundary_txn_commit(txn);
            if (commit_succeeded != null) *commit_succeeded = true;
            return;
        }
        BoundaryCommitFault promotion_fault = destination_promotion.fault_code;
        if (promotion_fault == BOUNDARY_COMMIT_FAULT_NONE) {
            promotion_fault = BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED;
        }
        if (requires_escape_promotion && plan.transplant_candidate) {
            bool pre_aborted_ctx = active_ctx.aborted;
            boundary_note_promotion_attempt();
            BoundaryScopeSwap promote_scope = boundary_scope_swap_begin(interp, releasing_scope);
            result = boundary_promote_to_escape_ctx(result, interp, active_ctx);
            boundary_scope_swap_end(interp, &promote_scope);
            *classification_io = boundary_classify_return_value(result, interp, target_scope, releasing_scope);
            *result_io = result;
            if (active_ctx.aborted) {
                BoundaryPromotionAbortReason abort_reason = BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED;
                if (pre_aborted_ctx || active_ctx.abort_reason != PROMOTION_ABORT_BUDGET_EXHAUSTED) {
                    abort_reason = BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX;
                }
                boundary_note_promotion_abort(abort_reason);
                boundary_commit_escape_fail_closed(
                    committed,
                    interp,
                    txn,
                    BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED
                );
                return;
            }
            if (allow_scope_splice &&
                classification_io.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                boundary_should_promote(result, active_ctx, releasing_scope)) {
                BoundaryTransplantProof splice_proof =
                    boundary_commit_build_splice_escape_root_proof(result, target_scope, releasing_scope, interp.global_env);
                if (splice_proof.transfer.allowed) {
                    BoundaryScopeTransferResult splice_result =
                        boundary_try_scope_splice_escapes_root_proof_result(target_scope, releasing_scope, result, &splice_proof);
                    if (splice_result.allowed) {
                        committed.value = result;
                        committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                        committed.releasing_scope_spliced = true;
                        boundary_commit_set_selected_plan(
                            committed,
                            BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT,
                            BOUNDARY_PLAN_REASON_TRANSPLANT_COMMITTED
                        );
                        boundary_txn_commit(txn);
                        if (commit_succeeded != null) *commit_succeeded = true;
                        return;
                    }
                }
                promotion_fault = BOUNDARY_COMMIT_FAULT_SPLICE_REJECTED;
            }
        }
        if (plan.compatibility_candidate) {
            BoundaryCommitDestinationPromotion compatibility_promotion =
                boundary_commit_try_destination_promotion(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx,
                    BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION,
                    false
                );
            if (compatibility_promotion.committed) {
                committed.value = compatibility_promotion.value;
                committed.outcome = compatibility_promotion.outcome;
                committed.compatibility_path_used = true;
                boundary_commit_set_selected_plan(
                    committed,
                    BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION,
                    BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY
                );
                boundary_txn_commit(txn);
                if (commit_succeeded != null) *commit_succeeded = true;
                return;
            }
            if (compatibility_promotion.fault_code != BOUNDARY_COMMIT_FAULT_NONE) {
                promotion_fault = compatibility_promotion.fault_code;
            }
        }
        boundary_commit_escape_fail_closed(committed, interp, txn, promotion_fault);
        return;
    }

    if (plan.route == BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT &&
        allow_scope_splice &&
        releasing_scope.refcount == 1 &&
        classification_io.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
        boundary_should_promote(result, active_ctx, releasing_scope)) {
        BoundaryTransplantProof splice_proof =
            boundary_commit_build_splice_escape_root_proof(result, target_scope, releasing_scope, interp.global_env);
        if (splice_proof.transfer.allowed) {
            BoundaryScopeTransferResult splice_result =
                boundary_try_scope_splice_escapes_root_proof_result(target_scope, releasing_scope, result, &splice_proof);
            if (splice_result.allowed) {
                committed.value = result;
                committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                committed.releasing_scope_spliced = true;
                boundary_commit_set_selected_plan(
                    committed,
                    BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT,
                    BOUNDARY_PLAN_REASON_TRANSPLANT_COMMITTED
                );
                boundary_txn_commit(txn);
                if (commit_succeeded != null) *commit_succeeded = true;
                return;
            }
        }
        boundary_commit_escape_fail_closed(committed, interp, txn, BOUNDARY_COMMIT_FAULT_SPLICE_REJECTED);
        return;
    }

    boundary_commit_escape_fail_closed(committed, interp, txn, BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED);
}

fn void boundary_commit_escape_handle_mixed_destination(
    BoundaryCommitEscapeResult* committed,
    Value* result,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx,
    BoundaryTxn* txn,
    bool* commit_succeeded
) {
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

    Value* mixed_destination = boundary_promote_to_escape_ctx(result, interp, mixed_ctx);
    if (!mixed_ctx.aborted &&
        mixed_destination != null &&
        mixed_destination != result &&
        mixed_destination.scope_gen == target_scope.escape_generation) {
        committed.value = mixed_destination;
        committed.outcome = BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED;
        committed.compatibility_path_used = true;
        boundary_commit_set_selected_plan(
            committed,
            BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION,
            BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY
        );
        boundary_txn_commit(txn);
        if (commit_succeeded != null) *commit_succeeded = true;
        return;
    }

    committed.fault_code = BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION;
    committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
    committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
    boundary_commit_set_selected_plan(
        committed,
        BOUNDARY_PLAN_ROUTE_FAIL_CLOSED,
        BOUNDARY_PLAN_REASON_MIXED_DESTINATION_FAILED
    );
    boundary_txn_abort(txn);
}
