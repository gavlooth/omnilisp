module lisp;

import main;

struct BoundaryCommitDestinationPromotion {
    Value* value;
    BoundaryCommitOutcome outcome;
    BoundaryCommitFault fault_code;
    bool committed;
}

fn BoundaryCommitDestinationPromotion boundary_commit_try_destination_promotion(
    Value* result,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* active_ctx,
    bool allow_direct_promotion
) {
    BoundaryCommitDestinationPromotion promotion = {
        .value = result,
        .outcome = BOUNDARY_COMMIT_PASSTHROUGH,
        .fault_code = BOUNDARY_COMMIT_FAULT_NONE,
        .committed = false,
    };
    if (result == null) return promotion;

    if (result.tag == ERROR) {
        Value* destination_error = boundary_build_destination_error_escape(
            result,
            interp,
            target_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_error != null &&
            destination_error != result &&
            destination_error.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_error;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_ERROR_BUILD;
        return promotion;
    }

    if (result.tag == CONS) {
        Value* destination_cons = boundary_build_destination_cons_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_cons != null &&
            destination_cons != result &&
            destination_cons.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_cons;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_CONS_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_CONS_BUILD;
        return promotion;
    }

    if (result.tag == PARTIAL_PRIM) {
        Value* destination_partial = boundary_build_destination_partial_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_partial != null &&
            destination_partial != result &&
            destination_partial.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_partial;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_PARTIAL_BUILD;
        return promotion;
    }

    if (result.tag == ITERATOR) {
        Value* destination_iterator = boundary_build_destination_iterator_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_iterator != null &&
            destination_iterator != result &&
            destination_iterator.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_iterator;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_ITERATOR_BUILD;
        return promotion;
    }

    if (allow_direct_promotion) {
        Value* destination_direct = boundary_promote_to_escape(result, interp);
        if (!active_ctx.aborted &&
            destination_direct != null &&
            destination_direct != result &&
            destination_direct.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_direct;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION;
    }
    return promotion;
}
