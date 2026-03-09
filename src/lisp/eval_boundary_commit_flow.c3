module lisp;

import std::io;
import main;

fn Value* boundary_fallback_disallowed_error(Interp* interp, BoundaryCommitFault fault_code) @noinline {
    char[192] msg_buf;
    if (try msg = io::bprintf(
        &msg_buf,
        "boundary: traversal fallback disallowed for commit path (%s)",
        (ZString)boundary_commit_fault_name(fault_code)
    )) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "boundary: traversal fallback disallowed for commit path");
}

<*
@require list != null : "boundary_build_destination_cons_escape: null list"
@require interp != null : "boundary_build_destination_cons_escape: null interp"
@require target_scope != null : "boundary_build_destination_cons_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_cons_escape: null releasing_scope"
@ensure return == list || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_cons_escape: expected ESCAPE-lane cons head"
*>
fn Value* boundary_build_destination_cons_escape(
    Value* list,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (list.tag != CONS) return list;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* head = interp.alloc_value_escape();
    head.tag = CONS;
    Value* current_new = head;
    Value* current_old = list;

    while (true) {
        Value* routed_car = boundary_copy_to_parent_site_ctx(
            current_old.cons_val.car,
            interp,
            COPY_SITE_CONS_BARRIER_CAR,
            ctx
        );
        current_new.cons_val.car = boundary_promote_to_escape(routed_car, interp);
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.car, target_scope),
            "boundary destination-cons: ESCAPE car must not point into target TEMP lane"
        );

        Value* old_cdr = current_old.cons_val.cdr;
        bool extend_spine = old_cdr != null &&
            old_cdr.tag == CONS &&
            old_cdr.scope_gen == releasing_scope.generation &&
            boundary_ptr_in_scope((void*)old_cdr, releasing_scope);
        if (extend_spine) {
            Value* next_new = interp.alloc_value_escape();
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
            continue;
        }

        Value* routed_cdr = boundary_copy_to_parent_site_ctx(
            old_cdr,
            interp,
            COPY_SITE_CONS_BARRIER_CDR,
            ctx
        );
        current_new.cons_val.cdr = boundary_promote_to_escape(routed_cdr, interp);
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.cdr, target_scope),
            "boundary destination-cons: ESCAPE cdr must not point into target TEMP lane"
        );
        break;
    }

    return head;
}

<*
@require partial != null : "boundary_build_destination_partial_escape: null partial"
@require interp != null : "boundary_build_destination_partial_escape: null interp"
@require target_scope != null : "boundary_build_destination_partial_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_partial_escape: null releasing_scope"
@ensure return == partial || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_partial_escape: expected ESCAPE-lane partial wrapper"
*>
fn Value* boundary_build_destination_partial_escape(
    Value* partial,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (partial.tag != PARTIAL_PRIM) return partial;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_first = boundary_copy_to_parent_site_ctx(
        partial.partial_val.first_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_first = boundary_promote_to_escape(routed_first, interp);
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_first, target_scope),
        "boundary destination-partial: first arg must not point into target TEMP lane"
    );

    Value* routed_second = boundary_copy_to_parent_site_ctx(
        partial.partial_val.second_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_second = boundary_promote_to_escape(routed_second, interp);
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_second, target_scope),
        "boundary destination-partial: second arg must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = partial.partial_val.func;
    result.partial_val.remaining = partial.partial_val.remaining;
    result.partial_val.first_arg = escaped_first;
    result.partial_val.second_arg = escaped_second;
    return result;
}

<*
@require iterator != null : "boundary_build_destination_iterator_escape: null iterator"
@require interp != null : "boundary_build_destination_iterator_escape: null interp"
@require target_scope != null : "boundary_build_destination_iterator_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_iterator_escape: null releasing_scope"
@ensure return == iterator || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_iterator_escape: expected ESCAPE-lane iterator wrapper"
*>
fn Value* boundary_build_destination_iterator_escape(
    Value* iterator,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (iterator.tag != ITERATOR) return iterator;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_inner = boundary_copy_to_parent_site_ctx(
        iterator.iterator_val,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_inner = boundary_promote_to_escape(routed_inner, interp);
    if (ctx != null && ctx.aborted) return iterator;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_inner, target_scope),
        "boundary destination-iterator: iterator payload must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = escaped_inner;
    return result;
}

<*
@require error_value != null : "boundary_build_destination_error_escape: null error value"
@require interp != null : "boundary_build_destination_error_escape: null interp"
@require target_scope != null : "boundary_build_destination_error_escape: null target_scope"
@ensure return == error_value || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_error_escape: expected ESCAPE-lane error wrapper"
*>
fn Value* boundary_build_destination_error_escape(
    Value* error_value,
    Interp* interp,
    main::ScopeRegion* target_scope
) {
    if (error_value.tag != ERROR) return error_value;
    Value* result = interp.alloc_value_escape();
    result.tag = ERROR;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);

    usz len = error_value.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = error_value.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    return result;
}

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
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    BoundaryReturnClassification classification =
        boundary_classify_return_value(result, interp, target_scope, releasing_scope);

    switch (classification.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            committed.value = result;
            committed.outcome = BOUNDARY_COMMIT_REUSED_IN_TARGET_CHAIN;
            committed.reused_in_target_chain = true;
            boundary_txn_commit(&txn);
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
            // A pre-aborted caller context should not force boundary finalize
            // into immediate fallback. Use an isolated boundary context so
            // destination routing and splice legality can still run.
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
            if (result.tag == ERROR) {
                // Stack-overflow/error unwinds must avoid disjoint promotion
                // routing that mutates promotion-context budget on deep stacks.
                Value* destination_error = boundary_build_destination_error_escape(
                    result,
                    interp,
                    target_scope
                );
                if (destination_error != null &&
                    destination_error != result &&
                    destination_error.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_error;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                fallback_fault = BOUNDARY_COMMIT_FAULT_DESTINATION_ERROR_BUILD;
            } else if (result.tag == CONS) {
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
                    committed.value = destination_cons;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_CONS_BUILT;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                fallback_fault = BOUNDARY_COMMIT_FAULT_DESTINATION_CONS_BUILD;
            } else if (result.tag == PARTIAL_PRIM) {
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
                    committed.value = destination_partial;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                fallback_fault = BOUNDARY_COMMIT_FAULT_DESTINATION_PARTIAL_BUILD;
            } else if (result.tag == ITERATOR) {
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
                    committed.value = destination_iterator;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                fallback_fault = BOUNDARY_COMMIT_FAULT_DESTINATION_ITERATOR_BUILD;
            } else {
                // For remaining TEMP-owned survivor tags, route directly into the
                // destination scope's ESCAPE lane instead of promoting in the
                // releasing scope and relying on splice legality.
                Value* destination_direct = boundary_promote_to_escape(result, interp);
                if (!active_ctx.aborted &&
                    destination_direct != null &&
                    destination_direct != result &&
                    destination_direct.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_direct;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                fallback_fault = BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION;
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
                    BoundaryScopeTransferResult splice_result =
                        boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                    if (splice_result.allowed) {
                        committed.value = result;
                        committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                        committed.releasing_scope_spliced = true;
                        boundary_txn_commit(&txn);
                        return committed;
                    }
                    splice_rejected_for_commit = true;
                    splice_reject_reason = splice_result.reason;
                }
                BoundaryPromotionAbortReason abort_reason =
                    BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED;
                if (pre_aborted_ctx || active_ctx.abort_reason != PROMOTION_ABORT_BUDGET_EXHAUSTED) {
                    abort_reason = BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX;
                }
                boundary_note_promotion_abort(
                    abort_reason
                );
                committed.fault_code = BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED;
                committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
                committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
                boundary_txn_abort(&txn);
                return committed;
            }

            if (allow_scope_splice &&
                classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                boundary_should_promote(result, active_ctx, releasing_scope)) {
                BoundaryScopeTransferResult splice_result =
                    boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                if (splice_result.allowed) {
                    committed.value = result;
                    committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                    committed.releasing_scope_spliced = true;
                    boundary_txn_commit(&txn);
                    return committed;
                }
                splice_rejected_for_commit = true;
                splice_reject_reason = splice_result.reason;
            }
        }

        // For non-unique releasing scopes, allow a conservative destination
        // retry for flat scalar tags that do not carry child graph edges.
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
                return committed;
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

    // Unknown or mixed provenance: first try destination ESCAPE promotion under
    // explicit promotion budget/context; if unresolved, fail deterministically.
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
        return committed;
    }

    committed.fault_code = BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION;
    committed.value = boundary_fallback_disallowed_error(interp, committed.fault_code);
    committed.outcome = BOUNDARY_COMMIT_FALLBACK_DISALLOWED;
    boundary_txn_abort(&txn);
    return committed;
}
