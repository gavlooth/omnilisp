module lisp;

import main;

struct ScopedFinalizeResult {
    Value* value;
    bool releasing_scope_spliced;
}

<*
@require interp != null : "boundary_finalize_scoped_result: null interp"
@require saved_scope != null : "boundary_finalize_scoped_result: null saved_scope"
@require active_scope != null : "boundary_finalize_scoped_result: null active_scope"
*>
fn ScopedFinalizeResult boundary_finalize_scoped_result(
    Value* result,
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope
) {
    boundary_assert_interp_scope_chain(interp);
    ScopedFinalizeResult finalized = {
        .value = result,
        .releasing_scope_spliced = false,
    };
    PromotionContext boundary_ctx = {};
    // Return-boundary finalize must not inherit traversal-budget fallback behavior.
    // Use an unbounded boundary context so commit routing can terminate on
    // destination-aware promotion/splice/copy policy rather than budget abort.
    boundary_promotion_context_begin(interp, &boundary_ctx);
    defer {
        boundary_promotion_context_end(interp, &boundary_ctx);
        boundary_assert_interp_scope_chain(interp);
    }
    BoundaryCommitEscapeResult committed = boundary_commit_escape(
        result,
        interp,
        saved_scope,
        active_scope,
        &boundary_ctx
    );
    finalized.value = committed.value;
    finalized.releasing_scope_spliced = committed.releasing_scope_spliced;
    return finalized;
}

fn Value* jit_finalize_scoped_result(
    Value* result,
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* active_scope,
    bool* scope_spliced_out = null
) {
    ScopedFinalizeResult finalized = boundary_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (scope_spliced_out != null) {
        *scope_spliced_out = finalized.releasing_scope_spliced;
    }
    if (!finalized.releasing_scope_spliced) {
        main::scope_release(active_scope);
    }
    return finalized.value;
}

fn Env* jit_copy_tco_env_chain_for_recycle(
    Env* env,
    Interp* interp,
    main::ScopeRegion* releasing_scope
) {
    boundary_assert_interp_scope_chain(interp);
    BoundaryInterpState saved = boundary_save_interp_state(interp);
    defer boundary_restore_interp_state(interp, saved);
    BoundaryReleasingScopeSwap releasing_swap =
        boundary_releasing_scope_swap_begin(interp, releasing_scope);
    defer boundary_releasing_scope_swap_end(interp, &releasing_swap);
    PromotionContext bounce_ctx = {};
    boundary_promotion_context_begin(interp, &bounce_ctx, PROMOTION_CTX_BUDGET_TCO);
    defer boundary_promotion_context_end(interp, &bounce_ctx);
    Env* copied = copy_tco_env_chain(env, interp);
    boundary_assert_interp_scope_chain(interp);
    return copied;
}
