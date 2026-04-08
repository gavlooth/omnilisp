module lisp;

import main;

fn void boundary_mark_active_promotion_ctx_aborted(Interp* interp, PromotionContext* local_ctx = null) @inline {
    if (local_ctx != null) {
        local_ctx.aborted = true;
        if (local_ctx.abort_reason == PROMOTION_ABORT_NONE) {
            local_ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
        }
    }
    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) {
        active_ctx.aborted = true;
        if (active_ctx.abort_reason == PROMOTION_ABORT_NONE) {
            active_ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
        }
    }
}

<*
@require interp != null : "boundary_copy_to_parent_site_ctx: null interp"
*>
fn BoundaryCopyResult boundary_copy_to_parent_site_ctx_checked(
    Value* v,
    Interp* interp,
    uint site,
    PromotionContext* ctx
) @nodiscard @inline {
    if (v == null) return boundary_copy_ok(null);
    if (ctx != null && ctx.aborted) {
        return boundary_copy_fail(BOUNDARY_COPY_FAULT_CONTEXT_ABORTED);
    }
    if (interp.current_scope == null || interp.releasing_scope == null) {
        (void)site;
        return boundary_copy_fail(BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE);
    }
    if (ctx != null) {
        Value* cached = promotion_context_lookup(ctx, v);
        if (cached != null) return boundary_copy_ok(cached);
    }
    bool can_track = promotion_context_consume(ctx, 1);
    BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
    Value* copied = copy_to_parent_site_checked(v, interp, site, &copy_fault);
    if (copied == null) {
        if (copy_fault == BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY) {
            boundary_mark_active_promotion_ctx_aborted(interp, ctx);
        }
        if (copy_fault == BOUNDARY_COPY_FAULT_NONE) {
            return boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        }
        return boundary_copy_fail(copy_fault);
    }
    if (can_track && ctx != null && copied != null) {
        promotion_context_remember(ctx, v, copied, interp);
    }
    return boundary_copy_ok(copied);
}

<*
@require interp != null : "boundary_copy_to_parent_site_ctx: null interp"
*>
fn Value* boundary_copy_to_parent_site_ctx(Value* v, Interp* interp, uint site, PromotionContext* ctx) @nodiscard @inline {
    BoundaryCopyResult copy_result =
        boundary_copy_to_parent_site_ctx_checked(v, interp, site, ctx);
    if (!boundary_copy_result_ok(copy_result)) {
        return raise_error(interp, boundary_copy_fault_message(copy_result.fault_code));
    }
    return copy_result.value;
}

<*
@require interp != null : "boundary_promote_to_escape: null interp"
*>
fn Value* boundary_promote_to_escape(Value* v, Interp* interp) @inline {
    return promote_to_escape(v, interp);
}

<*
@require interp != null : "boundary_promote_to_root_site: null interp"
*>
fn Value* boundary_promote_to_root_site(Value* v, Interp* interp, uint site) @inline {
    return promote_to_root_site(v, interp, site);
}

<*
@require interp != null : "boundary_promote_to_root: null interp"
*>
fn Value* boundary_promote_to_root(Value* v, Interp* interp) @inline {
    return promote_to_root(v, interp);
}

<*
@require interp != null : "boundary_copy_env_to_scope_checked: null interp"
*>
fn BoundaryEnvCopyResult boundary_copy_env_to_scope_checked(
    Env* env,
    Interp* interp,
    usz depth = 0,
    PromotionContext* ctx = null
) @nodiscard @inline {
    return copy_env_to_scope_checked(env, interp, depth, ctx);
}

<*
@require interp != null : "boundary_promotion_context_begin: null interp"
@require ctx != null : "boundary_promotion_context_begin: null ctx"
*>
fn void boundary_promotion_context_begin(Interp* interp, PromotionContext* ctx, usz budget = usz.max) @inline {
    promotion_context_begin(interp, ctx, budget);
}

<*
@require interp != null : "boundary_promotion_context_end: null interp"
@require ctx != null : "boundary_promotion_context_end: null ctx"
*>
fn void boundary_promotion_context_end(Interp* interp, PromotionContext* ctx) @inline {
    promotion_context_end(interp, ctx);
}
