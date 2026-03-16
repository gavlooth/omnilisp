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

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (audited boundary facade)
// =============================================================================

<*
@require interp != null : "boundary_enter_scope: null interp"
@require target_scope != null : "boundary_enter_scope: null target_scope"
*>
fn main::ScopeRegion* boundary_enter_scope(Interp* interp, main::ScopeRegion* target_scope) @inline {
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = target_scope;
    boundary_assert_interp_scope_chain(interp);
    return saved_scope;
}

<*
@require interp != null : "boundary_leave_scope: null interp"
*>
fn void boundary_leave_scope(Interp* interp, main::ScopeRegion* saved_scope) @inline {
    interp.current_scope = saved_scope;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_push_child_scope: null interp"
*>
fn main::ScopeRegion* boundary_push_child_scope(Interp* interp, main::ScopeRegion** saved_scope_out = null) @inline {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = interp.current_scope;
    if (saved_scope_out != null) {
        *saved_scope_out = saved_scope;
    }
    main::ScopeRegion* child_scope = main::scope_create(saved_scope);
    if (child_scope == null) {
        return null;
    }
    interp.current_scope = child_scope;
    return child_scope;
}

<*
@require interp != null : "boundary_pop_child_scope: null interp"
*>
fn void boundary_pop_child_scope(
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* child_scope
) @inline {
    interp.current_scope = saved_scope;
    boundary_assert_interp_scope_chain(interp);
    if (child_scope != null) {
        main::scope_release(child_scope);
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
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope_impl(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    Value* v = interp.alloc_value();
    if (register_dtor) {
        main::scope_register_dtor(target_scope, (void*)v, &scope_dtor_value);
    }
    return v;
}

<*
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @inline {
    return boundary_alloc_value_in_scope_impl(interp, target_scope, register_dtor);
}

<*
@require interp != null : "boundary_alloc_value_in_root: null interp"
*>
fn Value* boundary_alloc_value_in_root(Interp* interp, bool register_dtor = false) @inline {
    return boundary_alloc_value_in_scope(interp, interp.root_scope, register_dtor);
}

<*
@require interp != null : "boundary_make_env_in_scope: null interp"
@require target_scope != null : "boundary_make_env_in_scope: null target_scope"
*>
fn Env* boundary_make_env_in_scope_impl(Interp* interp, main::ScopeRegion* target_scope, Env* parent) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return make_env(interp, parent);
}

<*
@require interp != null : "boundary_make_env_in_scope: null interp"
@require target_scope != null : "boundary_make_env_in_scope: null target_scope"
*>
fn Env* boundary_make_env_in_scope(Interp* interp, main::ScopeRegion* target_scope, Env* parent) @inline {
    return boundary_make_env_in_scope_impl(interp, target_scope, parent);
}

<*
@require interp != null : "boundary_make_env_in_root: null interp"
*>
fn Env* boundary_make_env_in_root(Interp* interp, Env* parent) @inline {
    return boundary_make_env_in_scope(interp, interp.root_scope, parent);
}

<*
@require interp != null : "boundary_env_extend_in_scope: null interp"
@require target_scope != null : "boundary_env_extend_in_scope: null target_scope"
@require env != null : "boundary_env_extend_in_scope: null env"
*>
fn Env* boundary_env_extend_in_scope_impl(
    Interp* interp,
    main::ScopeRegion* target_scope,
    Env* env,
    SymbolId name,
    Value* value
) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return env.extend(interp, name, value);
}

<*
@require interp != null : "boundary_env_extend_in_scope: null interp"
@require target_scope != null : "boundary_env_extend_in_scope: null target_scope"
@require env != null : "boundary_env_extend_in_scope: null env"
*>
fn Env* boundary_env_extend_in_scope(
    Interp* interp,
    main::ScopeRegion* target_scope,
    Env* env,
    SymbolId name,
    Value* value
) @inline {
    return boundary_env_extend_in_scope_impl(interp, target_scope, env, name, value);
}

<*
@require interp != null : "boundary_env_extend_in_root: null interp"
@require env != null : "boundary_env_extend_in_root: null env"
*>
fn Env* boundary_env_extend_in_root(Interp* interp, Env* env, SymbolId name, Value* value) @inline {
    Env* box = boundary_env_extend_in_scope(interp, interp.root_scope, env, name, value);
    if (box != null) {
        box.lifetime_kind = ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX;
    }
    return box;
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
@require interp != null : "boundary_copy_env_to_target_scope: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope: null target_scope"
*>
fn Env* boundary_copy_env_to_target_scope_impl(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    // Env-copy boundaries release from the caller's active scope into target_scope.
    interp.releasing_scope = session.current_scope;
    return boundary_copy_env_to_scope_checked(env, interp, depth, ctx).env;
}

<*
@require interp != null : "boundary_copy_env_to_target_scope_checked: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope_checked: null target_scope"
*>
fn BoundaryEnvCopyResult boundary_copy_env_to_target_scope_checked(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @nodiscard @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = session.current_scope;
    return boundary_copy_env_to_scope_checked(env, interp, depth, ctx);
}

<*
@require interp != null : "boundary_copy_env_to_target_scope: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope: null target_scope"
*>
fn Env* boundary_copy_env_to_target_scope(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @nodiscard @inline {
    return boundary_copy_env_to_target_scope_impl(env, interp, target_scope, depth, ctx);
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
