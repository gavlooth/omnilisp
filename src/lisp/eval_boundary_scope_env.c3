module lisp;

import main;

// =============================================================================
// Boundary scope/env facade
// =============================================================================

int g_boundary_force_alloc_value_fail_nth = 0;
int g_boundary_force_alloc_escape_value_fail_nth = 0;

fn bool boundary_should_force_alloc_value_fail() @inline {
    if (g_boundary_force_alloc_value_fail_nth <= 0) return false;
    g_boundary_force_alloc_value_fail_nth--;
    return g_boundary_force_alloc_value_fail_nth == 0;
}

fn bool boundary_should_force_alloc_escape_value_fail() @inline {
    if (g_boundary_force_alloc_escape_value_fail_nth <= 0) return false;
    g_boundary_force_alloc_escape_value_fail_nth--;
    return g_boundary_force_alloc_escape_value_fail_nth == 0;
}

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
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope_impl(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    Value* v = boundary_should_force_alloc_value_fail() ? null : interp.alloc_value();
    if (register_dtor && v != null) {
        if (!main::scope_register_dtor(target_scope, (void*)v, &scope_dtor_value)) return null;
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
@require interp != null : "boundary_alloc_value_in_current_escape: null interp"
@require interp.current_scope != null : "boundary_alloc_value_in_current_escape: null current scope"
*>
fn Value* boundary_alloc_value_in_current_escape(Interp* interp, bool register_dtor = false) @inline {
    Value* v = boundary_should_force_alloc_escape_value_fail() ? null : interp.alloc_value_escape();
    if (register_dtor && v != null) {
        if (!main::scope_register_dtor_escape(interp.current_scope, (void*)v, &scope_dtor_value)) return null;
    }
    return v;
}

<*
@require interp != null : "boundary_alloc_value_in_escape_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_escape_scope: null target scope"
*>
fn Value* boundary_alloc_value_in_escape_scope_impl(
    Interp* interp,
    main::ScopeRegion* target_scope,
    bool register_dtor = false
) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return boundary_alloc_value_in_current_escape(interp, register_dtor);
}

<*
@require interp != null : "boundary_alloc_value_in_escape_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_escape_scope: null target scope"
*>
fn Value* boundary_alloc_value_in_escape_scope(
    Interp* interp,
    main::ScopeRegion* target_scope,
    bool register_dtor = false
) @inline {
    return boundary_alloc_value_in_escape_scope_impl(interp, target_scope, register_dtor);
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
    Env* extended = boundary_make_env_in_scope(interp, target_scope, env);
    if (extended == null) return null;
    // Preserve the caller's source-scope context for the write barrier so
    // cross-scope env extension copies child-scope values into the target env
    // instead of storing transient wrappers by raw pointer.
    if (!env_define_with_barrier_checked(extended, name, value, interp)) {
        return null;
    }
    return extended;
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
