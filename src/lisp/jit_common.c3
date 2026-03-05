module lisp;

import std::io;
import std::collections::list;
import main;

faultdef JIT_COMPILE_FAILED;

// =============================================================================
// Context boundary state — saved/restored at every StackCtx switch point.
// Replaces 12-field manual save/restore (24 lines per site → 2 lines).
// =============================================================================

struct SavedInterpState {
    Env*        jit_env;
    Env*        match_env;
    usz         eval_depth;
    InterpFlags flags;
    Expr*       tco_expr;
    Env*        tco_env;
    main::ScopeRegion* tco_recycle;
    usz         tco_scope_defer_slot;
    bool        tco_scope_defer_active;
    usz         reset_depth;
    usz         handler_count;
    main::ScopeRegion* scope;
    bool        escape_env_mode;
}

fn void save_interp_state(Interp* interp, SavedInterpState* s) {
    boundary_assert_interp_scope_chain(interp);
    s.jit_env         = interp.jit_env;
    s.match_env       = interp.match_env;
    s.eval_depth      = interp.eval_depth;
    s.flags           = interp.flags;
    s.tco_expr        = interp.jit_tco_expr;
    s.tco_env         = interp.jit_tco_env;
    s.tco_recycle     = interp.tco_recycle_scope;
    s.tco_scope_defer_slot = interp.tco_scope_defer_slot;
    s.tco_scope_defer_active = interp.tco_scope_defer_active;
    s.reset_depth     = interp.reset_depth;
    s.handler_count   = interp.handler_count;
    s.scope           = interp.current_scope;
    s.escape_env_mode = interp.escape_env_mode;
}

fn void restore_interp_state(Interp* interp, SavedInterpState* s) {
    interp.jit_env           = s.jit_env;
    interp.match_env         = s.match_env;
    interp.eval_depth        = s.eval_depth;
    interp.flags             = s.flags;
    interp.jit_tco_expr      = s.tco_expr;
    interp.jit_tco_env       = s.tco_env;
    interp.tco_recycle_scope = s.tco_recycle;
    interp.tco_scope_defer_slot = s.tco_scope_defer_slot;
    interp.tco_scope_defer_active = s.tco_scope_defer_active;
    interp.reset_depth       = s.reset_depth;
    interp.handler_count     = s.handler_count;
    interp.current_scope     = s.scope;
    interp.escape_env_mode   = s.escape_env_mode;
    boundary_assert_interp_scope_chain(interp);
}

fn void scope_guard_defer_destroy(void* arg) {
    main::ScopeRegion* scope = (main::ScopeRegion*)arg;
    if (scope != null) main::scope_release(scope);
}

fn void scope_guard_defer_clone(void* arg) {
    main::ScopeRegion* scope = (main::ScopeRegion*)arg;
    if (scope != null) main::scope_retain(scope);
}

fn void init_scope_guard_defer_ops(main::StackCtxDeferOps* ops) @inline {
    ops.destroy_fn = &scope_guard_defer_destroy;
    ops.clone_fn = &scope_guard_defer_clone;
}

fn bool pop_scope_guard_defer(main::StackCtx* fallback_ctx) {
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) current = fallback_ctx;
    if (current == null) return false;
    return main::stack_ctx_undefer(current);
}

fn bool suspend_with_scope_guard(main::StackCtx* current, main::ScopeRegion* scope) {
    if (current == null || scope == null) return false;

    // Keep scope alive for suspend lifetime and clone-safe destroy path.
    main::scope_retain(scope);
    main::StackCtxDeferOps ops;
    init_scope_guard_defer_ops(&ops);
    if (!main::stack_ctx_defer(current, &ops, (void*)scope)) {
        main::scope_release(scope);
        return false;
    }

    main::stack_ctx_suspend();

    bool popped = pop_scope_guard_defer(current);
    main::scope_release(scope);
    return popped;
}
