module lisp;

import std::io;
import std::collections::list;
import std::core::mem;
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
    char[256]   raise_msg;
    usz         raise_msg_len;
    Value*      raise_payload;
    Expr*       tco_expr;
    Env*        tco_env;
    main::ScopeRegion* tco_recycle;
    usz         tco_scope_defer_slot;
    bool        tco_scope_defer_active;
    usz         reset_depth;
    usz         jit_exec_depth;
    void*       current_reset_state;
    usz         handler_count;
    main::ScopeRegion* scope;
    main::ScopeRegion* releasing_scope;
    PromotionContext* active_promotion_ctx;
    bool        escape_env_mode;
}

fn void save_interp_state(Interp* interp, SavedInterpState* s) {
    boundary_assert_interp_scope_chain(interp);
    s.jit_env         = interp.jit_env;
    s.match_env       = interp.match_env;
    s.eval_depth      = interp.eval_depth;
    s.flags           = interp.flags;
    s.raise_msg_len   = interp.raise_msg_len > 255 ? 255 : interp.raise_msg_len;
    if (s.raise_msg_len > 0) {
        mem::copy(&s.raise_msg[0], &interp.raise_msg[0], s.raise_msg_len);
    }
    s.raise_msg[s.raise_msg_len] = 0;
    s.raise_payload   = interp.raise_payload;
    s.tco_expr        = interp.jit_tco_expr;
    s.tco_env         = interp.jit_tco_env;
    s.tco_recycle     = interp.tco_recycle_scope;
    s.tco_scope_defer_slot = interp.tco_scope_defer_slot;
    s.tco_scope_defer_active = interp.tco_scope_defer_active;
    s.reset_depth     = interp.reset_depth;
    s.jit_exec_depth  = g_jit_exec_depth;
    s.current_reset_state = interp.current_reset_state;
    s.handler_count   = interp.handler_count;
    s.scope           = interp.current_scope;
    s.releasing_scope = interp.releasing_scope;
    s.active_promotion_ctx = interp.active_promotion_ctx;
    s.escape_env_mode = interp.escape_env_mode;
}

fn void restore_interp_state(Interp* interp, SavedInterpState* s) {
    interp.jit_env           = s.jit_env;
    interp.match_env         = s.match_env;
    interp.eval_depth        = s.eval_depth;
    interp.flags             = s.flags;
    interp.raise_msg_len     = s.raise_msg_len;
    if (s.raise_msg_len > 0) {
        mem::copy(&interp.raise_msg[0], &s.raise_msg[0], s.raise_msg_len);
    }
    interp.raise_msg[s.raise_msg_len] = 0;
    interp.raise_payload     = s.raise_payload;
    interp.jit_tco_expr      = s.tco_expr;
    interp.jit_tco_env       = s.tco_env;
    interp.tco_recycle_scope = s.tco_recycle;
    interp.tco_scope_defer_slot = s.tco_scope_defer_slot;
    interp.tco_scope_defer_active = s.tco_scope_defer_active;
    interp.reset_depth       = s.reset_depth;
    g_jit_exec_depth         = s.jit_exec_depth;
    interp.current_reset_state = s.current_reset_state;
    interp.handler_count     = s.handler_count;
    interp.current_scope     = s.scope;
    interp.releasing_scope   = s.releasing_scope;
    interp.active_promotion_ctx = s.active_promotion_ctx;
    interp.escape_env_mode   = s.escape_env_mode;
    boundary_assert_interp_scope_chain(interp);
}

fn void restore_interp_state_preserve_raise(Interp* interp, SavedInterpState* s) {
    bool raise_pending = interp.flags.raise_pending;
    usz raise_len = interp.raise_msg_len;
    Value* raise_payload = interp.raise_payload;
    if (raise_len > 255) raise_len = 255;
    char[256] raise_buf;
    if (raise_pending && raise_len > 0) {
        mem::copy(&raise_buf[0], &interp.raise_msg[0], raise_len);
    }
    restore_interp_state(interp, s);
    if (raise_pending) {
        interp.flags.raise_pending = true;
        interp.raise_msg_len = raise_len;
        interp.raise_payload = raise_payload;
        if (raise_len > 0) {
            mem::copy(&interp.raise_msg[0], &raise_buf[0], raise_len);
        }
    } else if (interp.raise_msg_len == 0) {
        interp.raise_payload = null;
    }
}

fn void scope_guard_defer_destroy(void* arg) {
    main::ScopeRegion* scope = (main::ScopeRegion*)arg;
    if (scope != null) main::scope_release(scope);
}

fn void scope_guard_defer_clone(void* arg) {
    main::ScopeRegion* scope = (main::ScopeRegion*)arg;
    if (scope != null) main::scope_retain(scope);
}

fn void scope_guard_suspend_defer_destroy(void* arg) {
    scope_guard_defer_destroy(arg);
    jit_suspend_guard_unregister("jit_suspend_guard_unregister_destroy");
}

fn void scope_guard_suspend_defer_clone(void* arg) {
    scope_guard_defer_clone(arg);
    jit_suspend_guard_register();
}

fn void init_scope_guard_defer_ops(main::StackCtxDeferOps* ops) @inline {
    ops.destroy_fn = &scope_guard_defer_destroy;
    ops.clone_fn = &scope_guard_defer_clone;
}

fn void init_scope_guard_suspend_defer_ops(main::StackCtxDeferOps* ops) @inline {
    ops.destroy_fn = &scope_guard_suspend_defer_destroy;
    ops.clone_fn = &scope_guard_suspend_defer_clone;
}

fn bool pop_scope_guard_defer(main::StackCtx* fallback_ctx) {
    // Prefer the currently running stack context. This is required for
    // multi-shot continuation clones where stack locals may still reference
    // the original source context pointer.
    main::StackCtx* active = main::g_current_stack_ctx;
    if (active != null) {
        if (main::stack_ctx_undefer(active)) return true;
        if (fallback_ctx == null || fallback_ctx == active) return false;
        return main::stack_ctx_undefer(fallback_ctx);
    }
    if (fallback_ctx == null) return false;
    return main::stack_ctx_undefer(fallback_ctx);
}

fn bool suspend_with_scope_guard(main::StackCtx* current, main::ScopeRegion* scope) {
    if (current == null || scope == null) return false;

    // Keep scope alive for suspend lifetime and clone-safe destroy path.
    main::scope_retain(scope);
    main::StackCtxDeferOps ops;
    init_scope_guard_suspend_defer_ops(&ops);
    if (!main::stack_ctx_defer(current, &ops, (void*)scope)) {
        main::scope_release(scope);
        return false;
    }
    jit_suspend_guard_register();

    main::stack_ctx_suspend();

    bool popped = pop_scope_guard_defer(current);
    if (!popped) {
        // Defer entry still owns the retained scope on this path; avoid an
        // eager release that can over-release at later context teardown.
        return false;
    }
    jit_suspend_guard_unregister("jit_suspend_guard_unregister_pop");
    main::scope_release(scope);
    return true;
}
