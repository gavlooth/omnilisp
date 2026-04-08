module lisp;

import std::io;
import main;

fn JitCompiledFn jit_lookup_or_compile(Expr* expr, Interp* interp) {
    return runtime_backend_unwrap_compiled_expr(runtime_lookup_or_compile_expr(expr, interp));
}

fn Value* jit_tco_recycle_restore_on_error(
    Interp* interp,
    main::ScopeRegion* restore_scope,
    main::ScopeRegion* fresh_scope,
    Env* saved_env,
    char[] msg
) {
    interp.current_scope = restore_scope;
    interp.tco_recycle_scope = restore_scope;
    if (fresh_scope != null) {
        main::scope_release(fresh_scope);
    }
    interp.jit_env = saved_env;
    boundary_assert_interp_scope_chain(interp);
    return raise_error(interp, msg);
}

fn void jit_set_active_recycle_scope(Interp* interp, main::ScopeRegion* scope) @inline {
    interp.current_scope = scope;
    interp.tco_recycle_scope = scope;
    boundary_assert_interp_scope_chain(interp);
}

struct JitCallScopeState {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* tco_recycle_scope;
    usz tco_scope_defer_slot;
    bool tco_scope_defer_active;
    bool escape_env_mode;
}

fn JitCallScopeState jit_save_call_scope_state(Interp* interp) @inline {
    return {
        .current_scope = interp.current_scope,
        .tco_recycle_scope = interp.tco_recycle_scope,
        .tco_scope_defer_slot = interp.tco_scope_defer_slot,
        .tco_scope_defer_active = interp.tco_scope_defer_active,
        .escape_env_mode = interp.escape_env_mode,
    };
}

fn void jit_restore_call_scope_state(Interp* interp, JitCallScopeState saved) @inline {
    interp.current_scope = saved.current_scope;
    interp.tco_recycle_scope = saved.tco_recycle_scope;
    interp.tco_scope_defer_slot = saved.tco_scope_defer_slot;
    interp.tco_scope_defer_active = saved.tco_scope_defer_active;
    interp.escape_env_mode = saved.escape_env_mode;
    boundary_assert_interp_scope_chain(interp);
}

fn void jit_activate_call_scope_recycle(Interp* interp, main::ScopeRegion* call_scope, usz defer_slot, bool defer_active) @inline {
    interp.tco_recycle_scope = call_scope;
    interp.tco_scope_defer_slot = defer_slot;
    interp.tco_scope_defer_active = defer_active;
}

fn Value* jit_prepare_tco_recycle(
    Env** env_io,
    Env* saved_env,
    Interp* interp
) {
    return runtime_prepare_tco_recycle_env(env_io, saved_env, interp);
}

fn Value* jit_eval(Expr* expr, Env* env, Interp* interp) {
    return runtime_eval_expr(expr, env, interp);
}

fn bool jit_tco_scope_trace_enabled() @inline {
    return runtime_tco_scope_trace_enabled();
}
