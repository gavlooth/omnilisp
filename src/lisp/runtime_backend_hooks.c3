module lisp;

import std::io;
import main;

/**
 * Narrow backend seam for interpreter lifecycle code.
 *
 * Today these hooks delegate to the JIT/runtime implementation. Keeping the
 * signatures JIT-agnostic lets interpreter-owned files stop depending on
 * JIT-only state types directly, which is the first requirement for any future
 * interpreter-only dev target.
 */

struct RuntimeCompiledExpr {
    void* code_fn;
    ulong compile_nonce;
}

fn bool runtime_compiled_expr_is_live_for_interp(RuntimeCompiledExpr compiled, Interp* interp) {
    return runtime_backend_compiled_expr_is_live_for_interp(compiled, interp);
}

fn RuntimeCompiledExpr runtime_compiled_expr_null() @inline {
    return { .code_fn = null, .compile_nonce = 0 };
}

fn RuntimeCompiledExpr runtime_lookup_or_compile_expr(Expr* expr, Interp* interp) {
    RuntimeCompiledExpr cached = runtime_cache_lookup_expr(expr, interp);
    if (!runtime_compiled_expr_is_null(cached)) return cached;
    RuntimeCompiledExpr compiled = runtime_backend_compile_expr(expr, interp);
    if (runtime_compiled_expr_is_null(compiled)) return runtime_compiled_expr_null();
    runtime_cache_store_expr(expr, compiled, interp);
    return compiled;
}

fn Value* runtime_exec_compiled_checked_expr(RuntimeCompiledExpr compiled, Interp* interp) {
    return runtime_backend_exec_compiled_checked_expr(compiled, interp);
}

fn Value* runtime_prepare_tco_recycle_env(Env** env_io, Env* saved_env, Interp* interp) {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* cs = interp.current_scope;
    if (cs != interp.tco_recycle_scope || cs.refcount != 1) return null;

    bool chain_touches_temp_scope =
        env_chain_has_frame_in_temp_scope(*env_io, cs, interp.global_env);
    bool bindings_touch_temp_scope =
        env_chain_has_binding_in_temp_scope(*env_io, cs, interp.global_env);
    bool has_root_persistent_box =
        env_chain_contains_root_persistent_box(*env_io, interp.global_env);
    bool has_graph_binding_values_in_temp_scope =
        env_chain_has_graph_binding_values_in_temp_scope(*env_io, cs, interp.global_env);
    bool allow_fast_reset =
        !has_root_persistent_box &&
        !has_graph_binding_values_in_temp_scope;

    if (allow_fast_reset &&
        !chain_touches_temp_scope &&
        !bindings_touch_temp_scope) {
        main::scope_reset_temp_lane(cs);
        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        boundary_assert_interp_scope_chain(interp);
        return null;
    }

    // TEMP-lane graph accumulators are expensive to deep-copy on every bounce.
    // Escape-lane graphs are already safe across temp resets and should fall
    // through to the fast-reset path above instead of pinning the whole scope.
    if (has_graph_binding_values_in_temp_scope &&
        (chain_touches_temp_scope || bindings_touch_temp_scope)) {
        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        boundary_assert_interp_scope_chain(interp);
        return null;
    }

    main::ScopeRegion* fresh = main::scope_create(cs.parent);
    if (fresh == null) {
        interp.jit_env = saved_env;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: failed to allocate TCO recycle scope");
    }

    interp.current_scope = fresh;
    interp.tco_recycle_scope = fresh;
    boundary_assert_interp_scope_chain(interp);

    main::StackCtx* defer_ctx = null;
    if (interp.tco_scope_defer_active) {
        defer_ctx = main::g_current_stack_ctx;
        if (defer_ctx == null) {
            interp.current_scope = cs;
            interp.tco_recycle_scope = cs;
            main::scope_release(fresh);
            interp.jit_env = saved_env;
            boundary_assert_interp_scope_chain(interp);
            return raise_error(interp, "jit: missing active stack context for call-scope defer");
        }
        bool retargeted = main::stack_ctx_defer_update_arg(
            defer_ctx,
            interp.tco_scope_defer_slot,
            (void*)fresh
        );
        if (!retargeted) {
            interp.current_scope = cs;
            interp.tco_recycle_scope = cs;
            main::scope_release(fresh);
            interp.jit_env = saved_env;
            boundary_assert_interp_scope_chain(interp);
            return raise_error(interp, "jit: failed to retarget call-scope defer");
        }
    }

    Env* previous_env = *env_io;
    Env* copied_env = jit_copy_tco_env_chain_for_recycle(previous_env, interp, cs);
    if (previous_env != null && copied_env == null) {
        if (interp.tco_scope_defer_active && defer_ctx != null) {
            (void)main::stack_ctx_defer_update_arg(
                defer_ctx,
                interp.tco_scope_defer_slot,
                (void*)cs
            );
        }
        interp.current_scope = cs;
        interp.tco_recycle_scope = cs;
        main::scope_release(fresh);
        interp.jit_env = saved_env;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: failed to copy TCO recycle env");
    }
    *env_io = copied_env;
    main::scope_release(cs);
    boundary_assert_interp_scope_chain(interp);
    return null;
}

fn bool runtime_tco_scope_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_TCO_SCOPE") != null;
}

fn Value* runtime_eval_expr(Expr* expr, Env* env, Interp* interp) {
    Env* saved_env = interp.jit_env;

    for (;;) {
        if (interp_interrupt_requested(interp)) {
            interp_interrupt_clear(interp);
            interp.jit_env = saved_env;
            return interp_interrupt_error(interp);
        }

        if (expr == null) {
            interp.jit_env = saved_env;
            return make_nil(interp);
        }

        RuntimeCompiledExpr compiled = runtime_lookup_or_compile_expr(expr, interp);
        if (runtime_compiled_expr_is_null(compiled)) {
            interp.jit_env = saved_env;
            return raise_error(interp, "jit: failed to compile expression for runtime evaluation");
        }

        interp.jit_env = env;
        interp.flags.jit_tco_bounce = false;
        Value* result = runtime_exec_compiled_checked_expr(compiled, interp);
        if (is_error(result)) {
            interp.jit_env = saved_env;
            return result;
        }

        if (interp.flags.jit_tco_bounce) {
            expr = interp.jit_tco_expr;
            env = interp.jit_tco_env;
            interp.flags.jit_tco_bounce = false;

            Value* recycle_err = runtime_prepare_tco_recycle_env(&env, saved_env, interp);
            if (recycle_err != null) {
                interp.jit_env = saved_env;
                return recycle_err;
            }
            continue;
        }

        if (runtime_tco_scope_trace_enabled()) {
            io::printfn(
                "OMNI_TCO_SCOPE_TRACE stage=jit-eval-return expr_tag=%d current_gen=%d env_scope_gen=%d result_tag=%d",
                (int)expr.tag,
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                env != null ? (int)env.scope_gen : -1,
                result != null ? (int)result.tag : -1
            );
        }

        interp.jit_env = saved_env;
        return result;
    }
}

fn RuntimeCompiledExpr runtime_compile_expr(Expr* expr, Interp* interp) @inline {
    return runtime_backend_compile_expr(expr, interp);
}

fn bool runtime_compiled_expr_is_null(RuntimeCompiledExpr compiled) @inline {
    return compiled.code_fn == null;
}

fn Value* runtime_exec_compiled_expr(RuntimeCompiledExpr compiled, Interp* interp) @inline {
    return runtime_backend_exec_compiled_expr(compiled, interp);
}
