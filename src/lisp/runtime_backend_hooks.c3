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

fn bool runtime_handle_state_is_live(void* state) @inline {
    return handle_effect_state_is_live((HandleEffectState*)state);
}

fn void runtime_handle_state_release(void* state) @inline {
    handle_effect_state_release((HandleEffectState*)state);
}

fn uint runtime_handle_state_continuation_refcount(void* state) @inline {
    return ((HandleEffectState*)state).continuation_refcount;
}

fn void runtime_handle_state_invalidate_continuation(void* state, bool drop_retained_ref) @inline {
    HandleEffectState* live_state = (HandleEffectState*)state;
    live_state.frame_active = false;
    if (drop_retained_ref && live_state.continuation_refcount > 0) {
        live_state.continuation_refcount--;
    }
}

fn isz runtime_attached_interp_index(Interp* interp) {
    if (interp == null) return -1;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == interp) return (isz)i;
    }
    return -1;
}

fn bool runtime_interp_is_attached(Interp* interp) {
    return runtime_attached_interp_index(interp) >= 0;
}

fn bool runtime_interp_has_active_exec(Interp* interp) {
    isz idx = runtime_attached_interp_index(interp);
    return idx >= 0 && runtime_backend_attached_interp_exec_depth_at((usz)idx) > 0;
}

fn bool runtime_interp_on_current_stack(Interp* interp) {
    if (interp == null) return false;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) return false;
    return current.owner_pool == &interp.stack_ctx_pool;
}

fn bool runtime_attached_interp_serial(Interp* interp, ulong* serial_out) {
    if (serial_out == null) return false;
    isz idx = runtime_attached_interp_index(interp);
    if (idx < 0) return false;
    *serial_out = runtime_backend_attached_interp_serial_at((usz)idx);
    return true;
}

fn usz runtime_interp_exec_depth(Interp* interp) @inline {
    isz idx = runtime_attached_interp_index(interp);
    return idx >= 0 ? runtime_backend_attached_interp_exec_depth_at((usz)idx) : 0;
}

fn void runtime_set_interp_exec_depth(Interp* interp, usz depth) {
    isz idx = runtime_attached_interp_index(interp);
    if (idx >= 0) runtime_backend_set_attached_interp_exec_depth_at((usz)idx, depth);
}

fn void runtime_exec_enter(Interp* interp) @inline {
    jit_require_owner_thread("runtime_exec_enter");
    runtime_backend_increment_exec_depth();
    isz idx = runtime_attached_interp_index(interp);
    assert(idx >= 0, "runtime_exec_enter: interp is not attached");
    runtime_backend_increment_attached_interp_exec_depth_at((usz)idx);
}

fn void runtime_exec_leave(Interp* interp) @inline {
    jit_require_owner_thread("runtime_exec_leave");
    assert(runtime_backend_exec_depth() > 0, "runtime_exec_leave: jit exec depth underflow");
    runtime_backend_decrement_exec_depth();
    isz idx = runtime_attached_interp_index(interp);
    assert(idx >= 0, "runtime_exec_leave: interp is not attached");
    assert(runtime_backend_attached_interp_exec_depth_at((usz)idx) > 0,
        "runtime_exec_leave: interp exec depth underflow");
    runtime_backend_decrement_attached_interp_exec_depth_at((usz)idx);
}

fn bool runtime_backend_gc_needed() @inline {
    return g_jit_gc_needed;
}

fn void runtime_backend_mark_gc_needed() @inline {
    g_jit_gc_needed = true;
}

fn void runtime_backend_clear_gc_needed() @inline {
    g_jit_gc_needed = false;
}

fn bool runtime_backend_pool_warned() @inline {
    return g_jit_pool_warned;
}

fn void runtime_backend_mark_pool_warned() @inline {
    g_jit_pool_warned = true;
}

fn void runtime_backend_clear_pool_warned() @inline {
    g_jit_pool_warned = false;
}

fn void runtime_register_attached_interp(Interp* interp) {
    assert(interp != null, "runtime_register_attached_interp: null interp");
    if (runtime_interp_is_attached(interp)) return;
    ulong serial = runtime_backend_next_attach_serial();
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == null) {
            runtime_backend_store_attached_interp(i, interp, serial, 0);
            return;
        }
    }
    unreachable("runtime attached interp table full");
}

fn void runtime_unregister_attached_interp(Interp* interp) {
    assert(interp != null, "runtime_unregister_attached_interp: null interp");
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == interp) {
            runtime_backend_clear_attached_interp(i);
            return;
        }
    }
}

fn void runtime_attach_interp(Interp* interp) {
    jit_require_owner_thread("runtime_attach_interp");
    runtime_register_attached_interp(interp);
    runtime_backend_increment_interp_refcount();
}

fn void runtime_detach_interp(Interp* interp) {
    jit_require_owner_thread("runtime_detach_interp");
    ulong serial = 0;
    bool has_serial = runtime_attached_interp_serial(interp, &serial);
    if (runtime_interp_on_current_stack(interp)) {
        assert(!runtime_interp_has_active_exec(interp), "runtime_detach_interp: interp has active jit frames on current stack");
    }
    assert(runtime_backend_interp_refcount() > 0, "jit interp refcount underflow");
    runtime_backend_decrement_interp_refcount();
    runtime_unregister_attached_interp(interp);
    if (has_serial) {
        runtime_backend_mark_gc_needed();
    }
    if (runtime_backend_interp_refcount() == 0 && !runtime_backend_is_initialized()) {
        runtime_backend_clear_owner_thread_token();
        jit_retired_code_reset();
    }
}

fn void runtime_global_shutdown() @inline {
    jit_global_shutdown();
}

fn void runtime_gc_between_top_level_runs() @inline {
    jit_gc();
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

    if (interp.tco_scope_defer_active) {
        main::StackCtx* defer_ctx = main::g_current_stack_ctx;
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

    *env_io = jit_copy_tco_env_chain_for_recycle(*env_io, interp, cs);
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
