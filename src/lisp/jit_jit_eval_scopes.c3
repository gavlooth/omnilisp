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

// JIT-aware eval replacement with trampoline TCO.
// Compiles sub-expressions with cache, executes with env context.
// When a tail-call helper sets jit_tco_bounce, loops instead of recursing.
<* @require interp != null : "jit_eval: null interp" *>
fn Value* jit_eval(Expr* expr, Env* env, Interp* interp) {
    return runtime_eval_expr(expr, env, interp);
}

fn bool jit_tco_scope_trace_enabled() @inline {
    return runtime_tco_scope_trace_enabled();
}

// Single-scope wrapper: evaluates closure body in ONE child scope.
// Used for non-loop function calls. No TCO recycle scope is set.
// Avoids the overhead of two scopes (result_scope + call_scope).
fn Value* jit_eval_in_single_scope(Expr* body, Env* call_env, Interp* interp) {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = null;
    main::ScopeRegion* call_scope = boundary_push_child_scope(interp, &saved_scope);
    if (call_scope == null) {
        return raise_error(interp, "jit: failed to allocate call scope");
    }
    boundary_assert_interp_scope_chain(interp);

    main::StackCtx* current_ctx = main::g_current_stack_ctx;
    bool registered_defer = false;
    if (current_ctx != null) {
        main::StackCtxDeferOps ops;
        init_scope_guard_defer_ops(&ops);
        registered_defer = main::stack_ctx_defer(current_ctx, &ops, (void*)call_scope);
        if (!registered_defer) {
            boundary_pop_child_scope(interp, saved_scope, call_scope);
            return raise_error(interp, "jit: failed to register call-scope cleanup");
        }
    }

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        main::ScopeRegion* trace_scope = interp.current_scope;
        main::ScopeRegion* trace_recycle = interp.tco_recycle_scope;
        main::ScopeRegion* trace_saved = saved_scope;
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
            trace_scope != null ? (int)trace_scope.generation : -1,
            trace_recycle != null ? (int)trace_recycle.generation : -1,
            trace_saved != null ? (int)trace_saved.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1,
            result != null ? (int)result.tag : -1
        );
    }

    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        interp.current_scope = saved_scope;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-pop-defer active_gen=%d saved_gen=%d call_gen=%d",
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1
        );
    }

    boundary_leave_scope(interp, saved_scope);
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-leave current_gen=%d active_gen=%d saved_gen=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1
        );
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=after-finalize current_gen=%d final_tag=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            final != null ? (int)final.tag : -1
        );
    }
    boundary_assert_interp_scope_chain(interp);
    return final;
}

// Scope-per-call wrapper: evaluates closure body in a child scope.
// Unified lane model: one scope per call with TEMP+ESCAPE lanes.
fn Value* jit_eval_in_call_scope(Expr* body, Env* call_env, Interp* interp, bool enable_escape_env) {
    boundary_assert_interp_scope_chain(interp);
    JitCallScopeState saved_state = jit_save_call_scope_state(interp);
    main::ScopeRegion* saved_scope = null;

    // Unified one-scope lane model: TEMP recycles, ESCAPE splices.
    main::ScopeRegion* call_scope = boundary_push_child_scope(interp, &saved_scope);
    if (call_scope == null) {
        return raise_error(interp, "jit: failed to allocate call scope");
    }
    boundary_assert_interp_scope_chain(interp);

    if (enable_escape_env) {
        interp.escape_env_mode = true;
    }

    main::StackCtx* current_ctx = main::g_current_stack_ctx;
    bool registered_defer = false;
    usz defer_slot = 0;
    if (current_ctx != null) {
        main::StackCtxDeferOps ops;
        init_scope_guard_defer_ops(&ops);
        defer_slot = current_ctx.defer_count;
        registered_defer = main::stack_ctx_defer(current_ctx, &ops, (void*)call_scope);
        if (!registered_defer) {
            boundary_pop_child_scope(interp, saved_scope, call_scope);
            jit_restore_call_scope_state(interp, saved_state);
            return raise_error(interp, "jit: failed to register call-scope cleanup");
        }
    }

    // Mark call_scope as eligible for TCO recycling.
    jit_activate_call_scope_recycle(interp, call_scope, defer_slot, registered_defer);

    Value* result = jit_eval(body, call_env, interp);
    if (jit_tco_scope_trace_enabled()) {
        if (result != null && result.tag == ERROR) {
            io::printfn(
                "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d error=%s",
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                interp.tco_recycle_scope != null ? (int)interp.tco_recycle_scope.generation : -1,
                saved_scope != null ? (int)saved_scope.generation : -1,
                call_scope != null ? (int)call_scope.generation : -1,
                (int)result.tag,
                (ZString)result.str_chars
            );
        } else {
            io::printfn(
                "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                interp.tco_recycle_scope != null ? (int)interp.tco_recycle_scope.generation : -1,
                saved_scope != null ? (int)saved_scope.generation : -1,
                call_scope != null ? (int)call_scope.generation : -1,
                result != null ? (int)result.tag : -1
            );
        }
    }

    // After jit_eval, current_scope may differ from call_scope (TCO recycling)
    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        jit_restore_call_scope_state(interp, saved_state);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-pop-defer active_gen=%d saved_gen=%d call_gen=%d",
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1,
            call_scope != null ? (int)call_scope.generation : -1
        );
    }

    // Restore saved state
    jit_restore_call_scope_state(interp, saved_state);
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-restore current_gen=%d active_gen=%d saved_gen=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            active_scope != null ? (int)active_scope.generation : -1,
            saved_scope != null ? (int)saved_scope.generation : -1
        );
    }
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
    if (jit_tco_scope_trace_enabled()) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-finalize current_gen=%d final_tag=%d",
            interp.current_scope != null ? (int)interp.current_scope.generation : -1,
            final != null ? (int)final.tag : -1
        );
    }
    boundary_assert_interp_scope_chain(interp);
    return final;
}

<*
@require interp != null : "jit_scopes_copy_value_into_scope_site: null interp"
@require target_scope != null : "jit_scopes_copy_value_into_scope_site: null target_scope"
@require releasing_scope != null : "jit_scopes_copy_value_into_scope_site: null releasing_scope"
*>
fn Value* jit_scopes_copy_value_into_scope_site(
    Value* value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint site
) {
    if (value == null || target_scope == null) return value;
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx(value, interp, site, &copy_ctx);
}

/// Copy env frames from `src` up to (but not including) `stop_at` into current_scope.
/// Copy env frames from `src` up to (but not including) `global_env` into current_scope.
/// Used at TCO bounce to preserve the next iteration's env while releasing old scope.
/// Phase 4b: copies the FULL chain (including env_scope frames from closures) so the
/// fresh scope is independent of any env_scope that might be freed during scope_release.
fn Env* copy_tco_env_chain(Env* src, Interp* interp) {
    if (src == null || src == interp.global_env) return src;

    // Root-persistent mutable-box env nodes are kept as-is.
    // Their parent chain still needs fixing up when it points at releasing-scope frames.
    // Rewrites must not tie parent lifetime to transient target scopes.
    if (src.is_root_persistent_box()) {
        if (!jit_env_parent_needs_rewrite(src.parent, interp.releasing_scope)) {
            return src;
        }

        main::ScopeRegion* saved_scope = interp.current_scope;
        interp.current_scope = interp.root_scope;
        defer interp.current_scope = saved_scope;

        src.parent = copy_tco_env_chain(src.parent, interp);
        return src;
    }

    // Recursive: copy parent chain first (bottom-up)
    Env* parent_copy = copy_tco_env_chain(src.parent, interp);

    // Create new env frame in current_scope (already switched to fresh scope)
    Env* dst = make_env(interp, parent_copy);
    for (usz i = 0; i < src.binding_count; i++) {
        Value* sv = src.bindings[i].value;
        Value* v = sv;
        main::ScopeRegion* releasing_scope = interp.releasing_scope;
        bool sv_in_releasing_scope = jit_tco_binding_needs_copy(sv, interp, releasing_scope);
        if (sv_in_releasing_scope) {
            v = jit_scopes_copy_value_into_scope_site(
                sv,
                interp,
                interp.current_scope,
                releasing_scope,
                COPY_SITE_JIT_TCO_FRAME_COPY
            );
        }
        env_define_with_barrier(dst, src.bindings[i].name, v, interp);
    }
    // Rebuild hash table if source had one
    if (src.hash_table != null) {
        dst.build_hash_table();
    }
    return dst;
}
