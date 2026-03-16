module lisp;

import main;

struct ScopedFinalizeResult {
    Value* value;
    bool releasing_scope_spliced;
}

fn bool env_chain_contains_scope_gen(Env* env, uint scope_gen, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (!cur.is_root_persistent_box() && cur.scope_gen == scope_gen) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool is_in_temp_lane(void* ptr, main::ScopeRegion* scope) {
    if (ptr == null || scope == null) return false;
    main::ScopeChunk* chunk = scope.chunks;
    while (chunk != null) {
        char* data = (char*)chunk + main::ScopeChunk.sizeof;
        if ((char*)ptr >= data && (char*)ptr < data + chunk.capacity) return true;
        chunk = chunk.next;
    }
    return false;
}

fn bool env_chain_has_binding_in_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* v = cur.bindings[i].value;
            if (v != null && is_in_temp_lane((void*)v, scope)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_contains_root_persistent_box(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (cur.is_root_persistent_box()) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool jit_value_is_graph_carrying(Value* v) @inline {
    if (v == null) return false;
    switch (v.tag) {
        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
        case CLOSURE:
        case MODULE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
        case ITERATOR:
        case INSTANCE:
            return true;
        default:
            return false;
    }
}

fn bool env_chain_has_graph_binding_values(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            if (jit_value_is_graph_carrying(cur.bindings[i].value)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool jit_env_parent_needs_rewrite(Env* parent, main::ScopeRegion* releasing_scope) @inline {
    if (parent == null || releasing_scope == null) return false;
    if (parent.scope_gen != releasing_scope.generation &&
        parent.scope_gen != releasing_scope.escape_generation) {
        return false;
    }
    return boundary_ptr_in_scope((void*)parent, releasing_scope);
}

fn bool jit_binding_payload_in_releasing_scope(Value* v, main::ScopeRegion* releasing_scope) @inline {
    if (v == null || releasing_scope == null) return false;
    if (v.tag == CLOSURE) {
        return boundary_closure_payload_in_scope(v, releasing_scope);
    }
    if (v.tag == ITERATOR) {
        Value* payload = v.iterator_val;
        if (payload != null && boundary_ptr_in_scope((void*)payload, releasing_scope)) {
            return true;
        }
        if (payload != null &&
            payload.tag == CLOSURE &&
            boundary_closure_payload_in_scope(payload, releasing_scope)) {
            return true;
        }
    }
    return false;
}

fn bool jit_tco_binding_needs_copy(Value* v, main::ScopeRegion* releasing_scope) @inline {
    if (v == null || releasing_scope == null) return false;
    if (boundary_ptr_in_scope((void*)v, releasing_scope)) return true;
    return jit_binding_payload_in_releasing_scope(v, releasing_scope);
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

fn JitCompiledFn jit_lookup_or_compile(Expr* expr, Interp* interp) {
    JitCompiledFn cached = jit_cache_lookup(expr, interp);
    if (!jit_compiled_fn_is_null(cached)) return cached;
    cached = jit_compile(expr, interp);
    if (jit_compiled_fn_is_null(cached)) return jit_compiled_fn_null();
    jit_cache_store(expr, cached, interp);
    return cached;
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
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* cs = interp.current_scope;
    if (cs != interp.tco_recycle_scope || cs.refcount != 1) return null;

    bool chain_touches_releasing_scope =
        env_chain_contains_scope_gen(*env_io, cs.generation, interp.global_env);
    bool bindings_touch_releasing_scope =
        env_chain_has_binding_in_scope(*env_io, cs, interp.global_env);
    bool has_root_persistent_box =
        env_chain_contains_root_persistent_box(*env_io, interp.global_env);
    bool has_graph_binding_values =
        env_chain_has_graph_binding_values(*env_io, interp.global_env);
    bool allow_fast_reset = !has_root_persistent_box && !has_graph_binding_values;

    if (allow_fast_reset &&
        !chain_touches_releasing_scope &&
        !bindings_touch_releasing_scope) {
        // Fast path: loop env graph is detached from recyclable TEMP lane.
        // Recycle TEMP lane in-place and keep env as-is.
        main::scope_reset_temp_lane(cs);
        jit_set_active_recycle_scope(interp, cs);
        return null;
    }

    // Fallback: allocate fresh call_scope and copy full env chain.
    main::ScopeRegion* fresh = main::scope_create(cs.parent);
    if (fresh == null) {
        interp.jit_env = saved_env;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: failed to allocate TCO recycle scope");
    }
    // Update: fresh is now the recyclable scope.
    jit_set_active_recycle_scope(interp, fresh);
    if (interp.tco_scope_defer_active) {
        main::StackCtx* defer_ctx = main::g_current_stack_ctx;
        if (defer_ctx == null) {
            return jit_tco_recycle_restore_on_error(
                interp,
                cs,
                fresh,
                saved_env,
                "jit: missing active stack context for call-scope defer"
            );
        }
        bool retargeted = main::stack_ctx_defer_update_arg(
            defer_ctx,
            interp.tco_scope_defer_slot,
            (void*)fresh
        );
        if (!retargeted) {
            return jit_tco_recycle_restore_on_error(
                interp,
                cs,
                fresh,
                saved_env,
                "jit: failed to retarget call-scope defer"
            );
        }
    }
    *env_io = jit_copy_tco_env_chain_for_recycle(*env_io, interp, cs);
    main::scope_release(cs);
    boundary_assert_interp_scope_chain(interp);
    return null;
}

// JIT-aware eval replacement with trampoline TCO.
// Compiles sub-expressions with cache, executes with env context.
// When a tail-call helper sets jit_tco_bounce, loops instead of recursing.
<* @require interp != null : "jit_eval: null interp" *>
fn Value* jit_eval(Expr* expr, Env* env, Interp* interp) {
    Env* saved_env = interp.jit_env;

    for (;;) {
        if (expr == null) {
            interp.jit_env = saved_env;
            return make_nil(interp);
        }

        JitCompiledFn cached = jit_lookup_or_compile(expr, interp);
        if (jit_compiled_fn_is_null(cached)) {
            // JIT can't compile this expression
            interp.jit_env = saved_env;
            return raise_error(interp, "JIT compilation failed");
        }

        // Execute with env context.
        interp.jit_env = env;
        interp.flags.jit_tco_bounce = false;
        Value* result = jit_exec_compiled_checked(cached, interp);
        if (is_error(result)) {
            interp.jit_env = saved_env;
            return result;
        }

        // Check for TCO bounce.
        if (interp.flags.jit_tco_bounce) {
            // Tail call — loop with new expr/env instead of recursing
            expr = interp.jit_tco_expr;
            env = interp.jit_tco_env;
            interp.flags.jit_tco_bounce = false;

            // TCO scope recycling: only when current_scope is the scope created by
            // jit_eval_in_call_scope (not a scope from handle clause or other context).
            // Guards: scope is marked recyclable, nothing escaped (RC=1).
            // Phase 4b: copy_tco_env_chain copies the FULL chain to global_env so the
            // fresh scope is independent of any env_scope freed during scope_release.
            Value* recycle_err = jit_prepare_tco_recycle(&env, saved_env, interp);
            if (recycle_err != null) {
                return recycle_err;
            }

            continue;
        }

        interp.jit_env = saved_env;
        return result;
    }
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

    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        interp.current_scope = saved_scope;
        boundary_assert_interp_scope_chain(interp);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }

    boundary_leave_scope(interp, saved_scope);
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
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

    // After jit_eval, current_scope may differ from call_scope (TCO recycling)
    main::ScopeRegion* active_scope = interp.current_scope;
    if (registered_defer && !pop_scope_guard_defer(current_ctx)) {
        jit_restore_call_scope_state(interp, saved_state);
        return raise_error(interp, "jit: call-scope defer stack mismatch");
    }

    // Restore saved state
    jit_restore_call_scope_state(interp, saved_state);
    Value* final = jit_finalize_scoped_result(
        result,
        interp,
        saved_scope,
        active_scope
    );
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
        bool sv_in_releasing_scope = jit_tco_binding_needs_copy(sv, releasing_scope);
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
