module lisp;

import main;

fn BoundaryEnvCopyResult copy_env_rewrite_persistent_parent(
    Env* env,
    Interp* interp,
    usz depth,
    PromotionContext* active_ctx
) {
    if (!copy_env_can_rewrite_persistent_box_in_place(env, interp)) {
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_PARENT_COPY, depth);
    }

    if (env.parent == null) {
        return boundary_env_copy_ok(env, depth);
    }
    bool parent_is_persistent_box = env.parent.is_root_persistent_box();
    if (!parent_is_persistent_box &&
        !copy_env_parent_needs_rewrite(env.parent, interp.releasing_scope, interp.root_scope)) {
        return boundary_env_copy_ok(env, depth);
    }

    // Root-persistent mutable boxes keep identity, but parent rewrites must avoid
    // borrowing lifetime from an ephemeral target scope.
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;

    BoundaryEnvCopyResult parent_result = copy_env_to_scope_inner_checked(env.parent, interp, depth + 1, active_ctx);
    if (!boundary_env_copy_result_ok(parent_result)) {
        promotion_context_invalidate(active_ctx);
        return parent_result;
    }
    env.parent = parent_result.env;
    return boundary_env_copy_ok(env, depth);
}

fn BoundaryEnvCopyResult copy_env_materialize_frame(Env* env, Interp* interp, usz depth, PromotionContext* active_ctx) {
    BoundaryEnvCopyResult alloc_result = copy_env_alloc_frame(env, interp, depth);
    if (!boundary_env_copy_result_ok(alloc_result)) return alloc_result;
    Env* new_env = alloc_result.env;
    main::ScopeRegion* target_scope = interp.current_scope;

    if (!copy_env_copy_bindings(env, new_env, interp, active_ctx)) {
        promotion_context_invalidate(active_ctx);
        copy_env_cleanup_partial_frame(new_env);
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY, depth);
    }

    BoundaryEnvCopyResult parent_result = copy_env_to_scope_inner_checked(env.parent, interp, depth + 1, active_ctx);
    if (!boundary_env_copy_result_ok(parent_result)) {
        copy_env_rollback_materialized_bindings(env, new_env, target_scope, env.binding_count);
        promotion_context_invalidate(active_ctx);
        copy_env_cleanup_partial_frame(new_env);
        return parent_result;
    }
    new_env.parent = parent_result.env;
    if (env.hash_table != null) {
        new_env.build_hash_table();
    }
    main::scope_register_dtor(interp.current_scope, (void*)new_env, &scope_dtor_env);
    return boundary_env_copy_ok(new_env, depth);
}

fn BoundaryEnvCopyResult copy_env_to_scope_inner_checked(
    Env* env,
    Interp* interp,
    usz depth,
    PromotionContext* active_ctx
) {
    if (copy_env_is_terminal_frame(env, interp, depth)) {
        if (depth >= 256) {
            return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT, depth);
        }
        return boundary_env_copy_ok(env, depth);
    }

    // Root-persistent mutable-box env nodes are kept as-is to preserve sharing
    // between JIT stack slots and closure captures.
    // Their parent chain still needs fixing up since it may point to child-scope envs.
    if (env.is_root_persistent_box()) {
        return copy_env_rewrite_persistent_parent(env, interp, depth, active_ctx);
    }
    if (copy_env_can_reuse_existing_frame(env, interp)) {
        return boundary_env_copy_ok(env, depth);
    }
    return copy_env_materialize_frame(env, interp, depth, active_ctx);
}

/**
 * Copy an env chain into interp.current_scope (caller must set this to the target scope,
 * typically a closure's env_scope). Uses boundary copy routing for binding values.
 * Stops at global_env (already in root-scope lifetime). Skips root-persistent mutable-box env nodes.
 * Replaces deep_copy_env — allocates into any scope, not just root_scope.
 */
fn BoundaryEnvCopyResult copy_env_to_scope_checked(
    Env* env,
    Interp* interp,
    usz depth = 0,
    PromotionContext* ctx = null
) {
    PromotionContext* saved_active_ctx = interp.active_promotion_ctx;
    PromotionContext* active_ctx = ctx;
    bool owns_ctx = false;
    if (active_ctx == null) active_ctx = interp.active_promotion_ctx;
    if (depth == 0 && active_ctx == null) {
        // Keep boundary context in scope memory (not stack local) so JIT-driven
        // env-copy paths use the same deterministic region lifetime model.
        active_ctx = copy_env_begin_owned_promotion_ctx(interp);
        if (active_ctx == null) {
            return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_PROMOTION_CTX_ALLOC, depth);
        }
        owns_ctx = true;
    }
    if (active_ctx != null && active_ctx.aborted) {
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_CONTEXT_ABORTED, depth);
    }
    defer {
        if (owns_ctx) copy_env_end_owned_promotion_ctx(interp, active_ctx, saved_active_ctx);
    }
    return copy_env_to_scope_inner_checked(env, interp, depth, active_ctx);
}

// Continuations, effects, index — all handled by JIT (jit.c3)

// =============================================================================
