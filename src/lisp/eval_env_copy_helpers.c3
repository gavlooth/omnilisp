module lisp;

import main;

struct EnvCopyWorkItem {
    Env* env;
    usz depth;
    int action;
    main::ScopeRegion* alloc_scope;
}

fn bool copy_env_work_push(
    EnvCopyWorkItem** items,
    usz* count,
    usz* capacity,
    Env* env,
    usz depth,
    int action,
    main::ScopeRegion* alloc_scope
) @inline {
    if (count == null || capacity == null) return false;
    if (*count == *capacity) {
        usz new_capacity = *capacity > 0 ? *capacity * 2 : 8;
        if (new_capacity > usz.max / EnvCopyWorkItem.sizeof) return false;
        EnvCopyWorkItem* grown = (EnvCopyWorkItem*)mem::realloc(*items, EnvCopyWorkItem.sizeof * new_capacity);
        if (grown == null) return false;
        *items = grown;
        *capacity = new_capacity;
    }
    (*items)[*count] = {
        .env = env,
        .depth = depth,
        .action = action,
        .alloc_scope = alloc_scope
    };
    (*count)++;
    return true;
}

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
    if (!main::scope_register_dtor(interp.current_scope, (void*)new_env, &scope_dtor_env)) {
        copy_env_rollback_materialized_bindings(env, new_env, target_scope, env.binding_count);
        promotion_context_invalidate(active_ctx);
        copy_env_cleanup_partial_frame(new_env);
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_DTOR_REGISTRATION, depth);
    }
    boundary_value_shape_note_env_copy_frame(depth, env.binding_count);
    return boundary_env_copy_ok(new_env, depth);
}

fn BoundaryEnvCopyResult copy_env_to_scope_inner_checked(
    Env* env,
    Interp* interp,
    usz depth,
    PromotionContext* active_ctx
) {
    if (env == null) return boundary_env_copy_ok(env, depth);
    if (env == interp.global_env) return boundary_env_copy_ok(env, depth);

    EnvCopyWorkItem* items = null;
    usz count = 0;
    usz capacity = 0;
    defer if (items != null) mem::free(items);

    main::ScopeRegion* saved_scope = interp.current_scope;
    defer interp.current_scope = saved_scope;

    Env* cur = env;
    usz cur_depth = depth;

    while (cur != null && cur != interp.global_env) {
        if (!copy_env_work_push(&items, &count, &capacity,
                               cur, cur_depth, 0, interp.current_scope)) {
            return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_BINDING_BUFFER_ALLOC, cur_depth);
        }

        if (cur.is_root_persistent_box()) {
            if (!copy_env_can_rewrite_persistent_box_in_place(cur, interp)) {
                return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_PARENT_COPY, cur_depth);
            }
            if (cur.parent == null) {
                break;
            }
            bool parent_is_persistent_box = cur.parent.is_root_persistent_box();
            if (!parent_is_persistent_box &&
                !copy_env_parent_needs_rewrite(cur.parent, interp.releasing_scope, interp.root_scope)) {
                break;
            }
            items[count - 1].action = 1;
            interp.current_scope = interp.root_scope;
        } else if (copy_env_can_reuse_existing_frame(cur, interp)) {
            break;
        } else {
            items[count - 1].action = 2;
        }

        cur = cur.parent;
        cur_depth++;
    }

    BoundaryEnvCopyResult parent_result = boundary_env_copy_ok(cur, cur_depth);
    if (cur == null) {
        parent_result = boundary_env_copy_ok(null, cur_depth);
    }

    for (usz i = count; i > 0; i--) {
        EnvCopyWorkItem* item = &items[i - 1];
        if (!boundary_env_copy_result_ok(parent_result)) {
            return parent_result;
        }

        switch (item.action) {
            case 0:
                parent_result = boundary_env_copy_ok(item.env, item.depth);
                break;
            case 1:
                item.env.parent = parent_result.env;
                parent_result = boundary_env_copy_ok(item.env, item.depth);
                break;
            case 2:
                interp.current_scope = item.alloc_scope;
                BoundaryEnvCopyResult alloc_result = copy_env_alloc_frame(item.env, interp, item.depth);
                if (!boundary_env_copy_result_ok(alloc_result)) {
                    promotion_context_invalidate(active_ctx);
                    parent_result = alloc_result;
                    break;
                }
                Env* new_env = alloc_result.env;
                if (!copy_env_copy_bindings(item.env, new_env, interp, active_ctx)) {
                    promotion_context_invalidate(active_ctx);
                    copy_env_cleanup_partial_frame(new_env);
                    parent_result = boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY, item.depth);
                    break;
                }
                new_env.parent = parent_result.env;
                if (item.env.hash_table != null) {
                    new_env.build_hash_table();
                }
                if (!main::scope_register_dtor(item.alloc_scope, (void*)new_env, &scope_dtor_env)) {
                    copy_env_rollback_materialized_bindings(item.env, new_env, item.alloc_scope, item.env.binding_count);
                    promotion_context_invalidate(active_ctx);
                    copy_env_cleanup_partial_frame(new_env);
                    parent_result = boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_DTOR_REGISTRATION, item.depth);
                    break;
                }
                boundary_value_shape_note_env_copy_frame(item.depth, item.env.binding_count);
                parent_result = boundary_env_copy_ok(new_env, item.depth);
                break;
        }
    }

    return parent_result;
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
