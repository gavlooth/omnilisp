module lisp;

import main;

<*
@require interp != null : "jit_scopes_copy_value_into_scope_site: null interp"
@require target_scope != null : "jit_scopes_copy_value_into_scope_site: null target_scope"
@require releasing_scope != null : "jit_scopes_copy_value_into_scope_site: null releasing_scope"
*>
fn BoundaryCopyResult jit_scopes_copy_value_into_scope_site_checked(
    Value* value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint site
) {
    if (value == null || target_scope == null) return boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx_checked(value, interp, site, &copy_ctx);
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

        Env* copied_parent = copy_tco_env_chain(src.parent, interp);
        if (src.parent != null && src.parent != interp.global_env && copied_parent == null) return null;
        src.parent = copied_parent;
        return src;
    }

    if (copy_env_can_reuse_existing_frame(src, interp)) {
        return src;
    }

    // Recursive: copy parent chain first (bottom-up)
    Env* parent_copy = copy_tco_env_chain(src.parent, interp);
    if (src.parent != null && src.parent != interp.global_env && parent_copy == null) return null;

    // Create new env frame in current_scope (already switched to fresh scope)
    Env* dst = make_env(interp, parent_copy);
    if (dst == null) return null;
    for (usz i = 0; i < src.binding_count; i++) {
        Value* sv = src.bindings[i].value;
        Value* v = sv;
        main::ScopeRegion* releasing_scope = interp.releasing_scope;
        bool sv_in_releasing_scope = jit_tco_binding_needs_copy(sv, interp, releasing_scope);
        if (sv_in_releasing_scope) {
            BoundaryCopyResult copied = jit_scopes_copy_value_into_scope_site_checked(
                sv,
                interp,
                interp.current_scope,
                releasing_scope,
                COPY_SITE_JIT_TCO_FRAME_COPY
            );
            if (!boundary_copy_result_ok(copied) ||
                copied.value == null ||
                copied.value.tag == ERROR) {
                return null;
            }
            v = copied.value;
        }
        Value* bind_err = env_define_with_barrier_result(dst, src.bindings[i].name, v, interp);
        if (bind_err != null) return null;
    }
    // Rebuild hash table if source had one
    if (src.hash_table != null) {
        dst.build_hash_table();
    }
    return dst;
}
