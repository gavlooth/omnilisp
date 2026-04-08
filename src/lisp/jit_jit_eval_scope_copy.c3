module lisp;

import main;

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
