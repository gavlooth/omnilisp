module lisp;

import main;

fn Value* root_store_direct_promote_to_scope(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx(v, interp, COPY_SITE_PROMOTE_TO_ROOT, &copy_ctx);
}

fn Value* root_store_route_to_scope(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (v == null || interp == null || target_scope == null) return null;

    switch (v.tag) {
        case PRIMITIVE: {
            Value* cloned = root_store_clone_primitive_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (cloned == null) return raise_error(interp, "root-store: failed to clone primitive payload");
            return cloned;
        }
        case ARRAY: {
            Value* cloned = root_store_clone_array_to_scope(v, interp, target_scope, releasing_scope);
            if (cloned == null) return raise_error(interp, "root-store: failed to clone array payload");
            return cloned;
        }
        case TENSOR: {
            Value* cloned = root_store_clone_tensor_to_scope(v, interp, target_scope, releasing_scope);
            if (cloned == null) return raise_error(interp, "root-store: failed to clone tensor payload");
            return cloned;
        }
        case HASHMAP:
        case SET: {
            Value* cloned = root_store_clone_hashmap_to_scope(v, interp, target_scope, releasing_scope);
            if (cloned == null) return raise_error(interp, "root-store: failed to clone hashmap payload");
            return cloned;
        }
        case METHOD_TABLE: {
            Value* cloned = root_store_clone_method_table_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (cloned == null) return raise_error(interp, "root-store: failed to clone method-table payload");
            return cloned;
        }
        case NIL:
        case VOID:
        case INT:
        case STRING:
        case SYMBOL:
        case CONS:
        case CLOSURE:
        case CONTINUATION:
        case PARTIAL_PRIM:
        case ERROR:
        case DOUBLE:
        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TIME_POINT: {
            Value* promoted = root_store_direct_promote_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (promoted != null) return promoted;
            return raise_error(interp, "root-store: destination promotion returned null");
        }
    }
}

// Boundary helper: promote a child-scope value into root using explicit
// transfer-safe routing:
// - releasing-owned + transfer-safe tags -> direct destination ESCAPE promotion
// - unsupported payload classes -> deterministic hard error
fn Value* promote_to_root_site(Value* v, Interp* interp, uint site) {
    if (v == null || interp == null || interp.current_scope == interp.root_scope) return v;
    (void)site;
    main::ScopeRegion* target_scope = interp.root_scope;
    main::ScopeRegion* releasing_scope = interp.current_scope;
    BoundaryReturnClassification c =
        boundary_classify_return_value(v, interp, target_scope, releasing_scope);
    switch (c.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            return v;
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP:
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE:
        case BOUNDARY_RETURN_MIXED_UNCERTAIN:
            return root_store_route_to_scope(v, interp, target_scope, releasing_scope);
    }
}

/**
 * Promote a value to root_scope so it survives child scope release.
 * No-op when already in root_scope (fast path for stdlib loading, etc.).
 * Used when storing values into root-scope data structures (hashmap, array).
 */
fn Value* promote_to_root(Value* v, Interp* interp) {
    return promote_to_root_site(v, interp, COPY_SITE_PROMOTE_TO_ROOT);
}
