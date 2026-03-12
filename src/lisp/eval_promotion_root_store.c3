module lisp;

import main;

fn Value* root_store_clone_primitive_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* result = interp.alloc_value_escape();
    result.tag = PRIMITIVE;
    result.prim_val = null;

    Primitive* src_prim = src.prim_val;
    if (src_prim == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    if (dst_prim == null) return null;
    *dst_prim = *src_prim;

    result.prim_val = dst_prim;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_array_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = ARRAY;
    result.array_val = null;

    Array* src_arr = src.array_val;
    if (src_arr == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) return null;

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        mem::free(dst_arr);
        return null;
    }

    for (usz i = 0; i < src_arr.length; i++) {
        dst_arr.items[i] = boundary_copy_to_parent_site_ctx(
            src_arr.items[i],
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
    }

    result.array_val = dst_arr;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_hashmap_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = src.tag;
    result.hashmap_val = null;

    HashMap* src_map = src.hashmap_val;
    if (src_map == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) return null;
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;

    if (src_map.capacity > 0) {
        dst_map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_map.capacity);
        if (dst_map.entries == null) {
            mem::free(dst_map);
            return null;
        }

        for (uint i = 0; i < src_map.capacity; i++) {
            HashEntry* src_e = &src_map.entries[i];
            HashEntry* dst_e = &dst_map.entries[i];
            dst_e.key = null;
            dst_e.value = null;
            if (src_e.key != null) {
                dst_e.key = boundary_copy_to_parent_site_ctx(
                    src_e.key,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
                dst_e.value = boundary_copy_to_parent_site_ctx(
                    src_e.value,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
            }
        }
    }

    result.hashmap_val = dst_map;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn void root_store_free_method_table_partial(MethodTable* mt, usz copied_entries) {
    if (mt == null) return;
    if (mt.entries != null) {
        usz limit = copied_entries;
        if (limit > mt.entry_count) limit = mt.entry_count;
        for (usz i = 0; i < limit; i++) {
            method_signature_destroy_heap(&mt.entries[i].sig);
        }
        mem::free(mt.entries);
        mt.entries = null;
    }
    mem::free(mt);
}

fn Value* root_store_clone_method_table_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = METHOD_TABLE;
    result.method_table_val = null;

    MethodTable* src_mt = src.method_table_val;
    if (src_mt == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) return null;
    dst_mt.name = src_mt.name;
    dst_mt.entry_count = src_mt.entry_count;
    dst_mt.capacity = src_mt.capacity;
    if (dst_mt.capacity < dst_mt.entry_count) dst_mt.capacity = dst_mt.entry_count;
    dst_mt.entries = null;
    dst_mt.fallback = null;

    if (dst_mt.capacity > 0) {
        dst_mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * dst_mt.capacity);
        if (dst_mt.entries == null) {
            mem::free(dst_mt);
            return null;
        }
    }

    usz copied_entries = 0;
    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            root_store_free_method_table_partial(dst_mt, copied_entries);
            return null;
        }
        dst_mt.entries[i].implementation = boundary_copy_to_parent_site_ctx(
            src_mt.entries[i].implementation,
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        copied_entries++;
    }

    dst_mt.fallback = boundary_copy_to_parent_site_ctx(
        src_mt.fallback,
        interp,
        COPY_SITE_PROMOTE_TO_ROOT,
        &copy_ctx
    );

    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

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
