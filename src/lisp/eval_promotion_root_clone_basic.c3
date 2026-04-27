module lisp;

import std::core::mem;
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

    Primitive* src_prim = src.prim_val;
    if (src_prim == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) return null;
        result.tag = PRIMITIVE;
        result.prim_val = null;
        if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
            return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register primitive clone destructor");
        }
        return result;
    }
    if (primitive_has_opaque_user_data(src)) {
        return runtime_raise(interp, "runtime/evaluation-error", "root-store: refusing primitive clone with opaque user_data");
    }

    Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    if (dst_prim == null) return null;

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        mem::free(dst_prim);
        return null;
    }
    result.tag = PRIMITIVE;
    result.prim_val = null;
    if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
        mem::free(dst_prim);
        return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register primitive clone destructor");
    }
    if (!primitive_clone_plain_header(dst_prim, src_prim)) {
        primitive_destroy_payload(dst_prim);
        return runtime_raise(interp, "runtime/evaluation-error", "root-store: refusing primitive clone with opaque user_data");
    }
    result.prim_val = dst_prim;
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

    Array* src_arr = src.array_val;
    if (src_arr == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) return null;
        result.tag = ARRAY;
        result.array_val = null;
        if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
            return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register array clone destructor");
        }
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) return null;

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    if (cap > usz.max / Value*.sizeof) {
        mem::free(dst_arr);
        return null;
    }
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        mem::free(dst_arr);
        return null;
    }

    for (usz i = 0; i < src_arr.length; i++) {
        BoundaryCopyResult copy_result = boundary_copy_to_parent_site_ctx_checked(
            src_arr.items[i],
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        Value* copied = null;
        if (!boundary_copy_result_ok(copy_result)) {
            copied = boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(copy_result.fault_code));
        } else {
            copied = copy_result.value;
        }
        if (copied != null && copied.tag == ERROR) {
            boundary_cleanup_partial_array_copy(src_arr, dst_arr, i, target_scope);
            return copied;
        }
        dst_arr.items[i] = copied;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        boundary_cleanup_partial_array_copy(src_arr, dst_arr, src_arr.length, target_scope);
        return null;
    }
    result.tag = ARRAY;
    result.array_val = dst_arr;
    if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
        boundary_cleanup_partial_array_copy(src_arr, dst_arr, src_arr.length, target_scope);
        result.array_val = null;
        return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register array clone destructor");
    }
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

    HashMap* src_map = src.hashmap_val;
    if (src_map == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) return null;
        result.tag = src.tag;
        result.hashmap_val = null;
        if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
            return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register hashmap clone destructor");
        }
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) return null;
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;

    if (src_map.capacity > 0) {
        if ((usz)src_map.capacity > usz.max / HashEntry.sizeof) {
            mem::free(dst_map);
            return null;
        }
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
                BoundaryCopyResult key_copy = boundary_copy_to_parent_site_ctx_checked(
                    src_e.key,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
                Value* copied_key = null;
                if (!boundary_copy_result_ok(key_copy)) {
                    copied_key = boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(key_copy.fault_code));
                } else {
                    copied_key = key_copy.value;
                }
                if (copied_key != null && copied_key.tag == ERROR) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1, target_scope);
                    return copied_key;
                }
                dst_e.key = copied_key;
                BoundaryCopyResult value_copy = boundary_copy_to_parent_site_ctx_checked(
                    src_e.value,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
                Value* copied_value = null;
                if (!boundary_copy_result_ok(value_copy)) {
                    copied_value = boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(value_copy.fault_code));
                } else {
                    copied_value = value_copy.value;
                }
                if (copied_value != null && copied_value.tag == ERROR) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1, target_scope);
                    return copied_value;
                }
                dst_e.value = copied_value;
            }
        }
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        boundary_cleanup_partial_hashmap_copy(src_map, dst_map, src_map.capacity, target_scope);
        return null;
    }
    result.tag = src.tag;
    result.hashmap_val = dst_map;
    if (!main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value)) {
        boundary_cleanup_partial_hashmap_copy(src_map, dst_map, src_map.capacity, target_scope);
        result.hashmap_val = null;
        return runtime_raise(interp, "runtime/out-of-memory", "root-store: failed to register hashmap clone destructor");
    }
    return result;
}
