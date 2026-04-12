module lisp;

import std::core::mem;
import main;

bool g_boundary_force_copy_continuation_wrapper_alloc_fail = false;
bool g_boundary_force_copy_time_point_wrapper_alloc_fail = false;

fn Value* copy_partial_prim_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    Value* first_arg = copy_to_parent_with_fault(v.partial_val.first_arg, interp, fault_out);
    if (v.partial_val.first_arg != null && (first_arg == null || is_error(first_arg))) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (first_arg == null
                ? raise_error(interp, "boundary: failed to copy partial primitive first arg")
                : first_arg);
    }

    Value* second_arg = copy_to_parent_with_fault(v.partial_val.second_arg, interp, fault_out);
    if (v.partial_val.second_arg != null && (second_arg == null || is_error(second_arg))) {
        boundary_cleanup_materialized_value(
            v.partial_val.first_arg,
            first_arg,
            interp.current_scope
        );
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (second_arg == null
                ? raise_error(interp, "boundary: failed to copy partial primitive second arg")
                : second_arg);
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        boundary_cleanup_materialized_value(
            v.partial_val.first_arg,
            first_arg,
            interp.current_scope
        );
        if (v.partial_val.second_arg != v.partial_val.first_arg ||
            second_arg != first_arg) {
            boundary_cleanup_materialized_value(
                v.partial_val.second_arg,
                second_arg,
                interp.current_scope
            );
        }
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate partial primitive copy value");
    }
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = v.partial_val.func;
    result.partial_val.remaining = v.partial_val.remaining;
    result.partial_val.first_arg = first_arg;
    result.partial_val.second_arg = second_arg;
    return result;
}

fn Value* copy_array_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    Array* src_arr = v.array_val;
    if (src_arr == null) {
        Value* result = interp.alloc_value();
        if (result == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate array copy value");
        }
        result.tag = ARRAY;
        result.array_val = null;
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate array copy");
    }

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    if (cap > usz.max / Value*.sizeof) {
        mem::free(dst_arr);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate array copy");
    }
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        mem::free(dst_arr);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate array copy");
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        boundary_cleanup_partial_array_copy(src_arr, dst_arr, 0, interp.current_scope);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate array copy value");
    }
    for (usz i = 0; i < cap; i++) {
        dst_arr.items[i] = null;
    }

    result.tag = ARRAY;
    result.array_val = dst_arr;
    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) promotion_context_remember(active_ctx, v, result, interp);

    for (usz i = 0; i < src_arr.length; i++) {
        Value* copied = copy_to_parent_with_fault(src_arr.items[i], interp, fault_out);
        if (src_arr.items[i] != null && (copied == null || is_error(copied))) {
            boundary_cleanup_partial_array_copy(src_arr, dst_arr, i, interp.current_scope);
            if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
            if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            }
            return fault_out != null
                ? null
                : (copied == null
                    ? raise_error(interp, "boundary: failed to copy array item")
                    : copied);
        }
        dst_arr.items[i] = copied;
    }

    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_hashmap_backed_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    HashMap* src_map = v.hashmap_val;
    if (src_map == null) {
        Value* result = interp.alloc_value();
        if (result == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate hashmap copy value");
        }
        result.tag = v.tag;
        result.hashmap_val = null;
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate hashmap copy");
    }
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;

    if (src_map.capacity > 0) {
        if ((usz)src_map.capacity > usz.max / HashEntry.sizeof) {
            mem::free(dst_map);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate hashmap copy");
        }
        dst_map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_map.capacity);
        if (dst_map.entries == null) {
            mem::free(dst_map);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate hashmap copy");
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        boundary_cleanup_partial_hashmap_copy(src_map, dst_map, 0, interp.current_scope);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate hashmap copy value");
    }
    if (dst_map.entries != null) {
        for (uint i = 0; i < dst_map.capacity; i++) {
            dst_map.entries[i].key = null;
            dst_map.entries[i].value = null;
        }
    }

    result.tag = v.tag;
    result.hashmap_val = dst_map;
    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) promotion_context_remember(active_ctx, v, result, interp);

    if (src_map.capacity > 0) {
        for (uint i = 0; i < src_map.capacity; i++) {
            HashEntry* src_e = &src_map.entries[i];
            HashEntry* dst_e = &dst_map.entries[i];
            dst_e.key = null;
            dst_e.value = null;
            if (src_e.key != null) {
                Value* copied_key = copy_to_parent_with_fault(src_e.key, interp, fault_out);
                if (copied_key == null || is_error(copied_key)) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1, interp.current_scope);
                    if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
                    if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                        *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
                    }
                    return fault_out != null
                        ? null
                        : (copied_key == null
                            ? raise_error(interp, "boundary: failed to copy hashmap key")
                            : copied_key);
                }
                dst_e.key = copied_key;
                Value* copied_value = copy_to_parent_with_fault(src_e.value, interp, fault_out);
                if (src_e.value != null && (copied_value == null || is_error(copied_value))) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1, interp.current_scope);
                    if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
                    if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                        *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
                    }
                    return fault_out != null
                        ? null
                        : (copied_value == null
                            ? raise_error(interp, "boundary: failed to copy hashmap value")
                            : copied_value);
                }
                dst_e.value = copied_value;
            }
        }
    }

    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_method_table_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    MethodTable* src_mt = v.method_table_val;
    if (src_mt == null) {
        Value* result = interp.alloc_value();
        if (result == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate method table copy value");
        }
        result.tag = METHOD_TABLE;
        result.method_table_val = null;
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate method table copy");
    }
    dst_mt.name = src_mt.name;
    dst_mt.entry_count = src_mt.entry_count;
    dst_mt.capacity = src_mt.capacity;
    if (dst_mt.capacity < dst_mt.entry_count) dst_mt.capacity = dst_mt.entry_count;
    dst_mt.entries = null;
    dst_mt.fallback = null;

    if (dst_mt.capacity > 0) {
        if (dst_mt.capacity > usz.max / MethodEntry.sizeof) {
            mem::free(dst_mt);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate method table copy");
        }
        dst_mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * dst_mt.capacity);
        if (dst_mt.entries == null) {
            mem::free(dst_mt);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate method table copy");
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        boundary_cleanup_partial_method_table_copy(
            src_mt,
            dst_mt,
            0,
            true,
            interp.current_scope
        );
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate method table copy value");
    }
    if (dst_mt.entries != null) {
        for (usz i = 0; i < dst_mt.entry_count; i++) {
            method_signature_init_empty(&dst_mt.entries[i].sig);
            dst_mt.entries[i].implementation = null;
        }
    }

    result.tag = METHOD_TABLE;
    result.method_table_val = dst_mt;
    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) promotion_context_remember(active_ctx, v, result, interp);

    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        dst_mt.entries[i].implementation = null;
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, i, false, interp.current_scope);
            if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to copy method signature");
        }
        Value* copied_impl = copy_to_parent_with_fault(src_mt.entries[i].implementation, interp, fault_out);
        if (src_mt.entries[i].implementation != null && (copied_impl == null || is_error(copied_impl))) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, i, false, interp.current_scope);
            if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
            if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            }
            return fault_out != null
                ? null
                : (copied_impl == null
                    ? raise_error(interp, "boundary: failed to copy method implementation")
                    : copied_impl);
        }
        dst_mt.entries[i].implementation = copied_impl;
    }

    Value* copied_fallback = copy_to_parent_with_fault(src_mt.fallback, interp, fault_out);
    if (src_mt.fallback != null && (copied_fallback == null || is_error(copied_fallback))) {
        boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, dst_mt.entry_count, false, interp.current_scope);
        if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (copied_fallback == null
                ? raise_error(interp, "boundary: failed to copy method fallback")
                : copied_fallback);
    }
    dst_mt.fallback = copied_fallback;

    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_shared_wrapper_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    switch (v.tag) {
        case HASHMAP:
        case SET:
            return copy_hashmap_backed_to_parent(v, interp, fault_out);
        case ARRAY:
            return copy_array_to_parent(v, interp, fault_out);
        case TENSOR:
            return copy_tensor_to_parent(v, interp, fault_out);
        case METHOD_TABLE:
            return copy_method_table_to_parent(v, interp, fault_out);
        default:
            return v;
    }
}

fn Value* copy_continuation_to_parent(Value* v, Interp* interp) {
    bool retained_now = false;
    if (v.cont_val != null) {
        if (v.cont_val.owner_interp != null && v.cont_val.owner_interp != interp) {
            return raise_error(interp, "boundary: refusing continuation alias across interpreters");
        }
        retained_now = continuation_retain_handle_state_for_boundary(v.cont_val);
    }
    Value* result = g_boundary_force_copy_continuation_wrapper_alloc_fail
        ? null
        : interp.alloc_value();
    if (result == null) {
        if (retained_now) {
            continuation_release_handle_state_for_boundary(v.cont_val);
        }
        return raise_error(interp, "boundary: failed to allocate continuation copy value");
    }
    result.tag = CONTINUATION;
    result.cont_val = v.cont_val;
    result.continuation_boundary_owned = retained_now;
    if (retained_now) {
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    }
    return result;
}

fn Value* copy_primitive_to_parent(
    Value* v,
    Interp* interp,
    BoundaryCopyFault* fault_out = null
) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    Primitive* src_prim = v.prim_val;
    if (src_prim == null) {
        Value* result = interp.alloc_value();
        if (result == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate primitive copy value");
        }
        result.tag = PRIMITIVE;
        result.prim_val = null;
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }
    if (primitive_has_opaque_user_data(v)) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        return fault_out != null ? null : raise_error(interp, boundary_copy_fault_message(BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD));
    }

    Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    if (dst_prim == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate primitive copy");
    }
    if (!primitive_clone_plain_header(dst_prim, src_prim)) {
        mem::free(dst_prim);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        return fault_out != null ? null : raise_error(interp, boundary_copy_fault_message(BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD));
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        mem::free(dst_prim);
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate primitive copy value");
    }
    result.tag = PRIMITIVE;
    result.prim_val = null;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    result.prim_val = dst_prim;
    return result;
}

fn Value* copy_iterator_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    Value* promoted_thunk = copy_to_parent_with_fault(v.iterator_val, interp, fault_out);
    if (v.iterator_val != null && (promoted_thunk == null || is_error(promoted_thunk))) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (promoted_thunk == null
                ? raise_error(interp, "boundary: failed to copy iterator thunk")
                : promoted_thunk);
    }
    Value* result = interp.alloc_value();
    if (result == null) {
        boundary_cleanup_materialized_value(
            v.iterator_val,
            promoted_thunk,
            interp.current_scope
        );
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate iterator copy value");
    }
    result.tag = ITERATOR;
    result.iterator_val = promoted_thunk;
    return result;
}

fn CoroutineThunkState* copy_parent_clone_coroutine_state(main::StackCtx* source, Interp* interp) @inline {
    if (source == null || interp == null) return null;
    CoroutineThunkState* state = (CoroutineThunkState*)source.user_data;
    if (state == null || state.interp != interp) return null;
    return coroutine_alloc_thunk_state(state.thunk, interp);
}

fn main::StackCtx* copy_parent_clone_coroutine_ctx(main::StackCtx* source, Interp* interp) {
    if (source == null || interp == null) return null;
    if (source.owner_pool != &interp.stack_ctx_pool) return null;

    switch (source.status) {
        case main::StackCtxStatus.CTX_READY: {
            CoroutineThunkState* cloned_state = copy_parent_clone_coroutine_state(source, interp);
            if (cloned_state == null) return null;

            main::StackCtx* clone = main::stack_ctx_create(&interp.stack_ctx_pool);
            if (clone == null) {
                mem::free(cloned_state);
                return null;
            }

            clone.user_data = cloned_state;
            main::stack_ctx_init(clone, &coroutine_thunk_entry, cloned_state);
            return clone;
        }
        case main::StackCtxStatus.CTX_SUSPENDED: {
            main::StackCtx* clone = main::stack_ctx_clone(source, &interp.stack_ctx_pool);
            if (clone == null) return null;

            CoroutineThunkState* source_state = (CoroutineThunkState*)source.user_data;
            if (source_state != null) {
                CoroutineThunkState* cloned_state = copy_parent_clone_coroutine_state(source, interp);
                if (cloned_state == null) {
                    main::stack_ctx_destroy(clone, &interp.stack_ctx_pool);
                    return null;
                }
                clone.user_data = cloned_state;
            }
            return clone;
        }
        default:
            return null;
    }
}

fn Value* copy_coroutine_to_parent(Value* v, Interp* interp) {
    // Coroutine wrappers own StackCtx teardown. Boundary copies must produce an
    // independent context so source aliases stay semantically unchanged.
    if (!boundary_value_in_releasing_scope(v, interp)) {
        return raise_error(interp, "boundary: refusing coroutine wrapper alias across disjoint scopes");
    }
    main::StackCtx* cloned_ctx = copy_parent_clone_coroutine_ctx(v.coroutine_val, interp);
    if (cloned_ctx == null) {
        return raise_error(interp, "boundary: refusing coroutine wrapper copy without clonable context state");
    }
    Value* result = boundary_alloc_value_in_scope(interp, interp.current_scope, true);
    if (result == null) {
        if (cloned_ctx.user_data != null) {
            mem::free(cloned_ctx.user_data);
            cloned_ctx.user_data = null;
        }
        main::stack_ctx_destroy(cloned_ctx, &interp.stack_ctx_pool);
        return raise_error(interp, "boundary: failed to allocate coroutine copy value");
    }
    result.tag = COROUTINE;
    result.coroutine_val = cloned_ctx;
    return result;
}

fn Value* copy_time_point_to_parent(Value* v, Interp* interp) {
    Value* result = g_boundary_force_copy_time_point_wrapper_alloc_fail
        ? null
        : interp.alloc_value();
    if (result == null) {
        return raise_error(interp, "boundary: failed to allocate time-point copy value");
    }
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}
