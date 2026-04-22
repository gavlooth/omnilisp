module lisp;

import main;

bool g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
bool g_boundary_force_escape_continuation_wrapper_alloc_fail = false;
bool g_boundary_force_escape_coroutine_wrapper_alloc_fail = false;

fn Value* promote_escape_string_or_error(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    usz len = v.str_len;
    if (len == usz.max) {
        return boundary_raise(interp, "boundary/copy-fault", "boundary: string/error escape copy is too large");
    }
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    char* chars = g_boundary_force_escape_string_or_error_promotion_alloc_fail
        ? null
        : (char*)mem::malloc(cap);
    if (chars == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate string/error escape copy");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        mem::free(chars);
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate string/error escape copy");
    }
    result.tag = v.tag;
    result.str_chars = chars;
    result.string_heap_owned = true;
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = v.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_partial_prim(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* first_arg = promote_to_escape(v.partial_val.first_arg, interp);
    if (v.partial_val.first_arg != null && (first_arg == null || is_error(first_arg))) {
        return first_arg == null
            ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote partial primitive first arg during escape promotion")
            : first_arg;
    }

    Value* second_arg = promote_to_escape(v.partial_val.second_arg, interp);
    if (v.partial_val.second_arg != null && (second_arg == null || is_error(second_arg))) {
        boundary_cleanup_materialized_value(
            v.partial_val.first_arg,
            first_arg,
            interp.current_scope,
            true
        );
        return second_arg == null
            ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote partial primitive second arg during escape promotion")
            : second_arg;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        boundary_cleanup_materialized_value(
            v.partial_val.first_arg,
            first_arg,
            interp.current_scope,
            true
        );
        if (v.partial_val.second_arg != v.partial_val.first_arg ||
            second_arg != first_arg) {
            boundary_cleanup_materialized_value(
                v.partial_val.second_arg,
                second_arg,
                interp.current_scope,
                true
            );
        }
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate partial primitive escape copy");
    }
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = v.partial_val.func;
    result.partial_val.remaining = v.partial_val.remaining;
    result.partial_val.first_arg = first_arg;
    result.partial_val.second_arg = second_arg;
    return result;
}

fn Value* promote_escape_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    if (v.tag == PRIMITIVE) {
        Primitive* src_prim = v.prim_val;
        if (src_prim == null) {
            Value* result = boundary_alloc_value_in_current_escape(interp);
            if (result == null) {
                return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate primitive escape copy");
            }
            result.tag = PRIMITIVE;
            result.prim_val = null;
            main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
            return result;
        }
        if (primitive_has_opaque_user_data(v)) {
            return boundary_raise(interp, "boundary/invalid-state", "boundary: refusing primitive escape promotion with opaque user_data");
        }

        Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (dst_prim == null) return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate primitive escape copy");
        if (!primitive_clone_plain_header(dst_prim, src_prim)) {
            mem::free(dst_prim);
            return boundary_raise(interp, "boundary/invalid-state", "boundary: refusing primitive escape promotion with opaque user_data");
        }

        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) {
            mem::free(dst_prim);
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate primitive escape copy");
        }
        result.tag = PRIMITIVE;
        result.prim_val = null;
        result.prim_val = dst_prim;
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }
    switch (v.tag) {
        case HASHMAP:
        case SET:
            return promote_escape_hashmap_backed_shared_wrapper(v, interp, ctx);
        case ARRAY:
            return promote_escape_array_shared_wrapper(v, interp, ctx);
        case TENSOR:
            return promote_escape_tensor_shared_wrapper(v, interp, ctx);
        case METHOD_TABLE:
            return promote_escape_method_table_shared_wrapper(v, interp, ctx);
        default:
            break;
    }
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate shared wrapper escape copy");
    }
    *result = *v;
    result.scope_gen = interp.current_scope.escape_generation;
    return result;
}

fn Value* promote_escape_instance(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate instance escape copy");
    }
    result.tag = INSTANCE;
    result.scope_gen = interp.current_scope.escape_generation;
    result.instance_val = v.instance_val;
    instance_retain(v.instance_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_ffi_handle(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate ffi handle escape copy");
    }
    result.tag = FFI_HANDLE;
    result.ffi_val = v.ffi_val;
    ffi_handle_retain(result.ffi_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_scalar(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate scalar escape copy");
    }
    switch (v.tag) {
        case NIL:
            result.tag = NIL;
        case VOID:
            result.tag = VOID;
        case INT:
            result.tag = INT;
            result.int_val = v.int_val;
        case DOUBLE:
            result.tag = DOUBLE;
            result.double_val = v.double_val;
        case FLOAT32:
            result.tag = FLOAT32;
            result.float32_val = v.float32_val;
        case COMPLEX128:
            result.tag = COMPLEX128;
            result.complex128_val = v.complex128_val;
        case COMPLEX64:
            result.tag = COMPLEX64;
            result.complex64_val = v.complex64_val;
        case SYMBOL:
            result.tag = SYMBOL;
            result.sym_val = v.sym_val;
        default:
            return v;
    }
    return result;
}

fn Value* promote_escape_continuation(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    bool retained_now = false;
    if (v.cont_val != null) {
        if (v.cont_val.owner_interp != null && v.cont_val.owner_interp != interp) {
            return boundary_raise(interp, "boundary/invalid-state", "boundary: refusing continuation alias across interpreters");
        }
        retained_now = continuation_retain_handle_state_for_boundary(v.cont_val);
    }
    Value* result = g_boundary_force_escape_continuation_wrapper_alloc_fail
        ? null
        : boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        if (retained_now) {
            continuation_release_handle_state_for_boundary(v.cont_val);
        }
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate continuation escape copy");
    }
    result.tag = CONTINUATION;
    result.cont_val = v.cont_val;
    result.continuation_boundary_owned = retained_now;
    if (retained_now) {
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    }
    return result;
}

fn Value* promote_escape_iterator(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* promoted_thunk = promote_to_escape(v.iterator_val, interp);
    if (v.iterator_val != null && (promoted_thunk == null || is_error(promoted_thunk))) {
        return promoted_thunk == null
            ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote iterator thunk during escape promotion")
            : promoted_thunk;
    }
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        boundary_cleanup_materialized_value(
            v.iterator_val,
            promoted_thunk,
            interp.current_scope,
            true
        );
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate iterator escape copy");
    }
    result.tag = ITERATOR;
    result.iterator_val = promoted_thunk;
    return result;
}

fn Value* promote_escape_coroutine(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    if (!boundary_ptr_in_scope((void*)v, interp.current_scope) ||
        v.scope_gen != interp.current_scope.generation) {
        return boundary_raise(interp, "boundary/invalid-state", "boundary: refusing coroutine wrapper alias during escape promotion");
    }
    Value* result = g_boundary_force_escape_coroutine_wrapper_alloc_fail
        ? null
        : boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate coroutine escape copy");
    }
    result.tag = COROUTINE;
    result.coroutine_val = v.coroutine_val;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    v.coroutine_val = null;
    return result;
}

fn Value* promote_escape_time_point(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate time point escape copy");
    }
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}
