module lisp;

import main;

bool g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;

fn Value* promote_escape_string_or_error(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    usz len = v.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    char* chars = g_boundary_force_escape_string_or_error_promotion_alloc_fail
        ? null
        : (char*)mem::malloc(cap);
    if (chars == null) return v;

    Value* result = interp.alloc_value_escape();
    result.tag = v.tag;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    result.str_chars = chars;
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = v.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    return result;
}

fn Value* promote_escape_partial_prim(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = v.partial_val.func;
    result.partial_val.remaining = v.partial_val.remaining;
    result.partial_val.first_arg = promote_to_escape(v.partial_val.first_arg, interp);
    result.partial_val.second_arg = promote_to_escape(v.partial_val.second_arg, interp);
    return result;
}

fn Value* promote_escape_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    *result = *v;
    result.scope_gen = interp.current_scope.escape_generation;
    return result;
}

fn Value* promote_escape_instance(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = INSTANCE;
    result.scope_gen = interp.current_scope.escape_generation;
    result.instance_val = v.instance_val;
    instance_retain(v.instance_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_ffi_handle(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = FFI_HANDLE;
    result.ffi_val = v.ffi_val;
    ffi_handle_retain(result.ffi_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_scalar(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
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
    if (v.cont_val != null) {
        if (v.cont_val.owner_interp != null && v.cont_val.owner_interp != interp) {
            return raise_error(interp, "boundary: refusing continuation alias across interpreters");
        }
        continuation_retain_handle_state_for_boundary(v.cont_val);
    }
    Value* result = interp.alloc_value_escape();
    result.tag = CONTINUATION;
    result.cont_val = v.cont_val;
    return result;
}

fn Value* promote_escape_iterator(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = promote_to_escape(v.iterator_val, interp);
    return result;
}

fn Value* promote_escape_coroutine(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    if (!boundary_ptr_in_scope((void*)v, interp.current_scope) ||
        v.scope_gen != interp.current_scope.generation) {
        return raise_error(interp, "boundary: refusing coroutine wrapper alias during escape promotion");
    }
    Value* result = interp.alloc_value_escape();
    result.tag = COROUTINE;
    result.coroutine_val = v.coroutine_val;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    v.coroutine_val = null;
    return result;
}

fn Value* promote_escape_time_point(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}
