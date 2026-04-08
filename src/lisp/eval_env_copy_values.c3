module lisp;

import main;

fn void copy_env_invariant_fail(char[] msg) @noinline {
    boundary_invariant_fail("copy-env", msg);
}

fn bool copy_env_value_copy_failure(Value* src, Value* copied) @inline {
    return src != null && copied == null;
}

fn bool copy_env_binding_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_ENV_COPY_BINDINGS") != null;
}

<*
@require interp != null : "copy_env_begin_owned_promotion_ctx: null interp"
@require interp.current_scope != null : "copy_env_begin_owned_promotion_ctx: null current_scope"
@ensure return == null || interp.active_promotion_ctx == return : "copy_env_begin_owned_promotion_ctx: active context must point at returned ctx"
*>
fn PromotionContext* copy_env_begin_owned_promotion_ctx(Interp* interp) @inline {
    PromotionContext* local_ctx = (PromotionContext*)interp.current_scope.alloc(PromotionContext.sizeof);
    if (local_ctx == null) return null;
    boundary_promotion_context_begin(interp, local_ctx);
    return local_ctx;
}

<*
@require interp != null : "copy_env_end_owned_promotion_ctx: null interp"
@require ctx != null : "copy_env_end_owned_promotion_ctx: null ctx"
@ensure interp.active_promotion_ctx == saved_active_ctx : "copy_env_end_owned_promotion_ctx: active context restore mismatch"
*>
fn void copy_env_end_owned_promotion_ctx(Interp* interp, PromotionContext* ctx, PromotionContext* saved_active_ctx) @inline {
    boundary_promotion_context_end(interp, ctx);
    if (interp.active_promotion_ctx != saved_active_ctx) {
        copy_env_invariant_fail("copy_env_end_owned_promotion_ctx: active context restore mismatch");
    }
}

fn bool copy_env_should_reuse_value(Value* v, Interp* interp) @inline {
    return !boundary_should_copy_env(v, interp);
}

fn BoundaryCopyResult copy_env_copy_by_boundary_policy(Value* v, Interp* interp, PromotionContext* ctx) @inline {
    if (copy_env_should_reuse_value(v, interp)) return boundary_copy_ok(v);
    return boundary_copy_to_parent_site_ctx_checked(v, interp, COPY_SITE_COPY_ENV_TO_SCOPE, ctx);
}

fn Value* copy_env_copy_time_point(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}

fn Value* copy_env_value_fast(Value* v, Interp* interp, PromotionContext* ctx = null) {
    if (v == null) return null;
    switch (v.tag) {
        case NIL:
            return make_nil(interp);
        case VOID:
            return make_void(interp);
        case INT:
            return make_int(interp, v.int_val);
        case DOUBLE:
            return make_double(interp, v.double_val);
        case SYMBOL:
            return make_symbol(interp, v.sym_val);
        case STRING:
            return make_string(interp, v.str_chars[:v.str_len]);
        case ERROR:
            return make_error(interp, v.str_chars[:v.str_len]);
        case PRIMITIVE:
        case HASHMAP:
        case SET:
        case ARRAY:
        case METHOD_TABLE:
        case TYPE_INFO:
        case MODULE:
            // Root-lifetime or external-lifetime objects can be shared.
            return v;
        case TIME_POINT:
            return copy_env_copy_time_point(v, interp);
        case INSTANCE:
        case FFI_HANDLE: {
            BoundaryCopyResult copied = copy_env_copy_by_boundary_policy(v, interp, ctx);
            if (!boundary_copy_result_ok(copied)) return null;
            return copied.value;
        }
        default:
        {
            BoundaryCopyResult copied = copy_env_copy_by_boundary_policy(v, interp, ctx);
            if (!boundary_copy_result_ok(copied)) return null;
            return copied.value;
        }
    }
}

fn bool copy_env_should_clone_closure(Value* val, Interp* interp) @inline {
    if (val == null || val.tag != CLOSURE || val.closure_val == null) return false;
    if (copy_env_should_reuse_value(val, interp)) return false;
    if (val.closure_val.env_scope != null) {
        return val.closure_val.scope_gen != interp.current_scope.generation;
    }
    Env* captured_env = val.closure_val.env;
    if (captured_env == null || captured_env == interp.global_env) return true;
    if (interp.root_scope != null && main::is_in_scope((void*)captured_env, interp.root_scope)) return true;
    return false;
}

fn bool copy_env_invalid_closure_alias(Value* val, Interp* interp) @inline {
    if (val == null || interp == null || val.tag != CLOSURE || val.closure_val == null) return false;
    return boundary_closure_alias_unsafe_for_reuse(val, interp);
}

fn Value* copy_env_clone_closure_if_needed(Value* val, Interp* interp) {
    bool safe_undelimited_clone = copy_env_should_clone_closure(val, interp);
    if (copy_env_invalid_closure_alias(val, interp) && !safe_undelimited_clone) return null;
    if (!copy_env_should_clone_closure(val, interp)) return val;
    Value* result = interp.alloc_value();
    // Reuse the shared closure-clone payload path so env-copy and return-boundary
    // closure relocation keep the same retain/copy semantics.
    if (!copy_parent_clone_closure_payload(result, val, interp, null)) return null;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_closure);
    return result;
}

fn Value* copy_env_clone_iterator_if_needed(Value* val, Interp* interp) {
    if (val == null || val.tag != ITERATOR) return val;
    bool wrapper_in_target = boundary_ptr_in_target_scope_chain_with_hint((void*)val, interp, val.scope_gen);
    bool payload_alias_safe = !boundary_iterator_payload_alias_unsafe_for_reuse(val, interp);
    if (wrapper_in_target && payload_alias_safe) return val;

    Value* inner = val.iterator_val;
    if (inner != null && inner.tag == CLOSURE) {
        if (copy_env_invalid_closure_alias(inner, interp)) return null;
        if (inner.closure_val.env_scope != null) {
            Value* cloned = copy_env_clone_closure_if_needed(inner, interp);
            if (cloned == null || cloned.tag != CLOSURE) {
                copy_env_invariant_fail("copy_env_clone_iterator_if_needed: expected cloned closure payload");
            }
            inner = cloned;
        }
    } else if (inner != null && inner.tag == PARTIAL_PRIM) {
        Value* copied_inner = copy_env_value_fast(inner, interp);
        if (copied_inner == null || copied_inner.tag != PARTIAL_PRIM) {
            return null;
        }
        inner = copied_inner;
    } else if (inner != null && !boundary_ptr_in_target_scope_chain_with_hint((void*)inner, interp, inner.scope_gen)) {
        // Iterator thunks are required to be closure-backed at boundary crossings.
        // Scalar/shared payloads may alias only when they already live in the
        // target scope chain; reject disjoint/transient aliases.
        return null;
    }

    Value* iter_result = interp.alloc_value();
    iter_result.tag = ITERATOR;
    iter_result.iterator_val = inner;
    return iter_result;
}

fn Value* copy_env_binding_value(Value* val, Interp* interp, PromotionContext* ctx) {
    if (val != null && val.tag == CLOSURE) {
        return copy_env_clone_closure_if_needed(val, interp);
    }
    if (val != null && val.tag == ITERATOR) {
        return copy_env_clone_iterator_if_needed(val, interp);
    }
    return copy_env_value_fast(val, interp, ctx);
}
