module lisp;

import main;

bool g_boundary_force_copy_closure_wrapper_alloc_fail = false;
bool g_boundary_force_copy_instance_wrapper_alloc_fail = false;
bool g_boundary_force_copy_ffi_handle_wrapper_alloc_fail = false;

fn Value* copy_cons_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    // Iterative copy to prevent C stack overflow on long lists.
    Value* head = null;
    Value* current_new = null;
    Value* current_old = v;

    while (true) {
        Value* copied_car = copy_to_parent_with_fault(current_old.cons_val.car, interp, fault_out);
        if (current_old.cons_val.car != null && (copied_car == null || is_error(copied_car))) {
            boundary_cleanup_materialized_value(v, head, interp.current_scope);
            if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            }
            return fault_out != null
                ? null
                : (copied_car == null
                    ? raise_error(interp, "boundary: failed to copy cons car")
                    : copied_car);
        }

        if (head == null) {
            head = interp.alloc_value();
            if (head == null) {
                boundary_cleanup_materialized_value(
                    current_old.cons_val.car,
                    copied_car,
                    interp.current_scope
                );
                if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                    *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
                }
                return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate cons copy");
            }
            head.tag = CONS;
            head.cons_val.car = null;
            head.cons_val.cdr = null;
            current_new = head;
        }
        current_new.cons_val.car = copied_car;
        Value* old_cdr = current_old.cons_val.cdr;
        if (old_cdr != null && old_cdr.tag == CONS) {
            if (boundary_can_reuse_value(old_cdr, interp)) {
                // Safe fast-path: tail already lives in a surviving scope chain.
                current_new.cons_val.cdr = old_cdr;
                break;
            }
            // Deep-copy tail iteratively to avoid recursive overflow on long lists.
            Value* next_new = interp.alloc_value();
            if (next_new == null) {
                boundary_cleanup_materialized_value(v, head, interp.current_scope);
                if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                    *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
                }
                return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate cons tail copy");
            }
            next_new.tag = CONS;
            next_new.cons_val.car = null;
            next_new.cons_val.cdr = null;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
        } else {
            Value* copied_cdr = copy_to_parent_with_fault(old_cdr, interp, fault_out);
            if (old_cdr != null && (copied_cdr == null || is_error(copied_cdr))) {
                boundary_cleanup_materialized_value(v, head, interp.current_scope);
                if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
                    *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
                }
                return fault_out != null
                    ? null
                    : (copied_cdr == null
                        ? raise_error(interp, "boundary: failed to copy cons cdr")
                        : copied_cdr);
            }
            current_new.cons_val.cdr = copied_cdr;
            break;
        }
    }
    return head;
}

fn bool copy_parent_should_reuse_closure(Value* v, Interp* interp) @inline {
    if (boundary_closure_alias_unsafe_for_reuse(v, interp)) return false;
    if (boundary_closure_payload_in_scope(v, interp.releasing_scope)) return false;
    return boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
}

fn bool copy_parent_closure_env_needs_detach(Value* src, Interp* interp) @inline {
    if (src == null || interp == null || src.tag != CLOSURE || src.closure_val == null) return false;
    if (src.closure_val.env == null) return false;
    if (interp.releasing_scope != null &&
        env_chain_has_frame_in_scope(src.closure_val.env, interp.releasing_scope, interp.global_env)) {
        return true;
    }
    if (src.closure_val.env_scope != null) {
        return !boundary_ptr_in_scope((void*)src.closure_val.env, src.closure_val.env_scope);
    }
    return false;
}

fn bool copy_parent_clone_closure_payload(
    Value* result,
    Value* src,
    Interp* interp,
    BoundaryCopyFault* fault_out = null
) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;
    if (result == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return false;
    }
    result.tag = CLOSURE;
    result.closure_val = (Closure*)interp.current_scope.alloc(Closure.sizeof);
    if (result.closure_val == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return false;
    }
    *result.closure_val = *src.closure_val;
    result.closure_val.scope_gen = interp.current_scope.generation;

    if (src.closure_val.param_count > 0) {
        if (src.closure_val.param_count > usz.max / SymbolId.sizeof) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return false;
        }
        result.closure_val.params = (SymbolId*)interp.current_scope.alloc(SymbolId.sizeof * src.closure_val.param_count);
        if (result.closure_val.params == null) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return false;
        }
        for (usz i = 0; i < src.closure_val.param_count; i++) {
            result.closure_val.params[i] = src.closure_val.params[i];
        }
    } else {
        result.closure_val.params = null;
    }

    if (src.closure_val.type_sig != null) {
        result.closure_val.type_sig = (MethodSignature*)interp.current_scope.alloc(MethodSignature.sizeof);
        if (result.closure_val.type_sig == null ||
            !method_signature_copy_to_scope(result.closure_val.type_sig, src.closure_val.type_sig, interp.current_scope)) {
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY;
            return false;
        }
    }

    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) promotion_context_remember(active_ctx, src, result, interp);

    if (copy_parent_closure_env_needs_detach(src, interp)) {
        main::ScopeRegion* env_scope = main::scope_create(null);
        if (env_scope == null) {
            if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            return false;
        }

        BoundarySession session = boundary_session_begin(interp);
        defer boundary_session_end(interp, &session);
        interp.current_scope = env_scope;

        BoundaryEnvCopyResult copied = boundary_copy_env_to_scope_checked(src.closure_val.env, interp);
        if (!boundary_env_copy_result_ok(copied) || copied.env == null) {
            if (active_ctx != null) promotion_context_invalidate(active_ctx, PROMOTION_ABORT_BUDGET_EXHAUSTED);
            if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
            main::scope_release(env_scope);
            return false;
        }

        result.closure_val.env = copied.env;
        result.closure_val.env_scope = env_scope;
        return true;
    }

    if (closure_env_scope_is_root_owned(result, interp)) {
        result.closure_val.env_scope = null;
    } else if (result.closure_val.env_scope != null) {
        main::scope_retain(result.closure_val.env_scope);
    }
    return true;
}

fn Value* copy_closure_to_parent(
    Value* v,
    Interp* interp,
    BoundaryCopyFault* fault_out = null
) {
    if (copy_parent_should_reuse_closure(v, interp)) return v;
    Value* result = g_boundary_force_copy_closure_wrapper_alloc_fail
        ? null
        : interp.alloc_value();
    if (!copy_parent_clone_closure_payload(result, v, interp, fault_out)) return null;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_closure);
    return result;
}

fn bool needs_wrapper_copy(Value* v, Interp* interp) @inline {
    return !boundary_can_reuse_value(v, interp);
}

fn bool primitive_has_opaque_user_data(Value* v) @inline {
    return v != null &&
           v.tag == PRIMITIVE &&
           v.prim_val != null &&
           v.prim_val.user_data != null;
}

fn bool primitive_clone_plain_header(Primitive* dst, Primitive* src) @inline {
    if (dst == null || src == null || src.user_data != null) return false;
    *dst = *src;
    return true;
}

fn Value* copy_instance_to_parent(Value* v, Interp* interp) {
    if (!needs_wrapper_copy(v, interp)) return v;
    Value* result = g_boundary_force_copy_instance_wrapper_alloc_fail
        ? null
        : interp.alloc_value();
    if (result == null) return null;
    result.tag = INSTANCE;
    result.instance_val = v.instance_val;
    instance_retain(v.instance_val);
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_ffi_handle_to_parent(Value* v, Interp* interp) {
    if (!needs_wrapper_copy(v, interp)) return v;
    Value* result = g_boundary_force_copy_ffi_handle_wrapper_alloc_fail
        ? null
        : interp.alloc_value();
    if (result == null) return null;
    result.tag = FFI_HANDLE;
    result.ffi_val = v.ffi_val;
    ffi_handle_retain(result.ffi_val);
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn bool copy_to_parent_payload_in_releasing_scope(Value* v, Interp* interp) @inline {
    if (v == null || interp == null) return false;
    main::ScopeRegion* releasing_scope = interp.releasing_scope;
    if (v.tag == CONS) {
        Value* current = v;
        usz depth = 0;
        while (current != null && current.tag == CONS) {
            if (depth++ >= 128) return true;
            if (boundary_value_in_releasing_scope(current.cons_val.car, interp)) return true;
            Value* cdr = current.cons_val.cdr;
            if (cdr == null) return false;
            if (cdr.tag != CONS) {
                return boundary_value_in_releasing_scope(cdr, interp);
            }
            if (boundary_value_in_releasing_scope(cdr, interp)) return true;
            current = cdr;
        }
        return false;
    }
    if (v.tag == CLOSURE) {
        if (boundary_closure_alias_unsafe_for_reuse(v, interp)) return true;
        return boundary_closure_payload_in_scope(v, releasing_scope);
    }
    if (v.tag == PARTIAL_PRIM) {
        return boundary_partial_payload_alias_unsafe_for_reuse(v, interp);
    }
    if (v.tag == ITERATOR) {
        Value* payload = v.iterator_val;
        if (payload != null && boundary_ptr_in_scope((void*)payload, releasing_scope)) {
            return true;
        }
        if (payload != null &&
            payload.tag == CLOSURE &&
            boundary_closure_payload_in_scope(payload, releasing_scope)) {
            return true;
        }
        if (boundary_iterator_payload_alias_unsafe_for_reuse(v, interp)) {
            return true;
        }
    }
    if (v.tag == TENSOR && v.tensor_val != null) {
        TensorVal* tensor = v.tensor_val;
        if (boundary_ptr_in_scope((void*)tensor, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.shape, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.strides, releasing_scope) ||
            boundary_ptr_in_scope(tensor.data, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.contract_left_axes, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.contract_right_axes, releasing_scope)) {
            return true;
        }
        switch (tensor.kind) {
            case TENSOR_PAYLOAD_CONCRETE:
                return false;
            case TENSOR_PAYLOAD_MAP:
                return boundary_value_in_releasing_scope(tensor.map_func, interp) ||
                    boundary_value_in_releasing_scope(tensor.map_left, interp) ||
                    boundary_value_in_releasing_scope(tensor.map_right, interp);
            case TENSOR_PAYLOAD_CONTRACT:
                return boundary_value_in_releasing_scope(tensor.contract_left, interp) ||
                    boundary_value_in_releasing_scope(tensor.contract_right, interp);
            case TENSOR_PAYLOAD_VIEW:
                return boundary_value_in_releasing_scope(tensor.view_source, interp);
        }
    }
    return false;
}

fn Value* copy_to_parent_missing_scope_provenance_error(Interp* interp) @noinline {
    return raise_error(interp, "boundary: copy_to_parent requires target/releasing scope provenance");
}

<*
@require interp != null : "copy_to_parent_try_fast_reuse: null interp"
@require interp.current_scope != null : "copy_to_parent_try_fast_reuse: null current_scope"
@require interp.releasing_scope != null : "copy_to_parent_try_fast_reuse: null releasing_scope"
*>
fn bool copy_to_parent_try_fast_reuse(Value* v, Interp* interp, Value** out) {
    if (v == null) return false;
    // Chunk-transfer/moved wrappers can carry stale scope_gen stamps.
    // Fast-reuse must fail closed whenever wrapper or payload still resides in
    // releasing scope, independent of generation metadata.
    bool wrapper_in_releasing_scope = boundary_ptr_in_scope((void*)v, interp.releasing_scope);
    bool payload_in_releasing_scope = false;
    if (!wrapper_in_releasing_scope) {
        payload_in_releasing_scope = copy_to_parent_payload_in_releasing_scope(v, interp);
    }
    if (wrapper_in_releasing_scope || payload_in_releasing_scope) {
        g_copy_to_parent_stats.defensive_copies++;
        return false;
    }
    BoundaryReturnClassification classification = boundary_classify_return_value(
        v,
        interp,
        interp.current_scope,
        interp.releasing_scope
    );
    if (classification.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN) {
        g_copy_to_parent_stats.fast_reuse_hits++;
        *out = v;
        return true;
    }
    g_copy_to_parent_stats.defensive_copies++;
    return false;
}
