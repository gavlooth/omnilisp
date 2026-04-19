module lisp;

import std::core::mem;
import main;

fn Value* make_tensor_double_uninitialized_checked(Interp* interp, usz[] shape, char[] oom_message) {
    TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape, true, oom_message);
    if (tensor == null) return raise_error(interp, oom_message);

    Value* v = interp.alloc_value();
    if (v == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, oom_message);
    }
    v.tag = TENSOR;
    v.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_tensor_uninitialized_checked(Interp* interp, TensorDType dtype, usz[] shape, char[] oom_message) {
    if (dtype == TENSOR_DTYPE_DOUBLE) return make_tensor_double_uninitialized_checked(interp, shape, oom_message);
    TensorVal* tensor = tensor_alloc_payload_checked(dtype, shape, true, oom_message);
    if (tensor == null) return raise_error(interp, oom_message);

    Value* v = interp.alloc_value();
    if (v == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, oom_message);
    }
    v.tag = TENSOR;
    v.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_tensor_double_uninitialized(Interp* interp, usz[] shape) {
    return make_tensor_double_uninitialized_checked(interp, shape, "Tensor: out of memory");
}

fn Value* copy_tensor_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    TensorVal* dst = tensor_clone_payload(v.tensor_val);
    if (v.tensor_val != null && dst == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate tensor copy");
    }
    if (!tensor_copy_expr_edges_to_parent(dst, v.tensor_val, interp, fault_out)) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope);
        tensor_free_payload(dst);
        return fault_out != null ? null : raise_error(interp, "boundary: failed to copy tensor expression edges");
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope);
        tensor_free_payload(dst);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate tensor copy value");
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_tensor_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    TensorVal* dst = tensor_clone_payload(v.tensor_val);
    if (v.tensor_val != null && dst == null) {
        return raise_error(interp, "boundary: failed to allocate tensor escape copy");
    }
    if (!tensor_promote_expr_edges_to_escape(dst, v.tensor_val, interp)) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope, true);
        tensor_free_payload(dst);
        return raise_error(interp, "boundary: failed to promote tensor expression edges");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope, true);
        tensor_free_payload(dst);
        return raise_error(interp, "boundary: failed to allocate tensor escape copy value");
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_tensor_to_scope(
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

    TensorVal* dst = tensor_clone_payload(src.tensor_val);
    if (src.tensor_val != null && dst == null) return null;
    if (!tensor_promote_expr_edges_to_escape(dst, src.tensor_val, interp)) {
        tensor_cleanup_materialized_expr_edges(src.tensor_val, dst, target_scope, true);
        tensor_free_payload(dst);
        return null;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(src.tensor_val, dst, target_scope, true);
        tensor_free_payload(dst);
        return null;
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}
