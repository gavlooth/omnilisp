module lisp;

import main;

fn Value* ml_optimizer_vulkan_tensor_value(TensorVal* source, void* device_handle, Interp* interp, char[] metadata_label, char[] value_label) {
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, source.shape[0:source.rank], false, false, metadata_label);
    if (tensor == null) {
        if (device_handle != null) omni_tensor_backend_vulkan_free(device_handle);
        return tensor_runtime_raise(interp, metadata_label);
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = device_handle;
    tensor.device_finalizer = device_handle != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, value_label);
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn bool ml_optimizer_all_vulkan_sgd_operands(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (!tensor_expr_has_device(params, TENSOR_DEVICE_VULKAN) || !tensor_expr_has_device(grads, TENSOR_DEVICE_VULKAN)) return false;
    if (has_velocity && !tensor_expr_has_device(velocity, TENSOR_DEVICE_VULKAN)) return false;
    return true;
}

fn bool ml_optimizer_any_non_vulkan_sgd_operand(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (tensor_expr_has_non_vulkan_device(params) || tensor_expr_has_non_vulkan_device(grads)) return true;
    return has_velocity && tensor_expr_has_non_vulkan_device(velocity);
}

fn Value* ml_optimizer_step_tensor_try_vulkan(
    Value* params_arg,
    Value* grads_arg,
    Value* velocity_arg,
    double learning_rate,
    double momentum,
    double weight_decay,
    Interp* interp,
    Value** out_params,
    Value** out_velocity,
    bool* out_attempted
) {
    if (out_attempted != null) *out_attempted = false;
    bool has_velocity = momentum > 0.0 && velocity_arg != null && !is_nil(velocity_arg);
    bool touches_vulkan = tensor_expr_has_device(params_arg, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_VULKAN) ||
        (has_velocity && tensor_expr_has_device(velocity_arg, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;
    if (out_attempted != null) *out_attempted = true;

    if (!ml_optimizer_all_vulkan_sgd_operands(params_arg, grads_arg, velocity_arg, has_velocity) ||
        ml_optimizer_any_non_vulkan_sgd_operand(params_arg, grads_arg, velocity_arg, has_velocity)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan SGD operands must remain Vulkan-placed", null);
    }
    if (has_velocity && !is_tensor(velocity_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: velocity leaves must be tensors");

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* velocity = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* velocity_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_concrete_any_device(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_velocity) {
        err = tensor_expr_resolve_concrete_any_device(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);

    if (params.device != TENSOR_DEVICE_VULKAN || grads.device != TENSOR_DEVICE_VULKAN || (has_velocity && velocity.device != TENSOR_DEVICE_VULKAN)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan SGD operands must realize to Vulkan storage", null);
    }
    if (params.dtype != grads.dtype || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and velocity dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan SGD currently supports Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and velocity shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan SGD currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_VULKAN) ||
        (has_velocity && !tensor_has_valid_device_storage(velocity, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: Vulkan tensor backing storage is missing");
    }

    void* out_params_device = null;
    void* out_velocity_device = null;
    int status = omni_tensor_backend_vulkan_ml_sgd_f32(
        params.device_handle,
        params.byte_len,
        grads.device_handle,
        grads.byte_len,
        has_velocity ? velocity.device_handle : null,
        has_velocity ? velocity.byte_len : 0,
        params.element_count,
        (float)learning_rate,
        (float)momentum,
        (float)weight_decay,
        has_velocity ? 1 : 0,
        &out_params_device,
        &out_velocity_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_params_device != null) omni_tensor_backend_vulkan_free(out_params_device);
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        return tensor_vulkan_status_raise(interp, status, "ml/optimizer-step: Vulkan SGD kernel failed");
    }

    Value* updated = ml_optimizer_vulkan_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate Vulkan updated tensor metadata", "ml/optimizer-step: failed to allocate Vulkan updated tensor value");
    if (updated == null || updated.tag == ERROR) {
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        return updated;
    }
    *out_params = updated;
    if (momentum <= 0.0) {
        *out_velocity = null;
        return null;
    }
    Value* next_velocity = ml_optimizer_vulkan_tensor_value(params, out_velocity_device, interp, "ml/optimizer-step: failed to allocate Vulkan velocity tensor metadata", "ml/optimizer-step: failed to allocate Vulkan velocity tensor value");
    if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
    *out_velocity = next_velocity;
    return null;
}
