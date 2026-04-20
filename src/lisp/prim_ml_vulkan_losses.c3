module lisp;

import main;

fn Value* ml_mean_squared_error_vulkan_status_raise(Interp* interp, int status) {
    if (status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/mean-squared-error: Float32 result is not representable", null);
    }
    return tensor_vulkan_status_raise(interp, status, "ml/mean-squared-error: Vulkan loss kernel failed");
}

fn Value* ml_mean_squared_error_eval_vulkan(
    TensorVal* predictions,
    TensorVal* targets,
    Interp* interp
) {
    if (predictions == null || targets == null) {
        return tensor_runtime_raise(interp, "ml/mean-squared-error: Vulkan tensor metadata is missing");
    }
    if (predictions.dtype != TENSOR_DTYPE_DOUBLE && predictions.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/mean-squared-error: Vulkan currently supports Float64 and Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(predictions, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(targets, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/mean-squared-error: Vulkan tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    TensorVal* tensor = tensor_alloc_payload_base_checked(predictions.dtype, scalar_shape[0:0], false, false, "ml/mean-squared-error: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/mean-squared-error: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = predictions.dtype == TENSOR_DTYPE_DOUBLE
        ? omni_tensor_backend_vulkan_ml_mse_f64(
            predictions.device_handle,
            predictions.byte_len,
            targets.device_handle,
            targets.byte_len,
            predictions.element_count,
            &out_device
        )
        : omni_tensor_backend_vulkan_ml_mse_f32(
            predictions.device_handle,
            predictions.byte_len,
            targets.device_handle,
            targets.byte_len,
            predictions.element_count,
            &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return ml_mean_squared_error_vulkan_status_raise(interp, status);
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/mean-squared-error: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* ml_cross_entropy_vulkan_status_raise(Interp* interp, int status) {
    if (status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/cross-entropy: Float32 result is not representable", null);
    }
    return tensor_vulkan_status_raise(interp, status, "ml/cross-entropy: Vulkan loss kernel failed");
}

fn Value* ml_cross_entropy_eval_vulkan(
    TensorVal* logits,
    TensorVal* targets,
    usz axis,
    Interp* interp
) {
    if (logits == null || targets == null) {
        return tensor_runtime_raise(interp, "ml/cross-entropy: Vulkan tensor metadata is missing");
    }
    if (logits.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/cross-entropy: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(logits, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(targets, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/cross-entropy: Vulkan tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    TensorVal* tensor = tensor_alloc_payload_base_checked(logits.dtype, scalar_shape[0:0], false, false, "ml/cross-entropy: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/cross-entropy: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_cross_entropy_f32(
        logits.device_handle,
        logits.byte_len,
        targets.device_handle,
        targets.byte_len,
        logits.element_count,
        logits.rank,
        axis,
        logits.shape,
        logits.strides,
        targets.strides,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return ml_cross_entropy_vulkan_status_raise(interp, status);
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/cross-entropy: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
