module lisp;

import main;

fn Value* ml_optimizer_cuda_tensor_value(TensorVal* source, void* device_handle, Interp* interp, char[] metadata_label, char[] value_label) {
    if (source == null || device_handle == null) {
        if (device_handle != null) omni_tensor_backend_cuda_free(device_handle);
        return tensor_runtime_raise(interp, metadata_label);
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, source.shape[0:source.rank], false, false, metadata_label);
    if (tensor == null) {
        omni_tensor_backend_cuda_free(device_handle);
        return tensor_runtime_raise(interp, metadata_label);
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;
    tensor.device_handle = device_handle;
    tensor.device_finalizer = &tensor_cuda_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, value_label);
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}

fn Value* ml_optimizer_cuda_map_f32(
    TensorVal* left,
    double left_scalar,
    TensorVal* right,
    double right_scalar,
    uint op,
    Interp* interp,
    char[] label,
    Value** out_value
) {
    if (out_value == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: CUDA output state is missing");
    *out_value = null;
    TensorVal* source = left != null ? left : right;
    if (source == null) return tensor_runtime_raise(interp, label);
    Value* result = tensor_cuda_map_binary_resolved(
        left,
        left_scalar,
        right,
        right_scalar,
        TENSOR_DTYPE_FLOAT32,
        source.shape[0:source.rank],
        op,
        interp,
        label,
        label,
        "ml/optimizer-step: CUDA optimizer currently supports dense row-major Float32 tensors",
        "ml/optimizer-step: CUDA tensor backing storage is missing",
        "ml/optimizer-step: CUDA optimizer map kernel failed"
    );
    if (result == null || result.tag == ERROR) return result;
    *out_value = result;
    return null;
}

fn Value* ml_optimizer_cuda_mul_scalar_f32(TensorVal* tensor, double scalar, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(tensor, 0.0, null, scalar, 2, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_add_f32(TensorVal* left, TensorVal* right, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(left, 0.0, right, 0.0, 0, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_sub_f32(TensorVal* left, TensorVal* right, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(left, 0.0, right, 0.0, 1, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_mul_f32(TensorVal* left, TensorVal* right, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(left, 0.0, right, 0.0, 2, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_div_f32(TensorVal* left, TensorVal* right, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(left, 0.0, right, 0.0, 3, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_div_scalar_f32(TensorVal* tensor, double scalar, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(tensor, 0.0, null, scalar, 3, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_add_scalar_f32(TensorVal* tensor, double scalar, Interp* interp, char[] label, Value** out_value) {
    return ml_optimizer_cuda_map_f32(tensor, 0.0, null, scalar, 0, interp, label, out_value);
}

fn Value* ml_optimizer_cuda_sqrt_f32(TensorVal* tensor, Interp* interp, char[] label, Value** out_value) {
    if (out_value == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: CUDA output state is missing");
    *out_value = null;
    if (tensor == null) return tensor_runtime_raise(interp, label);
    Value* result = tensor_cuda_map_unary_value(
        tensor,
        2,
        tensor.shape[0:tensor.rank],
        interp,
        label,
        label,
        "ml/optimizer-step: CUDA optimizer currently supports dense row-major Float32 tensors",
        "ml/optimizer-step: CUDA tensor backing storage is missing",
        "ml/optimizer-step: CUDA optimizer sqrt kernel failed"
    );
    if (result == null || result.tag == ERROR) return result;
    *out_value = result;
    return null;
}

fn bool ml_optimizer_all_cuda_sgd_operands(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (!tensor_expr_has_device(params, TENSOR_DEVICE_CUDA) || !tensor_expr_has_device(grads, TENSOR_DEVICE_CUDA)) return false;
    if (has_velocity && !tensor_expr_has_device(velocity, TENSOR_DEVICE_CUDA)) return false;
    return true;
}

fn bool ml_optimizer_any_non_cuda_sgd_operand(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (tensor_expr_has_non_cuda_device(params) || tensor_expr_has_non_cuda_device(grads)) return true;
    return has_velocity && tensor_expr_has_non_cuda_device(velocity);
}

fn bool ml_optimizer_all_cuda_adam_operands(Value* params, Value* grads, Value* first, Value* second, bool has_moments) {
    if (!tensor_expr_has_device(params, TENSOR_DEVICE_CUDA) || !tensor_expr_has_device(grads, TENSOR_DEVICE_CUDA)) return false;
    if (has_moments && (!tensor_expr_has_device(first, TENSOR_DEVICE_CUDA) || !tensor_expr_has_device(second, TENSOR_DEVICE_CUDA))) return false;
    return true;
}

fn bool ml_optimizer_any_non_cuda_adam_operand(Value* params, Value* grads, Value* first, Value* second, bool has_moments) {
    if (tensor_expr_has_non_cuda_device(params) || tensor_expr_has_non_cuda_device(grads)) return true;
    return has_moments && (tensor_expr_has_non_cuda_device(first) || tensor_expr_has_non_cuda_device(second));
}

fn bool ml_optimizer_all_cuda_rmsprop_operands(Value* params, Value* grads, Value* square, Value* velocity, bool has_square, bool has_velocity) {
    if (!tensor_expr_has_device(params, TENSOR_DEVICE_CUDA) || !tensor_expr_has_device(grads, TENSOR_DEVICE_CUDA)) return false;
    if (has_square && !tensor_expr_has_device(square, TENSOR_DEVICE_CUDA)) return false;
    if (has_velocity && !tensor_expr_has_device(velocity, TENSOR_DEVICE_CUDA)) return false;
    return true;
}

fn bool ml_optimizer_any_non_cuda_rmsprop_operand(Value* params, Value* grads, Value* square, Value* velocity, bool has_square, bool has_velocity) {
    if (tensor_expr_has_non_cuda_device(params) || tensor_expr_has_non_cuda_device(grads)) return true;
    if (has_square && tensor_expr_has_non_cuda_device(square)) return true;
    return has_velocity && tensor_expr_has_non_cuda_device(velocity);
}

fn Value* ml_optimizer_cuda_sgd_update_f32(
    TensorVal* params,
    TensorVal* grads,
    TensorVal* velocity,
    bool has_velocity,
    double learning_rate,
    double momentum,
    double weight_decay,
    Interp* interp,
    Value** out_params,
    Value** out_velocity
) {
    if (omni_tensor_backend_cuda_ml_optimizer_available() != 0) {
        void* out_params_device = null;
        void* out_velocity_device = null;
        int status = omni_tensor_backend_cuda_ml_sgd_f32(
            params.device_handle,
            grads.device_handle,
            has_velocity ? velocity.device_handle : null,
            params.byte_len,
            params.element_count,
            (float)learning_rate,
            (float)momentum,
            (float)weight_decay,
            has_velocity ? 1 : 0,
            &out_params_device,
            &out_velocity_device
        );
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "ml/optimizer-step: CUDA fused SGD kernel failed");
        }
        Value* updated = ml_optimizer_cuda_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate CUDA fused SGD parameter metadata", "ml/optimizer-step: failed to allocate CUDA fused SGD parameter value");
        if (updated == null || updated.tag == ERROR) {
            if (out_velocity_device != null) omni_tensor_backend_cuda_free(out_velocity_device);
            return updated;
        }
        Value* next_velocity = null;
        if (momentum > 0.0) {
            next_velocity = ml_optimizer_cuda_tensor_value(params, out_velocity_device, interp, "ml/optimizer-step: failed to allocate CUDA fused SGD velocity metadata", "ml/optimizer-step: failed to allocate CUDA fused SGD velocity value");
            if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
        }
        *out_params = updated;
        *out_velocity = next_velocity;
        return null;
    }

    Value* effective_grad_value = null;
    TensorVal* effective_grad = grads;
    if (weight_decay != 0.0) {
        Value* decay_value = null;
        Value* err = ml_optimizer_cuda_mul_scalar_f32(params, weight_decay, interp, "ml/optimizer-step: failed to compute CUDA weight decay", &decay_value);
        if (err != null) return err;
        err = ml_optimizer_cuda_add_f32(grads, decay_value.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA effective gradient", &effective_grad_value);
        if (err != null) return err;
        effective_grad = effective_grad_value.tensor_val;
    }

    TensorVal* update_source = effective_grad;
    Value* next_velocity_value = null;
    if (momentum > 0.0) {
        if (has_velocity) {
            Value* momentum_value = null;
            Value* err = ml_optimizer_cuda_mul_scalar_f32(velocity, momentum, interp, "ml/optimizer-step: failed to compute CUDA momentum", &momentum_value);
            if (err != null) return err;
            err = ml_optimizer_cuda_add_f32(momentum_value.tensor_val, effective_grad, interp, "ml/optimizer-step: failed to compute CUDA velocity", &next_velocity_value);
            if (err != null) return err;
        } else {
            Value* err = ml_optimizer_cuda_mul_scalar_f32(effective_grad, 1.0, interp, "ml/optimizer-step: failed to initialize CUDA velocity", &next_velocity_value);
            if (err != null) return err;
        }
        update_source = next_velocity_value.tensor_val;
    }

    Value* scaled_update_value = null;
    Value* err = ml_optimizer_cuda_mul_scalar_f32(update_source, learning_rate, interp, "ml/optimizer-step: failed to scale CUDA update", &scaled_update_value);
    if (err != null) return err;
    Value* updated_value = null;
    err = ml_optimizer_cuda_sub_f32(params, scaled_update_value.tensor_val, interp, "ml/optimizer-step: failed to apply CUDA SGD update", &updated_value);
    if (err != null) return err;

    *out_params = updated_value;
    *out_velocity = momentum > 0.0 ? next_velocity_value : null;
    return null;
}

fn Value* ml_optimizer_step_tensor_try_cuda(
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
    bool touches_cuda = tensor_expr_has_device(params_arg, TENSOR_DEVICE_CUDA) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_CUDA) ||
        (has_velocity && tensor_expr_has_device(velocity_arg, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;
    if (out_attempted != null) *out_attempted = true;

    if (!ml_optimizer_all_cuda_sgd_operands(params_arg, grads_arg, velocity_arg, has_velocity) ||
        ml_optimizer_any_non_cuda_sgd_operand(params_arg, grads_arg, velocity_arg, has_velocity)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA SGD operands must remain CUDA-placed", null);
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

    if (params.device != TENSOR_DEVICE_CUDA || grads.device != TENSOR_DEVICE_CUDA || (has_velocity && velocity.device != TENSOR_DEVICE_CUDA)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA SGD operands must realize to CUDA storage", null);
    }
    if (params.dtype != grads.dtype || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and velocity dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA SGD currently supports Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and velocity shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA SGD currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_CUDA) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_CUDA) ||
        (has_velocity && !tensor_has_valid_device_storage(velocity, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: CUDA tensor backing storage is missing");
    }

    return ml_optimizer_cuda_sgd_update_f32(
        params,
        grads,
        velocity,
        has_velocity,
        learning_rate,
        momentum,
        weight_decay,
        interp,
        out_params,
        out_velocity
    );
}

fn Value* ml_adam_cuda_next_first_f32(TensorVal* effective_grad, TensorVal* first, bool has_moments, double beta1, Interp* interp, Value** out_first) {
    Value* grad_scaled = null;
    Value* err = ml_optimizer_cuda_mul_scalar_f32(effective_grad, 1.0 - beta1, interp, "ml/optimizer-step: failed to scale CUDA Adam gradient moment", &grad_scaled);
    if (err != null) return err;
    if (!has_moments) {
        *out_first = grad_scaled;
        return null;
    }
    Value* old_scaled = null;
    err = ml_optimizer_cuda_mul_scalar_f32(first, beta1, interp, "ml/optimizer-step: failed to scale CUDA Adam first moment", &old_scaled);
    if (err != null) return err;
    return ml_optimizer_cuda_add_f32(old_scaled.tensor_val, grad_scaled.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA Adam first moment", out_first);
}

fn Value* ml_adam_cuda_next_second_f32(TensorVal* effective_grad, TensorVal* second, bool has_moments, double beta2, Interp* interp, Value** out_second) {
    Value* grad_square = null;
    Value* err = ml_optimizer_cuda_mul_f32(effective_grad, effective_grad, interp, "ml/optimizer-step: failed to square CUDA Adam gradient", &grad_square);
    if (err != null) return err;
    Value* grad_scaled = null;
    err = ml_optimizer_cuda_mul_scalar_f32(grad_square.tensor_val, 1.0 - beta2, interp, "ml/optimizer-step: failed to scale CUDA Adam second moment", &grad_scaled);
    if (err != null) return err;
    if (!has_moments) {
        *out_second = grad_scaled;
        return null;
    }
    Value* old_scaled = null;
    err = ml_optimizer_cuda_mul_scalar_f32(second, beta2, interp, "ml/optimizer-step: failed to scale CUDA Adam prior second moment", &old_scaled);
    if (err != null) return err;
    return ml_optimizer_cuda_add_f32(old_scaled.tensor_val, grad_scaled.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA Adam second moment", out_second);
}

fn Value* ml_adam_cuda_update_f32(
    TensorVal* params,
    TensorVal* grads,
    TensorVal* first,
    TensorVal* second,
    bool has_moments,
    double learning_rate,
    double beta1,
    double beta2,
    double epsilon,
    double weight_decay,
    bool decoupled_weight_decay,
    double first_correction,
    double second_correction,
    Interp* interp,
    Value** out_params,
    Value** out_first,
    Value** out_second
) {
    if (omni_tensor_backend_cuda_ml_optimizer_available() != 0) {
        void* out_params_device = null;
        void* out_first_device = null;
        void* out_second_device = null;
        int status = omni_tensor_backend_cuda_ml_adam_f32(
            params.device_handle,
            grads.device_handle,
            has_moments ? first.device_handle : null,
            has_moments ? second.device_handle : null,
            params.byte_len,
            params.element_count,
            (float)learning_rate,
            (float)beta1,
            (float)beta2,
            (float)epsilon,
            (float)weight_decay,
            (float)first_correction,
            (float)second_correction,
            decoupled_weight_decay ? 1 : 0,
            has_moments ? 1 : 0,
            &out_params_device,
            &out_first_device,
            &out_second_device
        );
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (out_params_device != null) omni_tensor_backend_cuda_free(out_params_device);
            if (out_first_device != null) omni_tensor_backend_cuda_free(out_first_device);
            if (out_second_device != null) omni_tensor_backend_cuda_free(out_second_device);
            return tensor_cuda_status_raise(interp, status, "ml/optimizer-step: CUDA fused Adam kernel failed");
        }

        Value* updated = ml_optimizer_cuda_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate CUDA fused Adam parameter metadata", "ml/optimizer-step: failed to allocate CUDA fused Adam parameter value");
        if (updated == null || updated.tag == ERROR) {
            if (out_first_device != null) omni_tensor_backend_cuda_free(out_first_device);
            if (out_second_device != null) omni_tensor_backend_cuda_free(out_second_device);
            return updated;
        }
        Value* next_first = ml_optimizer_cuda_tensor_value(params, out_first_device, interp, "ml/optimizer-step: failed to allocate CUDA fused Adam first-moment metadata", "ml/optimizer-step: failed to allocate CUDA fused Adam first-moment value");
        if (next_first == null || next_first.tag == ERROR) {
            if (out_second_device != null) omni_tensor_backend_cuda_free(out_second_device);
            return next_first;
        }
        Value* next_second = ml_optimizer_cuda_tensor_value(params, out_second_device, interp, "ml/optimizer-step: failed to allocate CUDA fused Adam second-moment metadata", "ml/optimizer-step: failed to allocate CUDA fused Adam second-moment value");
        if (next_second == null || next_second.tag == ERROR) return next_second;
        *out_params = updated;
        *out_first = next_first;
        *out_second = next_second;
        return null;
    }

    Value* effective_grad_value = null;
    TensorVal* effective_grad = grads;
    if (!decoupled_weight_decay && weight_decay != 0.0) {
        Value* decay_value = null;
        Value* err = ml_optimizer_cuda_mul_scalar_f32(params, weight_decay, interp, "ml/optimizer-step: failed to compute CUDA Adam weight decay", &decay_value);
        if (err != null) return err;
        err = ml_optimizer_cuda_add_f32(grads, decay_value.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA Adam effective gradient", &effective_grad_value);
        if (err != null) return err;
        effective_grad = effective_grad_value.tensor_val;
    }

    Value* next_first_value = null;
    Value* err = ml_adam_cuda_next_first_f32(effective_grad, first, has_moments, beta1, interp, &next_first_value);
    if (err != null) return err;
    Value* next_second_value = null;
    err = ml_adam_cuda_next_second_f32(effective_grad, second, has_moments, beta2, interp, &next_second_value);
    if (err != null) return err;

    Value* first_hat = null;
    err = ml_optimizer_cuda_div_scalar_f32(next_first_value.tensor_val, first_correction, interp, "ml/optimizer-step: failed to compute CUDA Adam first bias correction", &first_hat);
    if (err != null) return err;
    Value* second_hat = null;
    err = ml_optimizer_cuda_div_scalar_f32(next_second_value.tensor_val, second_correction, interp, "ml/optimizer-step: failed to compute CUDA Adam second bias correction", &second_hat);
    if (err != null) return err;
    Value* sqrt_second = null;
    err = ml_optimizer_cuda_sqrt_f32(second_hat.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA Adam second moment sqrt", &sqrt_second);
    if (err != null) return err;
    Value* denom = null;
    err = ml_optimizer_cuda_add_scalar_f32(sqrt_second.tensor_val, epsilon, interp, "ml/optimizer-step: failed to compute CUDA Adam denominator", &denom);
    if (err != null) return err;
    Value* update_ratio = null;
    err = ml_optimizer_cuda_div_f32(first_hat.tensor_val, denom.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA Adam update ratio", &update_ratio);
    if (err != null) return err;
    Value* scaled_update = null;
    err = ml_optimizer_cuda_mul_scalar_f32(update_ratio.tensor_val, learning_rate, interp, "ml/optimizer-step: failed to scale CUDA Adam update", &scaled_update);
    if (err != null) return err;

    TensorVal* update_base = params;
    Value* decayed_params = null;
    if (decoupled_weight_decay && weight_decay != 0.0) {
        Value* decay_update = null;
        err = ml_optimizer_cuda_mul_scalar_f32(params, learning_rate * weight_decay, interp, "ml/optimizer-step: failed to compute CUDA AdamW decoupled weight decay", &decay_update);
        if (err != null) return err;
        err = ml_optimizer_cuda_sub_f32(params, decay_update.tensor_val, interp, "ml/optimizer-step: failed to apply CUDA AdamW decoupled weight decay", &decayed_params);
        if (err != null) return err;
        update_base = decayed_params.tensor_val;
    }

    Value* updated_value = null;
    err = ml_optimizer_cuda_sub_f32(update_base, scaled_update.tensor_val, interp, "ml/optimizer-step: failed to apply CUDA Adam update", &updated_value);
    if (err != null) return err;
    *out_params = updated_value;
    *out_first = next_first_value;
    *out_second = next_second_value;
    return null;
}

fn Value* ml_adam_step_tensor_try_cuda(
    Value* params_arg,
    Value* grads_arg,
    Value* first_arg,
    Value* second_arg,
    double learning_rate,
    double beta1,
    double beta2,
    double epsilon,
    double weight_decay,
    bool decoupled_weight_decay,
    long next_step,
    Interp* interp,
    Value** out_params,
    Value** out_first,
    Value** out_second,
    bool* out_attempted
) {
    if (out_attempted != null) *out_attempted = false;
    bool has_moments = false;
    Value* err = ml_optimizer_validate_adam_moments(first_arg, second_arg, interp, &has_moments);
    if (err != null) return err;
    bool touches_cuda = tensor_expr_has_device(params_arg, TENSOR_DEVICE_CUDA) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_CUDA) ||
        (has_moments && (tensor_expr_has_device(first_arg, TENSOR_DEVICE_CUDA) || tensor_expr_has_device(second_arg, TENSOR_DEVICE_CUDA)));
    if (!touches_cuda) return null;
    if (out_attempted != null) *out_attempted = true;

    if (!ml_optimizer_all_cuda_adam_operands(params_arg, grads_arg, first_arg, second_arg, has_moments) ||
        ml_optimizer_any_non_cuda_adam_operand(params_arg, grads_arg, first_arg, second_arg, has_moments)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA Adam operands must remain CUDA-placed", null);
    }
    if (has_moments && (!is_tensor(first_arg) || !is_tensor(second_arg))) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam moment leaves must be tensors");

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* first = null;
    TensorVal* second = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* first_value = null;
    Value* second_value = null;
    err = tensor_expr_resolve_concrete_any_device(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_concrete_any_device(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_moments) {
        err = tensor_expr_resolve_concrete_any_device(first_arg, &first, &first_value, interp, "ml/optimizer-step: first-moment tensor backing storage is missing");
        if (err != null) return err;
        err = tensor_expr_resolve_concrete_any_device(second_arg, &second, &second_value, interp, "ml/optimizer-step: second-moment tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (first_value != null && first_value != first_arg) boundary_cleanup_materialized_value(null, first_value, interp.current_scope);
    defer if (second_value != null && second_value != second_arg) boundary_cleanup_materialized_value(null, second_value, interp.current_scope);

    if (params.device != TENSOR_DEVICE_CUDA || grads.device != TENSOR_DEVICE_CUDA || (has_moments && (first.device != TENSOR_DEVICE_CUDA || second.device != TENSOR_DEVICE_CUDA))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA Adam operands must realize to CUDA storage", null);
    }
    if (params.dtype != grads.dtype || (has_moments && (params.dtype != first.dtype || params.dtype != second.dtype))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and Adam moment dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA Adam currently supports Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_moments && (!ml_sgd_tensor_shapes_match(params, first) || !ml_sgd_tensor_shapes_match(params, second)))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and Adam moment shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_moments && (!tensor_dense_row_major(first) || !tensor_dense_row_major(second)))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA Adam currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_CUDA) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_CUDA) ||
        (has_moments && (!tensor_has_valid_device_storage(first, TENSOR_DEVICE_CUDA) || !tensor_has_valid_device_storage(second, TENSOR_DEVICE_CUDA)))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: CUDA tensor backing storage is missing");
    }

    double first_correction = 1.0 - ml_optimizer_c_pow(beta1, (double)next_step);
    double second_correction = 1.0 - ml_optimizer_c_pow(beta2, (double)next_step);
    if (!ml_optimizer_adam_bias_correction_valid(first_correction, second_correction, true)) {
        return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam bias correction is too small or not finite");
    }

    return ml_adam_cuda_update_f32(
        params,
        grads,
        first,
        second,
        has_moments,
        learning_rate,
        beta1,
        beta2,
        epsilon,
        weight_decay,
        decoupled_weight_decay,
        first_correction,
        second_correction,
        interp,
        out_params,
        out_first,
        out_second
    );
}

fn Value* ml_rmsprop_cuda_next_square_f32(TensorVal* effective_grad, TensorVal* square, bool has_square, double alpha, Interp* interp, Value** out_square) {
    Value* grad_square = null;
    Value* err = ml_optimizer_cuda_mul_f32(effective_grad, effective_grad, interp, "ml/optimizer-step: failed to square CUDA RMSProp gradient", &grad_square);
    if (err != null) return err;
    Value* grad_scaled = null;
    err = ml_optimizer_cuda_mul_scalar_f32(grad_square.tensor_val, 1.0 - alpha, interp, "ml/optimizer-step: failed to scale CUDA RMSProp gradient square", &grad_scaled);
    if (err != null) return err;
    if (!has_square) {
        *out_square = grad_scaled;
        return null;
    }
    Value* old_scaled = null;
    err = ml_optimizer_cuda_mul_scalar_f32(square, alpha, interp, "ml/optimizer-step: failed to scale CUDA RMSProp square-average", &old_scaled);
    if (err != null) return err;
    return ml_optimizer_cuda_add_f32(old_scaled.tensor_val, grad_scaled.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA RMSProp square-average", out_square);
}

fn Value* ml_rmsprop_cuda_update_f32(
    TensorVal* params,
    TensorVal* grads,
    TensorVal* square,
    TensorVal* velocity,
    bool has_square,
    bool has_velocity,
    double learning_rate,
    double alpha,
    double epsilon,
    double momentum,
    double weight_decay,
    Interp* interp,
    Value** out_params,
    Value** out_square,
    Value** out_velocity
) {
    if (omni_tensor_backend_cuda_ml_optimizer_available() != 0) {
        void* out_params_device = null;
        void* out_square_device = null;
        void* out_velocity_device = null;
        int status = omni_tensor_backend_cuda_ml_rmsprop_f32(
            params.device_handle,
            grads.device_handle,
            has_square ? square.device_handle : null,
            has_velocity ? velocity.device_handle : null,
            params.byte_len,
            params.element_count,
            (float)learning_rate,
            (float)alpha,
            (float)epsilon,
            (float)momentum,
            (float)weight_decay,
            has_square ? 1 : 0,
            has_velocity ? 1 : 0,
            &out_params_device,
            &out_square_device,
            &out_velocity_device
        );
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (out_params_device != null) omni_tensor_backend_cuda_free(out_params_device);
            if (out_square_device != null) omni_tensor_backend_cuda_free(out_square_device);
            if (out_velocity_device != null) omni_tensor_backend_cuda_free(out_velocity_device);
            return tensor_cuda_status_raise(interp, status, "ml/optimizer-step: CUDA fused RMSProp kernel failed");
        }

        Value* updated = ml_optimizer_cuda_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate CUDA fused RMSProp parameter metadata", "ml/optimizer-step: failed to allocate CUDA fused RMSProp parameter value");
        if (updated == null || updated.tag == ERROR) {
            if (out_square_device != null) omni_tensor_backend_cuda_free(out_square_device);
            if (out_velocity_device != null) omni_tensor_backend_cuda_free(out_velocity_device);
            return updated;
        }
        Value* next_square = ml_optimizer_cuda_tensor_value(params, out_square_device, interp, "ml/optimizer-step: failed to allocate CUDA fused RMSProp square-average metadata", "ml/optimizer-step: failed to allocate CUDA fused RMSProp square-average value");
        if (next_square == null || next_square.tag == ERROR) {
            if (out_velocity_device != null) omni_tensor_backend_cuda_free(out_velocity_device);
            return next_square;
        }
        *out_params = updated;
        *out_square = next_square;
        if (momentum <= 0.0) {
            if (out_velocity_device != null) omni_tensor_backend_cuda_free(out_velocity_device);
            *out_velocity = null;
            return null;
        }
        Value* next_velocity = ml_optimizer_cuda_tensor_value(params, out_velocity_device, interp, "ml/optimizer-step: failed to allocate CUDA fused RMSProp velocity metadata", "ml/optimizer-step: failed to allocate CUDA fused RMSProp velocity value");
        if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
        *out_velocity = next_velocity;
        return null;
    }

    Value* effective_grad_value = null;
    TensorVal* effective_grad = grads;
    if (weight_decay != 0.0) {
        Value* decay_value = null;
        Value* err = ml_optimizer_cuda_mul_scalar_f32(params, weight_decay, interp, "ml/optimizer-step: failed to compute CUDA RMSProp weight decay", &decay_value);
        if (err != null) return err;
        err = ml_optimizer_cuda_add_f32(grads, decay_value.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA RMSProp effective gradient", &effective_grad_value);
        if (err != null) return err;
        effective_grad = effective_grad_value.tensor_val;
    }

    Value* next_square_value = null;
    Value* err = ml_rmsprop_cuda_next_square_f32(effective_grad, square, has_square, alpha, interp, &next_square_value);
    if (err != null) return err;
    Value* sqrt_square = null;
    err = ml_optimizer_cuda_sqrt_f32(next_square_value.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA RMSProp square-average sqrt", &sqrt_square);
    if (err != null) return err;
    Value* denom = null;
    err = ml_optimizer_cuda_add_scalar_f32(sqrt_square.tensor_val, epsilon, interp, "ml/optimizer-step: failed to compute CUDA RMSProp denominator", &denom);
    if (err != null) return err;
    Value* update_ratio = null;
    err = ml_optimizer_cuda_div_f32(effective_grad, denom.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA RMSProp update ratio", &update_ratio);
    if (err != null) return err;
    Value* scaled_update = null;
    err = ml_optimizer_cuda_mul_scalar_f32(update_ratio.tensor_val, learning_rate, interp, "ml/optimizer-step: failed to scale CUDA RMSProp update", &scaled_update);
    if (err != null) return err;

    TensorVal* update_source = scaled_update.tensor_val;
    Value* next_velocity_value = null;
    if (momentum > 0.0) {
        if (has_velocity) {
            Value* old_scaled = null;
            err = ml_optimizer_cuda_mul_scalar_f32(velocity, momentum, interp, "ml/optimizer-step: failed to scale CUDA RMSProp velocity", &old_scaled);
            if (err != null) return err;
            err = ml_optimizer_cuda_add_f32(old_scaled.tensor_val, scaled_update.tensor_val, interp, "ml/optimizer-step: failed to compute CUDA RMSProp velocity", &next_velocity_value);
            if (err != null) return err;
        } else {
            err = ml_optimizer_cuda_mul_scalar_f32(scaled_update.tensor_val, 1.0, interp, "ml/optimizer-step: failed to initialize CUDA RMSProp velocity", &next_velocity_value);
            if (err != null) return err;
        }
        update_source = next_velocity_value.tensor_val;
    }

    Value* updated_value = null;
    err = ml_optimizer_cuda_sub_f32(params, update_source, interp, "ml/optimizer-step: failed to apply CUDA RMSProp update", &updated_value);
    if (err != null) return err;
    *out_params = updated_value;
    *out_square = next_square_value;
    *out_velocity = momentum > 0.0 ? next_velocity_value : null;
    return null;
}

fn Value* ml_rmsprop_step_tensor_try_cuda(
    Value* params_arg,
    Value* grads_arg,
    Value* square_arg,
    Value* velocity_arg,
    double learning_rate,
    double alpha,
    double epsilon,
    double momentum,
    double weight_decay,
    Interp* interp,
    Value** out_params,
    Value** out_square,
    Value** out_velocity,
    bool* out_attempted
) {
    if (out_attempted != null) *out_attempted = false;
    bool has_square = square_arg != null && !is_nil(square_arg);
    bool has_velocity = momentum > 0.0 && velocity_arg != null && !is_nil(velocity_arg);
    bool touches_cuda = tensor_expr_has_device(params_arg, TENSOR_DEVICE_CUDA) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_CUDA) ||
        (has_square && tensor_expr_has_device(square_arg, TENSOR_DEVICE_CUDA)) ||
        (has_velocity && tensor_expr_has_device(velocity_arg, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;
    if (out_attempted != null) *out_attempted = true;

    if (!ml_optimizer_all_cuda_rmsprop_operands(params_arg, grads_arg, square_arg, velocity_arg, has_square, has_velocity) ||
        ml_optimizer_any_non_cuda_rmsprop_operand(params_arg, grads_arg, square_arg, velocity_arg, has_square, has_velocity)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA RMSProp operands must remain CUDA-placed", null);
    }
    if (has_square && !is_tensor(square_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp square-average leaves must be tensors");
    if (has_velocity && !is_tensor(velocity_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp velocity leaves must be tensors");

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* square = null;
    TensorVal* velocity = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* square_value = null;
    Value* velocity_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_concrete_any_device(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_square) {
        err = tensor_expr_resolve_concrete_any_device(square_arg, &square, &square_value, interp, "ml/optimizer-step: square-average tensor backing storage is missing");
        if (err != null) return err;
    }
    if (has_velocity) {
        err = tensor_expr_resolve_concrete_any_device(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (square_value != null && square_value != square_arg) boundary_cleanup_materialized_value(null, square_value, interp.current_scope);
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);

    if (params.device != TENSOR_DEVICE_CUDA || grads.device != TENSOR_DEVICE_CUDA ||
        (has_square && square.device != TENSOR_DEVICE_CUDA) || (has_velocity && velocity.device != TENSOR_DEVICE_CUDA)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA RMSProp operands must realize to CUDA storage", null);
    }
    if (params.dtype != grads.dtype || (has_square && params.dtype != square.dtype) || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and RMSProp state dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA RMSProp currently supports Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_square && !ml_sgd_tensor_shapes_match(params, square)) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and RMSProp state shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_square && !tensor_dense_row_major(square)) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: CUDA RMSProp currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_CUDA) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_CUDA) ||
        (has_square && !tensor_has_valid_device_storage(square, TENSOR_DEVICE_CUDA)) ||
        (has_velocity && !tensor_has_valid_device_storage(velocity, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: CUDA tensor backing storage is missing");
    }

    return ml_rmsprop_cuda_update_f32(
        params,
        grads,
        square,
        velocity,
        has_square,
        has_velocity,
        learning_rate,
        alpha,
        epsilon,
        momentum,
        weight_decay,
        interp,
        out_params,
        out_square,
        out_velocity
    );
}
