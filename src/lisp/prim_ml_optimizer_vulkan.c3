module lisp;

import main;

fn Value* ml_optimizer_vulkan_tensor_value(TensorVal* source, void* device_handle, Interp* interp, char[] metadata_label, char[] value_label) {
    if (source == null || device_handle == null) {
        if (device_handle != null) omni_tensor_backend_vulkan_free(device_handle);
        return tensor_runtime_raise(interp, metadata_label);
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, source.shape[0:source.rank], false, false, metadata_label);
    if (tensor == null) {
        omni_tensor_backend_vulkan_free(device_handle);
        return tensor_runtime_raise(interp, metadata_label);
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = device_handle;
    tensor.device_finalizer = &tensor_vulkan_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, value_label);
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return tensor_runtime_raise(interp, value_label);
    }
    return result;
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

    if (has_velocity && !is_tensor(velocity_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: velocity leaves must be tensors");

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* velocity = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* velocity_value = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_vulkan_or_migrate(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_velocity) {
        err = tensor_expr_resolve_vulkan_or_migrate(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);
    if (params.dtype != grads.dtype || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and velocity dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32 && params.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan SGD currently supports Float64 and Float32 tensors", null);
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
    int status;
    if (params.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_ml_sgd_f64(
            params.device_handle,
            params.byte_len,
            grads.device_handle,
            grads.byte_len,
            has_velocity ? velocity.device_handle : null,
            has_velocity ? velocity.byte_len : 0,
            params.element_count,
            learning_rate,
            momentum,
            weight_decay,
            has_velocity ? 1 : 0,
            &out_params_device,
            &out_velocity_device
        );
    } else {
        status = omni_tensor_backend_vulkan_ml_sgd_f32(
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
    }
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

fn Value* ml_adam_step_tensor_try_vulkan(
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
    bool touches_vulkan = tensor_expr_has_device(params_arg, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_VULKAN) ||
        (has_moments && (tensor_expr_has_device(first_arg, TENSOR_DEVICE_VULKAN) || tensor_expr_has_device(second_arg, TENSOR_DEVICE_VULKAN)));
    if (!touches_vulkan) return null;
    if (out_attempted != null) *out_attempted = true;

    if (has_moments && (!is_tensor(first_arg) || !is_tensor(second_arg))) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam moment leaves must be tensors");

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* first = null;
    TensorVal* second = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* first_value = null;
    Value* second_value = null;
    err = tensor_expr_resolve_vulkan_or_migrate(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_vulkan_or_migrate(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_moments) {
        err = tensor_expr_resolve_vulkan_or_migrate(first_arg, &first, &first_value, interp, "ml/optimizer-step: first-moment tensor backing storage is missing");
        if (err != null) return err;
        err = tensor_expr_resolve_vulkan_or_migrate(second_arg, &second, &second_value, interp, "ml/optimizer-step: second-moment tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (first_value != null && first_value != first_arg) boundary_cleanup_materialized_value(null, first_value, interp.current_scope);
    defer if (second_value != null && second_value != second_arg) boundary_cleanup_materialized_value(null, second_value, interp.current_scope);
    if (params.dtype != grads.dtype || (has_moments && (params.dtype != first.dtype || params.dtype != second.dtype))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and Adam moment dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32 && params.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan Adam currently supports Float64 and Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_moments && (!ml_sgd_tensor_shapes_match(params, first) || !ml_sgd_tensor_shapes_match(params, second)))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and Adam moment shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_moments && (!tensor_dense_row_major(first) || !tensor_dense_row_major(second)))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan Adam currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_VULKAN) ||
        (has_moments && (!tensor_has_valid_device_storage(first, TENSOR_DEVICE_VULKAN) || !tensor_has_valid_device_storage(second, TENSOR_DEVICE_VULKAN)))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: Vulkan tensor backing storage is missing");
    }

    double first_correction = 1.0 - ml_optimizer_c_pow(beta1, (double)next_step);
    double second_correction = 1.0 - ml_optimizer_c_pow(beta2, (double)next_step);
    if (!ml_optimizer_adam_bias_correction_valid(first_correction, second_correction, params.dtype == TENSOR_DTYPE_FLOAT32)) {
        return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam bias correction is too small or not finite");
    }

    void* out_params_device = null;
    void* out_first_device = null;
    void* out_second_device = null;
    int status;
    if (params.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_ml_adam_f64(
            params.device_handle,
            params.byte_len,
            grads.device_handle,
            grads.byte_len,
            has_moments ? first.device_handle : null,
            has_moments ? first.byte_len : 0,
            has_moments ? second.device_handle : null,
            has_moments ? second.byte_len : 0,
            params.element_count,
            learning_rate,
            beta1,
            beta2,
            epsilon,
            weight_decay,
            first_correction,
            second_correction,
            decoupled_weight_decay ? 1 : 0,
            has_moments ? 1 : 0,
            &out_params_device,
            &out_first_device,
            &out_second_device
        );
    } else {
        status = omni_tensor_backend_vulkan_ml_adam_f32(
            params.device_handle,
            params.byte_len,
            grads.device_handle,
            grads.byte_len,
            has_moments ? first.device_handle : null,
            has_moments ? first.byte_len : 0,
            has_moments ? second.device_handle : null,
            has_moments ? second.byte_len : 0,
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
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_params_device != null) omni_tensor_backend_vulkan_free(out_params_device);
        if (out_first_device != null) omni_tensor_backend_vulkan_free(out_first_device);
        if (out_second_device != null) omni_tensor_backend_vulkan_free(out_second_device);
        return tensor_vulkan_status_raise(interp, status, "ml/optimizer-step: Vulkan Adam kernel failed");
    }

    Value* updated = ml_optimizer_vulkan_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate Vulkan updated tensor metadata", "ml/optimizer-step: failed to allocate Vulkan updated tensor value");
    if (updated == null || updated.tag == ERROR) {
        if (out_first_device != null) omni_tensor_backend_vulkan_free(out_first_device);
        if (out_second_device != null) omni_tensor_backend_vulkan_free(out_second_device);
        return updated;
    }
    Value* next_first = ml_optimizer_vulkan_tensor_value(params, out_first_device, interp, "ml/optimizer-step: failed to allocate Vulkan first-moment tensor metadata", "ml/optimizer-step: failed to allocate Vulkan first-moment tensor value");
    if (next_first == null || next_first.tag == ERROR) {
        if (out_second_device != null) omni_tensor_backend_vulkan_free(out_second_device);
        return next_first;
    }
    Value* next_second = ml_optimizer_vulkan_tensor_value(params, out_second_device, interp, "ml/optimizer-step: failed to allocate Vulkan second-moment tensor metadata", "ml/optimizer-step: failed to allocate Vulkan second-moment tensor value");
    if (next_second == null || next_second.tag == ERROR) return next_second;
    *out_params = updated;
    *out_first = next_first;
    *out_second = next_second;
    return null;
}

fn Value* ml_rmsprop_step_tensor_try_vulkan(
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
    bool touches_vulkan = tensor_expr_has_device(params_arg, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(grads_arg, TENSOR_DEVICE_VULKAN) ||
        (has_square && tensor_expr_has_device(square_arg, TENSOR_DEVICE_VULKAN)) ||
        (has_velocity && tensor_expr_has_device(velocity_arg, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;
    if (out_attempted != null) *out_attempted = true;

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
    Value* err = tensor_expr_resolve_vulkan_or_migrate(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = tensor_expr_resolve_vulkan_or_migrate(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    if (has_square) {
        err = tensor_expr_resolve_vulkan_or_migrate(square_arg, &square, &square_value, interp, "ml/optimizer-step: square-average tensor backing storage is missing");
        if (err != null) return err;
    }
    if (has_velocity) {
        err = tensor_expr_resolve_vulkan_or_migrate(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (square_value != null && square_value != square_arg) boundary_cleanup_materialized_value(null, square_value, interp.current_scope);
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);
    if (params.dtype != grads.dtype || (has_square && params.dtype != square.dtype) || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and RMSProp state dtypes must match", null);
    }
    if (params.dtype != TENSOR_DTYPE_FLOAT32 && params.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan RMSProp currently supports Float64 and Float32 tensors", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_square && !ml_sgd_tensor_shapes_match(params, square)) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and RMSProp state shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_square && !tensor_dense_row_major(square)) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan RMSProp currently supports dense row-major tensors", null);
    }
    if (!tensor_has_valid_device_storage(params, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(grads, TENSOR_DEVICE_VULKAN) ||
        (has_square && !tensor_has_valid_device_storage(square, TENSOR_DEVICE_VULKAN)) ||
        (has_velocity && !tensor_has_valid_device_storage(velocity, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "ml/optimizer-step: Vulkan tensor backing storage is missing");
    }

    void* out_params_device = null;
    void* out_square_device = null;
    void* out_velocity_device = null;
    int status;
    if (params.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_ml_rmsprop_f64(
            params.device_handle,
            params.byte_len,
            grads.device_handle,
            grads.byte_len,
            has_square ? square.device_handle : null,
            has_square ? square.byte_len : 0,
            has_velocity ? velocity.device_handle : null,
            has_velocity ? velocity.byte_len : 0,
            params.element_count,
            learning_rate,
            alpha,
            epsilon,
            momentum,
            weight_decay,
            has_square ? 1 : 0,
            has_velocity ? 1 : 0,
            &out_params_device,
            &out_square_device,
            &out_velocity_device
        );
    } else {
        status = omni_tensor_backend_vulkan_ml_rmsprop_f32(
            params.device_handle,
            params.byte_len,
            grads.device_handle,
            grads.byte_len,
            has_square ? square.device_handle : null,
            has_square ? square.byte_len : 0,
            has_velocity ? velocity.device_handle : null,
            has_velocity ? velocity.byte_len : 0,
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
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_params_device != null) omni_tensor_backend_vulkan_free(out_params_device);
        if (out_square_device != null) omni_tensor_backend_vulkan_free(out_square_device);
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        return tensor_vulkan_status_raise(interp, status, "ml/optimizer-step: Vulkan RMSProp kernel failed");
    }

    Value* updated = ml_optimizer_vulkan_tensor_value(params, out_params_device, interp, "ml/optimizer-step: failed to allocate Vulkan updated tensor metadata", "ml/optimizer-step: failed to allocate Vulkan updated tensor value");
    if (updated == null || updated.tag == ERROR) {
        if (out_square_device != null) omni_tensor_backend_vulkan_free(out_square_device);
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        return updated;
    }
    Value* next_square = ml_optimizer_vulkan_tensor_value(params, out_square_device, interp, "ml/optimizer-step: failed to allocate Vulkan square-average tensor metadata", "ml/optimizer-step: failed to allocate Vulkan square-average tensor value");
    if (next_square == null || next_square.tag == ERROR) {
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        return next_square;
    }
    *out_params = updated;
    *out_square = next_square;
    if (momentum <= 0.0) {
        if (out_velocity_device != null) omni_tensor_backend_vulkan_free(out_velocity_device);
        *out_velocity = null;
        return null;
    }
    Value* next_velocity = ml_optimizer_vulkan_tensor_value(params, out_velocity_device, interp, "ml/optimizer-step: failed to allocate Vulkan velocity tensor metadata", "ml/optimizer-step: failed to allocate Vulkan velocity tensor value");
    if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
    *out_velocity = next_velocity;
    return null;
}
