module lisp;

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
        "ml/optimizer-step: CUDA SGD currently supports dense row-major Float32 tensors",
        "ml/optimizer-step: CUDA tensor backing storage is missing",
        "ml/optimizer-step: CUDA SGD kernel failed"
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

fn bool ml_optimizer_all_cuda_sgd_operands(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (!tensor_expr_has_device(params, TENSOR_DEVICE_CUDA) || !tensor_expr_has_device(grads, TENSOR_DEVICE_CUDA)) return false;
    if (has_velocity && !tensor_expr_has_device(velocity, TENSOR_DEVICE_CUDA)) return false;
    return true;
}

fn bool ml_optimizer_any_non_cuda_sgd_operand(Value* params, Value* grads, Value* velocity, bool has_velocity) {
    if (tensor_expr_has_non_cuda_device(params) || tensor_expr_has_non_cuda_device(grads)) return true;
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
