module lisp;

import std::core::mem;

extern fn double ml_optimizer_c_sqrt(double x) @extern("sqrt");
extern fn double ml_optimizer_c_pow(double x, double y) @extern("pow");

fn Value* ml_optimizer_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn Value* ml_optimizer_shape_mismatch(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", message, null);
}

fn Value* ml_optimizer_resolve_cpu(Value* value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (ml_grad_touches_vulkan(value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: Vulkan optimizer kernels are not implemented", null);
    }
    return tensor_expr_resolve_concrete_cpu(
        value,
        out_tensor,
        out_value,
        interp,
        label,
        "ml/optimizer-step: backend kernel is not implemented for non-CPU tensors"
    );
}

fn Value* ml_optimizer_parse_spec_number(Value* spec, Interp* interp, char[] key, bool required, double default_value, double* out_value) {
    Value* value = nn_dict_get(spec, interp, key);
    if (value == null) {
        if (required) return ml_optimizer_invalid(interp, "ml/optimizer-step: optimizer spec is missing a required numeric field");
        *out_value = default_value;
        return null;
    }
    if (!try_numeric_to_double(value, out_value) || !conversion_double_is_finite(*out_value)) {
        return ml_optimizer_invalid(interp, "ml/optimizer-step: optimizer numeric fields must be finite numbers");
    }
    return null;
}

fn Value* ml_optimizer_step_tensor(Value* params_arg, Value* grads_arg, Value* velocity_arg, double learning_rate, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_velocity) {
    bool vulkan_attempted = false;
    Value* vulkan_err = ml_optimizer_step_tensor_try_vulkan(params_arg, grads_arg, velocity_arg, learning_rate, momentum, weight_decay, interp, out_params, out_velocity, &vulkan_attempted);
    if (vulkan_err != null) return vulkan_err;
    if (vulkan_attempted) return null;

    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* velocity = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* velocity_value = null;
    Value* err = ml_optimizer_resolve_cpu(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = ml_optimizer_resolve_cpu(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    bool has_velocity = momentum > 0.0 && velocity_arg != null && !is_nil(velocity_arg);
    if (has_velocity) {
        if (!is_tensor(velocity_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: velocity leaves must be tensors");
        err = ml_optimizer_resolve_cpu(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);

    if (params.dtype != grads.dtype || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and velocity dtypes must match", null);
    }
    if (!ml_reduction_supported_dtype(params.dtype)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: only Float64 and Float32 tensors are supported", null);
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and velocity shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: currently supports dense row-major tensors", null);
    }
    if (params.element_count > usz.max / double.sizeof) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: parameter tensor is too large", null);

    double* param_values = (double*)mem::malloc(params.element_count * double.sizeof);
    double* velocity_values = momentum > 0.0 ? (double*)mem::malloc(params.element_count * double.sizeof) : null;
    if (param_values == null || (momentum > 0.0 && velocity_values == null)) {
        if (param_values != null) mem::free(param_values);
        if (velocity_values != null) mem::free(velocity_values);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate update buffers", null);
    }
    defer mem::free(param_values);
    defer if (velocity_values != null) mem::free(velocity_values);
    for (usz i = 0; i < params.element_count; i++) {
        double param_value = 0.0;
        double grad_value = 0.0;
        double old_velocity = 0.0;
        if (!ml_reduction_read(params, i, &param_value) || !ml_reduction_read(grads, i, &grad_value) ||
            (has_velocity && !ml_reduction_read(velocity, i, &old_velocity))) {
            return tensor_runtime_raise(interp, "ml/optimizer-step: tensor index is invalid");
        }
        double effective_grad = grad_value + (weight_decay * param_value);
        double new_velocity = (momentum * old_velocity) + effective_grad;
        param_values[i] = param_value - (learning_rate * new_velocity);
        if (momentum > 0.0) velocity_values[i] = new_velocity;
    }

    Value* updated = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate updated tensor");
    if (updated == null || updated.tag == ERROR) return updated;
    err = ml_reduction_write_result(updated.tensor_val, param_values, interp, "ml/optimizer-step: Float32 parameter update is not representable");
    if (err != null) return err;
    *out_params = updated;
    if (momentum <= 0.0) {
        *out_velocity = null;
        return null;
    }
    Value* next_velocity = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate velocity tensor");
    if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
    err = ml_reduction_write_result(next_velocity.tensor_val, velocity_values, interp, "ml/optimizer-step: Float32 velocity update is not representable");
    if (err != null) return err;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_optimizer_step_array(Value* params, Value* grads, Value* velocity, double learning_rate, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_velocity) {
    if (grads == null || grads.tag != ARRAY || grads.array_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_velocity = momentum > 0.0 && velocity != null && !is_nil(velocity);
    if (has_velocity && (velocity.tag != ARRAY || velocity.array_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: velocity tree must match the parameter tree");
    if (params.array_val.length != grads.array_val.length || (has_velocity && params.array_val.length != velocity.array_val.length)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: array optimizer trees must have the same length");
    Value* updated = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    Value* next_velocity = momentum > 0.0 ? make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory") : null;
    if (updated == null || updated.tag == ERROR || (next_velocity != null && next_velocity.tag == ERROR)) return updated != null && updated.tag == ERROR ? updated : next_velocity;
    for (usz i = 0; i < params.array_val.length; i++) {
        Value* child_params = null;
        Value* child_velocity = null;
        Value* velocity_child = has_velocity ? velocity.array_val.items[i] : null;
        Value* err = ml_optimizer_step_tree(params.array_val.items[i], grads.array_val.items[i], velocity_child, learning_rate, momentum, weight_decay, interp, &child_params, &child_velocity);
        if (err != null) return err;
        updated.array_val.items[i] = child_params;
        updated.array_val.length++;
        if (next_velocity != null) {
            next_velocity.array_val.items[i] = child_velocity;
            next_velocity.array_val.length++;
        }
    }
    *out_params = updated;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_optimizer_step_dict(Value* params, Value* grads, Value* velocity, double learning_rate, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_velocity) {
    if (grads == null || grads.tag != HASHMAP || grads.hashmap_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_velocity = momentum > 0.0 && velocity != null && !is_nil(velocity);
    if (has_velocity && (velocity.tag != HASHMAP || velocity.hashmap_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: velocity tree must match the parameter tree");
    if (params.hashmap_val.count != grads.hashmap_val.count || (has_velocity && params.hashmap_val.count != velocity.hashmap_val.count)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
    Value* updated = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    Value* next_velocity = momentum > 0.0 ? nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2) : null;
    if (updated == null || updated.tag == ERROR || (next_velocity != null && next_velocity.tag == ERROR)) return updated != null && updated.tag == ERROR ? updated : next_velocity;
    for (uint i = 0; i < params.hashmap_val.capacity; i++) {
        HashEntry* entry = &params.hashmap_val.entries[i];
        if (entry.key == null) continue;
        Value* grad_leaf = hashmap_get(grads.hashmap_val, entry.key);
        Value* velocity_leaf = has_velocity ? hashmap_get(velocity.hashmap_val, entry.key) : null;
        if (grad_leaf == null || (has_velocity && velocity_leaf == null)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
        Value* child_params = null;
        Value* child_velocity = null;
        Value* err = ml_optimizer_step_tree(entry.value, grad_leaf, velocity_leaf, learning_rate, momentum, weight_decay, interp, &child_params, &child_velocity);
        if (err != null) return err;
        if (!hashmap_set_checked(updated.hashmap_val, entry.key, child_params, interp) ||
            (next_velocity != null && !hashmap_set_checked(next_velocity.hashmap_val, entry.key, child_velocity, interp))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate result dictionary", null);
        }
    }
    *out_params = updated;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_optimizer_step_tree(Value* params, Value* grads, Value* velocity, double learning_rate, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_velocity) {
    if (params == null || grads == null || out_params == null || out_velocity == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: optimizer tree state is missing");
    if (is_tensor(params)) {
        if (!is_tensor(grads)) return ml_optimizer_invalid(interp, "ml/optimizer-step: gradient leaves must be tensors");
        return ml_optimizer_step_tensor(params, grads, velocity, learning_rate, momentum, weight_decay, interp, out_params, out_velocity);
    }
    if (params.tag == ARRAY && params.array_val != null) return ml_optimizer_step_array(params, grads, velocity, learning_rate, momentum, weight_decay, interp, out_params, out_velocity);
    if (params.tag == HASHMAP && params.hashmap_val != null) return ml_optimizer_step_dict(params, grads, velocity, learning_rate, momentum, weight_decay, interp, out_params, out_velocity);
    return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter tree leaves must be tensors");
}

fn Value* ml_adam_step_tensor(Value* params_arg, Value* grads_arg, Value* first_arg, Value* second_arg, double learning_rate, double beta1, double beta2, double epsilon, double weight_decay, bool decoupled_weight_decay, long next_step, Interp* interp, Value** out_params, Value** out_first, Value** out_second) {
    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* first = null;
    TensorVal* second = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* first_value = null;
    Value* second_value = null;
    Value* err = ml_optimizer_resolve_cpu(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = ml_optimizer_resolve_cpu(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    bool has_moments = first_arg != null && second_arg != null;
    if (has_moments) {
        if (!is_tensor(first_arg) || !is_tensor(second_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam moment leaves must be tensors");
        err = ml_optimizer_resolve_cpu(first_arg, &first, &first_value, interp, "ml/optimizer-step: first-moment tensor backing storage is missing");
        if (err != null) return err;
        err = ml_optimizer_resolve_cpu(second_arg, &second, &second_value, interp, "ml/optimizer-step: second-moment tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (first_value != null && first_value != first_arg) boundary_cleanup_materialized_value(null, first_value, interp.current_scope);
    defer if (second_value != null && second_value != second_arg) boundary_cleanup_materialized_value(null, second_value, interp.current_scope);

    if (params.dtype != grads.dtype || (has_moments && (params.dtype != first.dtype || params.dtype != second.dtype))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and Adam moment dtypes must match", null);
    }
    if (!ml_reduction_supported_dtype(params.dtype)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: only Float64 and Float32 tensors are supported", null);
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_moments && (!ml_sgd_tensor_shapes_match(params, first) || !ml_sgd_tensor_shapes_match(params, second)))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and Adam moment shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_moments && (!tensor_dense_row_major(first) || !tensor_dense_row_major(second)))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: currently supports dense row-major tensors", null);
    }
    if (params.element_count > usz.max / double.sizeof) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: parameter tensor is too large", null);

    double* param_values = (double*)mem::malloc(params.element_count * double.sizeof);
    double* first_values = (double*)mem::malloc(params.element_count * double.sizeof);
    double* second_values = (double*)mem::malloc(params.element_count * double.sizeof);
    if (param_values == null || first_values == null || second_values == null) {
        if (param_values != null) mem::free(param_values);
        if (first_values != null) mem::free(first_values);
        if (second_values != null) mem::free(second_values);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate update buffers", null);
    }
    defer mem::free(param_values);
    defer mem::free(first_values);
    defer mem::free(second_values);
    double first_correction = 1.0 - ml_optimizer_c_pow(beta1, (double)next_step);
    double second_correction = 1.0 - ml_optimizer_c_pow(beta2, (double)next_step);
    if (first_correction <= 0.0 || second_correction <= 0.0 || !conversion_double_is_finite(first_correction) || !conversion_double_is_finite(second_correction)) {
        return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam bias correction is not finite");
    }
    for (usz i = 0; i < params.element_count; i++) {
        double param_value = 0.0;
        double grad_value = 0.0;
        double old_first = 0.0;
        double old_second = 0.0;
        if (!ml_reduction_read(params, i, &param_value) || !ml_reduction_read(grads, i, &grad_value) ||
            (has_moments && (!ml_reduction_read(first, i, &old_first) || !ml_reduction_read(second, i, &old_second)))) {
            return tensor_runtime_raise(interp, "ml/optimizer-step: tensor index is invalid");
        }
        double moment_grad = decoupled_weight_decay ? grad_value : grad_value + (weight_decay * param_value);
        double next_first = (beta1 * old_first) + ((1.0 - beta1) * moment_grad);
        double next_second = (beta2 * old_second) + ((1.0 - beta2) * moment_grad * moment_grad);
        double first_hat = next_first / first_correction;
        double second_hat = next_second / second_correction;
        double decay_update = decoupled_weight_decay ? learning_rate * weight_decay * param_value : 0.0;
        param_values[i] = param_value - decay_update - (learning_rate * first_hat / (ml_optimizer_c_sqrt(second_hat) + epsilon));
        first_values[i] = next_first;
        second_values[i] = next_second;
    }

    Value* updated = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate updated tensor");
    if (updated == null || updated.tag == ERROR) return updated;
    err = ml_reduction_write_result(updated.tensor_val, param_values, interp, "ml/optimizer-step: Float32 parameter update is not representable");
    if (err != null) return err;
    Value* next_first = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate first-moment tensor");
    if (next_first == null || next_first.tag == ERROR) return next_first;
    err = ml_reduction_write_result(next_first.tensor_val, first_values, interp, "ml/optimizer-step: Float32 first-moment update is not representable");
    if (err != null) return err;
    Value* next_second = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate second-moment tensor");
    if (next_second == null || next_second.tag == ERROR) return next_second;
    err = ml_reduction_write_result(next_second.tensor_val, second_values, interp, "ml/optimizer-step: Float32 second-moment update is not representable");
    if (err != null) return err;
    *out_params = updated;
    *out_first = next_first;
    *out_second = next_second;
    return null;
}

fn Value* ml_adam_step_array(Value* params, Value* grads, Value* first, Value* second, double learning_rate, double beta1, double beta2, double epsilon, double weight_decay, bool decoupled_weight_decay, long next_step, Interp* interp, Value** out_params, Value** out_first, Value** out_second) {
    if (grads == null || grads.tag != ARRAY || grads.array_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_moments = first != null && second != null;
    if (has_moments && (first.tag != ARRAY || first.array_val == null || second.tag != ARRAY || second.array_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam moment trees must match the parameter tree");
    if (params.array_val.length != grads.array_val.length || (has_moments && (params.array_val.length != first.array_val.length || params.array_val.length != second.array_val.length))) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: array optimizer trees must have the same length");
    Value* updated = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    Value* next_first = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    Value* next_second = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    if (updated == null || updated.tag == ERROR || next_first == null || next_first.tag == ERROR || next_second == null || next_second.tag == ERROR) return updated != null && updated.tag == ERROR ? updated : (next_first != null && next_first.tag == ERROR ? next_first : next_second);
    for (usz i = 0; i < params.array_val.length; i++) {
        Value* child_params = null;
        Value* child_first = null;
        Value* child_second = null;
        Value* err = ml_adam_step_tree(params.array_val.items[i], grads.array_val.items[i], has_moments ? first.array_val.items[i] : null, has_moments ? second.array_val.items[i] : null, learning_rate, beta1, beta2, epsilon, weight_decay, decoupled_weight_decay, next_step, interp, &child_params, &child_first, &child_second);
        if (err != null) return err;
        updated.array_val.items[i] = child_params;
        next_first.array_val.items[i] = child_first;
        next_second.array_val.items[i] = child_second;
        updated.array_val.length++;
        next_first.array_val.length++;
        next_second.array_val.length++;
    }
    *out_params = updated;
    *out_first = next_first;
    *out_second = next_second;
    return null;
}

fn Value* ml_adam_step_dict(Value* params, Value* grads, Value* first, Value* second, double learning_rate, double beta1, double beta2, double epsilon, double weight_decay, bool decoupled_weight_decay, long next_step, Interp* interp, Value** out_params, Value** out_first, Value** out_second) {
    if (grads == null || grads.tag != HASHMAP || grads.hashmap_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_moments = first != null && second != null;
    if (has_moments && (first.tag != HASHMAP || first.hashmap_val == null || second.tag != HASHMAP || second.hashmap_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam moment trees must match the parameter tree");
    if (params.hashmap_val.count != grads.hashmap_val.count || (has_moments && (params.hashmap_val.count != first.hashmap_val.count || params.hashmap_val.count != second.hashmap_val.count))) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
    Value* updated = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    Value* next_first = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    Value* next_second = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    if (updated == null || updated.tag == ERROR || next_first == null || next_first.tag == ERROR || next_second == null || next_second.tag == ERROR) return updated != null && updated.tag == ERROR ? updated : (next_first != null && next_first.tag == ERROR ? next_first : next_second);
    for (uint i = 0; i < params.hashmap_val.capacity; i++) {
        HashEntry* entry = &params.hashmap_val.entries[i];
        if (entry.key == null) continue;
        Value* grad_leaf = hashmap_get(grads.hashmap_val, entry.key);
        Value* first_leaf = has_moments ? hashmap_get(first.hashmap_val, entry.key) : null;
        Value* second_leaf = has_moments ? hashmap_get(second.hashmap_val, entry.key) : null;
        if (grad_leaf == null || (has_moments && (first_leaf == null || second_leaf == null))) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
        Value* child_params = null;
        Value* child_first = null;
        Value* child_second = null;
        Value* err = ml_adam_step_tree(entry.value, grad_leaf, first_leaf, second_leaf, learning_rate, beta1, beta2, epsilon, weight_decay, decoupled_weight_decay, next_step, interp, &child_params, &child_first, &child_second);
        if (err != null) return err;
        if (!hashmap_set_checked(updated.hashmap_val, entry.key, child_params, interp) ||
            !hashmap_set_checked(next_first.hashmap_val, entry.key, child_first, interp) ||
            !hashmap_set_checked(next_second.hashmap_val, entry.key, child_second, interp)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate result dictionary", null);
        }
    }
    *out_params = updated;
    *out_first = next_first;
    *out_second = next_second;
    return null;
}

fn Value* ml_adam_step_tree(Value* params, Value* grads, Value* first, Value* second, double learning_rate, double beta1, double beta2, double epsilon, double weight_decay, bool decoupled_weight_decay, long next_step, Interp* interp, Value** out_params, Value** out_first, Value** out_second) {
    if (params == null || grads == null || out_params == null || out_first == null || out_second == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: optimizer tree state is missing");
    if (is_tensor(params)) {
        if (!is_tensor(grads)) return ml_optimizer_invalid(interp, "ml/optimizer-step: gradient leaves must be tensors");
        return ml_adam_step_tensor(params, grads, first, second, learning_rate, beta1, beta2, epsilon, weight_decay, decoupled_weight_decay, next_step, interp, out_params, out_first, out_second);
    }
    if (params.tag == ARRAY && params.array_val != null) return ml_adam_step_array(params, grads, first, second, learning_rate, beta1, beta2, epsilon, weight_decay, decoupled_weight_decay, next_step, interp, out_params, out_first, out_second);
    if (params.tag == HASHMAP && params.hashmap_val != null) return ml_adam_step_dict(params, grads, first, second, learning_rate, beta1, beta2, epsilon, weight_decay, decoupled_weight_decay, next_step, interp, out_params, out_first, out_second);
    return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter tree leaves must be tensors");
}

fn Value* ml_rmsprop_step_tensor(Value* params_arg, Value* grads_arg, Value* square_arg, Value* velocity_arg, double learning_rate, double alpha, double epsilon, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_square, Value** out_velocity) {
    TensorVal* params = null;
    TensorVal* grads = null;
    TensorVal* square = null;
    TensorVal* velocity = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* square_value = null;
    Value* velocity_value = null;
    Value* err = ml_optimizer_resolve_cpu(params_arg, &params, &params_value, interp, "ml/optimizer-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = ml_optimizer_resolve_cpu(grads_arg, &grads, &grads_value, interp, "ml/optimizer-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);
    bool has_square = square_arg != null && !is_nil(square_arg);
    bool has_velocity = momentum > 0.0 && velocity_arg != null && !is_nil(velocity_arg);
    if (has_square) {
        if (!is_tensor(square_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp square-average leaves must be tensors");
        err = ml_optimizer_resolve_cpu(square_arg, &square, &square_value, interp, "ml/optimizer-step: square-average tensor backing storage is missing");
        if (err != null) return err;
    }
    if (has_velocity) {
        if (!is_tensor(velocity_arg)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp velocity leaves must be tensors");
        err = ml_optimizer_resolve_cpu(velocity_arg, &velocity, &velocity_value, interp, "ml/optimizer-step: velocity tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (square_value != null && square_value != square_arg) boundary_cleanup_materialized_value(null, square_value, interp.current_scope);
    defer if (velocity_value != null && velocity_value != velocity_arg) boundary_cleanup_materialized_value(null, velocity_value, interp.current_scope);
    if (params.dtype != grads.dtype || (has_square && params.dtype != square.dtype) || (has_velocity && params.dtype != velocity.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/optimizer-step: parameter, gradient, and RMSProp state dtypes must match", null);
    }
    if (!ml_reduction_supported_dtype(params.dtype)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: only Float64 and Float32 tensors are supported", null);
    if (!ml_sgd_tensor_shapes_match(params, grads) || (has_square && !ml_sgd_tensor_shapes_match(params, square)) || (has_velocity && !ml_sgd_tensor_shapes_match(params, velocity))) {
        return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: parameter, gradient, and RMSProp state shapes must match");
    }
    if (params.element_count == 0) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads) || (has_square && !tensor_dense_row_major(square)) || (has_velocity && !tensor_dense_row_major(velocity))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/optimizer-step: currently supports dense row-major tensors", null);
    }
    if (params.element_count > usz.max / double.sizeof) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: parameter tensor is too large", null);
    double* param_values = (double*)mem::malloc(params.element_count * double.sizeof);
    double* square_values = (double*)mem::malloc(params.element_count * double.sizeof);
    double* velocity_values = momentum > 0.0 ? (double*)mem::malloc(params.element_count * double.sizeof) : null;
    if (param_values == null || square_values == null || (momentum > 0.0 && velocity_values == null)) {
        if (param_values != null) mem::free(param_values);
        if (square_values != null) mem::free(square_values);
        if (velocity_values != null) mem::free(velocity_values);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate update buffers", null);
    }
    defer mem::free(param_values);
    defer mem::free(square_values);
    defer if (velocity_values != null) mem::free(velocity_values);
    for (usz i = 0; i < params.element_count; i++) {
        double param_value = 0.0;
        double grad_value = 0.0;
        double old_square = 0.0;
        double old_velocity = 0.0;
        if (!ml_reduction_read(params, i, &param_value) || !ml_reduction_read(grads, i, &grad_value) ||
            (has_square && !ml_reduction_read(square, i, &old_square)) || (has_velocity && !ml_reduction_read(velocity, i, &old_velocity))) {
            return tensor_runtime_raise(interp, "ml/optimizer-step: tensor index is invalid");
        }
        double effective_grad = grad_value + (weight_decay * param_value);
        double next_square = (alpha * old_square) + ((1.0 - alpha) * effective_grad * effective_grad);
        double scaled_update = learning_rate * effective_grad / (ml_optimizer_c_sqrt(next_square) + epsilon);
        double next_velocity = momentum > 0.0 ? (momentum * old_velocity) + scaled_update : 0.0;
        param_values[i] = param_value - (momentum > 0.0 ? next_velocity : scaled_update);
        square_values[i] = next_square;
        if (momentum > 0.0) velocity_values[i] = next_velocity;
    }
    Value* updated = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate updated tensor");
    if (updated == null || updated.tag == ERROR) return updated;
    err = ml_reduction_write_result(updated.tensor_val, param_values, interp, "ml/optimizer-step: Float32 parameter update is not representable");
    if (err != null) return err;
    Value* next_square = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate square-average tensor");
    if (next_square == null || next_square.tag == ERROR) return next_square;
    err = ml_reduction_write_result(next_square.tensor_val, square_values, interp, "ml/optimizer-step: Float32 square-average update is not representable");
    if (err != null) return err;
    *out_params = updated;
    *out_square = next_square;
    if (momentum <= 0.0) {
        *out_velocity = null;
        return null;
    }
    Value* next_velocity = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/optimizer-step: failed to allocate velocity tensor");
    if (next_velocity == null || next_velocity.tag == ERROR) return next_velocity;
    err = ml_reduction_write_result(next_velocity.tensor_val, velocity_values, interp, "ml/optimizer-step: Float32 velocity update is not representable");
    if (err != null) return err;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_rmsprop_step_array(Value* params, Value* grads, Value* square, Value* velocity, double learning_rate, double alpha, double epsilon, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_square, Value** out_velocity) {
    if (grads == null || grads.tag != ARRAY || grads.array_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_square = square != null && !is_nil(square);
    bool has_velocity = momentum > 0.0 && velocity != null && !is_nil(velocity);
    if (has_square && (square.tag != ARRAY || square.array_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp square-average tree must match the parameter tree");
    if (has_velocity && (velocity.tag != ARRAY || velocity.array_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp velocity tree must match the parameter tree");
    if (params.array_val.length != grads.array_val.length || (has_square && params.array_val.length != square.array_val.length) || (has_velocity && params.array_val.length != velocity.array_val.length)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: array optimizer trees must have the same length");
    Value* updated = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    Value* next_square = make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory");
    Value* next_velocity = momentum > 0.0 ? make_array_checked(interp, params.array_val.length, "ml/optimizer-step: out of memory") : null;
    if (updated == null || updated.tag == ERROR || next_square == null || next_square.tag == ERROR || (next_velocity != null && next_velocity.tag == ERROR)) return updated != null && updated.tag == ERROR ? updated : (next_square != null && next_square.tag == ERROR ? next_square : next_velocity);
    for (usz i = 0; i < params.array_val.length; i++) {
        Value* child_params = null;
        Value* child_square = null;
        Value* child_velocity = null;
        Value* err = ml_rmsprop_step_tree(params.array_val.items[i], grads.array_val.items[i], has_square ? square.array_val.items[i] : null, has_velocity ? velocity.array_val.items[i] : null, learning_rate, alpha, epsilon, momentum, weight_decay, interp, &child_params, &child_square, &child_velocity);
        if (err != null) return err;
        updated.array_val.items[i] = child_params;
        next_square.array_val.items[i] = child_square;
        updated.array_val.length++;
        next_square.array_val.length++;
        if (next_velocity != null) {
            next_velocity.array_val.items[i] = child_velocity;
            next_velocity.array_val.length++;
        }
    }
    *out_params = updated;
    *out_square = next_square;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_rmsprop_step_dict(Value* params, Value* grads, Value* square, Value* velocity, double learning_rate, double alpha, double epsilon, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_square, Value** out_velocity) {
    if (grads == null || grads.tag != HASHMAP || grads.hashmap_val == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter and gradient trees must have matching structure");
    bool has_square = square != null && !is_nil(square);
    bool has_velocity = momentum > 0.0 && velocity != null && !is_nil(velocity);
    if (has_square && (square.tag != HASHMAP || square.hashmap_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp square-average tree must match the parameter tree");
    if (has_velocity && (velocity.tag != HASHMAP || velocity.hashmap_val == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp velocity tree must match the parameter tree");
    if (params.hashmap_val.count != grads.hashmap_val.count || (has_square && params.hashmap_val.count != square.hashmap_val.count) || (has_velocity && params.hashmap_val.count != velocity.hashmap_val.count)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
    Value* updated = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    Value* next_square = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    Value* next_velocity = momentum > 0.0 ? nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2) : null;
    if (updated == null || updated.tag == ERROR || next_square == null || next_square.tag == ERROR || (next_velocity != null && next_velocity.tag == ERROR)) return updated != null && updated.tag == ERROR ? updated : (next_square != null && next_square.tag == ERROR ? next_square : next_velocity);
    for (uint i = 0; i < params.hashmap_val.capacity; i++) {
        HashEntry* entry = &params.hashmap_val.entries[i];
        if (entry.key == null) continue;
        Value* grad_leaf = hashmap_get(grads.hashmap_val, entry.key);
        Value* square_leaf = has_square ? hashmap_get(square.hashmap_val, entry.key) : null;
        Value* velocity_leaf = has_velocity ? hashmap_get(velocity.hashmap_val, entry.key) : null;
        if (grad_leaf == null || (has_square && square_leaf == null) || (has_velocity && velocity_leaf == null)) return ml_optimizer_shape_mismatch(interp, "ml/optimizer-step: dictionary optimizer trees must have the same keys");
        Value* child_params = null;
        Value* child_square = null;
        Value* child_velocity = null;
        Value* err = ml_rmsprop_step_tree(entry.value, grad_leaf, square_leaf, velocity_leaf, learning_rate, alpha, epsilon, momentum, weight_decay, interp, &child_params, &child_square, &child_velocity);
        if (err != null) return err;
        if (!hashmap_set_checked(updated.hashmap_val, entry.key, child_params, interp) ||
            !hashmap_set_checked(next_square.hashmap_val, entry.key, child_square, interp) ||
            (next_velocity != null && !hashmap_set_checked(next_velocity.hashmap_val, entry.key, child_velocity, interp))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/optimizer-step: failed to allocate result dictionary", null);
        }
    }
    *out_params = updated;
    *out_square = next_square;
    *out_velocity = next_velocity;
    return null;
}

fn Value* ml_rmsprop_step_tree(Value* params, Value* grads, Value* square, Value* velocity, double learning_rate, double alpha, double epsilon, double momentum, double weight_decay, Interp* interp, Value** out_params, Value** out_square, Value** out_velocity) {
    if (params == null || grads == null || out_params == null || out_square == null || out_velocity == null) return ml_optimizer_invalid(interp, "ml/optimizer-step: optimizer tree state is missing");
    if (is_tensor(params)) {
        if (!is_tensor(grads)) return ml_optimizer_invalid(interp, "ml/optimizer-step: gradient leaves must be tensors");
        return ml_rmsprop_step_tensor(params, grads, square, velocity, learning_rate, alpha, epsilon, momentum, weight_decay, interp, out_params, out_square, out_velocity);
    }
    if (params.tag == ARRAY && params.array_val != null) return ml_rmsprop_step_array(params, grads, square, velocity, learning_rate, alpha, epsilon, momentum, weight_decay, interp, out_params, out_square, out_velocity);
    if (params.tag == HASHMAP && params.hashmap_val != null) return ml_rmsprop_step_dict(params, grads, square, velocity, learning_rate, alpha, epsilon, momentum, weight_decay, interp, out_params, out_square, out_velocity);
    return ml_optimizer_invalid(interp, "ml/optimizer-step: parameter tree leaves must be tensors");
}

fn Value* prim_ml_optimizer_step(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 4) return primitive_arity_raise(interp, "ml/optimizer-step", "4 arguments: spec parameters gradients state", args.len);
    if (args[0].tag != HASHMAP || args[0].hashmap_val == null) return primitive_arg_type_raise(interp, "ml/optimizer-step", 0, "Dictionary optimizer spec", args[0]);
    if (args[3].tag != HASHMAP || args[3].hashmap_val == null) return primitive_arg_type_raise(interp, "ml/optimizer-step", 3, "Dictionary optimizer state", args[3]);
    Value* kind = nn_dict_get(args[0], interp, "kind");
    double learning_rate = 0.0;
    Value* err = ml_optimizer_parse_spec_number(args[0], interp, "learning-rate", true, 0.0, &learning_rate);
    if (err != null) return err;
    Value* effective_grads = args[2];
    Value* clip_value = nn_dict_get(args[0], interp, "clip-norm");
    if (clip_value != null) {
        double clip_norm = 0.0;
        if (!try_numeric_to_double(clip_value, &clip_norm) || !conversion_double_is_finite(clip_norm) || clip_norm < 0.0) return ml_optimizer_invalid(interp, "ml/optimizer-step: clip-norm must be a non-negative finite number");
        err = ml_clip_gradient_tree(args[2], clip_norm, interp, &effective_grads);
        if (err != null) return err;
    }

    if (nn_symbol_is(kind, interp, "sgd")) {
        double momentum = 0.0;
        double weight_decay = 0.0;
        err = ml_optimizer_parse_spec_number(args[0], interp, "momentum", false, 0.0, &momentum);
        if (err != null) return err;
        err = ml_optimizer_parse_spec_number(args[0], interp, "weight-decay", false, 0.0, &weight_decay);
        if (err != null) return err;
        if (learning_rate < 0.0 || momentum < 0.0 || momentum >= 1.0 || weight_decay < 0.0) return ml_optimizer_invalid(interp, "ml/optimizer-step: learning-rate and weight-decay must be non-negative, and momentum must be in [0, 1)");
        Value* velocity = nn_dict_get(args[3], interp, "velocity");
        Value* updated_params = null;
        Value* next_velocity = null;
        err = ml_optimizer_step_tree(args[1], effective_grads, velocity, learning_rate, momentum, weight_decay, interp, &updated_params, &next_velocity);
        if (err != null) return err;
        Value* next_state = nn_make_dict(interp, 4);
        if (next_state == null || next_state.tag == ERROR) return next_state;
        if (next_velocity != null) {
            err = nn_dict_set(next_state, interp, "velocity", next_velocity);
            if (err != null) return err;
        }
        Value* result = nn_make_dict(interp, 8);
        if (result == null || result.tag == ERROR) return result;
        err = nn_dict_set_symbol(result, interp, "kind", "optimizer-step");
        if (err != null) return err;
        err = nn_dict_set_symbol(result, interp, "optimizer", "sgd");
        if (err != null) return err;
        err = nn_dict_set(result, interp, "parameters", updated_params);
        if (err != null) return err;
        err = nn_dict_set(result, interp, "state", next_state);
        if (err != null) return err;
        return result;
    }

    if (nn_symbol_is(kind, interp, "rmsprop")) {
        double alpha = 0.99;
        double epsilon = 0.00000001;
        double momentum = 0.0;
        double weight_decay = 0.0;
        err = ml_optimizer_parse_spec_number(args[0], interp, "alpha", false, 0.99, &alpha);
        if (err != null) return err;
        err = ml_optimizer_parse_spec_number(args[0], interp, "epsilon", false, 0.00000001, &epsilon);
        if (err != null) return err;
        err = ml_optimizer_parse_spec_number(args[0], interp, "momentum", false, 0.0, &momentum);
        if (err != null) return err;
        err = ml_optimizer_parse_spec_number(args[0], interp, "weight-decay", false, 0.0, &weight_decay);
        if (err != null) return err;
        if (learning_rate < 0.0 || alpha < 0.0 || alpha >= 1.0 || epsilon <= 0.0 || momentum < 0.0 || momentum >= 1.0 || weight_decay < 0.0) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp learning-rate, momentum, and weight-decay must be non-negative, alpha and momentum must be in [0, 1), and epsilon must be positive");
        long current_step = 0;
        Value* step_value = nn_dict_get(args[3], interp, "step");
        if (step_value != null) {
            if (!is_int(step_value) || step_value.int_val < 0 || step_value.int_val == long.max) return ml_optimizer_invalid(interp, "ml/optimizer-step: RMSProp step state must be a non-negative integer below integer max");
            current_step = step_value.int_val;
        }
        Value* updated_params = null;
        Value* next_square = null;
        Value* next_velocity = null;
        err = ml_rmsprop_step_tree(args[1], effective_grads, nn_dict_get(args[3], interp, "square-average"), nn_dict_get(args[3], interp, "velocity"), learning_rate, alpha, epsilon, momentum, weight_decay, interp, &updated_params, &next_square, &next_velocity);
        if (err != null) return err;
        Value* next_state = nn_make_dict(interp, 8);
        if (next_state == null || next_state.tag == ERROR) return next_state;
        err = nn_dict_set(next_state, interp, "square-average", next_square);
        if (err != null) return err;
        if (next_velocity != null) {
            err = nn_dict_set(next_state, interp, "velocity", next_velocity);
            if (err != null) return err;
        }
        err = nn_dict_set(next_state, interp, "step", make_int(interp, current_step + 1));
        if (err != null) return err;
        Value* result = nn_make_dict(interp, 8);
        if (result == null || result.tag == ERROR) return result;
        err = nn_dict_set_symbol(result, interp, "kind", "optimizer-step");
        if (err != null) return err;
        err = nn_dict_set_symbol(result, interp, "optimizer", "rmsprop");
        if (err != null) return err;
        err = nn_dict_set(result, interp, "parameters", updated_params);
        if (err != null) return err;
        err = nn_dict_set(result, interp, "state", next_state);
        if (err != null) return err;
        return result;
    }

    bool is_adam = nn_symbol_is(kind, interp, "adam");
    bool is_adamw = nn_symbol_is(kind, interp, "adamw");
    if (!is_adam && !is_adamw) return ml_optimizer_invalid(interp, "ml/optimizer-step: unsupported optimizer kind");
    double beta1 = 0.9;
    double beta2 = 0.999;
    double epsilon = 0.00000001;
    double weight_decay = 0.0;
    err = ml_optimizer_parse_spec_number(args[0], interp, "beta1", false, 0.9, &beta1);
    if (err != null) return err;
    err = ml_optimizer_parse_spec_number(args[0], interp, "beta2", false, 0.999, &beta2);
    if (err != null) return err;
    err = ml_optimizer_parse_spec_number(args[0], interp, "epsilon", false, 0.00000001, &epsilon);
    if (err != null) return err;
    err = ml_optimizer_parse_spec_number(args[0], interp, "weight-decay", false, 0.0, &weight_decay);
    if (err != null) return err;
    if (learning_rate < 0.0 || beta1 < 0.0 || beta1 >= 1.0 || beta2 < 0.0 || beta2 >= 1.0 || epsilon <= 0.0 || weight_decay < 0.0) {
        return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam learning-rate and weight-decay must be non-negative, beta values must be in [0, 1), and epsilon must be positive");
    }
    Value* first = nn_dict_get(args[3], interp, "first-moment");
    Value* second = nn_dict_get(args[3], interp, "second-moment");
    if ((first == null) != (second == null)) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam state must provide both first-moment and second-moment");
    long current_step = 0;
    Value* step_value = nn_dict_get(args[3], interp, "step");
    if (step_value != null) {
        if (!is_int(step_value) || step_value.int_val < 0 || step_value.int_val == long.max) return ml_optimizer_invalid(interp, "ml/optimizer-step: Adam step state must be a non-negative integer below integer max");
        current_step = step_value.int_val;
    }
    long next_step = current_step + 1;
    Value* updated_params = null;
    Value* next_first = null;
    Value* next_second = null;
    err = ml_adam_step_tree(args[1], effective_grads, first, second, learning_rate, beta1, beta2, epsilon, weight_decay, is_adamw, next_step, interp, &updated_params, &next_first, &next_second);
    if (err != null) return err;
    Value* next_state = nn_make_dict(interp, 8);
    if (next_state == null || next_state.tag == ERROR) return next_state;
    err = nn_dict_set(next_state, interp, "first-moment", next_first);
    if (err != null) return err;
    err = nn_dict_set(next_state, interp, "second-moment", next_second);
    if (err != null) return err;
    err = nn_dict_set(next_state, interp, "step", make_int(interp, next_step));
    if (err != null) return err;
    Value* result = nn_make_dict(interp, 8);
    if (result == null || result.tag == ERROR) return result;
    err = nn_dict_set_symbol(result, interp, "kind", "optimizer-step");
    if (err != null) return err;
    err = nn_dict_set_symbol(result, interp, "optimizer", is_adamw ? "adamw" : "adam");
    if (err != null) return err;
    err = nn_dict_set(result, interp, "parameters", updated_params);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "state", next_state);
    if (err != null) return err;
    return result;
}
