module lisp;

import std::core::mem;

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

fn Value* prim_ml_optimizer_step(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 4) return primitive_arity_raise(interp, "ml/optimizer-step", "4 arguments: spec parameters gradients state", args.len);
    if (args[0].tag != HASHMAP || args[0].hashmap_val == null) return primitive_arg_type_raise(interp, "ml/optimizer-step", 0, "Dictionary optimizer spec", args[0]);
    if (args[3].tag != HASHMAP || args[3].hashmap_val == null) return primitive_arg_type_raise(interp, "ml/optimizer-step", 3, "Dictionary optimizer state", args[3]);
    Value* kind = nn_dict_get(args[0], interp, "kind");
    if (!nn_symbol_is(kind, interp, "sgd")) return ml_optimizer_invalid(interp, "ml/optimizer-step: unsupported optimizer kind");
    double learning_rate = 0.0;
    double momentum = 0.0;
    double weight_decay = 0.0;
    Value* err = ml_optimizer_parse_spec_number(args[0], interp, "learning-rate", true, 0.0, &learning_rate);
    if (err != null) return err;
    err = ml_optimizer_parse_spec_number(args[0], interp, "momentum", false, 0.0, &momentum);
    if (err != null) return err;
    err = ml_optimizer_parse_spec_number(args[0], interp, "weight-decay", false, 0.0, &weight_decay);
    if (err != null) return err;
    if (learning_rate < 0.0 || momentum < 0.0 || momentum >= 1.0 || weight_decay < 0.0) return ml_optimizer_invalid(interp, "ml/optimizer-step: learning-rate and weight-decay must be non-negative, and momentum must be in [0, 1)");
    Value* velocity = nn_dict_get(args[3], interp, "velocity");
    Value* updated_params = null;
    Value* next_velocity = null;
    err = ml_optimizer_step_tree(args[1], args[2], velocity, learning_rate, momentum, weight_decay, interp, &updated_params, &next_velocity);
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
