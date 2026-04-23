module lisp;

import std::core::mem;

extern fn double ml_grad_c_exp(double x) @extern("exp");
extern fn double ml_grad_c_log(double x) @extern("log");
extern fn double ml_grad_c_tanh(double x) @extern("tanh");

fn Value* ml_grad_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn bool ml_grad_touches_vulkan(Value* value) @inline {
    return value != null && tensor_expr_has_device(value, TENSOR_DEVICE_VULKAN);
}

fn Value* ml_grad_required_tensor(Value* spec, Interp* interp, char[] key) {
    Value* value = nn_dict_get(spec, interp, key);
    if (value == null || !is_tensor(value)) return ml_grad_invalid(interp, "ml/grad: gradient spec requires Tensor fields");
    return value;
}

fn Value* ml_grad_resolve_cpu(Value* value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (ml_grad_touches_vulkan(value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward is not supported for this operation", null);
    }
    return tensor_expr_resolve_concrete_cpu(
        value,
        out_tensor,
        out_value,
        interp,
        label,
        "ml/grad: backend kernel is not implemented for non-CPU tensors"
    );
}

fn Value* ml_grad_make_tensor(Interp* interp, TensorDType dtype, usz[] shape, char[] message) {
    Value* out = make_tensor_uninitialized_checked(interp, dtype, shape, message);
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != TENSOR || !tensor_has_valid_storage(out.tensor_val)) return tensor_runtime_raise(interp, message);
    return out;
}

fn Value* ml_sgd_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn Value* ml_sgd_shape_mismatch(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", message, null);
}

fn bool ml_sgd_tensor_shapes_match(TensorVal* left, TensorVal* right) {
    if (left == null || right == null || left.rank != right.rank || left.element_count != right.element_count) return false;
    for (usz axis = 0; axis < left.rank; axis++) {
        if (left.shape[axis] != right.shape[axis]) return false;
    }
    return true;
}

fn Value* ml_sgd_resolve_cpu(Value* value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (ml_grad_touches_vulkan(value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/sgd-step: mixed-device SGD operands are not supported", null);
    }
    return tensor_expr_resolve_concrete_cpu(
        value,
        out_tensor,
        out_value,
        interp,
        label,
        "ml/sgd-step: backend kernel is not implemented for non-CPU tensors"
    );
}

fn Value* ml_sgd_step_tensor_try_device(
    Value* params_arg,
    Value* grads_arg,
    double learning_rate,
    Interp* interp,
    Value** out_value,
    bool* out_attempted
) {
    if (out_value != null) *out_value = null;
    if (out_attempted != null) *out_attempted = false;

    Value* updated = null;
    Value* ignored_velocity = null;
    bool cuda_attempted = false;
    Value* cuda_err = ml_optimizer_step_tensor_try_cuda(
        params_arg,
        grads_arg,
        null,
        learning_rate,
        0.0,
        0.0,
        interp,
        &updated,
        &ignored_velocity,
        &cuda_attempted
    );
    if (cuda_err != null) return cuda_err;
    if (cuda_attempted) {
        if (out_attempted != null) *out_attempted = true;
        if (out_value != null) *out_value = updated;
        return updated == null
            ? tensor_runtime_raise(interp, "ml/sgd-step: CUDA SGD kernel did not return a result")
            : null;
    }

    bool vulkan_attempted = false;
    Value* vulkan_err = ml_optimizer_step_tensor_try_vulkan(
        params_arg,
        grads_arg,
        null,
        learning_rate,
        0.0,
        0.0,
        interp,
        &updated,
        &ignored_velocity,
        &vulkan_attempted
    );
    if (vulkan_err != null) return vulkan_err;
    if (vulkan_attempted) {
        if (out_attempted != null) *out_attempted = true;
        if (out_value != null) *out_value = updated;
        return updated == null
            ? tensor_runtime_raise(interp, "ml/sgd-step: Vulkan SGD kernel did not return a result")
            : null;
    }

    return null;
}

fn Value* ml_sgd_step_tensor(Value* params_arg, Value* grads_arg, double learning_rate, Interp* interp) {
    Value* device_result = null;
    bool device_attempted = false;
    Value* device_err = ml_sgd_step_tensor_try_device(
        params_arg,
        grads_arg,
        learning_rate,
        interp,
        &device_result,
        &device_attempted
    );
    if (device_err != null) return device_err;
    if (device_attempted) return device_result;

    TensorVal* params = null;
    TensorVal* grads = null;
    Value* params_value = null;
    Value* grads_value = null;
    Value* err = ml_sgd_resolve_cpu(params_arg, &params, &params_value, interp, "ml/sgd-step: parameter tensor backing storage is missing");
    if (err != null) return err;
    defer if (params_value != null && params_value != params_arg) boundary_cleanup_materialized_value(null, params_value, interp.current_scope);
    err = ml_sgd_resolve_cpu(grads_arg, &grads, &grads_value, interp, "ml/sgd-step: gradient tensor backing storage is missing");
    if (err != null) return err;
    defer if (grads_value != null && grads_value != grads_arg) boundary_cleanup_materialized_value(null, grads_value, interp.current_scope);

    if (params.dtype != grads.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/sgd-step: parameter and gradient dtypes must match", null);
    }
    if (!ml_reduction_supported_dtype(params.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/sgd-step: only Float64 and Float32 tensors are supported", null);
    }
    if (!ml_sgd_tensor_shapes_match(params, grads)) return ml_sgd_shape_mismatch(interp, "ml/sgd-step: parameter and gradient shapes must match");
    if (params.element_count == 0) return ml_sgd_shape_mismatch(interp, "ml/sgd-step: empty parameter tensors are not supported");
    if (!tensor_dense_row_major(params) || !tensor_dense_row_major(grads)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/sgd-step: currently supports dense row-major tensors", null);
    }
    if (params.element_count > usz.max / double.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/sgd-step: parameter tensor is too large", null);
    }

    double* values = (double*)mem::malloc(params.element_count * double.sizeof);
    if (values == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/sgd-step: failed to allocate update buffer", null);
    }
    defer mem::free(values);
    for (usz i = 0; i < params.element_count; i++) {
        double param_value = 0.0;
        double grad_value = 0.0;
        if (!ml_reduction_read(params, i, &param_value) || !ml_reduction_read(grads, i, &grad_value)) {
            return tensor_runtime_raise(interp, "ml/sgd-step: tensor index is invalid");
        }
        values[i] = param_value - (learning_rate * grad_value);
    }

    Value* out = ml_grad_make_tensor(interp, params.dtype, params.shape[0:params.rank], "ml/sgd-step: failed to allocate updated tensor");
    if (out == null || out.tag == ERROR) return out;
    err = ml_reduction_write_result(out.tensor_val, values, interp, "ml/sgd-step: Float32 parameter update is not representable");
    if (err != null) return err;
    return out;
}

fn Value* ml_sgd_step_array(Value* params, Value* grads, double learning_rate, Interp* interp) {
    if (grads == null || grads.tag != ARRAY || grads.array_val == null) return ml_sgd_invalid(interp, "ml/sgd-step: parameter and gradient trees must have matching structure");
    if (params.array_val.length != grads.array_val.length) return ml_sgd_shape_mismatch(interp, "ml/sgd-step: parameter and gradient arrays must have the same length");
    Value* out = make_array_checked(interp, params.array_val.length, "ml/sgd-step: out of memory");
    if (out == null || out.tag == ERROR) return out;
    for (usz i = 0; i < params.array_val.length; i++) {
        Value* updated = ml_sgd_step_tree(params.array_val.items[i], grads.array_val.items[i], learning_rate, interp);
        if (updated == null || updated.tag == ERROR) return updated;
        out.array_val.items[i] = updated;
        out.array_val.length++;
    }
    return out;
}

fn Value* ml_sgd_step_dict(Value* params, Value* grads, double learning_rate, Interp* interp) {
    if (grads == null || grads.tag != HASHMAP || grads.hashmap_val == null) return ml_sgd_invalid(interp, "ml/sgd-step: parameter and gradient trees must have matching structure");
    if (params.hashmap_val.count != grads.hashmap_val.count) return ml_sgd_shape_mismatch(interp, "ml/sgd-step: parameter and gradient dictionaries must have the same keys");
    Value* out = nn_make_dict(interp, params.hashmap_val.count == 0 ? 2 : params.hashmap_val.count * 2);
    if (out == null || out.tag == ERROR) return out;
    for (uint i = 0; i < params.hashmap_val.capacity; i++) {
        HashEntry* entry = &params.hashmap_val.entries[i];
        if (entry.key == null) continue;
        Value* grad_leaf = hashmap_get(grads.hashmap_val, entry.key);
        if (grad_leaf == null) return ml_sgd_shape_mismatch(interp, "ml/sgd-step: parameter and gradient dictionaries must have the same keys");
        Value* updated = ml_sgd_step_tree(entry.value, grad_leaf, learning_rate, interp);
        if (updated == null || updated.tag == ERROR) return updated;
        if (!hashmap_set_checked(out.hashmap_val, entry.key, updated, interp)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/sgd-step: failed to allocate result dictionary", null);
        }
    }
    return out;
}

fn Value* ml_sgd_step_tree(Value* params, Value* grads, double learning_rate, Interp* interp) {
    if (params == null || grads == null) return ml_sgd_invalid(interp, "ml/sgd-step: parameter and gradient trees are required");
    if (is_tensor(params)) {
        if (!is_tensor(grads)) return ml_sgd_invalid(interp, "ml/sgd-step: gradient leaves must be tensors");
        return ml_sgd_step_tensor(params, grads, learning_rate, interp);
    }
    if (params.tag == ARRAY && params.array_val != null) return ml_sgd_step_array(params, grads, learning_rate, interp);
    if (params.tag == HASHMAP && params.hashmap_val != null) return ml_sgd_step_dict(params, grads, learning_rate, interp);
    return ml_sgd_invalid(interp, "ml/sgd-step: parameter tree leaves must be tensors");
}

fn bool ml_grad_activation_supported(SymbolId activation, Interp* interp) @inline {
    return nn_symbol_name_is(activation, interp, "relu") ||
        nn_symbol_name_is(activation, interp, "leaky-relu") ||
        nn_symbol_name_is(activation, interp, "sigmoid") ||
        nn_symbol_name_is(activation, interp, "tanh") ||
        nn_symbol_name_is(activation, interp, "gelu");
}

fn bool ml_grad_activation_accepts_dtype(SymbolId activation, TensorDType dtype, Interp* interp) @inline {
    if (nn_symbol_name_is(activation, interp, "relu") ||
        nn_symbol_name_is(activation, interp, "leaky-relu")) {
        return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
    }
    return dtype == TENSOR_DTYPE_FLOAT32;
}

fn bool ml_grad_activation_eval(SymbolId activation, Interp* interp, double x, double leaky_relu_slope, double* out_value, double* out_derivative) {
    if (out_value == null || out_derivative == null) return false;
    if (nn_symbol_name_is(activation, interp, "relu")) {
        *out_value = x > 0.0 ? x : 0.0;
        *out_derivative = x > 0.0 ? 1.0 : 0.0;
        return true;
    }
    if (nn_symbol_name_is(activation, interp, "leaky-relu")) {
        *out_value = x > 0.0 ? x : leaky_relu_slope * x;
        *out_derivative = x > 0.0 ? 1.0 : leaky_relu_slope;
        return true;
    }
    if (nn_symbol_name_is(activation, interp, "sigmoid")) {
        double y = 1.0 / (1.0 + ml_grad_c_exp(-x));
        *out_value = y;
        *out_derivative = y * (1.0 - y);
        return true;
    }
    if (nn_symbol_name_is(activation, interp, "tanh")) {
        double y = ml_grad_c_tanh(x);
        *out_value = y;
        *out_derivative = 1.0 - (y * y);
        return true;
    }
    if (nn_symbol_name_is(activation, interp, "gelu")) {
        double c = ML_GELU_SQRT_TWO_OVER_PI;
        double k = ML_GELU_CUBE_SCALE;
        double x2 = x * x;
        double inner = c * (x + (k * x * x2));
        double t = ml_grad_c_tanh(inner);
        *out_value = 0.5 * x * (1.0 + t);
        *out_derivative = (0.5 * (1.0 + t)) + (0.5 * x * (1.0 - (t * t)) * c * (1.0 + (3.0 * k * x2)));
        return true;
    }
    return false;
}

fn Value* ml_grad_tape_node(Interp* interp, usz id, char[] op, char[] role) {
    Value* node = nn_make_dict(interp, 6);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = nn_dict_set(node, interp, "id", make_int(interp, (isz)id))) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "op", op)) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "role", role)) return err;
    return node;
}

fn Value* ml_grad_tape_push_node(Value* nodes, Interp* interp, usz id, char[] op, char[] role) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || nodes.array_val.length >= nodes.array_val.capacity) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: failed to allocate tape node", null);
    }
    Value* node = ml_grad_tape_node(interp, id, op, role);
    if (node == null || node.tag == ERROR) return node;
    nodes.array_val.items[nodes.array_val.length] = node;
    nodes.array_val.length++;
    return null;
}

fn Value* ml_grad_make_tape(
    Interp* interp,
    char[] source_name,
    TensorDType dtype,
    bool has_bias,
    bool has_activation,
    SymbolId activation,
    bool softmax_cross_entropy
) {
    usz node_capacity = 4;
    if (has_bias) node_capacity++;
    if (has_activation) node_capacity++;
    if (softmax_cross_entropy) node_capacity++;
    Value* nodes = make_array_checked(interp, node_capacity, "ml/grad: failed to allocate tape nodes");
    if (nodes == null || nodes.tag == ERROR) return nodes;
    usz id = 0;
    if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "leaf", "input")) return err;
    if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "leaf", "weights")) return err;
    if (has_bias) {
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "leaf", "bias")) return err;
    }
    if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "linear", "forward")) return err;
    if (has_activation) {
        char[] activation_name = nn_symbol_name_is(activation, interp, "leaky-relu")
            ? "leaky-relu"
            : nn_symbol_name_is(activation, interp, "relu")
                ? "relu"
                : nn_symbol_name_is(activation, interp, "sigmoid")
                    ? "sigmoid"
                    : nn_symbol_name_is(activation, interp, "tanh")
                        ? "tanh"
                        : "gelu";
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, activation_name, "activation")) return err;
    }
    if (softmax_cross_entropy) {
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "softmax", "activation")) return err;
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "cross-entropy", "loss")) return err;
    } else {
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "mean-squared-error", "loss")) return err;
    }

    Value* tape = nn_make_dict(interp, 16);
    if (tape == null || tape.tag == ERROR) return tape;
    if (Value* err = nn_dict_set_symbol(tape, interp, "kind", "gradient-tape")) return err;
    if (Value* err = nn_dict_set(tape, interp, "version", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "policy", "metadata-only")) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "owner", "scope-region")) return err;
    if (Value* err = nn_dict_set(tape, interp, "retains-handles", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "device", "cpu")) return err;
    if (Value* err = nn_dict_set(tape, interp, "dtype", make_symbol(interp, tensor_dtype_symbol(dtype, interp)))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "source", source_name)) return err;
    if (has_activation) {
        if (Value* err = nn_dict_set(tape, interp, "activation", make_symbol(interp, activation))) return err;
    }
    if (Value* err = nn_dict_set(tape, interp, "node-count", make_int(interp, (isz)nodes.array_val.length))) return err;
    if (Value* err = nn_dict_set(tape, interp, "nodes", nodes)) return err;
    return tape;
}

fn Value* ml_grad_linear_mse(Value* spec, Interp* interp, bool with_activation) {
    Value* input_arg = ml_grad_required_tensor(spec, interp, "input");
    if (input_arg == null || input_arg.tag == ERROR) return input_arg;
    Value* weights_arg = ml_grad_required_tensor(spec, interp, "weights");
    if (weights_arg == null || weights_arg.tag == ERROR) return weights_arg;
    Value* targets_arg = ml_grad_required_tensor(spec, interp, "targets");
    if (targets_arg == null || targets_arg.tag == ERROR) return targets_arg;
    Value* bias_arg = nn_dict_get(spec, interp, "bias");
    bool has_bias = bias_arg != null && !is_nil(bias_arg);
    if (has_bias && !is_tensor(bias_arg)) return ml_grad_invalid(interp, "ml/grad: bias must be a Tensor when present");
    SymbolId activation = INVALID_SYMBOL_ID;
    if (with_activation) {
        Value* activation_arg = nn_dict_get(spec, interp, "activation");
        if (activation_arg == null || activation_arg.tag != SYMBOL) return ml_grad_invalid(interp, "ml/grad: activation spec requires an activation symbol");
        activation = activation_arg.sym_val;
        if (!ml_grad_activation_supported(activation, interp)) return ml_grad_invalid(interp, "ml/grad: unsupported activation backward rule");
    }
    double leaky_relu_slope = 0.01;
    if (with_activation && nn_symbol_name_is(activation, interp, "leaky-relu")) {
        Value* slope_arg = nn_dict_get(spec, interp, "negative-slope");
        if (slope_arg != null && !is_nil(slope_arg)) {
            if (!try_numeric_to_double(slope_arg, &leaky_relu_slope) || !conversion_double_is_finite(leaky_relu_slope) || leaky_relu_slope < 0.0) {
                return ml_grad_invalid(interp, "ml/grad: leaky-relu negative-slope must be a non-negative finite number");
            }
        }
    }

    TensorVal* input = null;
    TensorVal* weights = null;
    TensorVal* targets = null;
    TensorVal* bias = null;
    Value* input_value = null;
    Value* weights_value = null;
    Value* targets_value = null;
    Value* bias_value = null;
    Value* err = ml_grad_resolve_cpu(input_arg, &input, &input_value, interp, "ml/grad: input tensor backing storage is missing");
    if (err != null) return err;
    defer if (input_value != null && input_value != input_arg) boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    err = ml_grad_resolve_cpu(weights_arg, &weights, &weights_value, interp, "ml/grad: weights tensor backing storage is missing");
    if (err != null) return err;
    defer if (weights_value != null && weights_value != weights_arg) boundary_cleanup_materialized_value(null, weights_value, interp.current_scope);
    err = ml_grad_resolve_cpu(targets_arg, &targets, &targets_value, interp, "ml/grad: targets tensor backing storage is missing");
    if (err != null) return err;
    defer if (targets_value != null && targets_value != targets_arg) boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    if (has_bias) {
        err = ml_grad_resolve_cpu(bias_arg, &bias, &bias_value, interp, "ml/grad: bias tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (bias_value != null && bias_value != bias_arg) boundary_cleanup_materialized_value(null, bias_value, interp.current_scope);

    usz batch_count = 0;
    usz in_features = 0;
    usz out_features = 0;
    err = ml_linear_validate(input, weights, bias, &batch_count, &in_features, &out_features, false, interp);
    if (err != null) return err;
    if (batch_count == 0 || in_features == 0 || out_features == 0 || targets.element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: cannot differentiate empty linear mean-squared-error tensors", null);
    }
    if (targets.dtype != input.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/grad: target dtype is incompatible", null);
    }
    if (with_activation && !ml_grad_activation_accepts_dtype(activation, input.dtype, interp)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: activation backward dtype is not implemented", null);
    }
    float leaky_relu_slope_f32 = 0.0f;
    if (with_activation && nn_symbol_name_is(activation, interp, "leaky-relu") &&
        input.dtype == TENSOR_DTYPE_FLOAT32 && !tensor_double_to_float32(leaky_relu_slope, &leaky_relu_slope_f32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/grad: leaky-relu negative-slope must be representable as Float32", null);
    }
    if (targets.rank != input.rank || targets.shape[targets.rank - 1] != out_features) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: target shape must match linear output shape", null);
    }
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        if (targets.shape[axis] != input.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: target shape must match linear output shape", null);
        }
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(weights) || !tensor_dense_row_major(targets) || (has_bias && !tensor_dense_row_major(bias))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: currently supports dense row-major tensors", null);
    }

    usz output_count = batch_count * out_features;
    if (output_count > usz.max / double.sizeof || input.element_count > usz.max / double.sizeof ||
        weights.element_count > usz.max / double.sizeof || out_features > usz.max / double.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: gradient buffers are too large", null);
    }
    double* output_values = (double*)mem::malloc(output_count * double.sizeof);
    double* input_grad_values = (double*)mem::malloc(input.element_count * double.sizeof);
    double* weights_grad_values = (double*)mem::malloc(weights.element_count * double.sizeof);
    double* bias_grad_values = (double*)mem::malloc(out_features * double.sizeof);
    if (output_values == null || input_grad_values == null || weights_grad_values == null || bias_grad_values == null) {
        if (output_values != null) mem::free(output_values);
        if (input_grad_values != null) mem::free(input_grad_values);
        if (weights_grad_values != null) mem::free(weights_grad_values);
        if (bias_grad_values != null) mem::free(bias_grad_values);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: failed to allocate gradient buffers", null);
    }
    defer mem::free(output_values);
    defer mem::free(input_grad_values);
    defer mem::free(weights_grad_values);
    defer mem::free(bias_grad_values);
    for (usz i = 0; i < input.element_count; i++) input_grad_values[i] = 0.0;
    for (usz i = 0; i < weights.element_count; i++) weights_grad_values[i] = 0.0;
    for (usz i = 0; i < out_features; i++) bias_grad_values[i] = 0.0;

    double loss = 0.0;
    double inv_count = 1.0 / (double)output_count;
    for (usz batch = 0; batch < batch_count; batch++) {
        usz input_base = batch * in_features;
        usz output_base = batch * out_features;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            double sum = 0.0;
            if (has_bias && !ml_reduction_read(bias, out_axis, &sum)) return tensor_runtime_raise(interp, "ml/grad: bias tensor index is invalid");
            usz weight_base = out_axis * in_features;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                double input_value_read = 0.0;
                double weight_value_read = 0.0;
                if (!ml_reduction_read(input, input_base + in_axis, &input_value_read) ||
                    !ml_reduction_read(weights, weight_base + in_axis, &weight_value_read)) {
                    return tensor_runtime_raise(interp, "ml/grad: linear tensor index is invalid");
                }
                sum += input_value_read * weight_value_read;
            }
            double target_value = 0.0;
            if (!ml_reduction_read(targets, output_base + out_axis, &target_value)) return tensor_runtime_raise(interp, "ml/grad: target tensor index is invalid");
            double output_value_read = sum;
            double activation_derivative = 1.0;
            if (with_activation && !ml_grad_activation_eval(activation, interp, sum, leaky_relu_slope, &output_value_read, &activation_derivative)) {
                return ml_grad_invalid(interp, "ml/grad: unsupported activation backward rule");
            }
            output_values[output_base + out_axis] = output_value_read;
            double diff = output_value_read - target_value;
            loss += diff * diff;
            double upstream = 2.0 * diff * inv_count * activation_derivative;
            bias_grad_values[out_axis] += upstream;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                double input_value_read = 0.0;
                double weight_value_read = 0.0;
                if (!ml_reduction_read(input, input_base + in_axis, &input_value_read) ||
                    !ml_reduction_read(weights, weight_base + in_axis, &weight_value_read)) {
                    return tensor_runtime_raise(interp, "ml/grad: backward tensor index is invalid");
                }
                input_grad_values[input_base + in_axis] += upstream * weight_value_read;
                weights_grad_values[weight_base + in_axis] += upstream * input_value_read;
            }
        }
    }
    loss *= inv_count;

    usz[1] scalar_shape = { 0 };
    Value* loss_value = ml_grad_make_tensor(interp, input.dtype, scalar_shape[0:0], "ml/grad: failed to allocate loss tensor");
    if (loss_value == null || loss_value.tag == ERROR) return loss_value;
    double[1] loss_values = { loss };
    err = ml_reduction_write_result(loss_value.tensor_val, loss_values[..], interp, "ml/grad: Float32 loss is not representable");
    if (err != null) return err;
    Value* output_value = ml_grad_make_tensor(interp, input.dtype, targets.shape[0:targets.rank], "ml/grad: failed to allocate output tensor");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    err = ml_reduction_write_result(output_value.tensor_val, output_values, interp, "ml/grad: Float32 output is not representable");
    if (err != null) return err;
    Value* input_grad = ml_grad_make_tensor(interp, input.dtype, input.shape[0:input.rank], "ml/grad: failed to allocate input gradient tensor");
    if (input_grad == null || input_grad.tag == ERROR) return input_grad;
    err = ml_reduction_write_result(input_grad.tensor_val, input_grad_values, interp, "ml/grad: Float32 input gradient is not representable");
    if (err != null) return err;
    Value* weights_grad = ml_grad_make_tensor(interp, input.dtype, weights.shape[0:weights.rank], "ml/grad: failed to allocate weights gradient tensor");
    if (weights_grad == null || weights_grad.tag == ERROR) return weights_grad;
    err = ml_reduction_write_result(weights_grad.tensor_val, weights_grad_values, interp, "ml/grad: Float32 weights gradient is not representable");
    if (err != null) return err;
    usz[1] bias_shape = { out_features };
    Value* bias_grad = ml_grad_make_tensor(interp, input.dtype, bias_shape[..], "ml/grad: failed to allocate bias gradient tensor");
    if (bias_grad == null || bias_grad.tag == ERROR) return bias_grad;
    err = ml_reduction_write_result(bias_grad.tensor_val, bias_grad_values, interp, "ml/grad: Float32 bias gradient is not representable");
    if (err != null) return err;

    Value* parameter_gradients = nn_make_dict(interp, 4);
    if (parameter_gradients == null || parameter_gradients.tag == ERROR) return parameter_gradients;
    err = nn_dict_set(parameter_gradients, interp, "weights", weights_grad);
    if (err != null) return err;
    err = nn_dict_set(parameter_gradients, interp, "bias", bias_grad);
    if (err != null) return err;
    Value* tape = ml_grad_make_tape(
        interp,
        with_activation ? "linear-activation-mean-squared-error" : "linear-mean-squared-error",
        input.dtype,
        has_bias,
        with_activation,
        activation,
        false
    );
    if (tape == null || tape.tag == ERROR) return tape;

    Value* result = nn_make_dict(interp, 12);
    if (result == null || result.tag == ERROR) return result;
    err = nn_dict_set_symbol(result, interp, "kind", "gradients");
    if (err != null) return err;
    err = nn_dict_set_symbol(result, interp, "source", with_activation ? "linear-activation-mean-squared-error" : "linear-mean-squared-error");
    if (err != null) return err;
    if (with_activation) {
        err = nn_dict_set(result, interp, "activation", make_symbol(interp, activation));
        if (err != null) return err;
        if (nn_symbol_name_is(activation, interp, "leaky-relu")) {
            err = nn_dict_set(result, interp, "negative-slope", input.dtype == TENSOR_DTYPE_FLOAT32 ? make_float32(interp, leaky_relu_slope_f32) : make_double(interp, leaky_relu_slope));
            if (err != null) return err;
        }
    }
    err = nn_dict_set(result, interp, "loss", loss_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "output", output_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "input-gradient", input_grad);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "parameter-gradients", parameter_gradients);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "tape", tape);
    if (err != null) return err;
    return result;
}

fn Value* ml_grad_linear_softmax_cross_entropy(Value* spec, Interp* interp) {
    Value* input_arg = ml_grad_required_tensor(spec, interp, "input");
    if (input_arg == null || input_arg.tag == ERROR) return input_arg;
    Value* weights_arg = ml_grad_required_tensor(spec, interp, "weights");
    if (weights_arg == null || weights_arg.tag == ERROR) return weights_arg;
    Value* targets_arg = ml_grad_required_tensor(spec, interp, "targets");
    if (targets_arg == null || targets_arg.tag == ERROR) return targets_arg;
    Value* bias_arg = nn_dict_get(spec, interp, "bias");
    bool has_bias = bias_arg != null && !is_nil(bias_arg);
    if (has_bias && !is_tensor(bias_arg)) return ml_grad_invalid(interp, "ml/grad: bias must be a Tensor when present");

    TensorVal* input = null;
    TensorVal* weights = null;
    TensorVal* targets = null;
    TensorVal* bias = null;
    Value* input_value = null;
    Value* weights_value = null;
    Value* targets_value = null;
    Value* bias_value = null;
    Value* err = ml_grad_resolve_cpu(input_arg, &input, &input_value, interp, "ml/grad: input tensor backing storage is missing");
    if (err != null) return err;
    defer if (input_value != null && input_value != input_arg) boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    err = ml_grad_resolve_cpu(weights_arg, &weights, &weights_value, interp, "ml/grad: weights tensor backing storage is missing");
    if (err != null) return err;
    defer if (weights_value != null && weights_value != weights_arg) boundary_cleanup_materialized_value(null, weights_value, interp.current_scope);
    err = ml_grad_resolve_cpu(targets_arg, &targets, &targets_value, interp, "ml/grad: targets tensor backing storage is missing");
    if (err != null) return err;
    defer if (targets_value != null && targets_value != targets_arg) boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    if (has_bias) {
        err = ml_grad_resolve_cpu(bias_arg, &bias, &bias_value, interp, "ml/grad: bias tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (bias_value != null && bias_value != bias_arg) boundary_cleanup_materialized_value(null, bias_value, interp.current_scope);

    usz batch_count = 0;
    usz in_features = 0;
    usz out_features = 0;
    err = ml_linear_validate(input, weights, bias, &batch_count, &in_features, &out_features, false, interp);
    if (err != null) return err;
    if (batch_count == 0 || in_features == 0 || out_features == 0 || targets.element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: cannot differentiate empty linear softmax cross-entropy tensors", null);
    }
    if (targets.dtype != input.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/grad: target dtype is incompatible", null);
    }
    if (targets.rank != input.rank || targets.shape[targets.rank - 1] != out_features) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: target shape must match linear output shape", null);
    }
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        if (targets.shape[axis] != input.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: target shape must match linear output shape", null);
        }
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(weights) || !tensor_dense_row_major(targets) || (has_bias && !tensor_dense_row_major(bias))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: currently supports dense row-major tensors", null);
    }

    usz output_count = batch_count * out_features;
    if (output_count > usz.max / double.sizeof || input.element_count > usz.max / double.sizeof ||
        weights.element_count > usz.max / double.sizeof || out_features > usz.max / double.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: gradient buffers are too large", null);
    }
    double* output_values = (double*)mem::malloc(output_count * double.sizeof);
    double* input_grad_values = (double*)mem::malloc(input.element_count * double.sizeof);
    double* weights_grad_values = (double*)mem::malloc(weights.element_count * double.sizeof);
    double* bias_grad_values = (double*)mem::malloc(out_features * double.sizeof);
    if (output_values == null || input_grad_values == null || weights_grad_values == null || bias_grad_values == null) {
        if (output_values != null) mem::free(output_values);
        if (input_grad_values != null) mem::free(input_grad_values);
        if (weights_grad_values != null) mem::free(weights_grad_values);
        if (bias_grad_values != null) mem::free(bias_grad_values);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: failed to allocate gradient buffers", null);
    }
    defer mem::free(output_values);
    defer mem::free(input_grad_values);
    defer mem::free(weights_grad_values);
    defer mem::free(bias_grad_values);
    for (usz i = 0; i < input.element_count; i++) input_grad_values[i] = 0.0;
    for (usz i = 0; i < weights.element_count; i++) weights_grad_values[i] = 0.0;
    for (usz i = 0; i < out_features; i++) bias_grad_values[i] = 0.0;

    double loss = 0.0;
    double inv_batch = 1.0 / (double)batch_count;
    double tolerance = input.dtype == TENSOR_DTYPE_FLOAT32 ? 0.0001 : 0.000000001;
    for (usz batch = 0; batch < batch_count; batch++) {
        usz input_base = batch * in_features;
        usz output_base = batch * out_features;
        double max_logit = 0.0;
        bool seen_logit = false;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            double sum = 0.0;
            if (has_bias && !ml_reduction_read(bias, out_axis, &sum)) return tensor_runtime_raise(interp, "ml/grad: bias tensor index is invalid");
            usz weight_base = out_axis * in_features;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                double input_value_read = 0.0;
                double weight_value_read = 0.0;
                if (!ml_reduction_read(input, input_base + in_axis, &input_value_read) ||
                    !ml_reduction_read(weights, weight_base + in_axis, &weight_value_read)) {
                    return tensor_runtime_raise(interp, "ml/grad: linear tensor index is invalid");
                }
                sum += input_value_read * weight_value_read;
            }
            if (!conversion_double_is_finite(sum)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: logits must be finite", null);
            }
            output_values[output_base + out_axis] = sum;
            if (!seen_logit || sum > max_logit) {
                max_logit = sum;
                seen_logit = true;
            }
        }

        double exp_sum = 0.0;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            exp_sum += ml_grad_c_exp(output_values[output_base + out_axis] - max_logit);
        }

        double target_sum = 0.0;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            double target_value = 0.0;
            if (!ml_reduction_read(targets, output_base + out_axis, &target_value)) return tensor_runtime_raise(interp, "ml/grad: target tensor index is invalid");
            if (!conversion_double_is_finite(target_value) || target_value < -tolerance) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: targets must be probability or one-hot tensors", null);
            }
            double logit = output_values[output_base + out_axis];
            double probability = ml_grad_c_exp(logit - max_logit) / exp_sum;
            output_values[output_base + out_axis] = probability;
            loss += -target_value * (logit - max_logit - ml_grad_c_log(exp_sum));
            target_sum += target_value;
        }
        if (tensor_c_fabs(target_sum - 1.0) > tolerance) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: target probabilities must sum to one along the class axis", null);
        }

        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            double target_value = 0.0;
            if (!ml_reduction_read(targets, output_base + out_axis, &target_value)) return tensor_runtime_raise(interp, "ml/grad: target tensor index is invalid");
            double upstream = (output_values[output_base + out_axis] - target_value) * inv_batch;
            bias_grad_values[out_axis] += upstream;
            usz weight_base = out_axis * in_features;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                double input_value_read = 0.0;
                double weight_value_read = 0.0;
                if (!ml_reduction_read(input, input_base + in_axis, &input_value_read) ||
                    !ml_reduction_read(weights, weight_base + in_axis, &weight_value_read)) {
                    return tensor_runtime_raise(interp, "ml/grad: backward tensor index is invalid");
                }
                input_grad_values[input_base + in_axis] += upstream * weight_value_read;
                weights_grad_values[weight_base + in_axis] += upstream * input_value_read;
            }
        }
    }
    loss *= inv_batch;

    usz[1] scalar_shape = { 0 };
    Value* loss_value = ml_grad_make_tensor(interp, input.dtype, scalar_shape[0:0], "ml/grad: failed to allocate loss tensor");
    if (loss_value == null || loss_value.tag == ERROR) return loss_value;
    double[1] loss_values = { loss };
    err = ml_reduction_write_result(loss_value.tensor_val, loss_values[..], interp, "ml/grad: Float32 loss is not representable");
    if (err != null) return err;
    Value* output_value = ml_grad_make_tensor(interp, input.dtype, targets.shape[0:targets.rank], "ml/grad: failed to allocate output tensor");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    err = ml_reduction_write_result(output_value.tensor_val, output_values, interp, "ml/grad: Float32 output is not representable");
    if (err != null) return err;
    Value* input_grad = ml_grad_make_tensor(interp, input.dtype, input.shape[0:input.rank], "ml/grad: failed to allocate input gradient tensor");
    if (input_grad == null || input_grad.tag == ERROR) return input_grad;
    err = ml_reduction_write_result(input_grad.tensor_val, input_grad_values, interp, "ml/grad: Float32 input gradient is not representable");
    if (err != null) return err;
    Value* weights_grad = ml_grad_make_tensor(interp, input.dtype, weights.shape[0:weights.rank], "ml/grad: failed to allocate weights gradient tensor");
    if (weights_grad == null || weights_grad.tag == ERROR) return weights_grad;
    err = ml_reduction_write_result(weights_grad.tensor_val, weights_grad_values, interp, "ml/grad: Float32 weights gradient is not representable");
    if (err != null) return err;
    usz[1] bias_shape = { out_features };
    Value* bias_grad = ml_grad_make_tensor(interp, input.dtype, bias_shape[..], "ml/grad: failed to allocate bias gradient tensor");
    if (bias_grad == null || bias_grad.tag == ERROR) return bias_grad;
    err = ml_reduction_write_result(bias_grad.tensor_val, bias_grad_values, interp, "ml/grad: Float32 bias gradient is not representable");
    if (err != null) return err;

    Value* parameter_gradients = nn_make_dict(interp, 4);
    if (parameter_gradients == null || parameter_gradients.tag == ERROR) return parameter_gradients;
    err = nn_dict_set(parameter_gradients, interp, "weights", weights_grad);
    if (err != null) return err;
    err = nn_dict_set(parameter_gradients, interp, "bias", bias_grad);
    if (err != null) return err;
    Value* tape = ml_grad_make_tape(
        interp,
        "linear-softmax-cross-entropy",
        input.dtype,
        has_bias,
        false,
        INVALID_SYMBOL_ID,
        true
    );
    if (tape == null || tape.tag == ERROR) return tape;

    Value* result = nn_make_dict(interp, 12);
    if (result == null || result.tag == ERROR) return result;
    err = nn_dict_set_symbol(result, interp, "kind", "gradients");
    if (err != null) return err;
    err = nn_dict_set_symbol(result, interp, "source", "linear-softmax-cross-entropy");
    if (err != null) return err;
    err = nn_dict_set(result, interp, "loss", loss_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "output", output_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "input-gradient", input_grad);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "parameter-gradients", parameter_gradients);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "tape", tape);
    if (err != null) return err;
    return result;
}

fn Value* prim_ml_grad(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/grad", "1 argument: gradient spec Dictionary", args.len);
    if (args[0].tag != HASHMAP || args[0].hashmap_val == null) {
        return primitive_arg_type_raise(interp, "ml/grad", 0, "Dictionary gradient spec", args[0]);
    }
    Value* kind = nn_dict_get(args[0], interp, "kind");
    if (nn_symbol_is(kind, interp, "linear-mean-squared-error")) return ml_grad_linear_mse(args[0], interp, false);
    if (nn_symbol_is(kind, interp, "linear-activation-mean-squared-error")) return ml_grad_linear_mse(args[0], interp, true);
    if (nn_symbol_is(kind, interp, "linear-softmax-cross-entropy")) return ml_grad_linear_softmax_cross_entropy(args[0], interp);
    if (nn_symbol_is(kind, interp, "tensor-mean-squared-error")) return ml_grad_tensor_mse(args[0], interp);
    if (nn_symbol_is(kind, interp, "tensor-softmax-cross-entropy")) return ml_grad_tensor_softmax_cross_entropy(args[0], interp);
    return ml_grad_invalid(interp, "ml/grad: unsupported gradient spec kind");
}

fn Value* prim_ml_sgd_step(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return primitive_arity_raise(interp, "ml/sgd-step", "3 arguments: parameters gradients learning-rate", args.len);
    double learning_rate = 0.0;
    if (!try_numeric_to_double(args[2], &learning_rate) || !conversion_double_is_finite(learning_rate) || learning_rate < 0.0) {
        return primitive_arg_type_raise(interp, "ml/sgd-step", 2, "non-negative finite number", args[2]);
    }
    return ml_sgd_step_tree(args[0], args[1], learning_rate, interp);
}
