module lisp;

import std::core::mem;

extern fn double ml_grad_c_exp(double x) @extern("exp");
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
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward kernels are not implemented", null);
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

fn bool ml_grad_activation_supported(SymbolId activation, Interp* interp) @inline {
    return nn_symbol_name_is(activation, interp, "relu") ||
        nn_symbol_name_is(activation, interp, "sigmoid") ||
        nn_symbol_name_is(activation, interp, "tanh") ||
        nn_symbol_name_is(activation, interp, "gelu");
}

fn bool ml_grad_activation_accepts_dtype(SymbolId activation, TensorDType dtype, Interp* interp) @inline {
    if (nn_symbol_name_is(activation, interp, "relu")) return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
    return dtype == TENSOR_DTYPE_FLOAT32;
}

fn bool ml_grad_activation_eval(SymbolId activation, Interp* interp, double x, double* out_value, double* out_derivative) {
    if (out_value == null || out_derivative == null) return false;
    if (nn_symbol_name_is(activation, interp, "relu")) {
        *out_value = x > 0.0 ? x : 0.0;
        *out_derivative = x > 0.0 ? 1.0 : 0.0;
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
        double c = 0.7978845608028654;
        double k = 0.044715;
        double x2 = x * x;
        double inner = c * (x + (k * x * x2));
        double t = ml_grad_c_tanh(inner);
        *out_value = 0.5 * x * (1.0 + t);
        *out_derivative = (0.5 * (1.0 + t)) + (0.5 * x * (1.0 - (t * t)) * c * (1.0 + (3.0 * k * x2)));
        return true;
    }
    return false;
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
            if (with_activation && !ml_grad_activation_eval(activation, interp, sum, &output_value_read, &activation_derivative)) {
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

    Value* result = nn_make_dict(interp, 10);
    if (result == null || result.tag == ERROR) return result;
    err = nn_dict_set_symbol(result, interp, "kind", "gradients");
    if (err != null) return err;
    err = nn_dict_set_symbol(result, interp, "source", with_activation ? "linear-activation-mean-squared-error" : "linear-mean-squared-error");
    if (err != null) return err;
    if (with_activation) {
        err = nn_dict_set(result, interp, "activation", make_symbol(interp, activation));
        if (err != null) return err;
    }
    err = nn_dict_set(result, interp, "loss", loss_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "output", output_value);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "input-gradient", input_grad);
    if (err != null) return err;
    err = nn_dict_set(result, interp, "parameter-gradients", parameter_gradients);
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
    return ml_grad_invalid(interp, "ml/grad: unsupported gradient spec kind");
}
