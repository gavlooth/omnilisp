module lisp;

import std::core::mem;
import main;

fn bool ml_linear_supported_dtype(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
}

fn Value* ml_linear_validate(
    TensorVal* input,
    TensorVal* weights,
    TensorVal* bias,
    usz* out_batch_count,
    usz* out_in_features,
    usz* out_features,
    bool batched_reduce,
    Interp* interp
) {
    if (input == null || weights == null || out_batch_count == null || out_in_features == null || out_features == null) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: tensor backing storage is missing" : "ml/linear: tensor backing storage is missing");
    }
    *out_batch_count = 0;
    *out_in_features = 0;
    *out_features = 0;

    if (input.dtype != weights.dtype || (bias != null && input.dtype != bias.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", batched_reduce ? "ml/linear-batched-reduce: tensor dtype mismatch" : "ml/linear: tensor dtype mismatch", null);
    }
    if (!ml_linear_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: currently supports Float64 and Float32 tensors" : "ml/linear: currently supports Float64 and Float32 tensors", null);
    }
    if (input.rank < (batched_reduce ? 2 : 1) || weights.rank != 2 || (bias != null && bias.rank != 1)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", batched_reduce ? "ml/linear-batched-reduce: expected input rank >= 2, weights rank 2, and optional bias rank 1" : "ml/linear: expected input rank >= 1, weights rank 2, and optional bias rank 1", null);
    }

    usz in_features = input.shape[input.rank - 1];
    usz features = weights.shape[0];
    if (weights.shape[1] != in_features || (bias != null && bias.shape[0] != features)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", batched_reduce ? "ml/linear-batched-reduce: input, weights, and bias shapes are incompatible" : "ml/linear: input, weights, and bias shapes are incompatible", null);
    }

    usz batch_count = 1;
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        if (!tensor_checked_mul_usz(batch_count, input.shape[axis], &batch_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", batched_reduce ? "ml/linear-batched-reduce: result shape is too large" : "ml/linear: result shape is too large", null);
        }
    }

    *out_batch_count = batch_count;
    *out_in_features = in_features;
    *out_features = features;
    return null;
}

fn void ml_linear_eval_f64(TensorVal* input, TensorVal* weights, TensorVal* bias, TensorVal* out, usz batch_count, usz in_features, usz out_features) {
    double* input_data = (double*)input.data;
    double* weight_data = (double*)weights.data;
    double* bias_data = bias == null ? null : (double*)bias.data;
    double* out_data = (double*)out.data;
    for (usz batch = 0; batch < batch_count; batch++) {
        usz input_base = batch * in_features;
        usz out_base = batch * out_features;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            double sum = bias_data == null ? 0.0 : bias_data[out_axis];
            usz weight_base = out_axis * in_features;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                sum += input_data[input_base + in_axis] * weight_data[weight_base + in_axis];
            }
            out_data[out_base + out_axis] = sum;
        }
    }
}

fn void ml_linear_eval_f32(TensorVal* input, TensorVal* weights, TensorVal* bias, TensorVal* out, usz batch_count, usz in_features, usz out_features) {
    float* input_data = (float*)input.data;
    float* weight_data = (float*)weights.data;
    float* bias_data = bias == null ? null : (float*)bias.data;
    float* out_data = (float*)out.data;
    for (usz batch = 0; batch < batch_count; batch++) {
        usz input_base = batch * in_features;
        usz out_base = batch * out_features;
        for (usz out_axis = 0; out_axis < out_features; out_axis++) {
            float sum = bias_data == null ? 0.0f : bias_data[out_axis];
            usz weight_base = out_axis * in_features;
            for (usz in_axis = 0; in_axis < in_features; in_axis++) {
                sum += input_data[input_base + in_axis] * weight_data[weight_base + in_axis];
            }
            out_data[out_base + out_axis] = sum;
        }
    }
}

fn Value* ml_linear_try_vulkan(Value* input_value, Value* weights_value, Value* bias_value, bool has_bias, bool batched_reduce, Interp* interp) {
    bool touches_vulkan = tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(weights_value, TENSOR_DEVICE_VULKAN) ||
        (has_bias && tensor_expr_has_device(bias_value, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;

    if (!tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN) ||
        !tensor_expr_has_device(weights_value, TENSOR_DEVICE_VULKAN) ||
        (has_bias && !tensor_expr_has_device(bias_value, TENSOR_DEVICE_VULKAN))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan operands must be Vulkan-placed" : "ml/linear: Vulkan operands must be Vulkan-placed", null);
    }
    if (tensor_expr_has_non_vulkan_device(input_value) ||
        tensor_expr_has_non_vulkan_device(weights_value) ||
        (has_bias && tensor_expr_has_non_vulkan_device(bias_value))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan operands must remain Vulkan-placed" : "ml/linear: Vulkan operands must remain Vulkan-placed", null);
    }

    TensorVal* input = null;
    TensorVal* weights = null;
    TensorVal* bias = null;
    Value* resolved_input_value = null;
    Value* resolved_weights_value = null;
    Value* resolved_bias_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(input_value, &input, &resolved_input_value, interp, batched_reduce ? "ml/linear-batched-reduce: tensor metadata is missing" : "ml/linear: tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_input_value != null && resolved_input_value != input_value) {
        boundary_cleanup_materialized_value(null, resolved_input_value, interp.current_scope);
    }
    err = tensor_expr_resolve_concrete_any_device(weights_value, &weights, &resolved_weights_value, interp, batched_reduce ? "ml/linear-batched-reduce: tensor metadata is missing" : "ml/linear: tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_weights_value != null && resolved_weights_value != weights_value) {
        boundary_cleanup_materialized_value(null, resolved_weights_value, interp.current_scope);
    }
    if (has_bias) {
        err = tensor_expr_resolve_concrete_any_device(bias_value, &bias, &resolved_bias_value, interp, batched_reduce ? "ml/linear-batched-reduce: tensor metadata is missing" : "ml/linear: tensor metadata is missing");
        if (err != null) return err;
    }
    defer if (resolved_bias_value != null && resolved_bias_value != bias_value) {
        boundary_cleanup_materialized_value(null, resolved_bias_value, interp.current_scope);
    }

    if (input == null || weights == null || (has_bias && bias == null)) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: tensor metadata is missing" : "ml/linear: tensor metadata is missing");
    }
    if (!ml_linear_supported_dtype(input.dtype) || !ml_linear_supported_dtype(weights.dtype) || (has_bias && !ml_linear_supported_dtype(bias.dtype))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan currently supports Float64 and Float32 tensors" : "ml/linear: Vulkan currently supports Float64 and Float32 tensors", null);
    }
    if (input.device != TENSOR_DEVICE_VULKAN || weights.device != TENSOR_DEVICE_VULKAN || (has_bias && bias.device != TENSOR_DEVICE_VULKAN)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan operands must realize to Vulkan storage" : "ml/linear: Vulkan operands must realize to Vulkan storage", null);
    }
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(weights, TENSOR_DEVICE_VULKAN) ||
        (has_bias && !tensor_has_valid_device_storage(bias, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: Vulkan tensor backing storage is missing" : "ml/linear: Vulkan tensor backing storage is missing");
    }

    usz batch_count = 0;
    usz in_features = 0;
    usz out_features = 0;
    err = ml_linear_validate(input, weights, bias, &batch_count, &in_features, &out_features, batched_reduce, interp);
    if (err != null) return err;

    if (input.rank > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", batched_reduce ? "ml/linear-batched-reduce: result rank is too large" : "ml/linear: result rank is too large", null);
    }
    usz* result_shape = (usz*)mem::malloc(usz.sizeof * input.rank);
    if (result_shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", batched_reduce ? "ml/linear-batched-reduce: failed to allocate result shape" : "ml/linear: failed to allocate result shape", null);
    }
    defer mem::free(result_shape);
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        result_shape[axis] = input.shape[axis];
    }
    result_shape[input.rank - 1] = out_features;

    usz[1] left_axes = { input.rank - 1 };
    usz[1] right_axes = { 1 };
    Value* result = tensor_contract_try_vulkan_value(
        input,
        weights,
        &left_axes[0],
        &right_axes[0],
        1,
        result_shape[0:input.rank],
        interp
    );
    if (result == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan contract path is unavailable" : "ml/linear: Vulkan contract path is unavailable", null);
    }
    if (result.tag == ERROR) return result;
    bool keep_contract_result = !has_bias;
    defer if (!keep_contract_result) {
        boundary_cleanup_materialized_value(null, result, interp.current_scope);
    }
    if (!has_bias) return result;

    SymbolId plus_sym = interp.symbols.intern("+");
    Value* plus_func = interp.global_env.lookup(plus_sym);
    if (plus_func == null) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: + primitive is unavailable" : "ml/linear: + primitive is unavailable");
    }

    Value* biased = tensor_map_try_vulkan_direct(
        plus_func,
        result,
        result.tensor_val,
        resolved_bias_value,
        bias,
        true,
        input.dtype,
        result_shape[0:input.rank],
        false,
        interp
    );
    if (biased != null) return biased;
    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: Vulkan bias map path is unavailable" : "ml/linear: Vulkan bias map path is unavailable", null);
}

fn Value* ml_linear_apply(Value*[] args, bool batched_reduce, Interp* interp) {
    if (args.len != 2 && args.len != 3) {
        return primitive_arity_raise(interp, batched_reduce ? "ml/linear-batched-reduce" : "ml/linear", "2 or 3 arguments: input weights [bias]", args.len);
    }
    if (!is_tensor(args[0]) || !is_tensor(args[1]) || (args.len == 3 && !is_tensor(args[2]))) {
        return tensor_type_raise(interp, batched_reduce ? "ml/linear-batched-reduce: expected Tensor input, Tensor weights, and optional Tensor bias" : "ml/linear: expected Tensor input, Tensor weights, and optional Tensor bias");
    }

    Value* vulkan_result = ml_linear_try_vulkan(args[0], args[1], args.len == 3 ? args[2] : null, args.len == 3, batched_reduce, interp);
    if (vulkan_result != null) return vulkan_result;

    TensorVal* input = null;
    Value* input_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &input,
        &input_value,
        interp,
        batched_reduce ? "ml/linear-batched-reduce: input tensor backing storage is missing" : "ml/linear: input tensor backing storage is missing",
        batched_reduce ? "ml/linear-batched-reduce: backend kernel is not implemented for non-CPU tensors" : "ml/linear: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    bool cleanup_input = input_value != args[0];
    defer if (cleanup_input) {
        boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    }

    TensorVal* weights = null;
    Value* weights_value = null;
    err = tensor_expr_resolve_concrete_cpu(
        args[1],
        &weights,
        &weights_value,
        interp,
        batched_reduce ? "ml/linear-batched-reduce: weights tensor backing storage is missing" : "ml/linear: weights tensor backing storage is missing",
        batched_reduce ? "ml/linear-batched-reduce: backend kernel is not implemented for non-CPU tensors" : "ml/linear: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    bool cleanup_weights = weights_value != args[1];
    defer if (cleanup_weights) {
        boundary_cleanup_materialized_value(null, weights_value, interp.current_scope);
    }

    TensorVal* bias = null;
    Value* bias_value = null;
    if (args.len == 3) {
        err = tensor_expr_resolve_concrete_cpu(
            args[2],
            &bias,
            &bias_value,
            interp,
            batched_reduce ? "ml/linear-batched-reduce: bias tensor backing storage is missing" : "ml/linear: bias tensor backing storage is missing",
            batched_reduce ? "ml/linear-batched-reduce: backend kernel is not implemented for non-CPU tensors" : "ml/linear: backend kernel is not implemented for non-CPU tensors"
        );
        if (err != null) return err;
    }
    bool cleanup_bias = args.len == 3 && bias_value != args[2];
    defer if (cleanup_bias) {
        boundary_cleanup_materialized_value(null, bias_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(input) || !tensor_has_valid_storage(weights) || (bias != null && !tensor_has_valid_storage(bias))) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: tensor backing storage is missing" : "ml/linear: tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(weights) || (bias != null && !tensor_dense_row_major(bias))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", batched_reduce ? "ml/linear-batched-reduce: currently supports dense row-major tensors" : "ml/linear: currently supports dense row-major tensors", null);
    }

    usz batch_count = 0;
    usz in_features = 0;
    usz out_features = 0;
    err = ml_linear_validate(input, weights, bias, &batch_count, &in_features, &out_features, batched_reduce, interp);
    if (err != null) return err;

    usz* result_shape = null;
    if (input.rank > 0) {
        if (input.rank > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", batched_reduce ? "ml/linear-batched-reduce: result rank is too large" : "ml/linear: result rank is too large", null);
        }
        result_shape = (usz*)mem::malloc(usz.sizeof * input.rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", batched_reduce ? "ml/linear-batched-reduce: failed to allocate result shape" : "ml/linear: failed to allocate result shape", null);
        }
    }
    defer if (result_shape != null) mem::free(result_shape);
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        result_shape[axis] = input.shape[axis];
    }
    result_shape[input.rank - 1] = out_features;
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, result_shape[0:input.rank], batched_reduce ? "ml/linear-batched-reduce: failed to allocate result tensor" : "ml/linear: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, batched_reduce ? "ml/linear-batched-reduce: result tensor backing storage is missing" : "ml/linear: result tensor backing storage is missing");
    }

    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        ml_linear_eval_f64(input, weights, bias, out_value.tensor_val, batch_count, in_features, out_features);
    } else {
        ml_linear_eval_f32(input, weights, bias, out_value.tensor_val, batch_count, in_features, out_features);
    }
    keep_out = true;
    return out_value;
}

fn Value* prim_ml_linear(Value*[] args, Env* env, Interp* interp) {
    return ml_linear_apply(args, false, interp);
}

fn Value* prim_ml_linear_batched_reduce(Value*[] args, Env* env, Interp* interp) {
    return ml_linear_apply(args, true, interp);
}
