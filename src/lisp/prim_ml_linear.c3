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
    Interp* interp
) {
    if (input == null || weights == null || out_batch_count == null || out_in_features == null || out_features == null) {
        return tensor_runtime_raise(interp, "ml/linear: tensor backing storage is missing");
    }
    *out_batch_count = 0;
    *out_in_features = 0;
    *out_features = 0;

    if (input.dtype != weights.dtype || (bias != null && input.dtype != bias.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/linear: tensor dtype mismatch", null);
    }
    if (!ml_linear_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/linear: currently supports Float64 and Float32 tensors", null);
    }
    if (input.rank < 1 || weights.rank != 2 || (bias != null && bias.rank != 1)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/linear: expected input rank >= 1, weights rank 2, and optional bias rank 1", null);
    }

    usz in_features = input.shape[input.rank - 1];
    usz features = weights.shape[0];
    if (weights.shape[1] != in_features || (bias != null && bias.shape[0] != features)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/linear: input, weights, and bias shapes are incompatible", null);
    }

    usz batch_count = 1;
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        if (!tensor_checked_mul_usz(batch_count, input.shape[axis], &batch_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/linear: result shape is too large", null);
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

fn Value* prim_ml_linear(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 3) {
        return primitive_arity_raise(interp, "ml/linear", "2 or 3 arguments: input weights [bias]", args.len);
    }
    if (!is_tensor(args[0]) || !is_tensor(args[1]) || (args.len == 3 && !is_tensor(args[2]))) {
        return tensor_type_raise(interp, "ml/linear: expected Tensor input, Tensor weights, and optional Tensor bias");
    }

    TensorVal* input = null;
    Value* input_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &input,
        &input_value,
        interp,
        "ml/linear: input tensor backing storage is missing",
        "ml/linear: backend kernel is not implemented for non-CPU tensors"
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
        "ml/linear: weights tensor backing storage is missing",
        "ml/linear: backend kernel is not implemented for non-CPU tensors"
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
            "ml/linear: bias tensor backing storage is missing",
            "ml/linear: backend kernel is not implemented for non-CPU tensors"
        );
        if (err != null) return err;
    }
    bool cleanup_bias = args.len == 3 && bias_value != args[2];
    defer if (cleanup_bias) {
        boundary_cleanup_materialized_value(null, bias_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(input) || !tensor_has_valid_storage(weights) || (bias != null && !tensor_has_valid_storage(bias))) {
        return tensor_runtime_raise(interp, "ml/linear: tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(weights) || (bias != null && !tensor_dense_row_major(bias))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/linear: currently supports dense row-major tensors", null);
    }

    usz batch_count = 0;
    usz in_features = 0;
    usz out_features = 0;
    err = ml_linear_validate(input, weights, bias, &batch_count, &in_features, &out_features, interp);
    if (err != null) return err;

    usz* result_shape = null;
    if (input.rank > 0) {
        if (input.rank > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/linear: result rank is too large", null);
        }
        result_shape = (usz*)mem::malloc(usz.sizeof * input.rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/linear: failed to allocate result shape", null);
        }
    }
    defer if (result_shape != null) mem::free(result_shape);
    for (usz axis = 0; axis + 1 < input.rank; axis++) {
        result_shape[axis] = input.shape[axis];
    }
    result_shape[input.rank - 1] = out_features;
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, result_shape[0:input.rank], "ml/linear: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/linear: result tensor backing storage is missing");
    }

    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        ml_linear_eval_f64(input, weights, bias, out_value.tensor_val, batch_count, in_features, out_features);
    } else {
        ml_linear_eval_f32(input, weights, bias, out_value.tensor_val, batch_count, in_features, out_features);
    }
    keep_out = true;
    return out_value;
}
