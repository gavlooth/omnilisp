module lisp;

import std::core::mem;
import main;

extern fn double ml_norm_c_sqrt(double x) @extern("sqrt");

const double ML_LAYER_NORM_DEFAULT_EPSILON = 0.00001;

fn Value* ml_layer_norm_parse_epsilon(Value* value, Interp* interp, double* out_epsilon) {
    if (out_epsilon == null) return tensor_runtime_raise(interp, "ml/layer-normalization: epsilon parser state is missing");
    double epsilon = ML_LAYER_NORM_DEFAULT_EPSILON;
    if (value != null && !try_numeric_to_double(value, &epsilon)) {
        return tensor_type_raise(interp, "ml/layer-normalization: epsilon must be a positive finite number");
    }
    if (!conversion_double_is_finite(epsilon) || epsilon <= 0.0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/layer-normalization: epsilon must be positive and finite", null);
    }
    *out_epsilon = epsilon;
    return null;
}

fn bool ml_layer_norm_axis_coord(TensorVal* source, usz logical_index, usz axis, usz* out_coord) {
    if (source == null || out_coord == null || axis >= source.rank || logical_index >= source.element_count) return false;
    usz remaining = logical_index;
    for (usz i = source.rank; i > 0; i--) {
        usz current_axis = i - 1;
        usz dim = source.shape[current_axis];
        if (dim == 0) return false;
        usz coord = remaining % dim;
        remaining = remaining / dim;
        if (current_axis == axis) {
            *out_coord = coord;
            return true;
        }
    }
    return false;
}

fn bool ml_layer_norm_slice_base_offset(TensorVal* source, usz logical_index, usz axis, usz* out_base_offset, usz* out_axis_coord) {
    if (source == null || out_base_offset == null || out_axis_coord == null || axis >= source.rank) return false;
    usz storage_index = 0;
    usz axis_coord = 0;
    if (!tensor_storage_offset_for_logical_index(source, logical_index, &storage_index) ||
        !ml_layer_norm_axis_coord(source, logical_index, axis, &axis_coord)) {
        return false;
    }
    usz axis_delta = 0;
    if (!tensor_checked_mul_usz(axis_coord, source.strides[axis], &axis_delta) || axis_delta > storage_index) return false;
    *out_base_offset = storage_index - axis_delta;
    *out_axis_coord = axis_coord;
    return true;
}

fn bool ml_layer_norm_read_storage(TensorVal* source, usz storage_index, double* out_value) {
    if (source == null || out_value == null) return false;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE:
            *out_value = ((double*)source.data)[storage_index];
            return true;
        case TENSOR_DTYPE_FLOAT32:
            *out_value = (double)((float*)source.data)[storage_index];
            return true;
        default:
            return false;
    }
}

fn Value* ml_layer_norm_eval_cpu(TensorVal* source, TensorVal* out, usz axis, double epsilon, Interp* interp) {
    if (source == null || out == null || axis >= source.rank) {
        return tensor_runtime_raise(interp, "ml/layer-normalization: tensor metadata is missing");
    }
    if (out.element_count == 0) return null;

    usz bytes = 0;
    if (!tensor_checked_mul_usz(out.element_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/layer-normalization: output buffer is too large", null);
    }
    double* normalized = (double*)mem::malloc(bytes);
    if (normalized == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/layer-normalization: failed to allocate output buffer", null);
    }
    defer mem::free(normalized);

    usz axis_dim = source.shape[axis];
    usz axis_stride = source.strides[axis];
    for (usz i = 0; i < source.element_count; i++) {
        usz base_offset = 0;
        usz axis_coord = 0;
        if (!ml_layer_norm_slice_base_offset(source, i, axis, &base_offset, &axis_coord)) {
            return tensor_runtime_raise(interp, "ml/layer-normalization: tensor index offset is invalid");
        }

        double mean = 0.0;
        for (usz j = 0; j < axis_dim; j++) {
            double value = 0.0;
            if (!ml_layer_norm_read_storage(source, base_offset + (j * axis_stride), &value)) {
                return tensor_runtime_raise(interp, "ml/layer-normalization: tensor index offset is invalid");
            }
            mean += value;
        }
        mean = mean / (double)axis_dim;

        double variance = 0.0;
        for (usz j = 0; j < axis_dim; j++) {
            double value = 0.0;
            if (!ml_layer_norm_read_storage(source, base_offset + (j * axis_stride), &value)) {
                return tensor_runtime_raise(interp, "ml/layer-normalization: tensor index offset is invalid");
            }
            double diff = value - mean;
            variance += diff * diff;
        }
        variance = variance / (double)axis_dim;

        double current = 0.0;
        usz storage_index = 0;
        if (!tensor_storage_offset_for_logical_index(source, i, &storage_index) ||
            !ml_layer_norm_read_storage(source, storage_index, &current)) {
            return tensor_runtime_raise(interp, "ml/layer-normalization: tensor index offset is invalid");
        }
        normalized[i] = (current - mean) / ml_norm_c_sqrt(variance + epsilon);
    }

    return ml_reduction_write_result(out, normalized, interp, "ml/layer-normalization: Float32 result is not representable");
}

fn bool ml_norm_tensor_read_rank1(TensorVal* source, usz logical_index, double* out_value) {
    return ml_reduction_read(source, logical_index, out_value);
}

fn Value* ml_batch_norm_validate_param(TensorVal* param, TensorVal* input, usz channel_dim, char[] label, Interp* interp) {
    if (param == null || input == null) return tensor_runtime_raise(interp, label);
    if (param.device != input.device) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", label, null);
    }
    if (param.dtype != input.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", label, null);
    }
    if (param.rank != 1 || param.shape == null || param.shape[0] != channel_dim) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", label, null);
    }
    return null;
}

fn Value* ml_batch_norm_eval_cpu(
    TensorVal* input,
    TensorVal* scale,
    TensorVal* bias,
    TensorVal* mean,
    TensorVal* variance,
    TensorVal* out,
    usz channel_axis,
    double epsilon,
    Interp* interp
) {
    if (input == null || scale == null || bias == null || mean == null || variance == null || out == null || channel_axis >= input.rank) {
        return tensor_runtime_raise(interp, "ml/batch-normalization: tensor metadata is missing");
    }
    if (out.element_count == 0) return null;

    usz bytes = 0;
    if (!tensor_checked_mul_usz(out.element_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/batch-normalization: output buffer is too large", null);
    }
    double* normalized = (double*)mem::malloc(bytes);
    if (normalized == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/batch-normalization: failed to allocate output buffer", null);
    }
    defer mem::free(normalized);

    for (usz i = 0; i < input.element_count; i++) {
        usz channel_coord = 0;
        usz storage_index = 0;
        double x = 0.0;
        double gamma = 0.0;
        double beta = 0.0;
        double running_mean = 0.0;
        double running_variance = 0.0;
        if (!ml_layer_norm_axis_coord(input, i, channel_axis, &channel_coord) ||
            !tensor_storage_offset_for_logical_index(input, i, &storage_index) ||
            !ml_layer_norm_read_storage(input, storage_index, &x) ||
            !ml_norm_tensor_read_rank1(scale, channel_coord, &gamma) ||
            !ml_norm_tensor_read_rank1(bias, channel_coord, &beta) ||
            !ml_norm_tensor_read_rank1(mean, channel_coord, &running_mean) ||
            !ml_norm_tensor_read_rank1(variance, channel_coord, &running_variance)) {
            return tensor_runtime_raise(interp, "ml/batch-normalization: tensor index offset is invalid");
        }
        double denom = running_variance + epsilon;
        if (!conversion_double_is_finite(denom) || denom <= 0.0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/batch-normalization: variance plus epsilon must be positive and finite", null);
        }
        normalized[i] = ((x - running_mean) / ml_norm_c_sqrt(denom)) * gamma + beta;
    }

    return ml_reduction_write_result(out, normalized, interp, "ml/batch-normalization: Float32 result is not representable");
}

fn Value* ml_batch_norm_eval_vulkan(
    TensorVal* input,
    TensorVal* scale,
    TensorVal* bias,
    TensorVal* mean,
    TensorVal* variance,
    usz channel_axis,
    double epsilon,
    Interp* interp
) {
    if (input == null || scale == null || bias == null || mean == null || variance == null) {
        return tensor_runtime_raise(interp, "ml/batch-normalization: Vulkan tensor metadata is missing");
    }
    if (input.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(scale, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(bias, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(mean, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(variance, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/batch-normalization: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(input) ||
        !tensor_rank1_contiguous(scale) ||
        !tensor_rank1_contiguous(bias) ||
        !tensor_rank1_contiguous(mean) ||
        !tensor_rank1_contiguous(variance)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: Vulkan currently supports dense input and contiguous rank-1 parameter tensors", null);
    }

    float epsilon_f32 = 0.0f;
    if (!tensor_double_to_float32(epsilon, &epsilon_f32) || epsilon_f32 <= 0.0f) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/batch-normalization: Vulkan epsilon must be representable as positive Float32", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = input.rank == 0 ? scalar_shape[0:0] : input.shape[0:input.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(input.dtype, shape_slice, false, false, "ml/batch-normalization: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/batch-normalization: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_batch_norm_f32(
        input.device_handle,
        input.byte_len,
        input.element_count,
        input.rank,
        channel_axis,
        epsilon_f32,
        input.shape,
        input.strides,
        tensor.strides,
        scale.device_handle,
        scale.byte_len,
        bias.device_handle,
        bias.byte_len,
        mean.device_handle,
        mean.byte_len,
        variance.device_handle,
        variance.byte_len,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/batch-normalization: Vulkan batch-normalization kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/batch-normalization: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* ml_layer_norm_eval_vulkan(
    TensorVal* source,
    usz axis,
    double epsilon,
    Interp* interp
) {
    if (source == null) return tensor_runtime_raise(interp, "ml/layer-normalization: Vulkan tensor metadata is missing");
    if (source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/layer-normalization: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/layer-normalization: Vulkan tensor backing storage is missing");
    }

    float epsilon_f32 = 0.0f;
    if (!tensor_double_to_float32(epsilon, &epsilon_f32) || epsilon_f32 <= 0.0f) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/layer-normalization: Vulkan epsilon must be representable as positive Float32", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "ml/layer-normalization: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/layer-normalization: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_layer_norm_f32(
        source.device_handle,
        source.byte_len,
        source.element_count,
        source.rank,
        axis,
        epsilon_f32,
        source.shape,
        source.strides,
        tensor.strides,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/layer-normalization: Vulkan layer-normalization kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/layer-normalization: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_ml_layer_normalization(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 3) {
        return primitive_arity_raise(interp, "ml/layer-normalization", "2 or 3 arguments: input axis [epsilon]", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/layer-normalization: expected Tensor input");

    if (tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) && tensor_expr_has_non_vulkan_device(args[0])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/layer-normalization: Vulkan operands must remain Vulkan-placed", null);
    }

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(
        args[0],
        &source,
        &source_value,
        interp,
        "ml/layer-normalization: tensor backing storage is missing"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (source.device != TENSOR_DEVICE_CPU && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/layer-normalization: backend kernel is not implemented for this tensor placement", null);
    }
    if (source.device == TENSOR_DEVICE_CPU && !tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "ml/layer-normalization: tensor backing storage is missing");
    }
    if (source.device == TENSOR_DEVICE_VULKAN && !tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/layer-normalization: Vulkan tensor backing storage is missing");
    }
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/layer-normalization: currently supports Float64 and Float32 tensors", null);
    }
    if (source.device == TENSOR_DEVICE_VULKAN && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/layer-normalization: Vulkan currently supports Float32 tensors", null);
    }

    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        args[1],
        source,
        interp,
        "ml/layer-normalization: axis must be an integer",
        "ml/layer-normalization: axis out of range",
        "ml/layer-normalization: duplicate reduction axis",
        null,
        0,
        &axis
    );
    if (err != null) return err;

    usz[1] axes = { axis };
    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        source,
        axes[..],
        1,
        interp,
        "ml/layer-normalization: result shape is too large",
        "ml/layer-normalization: cannot normalize an empty axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    double epsilon = ML_LAYER_NORM_DEFAULT_EPSILON;
    err = ml_layer_norm_parse_epsilon(args.len == 3 ? args[2] : null, interp, &epsilon);
    if (err != null) return err;

    if (source.device == TENSOR_DEVICE_VULKAN) {
        return ml_layer_norm_eval_vulkan(source, axis, epsilon, interp);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml/layer-normalization: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/layer-normalization: result tensor backing storage is missing");
    }

    err = ml_layer_norm_eval_cpu(source, out_value.tensor_val, axis, epsilon, interp);
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

fn Value* prim_ml_batch_normalization(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 6 && args.len != 7) {
        return primitive_arity_raise(interp, "ml/batch-normalization", "6 or 7 arguments: input scale bias mean variance channel-axis [epsilon]", args.len);
    }
    for (usz i = 0; i < 5; i++) {
        if (!is_tensor(args[i])) return tensor_type_raise(interp, "ml/batch-normalization: expected Tensor input, scale, bias, mean, and variance");
    }

    bool touches_vulkan = false;
    for (usz i = 0; i < 5; i++) {
        if (tensor_expr_has_device(args[i], TENSOR_DEVICE_VULKAN)) touches_vulkan = true;
    }
    if (touches_vulkan) {
        for (usz i = 0; i < 5; i++) {
            if (!tensor_expr_has_device(args[i], TENSOR_DEVICE_VULKAN) || tensor_expr_has_non_vulkan_device(args[i])) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: Vulkan operands must remain Vulkan-placed", null);
            }
        }
    }

    TensorVal*[5] tensors = { null, null, null, null, null };
    Value*[5] values = { null, null, null, null, null };
    char[][5] missing_labels = {
        "ml/batch-normalization: input tensor backing storage is missing",
        "ml/batch-normalization: scale tensor backing storage is missing",
        "ml/batch-normalization: bias tensor backing storage is missing",
        "ml/batch-normalization: mean tensor backing storage is missing",
        "ml/batch-normalization: variance tensor backing storage is missing"
    };
    for (usz i = 0; i < 5; i++) {
        Value* err = tensor_expr_resolve_concrete_any_device(args[i], &tensors[i], &values[i], interp, missing_labels[i]);
        if (err != null) return err;
    }
    defer {
        for (usz i = 0; i < 5; i++) {
            if (values[i] != null && values[i] != args[i]) {
                boundary_cleanup_materialized_value(null, values[i], interp.current_scope);
            }
        }
    }

    TensorVal* input = tensors[0];
    TensorVal* scale = tensors[1];
    TensorVal* bias = tensors[2];
    TensorVal* mean = tensors[3];
    TensorVal* variance = tensors[4];
    if (input.device != TENSOR_DEVICE_CPU && input.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: backend kernel is not implemented for this tensor placement", null);
    }
    if (!ml_reduction_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: currently supports Float64 and Float32 tensors", null);
    }
    if (input.device == TENSOR_DEVICE_VULKAN && input.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/batch-normalization: Vulkan currently supports Float32 tensors", null);
    }
    if (input.device == TENSOR_DEVICE_CPU) {
        for (usz i = 0; i < 5; i++) {
            if (!tensor_has_valid_storage(tensors[i])) return tensor_runtime_raise(interp, missing_labels[i]);
        }
    } else {
        for (usz i = 0; i < 5; i++) {
            if (!tensor_has_valid_device_storage(tensors[i], TENSOR_DEVICE_VULKAN)) return tensor_runtime_raise(interp, missing_labels[i]);
        }
    }

    usz channel_axis = 0;
    Value* err = ml_reduction_parse_axis_value(
        args[5],
        input,
        interp,
        "ml/batch-normalization: channel axis must be an integer",
        "ml/batch-normalization: channel axis out of range",
        "ml/batch-normalization: duplicate channel axis",
        null,
        0,
        &channel_axis
    );
    if (err != null) return err;
    if (input.shape[channel_axis] == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/batch-normalization: cannot normalize an empty channel axis", null);
    }

    err = ml_batch_norm_validate_param(scale, input, input.shape[channel_axis], "ml/batch-normalization: scale must be a same-device rank-1 tensor matching the channel axis", interp);
    if (err != null) return err;
    err = ml_batch_norm_validate_param(bias, input, input.shape[channel_axis], "ml/batch-normalization: bias must be a same-device rank-1 tensor matching the channel axis", interp);
    if (err != null) return err;
    err = ml_batch_norm_validate_param(mean, input, input.shape[channel_axis], "ml/batch-normalization: mean must be a same-device rank-1 tensor matching the channel axis", interp);
    if (err != null) return err;
    err = ml_batch_norm_validate_param(variance, input, input.shape[channel_axis], "ml/batch-normalization: variance must be a same-device rank-1 tensor matching the channel axis", interp);
    if (err != null) return err;

    double epsilon = ML_LAYER_NORM_DEFAULT_EPSILON;
    err = ml_layer_norm_parse_epsilon(args.len == 7 ? args[6] : null, interp, &epsilon);
    if (err != null) return err;

    if (input.device == TENSOR_DEVICE_VULKAN) {
        return ml_batch_norm_eval_vulkan(input, scale, bias, mean, variance, channel_axis, epsilon, interp);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = input.rank == 0 ? scalar_shape[0:0] : input.shape[0:input.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, shape_slice, "ml/batch-normalization: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/batch-normalization: result tensor backing storage is missing");
    }
    err = ml_batch_norm_eval_cpu(input, scale, bias, mean, variance, out_value.tensor_val, channel_axis, epsilon, interp);
    if (err != null) return err;
    keep_out = true;
    return out_value;
}
