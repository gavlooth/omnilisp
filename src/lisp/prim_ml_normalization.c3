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
