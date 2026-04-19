module lisp;

import std::core::mem;
import main;

fn Value* tensor_sqrt_value(Value* tensor_value, Interp* interp) {
    return tensor_unary_math_value(tensor_value, BIG_FLOAT_MATH_SQRT, BIG_COMPLEX_MATH_SQRT, interp);
}

fn double tensor_apply_double_rounding(double input, int op) {
    switch (op) {
        case BIG_FLOAT_ROUND_FLOOR:
            return tensor_c_floor(input);
        case BIG_FLOAT_ROUND_CEILING:
            return tensor_c_ceil(input);
        case BIG_FLOAT_ROUND_ROUND:
            return tensor_c_round(input);
        case BIG_FLOAT_ROUND_TRUNCATE:
            return input;
        default:
            return input;
    }
}

fn Value* tensor_store_integer_value(void** out_values, usz index, Value* integer_value, Interp* interp, char[] message) {
    void* handle = null;
    if (integer_value == null) return raise_error(interp, message);
    if (integer_value.tag == INT) {
        handle = omni_big_integer_from_i64(integer_value.int_val);
    } else if (integer_value.tag == BIG_INTEGER) {
        handle = omni_big_integer_clone(integer_value.big_integer_val);
    } else {
        return raise_error(interp, message);
    }
    if (handle == null) return raise_error(interp, message);
    if (out_values[index] != null) omni_big_integer_free(out_values[index]);
    out_values[index] = handle;
    return null;
}

fn Value* tensor_cuda_rounding_to_cpu_big_integer_value(TensorVal* source, int op, char[] range_message, Interp* interp) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: expected CUDA Tensor source", null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA supports Float64 and Float32 Tensor rounding", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "rounding: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA rounding requires dense row-major Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    long* rounded_values = null;
    if (source.element_count > 0) {
        if (source.element_count > usz.max / long.sizeof) {
            return raise_error(interp, "rounding: tensor integer result storage is too large");
        }
        rounded_values = (long*)mem::malloc(long.sizeof * source.element_count);
        if (rounded_values == null) return raise_error(interp, "rounding: failed to allocate CUDA integer result buffer");
    }
    defer if (rounded_values != null) mem::free(rounded_values);

    uint cuda_op = (uint)op;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_round_i64_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            cuda_op,
            rounded_values
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_round_i64_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            cuda_op,
            rounded_values
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) return raise_error(interp, range_message);
        if (status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) return raise_error(interp, "rounding: invalid CUDA rounding operation");
        return tensor_cuda_status_raise(interp, status, "rounding: CUDA rounding kernel failed");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        void* result = omni_big_integer_from_i64(rounded_values[i]);
        if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
        if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
        dst_values[i] = result;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_vulkan_rounding_to_cpu_big_integer_value(TensorVal* source, int op, char[] range_message, Interp* interp) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: expected Vulkan Tensor source", null);
    }
    if (!tensor_dtype_is_vulkan_real(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan supports Float64 and Float32 Tensor rounding", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "rounding: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan rounding requires dense row-major Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    long* rounded_values = null;
    if (source.element_count > 0) {
        if (source.element_count > usz.max / long.sizeof) {
            return raise_error(interp, "rounding: tensor integer result storage is too large");
        }
        rounded_values = (long*)mem::malloc(long.sizeof * source.element_count);
        if (rounded_values == null) return raise_error(interp, "rounding: failed to allocate Vulkan integer result buffer");
    }
    defer if (rounded_values != null) mem::free(rounded_values);

    uint vulkan_op = (uint)op;
    int status = OMNI_TENSOR_VULKAN_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_round_i64_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            vulkan_op,
            rounded_values
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_round_i64_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            vulkan_op,
            rounded_values
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) return raise_error(interp, range_message);
        if (status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) return raise_error(interp, "rounding: invalid Vulkan rounding operation");
        return tensor_vulkan_status_raise(interp, status, "rounding: Vulkan rounding kernel failed");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        void* result = omni_big_integer_from_i64(rounded_values[i]);
        if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
        if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
        dst_values[i] = result;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_rounding_value(Value* tensor_value, int op, char[] range_message, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "rounding: expected Tensor");

    if (tensor_value.tensor_val == null || !tensor_has_valid_metadata(tensor_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
    }
    if (tensor_expr_has_device(tensor_value, TENSOR_DEVICE_CUDA)) {
        Value* realized = tensor_realize_to_value(tensor_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        bool cleanup_realized = realized != tensor_value;
        defer if (cleanup_realized) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val)) {
            return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
        }
        if (realized.tensor_val.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA Tensor rounding must execute on CUDA", null);
        }
        return tensor_cuda_rounding_to_cpu_big_integer_value(realized.tensor_val, op, range_message, interp);
    }
    if (tensor_expr_has_device(tensor_value, TENSOR_DEVICE_VULKAN)) {
        Value* realized = tensor_realize_to_value(tensor_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        bool cleanup_realized = realized != tensor_value;
        defer if (cleanup_realized) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val)) {
            return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
        }
        if (realized.tensor_val.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan Tensor rounding must execute on Vulkan", null);
        }
        return tensor_vulkan_rounding_to_cpu_big_integer_value(realized.tensor_val, op, range_message, interp);
    }

    Value* concrete_value = null;
    TensorVal* source = null;
    Value* resolve_err = tensor_expr_resolve_concrete_cpu(
        tensor_value,
        &source,
        &concrete_value,
        interp,
        "rounding: tensor backing storage is missing",
        "rounding: unsupported Tensor device"
    );
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }
    if (source.dtype == TENSOR_DTYPE_BIG_COMPLEX || tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error(interp, "rounding: complex numbers are not supported");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                double rounded = tensor_apply_double_rounding(src_values[i], op);
                long narrowed = 0;
                if (!math_try_double_to_int(rounded, &narrowed)) return raise_error(interp, range_message);
                void* result = omni_big_integer_from_i64(narrowed);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                double rounded = tensor_apply_double_rounding((double)src_values[i], op);
                long narrowed = 0;
                if (!math_try_double_to_int(rounded, &narrowed)) return raise_error(interp, range_message);
                void* result = omni_big_integer_from_i64(narrowed);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "rounding: tensor BigInteger element is missing");
                void* result = omni_big_integer_clone(src_values[i]);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "rounding: tensor BigFloat element is missing");
                Value* arg = make_big_float_from_owned_handle(interp, omni_big_float_clone(src_values[i]), range_message);
                if (arg == null || arg.tag == ERROR) return arg;
                Value* rounded = big_float_round_to_integer_value(interp, arg, op, range_message);
                if (rounded == null || rounded.tag == ERROR) return rounded;
                Value* err = tensor_store_integer_value(dst_values, i, rounded, interp, "rounding: failed to allocate BigInteger tensor element");
                if (err != null) return err;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX:
            return raise_error(interp, "rounding: complex numbers are not supported");
        case TENSOR_DTYPE_COMPLEX128:
        case TENSOR_DTYPE_COMPLEX64:
            return raise_error(interp, "rounding: complex numbers are not supported");
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_floor_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_FLOOR, "floor: tensor integer result out of supported range", interp);
}

fn Value* tensor_ceiling_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_CEILING, "ceiling: tensor integer result out of supported range", interp);
}

fn Value* tensor_round_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_ROUND, "round: tensor integer result out of supported range", interp);
}

fn Value* tensor_truncate_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_TRUNCATE, "truncate: tensor integer result out of supported range", interp);
}
