module lisp;

import std::core::mem;
import main;

fn Value* tensor_vulkan_map_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (source.dtype != TENSOR_DTYPE_DOUBLE && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (source.dtype == TENSOR_DTYPE_DOUBLE && op >= 5 && op != 19 && op != 20) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        if (op == 20) {
            status = omni_tensor_backend_vulkan_map_normal_quantile_f64(
                source.device_handle,
                source.byte_len,
                source.element_count,
                &out_device
            );
        } else {
            status = omni_tensor_backend_vulkan_map_unary_f64(
                source.device_handle,
                source.byte_len,
                source.element_count,
                op,
                &out_device
            );
        }
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        if (op == 20) {
            status = omni_tensor_backend_vulkan_map_normal_quantile_f32(
                source.device_handle,
                source.byte_len,
                source.element_count,
                &out_device
            );
        } else {
            status = omni_tensor_backend_vulkan_map_unary_f32(
                source.device_handle,
                source.byte_len,
                source.element_count,
                op,
                &out_device
            );
        }
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (op == 20 && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return tensor_raise(interp, "tensor/domain-error", "stats.normal-quantile: probability must be between 0 and 1");
        }
        if (op == 20 && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return tensor_raise(interp, "tensor/domain-error", "stats.normal-quantile: expected finite numeric input");
        }
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_vulkan_map_complex_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_vulkan_map_complex128_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_vulkan_map_complex64_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_vulkan_map_complex_to_real_value(
    TensorVal* source,
    uint op,
    TensorDType result_dtype,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if ((source.dtype == TENSOR_DTYPE_COMPLEX128 && result_dtype != TENSOR_DTYPE_DOUBLE) ||
        (source.dtype == TENSOR_DTYPE_COMPLEX64 && result_dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: Vulkan complex component result dtype mismatch", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(result_dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_vulkan_map_complex128_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_vulkan_map_complex64_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_map_complex128_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_cuda_map_complex64_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_to_real_value(
    TensorVal* source,
    uint op,
    TensorDType result_dtype,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if ((source.dtype == TENSOR_DTYPE_COMPLEX128 && result_dtype != TENSOR_DTYPE_DOUBLE) ||
        (source.dtype == TENSOR_DTYPE_COMPLEX64 && result_dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA complex component result dtype mismatch", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(result_dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_map_complex128_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_cuda_map_complex64_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype) || !tensor_cuda_unary_op_available(op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE && op <= 4) {
        status = omni_tensor_backend_cuda_map_unary_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32 && op <= 4) {
        status = omni_tensor_backend_cuda_map_unary_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_map_scientific_unary_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_map_scientific_unary_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (op == 20 && status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) {
            return tensor_raise(interp, "tensor/domain-error", "stats.normal-quantile: probability must be between 0 and 1");
        }
        if (op == 20 && status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) {
            return tensor_raise(interp, "tensor/domain-error", "stats.normal-quantile: expected finite numeric input");
        }
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_binary_resolved(
    TensorVal* left_tensor,
    double left_scalar,
    TensorVal* right_tensor,
    double right_scalar,
    TensorDType dtype,
    usz[] result_shape,
    uint op,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    if (tensor == null || tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports matching Float64 or Float32 Tensor storage", null);
    }
    if (tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }
    if (!tensor_zero_offset_strided_storage_span_valid(tensor) ||
        (left_tensor != null && !tensor_zero_offset_strided_storage_span_valid(left_tensor)) ||
        (right_tensor != null && !tensor_zero_offset_strided_storage_span_valid(right_tensor))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape) ||
        (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) ||
        (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }
    if (!tensor_zero_offset_strided_device_map_operand(tensor, TENSOR_DEVICE_CUDA) ||
        (left_tensor != null && !tensor_zero_offset_strided_device_map_operand(left_tensor, TENSOR_DEVICE_CUDA)) ||
        (right_tensor != null && !tensor_zero_offset_strided_device_map_operand(right_tensor, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, missing_message);
    }

    double scalar_f64 = 0.0;
    float scalar_f32 = 0.0f;
    uint mode = 2;
    if (left_tensor == null) {
        scalar_f64 = left_scalar;
        mode = 1;
    } else if (right_tensor == null) {
        scalar_f64 = right_scalar;
        mode = 0;
    }
    if (dtype == TENSOR_DTYPE_FLOAT32 && mode != 2 && !tensor_double_to_float32(scalar_f64, &scalar_f32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA Float32 scalar operand is not representable", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_map_f64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f64,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_map_f32(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f32,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_binary_resolved(
    TensorVal* left_tensor,
    Complex128Val left_complex128_scalar,
    Complex64Val left_complex64_scalar,
    TensorVal* right_tensor,
    Complex128Val right_complex128_scalar,
    Complex64Val right_complex64_scalar,
    TensorDType dtype,
    usz[] result_shape,
    uint op,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    if (tensor == null || tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports matching Complex128 or Complex64 Tensor storage", null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }
    if (!tensor_zero_offset_strided_storage_span_valid(tensor) ||
        (left_tensor != null && !tensor_zero_offset_strided_storage_span_valid(left_tensor)) ||
        (right_tensor != null && !tensor_zero_offset_strided_storage_span_valid(right_tensor))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape) ||
        (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) ||
        (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }
    if (!tensor_zero_offset_strided_device_map_operand(tensor, TENSOR_DEVICE_CUDA) ||
        (left_tensor != null && !tensor_zero_offset_strided_device_map_operand(left_tensor, TENSOR_DEVICE_CUDA)) ||
        (right_tensor != null && !tensor_zero_offset_strided_device_map_operand(right_tensor, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, missing_message);
    }

    uint mode = 2;
    if (left_tensor == null) {
        mode = 1;
    } else if (right_tensor == null) {
        mode = 0;
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar = mode == 1 ? left_complex128_scalar : right_complex128_scalar;
        status = omni_tensor_backend_cuda_map_complex128(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar = mode == 1 ? left_complex64_scalar : right_complex64_scalar;
        status = omni_tensor_backend_cuda_map_complex64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
