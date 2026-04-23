module lisp;

import std::core::mem;
import main;

fn Value* tensor_map_try_vulkan_direct_checked(
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    Value* provenance_func,
    uint op,
    Interp* interp
) {
    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_value = null;
    Value* right_value = null;
    Value* err = null;
    defer if (left_value != null && left_value != left_source) {
        boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
    }
    defer if (right_value != null && right_value != right_source) {
        boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
    }

    if ((left_source_tensor != null && tensor_expr_has_non_vulkan_device(left_source)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_non_vulkan_device(right_source))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (!tensor_dtype_is_vulkan_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((left_source_tensor != null && left_source_tensor.dtype != dtype) ||
        (is_binary && right_source_tensor != null && right_source_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: Vulkan tensor dtype mismatch", null);
    }

    if (left_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(left_source, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (is_binary && right_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(right_source, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_shape(left_tensor, result_shape)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                left_tensor,
                op,
                result_shape,
                interp,
                "map: failed to allocate Vulkan tensor metadata",
                "map: failed to allocate Vulkan tensor value",
                "map: Vulkan currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: Vulkan tensor backing storage is missing",
                "map: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            left_tensor,
            op,
            result_shape,
            interp,
            "map: failed to allocate Vulkan tensor metadata",
            "map: failed to allocate Vulkan tensor value",
            "map: Vulkan currently supports dense row-major Float64 and Float32 unary operations",
            "map: Vulkan tensor backing storage is missing",
            "map: Vulkan unary kernel failed"
        );
    }

    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    double scalar_f64 = 0.0;
    float scalar_f32 = 0.0f;
    Complex128Val scalar_complex128;
    scalar_complex128.real = 0.0;
    scalar_complex128.imag = 0.0;
    Complex64Val scalar_complex64;
    scalar_complex64.real = 0.0f;
    scalar_complex64.imag = 0.0f;
    uint mode = 2;
    if (left_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_FLOAT32
            ? tensor_value_to_float32(left_source, &scalar_f32)
            : dtype == TENSOR_DTYPE_COMPLEX128
                ? fixed_complex_value_to_complex128(left_source, &scalar_complex128)
                : dtype == TENSOR_DTYPE_COMPLEX64
                    ? fixed_complex_value_to_complex64(left_source, &scalar_complex64)
                    : tensor_value_to_double(left_source, &scalar_f64);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan scalar operand is not representable by the Tensor dtype", null);
        }
        mode = 1;
    } else if (right_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_FLOAT32
            ? tensor_value_to_float32(right_source, &scalar_f32)
            : dtype == TENSOR_DTYPE_COMPLEX128
                ? fixed_complex_value_to_complex128(right_source, &scalar_complex128)
                : dtype == TENSOR_DTYPE_COMPLEX64
                    ? fixed_complex_value_to_complex64(right_source, &scalar_complex64)
                    : tensor_value_to_double(right_source, &scalar_f64);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan scalar operand is not representable by the Tensor dtype", null);
        }
        mode = 0;
    }

    if (tensor == null || tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: Vulkan tensor dtype mismatch", null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_VULKAN)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "map: Vulkan tensor backing storage is missing");
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, "map: failed to allocate Vulkan tensor metadata");
    if (result_tensor == null) return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor metadata");
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_map_f64(
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
        status = omni_tensor_backend_vulkan_map_f32(
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
    } else if (dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_map_complex128(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_complex128.real,
            scalar_complex128.imag,
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
        status = omni_tensor_backend_vulkan_map_complex64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_complex64.real,
            scalar_complex64.imag,
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
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (tensor_dtype_is_fixed_complex(dtype) && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype) && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_vulkan_status_raise(interp, status, "map: Vulkan kernel failed");
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    if (provenance_func != null) {
        result_tensor.map_func = provenance_func;
        result_tensor.map_left = left_source_tensor != null ? left_source : null;
        result_tensor.map_right = is_binary && right_source_tensor != null ? right_source : null;
        result_tensor.map_is_binary = is_binary;
        if (left_source_tensor == null) {
            switch (dtype) {
                case TENSOR_DTYPE_DOUBLE:
                    result_tensor.map_left_scalar = scalar_f64;
                case TENSOR_DTYPE_FLOAT32:
                    result_tensor.map_left_scalar = (double)scalar_f32;
                case TENSOR_DTYPE_COMPLEX128:
                    result_tensor.map_left_complex128_scalar = scalar_complex128;
                case TENSOR_DTYPE_COMPLEX64:
                    result_tensor.map_left_complex64_scalar = scalar_complex64;
                default:
            }
        }
        if (is_binary && right_source_tensor == null) {
            switch (dtype) {
                case TENSOR_DTYPE_DOUBLE:
                    result_tensor.map_right_scalar = scalar_f64;
                case TENSOR_DTYPE_FLOAT32:
                    result_tensor.map_right_scalar = (double)scalar_f32;
                case TENSOR_DTYPE_COMPLEX128:
                    result_tensor.map_right_complex128_scalar = scalar_complex128;
                case TENSOR_DTYPE_COMPLEX64:
                    result_tensor.map_right_complex64_scalar = scalar_complex64;
                default:
            }
        }
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
