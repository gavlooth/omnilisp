module lisp;

import std::core::mem;
import main;

fn Value* tensor_map_try_vulkan_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP) return null;

    bool touches_vulkan = (source.map_left != null && tensor_expr_has_device(source.map_left, TENSOR_DEVICE_VULKAN)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_device(source.map_right, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_vulkan_callable_op_or_raise(source.map_func, source.map_is_binary, source.dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    return tensor_map_try_vulkan_value_checked(source, op, interp);
}

fn Value* tensor_map_try_vulkan_value_checked(TensorVal* source, uint op, Interp* interp) {

    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_value = null;
    Value* right_value = null;
    Value* err = null;
    defer if (left_value != null && left_value != source.map_left) {
        boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
    }
    defer if (right_value != null && right_value != source.map_right) {
        boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
    }

    if ((source.map_left != null && tensor_expr_has_non_vulkan_device(source.map_left)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_non_vulkan_device(source.map_right))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((source.map_left != null && source.map_left.tensor_val != null && source.map_left.tensor_val.dtype != source.dtype) ||
        (source.map_is_binary && source.map_right != null && source.map_right.tensor_val != null && source.map_right.tensor_val.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }

    if (source.map_left != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_left, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (source.map_is_binary && source.map_right != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_right, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!source.map_is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, source)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                left_tensor,
                op,
                shape_slice,
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
            shape_slice,
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
    uint mode = 2;
    if (left_tensor == null) {
        scalar_f64 = source.map_left_scalar;
        mode = 1;
    } else if (right_tensor == null) {
        scalar_f64 = source.map_right_scalar;
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
    if ((!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) || tensor.dtype != source.dtype ||
        (left_tensor != null && left_tensor.dtype != source.dtype) ||
        (right_tensor != null && right_tensor.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }
    if (source.dtype == TENSOR_DTYPE_FLOAT32 && mode != 2 && !tensor_double_to_float32(scalar_f64, &scalar_f32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan Float32 scalar operand is not representable", null);
    }
    if (!tensor_map_operand_matches_output(tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (left_tensor != null && !tensor_map_operand_matches_output(left_tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (right_tensor != null && !tensor_map_operand_matches_output(right_tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_VULKAN)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "map: Vulkan tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "map: failed to allocate Vulkan tensor metadata");
    if (result_tensor == null) return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor metadata");
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
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
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
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
    } else if (source.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar = mode == 1 ? source.map_left_complex128_scalar : source.map_right_complex128_scalar;
        status = omni_tensor_backend_vulkan_map_complex128(
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
    } else if (source.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar = mode == 1 ? source.map_left_complex64_scalar : source.map_right_complex64_scalar;
        status = omni_tensor_backend_vulkan_map_complex64(
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
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (tensor_dtype_is_fixed_complex(source.dtype) && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (tensor_dtype_is_fixed_complex(source.dtype) && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_vulkan_status_raise(interp, status, "map: Vulkan kernel failed");
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

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
