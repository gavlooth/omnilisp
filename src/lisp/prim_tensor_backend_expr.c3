module lisp;

import std::core::mem;
import main;

fn Value* tensor_contract_try_device_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_CONTRACT) return null;

    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_value = null;
    Value* right_value = null;
    defer if (left_value != null && left_value != source.contract_left) {
        boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
    }
    defer if (right_value != null && right_value != source.contract_right) {
        boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
    }

    Value* err = tensor_expr_resolve_concrete_any_device(source.contract_left, &left_tensor, &left_value, interp, "contract: tensor backing storage is missing");
    if (err != null) return err;
    err = tensor_expr_resolve_concrete_any_device(source.contract_right, &right_tensor, &right_value, interp, "contract: tensor backing storage is missing");
    if (err != null) return err;

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* vulkan_result = tensor_contract_try_vulkan_value(
        left_tensor,
        right_tensor,
        source.contract_left_axes,
        source.contract_right_axes,
        source.contract_axis_count,
        shape_slice,
        interp
    );
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = tensor_contract_try_cuda_value(
        left_tensor,
        right_tensor,
        source.contract_left_axes,
        source.contract_right_axes,
        source.contract_axis_count,
        shape_slice,
        interp
    );
    if (cuda_result != null) return cuda_result;

    if ((left_tensor != null && left_tensor.device != TENSOR_DEVICE_CPU) ||
        (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CPU)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: non-CPU Tensor operands require supported backend contract execution", null);
    }
    return null;
}

fn Value* tensor_map_try_cuda_direct(
    Value* func,
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
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

    bool touches_cuda = (left_source_tensor != null && tensor_expr_has_device(left_source, TENSOR_DEVICE_CUDA)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_device(right_source, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_cuda_callable_op_or_raise(func, is_binary, dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    if ((left_source_tensor != null && tensor_expr_has_non_cuda_device(left_source)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_non_cuda_device(right_source))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((left_source_tensor != null && left_source_tensor.dtype != dtype) ||
        (is_binary && right_source_tensor != null && right_source_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }

    if (left_source_tensor != null) {
        if (is_binary && tensor_zero_offset_strided_device_map_operand(left_source_tensor, TENSOR_DEVICE_CUDA)) {
            left_tensor = left_source_tensor;
            left_value = left_source;
        } else {
            err = tensor_expr_resolve_concrete_any_device(left_source, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
            if (err != null) return err;
        }
    }
    if (is_binary && right_source_tensor != null) {
        if (tensor_zero_offset_strided_device_map_operand(right_source_tensor, TENSOR_DEVICE_CUDA)) {
            right_tensor = right_source_tensor;
            right_value = right_source;
        } else {
            err = tensor_expr_resolve_concrete_any_device(right_source, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
            if (err != null) return err;
        }
    }

    if (!is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
        }
        if (!tensor_dtype_is_cuda_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_shape(left_tensor, result_shape)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype)) {
            return tensor_cuda_map_complex_unary_value(
                left_tensor,
                op,
                result_shape,
                interp,
                "map: failed to allocate CUDA tensor metadata",
                "map: failed to allocate CUDA tensor value",
                "map: CUDA currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: CUDA tensor backing storage is missing",
                "map: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            left_tensor,
            op,
            result_shape,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Float64 and Float32 unary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA unary kernel failed"
        );
    }

    double left_scalar = 0.0;
    double right_scalar = 0.0;
    Complex128Val left_complex128_scalar;
    left_complex128_scalar.real = 0.0;
    left_complex128_scalar.imag = 0.0;
    Complex64Val left_complex64_scalar;
    left_complex64_scalar.real = 0.0f;
    left_complex64_scalar.imag = 0.0f;
    Complex128Val right_complex128_scalar;
    right_complex128_scalar.real = 0.0;
    right_complex128_scalar.imag = 0.0;
    Complex64Val right_complex64_scalar;
    right_complex64_scalar.real = 0.0f;
    right_complex64_scalar.imag = 0.0f;
    if (left_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_COMPLEX128
            ? fixed_complex_value_to_complex128(left_source, &left_complex128_scalar)
            : dtype == TENSOR_DTYPE_COMPLEX64
                ? fixed_complex_value_to_complex64(left_source, &left_complex64_scalar)
                : tensor_value_to_double(left_source, &left_scalar);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA scalar operand is not representable by the Tensor dtype", null);
        }
    }
    if (right_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_COMPLEX128
            ? fixed_complex_value_to_complex128(right_source, &right_complex128_scalar)
            : dtype == TENSOR_DTYPE_COMPLEX64
                ? fixed_complex_value_to_complex64(right_source, &right_complex64_scalar)
                : tensor_value_to_double(right_source, &right_scalar);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA scalar operand is not representable by the Tensor dtype", null);
        }
    }
    if (tensor_dtype_is_fixed_complex(dtype)) {
        return tensor_cuda_map_complex_binary_resolved(
            left_tensor,
            left_complex128_scalar,
            left_complex64_scalar,
            right_tensor,
            right_complex128_scalar,
            right_complex64_scalar,
            dtype,
            result_shape,
            op,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Complex128 and Complex64 binary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA complex kernel failed"
        );
    }
    return tensor_cuda_map_binary_resolved(
        left_tensor,
        left_scalar,
        right_tensor,
        right_scalar,
        dtype,
        result_shape,
        op,
        interp,
        "map: failed to allocate CUDA tensor metadata",
        "map: failed to allocate CUDA tensor value",
        "map: CUDA currently supports dense row-major Float64 and Float32 binary operations",
        "map: CUDA tensor backing storage is missing",
        "map: CUDA kernel failed"
    );
}

fn Value* tensor_map_try_vulkan_direct(
    Value* func,
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    bool record_provenance,
    Interp* interp
) {
    bool touches_vulkan = (left_source_tensor != null && tensor_expr_has_device(left_source, TENSOR_DEVICE_VULKAN)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_device(right_source, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;
    if ((left_source_tensor != null && left_source_tensor.kind != TENSOR_PAYLOAD_CONCRETE) ||
        (is_binary && right_source_tensor != null && right_source_tensor.kind != TENSOR_PAYLOAD_CONCRETE)) {
        return null;
    }

    uint op = 0;
    Value* preflight_err = tensor_map_vulkan_callable_op_or_raise(func, is_binary, dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    return tensor_map_try_vulkan_direct_checked(
        left_source,
        left_source_tensor,
        right_source,
        right_source_tensor,
        is_binary,
        dtype,
        result_shape,
        record_provenance ? func : null,
        op,
        interp
    );
}
