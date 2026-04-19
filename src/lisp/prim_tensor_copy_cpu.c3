module lisp;

import std::core::mem;
import main;

fn Value* tensor_copy_into_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    if (dest != null && dest.device == TENSOR_DEVICE_CUDA) {
        return tensor_copy_into_cuda_destination(source_value, dest_value, interp);
    }
    if (dest != null && dest.device == TENSOR_DEVICE_VULKAN) {
        return tensor_copy_into_vulkan_destination(source_value, dest_value, interp);
    }
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    Value* dest_check = tensor_require_cpu_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (tensor_expr_has_non_cpu_device(source_value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: non-CPU Tensor source requires explicit to-device cpu copy before destination realization", null);
    }

    if (source.kind != TENSOR_PAYLOAD_CONCRETE) {
        if (tensor_contract_sources_alias_destination(source, dest)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/aliasing", "contract: destination aliases source tensor", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        Value* staged_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "realize: failed to allocate tensor result");
        if (staged_value == null || staged_value.tag == ERROR) return staged_value;
        bool keep_staged = false;
        defer if (!keep_staged) {
            boundary_cleanup_materialized_value(null, staged_value, interp.current_scope);
        }
        if (staged_value.tag != TENSOR || staged_value.tensor_val == null || !tensor_has_valid_storage(staged_value.tensor_val)) {
            return tensor_runtime_raise(interp, "realize: tensor result backing storage is missing");
        }
        Value* err = tensor_eval_expr_into(source, staged_value.tensor_val, interp);
        if (err != null) return err;
        if (!tensor_copy_concrete_storage(dest, staged_value.tensor_val)) {
            return tensor_runtime_raise(interp, "realize: failed to copy staged tensor result");
        }
        return dest_value;
    }
    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
    if (source_value == dest_value || source == dest || source.data == dest.data) return dest_value;
    if (!tensor_copy_concrete_storage(dest, source)) {
        return tensor_runtime_raise(interp, "realize: failed to copy tensor storage");
    }
    return dest_value;
}

fn Value* tensor_fill_cuda_destination(TensorVal* dest, Value* data_arg, Interp* interp) {
    Value* dest_check = tensor_require_cuda_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (!tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination must be dense row-major", null);
    }

    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dest.dtype == TENSOR_DTYPE_DOUBLE) {
        double scalar = 0.0;
        if (!tensor_value_to_double(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float64");
        }
        status = omni_tensor_backend_cuda_fill_f64(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_FLOAT32) {
        float scalar = 0.0f;
        if (!tensor_value_to_float32(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float32");
        }
        status = omni_tensor_backend_cuda_fill_f32(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar;
        if (!fixed_complex_value_to_complex128(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex128");
        }
        if (dest.byte_len == 0) return null;
        Complex128Val* staged = (Complex128Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex128 CUDA fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_cuda_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar;
        if (!fixed_complex_value_to_complex64(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex64");
        }
        if (dest.byte_len == 0) return null;
        Complex64Val* staged = (Complex64Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex64 CUDA fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_cuda_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    }

    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "realize: CUDA destination fill failed");
    }
    return null;
}

fn Value* tensor_copy_into_cuda_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    Value* dest_check = tensor_require_cuda_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (source.kind == TENSOR_PAYLOAD_VIEW || source.is_view ||
        !tensor_dense_row_major(source) || !tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination requires dense row-major tensors", null);
    }

    bool touches_non_cpu = tensor_expr_has_non_cpu_device(source_value);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    defer if (realized != source_value) {
        boundary_cleanup_materialized_value(null, realized, interp.current_scope);
    }
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "realize: realized Tensor backing storage is missing");
    }

    TensorVal* concrete = realized.tensor_val;
    if (concrete.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (concrete.element_count != dest.element_count || concrete.byte_len != dest.byte_len || !tensor_same_shape(concrete, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (!tensor_concrete_zero_offset_dense_row_major(concrete)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination requires dense row-major tensors", null);
    }

    if (concrete.device == TENSOR_DEVICE_CPU) {
        if (touches_non_cpu) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA source did not realize to CUDA storage", null);
        }
        if (!tensor_has_valid_storage(concrete)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_cuda_copy_to_existing_device(concrete.data, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "realize: CUDA destination copy failed");
        }
        return dest_value;
    }

    if (concrete.device == TENSOR_DEVICE_CUDA) {
        if (!tensor_has_valid_device_storage(concrete, TENSOR_DEVICE_CUDA)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_cuda_copy_device_to_existing_device(concrete.device_handle, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "realize: CUDA destination copy failed");
        }
        return dest_value;
    }

    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: unsupported source device for CUDA destination", null);
}

fn Value* tensor_fill_vulkan_destination(TensorVal* dest, Value* data_arg, Interp* interp) {
    Value* dest_check = tensor_require_vulkan_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (!tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination must be dense row-major", null);
    }

    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (dest.dtype == TENSOR_DTYPE_DOUBLE) {
        double scalar = 0.0;
        if (!tensor_value_to_double(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float64");
        }
        status = omni_tensor_backend_vulkan_fill_f64(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_FLOAT32) {
        float scalar = 0.0f;
        if (!tensor_value_to_float32(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float32");
        }
        status = omni_tensor_backend_vulkan_fill_f32(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar;
        if (!fixed_complex_value_to_complex128(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex128");
        }
        if (dest.byte_len == 0) return null;
        Complex128Val* staged = (Complex128Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex128 Vulkan fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_vulkan_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar;
        if (!fixed_complex_value_to_complex64(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex64");
        }
        if (dest.byte_len == 0) return null;
        Complex64Val* staged = (Complex64Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex64 Vulkan fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_vulkan_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination fill failed");
    }
    return null;
}

fn Value* tensor_copy_into_vulkan_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    Value* dest_check = tensor_require_vulkan_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    bool source_is_view = source.kind == TENSOR_PAYLOAD_VIEW || source.is_view;
    if (source_is_view && source.device != TENSOR_DEVICE_CPU && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination currently supports CPU-backed or Vulkan transpose-view materialization", null);
    }
    if ((!source_is_view && !tensor_dense_row_major(source)) || !tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination requires dense row-major tensors", null);
    }

    bool touches_non_cpu = tensor_expr_has_non_cpu_device(source_value);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    defer if (realized != source_value) {
        boundary_cleanup_materialized_value(null, realized, interp.current_scope);
    }
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "realize: realized Tensor backing storage is missing");
    }

    TensorVal* concrete = realized.tensor_val;
    if (concrete.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (concrete.element_count != dest.element_count || concrete.byte_len != dest.byte_len || !tensor_same_shape(concrete, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (!tensor_concrete_zero_offset_dense_row_major(concrete)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination requires dense row-major tensors", null);
    }

    if (concrete.device == TENSOR_DEVICE_CPU) {
        if (touches_non_cpu) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan source did not realize to Vulkan storage", null);
        }
        if (!tensor_has_valid_storage(concrete)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_vulkan_copy_to_existing_device(concrete.data, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination copy failed");
        }
        return dest_value;
    }

    if (concrete.device == TENSOR_DEVICE_VULKAN) {
        if (!tensor_has_valid_device_storage(concrete, TENSOR_DEVICE_VULKAN)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_vulkan_copy_device_to_existing_device(concrete.device_handle, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination copy failed");
        }
        return dest_value;
    }

    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: unsupported source device for Vulkan destination", null);
}


fn Value* tensor_expr_resolve_concrete_any_device(Value* source_value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (out_tensor == null) return tensor_runtime_raise(interp, label);
    *out_tensor = null;
    if (out_value != null) *out_value = null;
    if (!is_tensor(source_value)) return tensor_type_raise(interp, label);

    TensorVal* source = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, label);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val) || realized.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE) {
        return tensor_runtime_raise(interp, label);
    }
    *out_tensor = realized.tensor_val;
    if (out_value != null) *out_value = realized;
    return null;
}

fn Value* tensor_map_try_cuda_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP) return null;

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

    bool touches_cuda = (source.map_left != null && tensor_expr_has_device(source.map_left, TENSOR_DEVICE_CUDA)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_device(source.map_right, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_cuda_callable_op_or_raise(source.map_func, source.map_is_binary, source.dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    if ((source.map_left != null && tensor_expr_has_non_cuda_device(source.map_left)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_non_cuda_device(source.map_right))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((source.map_left != null && source.map_left.tensor_val != null && source.map_left.tensor_val.dtype != source.dtype) ||
        (source.map_is_binary && source.map_right != null && source.map_right.tensor_val != null && source.map_right.tensor_val.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
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
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
        }
        if (!tensor_dtype_is_cuda_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, source)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_cuda_map_complex_unary_value(
                left_tensor,
                op,
                shape_slice,
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
            shape_slice,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Float64 and Float32 unary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA unary kernel failed"
        );
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (tensor_dtype_is_fixed_complex(source.dtype)) {
        return tensor_cuda_map_complex_binary_resolved(
            left_tensor,
            source.map_left_complex128_scalar,
            source.map_left_complex64_scalar,
            right_tensor,
            source.map_right_complex128_scalar,
            source.map_right_complex64_scalar,
            source.dtype,
            shape_slice,
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
        source.map_left_scalar,
        right_tensor,
        source.map_right_scalar,
        source.dtype,
        shape_slice,
        op,
        interp,
        "map: failed to allocate CUDA tensor metadata",
        "map: failed to allocate CUDA tensor value",
        "map: CUDA currently supports dense row-major Float64 and Float32 binary operations",
        "map: CUDA tensor backing storage is missing",
        "map: CUDA kernel failed"
    );
}

fn Value* tensor_map_vulkan_callable_op_or_raise(Value* func, bool is_binary, TensorDType dtype, uint* out_op, Interp* interp) {
    if (out_op == null) return tensor_runtime_raise(interp, "map: Vulkan callable state is missing");
    uint op = 0;
    if (tensor_dtype_is_fixed_complex(dtype)) {
        bool supported = is_binary
            ? tensor_map_callable_complex_binary_op(func, &op)
            : tensor_map_callable_complex_unary_op(func, &op);
        if (!supported) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Complex128 and Complex64 arithmetic elementwise kernels", null);
        }
    } else if (!is_binary) {
        if (!tensor_map_callable_vulkan_unary_op(func, &op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", null);
        }
    } else if (!tensor_map_callable_vulkan_op(func, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", null);
    }
    *out_op = op;
    return null;
}
