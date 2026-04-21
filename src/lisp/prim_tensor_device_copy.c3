module lisp;

import std::core::mem;
import main;

fn Value* tensor_cuda_status_raise(Interp* interp, int status, char[] label) {
    switch (status) {
        case OMNI_TENSOR_CUDA_UNAVAILABLE:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unavailable", label, null);
        case OMNI_TENSOR_CUDA_ALLOCATION_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-allocation-failed", label, null);
        case OMNI_TENSOR_CUDA_COPY_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-copy-failed", label, null);
        case OMNI_TENSOR_CUDA_EXECUTION_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-execution-failed", label, null);
        case OMNI_TENSOR_CUDA_DOMAIN_ERROR:
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", label, null);
        case OMNI_TENSOR_CUDA_INVALID_ARGUMENT:
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
        case OMNI_TENSOR_CUDA_INVALID:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-invalid-state", label, null);
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-failure", label, null);
    }
}

fn char[] tensor_vulkan_status_code_name(int status) {
    switch (status) {
        case OMNI_TENSOR_VULKAN_UNAVAILABLE:
            return "tensor/backend-unavailable";
        case OMNI_TENSOR_VULKAN_ALLOCATION_FAILED:
            return "tensor/backend-allocation-failed";
        case OMNI_TENSOR_VULKAN_COPY_FAILED:
            return "tensor/backend-copy-failed";
        case OMNI_TENSOR_VULKAN_UNSUPPORTED:
            return "tensor/backend-unsupported";
        case OMNI_TENSOR_VULKAN_EXECUTION_FAILED:
            return "tensor/backend-execution-failed";
        case OMNI_TENSOR_VULKAN_SINGULAR:
            return "tensor/singular-matrix";
        case OMNI_TENSOR_VULKAN_NO_CONVERGENCE:
            return "tensor/no-convergence";
        case OMNI_TENSOR_VULKAN_NOT_SYMMETRIC:
            return "tensor/not-symmetric";
        case OMNI_TENSOR_VULKAN_DOMAIN_ERROR:
            return "tensor/domain-error";
        case OMNI_TENSOR_VULKAN_INVALID_ARGUMENT:
            return "tensor/invalid-argument";
        case OMNI_TENSOR_VULKAN_INVALID:
            return "tensor/backend-invalid-state";
        default:
            return "tensor/backend-failure";
    }
}

fn Value* tensor_vulkan_status_raise(Interp* interp, int status, char[] label) {
    return raise_error_with_payload_names(interp, "tensor", tensor_vulkan_status_code_name(status), label, null);
}

fn Value* tensor_copy_cpu_to_cuda_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source_meta = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source_meta)) return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    if (source_meta.kind == TENSOR_PAYLOAD_VIEW || source_meta.is_view || !tensor_dense_row_major(source_meta)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor views are not eligible for CUDA copy", null);
    }

    Value* realized = tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
    }

    TensorVal* source = realized.tensor_val;
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
            return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
        }
        return realized;
    }
    if (source.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }
    if (!tensor_concrete_zero_offset_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be zero-offset dense row-major for CUDA copy", null);
    }
    if (!tensor_dtype_is_cuda_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, source.mutable, false, "to-device: failed to allocate CUDA tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "to-device: failed to allocate CUDA tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;

    void* device_handle = null;
    int status = omni_tensor_backend_cuda_copy_to_device(source.data, source.byte_len, &device_handle);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "to-device: CUDA backend unavailable");
    }
    tensor.device_handle = device_handle;
    tensor.device_finalizer = device_handle != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "to-device: failed to allocate CUDA tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_copy_expr_to_vulkan_graph_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return null;
    TensorVal* source = source_value.tensor_val;
    if (source.kind == TENSOR_PAYLOAD_CONCRETE) {
        if (source.device == TENSOR_DEVICE_VULKAN) return source_value;
        if (source.device != TENSOR_DEVICE_CPU || source.dtype != TENSOR_DTYPE_FLOAT32) return null;
        return tensor_copy_cpu_to_vulkan_value(source_value, interp);
    }
    if (source.dtype != TENSOR_DTYPE_FLOAT32) return null;
    if (source.kind == TENSOR_PAYLOAD_CONTRACT) {
        Value* left_value = tensor_copy_expr_to_vulkan_graph_value(source.contract_left, interp);
        if (left_value == null || left_value.tag == ERROR) return left_value;
        Value* right_value = tensor_copy_expr_to_vulkan_graph_value(source.contract_right, interp);
        if (right_value == null || right_value.tag == ERROR) return right_value;

        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        Value* result = make_tensor_contract_expr(
            left_value,
            right_value,
            source.contract_left_axes,
            source.contract_right_axes,
            source.contract_axis_count,
            shape_slice,
            interp
        );
        if (result == null || result.tag == ERROR) return result;
        result.tensor_val.device = TENSOR_DEVICE_VULKAN;
        return result;
    }
    if (source.kind != TENSOR_PAYLOAD_MAP) return null;

    uint op = 0;
    bool supported = source.map_is_binary
        ? tensor_map_callable_vulkan_op(source.map_func, &op)
        : tensor_map_callable_vulkan_unary_op(source.map_func, &op);
    if (!supported) return null;

    Value* left_value = null;
    TensorVal* left_tensor = null;
    Value* right_value = null;
    TensorVal* right_tensor = null;
    if (source.map_left != null) {
        left_value = tensor_copy_expr_to_vulkan_graph_value(source.map_left, interp);
        if (left_value == null || left_value.tag == ERROR) return left_value;
        left_tensor = left_value.tensor_val;
    } else {
        left_value = make_float32(interp, (float)source.map_left_scalar);
        if (left_value == null) return nn_oom(interp);
    }
    if (source.map_is_binary) {
        if (source.map_right != null) {
            right_value = tensor_copy_expr_to_vulkan_graph_value(source.map_right, interp);
            if (right_value == null || right_value.tag == ERROR) return right_value;
            right_tensor = right_value.tensor_val;
        } else {
            right_value = make_float32(interp, (float)source.map_right_scalar);
            if (right_value == null) return nn_oom(interp);
        }
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* result = make_tensor_map_expr(
        source.map_func,
        left_value,
        left_tensor,
        right_value,
        right_tensor,
        source.map_is_binary,
        source.dtype,
        shape_slice,
        interp
    );
    if (result == null || result.tag == ERROR) return result;
    result.tensor_val.device = TENSOR_DEVICE_VULKAN;
    return result;
}

fn Value* tensor_copy_cpu_to_vulkan_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source_meta = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source_meta)) return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    if ((source_meta.kind == TENSOR_PAYLOAD_MAP || source_meta.kind == TENSOR_PAYLOAD_CONTRACT) && source_meta.dtype == TENSOR_DTYPE_FLOAT32) {
        Value* graph_value = tensor_copy_expr_to_vulkan_graph_value(source_value, interp);
        if (graph_value != null) return graph_value;
    }
    bool source_is_view = source_meta.kind == TENSOR_PAYLOAD_VIEW || source_meta.is_view;
    if (source_is_view && source_meta.device != TENSOR_DEVICE_CPU && source_meta.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan copy currently supports CPU-backed or Vulkan transpose-view materialization", null);
    }
    if (!source_is_view && !tensor_dense_row_major(source_meta)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be dense row-major for Vulkan copy", null);
    }

    Value* realized = tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
    }

    TensorVal* source = realized.tensor_val;
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
            return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
        }
        return realized;
    }
    if (source.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }
    if (!tensor_concrete_zero_offset_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be zero-offset dense row-major for Vulkan copy", null);
    }
    if (!tensor_dtype_is_vulkan_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, source.mutable, false, "to-device: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "to-device: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* device_handle = null;
    int status = omni_tensor_backend_vulkan_copy_to_device(source.data, source.byte_len, &device_handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "to-device: Vulkan device copy failed");
    }
    tensor.device_handle = device_handle;
    tensor.device_finalizer = device_handle != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "to-device: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_copy_device_to_cpu_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source = source_value.tensor_val;
    if (source.device == TENSOR_DEVICE_CPU) {
        Value* realized = tensor_realize_to_value(source_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        if (!is_tensor(realized) || realized.tensor_val == null) {
            return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
        }
        if (realized.tensor_val.device == TENSOR_DEVICE_CPU) return realized;
        return tensor_copy_device_to_cpu_value(realized, interp);
    }
    if (!tensor_has_valid_metadata(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    }
    if (source.kind != TENSOR_PAYLOAD_CONCRETE) {
        Value* realized = tensor_realize_to_value(source_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        if (!is_tensor(realized) || realized.tensor_val == null) {
            return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
        }
        if (realized.tensor_val.device == TENSOR_DEVICE_CPU) return realized;
        return tensor_copy_device_to_cpu_value(realized, interp);
    }
    if (source.device != TENSOR_DEVICE_CUDA && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (source.device == TENSOR_DEVICE_CUDA && !tensor_dtype_is_cuda_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: CUDA device placement currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (source.device == TENSOR_DEVICE_VULKAN && !tensor_dtype_is_vulkan_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan device placement currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (!tensor_has_valid_device_storage(source, source.device)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "to-device: failed to allocate CPU tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    if (out_value.tag != TENSOR || out_value.tensor_val == null || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "to-device: CPU tensor result backing storage is missing");
    }

    if (source.device == TENSOR_DEVICE_CUDA) {
        int status = omni_tensor_backend_cuda_copy_to_host(source.device_handle, source.byte_len, out_value.tensor_val.data);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "to-device: CUDA host copy failed");
        }
        return out_value;
    }

    int status = omni_tensor_backend_vulkan_copy_to_host(source.device_handle, source.byte_len, out_value.tensor_val.data);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "to-device: Vulkan host copy failed");
    }
    return out_value;
}
