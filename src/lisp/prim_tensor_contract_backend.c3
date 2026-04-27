module lisp;

import std::core::mem;
import main;

fn Value* tensor_contract_try_vulkan_value(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    usz[] result_shape,
    Interp* interp
) {
    if (left == null || right == null) return null;
    bool touches_vulkan = left.device == TENSOR_DEVICE_VULKAN || right.device == TENSOR_DEVICE_VULKAN;
    if (!touches_vulkan) return null;
    if (left.device != TENSOR_DEVICE_VULKAN || right.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan operands must both be Vulkan-placed", null);
    }
    if ((!tensor_dtype_is_vulkan_real(left.dtype) && !tensor_dtype_is_fixed_complex(left.dtype)) || left.dtype != right.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports matching Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (axis_count > 0 && (left_axes == null || right_axes == null)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports matching Float64 or Float32 explicit-axis contraction", null);
    }
    if (!tensor_has_valid_device_storage(left, TENSOR_DEVICE_VULKAN) || !tensor_has_valid_device_storage(right, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "contract: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(left) || !tensor_dense_row_major(right)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports dense row-major matching Float64 or Float32 tensors", null);
    }

    usz expected_rank = (left.rank - axis_count) + (right.rank - axis_count);
    if (result_shape.len != expected_rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
    }
    usz result_axis = 0;
    for (usz axis = 0; axis < left.rank; axis++) {
        if (tensor_axis_is_listed(axis, left_axes, axis_count)) continue;
        if (result_axis >= result_shape.len || result_shape[result_axis] != left.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
        }
        result_axis++;
    }
    for (usz axis = 0; axis < right.rank; axis++) {
        if (tensor_axis_is_listed(axis, right_axes, axis_count)) continue;
        if (result_axis >= result_shape.len || result_shape[result_axis] != right.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
        }
        result_axis++;
    }

    bool rank1_dot = left.dtype == TENSOR_DTYPE_DOUBLE && axis_count == 1 && tensor_rank1_contiguous(left) && tensor_rank1_contiguous(right);
    if (rank1_dot) {
        if (left_axes[0] != 0 || right_axes[0] != 0 || result_shape.len != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports rank-1 Float64 dot", null);
        }
        if (left.shape[0] != right.shape[0]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: contracted dimensions must match", null);
        }
        if (left.shape[0] > 0) {
            TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_DOUBLE, result_shape, false, false, "contract: failed to allocate Vulkan tensor metadata");
            if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor metadata");
            tensor.kind = TENSOR_PAYLOAD_CONCRETE;
            tensor.device = TENSOR_DEVICE_VULKAN;

            void* out_device = null;
            int status = omni_tensor_backend_vulkan_contract_dot_f64(
                left.device_handle,
                right.device_handle,
                left.shape[0],
                &out_device
            );
            if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
                tensor_free_payload(tensor);
                return tensor_vulkan_status_raise(interp, status, "contract: Vulkan rank-1 dot kernel failed");
            }
            tensor.device_handle = out_device;
            tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

            Value* result = interp.alloc_value();
            if (result == null) {
                tensor_free_payload(tensor);
                return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor value");
            }
            result.tag = TENSOR;
            result.tensor_val = tensor;
            if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
                return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
            }
            return result;
        }
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(left.dtype, result_shape, false, false, "contract: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_contract_f64(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_contract_f32(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_contract_complex128(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_contract_complex64(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "contract: Vulkan contract kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}

fn Value* tensor_contract_try_cuda_value(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    usz[] result_shape,
    Interp* interp
) {
    if (left == null || right == null) return null;
    bool touches_cuda = left.device == TENSOR_DEVICE_CUDA || right.device == TENSOR_DEVICE_CUDA;
    if (!touches_cuda) return null;
    if (left.device != TENSOR_DEVICE_CUDA || right.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA operands must both be CUDA-placed", null);
    }
    if (axis_count != 1 || left_axes == null || right_axes == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2 Float64 or Float32 single-axis contraction", null);
    }
    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];
    if (left_axis > 1 || right_axis > 1) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2 Float64 or Float32 single-axis contraction", null);
    }
    if ((!tensor_dtype_is_cuda_real(left.dtype) && !tensor_dtype_is_fixed_complex(left.dtype)) || left.dtype != right.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports matching Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (tensor_dtype_is_fixed_complex(left.dtype) && omni_tensor_backend_cublas_complex_available() == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA fixed-width complex contract helper is unavailable", null);
    }

    int cuda_contract_kind = 0;
    usz m = 0;
    usz n = 0;
    usz k = 0;
    int trans_left = 0;
    int trans_right = 0;
    usz matrix_rows = 0;
    usz matrix_cols = 0;
    usz matrix_leading_dim = 0;
    int trans_matrix = 0;
    TensorVal* matrix_tensor = null;
    TensorVal* vector_tensor = null;
    bool cuda_contract_zero_fill = false;

    if (tensor_rank1_contiguous(left) && tensor_rank1_contiguous(right)) {
        if (left_axis != 0 || right_axis != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-1/rank-1 Float64 or Float32 single-axis contraction", null);
        }
        usz contracted = left.shape[0];
        if (right.shape[0] != contracted || result_shape.len != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        matrix_rows = 1;
        matrix_cols = contracted;
        matrix_leading_dim = contracted;
        matrix_tensor = left;
        vector_tensor = right;
        cuda_contract_kind = 2;
    } else if (tensor_rank2_row_major(left) && tensor_rank2_row_major(right)) {
        usz left_free_axis = left_axis == 0 ? 1 : 0;
        usz right_free_axis = right_axis == 0 ? 1 : 0;
        m = left.shape[left_free_axis];
        k = left.shape[left_axis];
        n = right.shape[right_free_axis];
        if (right.shape[right_axis] != k || result_shape.len != 2 || result_shape[0] != m || result_shape[1] != n) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = k == 0;
        trans_left = left_axis == 0 ? 1 : 0;
        trans_right = right_axis == 1 ? 1 : 0;
        cuda_contract_kind = 1;
    } else if (tensor_rank2_row_major(left) && tensor_rank1_contiguous(right)) {
        if (left_axis > 1 || right_axis != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2/rank-1 Float64 or Float32 single-axis contraction", null);
        }
        matrix_rows = left.shape[0];
        matrix_cols = left.shape[1];
        usz contracted = left.shape[left_axis];
        usz result_len = left.shape[left_axis == 0 ? 1 : 0];
        if (right.shape[0] != contracted || result_shape.len != 1 || result_shape[0] != result_len) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        trans_matrix = left_axis == 0 ? 1 : 0;
        matrix_tensor = left;
        vector_tensor = right;
        matrix_leading_dim = left.strides[0];
        cuda_contract_kind = 2;
    } else if (tensor_rank1_contiguous(left) && tensor_rank2_row_major(right)) {
        if (left_axis != 0 || right_axis > 1) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-1/rank-2 Float64 or Float32 single-axis contraction", null);
        }
        matrix_rows = right.shape[0];
        matrix_cols = right.shape[1];
        usz contracted = right.shape[right_axis];
        usz result_len = right.shape[right_axis == 0 ? 1 : 0];
        if (left.shape[0] != contracted || result_shape.len != 1 || result_shape[0] != result_len) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        trans_matrix = right_axis == 0 ? 1 : 0;
        matrix_tensor = right;
        vector_tensor = left;
        matrix_leading_dim = right.strides[0];
        cuda_contract_kind = 2;
    } else {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports dense rank-1/rank-1, rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2 Float64 or Float32 tensors", null);
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(left.dtype, result_shape, false, false, "contract: failed to allocate CUDA tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate CUDA tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = omni_tensor_backend_cuda_alloc(tensor.byte_len, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "contract: CUDA output allocation failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    if (tensor.element_count == 0) {
        status = OMNI_TENSOR_CUDA_SUCCESS;
    } else if (cuda_contract_zero_fill) {
        if (left.dtype == TENSOR_DTYPE_DOUBLE) {
            status = omni_tensor_backend_cuda_fill_f64(out_device, tensor.element_count, 0.0);
        } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
            status = omni_tensor_backend_cuda_fill_f32(out_device, tensor.element_count, 0.0f);
        } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
            status = omni_tensor_backend_cuda_fill_f64(out_device, tensor.element_count * 2, 0.0);
        } else {
            status = omni_tensor_backend_cuda_fill_f32(out_device, tensor.element_count * 2, 0.0f);
        }
    } else if (left.dtype == TENSOR_DTYPE_DOUBLE) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_dgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_dgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_sgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_sgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_zgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_zgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_cgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_cgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "contract: cuBLAS execution failed");
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "contract: failed to allocate CUDA tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}
