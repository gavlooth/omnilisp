module lisp;

import std::core::mem;
import main;

fn bool matrix_cuda_complex_structural_available_for_dtype(TensorDType dtype) @inline {
    if (!tensor_dtype_is_fixed_complex(dtype)) return false;
    return omni_tensor_backend_cuda_complex_matrix_available() != 0;
}
fn Value* matrix_cuda_complex_structural_tensor_result(
    TensorDType dtype,
    usz[] result_shape,
    void* out_device,
    Interp* interp,
    char[] allocation_message,
    char[] value_message
) {
    TensorVal* tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, allocation_message);
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_cuda_free(out_device);
        return tensor_runtime_raise(interp, allocation_message);
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
fn Value* matrix_transpose_try_cuda_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_CUDA) return null;
    if (!tensor_dtype_is_fixed_complex(matrix.dtype) || !matrix_cuda_complex_structural_available_for_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose: CUDA currently supports Complex128 or Complex64 Tensor storage when fixed-width complex structural matrix kernels are available", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/transpose: expected a rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "matrix/transpose: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose: CUDA currently supports dense row-major Complex128 or Complex64 tensors", null);
    }
    void* out_device = null;
    int status = matrix.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_transpose_complex128(matrix.device_handle, matrix.byte_len, matrix.shape[0], matrix.shape[1], &out_device)
        : omni_tensor_backend_cuda_transpose_complex64(matrix.device_handle, matrix.byte_len, matrix.shape[0], matrix.shape[1], &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "matrix/transpose: CUDA kernel failed");
    }
    return matrix_cuda_complex_structural_tensor_result(
        matrix.dtype,
        result_shape,
        out_device,
        interp,
        "matrix/transpose: failed to allocate CUDA tensor metadata",
        "matrix/transpose: failed to allocate CUDA tensor value"
    );
}
fn Value* matrix_diagonal_try_cuda_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_CUDA) return null;
    if (!tensor_dtype_is_fixed_complex(matrix.dtype) || !matrix_cuda_complex_structural_available_for_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal: CUDA currently supports Complex128 or Complex64 Tensor storage when fixed-width complex structural matrix kernels are available", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || result_shape.len != 1) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/diagonal: expected a rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "matrix/diagonal: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal: CUDA currently supports dense row-major Complex128 or Complex64 tensors", null);
    }
    void* out_device = null;
    int status = matrix.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_diagonal_complex128(matrix.device_handle, matrix.byte_len, matrix.shape[0], matrix.shape[1], &out_device)
        : omni_tensor_backend_cuda_diagonal_complex64(matrix.device_handle, matrix.byte_len, matrix.shape[0], matrix.shape[1], &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "matrix/diagonal: CUDA kernel failed");
    }
    return matrix_cuda_complex_structural_tensor_result(
        matrix.dtype,
        result_shape,
        out_device,
        interp,
        "matrix/diagonal: failed to allocate CUDA tensor metadata",
        "matrix/diagonal: failed to allocate CUDA tensor value"
    );
}
fn Value* matrix_diagonal_matrix_try_cuda_value(TensorVal* diagonal, usz[] result_shape, Interp* interp) {
    if (diagonal == null || diagonal.device != TENSOR_DEVICE_CUDA) return null;
    if (!tensor_dtype_is_fixed_complex(diagonal.dtype) || !matrix_cuda_complex_structural_available_for_dtype(diagonal.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal-matrix: CUDA currently supports Complex128 or Complex64 Tensor storage when fixed-width complex structural matrix kernels are available", null);
    }
    if (diagonal.rank != 1 || diagonal.shape == null || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/diagonal-matrix: expected a rank-1 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(diagonal, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "matrix/diagonal-matrix: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(diagonal)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal-matrix: CUDA currently supports dense row-major Complex128 or Complex64 tensors", null);
    }
    void* out_device = null;
    int status = diagonal.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_diagonal_matrix_complex128(diagonal.device_handle, diagonal.byte_len, diagonal.shape[0], &out_device)
        : omni_tensor_backend_cuda_diagonal_matrix_complex64(diagonal.device_handle, diagonal.byte_len, diagonal.shape[0], &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "matrix/diagonal-matrix: CUDA kernel failed");
    }
    return matrix_cuda_complex_structural_tensor_result(
        diagonal.dtype,
        result_shape,
        out_device,
        interp,
        "matrix/diagonal-matrix: failed to allocate CUDA tensor metadata",
        "matrix/diagonal-matrix: failed to allocate CUDA tensor value"
    );
}
fn Value* matrix_trace_try_cuda_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_CUDA) return null;
    if (!tensor_dtype_is_fixed_complex(matrix.dtype) || !matrix_cuda_complex_structural_available_for_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/trace: CUDA currently supports Complex128 or Complex64 Tensor storage when fixed-width complex structural matrix kernels are available", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/trace: expected a square rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "matrix/trace: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/trace: CUDA currently supports dense row-major Complex128 or Complex64 tensors", null);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        double real = 0.0;
        double imag = 0.0;
        int status = omni_tensor_backend_cuda_trace_complex128(matrix.device_handle, matrix.byte_len, matrix.shape[0], &real, &imag);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "matrix/trace: CUDA kernel failed");
        }
        return make_complex128(interp, real, imag);
    }
    float real = 0.0f;
    float imag = 0.0f;
    int status = omni_tensor_backend_cuda_trace_complex64(matrix.device_handle, matrix.byte_len, matrix.shape[0], &real, &imag);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "matrix/trace: CUDA kernel failed");
    }
    return make_complex64(interp, real, imag);
}
fn Value* matrix_norm_try_vulkan_value(TensorVal* matrix, MatrixNormKind kind, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/norm: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/norm: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/norm: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/norm: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    uint mode = 0;
    switch (kind) {
        case MATRIX_NORM_FROBENIUS:
            mode = 0;
        case MATRIX_NORM_ONE:
            mode = 1;
        case MATRIX_NORM_INFINITY:
            mode = 2;
        case MATRIX_NORM_MAX:
            mode = 3;
        case MATRIX_NORM_SPECTRAL:
            double spectral_norm = 0.0;
            int spectral_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
            if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
                spectral_status = omni_tensor_backend_vulkan_singular_norm_f64(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    0,
                    &spectral_norm
                );
            } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
                float spectral_norm_f32 = 0.0f;
                spectral_status = omni_tensor_backend_vulkan_singular_norm_f32(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    0,
                    &spectral_norm_f32
                );
                spectral_norm = (double)spectral_norm_f32;
            } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
                spectral_status = omni_tensor_backend_vulkan_singular_norm_complex128(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    0,
                    &spectral_norm
                );
            } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
                float spectral_norm_f32 = 0.0f;
                spectral_status = omni_tensor_backend_vulkan_singular_norm_complex64(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    0,
                    &spectral_norm_f32
                );
                spectral_norm = (double)spectral_norm_f32;
            }
            if (spectral_status != OMNI_TENSOR_VULKAN_SUCCESS) {
                return tensor_vulkan_status_raise(interp, spectral_status, "matrix/norm: Vulkan singular-value kernel failed");
            }
            return make_double(interp, spectral_norm);
        case MATRIX_NORM_NUCLEAR:
            double nuclear_norm = 0.0;
            int nuclear_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
            if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
                nuclear_status = omni_tensor_backend_vulkan_singular_norm_f64(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    1,
                    &nuclear_norm
                );
            } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
                float nuclear_norm_f32 = 0.0f;
                nuclear_status = omni_tensor_backend_vulkan_singular_norm_f32(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    1,
                    &nuclear_norm_f32
                );
                nuclear_norm = (double)nuclear_norm_f32;
            } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
                nuclear_status = omni_tensor_backend_vulkan_singular_norm_complex128(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    1,
                    &nuclear_norm
                );
            } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
                float nuclear_norm_f32 = 0.0f;
                nuclear_status = omni_tensor_backend_vulkan_singular_norm_complex64(
                    matrix.device_handle,
                    matrix.byte_len,
                    matrix.shape[0],
                    matrix.shape[1],
                    1,
                    &nuclear_norm_f32
                );
                nuclear_norm = (double)nuclear_norm_f32;
            }
            if (nuclear_status != OMNI_TENSOR_VULKAN_SUCCESS) {
                return tensor_vulkan_status_raise(interp, nuclear_status, "matrix/norm: Vulkan singular-value kernel failed");
            }
            return make_double(interp, nuclear_norm);
    }
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    double norm = 0.0;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_norm_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            mode,
            &norm
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float norm_f32 = 0.0f;
        status = omni_tensor_backend_vulkan_norm_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            mode,
            &norm_f32
        );
        norm = (double)norm_f32;
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_norm_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            mode,
            &norm
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        float norm_f32 = 0.0f;
        status = omni_tensor_backend_vulkan_norm_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            mode,
            &norm_f32
        );
        norm = (double)norm_f32;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/norm: Vulkan kernel failed");
    }
    return make_double(interp, norm);
}
fn Value* matrix_rank_try_vulkan_value(TensorVal* matrix, double tolerance, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/rank: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/rank: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/rank: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/rank: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    double rank_value = 0.0;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_rank_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            tolerance,
            &rank_value
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        if (tolerance > TENSOR_FLOAT32_FINITE_LIMIT) return make_int(interp, 0);
        float rank_value_f32 = 0.0f;
        status = omni_tensor_backend_vulkan_rank_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            (float)tolerance,
            &rank_value_f32
        );
        rank_value = (double)rank_value_f32;
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_rank_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            tolerance,
            &rank_value
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        if (tolerance > TENSOR_FLOAT32_FINITE_LIMIT) return make_int(interp, 0);
        float rank_value_f32 = 0.0f;
        status = omni_tensor_backend_vulkan_rank_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            (float)tolerance,
            &rank_value_f32
        );
        rank_value = (double)rank_value_f32;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/rank: Vulkan kernel failed");
    }
    if (!conversion_double_is_finite(rank_value) || rank_value < 0.0) {
        return tensor_runtime_raise(interp, "matrix/rank: Vulkan kernel returned an invalid rank");
    }
    return make_int(interp, (long)rank_value);
}
fn Value* matrix_cholesky_try_vulkan_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/cholesky: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1] || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/cholesky: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/cholesky: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/cholesky: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_cholesky_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_cholesky_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_cholesky_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_cholesky_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    }
    if (status == OMNI_TENSOR_VULKAN_SINGULAR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-positive-definite", "matrix/cholesky: input matrix is not symmetric/Hermitian positive definite", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/cholesky: Vulkan kernel failed");
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape, false, false, "matrix/cholesky: failed to allocate Vulkan factor tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "matrix/cholesky: failed to allocate Vulkan factor tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/cholesky: failed to allocate Vulkan factor tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
fn Value* matrix_qr_try_vulkan_value(TensorVal* matrix, usz rows, usz cols, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/qr: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || rows < cols) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/qr: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/qr: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/qr: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    void* q_device = null;
    void* r_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_qr_f64(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &q_device,
            &r_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_qr_f32(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &q_device,
            &r_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_qr_complex128(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &q_device,
            &r_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_qr_complex64(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &q_device,
            &r_device
        );
    }
    if (status == OMNI_TENSOR_VULKAN_SINGULAR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/qr: Vulkan kernel failed");
    }
    usz[2] q_shape_buf = { rows, cols };
    TensorVal* q_tensor = tensor_alloc_payload_base_checked(matrix.dtype, q_shape_buf[0:2], false, false, "matrix/qr: failed to allocate Vulkan Q tensor metadata");
    if (q_tensor == null) {
        if (q_device != null) omni_tensor_backend_vulkan_free(q_device);
        if (r_device != null) omni_tensor_backend_vulkan_free(r_device);
        return tensor_runtime_raise(interp, "matrix/qr: failed to allocate Vulkan Q tensor metadata");
    }
    q_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    q_tensor.device = TENSOR_DEVICE_VULKAN;
    q_tensor.device_handle = q_device;
    q_tensor.device_finalizer = q_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* q_value = interp.alloc_value();
    if (q_value == null) {
        tensor_free_payload(q_tensor);
        if (r_device != null) omni_tensor_backend_vulkan_free(r_device);
        return tensor_runtime_raise(interp, "matrix/qr: failed to allocate Vulkan Q tensor value");
    }
    q_value.tag = TENSOR;
    q_value.tensor_val = q_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)q_value, &scope_dtor_value);
    bool keep_q_value = false;
    defer if (!keep_q_value) {
        boundary_cleanup_materialized_value(null, q_value, interp.current_scope);
    }
    usz[2] r_shape_buf = { cols, cols };
    TensorVal* r_tensor = tensor_alloc_payload_base_checked(matrix.dtype, r_shape_buf[0:2], false, false, "matrix/qr: failed to allocate Vulkan R tensor metadata");
    if (r_tensor == null) {
        if (r_device != null) omni_tensor_backend_vulkan_free(r_device);
        return tensor_runtime_raise(interp, "matrix/qr: failed to allocate Vulkan R tensor metadata");
    }
    r_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    r_tensor.device = TENSOR_DEVICE_VULKAN;
    r_tensor.device_handle = r_device;
    r_tensor.device_finalizer = r_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* r_value = interp.alloc_value();
    if (r_value == null) {
        tensor_free_payload(r_tensor);
        return tensor_runtime_raise(interp, "matrix/qr: failed to allocate Vulkan R tensor value");
    }
    r_value.tag = TENSOR;
    r_value.tensor_val = r_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)r_value, &scope_dtor_value);
    bool keep_r_value = false;
    defer if (!keep_r_value) {
        boundary_cleanup_materialized_value(null, r_value, interp.current_scope);
    }
    Value* q_for_dict = boundary_promote_to_root(q_value, interp);
    if (q_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote Q tensor", null);
    }
    if (q_for_dict.tag == ERROR) return q_for_dict;
    keep_q_value = q_for_dict == q_value;
    Value* r_for_dict = boundary_promote_to_root(r_value, interp);
    if (r_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote R tensor", null);
    }
    if (r_for_dict.tag == ERROR) return r_for_dict;
    keep_r_value = r_for_dict == r_value;
    Value* out = make_hashmap_checked(interp, 8, "matrix/qr: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/qr: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "q", q_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "r", r_for_dict)) return err;
    keep_q_value = true;
    keep_r_value = true;
    return out;
}
