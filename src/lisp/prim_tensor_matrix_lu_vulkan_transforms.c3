module lisp;

import std::core::mem;
import main;

fn bool matrix_norm_calculate(TensorVal* matrix, MatrixNormKind kind, double* out_norm) {
    if (out_norm == null) return false;
    *out_norm = 0.0;
    if (
        matrix == null ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        ) ||
        matrix.rank != 2 ||
        matrix.shape == null
    ) {
        return false;
    }
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz count = 0;
    if (!tensor_checked_mul_usz(rows, cols, &count)) return false;
    if (count == 0) return true;
    if (matrix.data == null) return false;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val* values = (Complex128Val*)matrix.data;
        switch (kind) {
            case MATRIX_NORM_FROBENIUS: {
                *out_norm = matrix_frobenius_norm_complex128(values, count);
                return true;
            }
            case MATRIX_NORM_ONE: {
                double max_col_sum = 0.0;
                for (usz col = 0; col < cols; col++) {
                    double col_sum = 0.0;
                    for (usz row = 0; row < rows; row++) {
                        col_sum += complex128_abs_value(values[row * cols + col]);
                    }
                    if (col_sum > max_col_sum) max_col_sum = col_sum;
                }
                *out_norm = max_col_sum;
                return true;
            }
            case MATRIX_NORM_INFINITY: {
                double max_row_sum = 0.0;
                for (usz row = 0; row < rows; row++) {
                    double row_sum = 0.0;
                    for (usz col = 0; col < cols; col++) {
                        row_sum += complex128_abs_value(values[row * cols + col]);
                    }
                    if (row_sum > max_row_sum) max_row_sum = row_sum;
                }
                *out_norm = max_row_sum;
                return true;
            }
            case MATRIX_NORM_MAX: {
                double max_abs = 0.0;
                for (usz i = 0; i < count; i++) {
                    double abs_value = complex128_abs_value(values[i]);
                    if (abs_value > max_abs) max_abs = abs_value;
                }
                *out_norm = max_abs;
                return true;
            }
            case MATRIX_NORM_SPECTRAL:
                return matrix_singular_value_norm(matrix, false, out_norm);
            case MATRIX_NORM_NUCLEAR:
                return matrix_singular_value_norm(matrix, true, out_norm);
        }
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val* values = (Complex64Val*)matrix.data;
        switch (kind) {
            case MATRIX_NORM_FROBENIUS: {
                *out_norm = matrix_frobenius_norm_complex64(values, count);
                return true;
            }
            case MATRIX_NORM_ONE: {
                double max_col_sum = 0.0;
                for (usz col = 0; col < cols; col++) {
                    double col_sum = 0.0;
                    for (usz row = 0; row < rows; row++) {
                        col_sum += (double)complex64_abs_value(values[row * cols + col]);
                    }
                    if (col_sum > max_col_sum) max_col_sum = col_sum;
                }
                *out_norm = max_col_sum;
                return true;
            }
            case MATRIX_NORM_INFINITY: {
                double max_row_sum = 0.0;
                for (usz row = 0; row < rows; row++) {
                    double row_sum = 0.0;
                    for (usz col = 0; col < cols; col++) {
                        row_sum += (double)complex64_abs_value(values[row * cols + col]);
                    }
                    if (row_sum > max_row_sum) max_row_sum = row_sum;
                }
                *out_norm = max_row_sum;
                return true;
            }
            case MATRIX_NORM_MAX: {
                double max_abs = 0.0;
                for (usz i = 0; i < count; i++) {
                    double abs_value = (double)complex64_abs_value(values[i]);
                    if (abs_value > max_abs) max_abs = abs_value;
                }
                *out_norm = max_abs;
                return true;
            }
            case MATRIX_NORM_SPECTRAL:
                return matrix_singular_value_norm(matrix, false, out_norm);
            case MATRIX_NORM_NUCLEAR:
                return matrix_singular_value_norm(matrix, true, out_norm);
        }
    }
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        double* values = (double*)matrix.data;
        switch (kind) {
            case MATRIX_NORM_FROBENIUS: {
                *out_norm = matrix_frobenius_norm(values, count);
                return true;
            }
            case MATRIX_NORM_ONE: {
                double max_col_sum = 0.0;
                for (usz col = 0; col < cols; col++) {
                    double col_sum = 0.0;
                    for (usz row = 0; row < rows; row++) {
                        col_sum += tensor_c_fabs(values[row * cols + col]);
                    }
                    if (col_sum > max_col_sum) max_col_sum = col_sum;
                }
                *out_norm = max_col_sum;
                return true;
            }
            case MATRIX_NORM_INFINITY: {
                double max_row_sum = 0.0;
                for (usz row = 0; row < rows; row++) {
                    double row_sum = 0.0;
                    for (usz col = 0; col < cols; col++) {
                        row_sum += tensor_c_fabs(values[row * cols + col]);
                    }
                    if (row_sum > max_row_sum) max_row_sum = row_sum;
                }
                *out_norm = max_row_sum;
                return true;
            }
            case MATRIX_NORM_MAX: {
                double max_abs = 0.0;
                for (usz i = 0; i < count; i++) {
                    double abs_value = tensor_c_fabs(values[i]);
                    if (abs_value > max_abs) max_abs = abs_value;
                }
                *out_norm = max_abs;
                return true;
            }
            case MATRIX_NORM_SPECTRAL: {
                return matrix_singular_value_norm(matrix, false, out_norm);
            }
            case MATRIX_NORM_NUCLEAR: {
                return matrix_singular_value_norm(matrix, true, out_norm);
            }
        }
    }
    float* values = (float*)matrix.data;
    switch (kind) {
        case MATRIX_NORM_FROBENIUS: {
            *out_norm = matrix_frobenius_norm_f32(values, count);
            return true;
        }
        case MATRIX_NORM_ONE: {
            double max_col_sum = 0.0;
            for (usz col = 0; col < cols; col++) {
                double col_sum = 0.0;
                for (usz row = 0; row < rows; row++) {
                    col_sum += tensor_c_fabs((double)values[row * cols + col]);
                }
                if (col_sum > max_col_sum) max_col_sum = col_sum;
            }
            *out_norm = max_col_sum;
            return true;
        }
        case MATRIX_NORM_INFINITY: {
            double max_row_sum = 0.0;
            for (usz row = 0; row < rows; row++) {
                double row_sum = 0.0;
                for (usz col = 0; col < cols; col++) {
                    row_sum += tensor_c_fabs((double)values[row * cols + col]);
                }
                if (row_sum > max_row_sum) max_row_sum = row_sum;
            }
            *out_norm = max_row_sum;
            return true;
        }
        case MATRIX_NORM_MAX: {
            double max_abs = 0.0;
            for (usz i = 0; i < count; i++) {
                double abs_value = tensor_c_fabs((double)values[i]);
                if (abs_value > max_abs) max_abs = abs_value;
            }
            *out_norm = max_abs;
            return true;
        }
        case MATRIX_NORM_SPECTRAL:
            return matrix_singular_value_norm(matrix, false, out_norm);
        case MATRIX_NORM_NUCLEAR:
            return matrix_singular_value_norm(matrix, true, out_norm);
    }
}
fn bool matrix_transpose_into(TensorVal* source, TensorVal* dest) {
    if (source == null || dest == null || source.shape == null || dest.shape == null) return false;
    if (source.rank != 2 || dest.rank != 2 || source.dtype != dest.dtype) return false;
    usz rows = source.shape[0];
    usz cols = source.shape[1];
    if (dest.shape[0] != cols || dest.shape[1] != rows) return false;
    if (source.element_count == 0) return true;
    if (source.data == null || dest.data == null) return false;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src = (double*)source.data;
            double* dst = (double*)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    dst[col * rows + row] = src[row * cols + col];
                }
            }
            return true;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src = (float*)source.data;
            float* dst = (float*)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    dst[col * rows + row] = src[row * cols + col];
                }
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src = (Complex128Val*)source.data;
            Complex128Val* dst = (Complex128Val*)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    dst[col * rows + row] = src[row * cols + col];
                }
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src = (Complex64Val*)source.data;
            Complex64Val* dst = (Complex64Val*)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    dst[col * rows + row] = src[row * cols + col];
                }
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    usz src_index = row * cols + col;
                    usz dst_index = col * rows + row;
                    if (src[src_index] == null) return false;
                    dst[dst_index] = omni_big_integer_clone(src[src_index]);
                    if (dst[dst_index] == null) return false;
                }
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    usz src_index = row * cols + col;
                    usz dst_index = col * rows + row;
                    if (src[src_index] == null) return false;
                    dst[dst_index] = omni_big_float_clone(src[src_index]);
                    if (dst[dst_index] == null) return false;
                }
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz row = 0; row < rows; row++) {
                for (usz col = 0; col < cols; col++) {
                    usz src_index = row * cols + col;
                    usz dst_index = col * rows + row;
                    if (src[src_index] == null) return false;
                    dst[dst_index] = omni_big_complex_clone(src[src_index]);
                    if (dst[dst_index] == null) return false;
                }
            }
            return true;
        }
    }
}
fn Value* matrix_transpose_try_vulkan_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/transpose: expected a rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/transpose: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape, false, false, "matrix/transpose: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "matrix/transpose: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_transpose_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_transpose_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_transpose_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_transpose_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "matrix/transpose: Vulkan kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/transpose: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
fn Value* matrix_diagonal_try_vulkan_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || result_shape.len != 1) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/diagonal: expected a rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/diagonal: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape, false, false, "matrix/diagonal: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "matrix/diagonal: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_diagonal_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_diagonal_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_diagonal_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_diagonal_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "matrix/diagonal: Vulkan kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/diagonal: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
fn Value* matrix_diagonal_matrix_try_vulkan_value(TensorVal* diagonal, usz[] result_shape, Interp* interp) {
    if (diagonal == null || diagonal.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(diagonal.dtype) && !tensor_dtype_is_fixed_complex(diagonal.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal-matrix: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (diagonal.rank != 1 || diagonal.shape == null || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/diagonal-matrix: expected a rank-1 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(diagonal, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/diagonal-matrix: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(diagonal)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal-matrix: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    TensorVal* tensor = tensor_alloc_payload_base_checked(diagonal.dtype, result_shape, false, false, "matrix/diagonal-matrix: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "matrix/diagonal-matrix: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (diagonal.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_diagonal_matrix_f64(
            diagonal.device_handle,
            diagonal.byte_len,
            diagonal.shape[0],
            &out_device
        );
    } else if (diagonal.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_diagonal_matrix_f32(
            diagonal.device_handle,
            diagonal.byte_len,
            diagonal.shape[0],
            &out_device
        );
    } else if (diagonal.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_diagonal_matrix_complex128(
            diagonal.device_handle,
            diagonal.byte_len,
            diagonal.shape[0],
            &out_device
        );
    } else if (diagonal.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_diagonal_matrix_complex64(
            diagonal.device_handle,
            diagonal.byte_len,
            diagonal.shape[0],
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "matrix/diagonal-matrix: Vulkan kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/diagonal-matrix: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}
fn Value* matrix_trace_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/trace: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/trace: expected a square rank-2 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/trace: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/trace: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        double trace = 0.0;
        int status = omni_tensor_backend_vulkan_trace_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &trace
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/trace: Vulkan kernel failed");
        }
        return make_double(interp, trace);
    }
    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float trace_f32 = 0.0f;
        int status = omni_tensor_backend_vulkan_trace_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &trace_f32
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/trace: Vulkan kernel failed");
        }
        return make_float32(interp, trace_f32);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        double real = 0.0;
        double imag = 0.0;
        int status = omni_tensor_backend_vulkan_trace_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &real,
            &imag
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/trace: Vulkan kernel failed");
        }
        return make_complex128(interp, real, imag);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        float real = 0.0f;
        float imag = 0.0f;
        int status = omni_tensor_backend_vulkan_trace_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &real,
            &imag
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/trace: Vulkan kernel failed");
        }
        return make_complex64(interp, real, imag);
    }
    return tensor_runtime_raise(interp, "matrix/trace: unsupported Vulkan tensor dtype");
}
