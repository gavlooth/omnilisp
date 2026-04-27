module lisp;

import main;

fn Value* matrix_determinant_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/determinant: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/determinant: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/determinant: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/determinant: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }

    double determinant = 0.0;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_determinant_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &determinant
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float determinant_f32 = 0.0f;
        status = omni_tensor_backend_vulkan_determinant_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &determinant_f32
        );
        determinant = (double)determinant_f32;
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        double real = 0.0;
        double imag = 0.0;
        status = omni_tensor_backend_vulkan_determinant_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &real,
            &imag
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/determinant: Vulkan kernel failed");
        }
        return make_complex128(interp, real, imag);
    } else {
        float real = 0.0f;
        float imag = 0.0f;
        status = omni_tensor_backend_vulkan_determinant_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &real,
            &imag
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "matrix/determinant: Vulkan kernel failed");
        }
        return make_complex64(interp, real, imag);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/determinant: Vulkan kernel failed");
    }
    return make_double(interp, determinant);
}

fn Value* matrix_lu_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/lu: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/lu: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/lu: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/lu: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }

    usz n = matrix.shape[0];
    if (n > (usz)long.max) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: factor metadata is too large", null);
    }

    usz* pivot_work = null;
    if (n > 0) {
        if (n > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: pivot workspace is too large", null);
        }
        pivot_work = (usz*)mem::malloc(n * usz.sizeof);
        if (pivot_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate pivot workspace", null);
        }
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_lu_f64(
            matrix.device_handle,
            matrix.byte_len,
            n,
            pivot_work,
            &swap_count,
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_lu_f32(
            matrix.device_handle,
            matrix.byte_len,
            n,
            pivot_work,
            &swap_count,
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_lu_complex128(
            matrix.device_handle,
            matrix.byte_len,
            n,
            pivot_work,
            &swap_count,
            &out_device
        );
    } else {
        status = omni_tensor_backend_vulkan_lu_complex64(
            matrix.device_handle,
            matrix.byte_len,
            n,
            pivot_work,
            &swap_count,
            &out_device
        );
    }
    if (status == OMNI_TENSOR_VULKAN_SINGULAR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/lu: Vulkan kernel failed");
    }
    if (swap_count > (usz)long.max) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: factor metadata is too large", null);
    }

    usz[2] result_shape_buf = { n, n };
    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape_buf[0:2], false, false, "matrix/lu: failed to allocate Vulkan factor tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "matrix/lu: failed to allocate Vulkan factor tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* lu_value = interp.alloc_value();
    if (lu_value == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/lu: failed to allocate Vulkan factor tensor value");
    }
    lu_value.tag = TENSOR;
    lu_value.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, lu_value)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    bool keep_lu_value = false;
    defer if (!keep_lu_value) {
        boundary_cleanup_materialized_value(null, lu_value, interp.current_scope);
    }

    Value* lu_for_dict = boundary_promote_to_root(lu_value, interp);
    if (lu_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to promote factor tensor", null);
    }
    if (lu_for_dict.tag == ERROR) return lu_for_dict;
    keep_lu_value = lu_for_dict == lu_value;

    Value* pivots_value = make_array_checked(interp, n, "matrix/lu: failed to allocate pivot array");
    if (pivots_value == null || pivots_value.tag == ERROR) return pivots_value;
    if (pivots_value.tag != ARRAY || pivots_value.array_val == null) {
        return tensor_runtime_raise(interp, "matrix/lu: pivot array backing storage is missing");
    }
    for (usz i = 0; i < n; i++) {
        Value* pivot_entry = make_int(interp, (long)pivot_work[i]);
        Value* promoted = boundary_promote_to_root(pivot_entry, interp);
        if (promoted == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to promote pivot entry", null);
        }
        if (promoted.tag == ERROR) return promoted;
        pivots_value.array_val.items[i] = promoted;
        pivots_value.array_val.length++;
    }

    Value* out = make_hashmap_checked(interp, 8, "matrix/lu: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/lu: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "lu", lu_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "pivots", pivots_value)) return err;
    if (Value* err = explain_dict_set(out, interp, "swap-count", make_int(interp, (long)swap_count))) return err;
    keep_lu_value = true;
    return out;
}

fn Value* matrix_inverse_try_vulkan_value(TensorVal* matrix, usz[] result_shape, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!tensor_dtype_is_vulkan_real(matrix.dtype) && !tensor_dtype_is_fixed_complex(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/inverse: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1] || result_shape.len != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/inverse: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/inverse: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/inverse: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_inverse_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_inverse_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_inverse_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_inverse_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            &out_device
        );
    }
    if (status == OMNI_TENSOR_VULKAN_SINGULAR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/inverse: Vulkan kernel failed");
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape, false, false, "matrix/inverse: failed to allocate Vulkan tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "matrix/inverse: failed to allocate Vulkan tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/inverse: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}

const usz OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F64 = 65;
const usz OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F32 = 65;

fn bool matrix_solve_vulkan_should_use_parallel_f64(usz n, usz rhs_cols) {
    (void)rhs_cols;
    if (n < 3) return false;
    return n >= OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F64;
}

fn bool matrix_solve_vulkan_should_use_parallel_f32(usz n, usz rhs_cols) {
    (void)rhs_cols;
    if (n < 3) return false;
    return n >= OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N_F32;
}

fn Value* matrix_solve_try_vulkan_value(
    TensorVal* coefficients,
    TensorVal* rhs,
    usz n,
    usz rhs_cols,
    usz[] result_shape,
    Interp* interp
) {
    if (coefficients == null || rhs == null) return null;
    if (coefficients.device == TENSOR_DEVICE_CPU && rhs.device == TENSOR_DEVICE_CPU) return null;
    if (coefficients.device != TENSOR_DEVICE_VULKAN || rhs.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/solve: Vulkan requires coefficient and right-hand tensors on Vulkan", null);
    }
    if (
        (
            coefficients.dtype != TENSOR_DTYPE_DOUBLE &&
            coefficients.dtype != TENSOR_DTYPE_FLOAT32 &&
            coefficients.dtype != TENSOR_DTYPE_COMPLEX128 &&
            coefficients.dtype != TENSOR_DTYPE_COMPLEX64
        ) ||
        rhs.dtype != coefficients.dtype
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/solve: Vulkan currently supports matching Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (!tensor_has_valid_device_storage(coefficients, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/solve: Vulkan coefficient tensor backing storage is missing");
    }
    if (!tensor_has_valid_device_storage(rhs, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/solve: Vulkan right-hand tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(coefficients) || !tensor_dense_row_major(rhs)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/solve: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (coefficients.dtype == TENSOR_DTYPE_DOUBLE && matrix_solve_vulkan_should_use_parallel_f64(n, rhs_cols)) {
        status = omni_tensor_backend_vulkan_solve_parallel_f64(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    } else if (coefficients.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_solve_f64(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    } else if (coefficients.dtype == TENSOR_DTYPE_FLOAT32 && matrix_solve_vulkan_should_use_parallel_f32(n, rhs_cols)) {
        status = omni_tensor_backend_vulkan_solve_parallel_f32(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    } else if (coefficients.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_solve_f32(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    } else if (coefficients.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_solve_complex128(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    } else {
        status = omni_tensor_backend_vulkan_solve_complex64(
            coefficients.device_handle,
            coefficients.byte_len,
            rhs.device_handle,
            rhs.byte_len,
            n,
            rhs_cols,
            &out_device
        );
    }
    if (status == OMNI_TENSOR_VULKAN_SINGULAR) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/solve: Vulkan kernel failed");
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(coefficients.dtype, result_shape, false, false, "matrix/solve: failed to allocate Vulkan tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "matrix/solve: failed to allocate Vulkan tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/solve: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}
