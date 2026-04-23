module lisp;

import std::core::mem;
import main;

<*
@require interp != null
*>
fn Value* prim_matrix_qr(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/qr", "1 argument: rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/qr: input must be a rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns");

    TensorVal* matrix_source = args[0].tensor_val;
    Value* matrix_value = matrix_source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    if (
        matrix == null ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        ) ||
        matrix.rank != 2 ||
        matrix.shape == null ||
        matrix.shape[0] < matrix.shape[1]
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/qr: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns",
            null
        );
    }
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    Value* vulkan_result = matrix_qr_try_vulkan_value(matrix, rows, cols, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/qr: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/qr: tensor backing storage is missing");

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        usz q_count = 0;
        if (!tensor_checked_mul_usz(rows, cols, &q_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: Q workspace is too large", null);
        }
        Complex128Val* q_work = null;
        if (!matrix_complex128_workspace_zero(q_count, &q_work)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Complex128 Q workspace", null);
        }
        defer if (q_work != null) mem::free(q_work);
        if (matrix.byte_len > 0) mem::copy(q_work, matrix.data, matrix.byte_len);

        usz r_count = 0;
        if (!tensor_checked_mul_usz(cols, cols, &r_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        Complex128Val* r_work = null;
        if (!matrix_complex128_workspace_zero(r_count, &r_work)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Complex128 R workspace", null);
        }
        defer if (r_work != null) mem::free(r_work);

        if (!matrix_qr_factor_complex128(q_work, r_work, rows, cols)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
        }

        usz[2] q_shape_buf_complex = { rows, cols };
        Value* q_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, q_shape_buf_complex[0:2], "matrix/qr: failed to allocate Complex128 Q tensor");
        if (q_value == null || q_value.tag == ERROR) return q_value;
        bool keep_q_value = false;
        defer if (!keep_q_value) {
            boundary_cleanup_materialized_value(null, q_value, interp.current_scope);
        }
        if (q_value.tag != TENSOR || !tensor_has_valid_storage(q_value.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: Q tensor backing storage is missing");
        }
        if (matrix.byte_len > 0) mem::copy(q_value.tensor_val.data, q_work, matrix.byte_len);

        Value* q_for_dict = boundary_promote_to_root(q_value, interp);
        if (q_for_dict == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote Q tensor", null);
        }
        if (q_for_dict.tag == ERROR) return q_for_dict;
        keep_q_value = q_for_dict == q_value;

        usz[2] r_shape_buf_complex = { cols, cols };
        Value* r_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, r_shape_buf_complex[0:2], "matrix/qr: failed to allocate Complex128 R tensor");
        if (r_value == null || r_value.tag == ERROR) return r_value;
        bool keep_r_value = false;
        defer if (!keep_r_value) {
            boundary_cleanup_materialized_value(null, r_value, interp.current_scope);
        }
        if (r_value.tag != TENSOR || !tensor_has_valid_storage(r_value.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: R tensor backing storage is missing");
        }
        usz r_bytes = 0;
        if (!tensor_checked_mul_usz(r_count, Complex128Val.sizeof, &r_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        if (r_bytes > 0) mem::copy(r_value.tensor_val.data, r_work, r_bytes);

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
        return out;
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        usz q_count = 0;
        if (!tensor_checked_mul_usz(rows, cols, &q_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: Q workspace is too large", null);
        }
        Complex64Val* q_work = null;
        if (!matrix_complex64_workspace_zero(q_count, &q_work)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Complex64 Q workspace", null);
        }
        defer if (q_work != null) mem::free(q_work);
        if (matrix.byte_len > 0) mem::copy(q_work, matrix.data, matrix.byte_len);

        usz r_count = 0;
        if (!tensor_checked_mul_usz(cols, cols, &r_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        Complex64Val* r_work = null;
        if (!matrix_complex64_workspace_zero(r_count, &r_work)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Complex64 R workspace", null);
        }
        defer if (r_work != null) mem::free(r_work);

        if (!matrix_qr_factor_complex64(q_work, r_work, rows, cols)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
        }

        usz[2] q_shape_buf_complex = { rows, cols };
        Value* q_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, q_shape_buf_complex[0:2], "matrix/qr: failed to allocate Complex64 Q tensor");
        if (q_value == null || q_value.tag == ERROR) return q_value;
        bool keep_q_value = false;
        defer if (!keep_q_value) {
            boundary_cleanup_materialized_value(null, q_value, interp.current_scope);
        }
        if (q_value.tag != TENSOR || !tensor_has_valid_storage(q_value.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: Q tensor backing storage is missing");
        }
        if (matrix.byte_len > 0) mem::copy(q_value.tensor_val.data, q_work, matrix.byte_len);

        Value* q_for_dict = boundary_promote_to_root(q_value, interp);
        if (q_for_dict == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote Q tensor", null);
        }
        if (q_for_dict.tag == ERROR) return q_for_dict;
        keep_q_value = q_for_dict == q_value;

        usz[2] r_shape_buf_complex = { cols, cols };
        Value* r_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, r_shape_buf_complex[0:2], "matrix/qr: failed to allocate Complex64 R tensor");
        if (r_value == null || r_value.tag == ERROR) return r_value;
        bool keep_r_value = false;
        defer if (!keep_r_value) {
            boundary_cleanup_materialized_value(null, r_value, interp.current_scope);
        }
        if (r_value.tag != TENSOR || !tensor_has_valid_storage(r_value.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: R tensor backing storage is missing");
        }
        usz r_bytes = 0;
        if (!tensor_checked_mul_usz(r_count, Complex64Val.sizeof, &r_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        if (r_bytes > 0) mem::copy(r_value.tensor_val.data, r_work, r_bytes);

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
        return out;
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* q_work_f32 = null;
        if (!matrix_copy_float32_workspace(matrix, &q_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Float32 Q workspace", null);
        }
        defer if (q_work_f32 != null) mem::free(q_work_f32);

        usz r_count_f32 = 0;
        if (!tensor_checked_mul_usz(cols, cols, &r_count_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        usz r_bytes_f32 = 0;
        if (!tensor_checked_mul_usz(r_count_f32, float.sizeof, &r_bytes_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
        }
        float* r_work_f32 = null;
        if (r_bytes_f32 > 0) {
            r_work_f32 = (float*)mem::malloc(r_bytes_f32);
            if (r_work_f32 == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Float32 R workspace", null);
            }
            for (usz i = 0; i < r_count_f32; i++) {
                r_work_f32[i] = 0.0f;
            }
        }
        defer if (r_work_f32 != null) mem::free(r_work_f32);

        if (!matrix_qr_factor_f32(q_work_f32, r_work_f32, rows, cols)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
        }

        usz[2] q_shape_buf_f32 = { rows, cols };
        Value* q_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, q_shape_buf_f32[0:2], "matrix/qr: failed to allocate Float32 Q tensor");
        if (q_value_f32 == null || q_value_f32.tag == ERROR) return q_value_f32;
        bool keep_q_value_f32 = false;
        defer if (!keep_q_value_f32) {
            boundary_cleanup_materialized_value(null, q_value_f32, interp.current_scope);
        }
        if (q_value_f32.tag != TENSOR || !tensor_has_valid_storage(q_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: Q tensor backing storage is missing");
        }
        if (matrix.byte_len > 0) mem::copy(q_value_f32.tensor_val.data, q_work_f32, matrix.byte_len);

        Value* q_for_dict_f32 = boundary_promote_to_root(q_value_f32, interp);
        if (q_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote Q tensor", null);
        }
        if (q_for_dict_f32.tag == ERROR) return q_for_dict_f32;
        keep_q_value_f32 = q_for_dict_f32 == q_value_f32;

        usz[2] r_shape_buf_f32 = { cols, cols };
        Value* r_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, r_shape_buf_f32[0:2], "matrix/qr: failed to allocate Float32 R tensor");
        if (r_value_f32 == null || r_value_f32.tag == ERROR) return r_value_f32;
        bool keep_r_value_f32 = false;
        defer if (!keep_r_value_f32) {
            boundary_cleanup_materialized_value(null, r_value_f32, interp.current_scope);
        }
        if (r_value_f32.tag != TENSOR || !tensor_has_valid_storage(r_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/qr: R tensor backing storage is missing");
        }
        if (r_bytes_f32 > 0) mem::copy(r_value_f32.tensor_val.data, r_work_f32, r_bytes_f32);

        Value* r_for_dict_f32 = boundary_promote_to_root(r_value_f32, interp);
        if (r_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote R tensor", null);
        }
        if (r_for_dict_f32.tag == ERROR) return r_for_dict_f32;
        keep_r_value_f32 = r_for_dict_f32 == r_value_f32;

        Value* out_f32 = make_hashmap_checked(interp, 8, "matrix/qr: failed to allocate result dictionary");
        if (out_f32 == null || out_f32.tag == ERROR) return out_f32;
        if (out_f32.tag != HASHMAP || out_f32.hashmap_val == null) {
            return tensor_runtime_raise(interp, "matrix/qr: result dictionary backing storage is missing");
        }
        if (Value* err = explain_dict_set(out_f32, interp, "q", q_for_dict_f32)) return err;
        if (Value* err = explain_dict_set(out_f32, interp, "r", r_for_dict_f32)) return err;
        return out_f32;
    }

    double* q_work = null;
    if (!matrix_copy_float64_workspace(matrix, &q_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate Q workspace", null);
    }
    defer if (q_work != null) mem::free(q_work);

    usz r_count = 0;
    if (!tensor_checked_mul_usz(cols, cols, &r_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
    }
    usz r_bytes = 0;
    if (!tensor_checked_mul_usz(r_count, double.sizeof, &r_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: R workspace is too large", null);
    }
    double* r_work = null;
    if (r_bytes > 0) {
        r_work = (double*)mem::malloc(r_bytes);
        if (r_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to allocate R workspace", null);
        }
        for (usz i = 0; i < r_count; i++) {
            r_work[i] = 0.0;
        }
    }
    defer if (r_work != null) mem::free(r_work);

    bool factored = false;
    if (cols > 0) {
        int lapack_status = omni_tensor_backend_lapack_dgeqrf(rows, cols, q_work, r_work);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                factored = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                factored = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
            case OMNI_TENSOR_LAPACK_OUT_OF_MEMORY:
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: LAPACK workspace allocation failed", null);
            case OMNI_TENSOR_LAPACK_INVALID:
            case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                return tensor_runtime_raise(interp, "matrix/qr: LAPACK backend rejected factorization input");
        }
    }
    if (!factored && !matrix_qr_factor(q_work, r_work, rows, cols)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/qr: input matrix is rank deficient", null);
    }

    usz[2] q_shape_buf = { rows, cols };
    Value* q_value = make_tensor_double_uninitialized_checked(interp, q_shape_buf[0:2], "matrix/qr: failed to allocate Q tensor");
    if (q_value == null || q_value.tag == ERROR) return q_value;
    bool keep_q_value = false;
    defer if (!keep_q_value) {
        boundary_cleanup_materialized_value(null, q_value, interp.current_scope);
    }
    if (q_value.tag != TENSOR || !tensor_has_valid_storage(q_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/qr: Q tensor backing storage is missing");
    }
    if (matrix.byte_len > 0) mem::copy(q_value.tensor_val.data, q_work, matrix.byte_len);

    Value* q_for_dict = boundary_promote_to_root(q_value, interp);
    if (q_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/qr: failed to promote Q tensor", null);
    }
    if (q_for_dict.tag == ERROR) return q_for_dict;
    keep_q_value = q_for_dict == q_value;

    usz[2] r_shape_buf = { cols, cols };
    Value* r_value = make_tensor_double_uninitialized_checked(interp, r_shape_buf[0:2], "matrix/qr: failed to allocate R tensor");
    if (r_value == null || r_value.tag == ERROR) return r_value;
    bool keep_r_value = false;
    defer if (!keep_r_value) {
        boundary_cleanup_materialized_value(null, r_value, interp.current_scope);
    }
    if (r_value.tag != TENSOR || !tensor_has_valid_storage(r_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/qr: R tensor backing storage is missing");
    }
    if (r_bytes > 0) mem::copy(r_value.tensor_val.data, r_work, r_bytes);

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
    return out;
}

<*
@require interp != null
*>
fn Value* prim_matrix_cholesky(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/cholesky", "1 argument: square symmetric/Hermitian positive-definite Float64, Float32, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/cholesky: input must be a square symmetric/Hermitian positive-definite Float64, Float32, Complex128, or Complex64 Tensor");

    TensorVal* matrix_source = args[0].tensor_val;
    Value* matrix_value = matrix_source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    if (
        matrix == null ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        ) ||
        matrix.rank != 2 ||
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1]
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/cholesky: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    usz n = matrix.shape[0];
    usz[2] result_shape_buf = { n, n };
    Value* vulkan_result = matrix_cholesky_try_vulkan_value(matrix, result_shape_buf[0:2], interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/cholesky: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/cholesky: tensor backing storage is missing");

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        Value* out_value_complex = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, result_shape_buf[0:2], "matrix/cholesky: failed to allocate Complex128 factor tensor");
        if (out_value_complex == null || out_value_complex.tag == ERROR) return out_value_complex;
        bool keep_out_complex = false;
        defer if (!keep_out_complex) {
            boundary_cleanup_materialized_value(null, out_value_complex, interp.current_scope);
        }
        if (out_value_complex.tag != TENSOR || !tensor_has_valid_storage(out_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/cholesky: factor tensor backing storage is missing");
        }

        Complex128Val* lower_complex = (Complex128Val*)out_value_complex.tensor_val.data;
        if (!matrix_cholesky_factor_complex128(matrix, lower_complex)) {
            return raise_error_with_payload_names(
                interp,
                "tensor",
                "tensor/not-positive-definite",
                "matrix/cholesky: input matrix is not Hermitian positive definite",
                null
            );
        }

        keep_out_complex = true;
        return out_value_complex;
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        Value* out_value_complex = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, result_shape_buf[0:2], "matrix/cholesky: failed to allocate Complex64 factor tensor");
        if (out_value_complex == null || out_value_complex.tag == ERROR) return out_value_complex;
        bool keep_out_complex = false;
        defer if (!keep_out_complex) {
            boundary_cleanup_materialized_value(null, out_value_complex, interp.current_scope);
        }
        if (out_value_complex.tag != TENSOR || !tensor_has_valid_storage(out_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/cholesky: factor tensor backing storage is missing");
        }

        Complex64Val* lower_complex = (Complex64Val*)out_value_complex.tensor_val.data;
        if (!matrix_cholesky_factor_complex64(matrix, lower_complex)) {
            return raise_error_with_payload_names(
                interp,
                "tensor",
                "tensor/not-positive-definite",
                "matrix/cholesky: input matrix is not Hermitian positive definite",
                null
            );
        }

        keep_out_complex = true;
        return out_value_complex;
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        Value* out_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape_buf[0:2], "matrix/cholesky: failed to allocate Float32 factor tensor");
        if (out_value_f32 == null || out_value_f32.tag == ERROR) return out_value_f32;
        bool keep_out_f32 = false;
        defer if (!keep_out_f32) {
            boundary_cleanup_materialized_value(null, out_value_f32, interp.current_scope);
        }
        if (out_value_f32.tag != TENSOR || !tensor_has_valid_storage(out_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/cholesky: factor tensor backing storage is missing");
        }

        float* lower_f32 = (float*)out_value_f32.tensor_val.data;
        if (!matrix_cholesky_copy_symmetric_lower_f32(matrix, lower_f32) || !matrix_cholesky_factor_f32(matrix, lower_f32)) {
            return raise_error_with_payload_names(
                interp,
                "tensor",
                "tensor/not-positive-definite",
                "matrix/cholesky: input matrix is not symmetric positive definite",
                null
            );
        }

        keep_out_f32 = true;
        return out_value_f32;
    }

    Value* out_value = make_tensor_double_uninitialized_checked(interp, result_shape_buf[0:2], "matrix/cholesky: failed to allocate factor tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/cholesky: factor tensor backing storage is missing");
    }

    double* lower = (double*)out_value.tensor_val.data;
    if (!matrix_cholesky_copy_symmetric_lower(matrix, lower)) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/not-positive-definite",
            "matrix/cholesky: input matrix is not symmetric positive definite",
            null
        );
    }

    bool factored = false;
    if (n > 0) {
        int lapack_status = omni_tensor_backend_lapack_dpotrf(n, lower);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                factored = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                factored = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return raise_error_with_payload_names(
                    interp,
                    "tensor",
                    "tensor/not-positive-definite",
                    "matrix/cholesky: input matrix is not symmetric positive definite",
                    null
                );
            case OMNI_TENSOR_LAPACK_INVALID:
            case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                return tensor_runtime_raise(interp, "matrix/cholesky: LAPACK backend rejected factorization input");
        }
    }
    if (!factored && !matrix_cholesky_factor(matrix, lower)) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/not-positive-definite",
            "matrix/cholesky: input matrix is not symmetric positive definite",
            null
        );
    }

    keep_out = true;
    return out_value;
}
