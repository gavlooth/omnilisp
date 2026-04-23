module lisp;

import std::core::mem;
import main;

<*
@require interp != null
*>
fn Value* prim_matrix_singular_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/singular-values", "1 argument: rank-2 Float64, Float32, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/singular-values: input must be a rank-2 Float64, Float32, Complex128, or Complex64 Tensor");

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
        matrix.shape == null
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/singular-values: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_singular_values_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_singular_values_try_cuda_value(matrix, interp);
    if (cuda_result != null) return cuda_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/singular-values: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/singular-values: tensor backing storage is missing");

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz k = matrix_min_usz(rows, cols);

    usz u_count = 0;
    if (!tensor_checked_mul_usz(rows, k, &u_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: U workspace is too large", null);
    }
    usz v_count = 0;
    if (!tensor_checked_mul_usz(cols, k, &v_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: V workspace is too large", null);
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        double* s_work_complex = null;
        if (!matrix_double_workspace_zero(k, &s_work_complex)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate Complex128 singular value workspace", null);
        }
        defer if (s_work_complex != null) mem::free(s_work_complex);

        if (!matrix_complex128_realified_singular_values(matrix, s_work_complex, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/singular-values: failed to compute Complex128 singular values", null);
        }

        usz[1] result_shape_buf_complex = { k };
        Value* out_value_complex = make_tensor_double_uninitialized_checked(interp, result_shape_buf_complex[0:1], "matrix/singular-values: failed to allocate Complex128 singular value tensor");
        if (out_value_complex == null || out_value_complex.tag == ERROR) return out_value_complex;
        bool keep_out_complex = false;
        defer if (!keep_out_complex) {
            boundary_cleanup_materialized_value(null, out_value_complex, interp.current_scope);
        }
        if (out_value_complex.tag != TENSOR || !tensor_has_valid_storage(out_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/singular-values: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_complex = 0;
            if (!tensor_checked_mul_usz(k, double.sizeof, &s_bytes_complex)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: singular value tensor is too large", null);
            }
            mem::copy(out_value_complex.tensor_val.data, s_work_complex, s_bytes_complex);
        }

        keep_out_complex = true;
        return out_value_complex;
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        float* s_work_complex64 = null;
        if (!matrix_float32_workspace_zero(k, &s_work_complex64)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate Complex64 singular value workspace", null);
        }
        defer if (s_work_complex64 != null) mem::free(s_work_complex64);

        if (!matrix_complex64_realified_singular_values(matrix, s_work_complex64, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/singular-values: failed to compute Complex64 singular values", null);
        }

        usz[1] result_shape_buf_complex64 = { k };
        Value* out_value_complex64 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape_buf_complex64[0:1], "matrix/singular-values: failed to allocate Complex64 singular value tensor");
        if (out_value_complex64 == null || out_value_complex64.tag == ERROR) return out_value_complex64;
        bool keep_out_complex64 = false;
        defer if (!keep_out_complex64) {
            boundary_cleanup_materialized_value(null, out_value_complex64, interp.current_scope);
        }
        if (out_value_complex64.tag != TENSOR || !tensor_has_valid_storage(out_value_complex64.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/singular-values: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_complex64 = 0;
            if (!tensor_checked_mul_usz(k, float.sizeof, &s_bytes_complex64)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: singular value tensor is too large", null);
            }
            mem::copy(out_value_complex64.tensor_val.data, s_work_complex64, s_bytes_complex64);
        }

        keep_out_complex64 = true;
        return out_value_complex64;
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* u_work_f32 = null;
        if (!matrix_float32_workspace_zero(u_count, &u_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate Float32 U workspace", null);
        }
        defer if (u_work_f32 != null) mem::free(u_work_f32);

        float* s_work_f32 = null;
        if (!matrix_float32_workspace_zero(k, &s_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate Float32 singular value workspace", null);
        }
        defer if (s_work_f32 != null) mem::free(s_work_f32);

        float* v_work_f32 = null;
        if (!matrix_float32_workspace_zero(v_count, &v_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate Float32 V workspace", null);
        }
        defer if (v_work_f32 != null) mem::free(v_work_f32);

        if (!matrix_svd_factor_f32(matrix, u_work_f32, s_work_f32, v_work_f32, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/singular-values: failed to compute Float32 singular values", null);
        }

        usz[1] result_shape_buf_f32 = { k };
        Value* out_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape_buf_f32[0:1], "matrix/singular-values: failed to allocate Float32 singular value tensor");
        if (out_value_f32 == null || out_value_f32.tag == ERROR) return out_value_f32;
        bool keep_out_f32 = false;
        defer if (!keep_out_f32) {
            boundary_cleanup_materialized_value(null, out_value_f32, interp.current_scope);
        }
        if (out_value_f32.tag != TENSOR || !tensor_has_valid_storage(out_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/singular-values: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_f32 = 0;
            if (!tensor_checked_mul_usz(k, float.sizeof, &s_bytes_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: singular value tensor is too large", null);
            }
            mem::copy(out_value_f32.tensor_val.data, s_work_f32, s_bytes_f32);
        }

        keep_out_f32 = true;
        return out_value_f32;
    }

    double* u_work = null;
    if (!matrix_double_workspace_zero(u_count, &u_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate U workspace", null);
    }
    defer if (u_work != null) mem::free(u_work);

    double* s_work = null;
    if (!matrix_double_workspace_zero(k, &s_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate singular value workspace", null);
    }
    defer if (s_work != null) mem::free(s_work);

    double* v_work = null;
    if (!matrix_double_workspace_zero(v_count, &v_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: failed to allocate V workspace", null);
    }
    defer if (v_work != null) mem::free(v_work);

    if (!matrix_svd_factor(matrix, u_work, s_work, v_work, k)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/singular-values: failed to compute singular values", null);
    }

    usz[1] result_shape_buf = { k };
    Value* out_value = make_tensor_double_uninitialized_checked(interp, result_shape_buf[0:1], "matrix/singular-values: failed to allocate singular value tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/singular-values: singular value tensor backing storage is missing");
    }
    if (k > 0) {
        usz s_bytes = 0;
        if (!tensor_checked_mul_usz(k, double.sizeof, &s_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/singular-values: singular value tensor is too large", null);
        }
        mem::copy(out_value.tensor_val.data, s_work, s_bytes);
    }

    keep_out = true;
    return out_value;
}

<*
@require interp != null
*>
fn Value* prim_matrix_svd(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/svd", "1 argument: rank-2 Float64, Float32, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/svd: input must be a rank-2 Float64, Float32, Complex128, or Complex64 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
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
        matrix.shape == null
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/svd: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_svd_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_svd_try_cuda_value(matrix, interp);
    if (cuda_result != null) return cuda_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/svd: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/svd: tensor backing storage is missing");

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz k = matrix_min_usz(rows, cols);

    usz u_count = 0;
    if (!tensor_checked_mul_usz(rows, k, &u_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: U workspace is too large", null);
    }
    usz v_count = 0;
    if (!tensor_checked_mul_usz(cols, k, &v_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: V workspace is too large", null);
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val* u_work_complex = null;
        if (!matrix_complex128_workspace_zero(u_count, &u_work_complex)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex128 U workspace", null);
        }
        defer if (u_work_complex != null) mem::free(u_work_complex);

        double* s_work_complex = null;
        if (!matrix_double_workspace_zero(k, &s_work_complex)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex128 singular value workspace", null);
        }
        defer if (s_work_complex != null) mem::free(s_work_complex);

        Complex128Val* v_work_complex = null;
        if (!matrix_complex128_workspace_zero(v_count, &v_work_complex)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex128 V workspace", null);
        }
        defer if (v_work_complex != null) mem::free(v_work_complex);

        if (!matrix_svd_factor_complex128(matrix, u_work_complex, s_work_complex, v_work_complex, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/svd: failed to compute Complex128 reduced SVD", null);
        }

        usz[2] u_shape_buf_complex = { rows, k };
        Value* u_value_complex = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, u_shape_buf_complex[0:2], "matrix/svd: failed to allocate Complex128 U tensor");
        if (u_value_complex == null || u_value_complex.tag == ERROR) return u_value_complex;
        bool keep_u_value_complex = false;
        defer if (!keep_u_value_complex) {
            boundary_cleanup_materialized_value(null, u_value_complex, interp.current_scope);
        }
        if (u_value_complex.tag != TENSOR || !tensor_has_valid_storage(u_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: U tensor backing storage is missing");
        }
        if (u_count > 0) {
            usz u_bytes_complex = 0;
            if (!tensor_checked_mul_usz(u_count, Complex128Val.sizeof, &u_bytes_complex)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: U tensor is too large", null);
            }
            mem::copy(u_value_complex.tensor_val.data, u_work_complex, u_bytes_complex);
        }

        Value* u_for_dict_complex = boundary_promote_to_root(u_value_complex, interp);
        if (u_for_dict_complex == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote U tensor", null);
        }
        if (u_for_dict_complex.tag == ERROR) return u_for_dict_complex;
        keep_u_value_complex = u_for_dict_complex == u_value_complex;

        usz[1] s_shape_buf_complex = { k };
        Value* s_value_complex = make_tensor_double_uninitialized_checked(interp, s_shape_buf_complex[0:1], "matrix/svd: failed to allocate Complex128 singular value tensor");
        if (s_value_complex == null || s_value_complex.tag == ERROR) return s_value_complex;
        bool keep_s_value_complex = false;
        defer if (!keep_s_value_complex) {
            boundary_cleanup_materialized_value(null, s_value_complex, interp.current_scope);
        }
        if (s_value_complex.tag != TENSOR || !tensor_has_valid_storage(s_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_complex = 0;
            if (!tensor_checked_mul_usz(k, double.sizeof, &s_bytes_complex)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: singular value tensor is too large", null);
            }
            mem::copy(s_value_complex.tensor_val.data, s_work_complex, s_bytes_complex);
        }

        Value* s_for_dict_complex = boundary_promote_to_root(s_value_complex, interp);
        if (s_for_dict_complex == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote singular value tensor", null);
        }
        if (s_for_dict_complex.tag == ERROR) return s_for_dict_complex;
        keep_s_value_complex = s_for_dict_complex == s_value_complex;

        usz[2] v_shape_buf_complex = { cols, k };
        Value* v_value_complex = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, v_shape_buf_complex[0:2], "matrix/svd: failed to allocate Complex128 V tensor");
        if (v_value_complex == null || v_value_complex.tag == ERROR) return v_value_complex;
        bool keep_v_value_complex = false;
        defer if (!keep_v_value_complex) {
            boundary_cleanup_materialized_value(null, v_value_complex, interp.current_scope);
        }
        if (v_value_complex.tag != TENSOR || !tensor_has_valid_storage(v_value_complex.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: V tensor backing storage is missing");
        }
        if (v_count > 0) {
            usz v_bytes_complex = 0;
            if (!tensor_checked_mul_usz(v_count, Complex128Val.sizeof, &v_bytes_complex)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: V tensor is too large", null);
            }
            mem::copy(v_value_complex.tensor_val.data, v_work_complex, v_bytes_complex);
        }

        Value* v_for_dict_complex = boundary_promote_to_root(v_value_complex, interp);
        if (v_for_dict_complex == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote V tensor", null);
        }
        if (v_for_dict_complex.tag == ERROR) return v_for_dict_complex;
        keep_v_value_complex = v_for_dict_complex == v_value_complex;

        Value* out_complex = make_hashmap_checked(interp, 8, "matrix/svd: failed to allocate result dictionary");
        if (out_complex == null || out_complex.tag == ERROR) return out_complex;
        if (out_complex.tag != HASHMAP || out_complex.hashmap_val == null) {
            return tensor_runtime_raise(interp, "matrix/svd: result dictionary backing storage is missing");
        }
        if (Value* err = explain_dict_set(out_complex, interp, "u", u_for_dict_complex)) return err;
        if (Value* err = explain_dict_set(out_complex, interp, "s", s_for_dict_complex)) return err;
        if (Value* err = explain_dict_set(out_complex, interp, "v", v_for_dict_complex)) return err;
        return out_complex;
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val* u_work_complex64 = null;
        if (!matrix_complex64_workspace_zero(u_count, &u_work_complex64)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex64 U workspace", null);
        }
        defer if (u_work_complex64 != null) mem::free(u_work_complex64);

        float* s_work_complex64 = null;
        if (!matrix_float32_workspace_zero(k, &s_work_complex64)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex64 singular value workspace", null);
        }
        defer if (s_work_complex64 != null) mem::free(s_work_complex64);

        Complex64Val* v_work_complex64 = null;
        if (!matrix_complex64_workspace_zero(v_count, &v_work_complex64)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Complex64 V workspace", null);
        }
        defer if (v_work_complex64 != null) mem::free(v_work_complex64);

        if (!matrix_svd_factor_complex64(matrix, u_work_complex64, s_work_complex64, v_work_complex64, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/svd: failed to compute Complex64 reduced SVD", null);
        }

        usz[2] u_shape_buf_complex64 = { rows, k };
        Value* u_value_complex64 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, u_shape_buf_complex64[0:2], "matrix/svd: failed to allocate Complex64 U tensor");
        if (u_value_complex64 == null || u_value_complex64.tag == ERROR) return u_value_complex64;
        bool keep_u_value_complex64 = false;
        defer if (!keep_u_value_complex64) {
            boundary_cleanup_materialized_value(null, u_value_complex64, interp.current_scope);
        }
        if (u_value_complex64.tag != TENSOR || !tensor_has_valid_storage(u_value_complex64.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: U tensor backing storage is missing");
        }
        if (u_count > 0) {
            usz u_bytes_complex64 = 0;
            if (!tensor_checked_mul_usz(u_count, Complex64Val.sizeof, &u_bytes_complex64)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: U tensor is too large", null);
            }
            mem::copy(u_value_complex64.tensor_val.data, u_work_complex64, u_bytes_complex64);
        }

        Value* u_for_dict_complex64 = boundary_promote_to_root(u_value_complex64, interp);
        if (u_for_dict_complex64 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote U tensor", null);
        }
        if (u_for_dict_complex64.tag == ERROR) return u_for_dict_complex64;
        keep_u_value_complex64 = u_for_dict_complex64 == u_value_complex64;

        usz[1] s_shape_buf_complex64 = { k };
        Value* s_value_complex64 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, s_shape_buf_complex64[0:1], "matrix/svd: failed to allocate Complex64 singular value tensor");
        if (s_value_complex64 == null || s_value_complex64.tag == ERROR) return s_value_complex64;
        bool keep_s_value_complex64 = false;
        defer if (!keep_s_value_complex64) {
            boundary_cleanup_materialized_value(null, s_value_complex64, interp.current_scope);
        }
        if (s_value_complex64.tag != TENSOR || !tensor_has_valid_storage(s_value_complex64.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_complex64 = 0;
            if (!tensor_checked_mul_usz(k, float.sizeof, &s_bytes_complex64)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: singular value tensor is too large", null);
            }
            mem::copy(s_value_complex64.tensor_val.data, s_work_complex64, s_bytes_complex64);
        }

        Value* s_for_dict_complex64 = boundary_promote_to_root(s_value_complex64, interp);
        if (s_for_dict_complex64 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote singular value tensor", null);
        }
        if (s_for_dict_complex64.tag == ERROR) return s_for_dict_complex64;
        keep_s_value_complex64 = s_for_dict_complex64 == s_value_complex64;

        usz[2] v_shape_buf_complex64 = { cols, k };
        Value* v_value_complex64 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, v_shape_buf_complex64[0:2], "matrix/svd: failed to allocate Complex64 V tensor");
        if (v_value_complex64 == null || v_value_complex64.tag == ERROR) return v_value_complex64;
        bool keep_v_value_complex64 = false;
        defer if (!keep_v_value_complex64) {
            boundary_cleanup_materialized_value(null, v_value_complex64, interp.current_scope);
        }
        if (v_value_complex64.tag != TENSOR || !tensor_has_valid_storage(v_value_complex64.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: V tensor backing storage is missing");
        }
        if (v_count > 0) {
            usz v_bytes_complex64 = 0;
            if (!tensor_checked_mul_usz(v_count, Complex64Val.sizeof, &v_bytes_complex64)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: V tensor is too large", null);
            }
            mem::copy(v_value_complex64.tensor_val.data, v_work_complex64, v_bytes_complex64);
        }

        Value* v_for_dict_complex64 = boundary_promote_to_root(v_value_complex64, interp);
        if (v_for_dict_complex64 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote V tensor", null);
        }
        if (v_for_dict_complex64.tag == ERROR) return v_for_dict_complex64;
        keep_v_value_complex64 = v_for_dict_complex64 == v_value_complex64;

        Value* out_complex64 = make_hashmap_checked(interp, 8, "matrix/svd: failed to allocate result dictionary");
        if (out_complex64 == null || out_complex64.tag == ERROR) return out_complex64;
        if (out_complex64.tag != HASHMAP || out_complex64.hashmap_val == null) {
            return tensor_runtime_raise(interp, "matrix/svd: result dictionary backing storage is missing");
        }
        if (Value* err = explain_dict_set(out_complex64, interp, "u", u_for_dict_complex64)) return err;
        if (Value* err = explain_dict_set(out_complex64, interp, "s", s_for_dict_complex64)) return err;
        if (Value* err = explain_dict_set(out_complex64, interp, "v", v_for_dict_complex64)) return err;
        return out_complex64;
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* u_work_f32 = null;
        if (!matrix_float32_workspace_zero(u_count, &u_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Float32 U workspace", null);
        }
        defer if (u_work_f32 != null) mem::free(u_work_f32);

        float* s_work_f32 = null;
        if (!matrix_float32_workspace_zero(k, &s_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Float32 singular value workspace", null);
        }
        defer if (s_work_f32 != null) mem::free(s_work_f32);

        float* v_work_f32 = null;
        if (!matrix_float32_workspace_zero(v_count, &v_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate Float32 V workspace", null);
        }
        defer if (v_work_f32 != null) mem::free(v_work_f32);

        if (!matrix_svd_factor_f32(matrix, u_work_f32, s_work_f32, v_work_f32, k)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/svd: failed to compute Float32 reduced SVD", null);
        }

        usz[2] u_shape_buf_f32 = { rows, k };
        Value* u_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, u_shape_buf_f32[0:2], "matrix/svd: failed to allocate Float32 U tensor");
        if (u_value_f32 == null || u_value_f32.tag == ERROR) return u_value_f32;
        bool keep_u_value_f32 = false;
        defer if (!keep_u_value_f32) {
            boundary_cleanup_materialized_value(null, u_value_f32, interp.current_scope);
        }
        if (u_value_f32.tag != TENSOR || !tensor_has_valid_storage(u_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: U tensor backing storage is missing");
        }
        if (u_count > 0) {
            usz u_bytes_f32 = 0;
            if (!tensor_checked_mul_usz(u_count, float.sizeof, &u_bytes_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: U tensor is too large", null);
            }
            mem::copy(u_value_f32.tensor_val.data, u_work_f32, u_bytes_f32);
        }

        Value* u_for_dict_f32 = boundary_promote_to_root(u_value_f32, interp);
        if (u_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote U tensor", null);
        }
        if (u_for_dict_f32.tag == ERROR) return u_for_dict_f32;
        keep_u_value_f32 = u_for_dict_f32 == u_value_f32;

        usz[1] s_shape_buf_f32 = { k };
        Value* s_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, s_shape_buf_f32[0:1], "matrix/svd: failed to allocate Float32 singular value tensor");
        if (s_value_f32 == null || s_value_f32.tag == ERROR) return s_value_f32;
        bool keep_s_value_f32 = false;
        defer if (!keep_s_value_f32) {
            boundary_cleanup_materialized_value(null, s_value_f32, interp.current_scope);
        }
        if (s_value_f32.tag != TENSOR || !tensor_has_valid_storage(s_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: singular value tensor backing storage is missing");
        }
        if (k > 0) {
            usz s_bytes_f32 = 0;
            if (!tensor_checked_mul_usz(k, float.sizeof, &s_bytes_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: singular value tensor is too large", null);
            }
            mem::copy(s_value_f32.tensor_val.data, s_work_f32, s_bytes_f32);
        }

        Value* s_for_dict_f32 = boundary_promote_to_root(s_value_f32, interp);
        if (s_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote singular value tensor", null);
        }
        if (s_for_dict_f32.tag == ERROR) return s_for_dict_f32;
        keep_s_value_f32 = s_for_dict_f32 == s_value_f32;

        usz[2] v_shape_buf_f32 = { cols, k };
        Value* v_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, v_shape_buf_f32[0:2], "matrix/svd: failed to allocate Float32 V tensor");
        if (v_value_f32 == null || v_value_f32.tag == ERROR) return v_value_f32;
        bool keep_v_value_f32 = false;
        defer if (!keep_v_value_f32) {
            boundary_cleanup_materialized_value(null, v_value_f32, interp.current_scope);
        }
        if (v_value_f32.tag != TENSOR || !tensor_has_valid_storage(v_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/svd: V tensor backing storage is missing");
        }
        if (v_count > 0) {
            usz v_bytes_f32 = 0;
            if (!tensor_checked_mul_usz(v_count, float.sizeof, &v_bytes_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: V tensor is too large", null);
            }
            mem::copy(v_value_f32.tensor_val.data, v_work_f32, v_bytes_f32);
        }

        Value* v_for_dict_f32 = boundary_promote_to_root(v_value_f32, interp);
        if (v_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote V tensor", null);
        }
        if (v_for_dict_f32.tag == ERROR) return v_for_dict_f32;
        keep_v_value_f32 = v_for_dict_f32 == v_value_f32;

        Value* out_f32 = make_hashmap_checked(interp, 8, "matrix/svd: failed to allocate result dictionary");
        if (out_f32 == null || out_f32.tag == ERROR) return out_f32;
        if (out_f32.tag != HASHMAP || out_f32.hashmap_val == null) {
            return tensor_runtime_raise(interp, "matrix/svd: result dictionary backing storage is missing");
        }
        if (Value* err = explain_dict_set(out_f32, interp, "u", u_for_dict_f32)) return err;
        if (Value* err = explain_dict_set(out_f32, interp, "s", s_for_dict_f32)) return err;
        if (Value* err = explain_dict_set(out_f32, interp, "v", v_for_dict_f32)) return err;
        return out_f32;
    }

    double* u_work = null;
    if (!matrix_double_workspace_zero(u_count, &u_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate U workspace", null);
    }
    defer if (u_work != null) mem::free(u_work);

    double* s_work = null;
    if (!matrix_double_workspace_zero(k, &s_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate singular value workspace", null);
    }
    defer if (s_work != null) mem::free(s_work);

    double* v_work = null;
    if (!matrix_double_workspace_zero(v_count, &v_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to allocate V workspace", null);
    }
    defer if (v_work != null) mem::free(v_work);

    if (!matrix_svd_factor(matrix, u_work, s_work, v_work, k)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/svd: failed to compute reduced SVD", null);
    }

    usz[2] u_shape_buf = { rows, k };
    Value* u_value = make_tensor_double_uninitialized_checked(interp, u_shape_buf[0:2], "matrix/svd: failed to allocate U tensor");
    if (u_value == null || u_value.tag == ERROR) return u_value;
    bool keep_u_value = false;
    defer if (!keep_u_value) {
        boundary_cleanup_materialized_value(null, u_value, interp.current_scope);
    }
    if (u_value.tag != TENSOR || !tensor_has_valid_storage(u_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/svd: U tensor backing storage is missing");
    }
    if (u_count > 0) {
        usz u_bytes = 0;
        if (!tensor_checked_mul_usz(u_count, double.sizeof, &u_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: U tensor is too large", null);
        }
        mem::copy(u_value.tensor_val.data, u_work, u_bytes);
    }

    Value* u_for_dict = boundary_promote_to_root(u_value, interp);
    if (u_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote U tensor", null);
    }
    if (u_for_dict.tag == ERROR) return u_for_dict;
    keep_u_value = u_for_dict == u_value;

    usz[1] s_shape_buf = { k };
    Value* s_value = make_tensor_double_uninitialized_checked(interp, s_shape_buf[0:1], "matrix/svd: failed to allocate singular value tensor");
    if (s_value == null || s_value.tag == ERROR) return s_value;
    bool keep_s_value = false;
    defer if (!keep_s_value) {
        boundary_cleanup_materialized_value(null, s_value, interp.current_scope);
    }
    if (s_value.tag != TENSOR || !tensor_has_valid_storage(s_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/svd: singular value tensor backing storage is missing");
    }
    if (k > 0) {
        usz s_bytes = 0;
        if (!tensor_checked_mul_usz(k, double.sizeof, &s_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: singular value tensor is too large", null);
        }
        mem::copy(s_value.tensor_val.data, s_work, s_bytes);
    }

    Value* s_for_dict = boundary_promote_to_root(s_value, interp);
    if (s_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote singular value tensor", null);
    }
    if (s_for_dict.tag == ERROR) return s_for_dict;
    keep_s_value = s_for_dict == s_value;

    usz[2] v_shape_buf = { cols, k };
    Value* v_value = make_tensor_double_uninitialized_checked(interp, v_shape_buf[0:2], "matrix/svd: failed to allocate V tensor");
    if (v_value == null || v_value.tag == ERROR) return v_value;
    bool keep_v_value = false;
    defer if (!keep_v_value) {
        boundary_cleanup_materialized_value(null, v_value, interp.current_scope);
    }
    if (v_value.tag != TENSOR || !tensor_has_valid_storage(v_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/svd: V tensor backing storage is missing");
    }
    if (v_count > 0) {
        usz v_bytes = 0;
        if (!tensor_checked_mul_usz(v_count, double.sizeof, &v_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: V tensor is too large", null);
        }
        mem::copy(v_value.tensor_val.data, v_work, v_bytes);
    }

    Value* v_for_dict = boundary_promote_to_root(v_value, interp);
    if (v_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote V tensor", null);
    }
    if (v_for_dict.tag == ERROR) return v_for_dict;
    keep_v_value = v_for_dict == v_value;

    Value* out = make_hashmap_checked(interp, 8, "matrix/svd: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/svd: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "u", u_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "s", s_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "v", v_for_dict)) return err;
    return out;
}
