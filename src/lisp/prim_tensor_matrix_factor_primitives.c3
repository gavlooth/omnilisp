module lisp;

import std::core::mem;
import main;

fn Value* prim_matrix_lu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/lu", "1 argument: square numeric Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/lu: input must be a rank-2 square numeric Tensor");

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
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1]
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/lu: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_lu_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/lu: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/lu: tensor backing storage is missing");

    usz n = matrix.shape[0];
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        return matrix_lu_complex128_cpu_value(matrix, n, interp);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_lu_complex64_cpu_value(matrix, n, interp);
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* lu_work_f32 = null;
        if (!matrix_copy_float32_workspace(matrix, &lu_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate Float32 factor workspace", null);
        }
        defer if (lu_work_f32 != null) mem::free(lu_work_f32);

        usz* pivot_work_f32 = null;
        if (n > 0) {
            if (n > usz.max / usz.sizeof) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: pivot workspace is too large", null);
            }
            pivot_work_f32 = (usz*)mem::malloc(n * usz.sizeof);
            if (pivot_work_f32 == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate pivot workspace", null);
            }
        }
        defer if (pivot_work_f32 != null) mem::free(pivot_work_f32);

        usz swap_count_f32 = 0;
        if (!matrix_lu_factor_f32(lu_work_f32, n, pivot_work_f32, &swap_count_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
        }

        if (n > (usz)long.max || swap_count_f32 > (usz)long.max) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: factor metadata is too large", null);
        }

        usz[2] result_shape_buf_f32 = { n, n };
        Value* lu_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape_buf_f32[0:2], "matrix/lu: failed to allocate Float32 factor tensor");
        if (lu_value_f32 == null || lu_value_f32.tag == ERROR) return lu_value_f32;
        bool keep_lu_value_f32 = false;
        defer if (!keep_lu_value_f32) {
            boundary_cleanup_materialized_value(null, lu_value_f32, interp.current_scope);
        }
        if (lu_value_f32.tag != TENSOR || !tensor_has_valid_storage(lu_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/lu: factor tensor backing storage is missing");
        }
        if (matrix.byte_len > 0) mem::copy(lu_value_f32.tensor_val.data, lu_work_f32, matrix.byte_len);

        Value* lu_for_dict_f32 = boundary_promote_to_root(lu_value_f32, interp);
        if (lu_for_dict_f32 == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to promote factor tensor", null);
        }
        if (lu_for_dict_f32.tag == ERROR) return lu_for_dict_f32;
        keep_lu_value_f32 = lu_for_dict_f32 == lu_value_f32;

        Value* pivots_value_f32 = make_array_checked(interp, n, "matrix/lu: failed to allocate pivot array");
        if (pivots_value_f32 == null || pivots_value_f32.tag == ERROR) return pivots_value_f32;
        if (pivots_value_f32.tag != ARRAY || pivots_value_f32.array_val == null) {
            return tensor_runtime_raise(interp, "matrix/lu: pivot array backing storage is missing");
        }
        for (usz i = 0; i < n; i++) {
            Value* pivot_entry = make_int(interp, (long)pivot_work_f32[i]);
            Value* promoted = boundary_promote_to_root(pivot_entry, interp);
            if (promoted == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to promote pivot entry", null);
            }
            if (promoted.tag == ERROR) return promoted;
            pivots_value_f32.array_val.items[i] = promoted;
            pivots_value_f32.array_val.length++;
        }

        Value* out_f32 = make_hashmap_checked(interp, 8, "matrix/lu: failed to allocate result dictionary");
        if (out_f32 == null || out_f32.tag == ERROR) return out_f32;
        if (out_f32.tag != HASHMAP || out_f32.hashmap_val == null) {
            return tensor_runtime_raise(interp, "matrix/lu: result dictionary backing storage is missing");
        }
        if (Value* err = explain_dict_set(out_f32, interp, "lu", lu_for_dict_f32)) return err;
        if (Value* err = explain_dict_set(out_f32, interp, "pivots", pivots_value_f32)) return err;
        if (Value* err = explain_dict_set(out_f32, interp, "swap-count", make_int(interp, (long)swap_count_f32))) return err;
        return out_f32;
    }

    double* lu_work = null;
    if (matrix.byte_len > 0) {
        lu_work = (double*)mem::malloc(matrix.byte_len);
        if (lu_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate factor workspace", null);
        }
        mem::copy(lu_work, matrix.data, matrix.byte_len);
    }
    defer if (lu_work != null) mem::free(lu_work);

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
    bool factored = false;
    if (n > 0) {
        int lapack_status = omni_tensor_backend_lapack_dgetrf(n, lu_work, pivot_work, &swap_count);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                factored = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                factored = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
            case OMNI_TENSOR_LAPACK_OUT_OF_MEMORY:
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: LAPACK pivot workspace allocation failed", null);
            default:
                return tensor_runtime_raise(interp, "matrix/lu: LAPACK backend rejected factorization input");
        }
    }
    if (!factored && !matrix_lu_factor(lu_work, n, pivot_work, &swap_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
    }

    if (n > (usz)long.max || swap_count > (usz)long.max) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: factor metadata is too large", null);
    }

    usz[2] result_shape_buf = { n, n };
    Value* lu_value = make_tensor_double_uninitialized_checked(interp, result_shape_buf[0:2], "matrix/lu: failed to allocate factor tensor");
    if (lu_value == null || lu_value.tag == ERROR) return lu_value;
    bool keep_lu_value = false;
    defer if (!keep_lu_value) {
        boundary_cleanup_materialized_value(null, lu_value, interp.current_scope);
    }
    if (lu_value.tag != TENSOR || !tensor_has_valid_storage(lu_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/lu: factor tensor backing storage is missing");
    }
    if (matrix.byte_len > 0) mem::copy(lu_value.tensor_val.data, lu_work, matrix.byte_len);

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
    return out;
}

fn Value* prim_matrix_determinant(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/determinant", "1 argument: square numeric Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/determinant: input must be a rank-2 square numeric Tensor");

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
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1]
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/determinant: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_determinant_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/determinant: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/determinant: tensor backing storage is missing");

    usz n = matrix.shape[0];
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        return matrix_determinant_complex128_cpu_value(matrix, n, interp);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_determinant_complex64_cpu_value(matrix, n, interp);
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* lu_work_f32 = null;
        if (!matrix_copy_float32_workspace(matrix, &lu_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate Float32 factor workspace", null);
        }
        defer if (lu_work_f32 != null) mem::free(lu_work_f32);

        usz* pivot_work_f32 = null;
        if (n > 0) {
            if (n > usz.max / usz.sizeof) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: pivot workspace is too large", null);
            }
            pivot_work_f32 = (usz*)mem::malloc(n * usz.sizeof);
            if (pivot_work_f32 == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate pivot workspace", null);
            }
        }
        defer if (pivot_work_f32 != null) mem::free(pivot_work_f32);

        usz swap_count_f32 = 0;
        if (!matrix_lu_factor_f32(lu_work_f32, n, pivot_work_f32, &swap_count_f32)) {
            return make_double(interp, 0.0);
        }

        double determinant_f32 = (swap_count_f32 % 2 == 0) ? 1.0 : -1.0;
        for (usz i = 0; i < n; i++) {
            determinant_f32 *= (double)lu_work_f32[i * n + i];
        }
        return make_double(interp, determinant_f32);
    }

    double* lu_work = null;
    if (!matrix_copy_float64_workspace(matrix, &lu_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate factor workspace", null);
    }
    defer if (lu_work != null) mem::free(lu_work);

    usz* pivot_work = null;
    if (n > 0) {
        if (n > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: pivot workspace is too large", null);
        }
        pivot_work = (usz*)mem::malloc(n * usz.sizeof);
        if (pivot_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate pivot workspace", null);
        }
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    bool factored = false;
    if (n > 0) {
        int lapack_status = omni_tensor_backend_lapack_dgetrf(n, lu_work, pivot_work, &swap_count);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                factored = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                factored = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return make_double(interp, 0.0);
            case OMNI_TENSOR_LAPACK_OUT_OF_MEMORY:
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: LAPACK pivot workspace allocation failed", null);
            default:
                return tensor_runtime_raise(interp, "matrix/determinant: LAPACK backend rejected factorization input");
        }
    }
    if (!factored && !matrix_lu_factor(lu_work, n, pivot_work, &swap_count)) {
        return make_double(interp, 0.0);
    }

    double determinant = (swap_count % 2 == 0) ? 1.0 : -1.0;
    for (usz i = 0; i < n; i++) {
        determinant *= lu_work[i * n + i];
    }
    return make_double(interp, determinant);
}

fn Value* prim_matrix_inverse(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/inverse", "1 argument: square Float64, Float32, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/inverse: input must be a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor");

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
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1]
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/inverse: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    usz n = matrix.shape[0];
    usz[2] result_shape_buf = { n, n };
    Value* vulkan_result = matrix_inverse_try_vulkan_value(matrix, result_shape_buf[0:2], interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/inverse: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/inverse: tensor backing storage is missing");

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        return matrix_inverse_complex128_cpu_value(matrix, n, result_shape_buf[0:2], interp);
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_inverse_complex64_cpu_value(matrix, n, result_shape_buf[0:2], interp);
    }

    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* coefficient_work_f32 = null;
        if (!matrix_copy_float32_workspace(matrix, &coefficient_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Float32 coefficient workspace", null);
        }
        defer if (coefficient_work_f32 != null) mem::free(coefficient_work_f32);

        usz inverse_count_f32 = 0;
        if (!tensor_checked_mul_usz(n, n, &inverse_count_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
        }
        float* inverse_work_f32 = null;
        if (!matrix_float32_workspace_zero(inverse_count_f32, &inverse_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Float32 inverse workspace", null);
        }
        defer if (inverse_work_f32 != null) mem::free(inverse_work_f32);
        matrix_identity_f32(inverse_work_f32, n);

        if (!matrix_solve_gaussian_elimination_f32(coefficient_work_f32, inverse_work_f32, n, n)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
        }

        Value* out_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape_buf[0:2], "matrix/inverse: failed to allocate Float32 result tensor");
        if (out_value_f32 == null || out_value_f32.tag == ERROR) return out_value_f32;
        bool keep_out_f32 = false;
        defer if (!keep_out_f32) {
            boundary_cleanup_materialized_value(null, out_value_f32, interp.current_scope);
        }
        if (out_value_f32.tag != TENSOR || !tensor_has_valid_storage(out_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/inverse: result tensor backing storage is missing");
        }
        if (matrix.byte_len > 0) mem::copy(out_value_f32.tensor_val.data, inverse_work_f32, matrix.byte_len);
        keep_out_f32 = true;
        return out_value_f32;
    }

    double* coefficient_work = null;
    if (!matrix_copy_float64_workspace(matrix, &coefficient_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate coefficient workspace", null);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz inverse_count = 0;
    if (!tensor_checked_mul_usz(n, n, &inverse_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
    }
    double* inverse_work = null;
    if (!matrix_double_workspace_zero(inverse_count, &inverse_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate inverse workspace", null);
    }
    defer if (inverse_work != null) mem::free(inverse_work);
    matrix_identity(inverse_work, n);

    bool inverted = n == 0;
    if (n > 0) {
        int lapack_status = omni_tensor_backend_lapack_dgesv(n, n, coefficient_work, inverse_work);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                inverted = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                inverted = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
            case OMNI_TENSOR_LAPACK_OUT_OF_MEMORY:
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: LAPACK pivot workspace allocation failed", null);
            default:
                return tensor_runtime_raise(interp, "matrix/inverse: LAPACK backend rejected inverse input");
        }
    }
    if (!inverted && !matrix_solve_gaussian_elimination(coefficient_work, inverse_work, n, n)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
    }

    Value* out_value = make_tensor_double_uninitialized_checked(interp, result_shape_buf[0:2], "matrix/inverse: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/inverse: result tensor backing storage is missing");
    }
    if (matrix.byte_len > 0) mem::copy(out_value.tensor_val.data, inverse_work, matrix.byte_len);
    keep_out = true;
    return out_value;
}
