module lisp;

import std::core::mem;
import main;

enum MatrixNormKind {
    MATRIX_NORM_FROBENIUS,
    MATRIX_NORM_ONE,
    MATRIX_NORM_INFINITY,
    MATRIX_NORM_MAX,
    MATRIX_NORM_SPECTRAL,
    MATRIX_NORM_NUCLEAR,
}

fn Value* prim_matrix_solve(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) {
        return primitive_arity_raise(interp, "matrix/solve", "2 arguments: coefficient Tensor and right-hand Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/solve: coefficient matrix must be a rank-2 square numeric Tensor");
    if (!is_tensor(args[1])) return tensor_type_raise(interp, "matrix/solve: right-hand side must be a rank-1 or rank-2 matching numeric Tensor");

    TensorVal* coefficients_source = args[0].tensor_val;
    Value* coefficients_value = coefficients_source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (coefficients_value == null || coefficients_value.tag == ERROR) return coefficients_value;
    bool cleanup_coefficients = coefficients_value != args[0];
    defer if (cleanup_coefficients) {
        boundary_cleanup_materialized_value(null, coefficients_value, interp.current_scope);
    }

    TensorVal* rhs_source = args[1].tensor_val;
    Value* rhs_value = rhs_source.kind == TENSOR_PAYLOAD_CONCRETE ? args[1] : tensor_realize_to_value(args[1], interp);
    if (rhs_value == null || rhs_value.tag == ERROR) return rhs_value;
    bool cleanup_rhs = rhs_value != args[1];
    defer if (cleanup_rhs) {
        boundary_cleanup_materialized_value(null, rhs_value, interp.current_scope);
    }

    TensorVal* coefficients = coefficients_value.tensor_val;
    TensorVal* rhs = rhs_value.tensor_val;
    usz n = 0;
    usz rhs_cols = 0;
    bool rhs_vector = false;
    if (!matrix_solve_validate_inputs(coefficients, rhs, &n, &rhs_cols, &rhs_vector)) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/solve: expected square Float64, Float32, Complex128, or Complex64 coefficient Tensor and matching rank-1 or rank-2 right-hand Tensor",
            null
        );
    }

    usz[2] result_shape_buf = { n, rhs_cols };
    usz[] result_shape = rhs_vector ? result_shape_buf[0:1] : result_shape_buf[0:2];
    Value* vulkan_result = matrix_solve_try_vulkan_value(coefficients, rhs, n, rhs_cols, result_shape, interp);
    if (vulkan_result != null) return vulkan_result;
    if (coefficients.device != TENSOR_DEVICE_CPU || rhs.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/solve: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(coefficients)) return tensor_runtime_raise(interp, "matrix/solve: coefficient tensor backing storage is missing");
    if (!tensor_has_valid_storage(rhs)) return tensor_runtime_raise(interp, "matrix/solve: right-hand tensor backing storage is missing");

    if (coefficients.dtype == TENSOR_DTYPE_COMPLEX128) {
        return matrix_solve_complex128_cpu_value(coefficients, rhs, n, rhs_cols, result_shape, interp);
    }
    if (coefficients.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_solve_complex64_cpu_value(coefficients, rhs, n, rhs_cols, result_shape, interp);
    }

    if (coefficients.dtype == TENSOR_DTYPE_FLOAT32) {
        float* coefficient_work_f32 = null;
        if (!matrix_copy_float32_workspace(coefficients, &coefficient_work_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Float32 coefficient workspace", null);
        }
        defer if (coefficient_work_f32 != null) mem::free(coefficient_work_f32);

        usz rhs_work_count_f32 = 0;
        if (!tensor_checked_mul_usz(n, rhs_cols, &rhs_work_count_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
        }
        usz rhs_work_bytes_f32 = 0;
        if (!tensor_checked_mul_usz(rhs_work_count_f32, float.sizeof, &rhs_work_bytes_f32)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
        }
        float* rhs_work_f32 = null;
        if (rhs_work_bytes_f32 > 0) {
            rhs_work_f32 = (float*)mem::malloc(rhs_work_bytes_f32);
            if (rhs_work_f32 == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Float32 right-hand workspace", null);
            }
            mem::copy(rhs_work_f32, rhs.data, rhs_work_bytes_f32);
        }
        defer if (rhs_work_f32 != null) mem::free(rhs_work_f32);

        if (!matrix_solve_gaussian_elimination_f32(coefficient_work_f32, rhs_work_f32, n, rhs_cols)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
        }

        Value* out_value_f32 = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, result_shape, "matrix/solve: failed to allocate Float32 result tensor");
        if (out_value_f32 == null || out_value_f32.tag == ERROR) return out_value_f32;
        bool keep_out_f32 = false;
        defer if (!keep_out_f32) {
            boundary_cleanup_materialized_value(null, out_value_f32, interp.current_scope);
        }
        if (out_value_f32.tag != TENSOR || !tensor_has_valid_storage(out_value_f32.tensor_val)) {
            return tensor_runtime_raise(interp, "matrix/solve: result tensor backing storage is missing");
        }
        if (rhs_work_bytes_f32 > 0) mem::copy(out_value_f32.tensor_val.data, rhs_work_f32, rhs_work_bytes_f32);
        keep_out_f32 = true;
        return out_value_f32;
    }

    double* coefficient_work = null;
    if (coefficients.byte_len > 0) {
        coefficient_work = (double*)mem::malloc(coefficients.byte_len);
        if (coefficient_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate coefficient workspace", null);
        }
        mem::copy(coefficient_work, coefficients.data, coefficients.byte_len);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz rhs_work_count = 0;
    if (!tensor_checked_mul_usz(n, rhs_cols, &rhs_work_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    usz rhs_work_bytes = 0;
    if (!tensor_checked_mul_usz(rhs_work_count, double.sizeof, &rhs_work_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    double* rhs_work = null;
    if (rhs_work_bytes > 0) {
        rhs_work = (double*)mem::malloc(rhs_work_bytes);
        if (rhs_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate right-hand workspace", null);
        }
        mem::copy(rhs_work, rhs.data, rhs_work_bytes);
    }
    defer if (rhs_work != null) mem::free(rhs_work);

    bool solved = n == 0;
    if (n > 0) {
        int lapack_status = omni_tensor_backend_lapack_dgesv(n, rhs_cols, coefficient_work, rhs_work);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                solved = true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                solved = false;
            case OMNI_TENSOR_LAPACK_SINGULAR:
                return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
            case OMNI_TENSOR_LAPACK_OUT_OF_MEMORY:
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: LAPACK pivot workspace allocation failed", null);
            default:
                return tensor_runtime_raise(interp, "matrix/solve: LAPACK backend rejected solver input");
        }
    }
    if (!solved && !matrix_solve_gaussian_elimination(coefficient_work, rhs_work, n, rhs_cols)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
    }

    Value* out_value = make_tensor_double_uninitialized_checked(interp, result_shape, "matrix/solve: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/solve: result tensor backing storage is missing");
    }
    if (rhs_work_bytes > 0) mem::copy(out_value.tensor_val.data, rhs_work, rhs_work_bytes);
    keep_out = true;
    return out_value;
}
