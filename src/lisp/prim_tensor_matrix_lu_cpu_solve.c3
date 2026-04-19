module lisp;

import std::core::mem;
import main;

fn bool matrix_solve_gaussian_elimination(double* coefficients, double* rhs, usz n, usz rhs_cols) {
    if (n == 0) return true;
    if (coefficients == null || rhs == null) return false;
    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = tensor_c_fabs(coefficients[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = tensor_c_fabs(coefficients[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows(coefficients, n, pivot, pivot_row)) return false;
            if (!matrix_solve_swap_rows(rhs, rhs_cols, pivot, pivot_row)) return false;
        }

        double pivot_value = coefficients[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            double factor = coefficients[row * n + pivot] / pivot_value;
            coefficients[row * n + pivot] = 0.0;
            for (usz col = pivot + 1; col < n; col++) {
                coefficients[row * n + col] -= factor * coefficients[pivot * n + col];
            }
            for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
                rhs[row * rhs_cols + rhs_col] -= factor * rhs[pivot * rhs_cols + rhs_col];
            }
        }
    }

    for (usz i = n; i > 0; i--) {
        usz row = i - 1;
        double diagonal = coefficients[row * n + row];
        if (!(tensor_c_fabs(diagonal) > 0.0)) return false;
        for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
            double sum = rhs[row * rhs_cols + rhs_col];
            for (usz col = row + 1; col < n; col++) {
                sum -= coefficients[row * n + col] * rhs[col * rhs_cols + rhs_col];
            }
            rhs[row * rhs_cols + rhs_col] = sum / diagonal;
        }
    }
    return true;
}

fn bool matrix_solve_gaussian_elimination_f32(float* coefficients, float* rhs, usz n, usz rhs_cols) {
    if (n == 0) return true;
    if (coefficients == null || rhs == null) return false;
    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = tensor_c_fabs((double)coefficients[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = tensor_c_fabs((double)coefficients[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_f32(coefficients, n, pivot, pivot_row)) return false;
            if (!matrix_solve_swap_rows_f32(rhs, rhs_cols, pivot, pivot_row)) return false;
        }

        float pivot_value = coefficients[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            float factor = coefficients[row * n + pivot] / pivot_value;
            coefficients[row * n + pivot] = 0.0f;
            for (usz col = pivot + 1; col < n; col++) {
                coefficients[row * n + col] -= factor * coefficients[pivot * n + col];
            }
            for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
                rhs[row * rhs_cols + rhs_col] -= factor * rhs[pivot * rhs_cols + rhs_col];
            }
        }
    }

    for (usz i = n; i > 0; i--) {
        usz row = i - 1;
        float diagonal = coefficients[row * n + row];
        if (!(tensor_c_fabs((double)diagonal) > 0.0)) return false;
        for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
            float sum = rhs[row * rhs_cols + rhs_col];
            for (usz col = row + 1; col < n; col++) {
                sum -= coefficients[row * n + col] * rhs[col * rhs_cols + rhs_col];
            }
            rhs[row * rhs_cols + rhs_col] = sum / diagonal;
        }
    }
    return true;
}

fn bool matrix_solve_gaussian_elimination_complex128(Complex128Val* coefficients, Complex128Val* rhs, usz n, usz rhs_cols) {
    if (n == 0) return true;
    if (coefficients == null || rhs == null) return false;
    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = complex128_abs_value(coefficients[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = complex128_abs_value(coefficients[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_complex128(coefficients, n, pivot, pivot_row)) return false;
            if (!matrix_solve_swap_rows_complex128(rhs, rhs_cols, pivot, pivot_row)) return false;
        }

        Complex128Val pivot_value = coefficients[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            Complex128Val factor;
            if (!fixed_complex_binary128(coefficients[row * n + pivot], pivot_value, BIG_INTEGER_OP_DIV, &factor)) return false;
            coefficients[row * n + pivot].real = 0.0;
            coefficients[row * n + pivot].imag = 0.0;
            for (usz col = pivot + 1; col < n; col++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(factor, coefficients[pivot * n + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(coefficients[row * n + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                coefficients[row * n + col] = updated;
            }
            for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(factor, rhs[pivot * rhs_cols + rhs_col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(rhs[row * rhs_cols + rhs_col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                rhs[row * rhs_cols + rhs_col] = updated;
            }
        }
    }

    for (usz i = n; i > 0; i--) {
        usz row = i - 1;
        Complex128Val diagonal = coefficients[row * n + row];
        if (!(complex128_abs_value(diagonal) > 0.0)) return false;
        for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
            Complex128Val sum = rhs[row * rhs_cols + rhs_col];
            for (usz col = row + 1; col < n; col++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(coefficients[row * n + col], rhs[col * rhs_cols + rhs_col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(sum, product, BIG_INTEGER_OP_SUB, &updated)) return false;
                sum = updated;
            }
            if (!fixed_complex_binary128(sum, diagonal, BIG_INTEGER_OP_DIV, &rhs[row * rhs_cols + rhs_col])) return false;
        }
    }
    return true;
}

fn bool matrix_solve_gaussian_elimination_complex64(Complex64Val* coefficients, Complex64Val* rhs, usz n, usz rhs_cols) {
    if (n == 0) return true;
    if (coefficients == null || rhs == null) return false;
    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        float pivot_abs = complex64_abs_value(coefficients[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            float candidate_abs = complex64_abs_value(coefficients[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0f)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_complex64(coefficients, n, pivot, pivot_row)) return false;
            if (!matrix_solve_swap_rows_complex64(rhs, rhs_cols, pivot, pivot_row)) return false;
        }

        Complex64Val pivot_value = coefficients[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            Complex64Val factor;
            if (!fixed_complex_binary64(coefficients[row * n + pivot], pivot_value, BIG_INTEGER_OP_DIV, &factor)) return false;
            coefficients[row * n + pivot].real = 0.0f;
            coefficients[row * n + pivot].imag = 0.0f;
            for (usz col = pivot + 1; col < n; col++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(factor, coefficients[pivot * n + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(coefficients[row * n + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                coefficients[row * n + col] = updated;
            }
            for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(factor, rhs[pivot * rhs_cols + rhs_col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(rhs[row * rhs_cols + rhs_col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                rhs[row * rhs_cols + rhs_col] = updated;
            }
        }
    }

    for (usz i = n; i > 0; i--) {
        usz row = i - 1;
        Complex64Val diagonal = coefficients[row * n + row];
        if (!(complex64_abs_value(diagonal) > 0.0f)) return false;
        for (usz rhs_col = 0; rhs_col < rhs_cols; rhs_col++) {
            Complex64Val sum = rhs[row * rhs_cols + rhs_col];
            for (usz col = row + 1; col < n; col++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(coefficients[row * n + col], rhs[col * rhs_cols + rhs_col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(sum, product, BIG_INTEGER_OP_SUB, &updated)) return false;
                sum = updated;
            }
            if (!fixed_complex_binary64(sum, diagonal, BIG_INTEGER_OP_DIV, &rhs[row * rhs_cols + rhs_col])) return false;
        }
    }
    return true;
}

fn Value* matrix_solve_complex128_cpu_value(TensorVal* coefficients, TensorVal* rhs, usz n, usz rhs_cols, usz[] result_shape, Interp* interp) {
    Complex128Val* coefficient_work = null;
    if (coefficients.byte_len > 0) {
        coefficient_work = (Complex128Val*)mem::malloc(coefficients.byte_len);
        if (coefficient_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Complex128 coefficient workspace", null);
        }
        mem::copy(coefficient_work, coefficients.data, coefficients.byte_len);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz rhs_work_count = 0;
    if (!tensor_checked_mul_usz(n, rhs_cols, &rhs_work_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    usz rhs_work_bytes = 0;
    if (!tensor_checked_mul_usz(rhs_work_count, Complex128Val.sizeof, &rhs_work_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    Complex128Val* rhs_work = null;
    if (rhs_work_bytes > 0) {
        rhs_work = (Complex128Val*)mem::malloc(rhs_work_bytes);
        if (rhs_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Complex128 right-hand workspace", null);
        }
        mem::copy(rhs_work, rhs.data, rhs_work_bytes);
    }
    defer if (rhs_work != null) mem::free(rhs_work);

    if (!matrix_solve_gaussian_elimination_complex128(coefficient_work, rhs_work, n, rhs_cols)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, result_shape, "matrix/solve: failed to allocate Complex128 result tensor");
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

fn Value* matrix_solve_complex64_cpu_value(TensorVal* coefficients, TensorVal* rhs, usz n, usz rhs_cols, usz[] result_shape, Interp* interp) {
    Complex64Val* coefficient_work = null;
    if (coefficients.byte_len > 0) {
        coefficient_work = (Complex64Val*)mem::malloc(coefficients.byte_len);
        if (coefficient_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Complex64 coefficient workspace", null);
        }
        mem::copy(coefficient_work, coefficients.data, coefficients.byte_len);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz rhs_work_count = 0;
    if (!tensor_checked_mul_usz(n, rhs_cols, &rhs_work_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    usz rhs_work_bytes = 0;
    if (!tensor_checked_mul_usz(rhs_work_count, Complex64Val.sizeof, &rhs_work_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: right-hand workspace is too large", null);
    }
    Complex64Val* rhs_work = null;
    if (rhs_work_bytes > 0) {
        rhs_work = (Complex64Val*)mem::malloc(rhs_work_bytes);
        if (rhs_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/solve: failed to allocate Complex64 right-hand workspace", null);
        }
        mem::copy(rhs_work, rhs.data, rhs_work_bytes);
    }
    defer if (rhs_work != null) mem::free(rhs_work);

    if (!matrix_solve_gaussian_elimination_complex64(coefficient_work, rhs_work, n, rhs_cols)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/solve: coefficient matrix is singular", null);
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, result_shape, "matrix/solve: failed to allocate Complex64 result tensor");
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

fn Value* matrix_inverse_complex128_cpu_value(TensorVal* matrix, usz n, usz[] result_shape, Interp* interp) {
    Complex128Val* coefficient_work = null;
    if (matrix.byte_len > 0) {
        coefficient_work = (Complex128Val*)mem::malloc(matrix.byte_len);
        if (coefficient_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Complex128 coefficient workspace", null);
        }
        mem::copy(coefficient_work, matrix.data, matrix.byte_len);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz inverse_count = 0;
    if (!tensor_checked_mul_usz(n, n, &inverse_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
    }
    usz inverse_bytes = 0;
    if (!tensor_checked_mul_usz(inverse_count, Complex128Val.sizeof, &inverse_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
    }
    Complex128Val* inverse_work = null;
    if (!matrix_complex128_workspace_zero(inverse_count, &inverse_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Complex128 inverse workspace", null);
    }
    defer if (inverse_work != null) mem::free(inverse_work);
    matrix_identity_complex128(inverse_work, n);

    if (!matrix_solve_gaussian_elimination_complex128(coefficient_work, inverse_work, n, n)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, result_shape, "matrix/inverse: failed to allocate Complex128 result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/inverse: result tensor backing storage is missing");
    }
    if (inverse_bytes > 0) mem::copy(out_value.tensor_val.data, inverse_work, inverse_bytes);
    keep_out = true;
    return out_value;
}

fn Value* matrix_inverse_complex64_cpu_value(TensorVal* matrix, usz n, usz[] result_shape, Interp* interp) {
    Complex64Val* coefficient_work = null;
    if (matrix.byte_len > 0) {
        coefficient_work = (Complex64Val*)mem::malloc(matrix.byte_len);
        if (coefficient_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Complex64 coefficient workspace", null);
        }
        mem::copy(coefficient_work, matrix.data, matrix.byte_len);
    }
    defer if (coefficient_work != null) mem::free(coefficient_work);

    usz inverse_count = 0;
    if (!tensor_checked_mul_usz(n, n, &inverse_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
    }
    usz inverse_bytes = 0;
    if (!tensor_checked_mul_usz(inverse_count, Complex64Val.sizeof, &inverse_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: inverse workspace is too large", null);
    }
    Complex64Val* inverse_work = null;
    if (!matrix_complex64_workspace_zero(inverse_count, &inverse_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/inverse: failed to allocate Complex64 inverse workspace", null);
    }
    defer if (inverse_work != null) mem::free(inverse_work);
    matrix_identity_complex64(inverse_work, n);

    if (!matrix_solve_gaussian_elimination_complex64(coefficient_work, inverse_work, n, n)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/inverse: input matrix is singular", null);
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, result_shape, "matrix/inverse: failed to allocate Complex64 result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/inverse: result tensor backing storage is missing");
    }
    if (inverse_bytes > 0) mem::copy(out_value.tensor_val.data, inverse_work, inverse_bytes);
    keep_out = true;
    return out_value;
}

fn Value* matrix_lu_result_from_work(
    TensorDType dtype,
    void* lu_work,
    usz lu_byte_len,
    usz n,
    usz* pivot_work,
    usz swap_count,
    Interp* interp
) {
    if (n > (usz)long.max || swap_count > (usz)long.max) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: factor metadata is too large", null);
    }

    usz[2] result_shape_buf = { n, n };
    Value* lu_value = make_tensor_uninitialized_checked(interp, dtype, result_shape_buf[0:2], "matrix/lu: failed to allocate factor tensor");
    if (lu_value == null || lu_value.tag == ERROR) return lu_value;
    bool keep_lu_value = false;
    defer if (!keep_lu_value) {
        boundary_cleanup_materialized_value(null, lu_value, interp.current_scope);
    }
    if (lu_value.tag != TENSOR || !tensor_has_valid_storage(lu_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/lu: factor tensor backing storage is missing");
    }
    if (lu_byte_len > 0) mem::copy(lu_value.tensor_val.data, lu_work, lu_byte_len);

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

fn bool matrix_alloc_pivot_work(usz n, usz** out_pivots) {
    if (out_pivots == null) return false;
    *out_pivots = null;
    if (n == 0) return true;
    if (n > usz.max / usz.sizeof) return false;
    *out_pivots = (usz*)mem::malloc(n * usz.sizeof);
    return *out_pivots != null;
}

fn Value* matrix_lu_complex128_cpu_value(TensorVal* matrix, usz n, Interp* interp) {
    Complex128Val* lu_work = null;
    if (matrix.byte_len > 0) {
        lu_work = (Complex128Val*)mem::malloc(matrix.byte_len);
        if (lu_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate Complex128 factor workspace", null);
        }
        mem::copy(lu_work, matrix.data, matrix.byte_len);
    }
    defer if (lu_work != null) mem::free(lu_work);

    usz* pivot_work = null;
    if (!matrix_alloc_pivot_work(n, &pivot_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate pivot workspace", null);
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    if (!matrix_lu_factor_complex128(lu_work, n, pivot_work, &swap_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
    }
    return matrix_lu_result_from_work(TENSOR_DTYPE_COMPLEX128, lu_work, matrix.byte_len, n, pivot_work, swap_count, interp);
}

fn Value* matrix_lu_complex64_cpu_value(TensorVal* matrix, usz n, Interp* interp) {
    Complex64Val* lu_work = null;
    if (matrix.byte_len > 0) {
        lu_work = (Complex64Val*)mem::malloc(matrix.byte_len);
        if (lu_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate Complex64 factor workspace", null);
        }
        mem::copy(lu_work, matrix.data, matrix.byte_len);
    }
    defer if (lu_work != null) mem::free(lu_work);

    usz* pivot_work = null;
    if (!matrix_alloc_pivot_work(n, &pivot_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/lu: failed to allocate pivot workspace", null);
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    if (!matrix_lu_factor_complex64(lu_work, n, pivot_work, &swap_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/singular-matrix", "matrix/lu: coefficient matrix is singular", null);
    }
    return matrix_lu_result_from_work(TENSOR_DTYPE_COMPLEX64, lu_work, matrix.byte_len, n, pivot_work, swap_count, interp);
}

fn Value* matrix_determinant_complex128_cpu_value(TensorVal* matrix, usz n, Interp* interp) {
    Complex128Val* lu_work = null;
    if (matrix.byte_len > 0) {
        lu_work = (Complex128Val*)mem::malloc(matrix.byte_len);
        if (lu_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate Complex128 factor workspace", null);
        }
        mem::copy(lu_work, matrix.data, matrix.byte_len);
    }
    defer if (lu_work != null) mem::free(lu_work);

    usz* pivot_work = null;
    if (!matrix_alloc_pivot_work(n, &pivot_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate pivot workspace", null);
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    if (!matrix_lu_factor_complex128(lu_work, n, pivot_work, &swap_count)) {
        return make_complex128(interp, 0.0, 0.0);
    }

    Complex128Val determinant;
    determinant.real = (swap_count % 2 == 0) ? 1.0 : -1.0;
    determinant.imag = 0.0;
    for (usz i = 0; i < n; i++) {
        Complex128Val product;
        if (!fixed_complex_binary128(determinant, lu_work[i * n + i], BIG_INTEGER_OP_MUL, &product)) {
            return tensor_runtime_raise(interp, "matrix/determinant: Complex128 determinant overflowed");
        }
        determinant = product;
    }
    return make_complex128(interp, determinant.real, determinant.imag);
}

fn Value* matrix_determinant_complex64_cpu_value(TensorVal* matrix, usz n, Interp* interp) {
    Complex64Val* lu_work = null;
    if (matrix.byte_len > 0) {
        lu_work = (Complex64Val*)mem::malloc(matrix.byte_len);
        if (lu_work == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate Complex64 factor workspace", null);
        }
        mem::copy(lu_work, matrix.data, matrix.byte_len);
    }
    defer if (lu_work != null) mem::free(lu_work);

    usz* pivot_work = null;
    if (!matrix_alloc_pivot_work(n, &pivot_work)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/determinant: failed to allocate pivot workspace", null);
    }
    defer if (pivot_work != null) mem::free(pivot_work);

    usz swap_count = 0;
    if (!matrix_lu_factor_complex64(lu_work, n, pivot_work, &swap_count)) {
        return make_complex64(interp, 0.0f, 0.0f);
    }

    Complex64Val determinant;
    determinant.real = (swap_count % 2 == 0) ? 1.0f : -1.0f;
    determinant.imag = 0.0f;
    for (usz i = 0; i < n; i++) {
        Complex64Val product;
        if (!fixed_complex_binary64(determinant, lu_work[i * n + i], BIG_INTEGER_OP_MUL, &product)) {
            return tensor_runtime_raise(interp, "matrix/determinant: Complex64 determinant overflowed");
        }
        determinant = product;
    }
    return make_complex64(interp, determinant.real, determinant.imag);
}
