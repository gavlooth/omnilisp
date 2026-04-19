module lisp;

import std::core::mem;
import main;

fn bool matrix_solve_validate_inputs(
    TensorVal* coefficients,
    TensorVal* rhs,
    usz* out_n,
    usz* out_rhs_cols,
    bool* out_rhs_vector
) {
    if (out_n == null || out_rhs_cols == null || out_rhs_vector == null) return false;
    *out_n = 0;
    *out_rhs_cols = 0;
    *out_rhs_vector = false;
    if (coefficients == null || rhs == null) return false;
    if (
        (
            coefficients.dtype != TENSOR_DTYPE_DOUBLE &&
            coefficients.dtype != TENSOR_DTYPE_FLOAT32 &&
            coefficients.dtype != TENSOR_DTYPE_COMPLEX128 &&
            coefficients.dtype != TENSOR_DTYPE_COMPLEX64
        ) ||
        rhs.dtype != coefficients.dtype
    ) {
        return false;
    }
    if (coefficients.rank != 2 || coefficients.shape == null || coefficients.shape[0] != coefficients.shape[1]) return false;

    usz n = coefficients.shape[0];
    if (rhs.rank == 1) {
        if (rhs.shape == null || rhs.shape[0] != n) return false;
        *out_n = n;
        *out_rhs_cols = 1;
        *out_rhs_vector = true;
        return true;
    }
    if (rhs.rank == 2) {
        if (rhs.shape == null || rhs.shape[0] != n) return false;
        *out_n = n;
        *out_rhs_cols = rhs.shape[1];
        *out_rhs_vector = false;
        return true;
    }
    return false;
}

fn bool matrix_solve_swap_rows(double* values, usz cols, usz left_row, usz right_row) {
    if (values == null) return false;
    if (left_row == right_row) return true;
    for (usz col = 0; col < cols; col++) {
        usz left_index = left_row * cols + col;
        usz right_index = right_row * cols + col;
        double tmp = values[left_index];
        values[left_index] = values[right_index];
        values[right_index] = tmp;
    }
    return true;
}

fn bool matrix_solve_swap_rows_f32(float* values, usz cols, usz left_row, usz right_row) {
    if (values == null) return false;
    if (left_row == right_row) return true;
    for (usz col = 0; col < cols; col++) {
        usz left_index = left_row * cols + col;
        usz right_index = right_row * cols + col;
        float tmp = values[left_index];
        values[left_index] = values[right_index];
        values[right_index] = tmp;
    }
    return true;
}

fn bool matrix_solve_swap_rows_complex128(Complex128Val* values, usz cols, usz left_row, usz right_row) {
    if (values == null) return false;
    if (left_row == right_row) return true;
    for (usz col = 0; col < cols; col++) {
        usz left_index = left_row * cols + col;
        usz right_index = right_row * cols + col;
        Complex128Val tmp = values[left_index];
        values[left_index] = values[right_index];
        values[right_index] = tmp;
    }
    return true;
}

fn bool matrix_solve_swap_rows_complex64(Complex64Val* values, usz cols, usz left_row, usz right_row) {
    if (values == null) return false;
    if (left_row == right_row) return true;
    for (usz col = 0; col < cols; col++) {
        usz left_index = left_row * cols + col;
        usz right_index = right_row * cols + col;
        Complex64Val tmp = values[left_index];
        values[left_index] = values[right_index];
        values[right_index] = tmp;
    }
    return true;
}

fn bool matrix_lu_factor(double* lu, usz n, usz* pivots, usz* out_swap_count) {
    if (out_swap_count == null) return false;
    *out_swap_count = 0;
    if (n == 0) return true;
    if (lu == null || pivots == null) return false;

    for (usz i = 0; i < n; i++) {
        pivots[i] = i;
    }

    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = tensor_c_fabs(lu[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = tensor_c_fabs(lu[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows(lu, n, pivot, pivot_row)) return false;
            usz tmp_pivot = pivots[pivot];
            pivots[pivot] = pivots[pivot_row];
            pivots[pivot_row] = tmp_pivot;
            *out_swap_count += 1;
        }

        double pivot_value = lu[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            double factor = lu[row * n + pivot] / pivot_value;
            lu[row * n + pivot] = factor;
            for (usz col = pivot + 1; col < n; col++) {
                lu[row * n + col] -= factor * lu[pivot * n + col];
            }
        }
    }
    return true;
}

fn bool matrix_lu_factor_f32(float* lu, usz n, usz* pivots, usz* out_swap_count) {
    if (out_swap_count == null) return false;
    *out_swap_count = 0;
    if (n == 0) return true;
    if (lu == null || pivots == null) return false;

    for (usz i = 0; i < n; i++) {
        pivots[i] = i;
    }

    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = tensor_c_fabs((double)lu[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = tensor_c_fabs((double)lu[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_f32(lu, n, pivot, pivot_row)) return false;
            usz tmp_pivot = pivots[pivot];
            pivots[pivot] = pivots[pivot_row];
            pivots[pivot_row] = tmp_pivot;
            *out_swap_count += 1;
        }

        float pivot_value = lu[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            float factor = lu[row * n + pivot] / pivot_value;
            lu[row * n + pivot] = factor;
            for (usz col = pivot + 1; col < n; col++) {
                lu[row * n + col] -= factor * lu[pivot * n + col];
            }
        }
    }
    return true;
}

fn bool matrix_lu_factor_complex128(Complex128Val* lu, usz n, usz* pivots, usz* out_swap_count) {
    if (out_swap_count == null) return false;
    *out_swap_count = 0;
    if (n == 0) return true;
    if (lu == null || pivots == null) return false;

    for (usz i = 0; i < n; i++) {
        pivots[i] = i;
    }

    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        double pivot_abs = complex128_abs_value(lu[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            double candidate_abs = complex128_abs_value(lu[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_complex128(lu, n, pivot, pivot_row)) return false;
            usz tmp_pivot = pivots[pivot];
            pivots[pivot] = pivots[pivot_row];
            pivots[pivot_row] = tmp_pivot;
            *out_swap_count += 1;
        }

        Complex128Val pivot_value = lu[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            Complex128Val factor;
            if (!fixed_complex_binary128(lu[row * n + pivot], pivot_value, BIG_INTEGER_OP_DIV, &factor)) return false;
            lu[row * n + pivot] = factor;
            for (usz col = pivot + 1; col < n; col++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(factor, lu[pivot * n + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(lu[row * n + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                lu[row * n + col] = updated;
            }
        }
    }
    return true;
}

fn bool matrix_lu_factor_complex64(Complex64Val* lu, usz n, usz* pivots, usz* out_swap_count) {
    if (out_swap_count == null) return false;
    *out_swap_count = 0;
    if (n == 0) return true;
    if (lu == null || pivots == null) return false;

    for (usz i = 0; i < n; i++) {
        pivots[i] = i;
    }

    for (usz pivot = 0; pivot < n; pivot++) {
        usz pivot_row = pivot;
        float pivot_abs = complex64_abs_value(lu[pivot * n + pivot]);
        for (usz row = pivot + 1; row < n; row++) {
            float candidate_abs = complex64_abs_value(lu[row * n + pivot]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > 0.0f)) return false;

        if (pivot_row != pivot) {
            if (!matrix_solve_swap_rows_complex64(lu, n, pivot, pivot_row)) return false;
            usz tmp_pivot = pivots[pivot];
            pivots[pivot] = pivots[pivot_row];
            pivots[pivot_row] = tmp_pivot;
            *out_swap_count += 1;
        }

        Complex64Val pivot_value = lu[pivot * n + pivot];
        for (usz row = pivot + 1; row < n; row++) {
            Complex64Val factor;
            if (!fixed_complex_binary64(lu[row * n + pivot], pivot_value, BIG_INTEGER_OP_DIV, &factor)) return false;
            lu[row * n + pivot] = factor;
            for (usz col = pivot + 1; col < n; col++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(factor, lu[pivot * n + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(lu[row * n + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                lu[row * n + col] = updated;
            }
        }
    }
    return true;
}
