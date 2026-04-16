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

fn bool matrix_qr_factor(double* q, double* r, usz rows, usz cols) {
    if (cols == 0) return true;
    if (q == null || r == null) return false;

    for (usz col = 0; col < cols; col++) {
        for (usz prev_col = 0; prev_col < col; prev_col++) {
            double dot = 0.0;
            for (usz row = 0; row < rows; row++) {
                dot += q[row * cols + prev_col] * q[row * cols + col];
            }
            r[prev_col * cols + col] = dot;
            for (usz row = 0; row < rows; row++) {
                q[row * cols + col] -= dot * q[row * cols + prev_col];
            }
        }

        double norm_squared = 0.0;
        for (usz row = 0; row < rows; row++) {
            double value = q[row * cols + col];
            norm_squared += value * value;
        }
        if (!(norm_squared > 0.0)) return false;

        double norm = tensor_c_sqrt(norm_squared);
        if (!(norm > 0.0)) return false;
        r[col * cols + col] = norm;
        for (usz row = 0; row < rows; row++) {
            q[row * cols + col] /= norm;
        }
    }

    return true;
}

fn bool matrix_qr_factor_f32(float* q, float* r, usz rows, usz cols) {
    if (cols == 0) return true;
    if (q == null || r == null) return false;

    for (usz col = 0; col < cols; col++) {
        for (usz prev_col = 0; prev_col < col; prev_col++) {
            float dot = 0.0f;
            for (usz row = 0; row < rows; row++) {
                dot += q[row * cols + prev_col] * q[row * cols + col];
            }
            r[prev_col * cols + col] = dot;
            for (usz row = 0; row < rows; row++) {
                q[row * cols + col] -= dot * q[row * cols + prev_col];
            }
        }

        double norm_squared = 0.0;
        for (usz row = 0; row < rows; row++) {
            double value = (double)q[row * cols + col];
            norm_squared += value * value;
        }
        if (!(norm_squared > 0.0)) return false;

        float norm = (float)tensor_c_sqrt(norm_squared);
        if (!((double)norm > 0.0)) return false;
        r[col * cols + col] = norm;
        for (usz row = 0; row < rows; row++) {
            q[row * cols + col] /= norm;
        }
    }

    return true;
}

fn Complex128Val matrix_complex128_conjugate(Complex128Val z) @inline {
    Complex128Val out;
    out.real = z.real;
    out.imag = -z.imag;
    return out;
}

fn Complex64Val matrix_complex64_conjugate(Complex64Val z) @inline {
    Complex64Val out;
    out.real = z.real;
    out.imag = -z.imag;
    return out;
}

fn bool matrix_complex128_close(Complex128Val left, Complex128Val right, double tolerance) @inline {
    return tensor_c_fabs(left.real - right.real) <= tolerance && tensor_c_fabs(left.imag - right.imag) <= tolerance;
}

fn bool matrix_complex64_close(Complex64Val left, Complex64Val right, double tolerance) @inline {
    return tensor_c_fabs((double)(left.real - right.real)) <= tolerance && tensor_c_fabs((double)(left.imag - right.imag)) <= tolerance;
}

fn bool matrix_qr_factor_complex128(Complex128Val* q, Complex128Val* r, usz rows, usz cols) {
    if (cols == 0) return true;
    if (q == null || r == null) return false;

    for (usz col = 0; col < cols; col++) {
        for (usz prev_col = 0; prev_col < col; prev_col++) {
            Complex128Val dot;
            dot.real = 0.0;
            dot.imag = 0.0;
            for (usz row = 0; row < rows; row++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(matrix_complex128_conjugate(q[row * cols + prev_col]), q[row * cols + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(dot, product, BIG_INTEGER_OP_ADD, &updated)) return false;
                dot = updated;
            }
            r[prev_col * cols + col] = dot;
            for (usz row = 0; row < rows; row++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(q[row * cols + prev_col], dot, BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(q[row * cols + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                q[row * cols + col] = updated;
            }
        }

        double norm_squared = 0.0;
        for (usz row = 0; row < rows; row++) {
            double real = q[row * cols + col].real;
            double imag = q[row * cols + col].imag;
            norm_squared += real * real + imag * imag;
        }
        if (!(norm_squared > 1e-24)) return false;

        double norm = tensor_c_sqrt(norm_squared);
        if (!(norm > 1e-12)) return false;
        r[col * cols + col].real = norm;
        r[col * cols + col].imag = 0.0;
        for (usz row = 0; row < rows; row++) {
            q[row * cols + col].real /= norm;
            q[row * cols + col].imag /= norm;
        }
    }

    return true;
}

fn bool matrix_qr_factor_complex64(Complex64Val* q, Complex64Val* r, usz rows, usz cols) {
    if (cols == 0) return true;
    if (q == null || r == null) return false;

    for (usz col = 0; col < cols; col++) {
        for (usz prev_col = 0; prev_col < col; prev_col++) {
            Complex64Val dot;
            dot.real = 0.0f;
            dot.imag = 0.0f;
            for (usz row = 0; row < rows; row++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(matrix_complex64_conjugate(q[row * cols + prev_col]), q[row * cols + col], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(dot, product, BIG_INTEGER_OP_ADD, &updated)) return false;
                dot = updated;
            }
            r[prev_col * cols + col] = dot;
            for (usz row = 0; row < rows; row++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(q[row * cols + prev_col], dot, BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(q[row * cols + col], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                q[row * cols + col] = updated;
            }
        }

        double norm_squared = 0.0;
        for (usz row = 0; row < rows; row++) {
            double real = (double)q[row * cols + col].real;
            double imag = (double)q[row * cols + col].imag;
            norm_squared += real * real + imag * imag;
        }
        if (!(norm_squared > 1e-12)) return false;

        double norm = tensor_c_sqrt(norm_squared);
        if (!(norm > 1e-6)) return false;
        float norm_f32 = (float)norm;
        if (!((double)norm_f32 > 1e-6)) return false;
        r[col * cols + col].real = norm_f32;
        r[col * cols + col].imag = 0.0f;
        for (usz row = 0; row < rows; row++) {
            q[row * cols + col].real /= norm_f32;
            q[row * cols + col].imag /= norm_f32;
        }
    }

    return true;
}

fn bool matrix_cholesky_factor(TensorVal* matrix, double* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    double* source = (double*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i] = 0.0;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            if (source[row * n + col] != source[col * n + row]) return false;

            double sum = source[row * n + col];
            for (usz k = 0; k < col; k++) {
                sum -= lower[row * n + k] * lower[col * n + k];
            }

            if (row == col) {
                if (!(sum > 0.0)) return false;
                double diagonal = tensor_c_sqrt(sum);
                if (!(diagonal > 0.0)) return false;
                lower[row * n + col] = diagonal;
            } else {
                double diagonal = lower[col * n + col];
                if (!(diagonal > 0.0)) return false;
                lower[row * n + col] = sum / diagonal;
            }
        }
    }

    return true;
}

fn bool matrix_cholesky_factor_f32(TensorVal* matrix, float* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    float* source = (float*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i] = 0.0f;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            if (source[row * n + col] != source[col * n + row]) return false;

            float sum = source[row * n + col];
            for (usz k = 0; k < col; k++) {
                sum -= lower[row * n + k] * lower[col * n + k];
            }

            if (row == col) {
                if (!((double)sum > 0.0)) return false;
                float diagonal = (float)tensor_c_sqrt((double)sum);
                if (!((double)diagonal > 0.0)) return false;
                lower[row * n + col] = diagonal;
            } else {
                float diagonal = lower[col * n + col];
                if (!((double)diagonal > 0.0)) return false;
                lower[row * n + col] = sum / diagonal;
            }
        }
    }

    return true;
}

fn bool matrix_cholesky_factor_complex128(TensorVal* matrix, Complex128Val* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    Complex128Val* source = (Complex128Val*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i].real = 0.0;
        lower[i].imag = 0.0;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            Complex128Val mirrored = matrix_complex128_conjugate(source[col * n + row]);
            if (!matrix_complex128_close(source[row * n + col], mirrored, 1e-12)) return false;

            Complex128Val sum = source[row * n + col];
            for (usz k = 0; k < col; k++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(lower[row * n + k], matrix_complex128_conjugate(lower[col * n + k]), BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(sum, product, BIG_INTEGER_OP_SUB, &updated)) return false;
                sum = updated;
            }

            if (row == col) {
                if (tensor_c_fabs(sum.imag) > 1e-12 || !(sum.real > 1e-12)) return false;
                double diagonal = tensor_c_sqrt(sum.real);
                if (!(diagonal > 1e-12)) return false;
                lower[row * n + col].real = diagonal;
                lower[row * n + col].imag = 0.0;
            } else {
                double diagonal = lower[col * n + col].real;
                if (!(diagonal > 1e-12) || tensor_c_fabs(lower[col * n + col].imag) > 1e-12) return false;
                lower[row * n + col].real = sum.real / diagonal;
                lower[row * n + col].imag = sum.imag / diagonal;
            }
        }
    }

    return true;
}

fn bool matrix_cholesky_factor_complex64(TensorVal* matrix, Complex64Val* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    Complex64Val* source = (Complex64Val*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i].real = 0.0f;
        lower[i].imag = 0.0f;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            Complex64Val mirrored = matrix_complex64_conjugate(source[col * n + row]);
            if (!matrix_complex64_close(source[row * n + col], mirrored, 1e-5)) return false;

            Complex64Val sum = source[row * n + col];
            for (usz k = 0; k < col; k++) {
                Complex64Val product;
                Complex64Val updated;
                if (!fixed_complex_binary64(lower[row * n + k], matrix_complex64_conjugate(lower[col * n + k]), BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary64(sum, product, BIG_INTEGER_OP_SUB, &updated)) return false;
                sum = updated;
            }

            if (row == col) {
                if (tensor_c_fabs((double)sum.imag) > 1e-5 || !((double)sum.real > 1e-6)) return false;
                float diagonal = (float)tensor_c_sqrt((double)sum.real);
                if (!((double)diagonal > 1e-6)) return false;
                lower[row * n + col].real = diagonal;
                lower[row * n + col].imag = 0.0f;
            } else {
                float diagonal = lower[col * n + col].real;
                if (!((double)diagonal > 1e-6) || tensor_c_fabs((double)lower[col * n + col].imag) > 1e-5) return false;
                lower[row * n + col].real = sum.real / diagonal;
                lower[row * n + col].imag = sum.imag / diagonal;
            }
        }
    }

    return true;
}

fn bool matrix_cholesky_copy_symmetric_lower(TensorVal* matrix, double* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    double* source = (double*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i] = 0.0;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            if (source[row * n + col] != source[col * n + row]) return false;
            lower[row * n + col] = source[row * n + col];
        }
    }
    return true;
}

fn bool matrix_cholesky_copy_symmetric_lower_f32(TensorVal* matrix, float* lower) {
    if (matrix == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (lower == null) return false;
    float* source = (float*)matrix.data;
    if (source == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        lower[i] = 0.0f;
    }

    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col <= row; col++) {
            if (source[row * n + col] != source[col * n + row]) return false;
            lower[row * n + col] = source[row * n + col];
        }
    }
    return true;
}

fn usz matrix_min_usz(usz left, usz right) @inline {
    return left < right ? left : right;
}

fn bool matrix_double_workspace_alloc(usz count, double** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (count == 0) return true;
    usz bytes = 0;
    if (!tensor_checked_mul_usz(count, double.sizeof, &bytes)) return false;
    double* work = (double*)mem::malloc(bytes);
    if (work == null) return false;
    *out_work = work;
    return true;
}

fn bool matrix_double_workspace_zero(usz count, double** out_work) {
    if (!matrix_double_workspace_alloc(count, out_work)) return false;
    if (out_work == null || *out_work == null) return count == 0;
    for (usz i = 0; i < count; i++) {
        (*out_work)[i] = 0.0;
    }
    return true;
}

fn bool matrix_float32_workspace_alloc(usz count, float** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (count == 0) return true;
    usz bytes = 0;
    if (!tensor_checked_mul_usz(count, float.sizeof, &bytes)) return false;
    float* work = (float*)mem::malloc(bytes);
    if (work == null) return false;
    *out_work = work;
    return true;
}

fn bool matrix_float32_workspace_zero(usz count, float** out_work) {
    if (!matrix_float32_workspace_alloc(count, out_work)) return false;
    if (out_work == null || *out_work == null) return count == 0;
    for (usz i = 0; i < count; i++) {
        (*out_work)[i] = 0.0f;
    }
    return true;
}

fn void matrix_identity(double* values, usz n) {
    if (values == null) return;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            values[row * n + col] = row == col ? 1.0 : 0.0;
        }
    }
}

fn void matrix_identity_f32(float* values, usz n) {
    if (values == null) return;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            values[row * n + col] = row == col ? 1.0f : 0.0f;
        }
    }
}

fn bool matrix_complex128_workspace_zero(usz count, Complex128Val** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (count == 0) return true;
    usz bytes = 0;
    if (!tensor_checked_mul_usz(count, Complex128Val.sizeof, &bytes)) return false;
    Complex128Val* work = (Complex128Val*)mem::malloc(bytes);
    if (work == null) return false;
    for (usz i = 0; i < count; i++) {
        work[i].real = 0.0;
        work[i].imag = 0.0;
    }
    *out_work = work;
    return true;
}

fn bool matrix_complex64_workspace_zero(usz count, Complex64Val** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (count == 0) return true;
    usz bytes = 0;
    if (!tensor_checked_mul_usz(count, Complex64Val.sizeof, &bytes)) return false;
    Complex64Val* work = (Complex64Val*)mem::malloc(bytes);
    if (work == null) return false;
    for (usz i = 0; i < count; i++) {
        work[i].real = 0.0f;
        work[i].imag = 0.0f;
    }
    *out_work = work;
    return true;
}

fn void matrix_identity_complex128(Complex128Val* values, usz n) {
    if (values == null) return;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            usz index = row * n + col;
            values[index].real = row == col ? 1.0 : 0.0;
            values[index].imag = 0.0;
        }
    }
}

fn void matrix_identity_complex64(Complex64Val* values, usz n) {
    if (values == null) return;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            usz index = row * n + col;
            values[index].real = row == col ? 1.0f : 0.0f;
            values[index].imag = 0.0f;
        }
    }
}

fn bool matrix_rank_from_singular_values(double* singular_values, usz k, double tolerance, usz* out_rank) {
    if (out_rank == null) return false;
    *out_rank = 0;
    if (k == 0) return true;
    if (singular_values == null) return false;

    usz rank = 0;
    for (usz i = 0; i < k; i++) {
        if (tensor_c_fabs(singular_values[i]) > tolerance) rank += 1;
    }
    *out_rank = rank;
    return true;
}

fn bool matrix_rank_lapack_count(TensorVal* matrix, double tolerance, usz* out_rank) {
    if (out_rank == null) return false;
    *out_rank = 0;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_DOUBLE || matrix.rank != 2 || matrix.shape == null) return false;
    if (omni_tensor_backend_lapack_dgesvd_available() == 0) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz k = matrix_min_usz(rows, cols);
    if (k == 0) return true;

    double* work = null;
    if (!matrix_copy_float64_workspace(matrix, &work)) return false;
    defer if (work != null) mem::free(work);

    usz u_count = 0;
    if (!tensor_checked_mul_usz(rows, k, &u_count)) return false;
    usz v_count = 0;
    if (!tensor_checked_mul_usz(cols, k, &v_count)) return false;

    double* u = null;
    if (!matrix_double_workspace_zero(u_count, &u)) return false;
    defer if (u != null) mem::free(u);

    double* s = null;
    if (!matrix_double_workspace_zero(k, &s)) return false;
    defer if (s != null) mem::free(s);

    double* v = null;
    if (!matrix_double_workspace_zero(v_count, &v)) return false;
    defer if (v != null) mem::free(v);

    int lapack_status = omni_tensor_backend_lapack_dgesvd(rows, cols, work, u, s, v);
    if (lapack_status != OMNI_TENSOR_LAPACK_SUCCESS) return false;
    return matrix_rank_from_singular_values(s, k, tolerance, out_rank);
}

fn bool matrix_rank_elimination_count(TensorVal* matrix, double tolerance, usz* out_rank) {
    if (out_rank == null) return false;
    *out_rank = 0;
    if (
        matrix == null ||
        (matrix.dtype != TENSOR_DTYPE_DOUBLE && matrix.dtype != TENSOR_DTYPE_FLOAT32) ||
        matrix.rank != 2 ||
        matrix.shape == null
    ) {
        return false;
    }
    if (!conversion_double_is_finite(tolerance) || tolerance < 0.0) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (rows == 0 || cols == 0) return true;
    if (matrix.data == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(rows, cols, &count)) return false;
    double* work = null;
    if (!matrix_double_workspace_alloc(count, &work)) return false;
    defer if (work != null) mem::free(work);

    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        double* source = (double*)matrix.data;
        for (usz i = 0; i < count; i++) {
            work[i] = source[i];
        }
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        float* source = (float*)matrix.data;
        for (usz i = 0; i < count; i++) {
            work[i] = (double)source[i];
        }
    }

    usz rank = 0;
    for (usz col = 0; col < cols && rank < rows; col++) {
        usz pivot_row = rank;
        double pivot_abs = tensor_c_fabs(work[pivot_row * cols + col]);
        for (usz row = rank + 1; row < rows; row++) {
            double candidate_abs = tensor_c_fabs(work[row * cols + col]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }

        if (!(pivot_abs > tolerance)) continue;
        if (!matrix_solve_swap_rows(work, cols, rank, pivot_row)) return false;

        double pivot = work[rank * cols + col];
        if (!(tensor_c_fabs(pivot) > tolerance)) continue;
        for (usz row = rank + 1; row < rows; row++) {
            double factor = work[row * cols + col] / pivot;
            work[row * cols + col] = 0.0;
            for (usz c = col + 1; c < cols; c++) {
                work[row * cols + c] -= factor * work[rank * cols + c];
            }
        }
        rank += 1;
    }

    *out_rank = rank;
    return true;
}

fn bool matrix_rank_elimination_count_fixed_complex(TensorVal* matrix, double tolerance, usz* out_rank) {
    if (out_rank == null) return false;
    *out_rank = 0;
    if (
        matrix == null ||
        (matrix.dtype != TENSOR_DTYPE_COMPLEX128 && matrix.dtype != TENSOR_DTYPE_COMPLEX64) ||
        matrix.rank != 2 ||
        matrix.shape == null
    ) {
        return false;
    }
    if (!conversion_double_is_finite(tolerance) || tolerance < 0.0) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (rows == 0 || cols == 0) return true;
    if (matrix.data == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(rows, cols, &count)) return false;
    usz bytes = 0;
    if (!tensor_checked_mul_usz(count, Complex128Val.sizeof, &bytes)) return false;
    Complex128Val* work = (Complex128Val*)mem::malloc(bytes);
    if (work == null) return false;
    defer mem::free(work);

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val* source = (Complex128Val*)matrix.data;
        for (usz i = 0; i < count; i++) {
            work[i] = source[i];
        }
    } else {
        Complex64Val* source = (Complex64Val*)matrix.data;
        for (usz i = 0; i < count; i++) {
            work[i].real = (double)source[i].real;
            work[i].imag = (double)source[i].imag;
        }
    }

    usz rank = 0;
    for (usz col = 0; col < cols && rank < rows; col++) {
        usz pivot_row = rank;
        double pivot_abs = complex128_abs_value(work[pivot_row * cols + col]);
        for (usz row = rank + 1; row < rows; row++) {
            double candidate_abs = complex128_abs_value(work[row * cols + col]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }

        if (!(pivot_abs > tolerance)) continue;
        if (!matrix_solve_swap_rows_complex128(work, cols, rank, pivot_row)) return false;

        Complex128Val pivot = work[rank * cols + col];
        if (!(complex128_abs_value(pivot) > tolerance)) continue;
        for (usz row = rank + 1; row < rows; row++) {
            Complex128Val factor;
            if (!fixed_complex_binary128(work[row * cols + col], pivot, BIG_INTEGER_OP_DIV, &factor)) return false;
            work[row * cols + col].real = 0.0;
            work[row * cols + col].imag = 0.0;
            for (usz c = col + 1; c < cols; c++) {
                Complex128Val product;
                Complex128Val updated;
                if (!fixed_complex_binary128(factor, work[rank * cols + c], BIG_INTEGER_OP_MUL, &product)) return false;
                if (!fixed_complex_binary128(work[row * cols + c], product, BIG_INTEGER_OP_SUB, &updated)) return false;
                work[row * cols + c] = updated;
            }
        }
        rank += 1;
    }

    *out_rank = rank;
    return true;
}

fn bool matrix_rank_count(TensorVal* matrix, double tolerance, usz* out_rank) {
    if (matrix_rank_lapack_count(matrix, tolerance, out_rank)) return true;
    if (matrix != null && (matrix.dtype == TENSOR_DTYPE_COMPLEX128 || matrix.dtype == TENSOR_DTYPE_COMPLEX64)) {
        return matrix_rank_elimination_count_fixed_complex(matrix, tolerance, out_rank);
    }
    return matrix_rank_elimination_count(matrix, tolerance, out_rank);
}

fn bool matrix_norm_kind_from_arg(Value* arg, Interp* interp, MatrixNormKind* out_kind) {
    if (out_kind == null) return false;
    if (arg == null) {
        *out_kind = MATRIX_NORM_FROBENIUS;
        return true;
    }
    if (arg.tag != SYMBOL) return false;

    SymbolId frobenius = interp.symbols.intern("frobenius");
    SymbolId one = interp.symbols.intern("one");
    SymbolId infinity = interp.symbols.intern("infinity");
    SymbolId max = interp.symbols.intern("max");
    SymbolId spectral = interp.symbols.intern("spectral");
    SymbolId nuclear = interp.symbols.intern("nuclear");
    if (
        frobenius == INVALID_SYMBOL_ID ||
        one == INVALID_SYMBOL_ID ||
        infinity == INVALID_SYMBOL_ID ||
        max == INVALID_SYMBOL_ID ||
        spectral == INVALID_SYMBOL_ID ||
        nuclear == INVALID_SYMBOL_ID
    ) {
        return false;
    }

    if (arg.sym_val == frobenius) {
        *out_kind = MATRIX_NORM_FROBENIUS;
        return true;
    }
    if (arg.sym_val == one) {
        *out_kind = MATRIX_NORM_ONE;
        return true;
    }
    if (arg.sym_val == infinity) {
        *out_kind = MATRIX_NORM_INFINITY;
        return true;
    }
    if (arg.sym_val == max) {
        *out_kind = MATRIX_NORM_MAX;
        return true;
    }
    if (arg.sym_val == spectral) {
        *out_kind = MATRIX_NORM_SPECTRAL;
        return true;
    }
    if (arg.sym_val == nuclear) {
        *out_kind = MATRIX_NORM_NUCLEAR;
        return true;
    }
    return false;
}

fn double matrix_frobenius_norm(double* values, usz count) {
    if (values == null || count == 0) return 0.0;

    double scale = 0.0;
    double sum_squares = 1.0;
    bool saw_nonzero = false;
    for (usz i = 0; i < count; i++) {
        double abs_value = tensor_c_fabs(values[i]);
        if (!(abs_value > 0.0)) continue;
        saw_nonzero = true;
        if (scale < abs_value) {
            double ratio = scale / abs_value;
            sum_squares = 1.0 + sum_squares * ratio * ratio;
            scale = abs_value;
        } else {
            double ratio = abs_value / scale;
            sum_squares += ratio * ratio;
        }
    }
    if (!saw_nonzero) return 0.0;
    return scale * tensor_c_sqrt(sum_squares);
}

fn double matrix_frobenius_norm_f32(float* values, usz count) {
    if (values == null || count == 0) return 0.0;

    double scale = 0.0;
    double sum_squares = 1.0;
    bool saw_nonzero = false;
    for (usz i = 0; i < count; i++) {
        double abs_value = tensor_c_fabs((double)values[i]);
        if (!(abs_value > 0.0)) continue;
        saw_nonzero = true;
        if (scale < abs_value) {
            double ratio = scale / abs_value;
            sum_squares = 1.0 + sum_squares * ratio * ratio;
            scale = abs_value;
        } else {
            double ratio = abs_value / scale;
            sum_squares += ratio * ratio;
        }
    }
    if (!saw_nonzero) return 0.0;
    return scale * tensor_c_sqrt(sum_squares);
}

fn double matrix_frobenius_norm_complex128(Complex128Val* values, usz count) {
    if (values == null || count == 0) return 0.0;

    double scale = 0.0;
    double sum_squares = 1.0;
    bool saw_nonzero = false;
    for (usz i = 0; i < count; i++) {
        double abs_value = complex128_abs_value(values[i]);
        if (!(abs_value > 0.0)) continue;
        saw_nonzero = true;
        if (scale < abs_value) {
            double ratio = scale / abs_value;
            sum_squares = 1.0 + sum_squares * ratio * ratio;
            scale = abs_value;
        } else {
            double ratio = abs_value / scale;
            sum_squares += ratio * ratio;
        }
    }
    if (!saw_nonzero) return 0.0;
    return scale * tensor_c_sqrt(sum_squares);
}

fn double matrix_frobenius_norm_complex64(Complex64Val* values, usz count) {
    if (values == null || count == 0) return 0.0;

    double scale = 0.0;
    double sum_squares = 1.0;
    bool saw_nonzero = false;
    for (usz i = 0; i < count; i++) {
        double abs_value = (double)complex64_abs_value(values[i]);
        if (!(abs_value > 0.0)) continue;
        saw_nonzero = true;
        if (scale < abs_value) {
            double ratio = scale / abs_value;
            sum_squares = 1.0 + sum_squares * ratio * ratio;
            scale = abs_value;
        } else {
            double ratio = abs_value / scale;
            sum_squares += ratio * ratio;
        }
    }
    if (!saw_nonzero) return 0.0;
    return scale * tensor_c_sqrt(sum_squares);
}

fn bool matrix_norm_kind_requires_singular_values(MatrixNormKind kind) @inline {
    return kind == MATRIX_NORM_SPECTRAL || kind == MATRIX_NORM_NUCLEAR;
}

fn bool matrix_pure_singular_values_f64(TensorVal* matrix, double* s_values, usz k) {
    if (matrix == null || matrix.shape == null || s_values == null) return k == 0;
    if (k == 0) return true;
    if (matrix.data == null) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    bool tall_or_square = rows >= cols;
    usz gram_n = tall_or_square ? cols : rows;

    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    double* gram = null;
    if (!matrix_double_workspace_zero(gram_count, &gram)) return false;
    defer if (gram != null) mem::free(gram);

    double* eigenvectors = null;
    if (!matrix_double_workspace_zero(gram_count, &eigenvectors)) return false;
    defer if (eigenvectors != null) mem::free(eigenvectors);

    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(gram_n, &eigenvalues)) return false;
    defer if (eigenvalues != null) mem::free(eigenvalues);

    if (!matrix_build_svd_gram(matrix, gram, tall_or_square)) return false;
    if (!matrix_symmetric_jacobi_eigen(gram, eigenvalues, eigenvectors, gram_n)) return false;
    matrix_sort_eigen_desc(eigenvalues, eigenvectors, gram_n);

    for (usz i = 0; i < k; i++) {
        double eigenvalue = eigenvalues[i];
        if (eigenvalue < 0.0 && eigenvalue > -1e-10) eigenvalue = 0.0;
        if (eigenvalue < 0.0) eigenvalue = 0.0;
        s_values[i] = tensor_c_sqrt(eigenvalue);
    }
    return true;
}

fn bool matrix_pure_singular_values_f32(TensorVal* matrix, float* s_values, usz k) {
    if (matrix == null || matrix.shape == null || s_values == null) return k == 0;
    if (k == 0) return true;
    if (matrix.data == null) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    bool tall_or_square = rows >= cols;
    usz gram_n = tall_or_square ? cols : rows;

    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    float* gram = null;
    if (!matrix_float32_workspace_zero(gram_count, &gram)) return false;
    defer if (gram != null) mem::free(gram);

    float* eigenvectors = null;
    if (!matrix_float32_workspace_zero(gram_count, &eigenvectors)) return false;
    defer if (eigenvectors != null) mem::free(eigenvectors);

    float* eigenvalues = null;
    if (!matrix_float32_workspace_zero(gram_n, &eigenvalues)) return false;
    defer if (eigenvalues != null) mem::free(eigenvalues);

    if (!matrix_build_svd_gram_f32(matrix, gram, tall_or_square)) return false;
    if (!matrix_symmetric_jacobi_eigen_f32(gram, eigenvalues, eigenvectors, gram_n)) return false;
    matrix_sort_eigen_desc_f32(eigenvalues, eigenvectors, gram_n);

    for (usz i = 0; i < k; i++) {
        double eigenvalue = (double)eigenvalues[i];
        if (eigenvalue < 0.0 && eigenvalue > -1e-5) eigenvalue = 0.0;
        if (eigenvalue < 0.0) eigenvalue = 0.0;
        s_values[i] = (float)tensor_c_sqrt(eigenvalue);
    }
    return true;
}

fn bool matrix_complex128_realified_singular_values(TensorVal* matrix, double* s_values, usz k) {
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_COMPLEX128 || matrix.shape == null || s_values == null) return k == 0;
    if (k == 0) return true;
    if (matrix.data == null) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (rows > usz.max / 2 || cols > usz.max / 2 || k > usz.max / 2) return false;
    usz real_rows = rows * 2;
    usz real_cols = cols * 2;
    usz real_k = k * 2;

    usz real_count = 0;
    if (!tensor_checked_mul_usz(real_rows, real_cols, &real_count)) return false;
    double* realified = null;
    if (!matrix_double_workspace_zero(real_count, &realified)) return false;
    defer if (realified != null) mem::free(realified);

    Complex128Val* source = (Complex128Val*)matrix.data;
    for (usz row = 0; row < rows; row++) {
        for (usz col = 0; col < cols; col++) {
            Complex128Val value = source[row * cols + col];
            usz top = row;
            usz bottom = row + rows;
            usz left = col;
            usz right = col + cols;
            realified[top * real_cols + left] = value.real;
            realified[top * real_cols + right] = -value.imag;
            realified[bottom * real_cols + left] = value.imag;
            realified[bottom * real_cols + right] = value.real;
        }
    }

    double* real_s = null;
    if (!matrix_double_workspace_zero(real_k, &real_s)) return false;
    defer if (real_s != null) mem::free(real_s);

    usz[2] shape_buf = { real_rows, real_cols };
    TensorVal real_matrix;
    real_matrix.dtype = TENSOR_DTYPE_DOUBLE;
    real_matrix.rank = 2;
    real_matrix.shape = &shape_buf[0];
    real_matrix.data = realified;

    if (!matrix_pure_singular_values_f64(&real_matrix, real_s, real_k)) return false;
    for (usz i = 0; i < k; i++) {
        s_values[i] = (real_s[i * 2] + real_s[i * 2 + 1]) * 0.5;
    }
    return true;
}

fn bool matrix_complex64_realified_singular_values(TensorVal* matrix, float* s_values, usz k) {
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_COMPLEX64 || matrix.shape == null || s_values == null) return k == 0;
    if (k == 0) return true;
    if (matrix.data == null) return false;

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (rows > usz.max / 2 || cols > usz.max / 2 || k > usz.max / 2) return false;
    usz real_rows = rows * 2;
    usz real_cols = cols * 2;
    usz real_k = k * 2;

    usz real_count = 0;
    if (!tensor_checked_mul_usz(real_rows, real_cols, &real_count)) return false;
    float* realified = null;
    if (!matrix_float32_workspace_zero(real_count, &realified)) return false;
    defer if (realified != null) mem::free(realified);

    Complex64Val* source = (Complex64Val*)matrix.data;
    for (usz row = 0; row < rows; row++) {
        for (usz col = 0; col < cols; col++) {
            Complex64Val value = source[row * cols + col];
            usz top = row;
            usz bottom = row + rows;
            usz left = col;
            usz right = col + cols;
            realified[top * real_cols + left] = value.real;
            realified[top * real_cols + right] = -value.imag;
            realified[bottom * real_cols + left] = value.imag;
            realified[bottom * real_cols + right] = value.real;
        }
    }

    float* real_s = null;
    if (!matrix_float32_workspace_zero(real_k, &real_s)) return false;
    defer if (real_s != null) mem::free(real_s);

    usz[2] shape_buf = { real_rows, real_cols };
    TensorVal real_matrix;
    real_matrix.dtype = TENSOR_DTYPE_FLOAT32;
    real_matrix.rank = 2;
    real_matrix.shape = &shape_buf[0];
    real_matrix.data = realified;

    if (!matrix_pure_singular_values_f32(&real_matrix, real_s, real_k)) return false;
    for (usz i = 0; i < k; i++) {
        s_values[i] = (real_s[i * 2] + real_s[i * 2 + 1]) * 0.5f;
    }
    return true;
}

fn bool matrix_singular_value_norm(TensorVal* matrix, bool nuclear, double* out_norm) {
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
    usz k = matrix_min_usz(rows, cols);
    if (k == 0) return true;

    usz u_count = 0;
    if (!tensor_checked_mul_usz(rows, k, &u_count)) return false;
    usz v_count = 0;
    if (!tensor_checked_mul_usz(cols, k, &v_count)) return false;

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        double* s = null;
        if (!matrix_double_workspace_zero(k, &s)) return false;
        defer if (s != null) mem::free(s);

        if (!matrix_complex128_realified_singular_values(matrix, s, k)) return false;
        if (!nuclear) {
            *out_norm = s[0];
            return true;
        }

        double sum = 0.0;
        for (usz i = 0; i < k; i++) {
            sum += s[i];
        }
        *out_norm = sum;
        return true;
    }

    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        float* s = null;
        if (!matrix_float32_workspace_zero(k, &s)) return false;
        defer if (s != null) mem::free(s);

        if (!matrix_complex64_realified_singular_values(matrix, s, k)) return false;
        if (!nuclear) {
            *out_norm = (double)s[0];
            return true;
        }

        double sum = 0.0;
        for (usz i = 0; i < k; i++) {
            sum += (double)s[i];
        }
        *out_norm = sum;
        return true;
    }

    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        double* u = null;
        if (!matrix_double_workspace_zero(u_count, &u)) return false;
        defer if (u != null) mem::free(u);

        double* s = null;
        if (!matrix_double_workspace_zero(k, &s)) return false;
        defer if (s != null) mem::free(s);

        double* v = null;
        if (!matrix_double_workspace_zero(v_count, &v)) return false;
        defer if (v != null) mem::free(v);

        if (!matrix_svd_factor(matrix, u, s, v, k)) return false;
        if (!nuclear) {
            *out_norm = s[0];
            return true;
        }

        double sum = 0.0;
        for (usz i = 0; i < k; i++) {
            sum += s[i];
        }
        *out_norm = sum;
        return true;
    }

    float* u = null;
    if (!matrix_float32_workspace_zero(u_count, &u)) return false;
    defer if (u != null) mem::free(u);

    float* s = null;
    if (!matrix_float32_workspace_zero(k, &s)) return false;
    defer if (s != null) mem::free(s);

    float* v = null;
    if (!matrix_float32_workspace_zero(v_count, &v)) return false;
    defer if (v != null) mem::free(v);

    if (!matrix_svd_factor_f32(matrix, u, s, v, k)) return false;
    if (!nuclear) {
        *out_norm = (double)s[0];
        return true;
    }

    double sum = 0.0;
    for (usz i = 0; i < k; i++) {
        sum += (double)s[i];
    }
    *out_norm = sum;
    return true;
}

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
    main::scope_register_dtor(interp.current_scope, (void*)lu_value, &scope_dtor_value);
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
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
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
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
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

fn bool matrix_diagonal_into(TensorVal* source, TensorVal* dest) {
    if (source == null || dest == null || source.shape == null || dest.shape == null) return false;
    if (source.rank != 2 || dest.rank != 1 || source.dtype != dest.dtype) return false;
    usz rows = source.shape[0];
    usz cols = source.shape[1];
    usz diagonal_len = matrix_min_usz(rows, cols);
    if (dest.shape[0] != diagonal_len) return false;
    if (diagonal_len == 0) return true;
    if (source.data == null || dest.data == null) return false;

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src = (double*)source.data;
            double* dst = (double*)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                dst[i] = src[i * cols + i];
            }
            return true;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src = (float*)source.data;
            float* dst = (float*)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                dst[i] = src[i * cols + i];
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src = (Complex128Val*)source.data;
            Complex128Val* dst = (Complex128Val*)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                dst[i] = src[i * cols + i];
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src = (Complex64Val*)source.data;
            Complex64Val* dst = (Complex64Val*)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                dst[i] = src[i * cols + i];
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                usz src_index = i * cols + i;
                if (src[src_index] == null) return false;
                dst[i] = omni_big_integer_clone(src[src_index]);
                if (dst[i] == null) return false;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                usz src_index = i * cols + i;
                if (src[src_index] == null) return false;
                dst[i] = omni_big_float_clone(src[src_index]);
                if (dst[i] == null) return false;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < diagonal_len; i++) {
                usz src_index = i * cols + i;
                if (src[src_index] == null) return false;
                dst[i] = omni_big_complex_clone(src[src_index]);
                if (dst[i] == null) return false;
            }
            return true;
        }
    }
}

fn bool matrix_diagonal_matrix_into(TensorVal* source, TensorVal* dest) {
    if (source == null || dest == null || source.shape == null || dest.shape == null) return false;
    if (source.rank != 1 || dest.rank != 2 || source.dtype != dest.dtype) return false;
    usz n = source.shape[0];
    if (dest.shape[0] != n || dest.shape[1] != n) return false;
    if (n == 0) return true;
    if (source.data == null || dest.data == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src = (double*)source.data;
            double* dst = (double*)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i] = 0.0;
            }
            for (usz i = 0; i < n; i++) {
                dst[i * n + i] = src[i];
            }
            return true;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src = (float*)source.data;
            float* dst = (float*)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i] = 0.0f;
            }
            for (usz i = 0; i < n; i++) {
                dst[i * n + i] = src[i];
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src = (Complex128Val*)source.data;
            Complex128Val* dst = (Complex128Val*)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i].real = 0.0;
                dst[i].imag = 0.0;
            }
            for (usz i = 0; i < n; i++) {
                dst[i * n + i] = src[i];
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src = (Complex64Val*)source.data;
            Complex64Val* dst = (Complex64Val*)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i].real = 0.0f;
                dst[i].imag = 0.0f;
            }
            for (usz i = 0; i < n; i++) {
                dst[i * n + i] = src[i];
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i] = omni_big_integer_from_i64(0);
                if (dst[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                if (src[i] == null) return false;
                void* cloned = omni_big_integer_clone(src[i]);
                if (cloned == null) return false;
                usz index = i * n + i;
                omni_big_integer_free(dst[index]);
                dst[index] = cloned;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i] = omni_big_float_from_i64(0);
                if (dst[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                if (src[i] == null) return false;
                void* cloned = omni_big_float_clone(src[i]);
                if (cloned == null) return false;
                usz index = i * n + i;
                omni_big_float_free(dst[index]);
                dst[index] = cloned;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src = (void**)source.data;
            void** dst = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                dst[i] = omni_big_complex_from_i64(0);
                if (dst[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                if (src[i] == null) return false;
                void* cloned = omni_big_complex_clone(src[i]);
                if (cloned == null) return false;
                usz index = i * n + i;
                omni_big_complex_free(dst[index]);
                dst[index] = cloned;
            }
            return true;
        }
    }
}

fn bool matrix_identity_tensor_into(TensorVal* dest) {
    if (dest == null || dest.shape == null || dest.rank != 2) return false;
    usz n = dest.shape[0];
    if (dest.shape[1] != n) return false;
    if (n == 0) return true;
    if (dest.data == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;

    switch (dest.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* values = (double*)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i] = 0.0;
            }
            for (usz i = 0; i < n; i++) {
                values[i * n + i] = 1.0;
            }
            return true;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* values = (float*)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i] = 0.0f;
            }
            for (usz i = 0; i < n; i++) {
                values[i * n + i] = 1.0f;
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* values = (Complex128Val*)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i].real = 0.0;
                values[i].imag = 0.0;
            }
            for (usz i = 0; i < n; i++) {
                usz index = i * n + i;
                values[index].real = 1.0;
                values[index].imag = 0.0;
            }
            return true;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* values = (Complex64Val*)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i].real = 0.0f;
                values[i].imag = 0.0f;
            }
            for (usz i = 0; i < n; i++) {
                usz index = i * n + i;
                values[index].real = 1.0f;
                values[index].imag = 0.0f;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** values = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i] = omni_big_integer_from_i64(0);
                if (values[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                void* one = omni_big_integer_from_i64(1);
                if (one == null) return false;
                usz index = i * n + i;
                omni_big_integer_free(values[index]);
                values[index] = one;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** values = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i] = omni_big_float_from_i64(0);
                if (values[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                void* one = omni_big_float_from_i64(1);
                if (one == null) return false;
                usz index = i * n + i;
                omni_big_float_free(values[index]);
                values[index] = one;
            }
            return true;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** values = (void**)dest.data;
            for (usz i = 0; i < count; i++) {
                values[i] = omni_big_complex_from_i64(0);
                if (values[i] == null) return false;
            }
            for (usz i = 0; i < n; i++) {
                void* one = omni_big_complex_from_i64(1);
                if (one == null) return false;
                usz index = i * n + i;
                omni_big_complex_free(values[index]);
                values[index] = one;
            }
            return true;
        }
    }
}

fn Value* matrix_trace_scalar_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.shape == null || matrix.rank != 2 || matrix.shape[0] != matrix.shape[1]) {
        return tensor_runtime_raise(interp, "matrix/trace: expected a square rank-2 Tensor");
    }

    usz n = matrix.shape[0];
    switch (matrix.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double sum = 0.0;
            double* values = (double*)matrix.data;
            for (usz i = 0; i < n; i++) {
                sum += values[i * n + i];
            }
            return make_double(interp, sum);
        }
        case TENSOR_DTYPE_FLOAT32: {
            float sum = 0.0f;
            float* values = (float*)matrix.data;
            for (usz i = 0; i < n; i++) {
                sum += values[i * n + i];
            }
            return make_float32(interp, sum);
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val sum;
            sum.real = 0.0;
            sum.imag = 0.0;
            Complex128Val* values = (Complex128Val*)matrix.data;
            for (usz i = 0; i < n; i++) {
                Complex128Val value = values[i * n + i];
                sum.real += value.real;
                sum.imag += value.imag;
            }
            return make_complex128(interp, sum.real, sum.imag);
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val sum;
            sum.real = 0.0f;
            sum.imag = 0.0f;
            Complex64Val* values = (Complex64Val*)matrix.data;
            for (usz i = 0; i < n; i++) {
                Complex64Val value = values[i * n + i];
                sum.real += value.real;
                sum.imag += value.imag;
            }
            return make_complex64(interp, sum.real, sum.imag);
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** values = (void**)matrix.data;
            void* sum = omni_big_integer_from_i64(0);
            if (sum == null) return raise_error(interp, "matrix/trace: failed to allocate BigInteger zero");
            for (usz i = 0; i < n; i++) {
                usz index = i * n + i;
                if (values[index] == null) {
                    omni_big_integer_free(sum);
                    return tensor_runtime_raise(interp, "matrix/trace: tensor backing storage is missing");
                }
                void* next = omni_big_integer_binary(sum, values[index], BIG_INTEGER_OP_ADD);
                omni_big_integer_free(sum);
                if (next == null) return raise_error(interp, "matrix/trace: failed to allocate BigInteger trace");
                sum = next;
            }
            return make_big_integer_from_owned_handle(interp, sum, "matrix/trace: failed to allocate BigInteger trace");
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** values = (void**)matrix.data;
            void* sum = omni_big_float_from_i64(0);
            if (sum == null) return raise_error(interp, "matrix/trace: failed to allocate BigFloat zero");
            for (usz i = 0; i < n; i++) {
                usz index = i * n + i;
                if (values[index] == null) {
                    omni_big_float_free(sum);
                    return tensor_runtime_raise(interp, "matrix/trace: tensor backing storage is missing");
                }
                void* next = omni_big_float_binary(sum, values[index], BIG_FLOAT_OP_ADD);
                omni_big_float_free(sum);
                if (next == null) return raise_error(interp, "matrix/trace: failed to allocate BigFloat trace");
                sum = next;
            }
            return make_big_float_from_owned_handle(interp, sum, "matrix/trace: failed to allocate BigFloat trace");
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** values = (void**)matrix.data;
            void* sum = omni_big_complex_from_i64(0);
            if (sum == null) return raise_error(interp, "matrix/trace: failed to allocate BigComplex zero");
            for (usz i = 0; i < n; i++) {
                usz index = i * n + i;
                if (values[index] == null) {
                    omni_big_complex_free(sum);
                    return tensor_runtime_raise(interp, "matrix/trace: tensor backing storage is missing");
                }
                void* next = omni_big_complex_binary(sum, values[index], BIG_COMPLEX_OP_ADD);
                omni_big_complex_free(sum);
                if (next == null) return raise_error(interp, "matrix/trace: failed to allocate BigComplex trace");
                sum = next;
            }
            return make_big_complex_from_owned_handle(interp, sum, "matrix/trace: failed to allocate BigComplex trace");
        }
    }
}

fn bool matrix_build_svd_gram(TensorVal* matrix, double* gram, bool tall_or_square) {
    if (matrix == null || gram == null || matrix.data == null || matrix.shape == null) return false;
    double* source = (double*)matrix.data;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (tall_or_square) {
        for (usz left = 0; left < cols; left++) {
            for (usz right = left; right < cols; right++) {
                double sum = 0.0;
                for (usz row = 0; row < rows; row++) {
                    sum += source[row * cols + left] * source[row * cols + right];
                }
                gram[left * cols + right] = sum;
                gram[right * cols + left] = sum;
            }
        }
    } else {
        for (usz top = 0; top < rows; top++) {
            for (usz bottom = top; bottom < rows; bottom++) {
                double sum = 0.0;
                for (usz col = 0; col < cols; col++) {
                    sum += source[top * cols + col] * source[bottom * cols + col];
                }
                gram[top * rows + bottom] = sum;
                gram[bottom * rows + top] = sum;
            }
        }
    }
    return true;
}

fn bool matrix_symmetric_jacobi_eigen(double* matrix, double* eigenvalues, double* eigenvectors, usz n) {
    if (n == 0) return true;
    if (matrix == null || eigenvalues == null || eigenvectors == null) return false;
    matrix_identity(eigenvectors, n);

    usz sweep_base = 0;
    if (!tensor_checked_mul_usz(n, n, &sweep_base)) return false;
    if (sweep_base > (usz.max - (usz)1) / (usz)64) return false;
    usz max_sweeps = sweep_base * (usz)64 + (usz)1;
    for (usz sweep = 0; sweep < max_sweeps; sweep++) {
        usz p = 0;
        usz q = n > 1 ? 1 : 0;
        double max_offdiag = 0.0;
        for (usz row = 0; row < n; row++) {
            for (usz col = row + 1; col < n; col++) {
                double offdiag = tensor_c_fabs(matrix[row * n + col]);
                if (offdiag > max_offdiag) {
                    max_offdiag = offdiag;
                    p = row;
                    q = col;
                }
            }
        }
        if (max_offdiag <= 1e-12) {
            for (usz i = 0; i < n; i++) {
                eigenvalues[i] = matrix[i * n + i];
            }
            return true;
        }

        double app = matrix[p * n + p];
        double aqq = matrix[q * n + q];
        double apq = matrix[p * n + q];
        if (apq == 0.0) continue;

        double tau = (aqq - app) / (2.0 * apq);
        double tau_abs = tensor_c_fabs(tau);
        double t = 1.0 / (tau_abs + tensor_c_sqrt(1.0 + tau * tau));
        if (tau < 0.0) t = -t;
        double c = 1.0 / tensor_c_sqrt(1.0 + t * t);
        double s = t * c;

        for (usz k = 0; k < n; k++) {
            if (k == p || k == q) continue;
            double akp = matrix[k * n + p];
            double akq = matrix[k * n + q];
            double new_kp = c * akp - s * akq;
            double new_kq = s * akp + c * akq;
            matrix[k * n + p] = new_kp;
            matrix[p * n + k] = new_kp;
            matrix[k * n + q] = new_kq;
            matrix[q * n + k] = new_kq;
        }

        matrix[p * n + p] = c * c * app - 2.0 * s * c * apq + s * s * aqq;
        matrix[q * n + q] = s * s * app + 2.0 * s * c * apq + c * c * aqq;
        matrix[p * n + q] = 0.0;
        matrix[q * n + p] = 0.0;

        for (usz row = 0; row < n; row++) {
            double vip = eigenvectors[row * n + p];
            double viq = eigenvectors[row * n + q];
            eigenvectors[row * n + p] = c * vip - s * viq;
            eigenvectors[row * n + q] = s * vip + c * viq;
        }
    }

    return false;
}

fn void matrix_swap_eigen_columns(double* values, usz n, usz left, usz right) {
    if (values == null || left == right) return;
    for (usz row = 0; row < n; row++) {
        double tmp = values[row * n + left];
        values[row * n + left] = values[row * n + right];
        values[row * n + right] = tmp;
    }
}

fn void matrix_sort_eigen_desc(double* eigenvalues, double* eigenvectors, usz n) {
    if (eigenvalues == null || eigenvectors == null) return;
    for (usz i = 0; i < n; i++) {
        usz best = i;
        double best_value = eigenvalues[i];
        for (usz j = i + 1; j < n; j++) {
            if (eigenvalues[j] > best_value) {
                best = j;
                best_value = eigenvalues[j];
            }
        }
        if (best != i) {
            double tmp = eigenvalues[i];
            eigenvalues[i] = eigenvalues[best];
            eigenvalues[best] = tmp;
            matrix_swap_eigen_columns(eigenvectors, n, i, best);
        }
    }
}

fn double matrix_clean_near_zero(double value) @inline {
    return tensor_c_fabs(value) < 1e-12 ? 0.0 : value;
}

fn double matrix_complex_magnitude_squared(double real, double imag) @inline {
    return real * real + imag * imag;
}

fn bool matrix_general_eigen_index_precedes(double* real_parts, double* imag_parts, usz left, usz right) {
    double left_magnitude = matrix_complex_magnitude_squared(real_parts[left], imag_parts[left]);
    double right_magnitude = matrix_complex_magnitude_squared(real_parts[right], imag_parts[right]);
    if (left_magnitude > right_magnitude) return true;
    if (left_magnitude < right_magnitude) return false;
    if (real_parts[left] > real_parts[right]) return true;
    if (real_parts[left] < real_parts[right]) return false;
    return imag_parts[left] > imag_parts[right];
}

fn bool matrix_general_eigen_order(double* real_parts, double* imag_parts, usz n, usz** out_order) {
    if (out_order == null) return false;
    *out_order = null;
    if (n == 0) return true;
    if (real_parts == null || imag_parts == null) return false;

    usz order_bytes = 0;
    if (!tensor_checked_mul_usz(n, usz.sizeof, &order_bytes)) return false;
    usz* order = (usz*)mem::malloc(order_bytes);
    if (order == null) return false;
    for (usz i = 0; i < n; i++) {
        order[i] = i;
    }

    for (usz i = 0; i < n; i++) {
        usz best = i;
        for (usz j = i + 1; j < n; j++) {
            if (matrix_general_eigen_index_precedes(real_parts, imag_parts, order[j], order[best])) best = j;
        }
        if (best != i) {
            usz tmp = order[i];
            order[i] = order[best];
            order[best] = tmp;
        }
    }

    *out_order = order;
    return true;
}

fn bool matrix_lapack_general_right_eigenvector_component(
    double* raw_vectors,
    double* imag_parts,
    usz n,
    usz eigen_index,
    usz row,
    double* out_real,
    double* out_imag
) {
    if (raw_vectors == null || imag_parts == null || out_real == null || out_imag == null) return false;
    if (eigen_index >= n || row >= n) return false;
    if (imag_parts[eigen_index] > 0.0 && eigen_index + 1 < n) {
        *out_real = raw_vectors[row * n + eigen_index];
        *out_imag = raw_vectors[row * n + eigen_index + 1];
        return true;
    }
    if (imag_parts[eigen_index] < 0.0 && eigen_index > 0) {
        *out_real = raw_vectors[row * n + eigen_index - 1];
        *out_imag = -raw_vectors[row * n + eigen_index];
        return true;
    }
    *out_real = raw_vectors[row * n + eigen_index];
    *out_imag = 0.0;
    return true;
}

fn bool matrix_general_vectors_from_lapack(
    double* raw_vectors,
    double* imag_parts,
    usz n,
    double* vector_real,
    double* vector_imag
) {
    if (n == 0) return true;
    if (raw_vectors == null || imag_parts == null || vector_real == null || vector_imag == null) return false;
    for (usz col = 0; col < n; col++) {
        for (usz row = 0; row < n; row++) {
            double component_real = 0.0;
            double component_imag = 0.0;
            if (!matrix_lapack_general_right_eigenvector_component(
                    raw_vectors,
                    imag_parts,
                    n,
                    col,
                    row,
                    &component_real,
                    &component_imag
                )) return false;
            vector_real[row * n + col] = component_real;
            vector_imag[row * n + col] = component_imag;
        }
    }
    return true;
}

fn bool matrix_store_big_complex_handle(void** slots, usz index, double real_part, double imag_part) {
    if (slots == null) return false;
    void* handle = omni_big_complex_from_doubles(matrix_clean_near_zero(real_part), matrix_clean_near_zero(imag_part));
    if (handle == null) return false;
    if (slots[index] != null) omni_big_complex_free(slots[index]);
    slots[index] = handle;
    return true;
}

fn bool matrix_fill_general_eigenpairs_from_vectors_big_complex(
    usz n,
    double* real_parts,
    double* imag_parts,
    double* vector_real,
    double* vector_imag,
    TensorVal* values_tensor,
    TensorVal* vectors_tensor
) {
    if (values_tensor == null || vectors_tensor == null) return false;
    if (values_tensor.dtype != TENSOR_DTYPE_BIG_COMPLEX || vectors_tensor.dtype != TENSOR_DTYPE_BIG_COMPLEX) return false;
    if (values_tensor.rank != 1 || vectors_tensor.rank != 2 || values_tensor.shape == null || vectors_tensor.shape == null) return false;
    if (values_tensor.shape[0] != n || vectors_tensor.shape[0] != n || vectors_tensor.shape[1] != n) return false;
    if (!tensor_has_valid_storage(values_tensor) || !tensor_has_valid_storage(vectors_tensor)) return false;

    usz* order = null;
    if (!matrix_general_eigen_order(real_parts, imag_parts, n, &order)) return false;
    defer if (order != null) mem::free(order);

    void** value_slots = (void**)values_tensor.data;
    void** vector_slots = (void**)vectors_tensor.data;
    for (usz out_col = 0; out_col < n; out_col++) {
        usz eigen_index = order[out_col];
        if (!matrix_store_big_complex_handle(value_slots, out_col, real_parts[eigen_index], imag_parts[eigen_index])) return false;

        double pivot_real = 0.0;
        double pivot_imag = 0.0;
        double pivot_magnitude = 0.0;
        for (usz row = 0; row < n; row++) {
            double component_real = vector_real[row * n + eigen_index];
            double component_imag = vector_imag[row * n + eigen_index];
            double component_magnitude = matrix_complex_magnitude_squared(component_real, component_imag);
            if (component_magnitude > pivot_magnitude) {
                pivot_magnitude = component_magnitude;
                pivot_real = component_real;
                pivot_imag = component_imag;
            }
        }

        double rotation_real = 1.0;
        double rotation_imag = 0.0;
        if (pivot_magnitude > 0.0) {
            double pivot_abs = tensor_c_sqrt(pivot_magnitude);
            rotation_real = pivot_real / pivot_abs;
            rotation_imag = -pivot_imag / pivot_abs;
        }

        for (usz row = 0; row < n; row++) {
            double component_real = vector_real[row * n + eigen_index];
            double component_imag = vector_imag[row * n + eigen_index];
            double normalized_real = component_real * rotation_real - component_imag * rotation_imag;
            double normalized_imag = component_real * rotation_imag + component_imag * rotation_real;
            if (!matrix_store_big_complex_handle(vector_slots, row * n + out_col, normalized_real, normalized_imag)) return false;
        }
    }

    return true;
}

fn bool matrix_fill_general_eigenpairs_big_complex(
    usz n,
    double* real_parts,
    double* imag_parts,
    double* raw_vectors,
    TensorVal* values_tensor,
    TensorVal* vectors_tensor
) {
    usz vector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &vector_count)) return false;

    double* vector_real = null;
    if (!matrix_double_workspace_zero(vector_count, &vector_real)) return false;
    defer if (vector_real != null) mem::free(vector_real);

    double* vector_imag = null;
    if (!matrix_double_workspace_zero(vector_count, &vector_imag)) return false;
    defer if (vector_imag != null) mem::free(vector_imag);

    if (!matrix_general_vectors_from_lapack(raw_vectors, imag_parts, n, vector_real, vector_imag)) return false;
    return matrix_fill_general_eigenpairs_from_vectors_big_complex(
        n,
        real_parts,
        imag_parts,
        vector_real,
        vector_imag,
        values_tensor,
        vectors_tensor
    );
}

fn void matrix_square_multiply(double* left, double* right, double* out, usz n) {
    if (left == null || right == null || out == null) return;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            double sum = 0.0;
            for (usz k = 0; k < n; k++) {
                sum += left[row * n + k] * right[k * n + col];
            }
            out[row * n + col] = sum;
        }
    }
}

fn bool matrix_square_qr_decompose(double* input, double* q, double* r, usz n) {
    if (n == 0) return true;
    if (input == null || q == null || r == null) return false;

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    for (usz i = 0; i < count; i++) {
        q[i] = input[i];
        r[i] = 0.0;
    }

    for (usz col = 0; col < n; col++) {
        for (usz prev = 0; prev < col; prev++) {
            double dot = 0.0;
            for (usz row = 0; row < n; row++) {
                dot += q[row * n + prev] * q[row * n + col];
            }
            r[prev * n + col] = dot;
            for (usz row = 0; row < n; row++) {
                q[row * n + col] -= dot * q[row * n + prev];
            }
        }

        double norm_squared = 0.0;
        for (usz row = 0; row < n; row++) {
            double value = q[row * n + col];
            norm_squared += value * value;
        }
        if (!(norm_squared > 1e-24)) {
            if (!matrix_fill_orthonormal_completion_column(q, n, n, col)) return false;
            norm_squared = 1.0;
        }
        double norm = tensor_c_sqrt(norm_squared);
        if (!(norm > 0.0)) return false;
        r[col * n + col] = norm;
        for (usz row = 0; row < n; row++) {
            q[row * n + col] /= norm;
        }
    }
    return true;
}

fn bool matrix_general_qr_eigenvalues(TensorVal* matrix, double* real_parts, double* imag_parts) {
    if (matrix == null || matrix.shape == null || real_parts == null || imag_parts == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (n == 1) {
        double* source = (double*)matrix.data;
        if (source == null) return false;
        real_parts[0] = source[0];
        imag_parts[0] = 0.0;
        return true;
    }

    double* work = null;
    if (!matrix_copy_float64_workspace(matrix, &work)) return false;
    defer if (work != null) mem::free(work);

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;

    double* shifted = null;
    if (!matrix_double_workspace_zero(count, &shifted)) return false;
    defer if (shifted != null) mem::free(shifted);

    double* q = null;
    if (!matrix_double_workspace_zero(count, &q)) return false;
    defer if (q != null) mem::free(q);

    double* r = null;
    if (!matrix_double_workspace_zero(count, &r)) return false;
    defer if (r != null) mem::free(r);

    double* next = null;
    if (!matrix_double_workspace_zero(count, &next)) return false;
    defer if (next != null) mem::free(next);

    usz max_iter = n * 2048;
    for (usz iter = 0; iter < max_iter; iter++) {
        bool converged = true;
        usz scan = 1;
        while (scan < n) {
            usz i = scan;
            double scale = tensor_c_fabs(work[(i - 1) * n + (i - 1)]) + tensor_c_fabs(work[i * n + i]) + 1.0;
            if (tensor_c_fabs(work[i * n + i - 1]) <= 1e-12 * scale) {
                work[i * n + i - 1] = 0.0;
                scan += 1;
                continue;
            }

            bool isolated_block = true;
            if (i > 1) {
                double prev_scale = tensor_c_fabs(work[(i - 2) * n + (i - 2)]) + tensor_c_fabs(work[(i - 1) * n + (i - 1)]) + 1.0;
                isolated_block = tensor_c_fabs(work[(i - 1) * n + i - 2]) <= 1e-8 * prev_scale;
            }
            if (isolated_block && i + 1 < n) {
                double next_scale = tensor_c_fabs(work[i * n + i]) + tensor_c_fabs(work[(i + 1) * n + (i + 1)]) + 1.0;
                isolated_block = tensor_c_fabs(work[(i + 1) * n + i]) <= 1e-8 * next_scale;
            }
            if (isolated_block) {
                scan += 2;
            } else {
                converged = false;
                break;
            }
        }
        if (converged) break;

        double shift = work[(n - 1) * n + (n - 1)];
        for (usz i = 0; i < count; i++) shifted[i] = work[i];
        for (usz i = 0; i < n; i++) shifted[i * n + i] -= shift;
        if (!matrix_square_qr_decompose(shifted, q, r, n)) return false;
        matrix_square_multiply(r, q, next, n);
        for (usz i = 0; i < count; i++) work[i] = next[i];
        for (usz i = 0; i < n; i++) work[i * n + i] += shift;
    }

    usz cursor = n;
    while (cursor > 0) {
        usz i = cursor - 1;
        if (i > 0) {
            double scale = tensor_c_fabs(work[(i - 1) * n + (i - 1)]) + tensor_c_fabs(work[i * n + i]) + 1.0;
            if (tensor_c_fabs(work[i * n + i - 1]) > 1e-8 * scale) {
                double a = work[(i - 1) * n + (i - 1)];
                double b = work[(i - 1) * n + i];
                double c = work[i * n + i - 1];
                double d = work[i * n + i];
                double trace = a + d;
                double det = a * d - b * c;
                double disc = trace * trace - 4.0 * det;
                if (disc >= 0.0) {
                    double root = tensor_c_sqrt(disc);
                    real_parts[i - 1] = (trace + root) * 0.5;
                    real_parts[i] = (trace - root) * 0.5;
                    imag_parts[i - 1] = 0.0;
                    imag_parts[i] = 0.0;
                } else {
                    real_parts[i - 1] = trace * 0.5;
                    real_parts[i] = trace * 0.5;
                    double imag = tensor_c_sqrt(-disc) * 0.5;
                    imag_parts[i - 1] = imag;
                    imag_parts[i] = -imag;
                }
                cursor -= 2;
                continue;
            }
        }
        real_parts[i] = work[i * n + i];
        imag_parts[i] = 0.0;
        cursor -= 1;
    }

    return true;
}

fn void matrix_complex_div(
    double ar,
    double ai,
    double br,
    double bi,
    double* out_real,
    double* out_imag
) {
    double denom = br * br + bi * bi;
    if (!(denom > 0.0)) {
        *out_real = 0.0;
        *out_imag = 0.0;
        return;
    }
    *out_real = (ar * br + ai * bi) / denom;
    *out_imag = (ai * br - ar * bi) / denom;
}

fn void matrix_complex_row_swap(double* real_values, double* imag_values, usz n, usz left, usz right) {
    if (real_values == null || imag_values == null || left == right) return;
    for (usz col = 0; col < n; col++) {
        usz left_index = left * n + col;
        usz right_index = right * n + col;
        double tmp_real = real_values[left_index];
        double tmp_imag = imag_values[left_index];
        real_values[left_index] = real_values[right_index];
        imag_values[left_index] = imag_values[right_index];
        real_values[right_index] = tmp_real;
        imag_values[right_index] = tmp_imag;
    }
}

fn bool matrix_general_complex_null_vector(
    TensorVal* matrix,
    double lambda_real,
    double lambda_imag,
    double* out_real,
    double* out_imag
) {
    if (matrix == null || matrix.shape == null || out_real == null || out_imag == null) return false;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (n == 1) {
        out_real[0] = 1.0;
        out_imag[0] = 0.0;
        return true;
    }

    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;

    double* real_values = null;
    if (!matrix_double_workspace_zero(count, &real_values)) return false;
    defer if (real_values != null) mem::free(real_values);

    double* imag_values = null;
    if (!matrix_double_workspace_zero(count, &imag_values)) return false;
    defer if (imag_values != null) mem::free(imag_values);

    double* source = (double*)matrix.data;
    if (source == null) return false;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            usz index = row * n + col;
            real_values[index] = source[index];
            imag_values[index] = 0.0;
        }
        real_values[row * n + row] -= lambda_real;
        imag_values[row * n + row] -= lambda_imag;
    }

    usz pivot_bytes = 0;
    if (!tensor_checked_mul_usz(n, usz.sizeof, &pivot_bytes)) return false;
    usz* pivot_cols = (usz*)mem::malloc(pivot_bytes);
    if (pivot_cols == null) return false;
    defer mem::free(pivot_cols);

    usz rank = 0;
    double tolerance = 1e-8;
    for (usz col = 0; col < n && rank < n; col++) {
        usz pivot_row = rank;
        double pivot_abs = 0.0;
        for (usz row = rank; row < n; row++) {
            double candidate_abs = matrix_complex_magnitude_squared(real_values[row * n + col], imag_values[row * n + col]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (!(pivot_abs > tolerance * tolerance)) continue;

        matrix_complex_row_swap(real_values, imag_values, n, rank, pivot_row);
        double pivot_real = real_values[rank * n + col];
        double pivot_imag = imag_values[rank * n + col];
        for (usz c = col; c < n; c++) {
            double normalized_real = 0.0;
            double normalized_imag = 0.0;
            matrix_complex_div(real_values[rank * n + c], imag_values[rank * n + c], pivot_real, pivot_imag, &normalized_real, &normalized_imag);
            real_values[rank * n + c] = normalized_real;
            imag_values[rank * n + c] = normalized_imag;
        }

        for (usz row = 0; row < n; row++) {
            if (row == rank) continue;
            double factor_real = real_values[row * n + col];
            double factor_imag = imag_values[row * n + col];
            if (!(matrix_complex_magnitude_squared(factor_real, factor_imag) > tolerance * tolerance)) continue;
            for (usz c = col; c < n; c++) {
                double pivot_entry_real = real_values[rank * n + c];
                double pivot_entry_imag = imag_values[rank * n + c];
                double product_real = factor_real * pivot_entry_real - factor_imag * pivot_entry_imag;
                double product_imag = factor_real * pivot_entry_imag + factor_imag * pivot_entry_real;
                real_values[row * n + c] -= product_real;
                imag_values[row * n + c] -= product_imag;
            }
        }

        pivot_cols[rank] = col;
        rank += 1;
    }

    for (usz i = 0; i < n; i++) {
        out_real[i] = 0.0;
        out_imag[i] = 0.0;
    }

    usz free_col = n - 1;
    bool found_free = false;
    for (usz col = n; col > 0; col--) {
        usz candidate = col - 1;
        bool is_pivot = false;
        for (usz row = 0; row < rank; row++) {
            if (pivot_cols[row] == candidate) {
                is_pivot = true;
                break;
            }
        }
        if (!is_pivot) {
            free_col = candidate;
            found_free = true;
            break;
        }
    }
    if (!found_free && rank > 0) {
        rank -= 1;
        free_col = pivot_cols[rank];
    }

    out_real[free_col] = 1.0;
    out_imag[free_col] = 0.0;
    while (rank > 0) {
        rank -= 1;
        usz pivot_col = pivot_cols[rank];
        double sum_real = 0.0;
        double sum_imag = 0.0;
        for (usz col = pivot_col + 1; col < n; col++) {
            double ar = real_values[rank * n + col];
            double ai = imag_values[rank * n + col];
            double xr = out_real[col];
            double xi = out_imag[col];
            sum_real += ar * xr - ai * xi;
            sum_imag += ar * xi + ai * xr;
        }
        out_real[pivot_col] = -sum_real;
        out_imag[pivot_col] = -sum_imag;
    }

    double norm_squared = 0.0;
    for (usz i = 0; i < n; i++) {
        norm_squared += matrix_complex_magnitude_squared(out_real[i], out_imag[i]);
    }
    if (!(norm_squared > 0.0)) {
        out_real[0] = 1.0;
        out_imag[0] = 0.0;
        return true;
    }
    double inv_norm = 1.0 / tensor_c_sqrt(norm_squared);
    for (usz i = 0; i < n; i++) {
        out_real[i] *= inv_norm;
        out_imag[i] *= inv_norm;
    }
    return true;
}

fn bool matrix_general_eigenpairs_pure(
    TensorVal* matrix,
    double* real_parts,
    double* imag_parts,
    double* vector_real,
    double* vector_imag
) {
    if (matrix == null || matrix.shape == null || real_parts == null || imag_parts == null || vector_real == null || vector_imag == null) return false;
    usz n = matrix.shape[0];
    if (!matrix_general_qr_eigenvalues(matrix, real_parts, imag_parts)) return false;

    double* column_real = null;
    if (!matrix_double_workspace_zero(n, &column_real)) return false;
    defer if (column_real != null) mem::free(column_real);

    double* column_imag = null;
    if (!matrix_double_workspace_zero(n, &column_imag)) return false;
    defer if (column_imag != null) mem::free(column_imag);

    for (usz col = 0; col < n; col++) {
        if (!matrix_general_complex_null_vector(
                matrix,
                real_parts[col],
                imag_parts[col],
                column_real,
                column_imag
            )) return false;
        for (usz row = 0; row < n; row++) {
            vector_real[row * n + col] = column_real[row];
            vector_imag[row * n + col] = column_imag[row];
        }
    }
    return true;
}

fn void matrix_orthogonalize_column(double* values, usz rows, usz cols, usz col) {
    if (values == null) return;
    for (usz prev = 0; prev < col; prev++) {
        double dot = 0.0;
        for (usz row = 0; row < rows; row++) {
            dot += values[row * cols + col] * values[row * cols + prev];
        }
        for (usz row = 0; row < rows; row++) {
            values[row * cols + col] -= dot * values[row * cols + prev];
        }
    }
}

fn bool matrix_normalize_column(double* values, usz rows, usz cols, usz col) {
    if (values == null) return false;
    double norm_squared = 0.0;
    for (usz row = 0; row < rows; row++) {
        double value = values[row * cols + col];
        norm_squared += value * value;
    }
    if (!(norm_squared > 1e-24)) return false;
    double inv_norm = 1.0 / tensor_c_sqrt(norm_squared);
    for (usz row = 0; row < rows; row++) {
        values[row * cols + col] *= inv_norm;
    }
    return true;
}

fn bool matrix_fill_orthonormal_completion_column(double* values, usz rows, usz cols, usz col) {
    if (values == null || col >= cols || col >= rows) return false;
    for (usz candidate = 0; candidate < rows; candidate++) {
        for (usz row = 0; row < rows; row++) {
            values[row * cols + col] = row == candidate ? 1.0 : 0.0;
        }
        matrix_orthogonalize_column(values, rows, cols, col);
        if (matrix_normalize_column(values, rows, cols, col)) return true;
    }
    return false;
}

fn void matrix_copy_eigenvectors_to_reduced(double* dst, double* src, usz rows, usz cols) {
    if (dst == null || src == null) return;
    for (usz row = 0; row < rows; row++) {
        for (usz col = 0; col < cols; col++) {
            dst[row * cols + col] = src[row * rows + col];
        }
    }
}

fn bool matrix_build_svd_gram_f32(TensorVal* matrix, float* gram, bool tall_or_square) {
    if (matrix == null || gram == null || matrix.data == null || matrix.shape == null) return false;
    float* source = (float*)matrix.data;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (tall_or_square) {
        for (usz left = 0; left < cols; left++) {
            for (usz right = left; right < cols; right++) {
                float sum = 0.0f;
                for (usz row = 0; row < rows; row++) {
                    sum += source[row * cols + left] * source[row * cols + right];
                }
                gram[left * cols + right] = sum;
                gram[right * cols + left] = sum;
            }
        }
    } else {
        for (usz top = 0; top < rows; top++) {
            for (usz bottom = top; bottom < rows; bottom++) {
                float sum = 0.0f;
                for (usz col = 0; col < cols; col++) {
                    sum += source[top * cols + col] * source[bottom * cols + col];
                }
                gram[top * rows + bottom] = sum;
                gram[bottom * rows + top] = sum;
            }
        }
    }
    return true;
}

fn void matrix_orthogonalize_column_f32(float* values, usz rows, usz cols, usz col) {
    if (values == null) return;
    for (usz prev = 0; prev < col; prev++) {
        float dot = 0.0f;
        for (usz row = 0; row < rows; row++) {
            dot += values[row * cols + col] * values[row * cols + prev];
        }
        for (usz row = 0; row < rows; row++) {
            values[row * cols + col] -= dot * values[row * cols + prev];
        }
    }
}

fn bool matrix_normalize_column_f32(float* values, usz rows, usz cols, usz col) {
    if (values == null) return false;
    double norm_squared = 0.0;
    for (usz row = 0; row < rows; row++) {
        double value = (double)values[row * cols + col];
        norm_squared += value * value;
    }
    if (!(norm_squared > 1e-12)) return false;
    float inv_norm = (float)(1.0 / tensor_c_sqrt(norm_squared));
    for (usz row = 0; row < rows; row++) {
        values[row * cols + col] *= inv_norm;
    }
    return true;
}

fn bool matrix_fill_orthonormal_completion_column_f32(float* values, usz rows, usz cols, usz col) {
    if (values == null || col >= cols || col >= rows) return false;
    for (usz candidate = 0; candidate < rows; candidate++) {
        for (usz row = 0; row < rows; row++) {
            values[row * cols + col] = row == candidate ? 1.0f : 0.0f;
        }
        matrix_orthogonalize_column_f32(values, rows, cols, col);
        if (matrix_normalize_column_f32(values, rows, cols, col)) return true;
    }
    return false;
}

fn void matrix_swap_eigen_columns_f32(float* values, usz n, usz left, usz right) {
    if (values == null || left == right) return;
    for (usz row = 0; row < n; row++) {
        float tmp = values[row * n + left];
        values[row * n + left] = values[row * n + right];
        values[row * n + right] = tmp;
    }
}

fn void matrix_sort_eigen_desc_f32(float* eigenvalues, float* eigenvectors, usz n) {
    if (eigenvalues == null || eigenvectors == null) return;
    for (usz i = 0; i < n; i++) {
        usz best = i;
        float best_value = eigenvalues[i];
        for (usz j = i + 1; j < n; j++) {
            if (eigenvalues[j] > best_value) {
                best = j;
                best_value = eigenvalues[j];
            }
        }
        if (best != i) {
            float tmp = eigenvalues[i];
            eigenvalues[i] = eigenvalues[best];
            eigenvalues[best] = tmp;
            matrix_swap_eigen_columns_f32(eigenvectors, n, i, best);
        }
    }
}

fn bool matrix_symmetric_jacobi_eigen_f32(float* matrix, float* eigenvalues, float* eigenvectors, usz n) {
    if (n == 0) return true;
    if (matrix == null || eigenvalues == null || eigenvectors == null) return false;
    matrix_identity_f32(eigenvectors, n);

    usz sweep_base = 0;
    if (!tensor_checked_mul_usz(n, n, &sweep_base)) return false;
    if (sweep_base > (usz.max - (usz)1) / (usz)64) return false;
    usz max_sweeps = sweep_base * (usz)64 + (usz)1;
    for (usz sweep = 0; sweep < max_sweeps; sweep++) {
        usz p = 0;
        usz q = n > 1 ? 1 : 0;
        double max_offdiag = 0.0;
        double max_diag = 0.0;
        for (usz row = 0; row < n; row++) {
            double diag = tensor_c_fabs((double)matrix[row * n + row]);
            if (diag > max_diag) max_diag = diag;
            for (usz col = row + 1; col < n; col++) {
                double offdiag = tensor_c_fabs((double)matrix[row * n + col]);
                if (offdiag > max_offdiag) {
                    max_offdiag = offdiag;
                    p = row;
                    q = col;
                }
            }
        }
        if (max_offdiag <= 1e-6 * (max_diag + 1.0)) {
            for (usz i = 0; i < n; i++) {
                eigenvalues[i] = matrix[i * n + i];
            }
            return true;
        }

        double app = (double)matrix[p * n + p];
        double aqq = (double)matrix[q * n + q];
        double apq = (double)matrix[p * n + q];
        if (apq == 0.0) continue;

        double tau = (aqq - app) / (2.0 * apq);
        double tau_abs = tensor_c_fabs(tau);
        double t = 1.0 / (tau_abs + tensor_c_sqrt(1.0 + tau * tau));
        if (tau < 0.0) t = -t;
        double c = 1.0 / tensor_c_sqrt(1.0 + t * t);
        double s = t * c;

        for (usz k = 0; k < n; k++) {
            if (k == p || k == q) continue;
            double akp = (double)matrix[k * n + p];
            double akq = (double)matrix[k * n + q];
            float new_kp = (float)(c * akp - s * akq);
            float new_kq = (float)(s * akp + c * akq);
            matrix[k * n + p] = new_kp;
            matrix[p * n + k] = new_kp;
            matrix[k * n + q] = new_kq;
            matrix[q * n + k] = new_kq;
        }

        matrix[p * n + p] = (float)(c * c * app - 2.0 * s * c * apq + s * s * aqq);
        matrix[q * n + q] = (float)(s * s * app + 2.0 * s * c * apq + c * c * aqq);
        matrix[p * n + q] = 0.0f;
        matrix[q * n + p] = 0.0f;

        for (usz row = 0; row < n; row++) {
            double vip = (double)eigenvectors[row * n + p];
            double viq = (double)eigenvectors[row * n + q];
            eigenvectors[row * n + p] = (float)(c * vip - s * viq);
            eigenvectors[row * n + q] = (float)(s * vip + c * viq);
        }
    }

    return false;
}

fn void matrix_copy_eigenvectors_to_reduced_f32(float* dst, float* src, usz rows, usz cols) {
    if (dst == null || src == null) return;
    for (usz row = 0; row < rows; row++) {
        for (usz col = 0; col < cols; col++) {
            dst[row * cols + col] = src[row * rows + col];
        }
    }
}

fn bool matrix_svd_factor_f32(TensorVal* matrix, float* u, float* s_values, float* v, usz k) {
    if (matrix == null || matrix.shape == null || u == null || s_values == null || v == null) return k == 0;
    float* source = (float*)matrix.data;
    if (source == null) return k == 0;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    bool tall_or_square = rows >= cols;
    usz gram_n = tall_or_square ? cols : rows;

    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    float* gram = null;
    if (!matrix_float32_workspace_zero(gram_count, &gram)) return false;
    defer if (gram != null) mem::free(gram);

    float* eigenvectors = null;
    if (!matrix_float32_workspace_zero(gram_count, &eigenvectors)) return false;
    defer if (eigenvectors != null) mem::free(eigenvectors);

    float* eigenvalues = null;
    if (!matrix_float32_workspace_zero(gram_n, &eigenvalues)) return false;
    defer if (eigenvalues != null) mem::free(eigenvalues);

    if (!matrix_build_svd_gram_f32(matrix, gram, tall_or_square)) return false;
    if (!matrix_symmetric_jacobi_eigen_f32(gram, eigenvalues, eigenvectors, gram_n)) return false;
    matrix_sort_eigen_desc_f32(eigenvalues, eigenvectors, gram_n);

    float max_singular = 0.0f;
    for (usz i = 0; i < k; i++) {
        double eigenvalue = (double)eigenvalues[i];
        if (eigenvalue < 0.0 && eigenvalue > -1e-5) eigenvalue = 0.0;
        if (eigenvalue < 0.0) eigenvalue = 0.0;
        s_values[i] = (float)tensor_c_sqrt(eigenvalue);
        if (s_values[i] > max_singular) max_singular = s_values[i];
    }
    double singular_tolerance = 1e-6 * ((double)max_singular + 1.0);

    if (tall_or_square) {
        matrix_copy_eigenvectors_to_reduced_f32(v, eigenvectors, cols, k);
        for (usz col = 0; col < k; col++) {
            if ((double)s_values[col] > singular_tolerance) {
                for (usz row = 0; row < rows; row++) {
                    float sum = 0.0f;
                    for (usz source_col = 0; source_col < cols; source_col++) {
                        sum += source[row * cols + source_col] * v[source_col * k + col];
                    }
                    u[row * k + col] = sum / s_values[col];
                }
                matrix_orthogonalize_column_f32(u, rows, k, col);
                if (!matrix_normalize_column_f32(u, rows, k, col)) {
                    if (!matrix_fill_orthonormal_completion_column_f32(u, rows, k, col)) return false;
                }
            } else if (!matrix_fill_orthonormal_completion_column_f32(u, rows, k, col)) {
                return false;
            }
        }
    } else {
        matrix_copy_eigenvectors_to_reduced_f32(u, eigenvectors, rows, k);
        for (usz col = 0; col < k; col++) {
            if ((double)s_values[col] > singular_tolerance) {
                for (usz source_col = 0; source_col < cols; source_col++) {
                    float sum = 0.0f;
                    for (usz row = 0; row < rows; row++) {
                        sum += source[row * cols + source_col] * u[row * k + col];
                    }
                    v[source_col * k + col] = sum / s_values[col];
                }
                matrix_orthogonalize_column_f32(v, cols, k, col);
                if (!matrix_normalize_column_f32(v, cols, k, col)) {
                    if (!matrix_fill_orthonormal_completion_column_f32(v, cols, k, col)) return false;
                }
            } else if (!matrix_fill_orthonormal_completion_column_f32(v, cols, k, col)) {
                return false;
            }
        }
    }

    return true;
}

fn bool matrix_svd_factor(TensorVal* matrix, double* u, double* s_values, double* v, usz k) {
    if (matrix == null || matrix.shape == null || u == null || s_values == null || v == null) return k == 0;
    double* source = (double*)matrix.data;
    if (source == null) return k == 0;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    if (k > 0) {
        double* work = null;
        if (!matrix_copy_float64_workspace(matrix, &work)) return false;
        defer if (work != null) mem::free(work);

        int lapack_status = omni_tensor_backend_lapack_dgesvd(rows, cols, work, u, s_values, v);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                return true;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                /* Fall through to the pure Gram/Jacobi SVD below. */
                break;
            case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                return false;
            default:
                return false;
        }
    }

    bool tall_or_square = rows >= cols;
    usz gram_n = tall_or_square ? cols : rows;

    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    double* gram = null;
    if (!matrix_double_workspace_zero(gram_count, &gram)) return false;
    defer if (gram != null) mem::free(gram);

    double* eigenvectors = null;
    if (!matrix_double_workspace_zero(gram_count, &eigenvectors)) return false;
    defer if (eigenvectors != null) mem::free(eigenvectors);

    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(gram_n, &eigenvalues)) return false;
    defer if (eigenvalues != null) mem::free(eigenvalues);

    if (!matrix_build_svd_gram(matrix, gram, tall_or_square)) return false;
    if (!matrix_symmetric_jacobi_eigen(gram, eigenvalues, eigenvectors, gram_n)) return false;
    matrix_sort_eigen_desc(eigenvalues, eigenvectors, gram_n);

    for (usz i = 0; i < k; i++) {
        double eigenvalue = eigenvalues[i];
        if (eigenvalue < 0.0 && eigenvalue > -1e-10) eigenvalue = 0.0;
        if (eigenvalue < 0.0) eigenvalue = 0.0;
        s_values[i] = tensor_c_sqrt(eigenvalue);
    }

    if (tall_or_square) {
        matrix_copy_eigenvectors_to_reduced(v, eigenvectors, cols, k);
        for (usz col = 0; col < k; col++) {
            if (s_values[col] > 1e-12) {
                for (usz row = 0; row < rows; row++) {
                    double sum = 0.0;
                    for (usz source_col = 0; source_col < cols; source_col++) {
                        sum += source[row * cols + source_col] * v[source_col * k + col];
                    }
                    u[row * k + col] = sum / s_values[col];
                }
                matrix_orthogonalize_column(u, rows, k, col);
                if (!matrix_normalize_column(u, rows, k, col)) {
                    if (!matrix_fill_orthonormal_completion_column(u, rows, k, col)) return false;
                }
            } else if (!matrix_fill_orthonormal_completion_column(u, rows, k, col)) {
                return false;
            }
        }
    } else {
        matrix_copy_eigenvectors_to_reduced(u, eigenvectors, rows, k);
        for (usz col = 0; col < k; col++) {
            if (s_values[col] > 1e-12) {
                for (usz source_col = 0; source_col < cols; source_col++) {
                    double sum = 0.0;
                    for (usz row = 0; row < rows; row++) {
                        sum += source[row * cols + source_col] * u[row * k + col];
                    }
                    v[source_col * k + col] = sum / s_values[col];
                }
                matrix_orthogonalize_column(v, cols, k, col);
                if (!matrix_normalize_column(v, cols, k, col)) {
                    if (!matrix_fill_orthonormal_completion_column(v, cols, k, col)) return false;
                }
            } else if (!matrix_fill_orthonormal_completion_column(v, cols, k, col)) {
                return false;
            }
        }
    }

    return true;
}

fn bool matrix_is_exact_symmetric(TensorVal* matrix) {
    if (matrix == null || matrix.shape == null || matrix.data == null) return false;
    usz n = matrix.shape[0];
    double* source = (double*)matrix.data;
    for (usz row = 0; row < n; row++) {
        for (usz col = row + 1; col < n; col++) {
            if (source[row * n + col] != source[col * n + row]) return false;
        }
    }
    return true;
}

fn bool matrix_symmetric_eigen_factor(TensorVal* matrix, double* eigenvalues, double* eigenvectors) {
    if (matrix == null || matrix.shape == null || eigenvalues == null || eigenvectors == null) return matrix != null && matrix.shape != null && matrix.shape[0] == 0;
    usz n = matrix.shape[0];
    if (n == 0) return true;
    if (!matrix_is_exact_symmetric(matrix)) return false;

    double* work = null;
    if (!matrix_copy_float64_workspace(matrix, &work)) return false;
    defer if (work != null) mem::free(work);

    bool factored = false;
    int lapack_status = omni_tensor_backend_lapack_dsyev(n, work, eigenvalues);
    switch (lapack_status) {
        case OMNI_TENSOR_LAPACK_SUCCESS:
            if (matrix.byte_len > 0) mem::copy(eigenvectors, work, matrix.byte_len);
            factored = true;
        case OMNI_TENSOR_LAPACK_UNAVAILABLE:
            factored = false;
        case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
            return false;
        default:
            return false;
    }

    if (!factored) {
        if (!matrix_symmetric_jacobi_eigen(work, eigenvalues, eigenvectors, n)) return false;
        matrix_sort_eigen_desc(eigenvalues, eigenvectors, n);
    }
    for (usz i = 0; i < n; i++) {
        if (tensor_c_fabs(eigenvalues[i]) < 1e-12) eigenvalues[i] = 0.0;
    }
    return true;
}

fn bool matrix_copy_float64_workspace(TensorVal* matrix, double** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null) return false;
    if (matrix.byte_len == 0) return true;

    double* work = (double*)mem::malloc(matrix.byte_len);
    if (work == null) {
        return false;
    }
    mem::copy(work, matrix.data, matrix.byte_len);
    *out_work = work;
    return true;
}

fn bool matrix_copy_float32_workspace(TensorVal* matrix, float** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_FLOAT32) return false;
    if (matrix.byte_len == 0) return true;

    float* work = (float*)mem::malloc(matrix.byte_len);
    if (work == null) {
        return false;
    }
    mem::copy(work, matrix.data, matrix.byte_len);
    *out_work = work;
    return true;
}

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

fn Value* matrix_transpose_view_value(Value* source_value, TensorVal* matrix, Interp* interp, char[] label) {
    if (source_value == null || matrix == null || !tensor_has_valid_metadata(matrix)) {
        return tensor_runtime_raise(interp, label);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.strides == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/transpose-view: expected a rank-2 Tensor", null);
    }
    if (matrix.device == TENSOR_DEVICE_CPU) {
        if (matrix.byte_len > 0 && matrix.data == null) {
            return tensor_runtime_raise(interp, "matrix/transpose-view: tensor backing storage is missing");
        }
    } else if (matrix.device == TENSOR_DEVICE_VULKAN) {
        if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: Vulkan source must be dense row-major concrete storage", null);
        }
        if (!tensor_dtype_is_vulkan_storage(matrix.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
    } else {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: currently supports CPU or Vulkan Tensor storage", null);
    }

    usz[2] result_shape = { matrix.shape[1], matrix.shape[0] };
    TensorVal* tensor = tensor_alloc_payload_base_checked(matrix.dtype, result_shape[0:2], false, false, "matrix/transpose-view: failed to allocate Tensor view metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "matrix/transpose-view: failed to allocate Tensor view metadata");
    tensor.kind = TENSOR_PAYLOAD_VIEW;
    tensor.device = matrix.device;
    tensor.mutable = false;
    tensor.is_view = true;
    tensor.storage_offset = matrix.storage_offset;
    tensor.storage_element_count = matrix.storage_element_count;
    tensor.storage_byte_len = matrix.storage_byte_len;
    tensor.data = matrix.device == TENSOR_DEVICE_CPU ? matrix.data : null;
    tensor.device_handle = matrix.device == TENSOR_DEVICE_VULKAN ? matrix.device_handle : null;
    tensor.device_finalizer = null;
    tensor.view_source = source_value;
    tensor.strides[0] = matrix.strides[1];
    tensor.strides[1] = matrix.strides[0];

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/transpose-view: failed to allocate Tensor view value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_matrix_transpose_view(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/transpose-view", "1 argument: rank-2 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/transpose-view: input must be a rank-2 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_MAP || source.kind == TENSOR_PAYLOAD_CONTRACT
        ? tensor_realize_to_value(args[0], interp)
        : args[0];
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    if (!is_tensor(matrix_value) || matrix_value.tensor_val == null) {
        return tensor_runtime_raise(interp, "matrix/transpose-view: tensor backing storage is missing");
    }
    return matrix_transpose_view_value(matrix_value, matrix_value.tensor_val, interp, "matrix/transpose-view: tensor metadata is missing");
}

fn Value* prim_matrix_transpose(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/transpose", "1 argument: rank-2 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/transpose: input must be a rank-2 Tensor");

    TensorVal* source = args[0].tensor_val;
    if (source != null && source.kind == TENSOR_PAYLOAD_VIEW) {
        return matrix_transpose_view_value(args[0], source, interp, "matrix/transpose: tensor metadata is missing");
    }
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    if (matrix == null || matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/transpose: expected a rank-2 Tensor",
            null
        );
    }
    usz[2] result_shape_buf = { matrix.shape[1], matrix.shape[0] };
    Value* vulkan_result = matrix_transpose_try_vulkan_value(matrix, result_shape_buf[0:2], interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_transpose_try_cuda_value(matrix, result_shape_buf[0:2], interp);
    if (cuda_result != null) return cuda_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/transpose: tensor backing storage is missing");

    Value* out_value = make_tensor_uninitialized_checked(interp, matrix.dtype, result_shape_buf[0:2], "matrix/transpose: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/transpose: result tensor backing storage is missing");
    }
    if (!matrix_transpose_into(matrix, out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/transpose: failed to transpose tensor storage");
    }

    keep_out = true;
    return out_value;
}

fn Value* prim_matrix_diagonal(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/diagonal", "1 argument: rank-2 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/diagonal: input must be a rank-2 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    if (matrix == null || matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/diagonal: expected a rank-2 Tensor",
            null
        );
    }
    usz[1] result_shape_buf = { matrix_min_usz(matrix.shape[0], matrix.shape[1]) };
    Value* vulkan_result = matrix_diagonal_try_vulkan_value(matrix, result_shape_buf[0:1], interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_diagonal_try_cuda_value(matrix, result_shape_buf[0:1], interp);
    if (cuda_result != null) return cuda_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/diagonal: tensor backing storage is missing");

    Value* out_value = make_tensor_uninitialized_checked(interp, matrix.dtype, result_shape_buf[0:1], "matrix/diagonal: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/diagonal: result tensor backing storage is missing");
    }
    if (!matrix_diagonal_into(matrix, out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/diagonal: failed to extract diagonal tensor storage");
    }

    keep_out = true;
    return out_value;
}

fn Value* prim_matrix_diagonal_matrix(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/diagonal-matrix", "1 argument: rank-1 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/diagonal-matrix: input must be a rank-1 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* diagonal_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (diagonal_value == null || diagonal_value.tag == ERROR) return diagonal_value;
    bool cleanup_diagonal = diagonal_value != args[0];
    defer if (cleanup_diagonal) {
        boundary_cleanup_materialized_value(null, diagonal_value, interp.current_scope);
    }

    TensorVal* diagonal = diagonal_value.tensor_val;
    if (diagonal == null || diagonal.rank != 1 || diagonal.shape == null) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/diagonal-matrix: expected a rank-1 Tensor",
            null
        );
    }
    usz[2] result_shape_buf = { diagonal.shape[0], diagonal.shape[0] };
    Value* vulkan_result = matrix_diagonal_matrix_try_vulkan_value(diagonal, result_shape_buf[0:2], interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_diagonal_matrix_try_cuda_value(diagonal, result_shape_buf[0:2], interp);
    if (cuda_result != null) return cuda_result;
    if (diagonal.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/diagonal-matrix: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(diagonal)) return tensor_runtime_raise(interp, "matrix/diagonal-matrix: tensor backing storage is missing");

    Value* out_value = make_tensor_uninitialized_checked(interp, diagonal.dtype, result_shape_buf[0:2], "matrix/diagonal-matrix: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/diagonal-matrix: result tensor backing storage is missing");
    }
    if (!matrix_diagonal_matrix_into(diagonal, out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/diagonal-matrix: failed to build diagonal matrix storage");
    }

    keep_out = true;
    return out_value;
}

fn Value* prim_matrix_identity(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) {
        return primitive_arity_raise(interp, "matrix/identity", "1 or 2 arguments: non-negative Integer size [dtype]", args.len);
    }
    if (!is_int(args[0]) || args[0].int_val < 0) {
        return tensor_type_raise(interp, "matrix/identity: size must be a non-negative Integer");
    }

    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    if (args.len == 2 && !tensor_dtype_arg_to_dtype(args[1], interp, &dtype)) {
        return tensor_type_raise(interp, "matrix/identity: dtype must be Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex");
    }

    usz n = (usz)args[0].int_val;
    usz[2] result_shape_buf = { n, n };
    Value* out_value = make_tensor_uninitialized_checked(interp, dtype, result_shape_buf[0:2], "matrix/identity: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/identity: result tensor backing storage is missing");
    }
    if (!matrix_identity_tensor_into(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/identity: failed to build identity tensor storage");
    }

    keep_out = true;
    return out_value;
}

fn Value* prim_matrix_trace(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/trace", "1 argument: square rank-2 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/trace: input must be a square rank-2 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    if (matrix == null || matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/trace: expected a square rank-2 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_trace_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = matrix_trace_try_cuda_value(matrix, interp);
    if (cuda_result != null) return cuda_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/trace: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/trace: tensor backing storage is missing");

    return matrix_trace_scalar_value(matrix, interp);
}

fn Value* prim_matrix_rank(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) {
        return primitive_arity_raise(interp, "matrix/rank", "1 or 2 arguments: rank-2 Float64, Float32, Complex128, or Complex64 Tensor [non-negative tolerance]", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/rank: input must be a rank-2 Float64, Float32, Complex128, or Complex64 Tensor");

    double tolerance = 1e-12;
    if (args.len == 2) {
        if (!try_numeric_to_double(args[1], &tolerance) || !conversion_double_is_finite(tolerance) || tolerance < 0.0) {
            return tensor_type_raise(interp, "matrix/rank: tolerance must be a non-negative finite number");
        }
    }

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
        matrix.rank != 2 ||
        matrix.shape == null ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        )
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/rank: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_rank_try_vulkan_value(matrix, tolerance, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/rank: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/rank: tensor backing storage is missing");

    usz rank = 0;
    if (!matrix_rank_count(matrix, tolerance, &rank)) {
        return tensor_runtime_raise(interp, "matrix/rank: failed to compute rank");
    }
    return make_int(interp, (long)rank);
}

fn Value* prim_matrix_norm(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) {
        return primitive_arity_raise(interp, "matrix/norm", "1 or 2 arguments: rank-2 Float64, Float32, Complex128, or Complex64 Tensor [norm selector]", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/norm: input must be a rank-2 Float64, Float32, Complex128, or Complex64 Tensor");

    MatrixNormKind kind = MATRIX_NORM_FROBENIUS;
    if (args.len == 2 && !matrix_norm_kind_from_arg(args[1], interp, &kind)) {
        return tensor_type_raise(interp, "matrix/norm: norm selector must be 'frobenius, 'one, 'infinity, 'max, 'spectral, or 'nuclear");
    }

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
        matrix.rank != 2 ||
        matrix.shape == null ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        )
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/norm: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_norm_try_vulkan_value(matrix, kind, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/norm: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/norm: tensor backing storage is missing");

    double norm = 0.0;
    if (!matrix_norm_calculate(matrix, kind, &norm)) {
        return tensor_runtime_raise(interp, "matrix/norm: failed to compute norm");
    }
    return make_double(interp, norm);
}

fn Value* matrix_singular_values_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/singular-values: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/singular-values: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/singular-values: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/singular-values: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }

    usz k = matrix_min_usz(matrix.shape[0], matrix.shape[1]);
    usz[1] result_shape_buf = { k };
    TensorDType result_dtype = matrix.dtype;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) result_dtype = TENSOR_DTYPE_DOUBLE;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) result_dtype = TENSOR_DTYPE_FLOAT32;
    TensorVal* tensor = tensor_alloc_payload_base_checked(result_dtype, result_shape_buf[0:1], false, false, "matrix/singular-values: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "matrix/singular-values: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_singular_values_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_singular_values_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_singular_values_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_singular_values_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            matrix.shape[1],
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "matrix/singular-values: Vulkan singular-value kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/singular-values: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* matrix_svd_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (matrix.dtype != TENSOR_DTYPE_DOUBLE && matrix.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/svd: Vulkan currently supports Float64 or Float32 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/svd: expected a rank-2 Float64 or Float32 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/svd: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/svd: Vulkan currently supports dense row-major Float64 or Float32 tensors", null);
    }

    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz k = matrix_min_usz(rows, cols);
    void* u_device = null;
    void* s_device = null;
    void* v_device = null;
    int status = matrix.dtype == TENSOR_DTYPE_DOUBLE
        ? omni_tensor_backend_vulkan_svd_f64(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        )
        : omni_tensor_backend_vulkan_svd_f32(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "matrix/svd: Vulkan kernel failed");
    }

    usz[2] u_shape_buf = { rows, k };
    TensorVal* u_tensor = tensor_alloc_payload_base_checked(matrix.dtype, u_shape_buf[0:2], false, false, "matrix/svd: failed to allocate Vulkan U tensor metadata");
    if (u_tensor == null) {
        if (u_device != null) omni_tensor_backend_vulkan_free(u_device);
        if (s_device != null) omni_tensor_backend_vulkan_free(s_device);
        if (v_device != null) omni_tensor_backend_vulkan_free(v_device);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan U tensor metadata");
    }
    u_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    u_tensor.device = TENSOR_DEVICE_VULKAN;
    u_tensor.device_handle = u_device;
    u_tensor.device_finalizer = u_device != null ? &tensor_vulkan_device_finalizer : null;
    u_device = null;

    Value* u_value = interp.alloc_value();
    if (u_value == null) {
        tensor_free_payload(u_tensor);
        if (s_device != null) omni_tensor_backend_vulkan_free(s_device);
        if (v_device != null) omni_tensor_backend_vulkan_free(v_device);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan U tensor value");
    }
    u_value.tag = TENSOR;
    u_value.tensor_val = u_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)u_value, &scope_dtor_value);
    bool keep_u_value = false;
    defer if (!keep_u_value) {
        boundary_cleanup_materialized_value(null, u_value, interp.current_scope);
    }

    usz[1] s_shape_buf = { k };
    TensorVal* s_tensor = tensor_alloc_payload_base_checked(matrix.dtype, s_shape_buf[0:1], false, false, "matrix/svd: failed to allocate Vulkan singular value tensor metadata");
    if (s_tensor == null) {
        if (s_device != null) omni_tensor_backend_vulkan_free(s_device);
        if (v_device != null) omni_tensor_backend_vulkan_free(v_device);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan singular value tensor metadata");
    }
    s_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    s_tensor.device = TENSOR_DEVICE_VULKAN;
    s_tensor.device_handle = s_device;
    s_tensor.device_finalizer = s_device != null ? &tensor_vulkan_device_finalizer : null;
    s_device = null;

    Value* s_value = interp.alloc_value();
    if (s_value == null) {
        tensor_free_payload(s_tensor);
        if (v_device != null) omni_tensor_backend_vulkan_free(v_device);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan singular value tensor value");
    }
    s_value.tag = TENSOR;
    s_value.tensor_val = s_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)s_value, &scope_dtor_value);
    bool keep_s_value = false;
    defer if (!keep_s_value) {
        boundary_cleanup_materialized_value(null, s_value, interp.current_scope);
    }

    usz[2] v_shape_buf = { cols, k };
    TensorVal* v_tensor = tensor_alloc_payload_base_checked(matrix.dtype, v_shape_buf[0:2], false, false, "matrix/svd: failed to allocate Vulkan V tensor metadata");
    if (v_tensor == null) {
        if (v_device != null) omni_tensor_backend_vulkan_free(v_device);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan V tensor metadata");
    }
    v_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    v_tensor.device = TENSOR_DEVICE_VULKAN;
    v_tensor.device_handle = v_device;
    v_tensor.device_finalizer = v_device != null ? &tensor_vulkan_device_finalizer : null;
    v_device = null;

    Value* v_value = interp.alloc_value();
    if (v_value == null) {
        tensor_free_payload(v_tensor);
        return tensor_runtime_raise(interp, "matrix/svd: failed to allocate Vulkan V tensor value");
    }
    v_value.tag = TENSOR;
    v_value.tensor_val = v_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)v_value, &scope_dtor_value);
    bool keep_v_value = false;
    defer if (!keep_v_value) {
        boundary_cleanup_materialized_value(null, v_value, interp.current_scope);
    }

    Value* u_for_dict = boundary_promote_to_root(u_value, interp);
    if (u_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote U tensor", null);
    }
    if (u_for_dict.tag == ERROR) return u_for_dict;
    keep_u_value = u_for_dict == u_value;

    Value* s_for_dict = boundary_promote_to_root(s_value, interp);
    if (s_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/svd: failed to promote singular value tensor", null);
    }
    if (s_for_dict.tag == ERROR) return s_for_dict;
    keep_s_value = s_for_dict == s_value;

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
    keep_u_value = true;
    keep_s_value = true;
    keep_v_value = true;
    return out;
}

fn Value* matrix_eigenvalues_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (matrix.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports Float64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports square dense row-major Float64 tensors", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/eigenvalues: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports dense row-major Float64 tensors", null);
    }

    usz n = matrix.shape[0];
    void* values_device = null;
    void* vectors_device = null;
    int status = omni_tensor_backend_vulkan_symmetric_eigen_f64(
        matrix.device_handle,
        matrix.byte_len,
        n,
        &values_device,
        &vectors_device
    );
    if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
    if (status == OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvalues: input matrix is not symmetric", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_vulkan_status_raise(interp, status, "matrix/eigenvalues: Vulkan Float64 symmetric eigen kernel failed");
    }

    usz[1] value_shape_buf = { n };
    TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_DOUBLE, value_shape_buf[0:1], false, false, "matrix/eigenvalues: failed to allocate Vulkan eigenvalue tensor metadata");
    if (tensor == null) {
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_runtime_raise(interp, "matrix/eigenvalues: failed to allocate Vulkan eigenvalue tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = values_device;
    tensor.device_finalizer = values_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "matrix/eigenvalues: failed to allocate Vulkan eigenvalue tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* matrix_eigenvectors_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (matrix.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports Float64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports square dense row-major Float64 tensors", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports dense row-major Float64 tensors", null);
    }

    usz n = matrix.shape[0];
    void* values_device = null;
    void* vectors_device = null;
    int status = omni_tensor_backend_vulkan_symmetric_eigen_f64(
        matrix.device_handle,
        matrix.byte_len,
        n,
        &values_device,
        &vectors_device
    );
    if (status == OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvectors: input matrix is not symmetric", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_vulkan_status_raise(interp, status, "matrix/eigenvectors: Vulkan Float64 symmetric eigen kernel failed");
    }

    usz[1] value_shape_buf = { n };
    TensorVal* values_tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_DOUBLE, value_shape_buf[0:1], false, false, "matrix/eigenvectors: failed to allocate Vulkan eigenvalue tensor metadata");
    if (values_tensor == null) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_runtime_raise(interp, "matrix/eigenvectors: failed to allocate Vulkan eigenvalue tensor metadata");
    }
    values_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    values_tensor.device = TENSOR_DEVICE_VULKAN;
    values_tensor.device_handle = values_device;
    values_tensor.device_finalizer = values_device != null ? &tensor_vulkan_device_finalizer : null;
    values_device = null;

    Value* values_value = interp.alloc_value();
    if (values_value == null) {
        tensor_free_payload(values_tensor);
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        return tensor_runtime_raise(interp, "matrix/eigenvectors: failed to allocate Vulkan eigenvalue tensor value");
    }
    values_value.tag = TENSOR;
    values_value.tensor_val = values_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)values_value, &scope_dtor_value);
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }

    usz[2] vector_shape_buf = { n, n };
    TensorVal* vectors_tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_DOUBLE, vector_shape_buf[0:2], false, false, "matrix/eigenvectors: failed to allocate Vulkan eigenvector tensor metadata");
    if (vectors_tensor == null) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        return tensor_runtime_raise(interp, "matrix/eigenvectors: failed to allocate Vulkan eigenvector tensor metadata");
    }
    vectors_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    vectors_tensor.device = TENSOR_DEVICE_VULKAN;
    vectors_tensor.device_handle = vectors_device;
    vectors_tensor.device_finalizer = vectors_device != null ? &tensor_vulkan_device_finalizer : null;
    vectors_device = null;

    Value* vectors_value = interp.alloc_value();
    if (vectors_value == null) {
        tensor_free_payload(vectors_tensor);
        return tensor_runtime_raise(interp, "matrix/eigenvectors: failed to allocate Vulkan eigenvector tensor value");
    }
    vectors_value.tag = TENSOR;
    vectors_value.tensor_val = vectors_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)vectors_value, &scope_dtor_value);
    bool keep_vectors = false;
    defer if (!keep_vectors) {
        boundary_cleanup_materialized_value(null, vectors_value, interp.current_scope);
    }

    Value* values_for_dict = boundary_promote_to_root(values_value, interp);
    if (values_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to promote eigenvalue tensor", null);
    }
    if (values_for_dict.tag == ERROR) return values_for_dict;
    keep_values = values_for_dict == values_value;

    Value* vectors_for_dict = boundary_promote_to_root(vectors_value, interp);
    if (vectors_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to promote eigenvector tensor", null);
    }
    if (vectors_for_dict.tag == ERROR) return vectors_for_dict;
    keep_vectors = vectors_for_dict == vectors_value;

    Value* out = make_hashmap_checked(interp, 8, "matrix/eigenvectors: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "values", values_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "vectors", vectors_for_dict)) return err;
    keep_values = true;
    keep_vectors = true;
    return out;
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
            default:
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
            default:
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

fn Value* prim_matrix_svd(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/svd", "1 argument: rank-2 Float64 Tensor or Vulkan Float32 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/svd: input must be a rank-2 Float64 Tensor or Vulkan Float32 Tensor");

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
        (matrix.dtype != TENSOR_DTYPE_DOUBLE && matrix.dtype != TENSOR_DTYPE_FLOAT32) ||
        matrix.rank != 2 ||
        matrix.shape == null
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/svd: expected a rank-2 Float64 Tensor or Vulkan Float32 Tensor",
            null
        );
    }
    Value* vulkan_result = matrix_svd_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
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

fn Value* prim_matrix_eigenvalues(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenvalues", "1 argument: square symmetric Float64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenvalues: input must be a square symmetric Float64 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    Value* vulkan_result = matrix_eigenvalues_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_DOUBLE || matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/eigenvalues: expected a square rank-2 Float64 Tensor",
            null
        );
    }
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/eigenvalues: tensor backing storage is missing");
    if (!matrix_is_exact_symmetric(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvalues: input matrix is not symmetric", null);
    }

    usz n = matrix.shape[0];
    usz eigenvector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &eigenvector_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: eigenvector workspace is too large", null);
    }

    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(n, &eigenvalues)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: failed to allocate eigenvalue workspace", null);
    }
    defer if (eigenvalues != null) mem::free(eigenvalues);

    double* eigenvectors = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: failed to allocate eigenvector workspace", null);
    }
    defer if (eigenvectors != null) mem::free(eigenvectors);

    if (!matrix_symmetric_eigen_factor(matrix, eigenvalues, eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/eigenvalues: failed to compute symmetric eigenvalues", null);
    }

    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_double_uninitialized_checked(interp, value_shape_buf[0:1], "matrix/eigenvalues: failed to allocate eigenvalue tensor");
    if (values_value == null || values_value.tag == ERROR) return values_value;
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    if (values_value.tag != TENSOR || !tensor_has_valid_storage(values_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvalues: eigenvalue tensor backing storage is missing");
    }
    if (n > 0) {
        usz value_bytes = 0;
        if (!tensor_checked_mul_usz(n, double.sizeof, &value_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: eigenvalue tensor is too large", null);
        }
        mem::copy(values_value.tensor_val.data, eigenvalues, value_bytes);
    }

    keep_values = true;
    return values_value;
}

fn Value* prim_matrix_eigenvectors(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenvectors", "1 argument: square symmetric Float64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenvectors: input must be a square symmetric Float64 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* matrix_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (matrix_value == null || matrix_value.tag == ERROR) return matrix_value;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    TensorVal* matrix = matrix_value.tensor_val;
    Value* vulkan_result = matrix_eigenvectors_try_vulkan_value(matrix, interp);
    if (vulkan_result != null) return vulkan_result;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_DOUBLE || matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/eigenvectors: expected a square rank-2 Float64 Tensor",
            null
        );
    }
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/eigenvectors: tensor backing storage is missing");
    if (!matrix_is_exact_symmetric(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvectors: input matrix is not symmetric", null);
    }

    usz n = matrix.shape[0];
    usz eigenvector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &eigenvector_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: eigenvector workspace is too large", null);
    }

    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(n, &eigenvalues)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to allocate eigenvalue workspace", null);
    }
    defer if (eigenvalues != null) mem::free(eigenvalues);

    double* eigenvectors = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to allocate eigenvector workspace", null);
    }
    defer if (eigenvectors != null) mem::free(eigenvectors);

    if (!matrix_symmetric_eigen_factor(matrix, eigenvalues, eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/no-convergence", "matrix/eigenvectors: failed to compute symmetric eigenvectors", null);
    }

    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_double_uninitialized_checked(interp, value_shape_buf[0:1], "matrix/eigenvectors: failed to allocate eigenvalue tensor");
    if (values_value == null || values_value.tag == ERROR) return values_value;
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    if (values_value.tag != TENSOR || !tensor_has_valid_storage(values_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: eigenvalue tensor backing storage is missing");
    }
    if (n > 0) {
        usz value_bytes = 0;
        if (!tensor_checked_mul_usz(n, double.sizeof, &value_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: eigenvalue tensor is too large", null);
        }
        mem::copy(values_value.tensor_val.data, eigenvalues, value_bytes);
    }

    Value* values_for_dict = boundary_promote_to_root(values_value, interp);
    if (values_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to promote eigenvalue tensor", null);
    }
    if (values_for_dict.tag == ERROR) return values_for_dict;
    keep_values = values_for_dict == values_value;

    usz[2] vector_shape_buf = { n, n };
    Value* vectors_value = make_tensor_double_uninitialized_checked(interp, vector_shape_buf[0:2], "matrix/eigenvectors: failed to allocate eigenvector tensor");
    if (vectors_value == null || vectors_value.tag == ERROR) return vectors_value;
    bool keep_vectors = false;
    defer if (!keep_vectors) {
        boundary_cleanup_materialized_value(null, vectors_value, interp.current_scope);
    }
    if (vectors_value.tag != TENSOR || !tensor_has_valid_storage(vectors_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: eigenvector tensor backing storage is missing");
    }
    if (eigenvector_count > 0) {
        usz vector_bytes = 0;
        if (!tensor_checked_mul_usz(eigenvector_count, double.sizeof, &vector_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: eigenvector tensor is too large", null);
        }
        mem::copy(vectors_value.tensor_val.data, eigenvectors, vector_bytes);
    }

    Value* vectors_for_dict = boundary_promote_to_root(vectors_value, interp);
    if (vectors_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to promote eigenvector tensor", null);
    }
    if (vectors_for_dict.tag == ERROR) return vectors_for_dict;
    keep_vectors = vectors_for_dict == vectors_value;

    Value* out = make_hashmap_checked(interp, 8, "matrix/eigenvectors: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "values", values_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "vectors", vectors_for_dict)) return err;
    return out;
}

fn Value* prim_matrix_eigenpairs(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenpairs", "1 argument: square Float64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenpairs: input must be a square Float64 Tensor");

    TensorVal* matrix = null;
    Value* matrix_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &matrix,
        &matrix_value,
        interp,
        "matrix/eigenpairs: tensor backing storage is missing",
        "matrix/eigenpairs: unsupported Tensor device"
    );
    if (resolve_err != null) return resolve_err;
    bool cleanup_matrix = matrix_value != args[0];
    defer if (cleanup_matrix) {
        boundary_cleanup_materialized_value(null, matrix_value, interp.current_scope);
    }

    if (matrix == null || matrix.dtype != TENSOR_DTYPE_DOUBLE || matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/eigenpairs: expected a square rank-2 Float64 Tensor",
            null
        );
    }
    usz n = matrix.shape[0];
    usz eigenvector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &eigenvector_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: eigenvector workspace is too large", null);
    }

    double* real_parts = null;
    if (!matrix_double_workspace_zero(n, &real_parts)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvalue real workspace", null);
    }
    defer if (real_parts != null) mem::free(real_parts);

    double* imag_parts = null;
    if (!matrix_double_workspace_zero(n, &imag_parts)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvalue imaginary workspace", null);
    }
    defer if (imag_parts != null) mem::free(imag_parts);

    double* raw_vectors = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &raw_vectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvector workspace", null);
    }
    defer if (raw_vectors != null) mem::free(raw_vectors);

    double* pure_vector_real = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &pure_vector_real)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate pure eigenvector real workspace", null);
    }
    defer if (pure_vector_real != null) mem::free(pure_vector_real);

    double* pure_vector_imag = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &pure_vector_imag)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate pure eigenvector imaginary workspace", null);
    }
    defer if (pure_vector_imag != null) mem::free(pure_vector_imag);

    bool use_pure_vectors = false;
    if (n > 0) {
        double* work = null;
        if (!matrix_copy_float64_workspace(matrix, &work)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate LAPACK workspace", null);
        }
        defer if (work != null) mem::free(work);

        int lapack_status = omni_tensor_backend_lapack_dgeev(n, work, real_parts, imag_parts, raw_vectors);
        switch (lapack_status) {
            case OMNI_TENSOR_LAPACK_SUCCESS:
                break;
            case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                if (!matrix_general_eigenpairs_pure(matrix, real_parts, imag_parts, pure_vector_real, pure_vector_imag)) {
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/no-convergence",
                        "matrix/eigenpairs: failed to compute general eigenpairs",
                        null
                    );
                }
                use_pure_vectors = true;
            case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                return raise_error_with_payload_names(
                    interp,
                    "tensor",
                    "tensor/no-convergence",
                    "matrix/eigenpairs: failed to compute general eigenpairs",
                    null
                );
            default:
                return tensor_runtime_raise(interp, "matrix/eigenpairs: LAPACK backend rejected eigenpair input");
        }
    }

    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_uninitialized_checked(
        interp,
        TENSOR_DTYPE_BIG_COMPLEX,
        value_shape_buf[0:1],
        "matrix/eigenpairs: failed to allocate eigenvalue tensor"
    );
    if (values_value == null || values_value.tag == ERROR) return values_value;
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    if (values_value.tag != TENSOR || !tensor_has_valid_storage(values_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenpairs: eigenvalue tensor backing storage is missing");
    }

    usz[2] vector_shape_buf = { n, n };
    Value* vectors_value = make_tensor_uninitialized_checked(
        interp,
        TENSOR_DTYPE_BIG_COMPLEX,
        vector_shape_buf[0:2],
        "matrix/eigenpairs: failed to allocate eigenvector tensor"
    );
    if (vectors_value == null || vectors_value.tag == ERROR) return vectors_value;
    bool keep_vectors = false;
    defer if (!keep_vectors) {
        boundary_cleanup_materialized_value(null, vectors_value, interp.current_scope);
    }
    if (vectors_value.tag != TENSOR || !tensor_has_valid_storage(vectors_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenpairs: eigenvector tensor backing storage is missing");
    }

    bool built_eigenpairs = use_pure_vectors
        ? matrix_fill_general_eigenpairs_from_vectors_big_complex(
            n,
            real_parts,
            imag_parts,
            pure_vector_real,
            pure_vector_imag,
            values_value.tensor_val,
            vectors_value.tensor_val
        )
        : matrix_fill_general_eigenpairs_big_complex(
            n,
            real_parts,
            imag_parts,
            raw_vectors,
            values_value.tensor_val,
            vectors_value.tensor_val
        );
    if (!built_eigenpairs) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/out-of-memory",
            "matrix/eigenpairs: failed to build BigComplex eigenpair tensors",
            null
        );
    }

    Value* values_for_dict = boundary_promote_to_root(values_value, interp);
    if (values_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to promote eigenvalue tensor", null);
    }
    if (values_for_dict.tag == ERROR) return values_for_dict;
    keep_values = values_for_dict == values_value;

    Value* vectors_for_dict = boundary_promote_to_root(vectors_value, interp);
    if (vectors_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to promote eigenvector tensor", null);
    }
    if (vectors_for_dict.tag == ERROR) return vectors_for_dict;
    keep_vectors = vectors_for_dict == vectors_value;

    Value* out = make_hashmap_checked(interp, 8, "matrix/eigenpairs: failed to allocate result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/eigenpairs: result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "values", values_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "vectors", vectors_for_dict)) return err;
    return out;
}
