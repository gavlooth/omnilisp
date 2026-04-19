module lisp;

import std::core::mem;
import main;



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
