module lisp;

import std::core::mem;
import main;

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

fn Complex128Val matrix_complex128_add(Complex128Val left, Complex128Val right) @inline {
    Complex128Val out;
    out.real = left.real + right.real;
    out.imag = left.imag + right.imag;
    return out;
}
fn Complex128Val matrix_complex128_sub(Complex128Val left, Complex128Val right) @inline {
    Complex128Val out;
    out.real = left.real - right.real;
    out.imag = left.imag - right.imag;
    return out;
}
fn Complex128Val matrix_complex128_mul(Complex128Val left, Complex128Val right) @inline {
    Complex128Val out;
    out.real = left.real * right.real - left.imag * right.imag;
    out.imag = left.real * right.imag + left.imag * right.real;
    return out;
}
fn Complex128Val matrix_complex128_scale(Complex128Val value, double scale) @inline {
    Complex128Val out;
    out.real = value.real * scale;
    out.imag = value.imag * scale;
    return out;
}
fn double matrix_complex128_abs_squared(Complex128Val value) @inline {
    return value.real * value.real + value.imag * value.imag;
}
fn void matrix_complex128_column_orthogonalize_vector(Complex128Val* vector, Complex128Val* basis, usz rows, usz columns, usz col) {
    if (vector == null || basis == null) return;
    for (usz prev = 0; prev < col; prev++) {
        Complex128Val dot;
        dot.real = 0.0;
        dot.imag = 0.0;
        for (usz row = 0; row < rows; row++) {
            dot = matrix_complex128_add(
                dot,
                matrix_complex128_mul(matrix_complex128_conjugate(basis[row * columns + prev]), vector[row])
            );
        }
        for (usz row = 0; row < rows; row++) {
            vector[row] = matrix_complex128_sub(vector[row], matrix_complex128_mul(basis[row * columns + prev], dot));
        }
    }
}
fn bool matrix_complex128_normalize_vector(Complex128Val* vector, usz rows) {
    if (rows == 0) return true;
    if (vector == null) return false;
    double norm_squared = 0.0;
    for (usz row = 0; row < rows; row++) {
        norm_squared += matrix_complex128_abs_squared(vector[row]);
    }
    if (!(norm_squared > 1e-24)) return false;
    double inv_norm = 1.0 / tensor_c_sqrt(norm_squared);
    for (usz row = 0; row < rows; row++) {
        vector[row] = matrix_complex128_scale(vector[row], inv_norm);
    }
    return true;
}
fn bool matrix_complex128_fill_completion_column(Complex128Val* matrix, usz rows, usz columns, usz col) {
    if (matrix == null) return rows == 0;
    Complex128Val* trial = null;
    if (!matrix_complex128_workspace_zero(rows, &trial)) return false;
    defer if (trial != null) mem::free(trial);
    for (usz seed = 0; seed < rows; seed++) {
        for (usz row = 0; row < rows; row++) {
            trial[row].real = row == seed ? 1.0 : 0.0;
            trial[row].imag = 0.0;
        }
        matrix_complex128_column_orthogonalize_vector(trial, matrix, rows, columns, col);
        if (!matrix_complex128_normalize_vector(trial, rows)) continue;
        for (usz row = 0; row < rows; row++) {
            matrix[row * columns + col] = trial[row];
        }
        return true;
    }
    return rows == 0;
}
fn bool matrix_complex128_orthonormalize_column(Complex128Val* matrix, usz rows, usz columns, usz col) {
    if (matrix == null) return rows == 0;
    Complex128Val* vector = null;
    if (!matrix_complex128_workspace_zero(rows, &vector)) return false;
    defer if (vector != null) mem::free(vector);
    for (usz row = 0; row < rows; row++) {
        vector[row] = matrix[row * columns + col];
    }
    matrix_complex128_column_orthogonalize_vector(vector, matrix, rows, columns, col);
    if (!matrix_complex128_normalize_vector(vector, rows)) return false;
    for (usz row = 0; row < rows; row++) {
        matrix[row * columns + col] = vector[row];
    }
    return true;
}
fn bool matrix_build_svd_hermitian_complex128(TensorVal* matrix, Complex128Val* gram, bool tall_or_square) {
    if (matrix == null || matrix.shape == null || matrix.data == null || gram == null) return false;
    Complex128Val* source = (Complex128Val*)matrix.data;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz gram_n = tall_or_square ? cols : rows;
    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    for (usz i = 0; i < gram_count; i++) {
        gram[i].real = 0.0;
        gram[i].imag = 0.0;
    }
    if (tall_or_square) {
        for (usz left = 0; left < cols; left++) {
            for (usz right = 0; right < cols; right++) {
                Complex128Val sum;
                sum.real = 0.0;
                sum.imag = 0.0;
                for (usz row = 0; row < rows; row++) {
                    sum = matrix_complex128_add(
                        sum,
                        matrix_complex128_mul(matrix_complex128_conjugate(source[row * cols + left]), source[row * cols + right])
                    );
                }
                gram[left * gram_n + right] = sum;
            }
        }
    } else {
        for (usz top = 0; top < rows; top++) {
            for (usz bottom = 0; bottom < rows; bottom++) {
                Complex128Val sum;
                sum.real = 0.0;
                sum.imag = 0.0;
                for (usz col = 0; col < cols; col++) {
                    sum = matrix_complex128_add(
                        sum,
                        matrix_complex128_mul(source[top * cols + col], matrix_complex128_conjugate(source[bottom * cols + col]))
                    );
                }
                gram[top * gram_n + bottom] = sum;
            }
        }
    }
    return true;
}
fn void matrix_complex128_hermitian_matvec(Complex128Val* matrix, Complex128Val* vector, Complex128Val* out, usz n) {
    if (matrix == null || vector == null || out == null) return;
    for (usz row = 0; row < n; row++) {
        Complex128Val sum;
        sum.real = 0.0;
        sum.imag = 0.0;
        for (usz col = 0; col < n; col++) {
            sum = matrix_complex128_add(sum, matrix_complex128_mul(matrix[row * n + col], vector[col]));
        }
        out[row] = sum;
    }
}
fn double matrix_complex128_rayleigh_real(Complex128Val* matrix, Complex128Val* vector, Complex128Val* work, usz n) {
    if (matrix == null || vector == null || work == null) return 0.0;
    matrix_complex128_hermitian_matvec(matrix, vector, work, n);
    Complex128Val dot;
    dot.real = 0.0;
    dot.imag = 0.0;
    for (usz i = 0; i < n; i++) {
        dot = matrix_complex128_add(dot, matrix_complex128_mul(matrix_complex128_conjugate(vector[i]), work[i]));
    }
    return dot.real;
}
fn bool matrix_complex128_hermitian_power_eigen(
    Complex128Val* hermitian,
    double* eigenvalues,
    Complex128Val* eigenvectors,
    usz n,
    usz k
) {
    if (hermitian == null || eigenvalues == null || eigenvectors == null) return k == 0;
    if (k == 0) return true;
    usz matrix_count = 0;
    if (!tensor_checked_mul_usz(n, n, &matrix_count)) return false;
    Complex128Val* work_matrix = null;
    if (!matrix_complex128_workspace_zero(matrix_count, &work_matrix)) return false;
    defer if (work_matrix != null) mem::free(work_matrix);
    for (usz i = 0; i < matrix_count; i++) work_matrix[i] = hermitian[i];
    Complex128Val* vector = null;
    if (!matrix_complex128_workspace_zero(n, &vector)) return false;
    defer if (vector != null) mem::free(vector);
    Complex128Val* next = null;
    if (!matrix_complex128_workspace_zero(n, &next)) return false;
    defer if (next != null) mem::free(next);
    usz max_iterations = (usz)128;
    if (n <= (usz.max - (usz)128) / (usz)64) max_iterations = n * (usz)64 + (usz)128;
    for (usz col = 0; col < k; col++) {
        usz seed = 0;
        double best_diagonal = -1.0;
        for (usz i = 0; i < n; i++) {
            double diagonal = work_matrix[i * n + i].real;
            if (diagonal > best_diagonal) {
                best_diagonal = diagonal;
                seed = i;
            }
        }
        for (usz row = 0; row < n; row++) {
            vector[row].real = row == seed ? 1.0 : 0.0;
            vector[row].imag = 0.0;
        }
        matrix_complex128_column_orthogonalize_vector(vector, eigenvectors, n, k, col);
        if (!matrix_complex128_normalize_vector(vector, n)) {
            if (!matrix_complex128_fill_completion_column(eigenvectors, n, k, col)) return false;
            eigenvalues[col] = 0.0;
            continue;
        }
        for (usz iter = 0; iter < max_iterations; iter++) {
            matrix_complex128_hermitian_matvec(work_matrix, vector, next, n);
            matrix_complex128_column_orthogonalize_vector(next, eigenvectors, n, k, col);
            if (!matrix_complex128_normalize_vector(next, n)) break;
            double diff = 0.0;
            for (usz row = 0; row < n; row++) {
                diff += matrix_complex128_abs_squared(matrix_complex128_sub(next[row], vector[row]));
                vector[row] = next[row];
            }
            if (diff <= 1e-20) break;
        }
        double eigenvalue = matrix_complex128_rayleigh_real(hermitian, vector, next, n);
        if (eigenvalue < 0.0 && eigenvalue > -1e-10) eigenvalue = 0.0;
        if (eigenvalue < 0.0) eigenvalue = 0.0;
        eigenvalues[col] = eigenvalue;
        for (usz row = 0; row < n; row++) {
            eigenvectors[row * k + col] = vector[row];
        }
        for (usz row = 0; row < n; row++) {
            for (usz inner_col = 0; inner_col < n; inner_col++) {
                Complex128Val outer = matrix_complex128_mul(vector[row], matrix_complex128_conjugate(vector[inner_col]));
                work_matrix[row * n + inner_col] = matrix_complex128_sub(
                    work_matrix[row * n + inner_col],
                    matrix_complex128_scale(outer, eigenvalue)
                );
            }
        }
    }
    return true;
}
fn void matrix_complex128_sort_eigen_desc(double* eigenvalues, Complex128Val* eigenvectors, usz rows, usz columns) {
    if (eigenvalues == null || eigenvectors == null) return;
    for (usz i = 0; i < columns; i++) {
        usz best = i;
        for (usz j = i + 1; j < columns; j++) {
            if (eigenvalues[j] > eigenvalues[best]) best = j;
        }
        if (best == i) continue;
        double tmp_value = eigenvalues[i];
        eigenvalues[i] = eigenvalues[best];
        eigenvalues[best] = tmp_value;
        for (usz row = 0; row < rows; row++) {
            Complex128Val tmp_vector = eigenvectors[row * columns + i];
            eigenvectors[row * columns + i] = eigenvectors[row * columns + best];
            eigenvectors[row * columns + best] = tmp_vector;
        }
    }
}
fn void matrix_complex128_phase_normalize_pair(
    Complex128Val* u,
    Complex128Val* v,
    usz rows,
    usz cols,
    usz columns,
    usz col,
    bool use_v_anchor
) {
    Complex128Val* anchor_matrix = use_v_anchor ? v : u;
    usz anchor_rows = use_v_anchor ? cols : rows;
    usz anchor_index = 0;
    double anchor_abs = 0.0;
    for (usz row = 0; row < anchor_rows; row++) {
        double candidate = matrix_complex128_abs_squared(anchor_matrix[row * columns + col]);
        if (candidate > anchor_abs) {
            anchor_abs = candidate;
            anchor_index = row;
        }
    }
    if (!(anchor_abs > 1e-24)) return;
    Complex128Val anchor = anchor_matrix[anchor_index * columns + col];
    double inv_abs = 1.0 / tensor_c_sqrt(anchor_abs);
    Complex128Val phase = matrix_complex128_scale(matrix_complex128_conjugate(anchor), inv_abs);
    for (usz row = 0; row < rows; row++) {
        u[row * columns + col] = matrix_complex128_mul(u[row * columns + col], phase);
    }
    for (usz row = 0; row < cols; row++) {
        v[row * columns + col] = matrix_complex128_mul(v[row * columns + col], phase);
    }
}
fn bool matrix_svd_factor_complex128(TensorVal* matrix, Complex128Val* u, double* s_values, Complex128Val* v, usz k) {
    if (matrix == null || matrix.shape == null || u == null || s_values == null || v == null) return k == 0;
    if (matrix.data == null) return k == 0;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    bool tall_or_square = rows >= cols;
    usz gram_n = tall_or_square ? cols : rows;
    usz gram_count = 0;
    if (!tensor_checked_mul_usz(gram_n, gram_n, &gram_count)) return false;
    Complex128Val* gram = null;
    if (!matrix_complex128_workspace_zero(gram_count, &gram)) return false;
    defer if (gram != null) mem::free(gram);
    usz eigenvector_count = 0;
    if (!tensor_checked_mul_usz(gram_n, k, &eigenvector_count)) return false;
    Complex128Val* eigenvectors = null;
    if (!matrix_complex128_workspace_zero(eigenvector_count, &eigenvectors)) return false;
    defer if (eigenvectors != null) mem::free(eigenvectors);
    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(k, &eigenvalues)) return false;
    defer if (eigenvalues != null) mem::free(eigenvalues);
    if (!matrix_build_svd_hermitian_complex128(matrix, gram, tall_or_square)) return false;
    if (!matrix_complex128_hermitian_power_eigen(gram, eigenvalues, eigenvectors, gram_n, k)) return false;
    matrix_complex128_sort_eigen_desc(eigenvalues, eigenvectors, gram_n, k);
    double max_singular = 0.0;
    for (usz i = 0; i < k; i++) {
        s_values[i] = tensor_c_sqrt(eigenvalues[i]);
        if (s_values[i] > max_singular) max_singular = s_values[i];
    }
    double singular_tolerance = 1e-10 * (max_singular + 1.0);
    Complex128Val* source = (Complex128Val*)matrix.data;
    if (tall_or_square) {
        for (usz row = 0; row < cols; row++) {
            for (usz col = 0; col < k; col++) v[row * k + col] = eigenvectors[row * k + col];
        }
        for (usz col = 0; col < k; col++) {
            if (s_values[col] > singular_tolerance) {
                for (usz row = 0; row < rows; row++) {
                    Complex128Val sum;
                    sum.real = 0.0;
                    sum.imag = 0.0;
                    for (usz source_col = 0; source_col < cols; source_col++) {
                        sum = matrix_complex128_add(sum, matrix_complex128_mul(source[row * cols + source_col], v[source_col * k + col]));
                    }
                    u[row * k + col] = matrix_complex128_scale(sum, 1.0 / s_values[col]);
                }
                if (!matrix_complex128_orthonormalize_column(u, rows, k, col)) {
                    if (!matrix_complex128_fill_completion_column(u, rows, k, col)) return false;
                }
            } else if (!matrix_complex128_fill_completion_column(u, rows, k, col)) {
                return false;
            }
            matrix_complex128_phase_normalize_pair(u, v, rows, cols, k, col, true);
        }
    } else {
        for (usz row = 0; row < rows; row++) {
            for (usz col = 0; col < k; col++) u[row * k + col] = eigenvectors[row * k + col];
        }
        for (usz col = 0; col < k; col++) {
            if (s_values[col] > singular_tolerance) {
                for (usz source_col = 0; source_col < cols; source_col++) {
                    Complex128Val sum;
                    sum.real = 0.0;
                    sum.imag = 0.0;
                    for (usz row = 0; row < rows; row++) {
                        sum = matrix_complex128_add(
                            sum,
                            matrix_complex128_mul(matrix_complex128_conjugate(source[row * cols + source_col]), u[row * k + col])
                        );
                    }
                    v[source_col * k + col] = matrix_complex128_scale(sum, 1.0 / s_values[col]);
                }
                if (!matrix_complex128_orthonormalize_column(v, cols, k, col)) {
                    if (!matrix_complex128_fill_completion_column(v, cols, k, col)) return false;
                }
            } else if (!matrix_complex128_fill_completion_column(v, cols, k, col)) {
                return false;
            }
            matrix_complex128_phase_normalize_pair(u, v, rows, cols, k, col, false);
        }
    }
    return true;
}
fn bool matrix_svd_factor_complex64(TensorVal* matrix, Complex64Val* u, float* s_values, Complex64Val* v, usz k) {
    if (matrix == null || matrix.shape == null || u == null || s_values == null || v == null) return k == 0;
    if (matrix.data == null) return k == 0;
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz source_count = 0;
    if (!tensor_checked_mul_usz(rows, cols, &source_count)) return false;
    Complex128Val* source_wide = null;
    if (!matrix_complex128_workspace_zero(source_count, &source_wide)) return false;
    defer if (source_wide != null) mem::free(source_wide);
    Complex64Val* source = (Complex64Val*)matrix.data;
    for (usz i = 0; i < source_count; i++) {
        source_wide[i].real = (double)source[i].real;
        source_wide[i].imag = (double)source[i].imag;
    }
    usz u_count = 0;
    if (!tensor_checked_mul_usz(rows, k, &u_count)) return false;
    usz v_count = 0;
    if (!tensor_checked_mul_usz(cols, k, &v_count)) return false;
    Complex128Val* u_wide = null;
    if (!matrix_complex128_workspace_zero(u_count, &u_wide)) return false;
    defer if (u_wide != null) mem::free(u_wide);
    Complex128Val* v_wide = null;
    if (!matrix_complex128_workspace_zero(v_count, &v_wide)) return false;
    defer if (v_wide != null) mem::free(v_wide);
    double* s_wide = null;
    if (!matrix_double_workspace_zero(k, &s_wide)) return false;
    defer if (s_wide != null) mem::free(s_wide);
    TensorVal wide_matrix = *matrix;
    wide_matrix.dtype = TENSOR_DTYPE_COMPLEX128;
    wide_matrix.data = source_wide;
    if (!matrix_svd_factor_complex128(&wide_matrix, u_wide, s_wide, v_wide, k)) return false;
    for (usz i = 0; i < u_count; i++) {
        u[i].real = (float)u_wide[i].real;
        u[i].imag = (float)u_wide[i].imag;
    }
    for (usz i = 0; i < v_count; i++) {
        v[i].real = (float)v_wide[i].real;
        v[i].imag = (float)v_wide[i].imag;
    }
    for (usz i = 0; i < k; i++) s_values[i] = (float)s_wide[i];
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
    if (matrix.data == null) return false;
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
    if (matrix.data == null) return false;
    float* work = (float*)mem::malloc(matrix.byte_len);
    if (work == null) {
        return false;
    }
    mem::copy(work, matrix.data, matrix.byte_len);
    *out_work = work;
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
        if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN) ||
            !tensor_concrete_zero_offset_dense_row_major(matrix)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: Vulkan source must be dense row-major concrete storage", null);
        }
        if (!tensor_dtype_is_vulkan_storage(matrix.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
    } else if (matrix.device == TENSOR_DEVICE_CUDA) {
        if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_CUDA) ||
            !tensor_concrete_zero_offset_dense_row_major(matrix)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: CUDA source must be dense row-major concrete storage", null);
        }
        if (!tensor_dtype_is_cuda_storage(matrix.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
    } else {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/transpose-view: currently supports CPU, CUDA, or Vulkan Tensor storage", null);
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
    tensor.alias_origin = matrix.alias_origin != null ? matrix.alias_origin : (void*)matrix;
    tensor.data = matrix.device == TENSOR_DEVICE_CPU ? matrix.data : null;
    tensor.device_handle = matrix.device != TENSOR_DEVICE_CPU ? matrix.device_handle : null;
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
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "matrix/transpose-view: failed to register Tensor view destructor");
    }
    return result;
}
<*
@require interp != null
*>
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
<*
@require interp != null
*>
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
<*
@require interp != null
*>
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
<*
@require interp != null
*>
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
