module lisp;

import std::core::mem;
import main;

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
