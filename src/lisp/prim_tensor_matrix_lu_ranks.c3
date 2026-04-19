module lisp;

import std::core::mem;
import main;

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
