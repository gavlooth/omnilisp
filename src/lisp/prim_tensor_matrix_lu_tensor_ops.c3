module lisp;

import std::core::mem;
import main;

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
