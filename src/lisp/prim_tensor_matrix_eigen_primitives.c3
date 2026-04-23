module lisp;

import std::core::mem;
import main;

fn bool matrix_is_exact_hermitian_complex128(TensorVal* matrix) {
    if (matrix == null || matrix.shape == null || matrix.data == null) return false;
    usz n = matrix.shape[0];
    Complex128Val* source = (Complex128Val*)matrix.data;
    for (usz row = 0; row < n; row++) {
        if (source[row * n + row].imag != 0.0) return false;
        for (usz col = row + 1; col < n; col++) {
            Complex128Val upper = source[row * n + col];
            Complex128Val lower = source[col * n + row];
            if (upper.real != lower.real || upper.imag != -lower.imag) return false;
        }
    }
    return true;
}

fn bool matrix_is_exact_hermitian_complex64(TensorVal* matrix) {
    if (matrix == null || matrix.shape == null || matrix.data == null) return false;
    usz n = matrix.shape[0];
    Complex64Val* source = (Complex64Val*)matrix.data;
    for (usz row = 0; row < n; row++) {
        if (source[row * n + row].imag != 0.0f) return false;
        for (usz col = row + 1; col < n; col++) {
            Complex64Val upper = source[row * n + col];
            Complex64Val lower = source[col * n + row];
            if (upper.real != lower.real || upper.imag != -lower.imag) return false;
        }
    }
    return true;
}

fn bool matrix_copy_hermitian_complex128_workspace(TensorVal* matrix, Complex128Val** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null || matrix.shape == null) return false;
    usz n = matrix.shape[0];
    usz count = 0;
    if (!tensor_checked_mul_usz(n, n, &count)) return false;
    if (count > 0 && matrix.data == null) return false;
    if (!matrix_complex128_workspace_zero(count, out_work)) return false;
    if (count == 0) return true;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        usz byte_count = 0;
        if (!tensor_checked_mul_usz(count, Complex128Val.sizeof, &byte_count)) return false;
        mem::copy(*out_work, matrix.data, byte_count);
        return true;
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val* source = (Complex64Val*)matrix.data;
        for (usz i = 0; i < count; i++) {
            (*out_work)[i].real = (double)source[i].real;
            (*out_work)[i].imag = (double)source[i].imag;
        }
        return true;
    }
    return false;
}

fn void matrix_complex128_phase_normalize_columns(Complex128Val* vectors, usz rows, usz columns) {
    if (vectors == null) return;
    for (usz col = 0; col < columns; col++) {
        usz anchor_row = 0;
        double anchor_abs = 0.0;
        for (usz row = 0; row < rows; row++) {
            double candidate = matrix_complex128_abs_squared(vectors[row * columns + col]);
            if (candidate > anchor_abs) {
                anchor_abs = candidate;
                anchor_row = row;
            }
        }
        if (!(anchor_abs > 1e-24)) continue;
        Complex128Val anchor = vectors[anchor_row * columns + col];
        Complex128Val phase = matrix_complex128_scale(matrix_complex128_conjugate(anchor), 1.0 / tensor_c_sqrt(anchor_abs));
        for (usz row = 0; row < rows; row++) {
            vectors[row * columns + col] = matrix_complex128_mul(vectors[row * columns + col], phase);
        }
    }
}

fn bool matrix_hermitian_jacobi_eigen_complex128(
    Complex128Val* matrix,
    double* eigenvalues,
    Complex128Val* eigenvectors,
    usz n
) {
    if (n == 0) return true;
    if (matrix == null || eigenvalues == null || eigenvectors == null) return false;
    for (usz row = 0; row < n; row++) {
        for (usz col = 0; col < n; col++) {
            eigenvectors[row * n + col].real = row == col ? 1.0 : 0.0;
            eigenvectors[row * n + col].imag = 0.0;
        }
    }
    usz sweep_base = 0;
    if (!tensor_checked_mul_usz(n, n, &sweep_base)) return false;
    if (sweep_base > (usz.max - (usz)1) / (usz)96) return false;
    usz max_sweeps = sweep_base * (usz)96 + (usz)1;
    for (usz sweep = 0; sweep < max_sweeps; sweep++) {
        usz p = 0;
        usz q = n > 1 ? 1 : 0;
        double max_offdiag_sq = 0.0;
        double max_diag = 0.0;
        for (usz row = 0; row < n; row++) {
            double diag = tensor_c_fabs(matrix[row * n + row].real);
            if (diag > max_diag) max_diag = diag;
            for (usz col = row + 1; col < n; col++) {
                double offdiag_sq = matrix_complex128_abs_squared(matrix[row * n + col]);
                if (offdiag_sq > max_offdiag_sq) {
                    max_offdiag_sq = offdiag_sq;
                    p = row;
                    q = col;
                }
            }
        }
        double tolerance = 1e-12 * (max_diag + 1.0);
        if (max_offdiag_sq <= tolerance * tolerance) {
            for (usz i = 0; i < n; i++) {
                eigenvalues[i] = tensor_c_fabs(matrix[i * n + i].real) < 1e-12 ? 0.0 : matrix[i * n + i].real;
            }
            matrix_complex128_phase_normalize_columns(eigenvectors, n, n);
            matrix_complex128_sort_eigen_desc(eigenvalues, eigenvectors, n, n);
            return true;
        }
        Complex128Val apq = matrix[p * n + q];
        double apq_abs = tensor_c_sqrt(max_offdiag_sq);
        if (!(apq_abs > 0.0)) continue;
        Complex128Val phase_conj = matrix_complex128_scale(matrix_complex128_conjugate(apq), 1.0 / apq_abs);
        double app = matrix[p * n + p].real;
        double aqq = matrix[q * n + q].real;
        double tau = (aqq - app) / (2.0 * apq_abs);
        double tau_abs = tensor_c_fabs(tau);
        double t = 1.0 / (tau_abs + tensor_c_sqrt(1.0 + tau * tau));
        if (tau < 0.0) t = -t;
        double c = 1.0 / tensor_c_sqrt(1.0 + t * t);
        double s = t * c;
        for (usz k = 0; k < n; k++) {
            if (k == p || k == q) continue;
            Complex128Val akp = matrix[k * n + p];
            Complex128Val akq = matrix_complex128_mul(matrix[k * n + q], phase_conj);
            Complex128Val new_kp = matrix_complex128_sub(matrix_complex128_scale(akp, c), matrix_complex128_scale(akq, s));
            Complex128Val new_kq = matrix_complex128_add(matrix_complex128_scale(akp, s), matrix_complex128_scale(akq, c));
            matrix[k * n + p] = new_kp;
            matrix[p * n + k] = matrix_complex128_conjugate(new_kp);
            matrix[k * n + q] = new_kq;
            matrix[q * n + k] = matrix_complex128_conjugate(new_kq);
        }
        matrix[p * n + p].real = c * c * app - 2.0 * s * c * apq_abs + s * s * aqq;
        matrix[p * n + p].imag = 0.0;
        matrix[q * n + q].real = s * s * app + 2.0 * s * c * apq_abs + c * c * aqq;
        matrix[q * n + q].imag = 0.0;
        matrix[p * n + q].real = 0.0;
        matrix[p * n + q].imag = 0.0;
        matrix[q * n + p].real = 0.0;
        matrix[q * n + p].imag = 0.0;
        for (usz row = 0; row < n; row++) {
            Complex128Val vip = eigenvectors[row * n + p];
            Complex128Val viq = matrix_complex128_mul(eigenvectors[row * n + q], phase_conj);
            eigenvectors[row * n + p] = matrix_complex128_sub(matrix_complex128_scale(vip, c), matrix_complex128_scale(viq, s));
            eigenvectors[row * n + q] = matrix_complex128_add(matrix_complex128_scale(vip, s), matrix_complex128_scale(viq, c));
        }
    }
    return false;
}

fn bool matrix_hermitian_eigen_factor_complex128(TensorVal* matrix, double* eigenvalues, Complex128Val* eigenvectors) {
    if (matrix == null || matrix.shape == null || eigenvalues == null || eigenvectors == null) {
        return matrix != null && matrix.shape != null && matrix.shape[0] == 0;
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        if (!matrix_is_exact_hermitian_complex128(matrix)) return false;
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        if (!matrix_is_exact_hermitian_complex64(matrix)) return false;
    } else {
        return false;
    }
    usz n = matrix.shape[0];
    Complex128Val* work = null;
    if (!matrix_copy_hermitian_complex128_workspace(matrix, &work)) return false;
    defer if (work != null) mem::free(work);
    return matrix_hermitian_jacobi_eigen_complex128(work, eigenvalues, eigenvectors, n);
}

fn Value* matrix_hermitian_eigenvalues_cpu_value(TensorVal* matrix, Interp* interp) {
    usz n = matrix.shape[0];
    usz vector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &vector_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: eigenvector workspace is too large", null);
    }
    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(n, &eigenvalues)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: failed to allocate eigenvalue workspace", null);
    }
    defer if (eigenvalues != null) mem::free(eigenvalues);
    Complex128Val* eigenvectors = null;
    if (!matrix_complex128_workspace_zero(vector_count, &eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: failed to allocate eigenvector workspace", null);
    }
    defer if (eigenvectors != null) mem::free(eigenvectors);
    if (!matrix_hermitian_eigen_factor_complex128(matrix, eigenvalues, eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvalues: input matrix is not Hermitian", null);
    }
    TensorDType value_dtype = matrix.dtype == TENSOR_DTYPE_COMPLEX64 ? TENSOR_DTYPE_FLOAT32 : TENSOR_DTYPE_DOUBLE;
    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_uninitialized_checked(interp, value_dtype, value_shape_buf[0:1], "matrix/eigenvalues: failed to allocate eigenvalue tensor");
    if (values_value == null || values_value.tag == ERROR) return values_value;
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    if (values_value.tag != TENSOR || !tensor_has_valid_storage(values_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvalues: eigenvalue tensor backing storage is missing");
    }
    if (value_dtype == TENSOR_DTYPE_FLOAT32) {
        float* out_values = (float*)values_value.tensor_val.data;
        for (usz i = 0; i < n; i++) out_values[i] = (float)eigenvalues[i];
    } else if (n > 0) {
        usz value_bytes = 0;
        if (!tensor_checked_mul_usz(n, double.sizeof, &value_bytes)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvalues: eigenvalue tensor is too large", null);
        }
        mem::copy(values_value.tensor_val.data, eigenvalues, value_bytes);
    }
    keep_values = true;
    return values_value;
}

fn Value* matrix_hermitian_eigenvectors_cpu_value(TensorVal* matrix, Interp* interp) {
    usz n = matrix.shape[0];
    usz vector_count = 0;
    if (!tensor_checked_mul_usz(n, n, &vector_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: eigenvector workspace is too large", null);
    }
    double* eigenvalues = null;
    if (!matrix_double_workspace_zero(n, &eigenvalues)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to allocate eigenvalue workspace", null);
    }
    defer if (eigenvalues != null) mem::free(eigenvalues);
    Complex128Val* eigenvectors = null;
    if (!matrix_complex128_workspace_zero(vector_count, &eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenvectors: failed to allocate eigenvector workspace", null);
    }
    defer if (eigenvectors != null) mem::free(eigenvectors);
    if (!matrix_hermitian_eigen_factor_complex128(matrix, eigenvalues, eigenvectors)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvectors: input matrix is not Hermitian", null);
    }
    TensorDType value_dtype = matrix.dtype == TENSOR_DTYPE_COMPLEX64 ? TENSOR_DTYPE_FLOAT32 : TENSOR_DTYPE_DOUBLE;
    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_uninitialized_checked(interp, value_dtype, value_shape_buf[0:1], "matrix/eigenvectors: failed to allocate eigenvalue tensor");
    if (values_value == null || values_value.tag == ERROR) return values_value;
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    if (values_value.tag != TENSOR || !tensor_has_valid_storage(values_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: eigenvalue tensor backing storage is missing");
    }
    if (value_dtype == TENSOR_DTYPE_FLOAT32) {
        float* out_values = (float*)values_value.tensor_val.data;
        for (usz i = 0; i < n; i++) out_values[i] = (float)eigenvalues[i];
    } else if (n > 0) {
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
    Value* vectors_value = make_tensor_uninitialized_checked(interp, matrix.dtype, vector_shape_buf[0:2], "matrix/eigenvectors: failed to allocate eigenvector tensor");
    if (vectors_value == null || vectors_value.tag == ERROR) return vectors_value;
    bool keep_vectors = false;
    defer if (!keep_vectors) {
        boundary_cleanup_materialized_value(null, vectors_value, interp.current_scope);
    }
    if (vectors_value.tag != TENSOR || !tensor_has_valid_storage(vectors_value.tensor_val)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: eigenvector tensor backing storage is missing");
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val* out_vectors = (Complex64Val*)vectors_value.tensor_val.data;
        for (usz i = 0; i < vector_count; i++) {
            out_vectors[i].real = (float)eigenvectors[i].real;
            out_vectors[i].imag = (float)eigenvectors[i].imag;
        }
    } else if (vector_count > 0) {
        usz vector_bytes = 0;
        if (!tensor_checked_mul_usz(vector_count, Complex128Val.sizeof, &vector_bytes)) {
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

fn bool matrix_copy_complex128_workspace(TensorVal* matrix, Complex128Val** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_COMPLEX128) return false;
    if (matrix.byte_len == 0) return true;
    if (matrix.data == null) return false;
    Complex128Val* work = (Complex128Val*)mem::malloc(matrix.byte_len);
    if (work == null) return false;
    mem::copy(work, matrix.data, matrix.byte_len);
    *out_work = work;
    return true;
}

fn bool matrix_copy_complex64_workspace(TensorVal* matrix, Complex64Val** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_COMPLEX64) return false;
    if (matrix.byte_len == 0) return true;
    if (matrix.data == null) return false;
    Complex64Val* work = (Complex64Val*)mem::malloc(matrix.byte_len);
    if (work == null) return false;
    mem::copy(work, matrix.data, matrix.byte_len);
    *out_work = work;
    return true;
}

fn bool matrix_copy_float32_to_double_workspace(TensorVal* matrix, double** out_work) {
    if (out_work == null) return false;
    *out_work = null;
    if (matrix == null || matrix.dtype != TENSOR_DTYPE_FLOAT32 || matrix.shape == null) return false;
    usz count = 0;
    if (!tensor_checked_mul_usz(matrix.shape[0], matrix.shape[1], &count)) return false;
    if (count > 0 && matrix.data == null) return false;
    if (!matrix_double_workspace_zero(count, out_work)) return false;
    float* source = (float*)matrix.data;
    for (usz i = 0; i < count; i++) (*out_work)[i] = (double)source[i];
    return true;
}

<*
@require interp != null
*>
fn Value* prim_matrix_eigenvalues(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenvalues", "1 argument: square symmetric/Hermitian Float64, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenvalues: input must be a square symmetric/Hermitian Tensor");

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
    if (
        matrix == null ||
        matrix.rank != 2 ||
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1] ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        )
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/eigenvalues: expected a square rank-2 Float64, Complex128, or Complex64 Tensor",
            null
        );
    }
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/eigenvalues: tensor backing storage is missing");
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128 || matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_hermitian_eigenvalues_cpu_value(matrix, interp);
    }
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

<*
@require interp != null
*>
fn Value* prim_matrix_eigenvectors(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenvectors", "1 argument: square symmetric/Hermitian Float64, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenvectors: input must be a square symmetric/Hermitian Tensor");

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
    if (
        matrix == null ||
        matrix.rank != 2 ||
        matrix.shape == null ||
        matrix.shape[0] != matrix.shape[1] ||
        (
            matrix.dtype != TENSOR_DTYPE_DOUBLE &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
            matrix.dtype != TENSOR_DTYPE_COMPLEX64
        )
    ) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/shape-mismatch",
            "matrix/eigenvectors: expected a square rank-2 Float64, Complex128, or Complex64 Tensor",
            null
        );
    }
    if (matrix.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: unsupported Tensor device", null);
    }
    if (!tensor_has_valid_storage(matrix)) return tensor_runtime_raise(interp, "matrix/eigenvectors: tensor backing storage is missing");
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128 || matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return matrix_hermitian_eigenvectors_cpu_value(matrix, interp);
    }
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

<*
@require interp != null
*>
fn Value* prim_matrix_eigenpairs(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return primitive_arity_raise(interp, "matrix/eigenpairs", "1 argument: square Float64, Float32, Complex128, or Complex64 Tensor", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "matrix/eigenpairs: input must be a square Float64, Float32, Complex128, or Complex64 Tensor");

    TensorVal* source = args[0].tensor_val;
    Value* concrete_value = source.kind == TENSOR_PAYLOAD_CONCRETE ? args[0] : tensor_realize_to_value(args[0], interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete = concrete_value != args[0];
    defer if (cleanup_concrete) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }
    Value* vulkan_result = matrix_eigenpairs_try_vulkan_value(concrete_value.tensor_val, interp);
    if (vulkan_result != null) return vulkan_result;

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
            "matrix/eigenpairs: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor",
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

    double* vector_real = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &vector_real)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvector real workspace", null);
    }
    defer if (vector_real != null) mem::free(vector_real);

    double* vector_imag = null;
    if (!matrix_double_workspace_zero(eigenvector_count, &vector_imag)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvector imaginary workspace", null);
    }
    defer if (vector_imag != null) mem::free(vector_imag);

    if (n > 0) {
        if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
            double* work = null;
            if (!matrix_copy_float64_workspace(matrix, &work)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate LAPACK workspace", null);
            }
            defer if (work != null) mem::free(work);
            double* raw_vectors = null;
            if (!matrix_double_workspace_zero(eigenvector_count, &raw_vectors)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate eigenvector workspace", null);
            }
            defer if (raw_vectors != null) mem::free(raw_vectors);
            int lapack_status = omni_tensor_backend_lapack_dgeev(n, work, real_parts, imag_parts, raw_vectors);
            switch (lapack_status) {
                case OMNI_TENSOR_LAPACK_SUCCESS:
                    if (!matrix_general_vectors_from_lapack(raw_vectors, imag_parts, n, vector_real, vector_imag)) {
                        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to build LAPACK eigenvectors");
                    }
                    break;
                case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                    if (!matrix_general_eigenpairs_pure(matrix, real_parts, imag_parts, vector_real, vector_imag)) {
                        return raise_error_with_payload_names(
                            interp,
                            "tensor",
                            "tensor/no-convergence",
                            "matrix/eigenpairs: failed to compute general eigenpairs",
                            null
                        );
                    }
                    break;
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
        } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
            float* work = null;
            if (!matrix_copy_float32_workspace(matrix, &work)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Float32 LAPACK workspace", null);
            }
            defer if (work != null) mem::free(work);
            float* real_parts_f32 = null;
            if (!matrix_float32_workspace_zero(n, &real_parts_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Float32 eigenvalue real workspace", null);
            }
            defer if (real_parts_f32 != null) mem::free(real_parts_f32);
            float* imag_parts_f32 = null;
            if (!matrix_float32_workspace_zero(n, &imag_parts_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Float32 eigenvalue imaginary workspace", null);
            }
            defer if (imag_parts_f32 != null) mem::free(imag_parts_f32);
            float* raw_vectors_f32 = null;
            if (!matrix_float32_workspace_zero(eigenvector_count, &raw_vectors_f32)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Float32 eigenvector workspace", null);
            }
            defer if (raw_vectors_f32 != null) mem::free(raw_vectors_f32);
            int lapack_status = omni_tensor_backend_lapack_sgeev(n, work, real_parts_f32, imag_parts_f32, raw_vectors_f32);
            switch (lapack_status) {
                case OMNI_TENSOR_LAPACK_SUCCESS:
                    for (usz i = 0; i < n; i++) {
                        real_parts[i] = (double)real_parts_f32[i];
                        imag_parts[i] = (double)imag_parts_f32[i];
                    }
                    if (!matrix_general_vectors_from_lapack_f32(raw_vectors_f32, imag_parts_f32, n, vector_real, vector_imag)) {
                        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to build Float32 LAPACK eigenvectors");
                    }
                    break;
                case OMNI_TENSOR_LAPACK_UNAVAILABLE: {
                    double* wide_work = null;
                    if (!matrix_copy_float32_to_double_workspace(matrix, &wide_work)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Float32 pure fallback workspace", null);
                    }
                    defer if (wide_work != null) mem::free(wide_work);
                    usz wide_byte_len = 0;
                    if (!tensor_checked_mul_usz(eigenvector_count, double.sizeof, &wide_byte_len)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: Float32 pure fallback workspace is too large", null);
                    }
                    TensorVal wide_matrix = *matrix;
                    wide_matrix.dtype = TENSOR_DTYPE_DOUBLE;
                    wide_matrix.data = wide_work;
                    wide_matrix.byte_len = wide_byte_len;
                    if (!matrix_general_eigenpairs_pure(&wide_matrix, real_parts, imag_parts, vector_real, vector_imag)) {
                        return raise_error_with_payload_names(
                            interp,
                            "tensor",
                            "tensor/no-convergence",
                            "matrix/eigenpairs: failed to compute Float32 general eigenpairs",
                            null
                        );
                    }
                    break;
                }
                case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/no-convergence",
                        "matrix/eigenpairs: failed to compute Float32 general eigenpairs",
                        null
                    );
                default:
                    return tensor_runtime_raise(interp, "matrix/eigenpairs: Float32 LAPACK backend rejected eigenpair input");
            }
        } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
            Complex128Val* work = null;
            if (!matrix_copy_complex128_workspace(matrix, &work)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex128 LAPACK workspace", null);
            }
            defer if (work != null) mem::free(work);
            Complex128Val* raw_values = null;
            if (!matrix_complex128_workspace_zero(n, &raw_values)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex128 eigenvalue workspace", null);
            }
            defer if (raw_values != null) mem::free(raw_values);
            Complex128Val* raw_vectors = null;
            if (!matrix_complex128_workspace_zero(eigenvector_count, &raw_vectors)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex128 eigenvector workspace", null);
            }
            defer if (raw_vectors != null) mem::free(raw_vectors);
            int lapack_status = omni_tensor_backend_lapack_zgeev(n, work, raw_values, raw_vectors);
            switch (lapack_status) {
                case OMNI_TENSOR_LAPACK_SUCCESS:
                    for (usz i = 0; i < n; i++) {
                        real_parts[i] = raw_values[i].real;
                        imag_parts[i] = raw_values[i].imag;
                    }
                    for (usz i = 0; i < eigenvector_count; i++) {
                        vector_real[i] = raw_vectors[i].real;
                        vector_imag[i] = raw_vectors[i].imag;
                    }
                    break;
                case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/backend-unavailable",
                        "matrix/eigenpairs: LAPACK zgeev is required for general Complex128 eigenpairs",
                        null
                    );
                case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/no-convergence",
                        "matrix/eigenpairs: failed to compute Complex128 general eigenpairs",
                        null
                    );
                default:
                    return tensor_runtime_raise(interp, "matrix/eigenpairs: Complex128 LAPACK backend rejected eigenpair input");
            }
        } else {
            Complex64Val* work = null;
            if (!matrix_copy_complex64_workspace(matrix, &work)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex64 LAPACK workspace", null);
            }
            defer if (work != null) mem::free(work);
            Complex64Val* raw_values = null;
            if (!matrix_complex64_workspace_zero(n, &raw_values)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex64 eigenvalue workspace", null);
            }
            defer if (raw_values != null) mem::free(raw_values);
            Complex64Val* raw_vectors = null;
            if (!matrix_complex64_workspace_zero(eigenvector_count, &raw_vectors)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to allocate Complex64 eigenvector workspace", null);
            }
            defer if (raw_vectors != null) mem::free(raw_vectors);
            int lapack_status = omni_tensor_backend_lapack_cgeev(n, work, raw_values, raw_vectors);
            switch (lapack_status) {
                case OMNI_TENSOR_LAPACK_SUCCESS:
                    for (usz i = 0; i < n; i++) {
                        real_parts[i] = (double)raw_values[i].real;
                        imag_parts[i] = (double)raw_values[i].imag;
                    }
                    for (usz i = 0; i < eigenvector_count; i++) {
                        vector_real[i] = (double)raw_vectors[i].real;
                        vector_imag[i] = (double)raw_vectors[i].imag;
                    }
                    break;
                case OMNI_TENSOR_LAPACK_UNAVAILABLE:
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/backend-unavailable",
                        "matrix/eigenpairs: LAPACK cgeev is required for general Complex64 eigenpairs",
                        null
                    );
                case OMNI_TENSOR_LAPACK_NO_CONVERGENCE:
                    return raise_error_with_payload_names(
                        interp,
                        "tensor",
                        "tensor/no-convergence",
                        "matrix/eigenpairs: failed to compute Complex64 general eigenpairs",
                        null
                    );
                default:
                    return tensor_runtime_raise(interp, "matrix/eigenpairs: Complex64 LAPACK backend rejected eigenpair input");
            }
        }
    }

    TensorDType result_dtype = (matrix.dtype == TENSOR_DTYPE_FLOAT32 || matrix.dtype == TENSOR_DTYPE_COMPLEX64)
        ? TENSOR_DTYPE_COMPLEX64
        : TENSOR_DTYPE_COMPLEX128;
    usz[1] value_shape_buf = { n };
    Value* values_value = make_tensor_uninitialized_checked(
        interp,
        result_dtype,
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
        result_dtype,
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

    bool built_eigenpairs = result_dtype == TENSOR_DTYPE_COMPLEX64
        ? matrix_fill_general_eigenpairs_from_vectors_complex64(
            n,
            real_parts,
            imag_parts,
            vector_real,
            vector_imag,
            values_value.tensor_val,
            vectors_value.tensor_val
        )
        : matrix_fill_general_eigenpairs_from_vectors_complex128(
            n,
            real_parts,
            imag_parts,
            vector_real,
            vector_imag,
            values_value.tensor_val,
            vectors_value.tensor_val
        );
    if (!built_eigenpairs) {
        return raise_error_with_payload_names(
            interp,
            "tensor",
            "tensor/out-of-memory",
            "matrix/eigenpairs: failed to build fixed-width complex eigenpair tensors",
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
