module lisp;

import std::core::mem;
import main;

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
