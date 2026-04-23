module lisp;

import std::core::mem;
import main;

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
    Value* cuda_result = matrix_norm_try_cuda_value(matrix, kind, interp);
    if (cuda_result != null) return cuda_result;
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
    if (
        matrix.dtype != TENSOR_DTYPE_DOUBLE &&
        matrix.dtype != TENSOR_DTYPE_FLOAT32 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX128 &&
        matrix.dtype != TENSOR_DTYPE_COMPLEX64
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/svd: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "matrix/svd: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/svd: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/svd: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    usz rows = matrix.shape[0];
    usz cols = matrix.shape[1];
    usz k = matrix_min_usz(rows, cols);
    void* u_device = null;
    void* s_device = null;
    void* v_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_svd_f64(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_svd_f32(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_svd_complex128(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        );
    } else if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_svd_complex64(
            matrix.device_handle,
            matrix.byte_len,
            rows,
            cols,
            &u_device,
            &s_device,
            &v_device
        );
    }
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
    TensorDType s_dtype = matrix.dtype;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) s_dtype = TENSOR_DTYPE_DOUBLE;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) s_dtype = TENSOR_DTYPE_FLOAT32;
    TensorVal* s_tensor = tensor_alloc_payload_base_checked(s_dtype, s_shape_buf[0:1], false, false, "matrix/svd: failed to allocate Vulkan singular value tensor metadata");
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
    if (!matrix_vulkan_hermitian_eigen_supported_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports Float64, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports square dense row-major Float64, Complex128, or Complex64 tensors", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/eigenvalues: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvalues: Vulkan currently supports dense row-major Float64, Complex128, or Complex64 tensors", null);
    }
    usz n = matrix.shape[0];
    void* values_device = null;
    void* vectors_device = null;
    int status = matrix_vulkan_hermitian_eigen_call(matrix, &values_device, &vectors_device);
    if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
    if (status == OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvalues: input matrix is not symmetric/Hermitian", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_vulkan_status_raise(interp, status, "matrix/eigenvalues: Vulkan Hermitian eigen kernel failed");
    }
    usz[1] value_shape_buf = { n };
    TensorDType value_dtype = matrix_vulkan_hermitian_eigen_value_dtype(matrix.dtype);
    TensorVal* tensor = tensor_alloc_payload_base_checked(value_dtype, value_shape_buf[0:1], false, false, "matrix/eigenvalues: failed to allocate Vulkan eigenvalue tensor metadata");
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
