module lisp;

import std::core::mem;
import main;

fn bool matrix_vulkan_hermitian_eigen_supported_dtype(TensorDType dtype) {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_COMPLEX128 || dtype == TENSOR_DTYPE_COMPLEX64;
}

fn TensorDType matrix_vulkan_hermitian_eigen_value_dtype(TensorDType dtype) {
    return dtype == TENSOR_DTYPE_COMPLEX64 ? TENSOR_DTYPE_FLOAT32 : TENSOR_DTYPE_DOUBLE;
}

fn int matrix_vulkan_hermitian_eigen_call(TensorVal* matrix, void** values_device, void** vectors_device) {
    if (matrix == null || values_device == null || vectors_device == null) return OMNI_TENSOR_VULKAN_INVALID;
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        return omni_tensor_backend_vulkan_symmetric_eigen_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        return omni_tensor_backend_vulkan_hermitian_eigen_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return omni_tensor_backend_vulkan_hermitian_eigen_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    return OMNI_TENSOR_VULKAN_UNSUPPORTED;
}

fn bool matrix_vulkan_general_eigen_supported_dtype(TensorDType dtype) {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32 || dtype == TENSOR_DTYPE_COMPLEX128 || dtype == TENSOR_DTYPE_COMPLEX64;
}

fn int matrix_vulkan_general_eigen_call(TensorVal* matrix, void** values_device, void** vectors_device) {
    if (matrix == null || values_device == null || vectors_device == null) return OMNI_TENSOR_VULKAN_INVALID;
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX128) {
        return omni_tensor_backend_vulkan_general_eigen_complex128(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    if (matrix.dtype == TENSOR_DTYPE_COMPLEX64) {
        return omni_tensor_backend_vulkan_general_eigen_complex64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    if (matrix.dtype == TENSOR_DTYPE_DOUBLE) {
        return omni_tensor_backend_vulkan_general_eigen_f64(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    if (matrix.dtype == TENSOR_DTYPE_FLOAT32) {
        return omni_tensor_backend_vulkan_general_eigen_f32(
            matrix.device_handle,
            matrix.byte_len,
            matrix.shape[0],
            values_device,
            vectors_device
        );
    }
    return OMNI_TENSOR_VULKAN_UNSUPPORTED;
}

fn Value* matrix_eigenpairs_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!matrix_vulkan_general_eigen_supported_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenpairs: Vulkan currently supports Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenpairs: Vulkan currently supports square dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/eigenpairs: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenpairs: Vulkan currently supports dense row-major Float64, Float32, Complex128, or Complex64 tensors", null);
    }
    usz n = matrix.shape[0];
    void* values_device = null;
    void* vectors_device = null;
    int status = matrix_vulkan_general_eigen_call(matrix, &values_device, &vectors_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_vulkan_status_raise(interp, status, "matrix/eigenpairs: Vulkan general eigen kernel failed");
    }
    if (n > 0 && (values_device == null || vectors_device == null)) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_runtime_raise(interp, "matrix/eigenpairs: Vulkan general eigen kernel returned missing output storage");
    }
    TensorDType result_dtype = (matrix.dtype == TENSOR_DTYPE_FLOAT32 || matrix.dtype == TENSOR_DTYPE_COMPLEX64)
        ? TENSOR_DTYPE_COMPLEX64
        : TENSOR_DTYPE_COMPLEX128;
    usz[1] value_shape_buf = { n };
    TensorVal* values_tensor = tensor_alloc_payload_base_checked(result_dtype, value_shape_buf[0:1], false, false, "matrix/eigenpairs: failed to allocate Vulkan eigenvalue tensor metadata");
    if (values_tensor == null) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to allocate Vulkan eigenvalue tensor metadata");
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
        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to allocate Vulkan eigenvalue tensor value");
    }
    values_value.tag = TENSOR;
    values_value.tensor_val = values_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)values_value, &scope_dtor_value);
    bool keep_values = false;
    defer if (!keep_values) {
        boundary_cleanup_materialized_value(null, values_value, interp.current_scope);
    }
    usz[2] vector_shape_buf = { n, n };
    TensorVal* vectors_tensor = tensor_alloc_payload_base_checked(result_dtype, vector_shape_buf[0:2], false, false, "matrix/eigenpairs: failed to allocate Vulkan eigenvector tensor metadata");
    if (vectors_tensor == null) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to allocate Vulkan eigenvector tensor metadata");
    }
    vectors_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    vectors_tensor.device = TENSOR_DEVICE_VULKAN;
    vectors_tensor.device_handle = vectors_device;
    vectors_tensor.device_finalizer = vectors_device != null ? &tensor_vulkan_device_finalizer : null;
    vectors_device = null;
    Value* vectors_value = interp.alloc_value();
    if (vectors_value == null) {
        tensor_free_payload(vectors_tensor);
        return tensor_runtime_raise(interp, "matrix/eigenpairs: failed to allocate Vulkan eigenvector tensor value");
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
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to promote Vulkan eigenvalue tensor", null);
    }
    if (values_for_dict.tag == ERROR) return values_for_dict;
    keep_values = values_for_dict == values_value;
    Value* vectors_for_dict = boundary_promote_to_root(vectors_value, interp);
    if (vectors_for_dict == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "matrix/eigenpairs: failed to promote Vulkan eigenvector tensor", null);
    }
    if (vectors_for_dict.tag == ERROR) return vectors_for_dict;
    keep_vectors = vectors_for_dict == vectors_value;
    Value* out = make_hashmap_checked(interp, 8, "matrix/eigenpairs: failed to allocate Vulkan result dictionary");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != HASHMAP || out.hashmap_val == null) {
        return tensor_runtime_raise(interp, "matrix/eigenpairs: Vulkan result dictionary backing storage is missing");
    }
    if (Value* err = explain_dict_set(out, interp, "values", values_for_dict)) return err;
    if (Value* err = explain_dict_set(out, interp, "vectors", vectors_for_dict)) return err;
    keep_values = true;
    keep_vectors = true;
    return out;
}

fn Value* matrix_eigenvectors_try_vulkan_value(TensorVal* matrix, Interp* interp) {
    if (matrix == null || matrix.device != TENSOR_DEVICE_VULKAN) return null;
    if (!matrix_vulkan_hermitian_eigen_supported_dtype(matrix.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports Float64, Complex128, or Complex64 Tensor storage", null);
    }
    if (matrix.rank != 2 || matrix.shape == null || matrix.shape[0] != matrix.shape[1]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports square dense row-major Float64, Complex128, or Complex64 tensors", null);
    }
    if (!tensor_has_valid_device_storage(matrix, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "matrix/eigenvectors: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(matrix)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "matrix/eigenvectors: Vulkan currently supports dense row-major Float64, Complex128, or Complex64 tensors", null);
    }
    usz n = matrix.shape[0];
    void* values_device = null;
    void* vectors_device = null;
    int status = matrix_vulkan_hermitian_eigen_call(matrix, &values_device, &vectors_device);
    if (status == OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return raise_error_with_payload_names(interp, "tensor", "tensor/not-symmetric", "matrix/eigenvectors: input matrix is not symmetric/Hermitian", null);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (vectors_device != null) omni_tensor_backend_vulkan_free(vectors_device);
        if (values_device != null) omni_tensor_backend_vulkan_free(values_device);
        return tensor_vulkan_status_raise(interp, status, "matrix/eigenvectors: Vulkan Hermitian eigen kernel failed");
    }
    usz[1] value_shape_buf = { n };
    TensorDType value_dtype = matrix_vulkan_hermitian_eigen_value_dtype(matrix.dtype);
    TensorVal* values_tensor = tensor_alloc_payload_base_checked(value_dtype, value_shape_buf[0:1], false, false, "matrix/eigenvectors: failed to allocate Vulkan eigenvalue tensor metadata");
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
    TensorVal* vectors_tensor = tensor_alloc_payload_base_checked(matrix.dtype, vector_shape_buf[0:2], false, false, "matrix/eigenvectors: failed to allocate Vulkan eigenvector tensor metadata");
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
