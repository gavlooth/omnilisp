module lisp;

import std::core::mem;
import main;

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
