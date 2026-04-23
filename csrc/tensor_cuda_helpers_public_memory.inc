int omni_tensor_backend_cuda_available(void) {
    if (!omni_tensor_cuda_resolve()) return 0;
    int count = 0;
    if (omni_cuda_get_device_count(&count) != 0) return 0;
    if (count <= 0) return 0;

    void* probe = NULL;
    if (omni_cuda_malloc(&probe, 1) != 0 || probe == NULL) return 0;
    omni_cuda_free(probe);
    return 1;
}

void omni_tensor_backend_cuda_disable_for_tests(int disabled) {
    omni_tensor_cuda_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_cublas_disable_for_tests(int disabled) {
    omni_tensor_cublas_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_cusolver_disable_for_tests(int disabled) {
    omni_tensor_cusolver_disabled_for_tests = disabled != 0;
}

int omni_tensor_backend_cublas_available(void) {
    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cublas_resolve()) return 0;
    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) return 0;
    int status = omni_cublas_destroy(handle);
    return status == OMNI_CUBLAS_STATUS_SUCCESS ? 1 : 0;
}

int omni_tensor_backend_cublas_float32_available(void) {
    if (!omni_tensor_backend_cublas_available()) return 0;
    return omni_cublas_sgemm != NULL && omni_cublas_sgemv != NULL ? 1 : 0;
}

int omni_tensor_backend_cublas_complex_available(void) {
    if (!omni_tensor_backend_cublas_available()) return 0;
    return omni_cublas_zgemm != NULL &&
        omni_cublas_cgemm != NULL &&
        omni_cublas_zgemv != NULL &&
        omni_cublas_cgemv != NULL ? 1 : 0;
}

int omni_tensor_backend_cusolver_available(void) {
    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cusolver_resolve()) return 0;
    void* handle = NULL;
    if (omni_cusolver_create(&handle) != OMNI_CUSOLVER_STATUS_SUCCESS || handle == NULL) return 0;
    int status = omni_cusolver_destroy(handle);
    return status == OMNI_CUSOLVER_STATUS_SUCCESS ? 1 : 0;
}

int omni_tensor_backend_cusolver_complex_svd_available(void) {
    if (!omni_tensor_backend_cusolver_available()) return 0;
    return omni_cusolver_dn_zgesvd_buffer_size != NULL &&
        omni_cusolver_dn_cgesvd_buffer_size != NULL &&
        omni_cusolver_dn_zgesvd != NULL &&
        omni_cusolver_dn_cgesvd != NULL ? 1 : 0;
}

long omni_tensor_backend_cusolver_zgesvd_call_count(void) {
    return omni_tensor_cusolver_zgesvd_call_count;
}

long omni_tensor_backend_cusolver_cgesvd_call_count(void) {
    return omni_tensor_cusolver_cgesvd_call_count;
}

int omni_tensor_backend_cuda_map_available(void) {
    return omni_tensor_cuda_map_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_scientific_map_available(void) {
    return omni_tensor_cuda_map_scientific_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_complex_map_available(void) {
    return omni_tensor_cuda_complex_map_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_complex_matrix_available(void) {
    return omni_tensor_cuda_complex_matrix_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_rounding_i64_available(void) {
    return omni_tensor_cuda_round_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_alloc(size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    void* device_ptr = NULL;
    if (omni_cuda_malloc(&device_ptr, byte_len) != 0 || device_ptr == NULL) {
        return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    }
    *out_device_ptr = device_ptr;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_device(const void* host_ptr, size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (host_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;

    void* device_ptr = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &device_ptr);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;
    if (omni_cuda_memcpy(device_ptr, host_ptr, byte_len, OMNI_CUDA_MEMCPY_HOST_TO_DEVICE) != 0) {
        omni_cuda_free(device_ptr);
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    *out_device_ptr = device_ptr;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_host(const void* device_ptr, size_t byte_len, void* host_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(host_ptr, device_ptr, byte_len, OMNI_CUDA_MEMCPY_DEVICE_TO_HOST) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_existing_device(const void* host_ptr, size_t byte_len, void* device_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (host_ptr == NULL || device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(device_ptr, host_ptr, byte_len, OMNI_CUDA_MEMCPY_HOST_TO_DEVICE) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_device_to_existing_device(const void* source_device_ptr, size_t byte_len, void* dest_device_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (source_device_ptr == NULL || dest_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (source_device_ptr == dest_device_ptr) return OMNI_TENSOR_CUDA_SUCCESS;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(dest_device_ptr, source_device_ptr, byte_len, OMNI_CUDA_MEMCPY_DEVICE_TO_DEVICE) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_null_alias_guard_for_tests(void) {
    return omni_tensor_backend_cuda_copy_device_to_existing_device(NULL, 1u, NULL);
}

int omni_tensor_backend_cuda_fill_f64(void* device_ptr, size_t element_count, double value) {
    if (element_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || element_count > ((size_t)-1) / sizeof(double)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(double);
    double* host_values = (double*)malloc(byte_len);
    if (host_values == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    for (size_t i = 0; i < element_count; i++) {
        host_values[i] = value;
    }
    int status = omni_tensor_backend_cuda_copy_to_existing_device(host_values, byte_len, device_ptr);
    free(host_values);
    return status;
}

int omni_tensor_backend_cuda_fill_f32(void* device_ptr, size_t element_count, float value) {
    if (element_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || element_count > ((size_t)-1) / sizeof(float)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(float);
    float* host_values = (float*)malloc(byte_len);
    if (host_values == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    for (size_t i = 0; i < element_count; i++) {
        host_values[i] = value;
    }
    int status = omni_tensor_backend_cuda_copy_to_existing_device(host_values, byte_len, device_ptr);
    free(host_values);
    return status;
}
