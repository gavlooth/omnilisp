static int omni_tensor_backend_cuda_svd_complex_shape(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t element_size,
    int* out_m,
    int* out_n,
    size_t* out_k
) {
    if (out_m == NULL || out_n == NULL || out_k == NULL || element_size == 0) return OMNI_TENSOR_CUDA_INVALID;
    *out_m = 0;
    *out_n = 0;
    *out_k = 0;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    if (element_count == 0) {
        if (byte_len != 0) return OMNI_TENSOR_CUDA_INVALID;
        return OMNI_TENSOR_CUDA_SUCCESS;
    }
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    size_t m = rows >= cols ? rows : cols;
    size_t n = rows >= cols ? cols : rows;
    if (m > (size_t)INT_MAX || n > (size_t)INT_MAX) return OMNI_TENSOR_CUDA_INVALID;
    *out_m = (int)m;
    *out_n = (int)n;
    *out_k = n;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static void omni_tensor_backend_cuda_free_if_not_null(void* device_ptr) {
    if (device_ptr != NULL) omni_tensor_backend_cuda_free(device_ptr);
}

int omni_tensor_backend_cuda_svd_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    int m = 0;
    int n = 0;
    size_t k = 0;
    int shape_status = omni_tensor_backend_cuda_svd_complex_shape(
        byte_len,
        rows,
        cols,
        sizeof(OmniTensorCudaComplex128),
        &m,
        &n,
        &k
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || k == 0) return shape_status;
    if (input_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cusolver_complex_svd_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    int tall_or_square = rows >= cols;
    void* a_device = NULL;
    int status = tall_or_square
        ? omni_tensor_backend_cuda_svd_input_column_major_complex128(input_device, byte_len, rows, cols, &a_device)
        : omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex128(input_device, byte_len, rows, cols, &a_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;

    void* handle = NULL;
    void* s_device = NULL;
    void* u_raw_device = NULL;
    void* vt_raw_device = NULL;
    void* u_public_device = NULL;
    void* v_public_device = NULL;
    void* work_device = NULL;
    void* dev_info_device = NULL;
    int lwork = 0;
    int dev_info = 0;
    int result = OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    if (omni_cusolver_create(&handle) != OMNI_CUSOLVER_STATUS_SUCCESS || handle == NULL) goto cleanup;
    if (omni_cusolver_dn_zgesvd_buffer_size(handle, m, n, &lwork) != OMNI_CUSOLVER_STATUS_SUCCESS || lwork <= 0) goto cleanup;
    if ((size_t)lwork > ((size_t)-1) / sizeof(OmniTensorCudaComplex128)) goto cleanup;

    size_t s_byte_len = k * sizeof(double);
    size_t u_raw_byte_len = (size_t)m * k * sizeof(OmniTensorCudaComplex128);
    size_t vt_raw_byte_len = k * (size_t)n * sizeof(OmniTensorCudaComplex128);
    status = omni_tensor_backend_cuda_alloc(s_byte_len, &s_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(u_raw_byte_len, &u_raw_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(vt_raw_byte_len, &vt_raw_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc((size_t)lwork * sizeof(OmniTensorCudaComplex128), &work_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(sizeof(int), &dev_info_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }

    int cusolver_status = omni_cusolver_dn_zgesvd(
        handle,
        (signed char)'S',
        (signed char)'S',
        m,
        n,
        (OmniTensorCudaComplex128*)a_device,
        m,
        (double*)s_device,
        (OmniTensorCudaComplex128*)u_raw_device,
        m,
        (OmniTensorCudaComplex128*)vt_raw_device,
        n,
        (OmniTensorCudaComplex128*)work_device,
        lwork,
        NULL,
        (int*)dev_info_device
    );
    omni_tensor_cusolver_zgesvd_call_count++;
    if (cusolver_status != OMNI_CUSOLVER_STATUS_SUCCESS) goto cleanup;
    status = omni_tensor_backend_cuda_copy_to_host(dev_info_device, sizeof(int), &dev_info);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    if (dev_info != 0) goto cleanup;

    status = tall_or_square
        ? omni_tensor_backend_cuda_svd_u_row_major_complex128(u_raw_device, u_raw_byte_len, rows, k, &u_public_device)
        : omni_tensor_backend_cuda_svd_v_from_vt_complex128(vt_raw_device, vt_raw_byte_len, k, rows, &u_public_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = tall_or_square
        ? omni_tensor_backend_cuda_svd_v_from_vt_complex128(vt_raw_device, vt_raw_byte_len, k, cols, &v_public_device)
        : omni_tensor_backend_cuda_svd_u_row_major_complex128(u_raw_device, u_raw_byte_len, cols, k, &v_public_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }

    *out_u_device_ptr = u_public_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_public_device;
    u_public_device = NULL;
    s_device = NULL;
    v_public_device = NULL;
    result = OMNI_TENSOR_CUDA_SUCCESS;

cleanup:
    omni_tensor_backend_cuda_free_if_not_null(a_device);
    omni_tensor_backend_cuda_free_if_not_null(u_raw_device);
    omni_tensor_backend_cuda_free_if_not_null(vt_raw_device);
    omni_tensor_backend_cuda_free_if_not_null(work_device);
    omni_tensor_backend_cuda_free_if_not_null(dev_info_device);
    omni_tensor_backend_cuda_free_if_not_null(s_device);
    omni_tensor_backend_cuda_free_if_not_null(u_public_device);
    omni_tensor_backend_cuda_free_if_not_null(v_public_device);
    if (handle != NULL) omni_cusolver_destroy(handle);
    return result;
}

int omni_tensor_backend_cuda_svd_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    int m = 0;
    int n = 0;
    size_t k = 0;
    int shape_status = omni_tensor_backend_cuda_svd_complex_shape(
        byte_len,
        rows,
        cols,
        sizeof(OmniTensorCudaComplex64),
        &m,
        &n,
        &k
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || k == 0) return shape_status;
    if (input_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cusolver_complex_svd_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    int tall_or_square = rows >= cols;
    void* a_device = NULL;
    int status = tall_or_square
        ? omni_tensor_backend_cuda_svd_input_column_major_complex64(input_device, byte_len, rows, cols, &a_device)
        : omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex64(input_device, byte_len, rows, cols, &a_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;

    void* handle = NULL;
    void* s_device = NULL;
    void* u_raw_device = NULL;
    void* vt_raw_device = NULL;
    void* u_public_device = NULL;
    void* v_public_device = NULL;
    void* work_device = NULL;
    void* dev_info_device = NULL;
    int lwork = 0;
    int dev_info = 0;
    int result = OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    if (omni_cusolver_create(&handle) != OMNI_CUSOLVER_STATUS_SUCCESS || handle == NULL) goto cleanup;
    if (omni_cusolver_dn_cgesvd_buffer_size(handle, m, n, &lwork) != OMNI_CUSOLVER_STATUS_SUCCESS || lwork <= 0) goto cleanup;
    if ((size_t)lwork > ((size_t)-1) / sizeof(OmniTensorCudaComplex64)) goto cleanup;

    size_t s_byte_len = k * sizeof(float);
    size_t u_raw_byte_len = (size_t)m * k * sizeof(OmniTensorCudaComplex64);
    size_t vt_raw_byte_len = k * (size_t)n * sizeof(OmniTensorCudaComplex64);
    status = omni_tensor_backend_cuda_alloc(s_byte_len, &s_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(u_raw_byte_len, &u_raw_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(vt_raw_byte_len, &vt_raw_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc((size_t)lwork * sizeof(OmniTensorCudaComplex64), &work_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = omni_tensor_backend_cuda_alloc(sizeof(int), &dev_info_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }

    int cusolver_status = omni_cusolver_dn_cgesvd(
        handle,
        (signed char)'S',
        (signed char)'S',
        m,
        n,
        (OmniTensorCudaComplex64*)a_device,
        m,
        (float*)s_device,
        (OmniTensorCudaComplex64*)u_raw_device,
        m,
        (OmniTensorCudaComplex64*)vt_raw_device,
        n,
        (OmniTensorCudaComplex64*)work_device,
        lwork,
        NULL,
        (int*)dev_info_device
    );
    omni_tensor_cusolver_cgesvd_call_count++;
    if (cusolver_status != OMNI_CUSOLVER_STATUS_SUCCESS) goto cleanup;
    status = omni_tensor_backend_cuda_copy_to_host(dev_info_device, sizeof(int), &dev_info);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    if (dev_info != 0) goto cleanup;

    status = tall_or_square
        ? omni_tensor_backend_cuda_svd_u_row_major_complex64(u_raw_device, u_raw_byte_len, rows, k, &u_public_device)
        : omni_tensor_backend_cuda_svd_v_from_vt_complex64(vt_raw_device, vt_raw_byte_len, k, rows, &u_public_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }
    status = tall_or_square
        ? omni_tensor_backend_cuda_svd_v_from_vt_complex64(vt_raw_device, vt_raw_byte_len, k, cols, &v_public_device)
        : omni_tensor_backend_cuda_svd_u_row_major_complex64(u_raw_device, u_raw_byte_len, cols, k, &v_public_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) { result = status; goto cleanup; }

    *out_u_device_ptr = u_public_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_public_device;
    u_public_device = NULL;
    s_device = NULL;
    v_public_device = NULL;
    result = OMNI_TENSOR_CUDA_SUCCESS;

cleanup:
    omni_tensor_backend_cuda_free_if_not_null(a_device);
    omni_tensor_backend_cuda_free_if_not_null(u_raw_device);
    omni_tensor_backend_cuda_free_if_not_null(vt_raw_device);
    omni_tensor_backend_cuda_free_if_not_null(work_device);
    omni_tensor_backend_cuda_free_if_not_null(dev_info_device);
    omni_tensor_backend_cuda_free_if_not_null(s_device);
    omni_tensor_backend_cuda_free_if_not_null(u_public_device);
    omni_tensor_backend_cuda_free_if_not_null(v_public_device);
    if (handle != NULL) omni_cusolver_destroy(handle);
    return result;
}
