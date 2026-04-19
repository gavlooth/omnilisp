int omni_tensor_backend_cublas_dgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const double alpha = 1.0;
    const double beta = 0.0;
    int status = omni_cublas_dgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const double*)matrix_device,
        (int)matrix_leading_dim,
        (const double*)vector_device,
        1,
        &beta,
        (double*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_sgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_float32_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const float alpha = 1.0f;
    const float beta = 0.0f;
    int status = omni_cublas_sgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const float*)matrix_device,
        (int)matrix_leading_dim,
        (const float*)vector_device,
        1,
        &beta,
        (float*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_zgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex128 alpha = { 1.0, 0.0 };
    const OmniTensorCudaComplex128 beta = { 0.0, 0.0 };
    int status = omni_cublas_zgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const OmniTensorCudaComplex128*)matrix_device,
        (int)matrix_leading_dim,
        (const OmniTensorCudaComplex128*)vector_device,
        1,
        &beta,
        (OmniTensorCudaComplex128*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_cgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex64 alpha = { 1.0f, 0.0f };
    const OmniTensorCudaComplex64 beta = { 0.0f, 0.0f };
    int status = omni_cublas_cgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const OmniTensorCudaComplex64*)matrix_device,
        (int)matrix_leading_dim,
        (const OmniTensorCudaComplex64*)vector_device,
        1,
        &beta,
        (OmniTensorCudaComplex64*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

void omni_tensor_backend_cuda_free(void* device_ptr) {
    if (device_ptr == NULL) return;
    if (!omni_tensor_cuda_resolve()) return;
    omni_cuda_free(device_ptr);
}

int omni_tensor_backend_cublas_dgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const double alpha = 1.0;
    const double beta = 0.0;
    int status = omni_cublas_dgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const double*)right_device,
        (int)right_leading_dim,
        (const double*)left_device,
        (int)left_leading_dim,
        &beta,
        (double*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_sgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_float32_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const float alpha = 1.0f;
    const float beta = 0.0f;
    int status = omni_cublas_sgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const float*)right_device,
        (int)right_leading_dim,
        (const float*)left_device,
        (int)left_leading_dim,
        &beta,
        (float*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_zgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex128 alpha = { 1.0, 0.0 };
    const OmniTensorCudaComplex128 beta = { 0.0, 0.0 };
    int status = omni_cublas_zgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const OmniTensorCudaComplex128*)right_device,
        (int)right_leading_dim,
        (const OmniTensorCudaComplex128*)left_device,
        (int)left_leading_dim,
        &beta,
        (OmniTensorCudaComplex128*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_cgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex64 alpha = { 1.0f, 0.0f };
    const OmniTensorCudaComplex64 beta = { 0.0f, 0.0f };
    int status = omni_cublas_cgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const OmniTensorCudaComplex64*)right_device,
        (int)right_leading_dim,
        (const OmniTensorCudaComplex64*)left_device,
        (int)left_leading_dim,
        &beta,
        (OmniTensorCudaComplex64*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}
