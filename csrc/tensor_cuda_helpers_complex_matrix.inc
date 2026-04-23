static int omni_tensor_backend_cuda_complex_matrix_launch_5(
    const void* input_device,
    size_t out_byte_len,
    size_t launch_count,
    size_t element_size,
    unsigned long long arg0,
    unsigned long long arg1,
    unsigned long long arg2,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (launch_count == 0) return out_byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        element_size == 0 ||
        launch_count > UINT32_MAX ||
        out_byte_len == 0 ||
        out_byte_len % element_size != 0) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(launch_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&arg0,
        (void*)&arg1,
        (void*)&arg2,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_matrix_launch_4(
    const void* input_device,
    size_t out_byte_len,
    size_t launch_count,
    size_t element_size,
    unsigned long long arg0,
    unsigned long long arg1,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (launch_count == 0) return out_byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        element_size == 0 ||
        launch_count > UINT32_MAX ||
        out_byte_len == 0 ||
        out_byte_len % element_size != 0) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(launch_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&arg0,
        (void*)&arg1,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_matrix_trace_launch(
    const void* input_device,
    size_t element_size,
    unsigned long long n,
    void* function,
    void* host_out
) {
    if (host_out == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (input_device == NULL || function == NULL || element_size == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(element_size, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    void* params[] = {
        (void*)&input_device,
        (void*)&n,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(out_device, element_size, host_out);
    omni_tensor_backend_cuda_free(out_device);
    return copy_status;
}

int omni_tensor_backend_cuda_transpose_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > UINT32_MAX ||
        element_count > ((size_t)-1) / element_size ||
        byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_transpose_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_transpose_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > UINT32_MAX ||
        element_count > ((size_t)-1) / element_size ||
        byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_transpose_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (diagonal_count > UINT32_MAX || diagonal_count > ((size_t)-1) / element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        diagonal_count * element_size,
        diagonal_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)diagonal_count,
        omni_tensor_cuda_diagonal_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (diagonal_count > UINT32_MAX || diagonal_count > ((size_t)-1) / element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        diagonal_count * element_size,
        diagonal_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)diagonal_count,
        omni_tensor_cuda_diagonal_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_matrix_complex128(
    const void* input_device,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t output_count = n * n;
    if (n > ((size_t)-1) / element_size || byte_len != n * element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (output_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (output_count > UINT32_MAX || output_count > ((size_t)-1) / element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_4(
        input_device,
        output_count * element_size,
        output_count,
        element_size,
        (unsigned long long)n,
        (unsigned long long)output_count,
        omni_tensor_cuda_diagonal_matrix_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_matrix_complex64(
    const void* input_device,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t output_count = n * n;
    if (n > ((size_t)-1) / element_size || byte_len != n * element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (output_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (output_count > UINT32_MAX || output_count > ((size_t)-1) / element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_4(
        input_device,
        output_count * element_size,
        output_count,
        element_size,
        (unsigned long long)n,
        (unsigned long long)output_count,
        omni_tensor_cuda_diagonal_matrix_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_trace_complex128(
    const void* input_device,
    size_t byte_len,
    size_t n,
    double* out_real,
    double* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_real = 0.0;
    *out_imag = 0.0;
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    OmniTensorCudaComplex128 trace = { 0.0, 0.0 };
    int status = omni_tensor_backend_cuda_complex_matrix_trace_launch(
        input_device,
        element_size,
        (unsigned long long)n,
        omni_tensor_cuda_trace_complex128_function,
        &trace
    );
    if (status == OMNI_TENSOR_CUDA_SUCCESS) {
        *out_real = trace.real;
        *out_imag = trace.imag;
    }
    return status;
}

int omni_tensor_backend_cuda_trace_complex64(
    const void* input_device,
    size_t byte_len,
    size_t n,
    float* out_real,
    float* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_real = 0.0f;
    *out_imag = 0.0f;
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    OmniTensorCudaComplex64 trace = { 0.0f, 0.0f };
    int status = omni_tensor_backend_cuda_complex_matrix_trace_launch(
        input_device,
        element_size,
        (unsigned long long)n,
        omni_tensor_cuda_trace_complex64_function,
        &trace
    );
    if (status == OMNI_TENSOR_CUDA_SUCCESS) {
        *out_real = trace.real;
        *out_imag = trace.imag;
    }
    return status;
}

static int omni_tensor_backend_cuda_complex_matrix_shape_bytes(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t element_size,
    size_t* out_element_count
) {
    if (out_element_count == NULL || element_size == 0) return OMNI_TENSOR_CUDA_INVALID;
    *out_element_count = 0;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > UINT32_MAX ||
        element_count > ((size_t)-1) / element_size ||
        byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    *out_element_count = element_count;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_svd_input_column_major_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_input_column_major_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_input_column_major_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_input_column_major_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_adjoint_input_column_major_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_adjoint_input_column_major_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_u_row_major_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t k,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        k,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)k,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_u_row_major_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_u_row_major_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t k,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        rows,
        k,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)k,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_u_row_major_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_v_from_vt_complex128(
    const void* input_device,
    size_t byte_len,
    size_t k,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        k,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)k,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_v_from_vt_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_v_from_vt_complex64(
    const void* input_device,
    size_t byte_len,
    size_t k,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    size_t element_count = 0;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    int shape_status = omni_tensor_backend_cuda_complex_matrix_shape_bytes(
        byte_len,
        k,
        cols,
        element_size,
        &element_count
    );
    if (shape_status != OMNI_TENSOR_CUDA_SUCCESS || element_count == 0) return shape_status;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)k,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_svd_v_from_vt_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_svd_adapter_shape_guard_for_tests(void) {
    void* out_device = (void*)1;
    int status = omni_tensor_backend_cuda_svd_input_column_major_complex128(
        (const void*)1,
        sizeof(OmniTensorCudaComplex128),
        (size_t)-1,
        2,
        &out_device
    );
    if (status != OMNI_TENSOR_CUDA_INVALID || out_device != NULL) return OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    out_device = (void*)1;
    status = omni_tensor_backend_cuda_svd_input_column_major_complex64(
        (const void*)1,
        1,
        1,
        1,
        &out_device
    );
    if (status != OMNI_TENSOR_CUDA_INVALID || out_device != NULL) return OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    out_device = (void*)1;
    status = omni_tensor_backend_cuda_svd_u_row_major_complex128(
        (const void*)1,
        sizeof(OmniTensorCudaComplex128),
        (size_t)-1,
        2,
        &out_device
    );
    if (status != OMNI_TENSOR_CUDA_INVALID || out_device != NULL) return OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    out_device = (void*)1;
    status = omni_tensor_backend_cuda_svd_v_from_vt_complex64(
        (const void*)1,
        sizeof(OmniTensorCudaComplex64),
        (size_t)-1,
        2,
        &out_device
    );
    if (status != OMNI_TENSOR_CUDA_INVALID || out_device != NULL) return OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    out_device = (void*)1;
    status = omni_tensor_backend_cuda_svd_adjoint_input_column_major_complex128(
        (const void*)1,
        sizeof(OmniTensorCudaComplex128),
        (size_t)-1,
        2,
        &out_device
    );
    if (status != OMNI_TENSOR_CUDA_INVALID || out_device != NULL) return OMNI_TENSOR_CUDA_EXECUTION_FAILED;

    return OMNI_TENSOR_CUDA_SUCCESS;
}
