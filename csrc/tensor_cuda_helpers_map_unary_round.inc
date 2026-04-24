static int omni_tensor_backend_cuda_map_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    unsigned int min_op,
    unsigned int max_op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < min_op ||
        op > max_op ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
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

static int omni_tensor_backend_cuda_map_scientific_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < 5 ||
        op > 20 ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
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
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return copy_status;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t input_element_size,
    size_t output_element_size,
    unsigned int op,
    unsigned int min_op,
    unsigned int max_op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < min_op ||
        op > max_op ||
        input_element_size == 0 ||
        output_element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / input_element_size || byte_len != element_count * input_element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / output_element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    size_t out_byte_len = element_count * output_element_size;
    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
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
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return copy_status;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_round_i64_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    void* function,
    int64_t* host_out
) {
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < 1 ||
        op > 4 ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (host_out == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / sizeof(int64_t)) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    size_t out_byte_len = element_count * sizeof(int64_t);
    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    int grid_status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (grid_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return grid_status;
    }
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
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
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int status_copy = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (status_copy != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_copy;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(out_device, out_byte_len, host_out);
    omni_tensor_backend_cuda_free(out_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) return copy_status;

    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_map_unary_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        0,
        4,
        omni_tensor_cuda_map_unary_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_unary_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        0,
        4,
        omni_tensor_cuda_map_unary_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128_unary(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        sizeof(OmniTensorCudaComplex128),
        op,
        0,
        5,
        omni_tensor_cuda_map_complex128_unary_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64_unary(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        sizeof(OmniTensorCudaComplex64),
        op,
        0,
        5,
        omni_tensor_cuda_map_complex64_unary_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128_to_real(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        sizeof(double),
        op,
        0,
        4,
        omni_tensor_cuda_map_complex128_to_real_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64_to_real(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        sizeof(float),
        op,
        0,
        4,
        omni_tensor_cuda_map_complex64_to_real_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_scientific_unary_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_scientific_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_cuda_map_scientific_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_scientific_unary_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_scientific_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_cuda_map_scientific_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_round_i64_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_round_i64_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_cuda_round_i64_f64_function,
        host_out
    );
}

int omni_tensor_backend_cuda_round_i64_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_round_i64_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_cuda_round_i64_f32_function,
        host_out
    );
}
