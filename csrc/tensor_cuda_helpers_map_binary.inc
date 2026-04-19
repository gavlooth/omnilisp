static int omni_tensor_cuda_operand_matches_broadcast(
    size_t out_rank,
    size_t operand_rank,
    const size_t* out_shape,
    const size_t* operand_shape
) {
    if (operand_rank > out_rank) return 0;
    if (operand_rank == 0) return 1;
    if (out_shape == NULL || operand_shape == NULL) return 0;
    size_t axis_delta = out_rank - operand_rank;
    for (size_t out_axis = 0; out_axis < out_rank; out_axis++) {
        if (out_axis < axis_delta) continue;
        size_t operand_axis = out_axis - axis_delta;
        size_t operand_dim = operand_shape[operand_axis];
        size_t out_dim = out_shape[out_axis];
        if (operand_dim != out_dim && operand_dim != 1) return 0;
    }
    return 1;
}

static int omni_tensor_cuda_operand_needs_offsets(
    size_t out_rank,
    size_t operand_rank,
    size_t element_count,
    const size_t* out_shape,
    const size_t* operand_shape
) {
    if (!omni_tensor_cuda_operand_matches_broadcast(out_rank, operand_rank, out_shape, operand_shape)) return -1;
    if (element_count <= 1) return 0;
    if (operand_rank == 0) return 1;
    if (operand_rank != out_rank) return 1;
    for (size_t axis = 0; axis < out_rank; axis++) {
        if (operand_shape[axis] != out_shape[axis]) return 1;
    }
    return 0;
}

static int omni_tensor_cuda_build_operand_offsets(
    size_t out_rank,
    size_t operand_rank,
    size_t element_count,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* operand_shape,
    const size_t* operand_strides,
    void** out_offsets_device
) {
    if (out_offsets_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_offsets_device = NULL;
    int needs_offsets = omni_tensor_cuda_operand_needs_offsets(out_rank, operand_rank, element_count, out_shape, operand_shape);
    if (needs_offsets < 0) return OMNI_TENSOR_CUDA_INVALID;
    if (!needs_offsets) return OMNI_TENSOR_CUDA_SUCCESS;
    if (element_count > ((size_t)-1) / sizeof(uint64_t)) return OMNI_TENSOR_CUDA_INVALID;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (operand_rank > 0 && (operand_shape == NULL || operand_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(uint64_t);
    uint64_t* offsets = (uint64_t*)calloc(element_count, sizeof(uint64_t));
    if (offsets == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;

    size_t axis_delta = out_rank - operand_rank;
    for (size_t index = 0; index < element_count; index++) {
        size_t offset = 0;
        for (size_t out_axis = 0; out_axis < out_rank; out_axis++) {
            if (out_axis < axis_delta) continue;
            size_t operand_axis = out_axis - axis_delta;
            size_t operand_dim = operand_rank == 0 ? 1 : operand_shape[operand_axis];
            if (operand_dim == 1) continue;
            size_t out_dim = out_shape[out_axis];
            size_t out_stride = out_strides[out_axis];
            if (out_dim == 0 || out_stride == 0) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            size_t coord = (index / out_stride) % out_dim;
            if (operand_strides[operand_axis] != 0 && coord > ((size_t)-1) / operand_strides[operand_axis]) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            size_t axis_offset = coord * operand_strides[operand_axis];
            if (axis_offset > ((size_t)-1) - offset) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            offset += axis_offset;
        }
        offsets[index] = (uint64_t)offset;
    }

    int status = omni_tensor_backend_cuda_copy_to_device(offsets, byte_len, out_offsets_device);
    free(offsets);
    return status;
}

static int omni_tensor_backend_cuda_map_launch(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    void* scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (op > 5 || mode > 2 || element_size == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if ((mode == 0 && left_device == NULL) ||
        (mode == 1 && right_device == NULL) ||
        (mode == 2 && (left_device == NULL || right_device == NULL)) ||
        scalar == NULL ||
        function == NULL) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 1 && left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 0 && right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    void* left_offsets_device = NULL;
    void* right_offsets_device = NULL;
    int offset_status = OMNI_TENSOR_CUDA_SUCCESS;
    if (mode != 1) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            left_rank,
            element_count,
            out_shape,
            out_strides,
            left_shape,
            left_strides,
            &left_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) return offset_status;
    }
    if (mode != 0) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            right_rank,
            element_count,
            out_shape,
            out_strides,
            right_shape,
            right_strides,
            &right_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
            return offset_status;
        }
    }

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        return alloc_status;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&left_device,
        (void*)&right_device,
        scalar,
        (void*)&mode,
        (void*)&op,
        (void*)&element_count,
        (void*)&left_offsets_device,
        (void*)&right_offsets_device,
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
    if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
    if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_map_launch(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    void* scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (op > 3 || mode > 2 || element_size == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if ((mode == 0 && left_device == NULL) ||
        (mode == 1 && right_device == NULL) ||
        (mode == 2 && (left_device == NULL || right_device == NULL)) ||
        scalar == NULL ||
        function == NULL) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 1 && left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 0 && right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    void* left_offsets_device = NULL;
    void* right_offsets_device = NULL;
    int offset_status = OMNI_TENSOR_CUDA_SUCCESS;
    if (mode != 1) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            left_rank,
            element_count,
            out_shape,
            out_strides,
            left_shape,
            left_strides,
            &left_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) return offset_status;
    }
    if (mode != 0) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            right_rank,
            element_count,
            out_shape,
            out_strides,
            right_shape,
            right_strides,
            &right_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
            return offset_status;
        }
    }

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        return alloc_status;
    }

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&left_device,
        (void*)&right_device,
        scalar,
        (void*)&mode,
        (void*)&op,
        (void*)&element_count,
        (void*)&left_offsets_device,
        (void*)&right_offsets_device,
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
    if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
    if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
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

int omni_tensor_backend_cuda_map_f64(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    double scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(double),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_f32(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    float scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(float),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    double scalar_real,
    double scalar_imag,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    OmniTensorCudaComplex128 scalar = { scalar_real, scalar_imag };
    return omni_tensor_backend_cuda_complex_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    float scalar_real,
    float scalar_imag,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    OmniTensorCudaComplex64 scalar = { scalar_real, scalar_imag };
    return omni_tensor_backend_cuda_complex_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_complex64_function,
        out_device_ptr
    );
}
