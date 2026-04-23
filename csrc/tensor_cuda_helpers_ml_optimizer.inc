static int omni_tensor_cuda_ml_optimizer_resolve(void) {
    if (!omni_tensor_backend_cuda_available()) return 0;
    if (omni_tensor_cuda_ml_sgd_f32_function != NULL &&
        omni_tensor_cuda_ml_adam_f32_function != NULL &&
        omni_tensor_cuda_ml_rmsprop_f32_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_ml_optimizer_module_attempted) return 0;

    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_ml_optimizer_ptx) != 0 || module == NULL) {
        omni_tensor_cuda_ml_optimizer_module_attempted = 1;
        return 0;
    }
    void* sgd_function = NULL;
    if (omni_cu_module_get_function(&sgd_function, module, "omni_cuda_ml_sgd_f32") != 0 ||
        sgd_function == NULL) {
        omni_cu_module_unload(module);
        omni_tensor_cuda_ml_optimizer_module_attempted = 1;
        return 0;
    }
    void* adam_function = NULL;
    if (omni_cu_module_get_function(&adam_function, module, "omni_cuda_ml_adam_f32") != 0 ||
        adam_function == NULL) {
        omni_cu_module_unload(module);
        omni_tensor_cuda_ml_optimizer_module_attempted = 1;
        return 0;
    }
    void* rmsprop_function = NULL;
    if (omni_cu_module_get_function(&rmsprop_function, module, "omni_cuda_ml_rmsprop_f32") != 0 ||
        rmsprop_function == NULL) {
        omni_cu_module_unload(module);
        omni_tensor_cuda_ml_optimizer_module_attempted = 1;
        return 0;
    }

    omni_tensor_cuda_ml_optimizer_module = module;
    omni_tensor_cuda_ml_sgd_f32_function = sgd_function;
    omni_tensor_cuda_ml_adam_f32_function = adam_function;
    omni_tensor_cuda_ml_rmsprop_f32_function = rmsprop_function;
    return 1;
}

int omni_tensor_backend_cuda_ml_optimizer_available(void) {
    return omni_tensor_cuda_ml_optimizer_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_ml_sgd_f32(
    const void* params_device,
    const void* grads_device,
    const void* velocity_device,
    size_t byte_len,
    size_t element_count,
    float learning_rate,
    float momentum,
    float weight_decay,
    unsigned int has_velocity,
    void** out_params_device,
    void** out_velocity_device
) {
    if (out_params_device == NULL || out_velocity_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_params_device = NULL;
    *out_velocity_device = NULL;
    if (params_device == NULL || grads_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (has_velocity != 0u && velocity_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (byte_len == 0 ||
        element_count == 0 ||
        element_count > ((size_t)-1) / sizeof(float) ||
        byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!isfinite(learning_rate) || !isfinite(momentum) || !isfinite(weight_decay) ||
        learning_rate < 0.0f || momentum < 0.0f || weight_decay < 0.0f) {
        return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
    }
    if (!omni_tensor_cuda_ml_optimizer_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_params = NULL;
    int status = omni_tensor_backend_cuda_alloc(byte_len, &out_params);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;

    void* out_velocity = NULL;
    if (momentum > 0.0f) {
        status = omni_tensor_backend_cuda_alloc(byte_len, &out_velocity);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            omni_tensor_backend_cuda_free(out_params);
            return status;
        }
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (out_velocity != NULL) omni_tensor_backend_cuda_free(out_velocity);
        omni_tensor_backend_cuda_free(out_params);
        return status;
    }
    void* kernel_params[] = {
        (void*)&params_device,
        (void*)&grads_device,
        (void*)&velocity_device,
        (void*)&learning_rate,
        (void*)&momentum,
        (void*)&weight_decay,
        (void*)&has_velocity,
        (void*)&element_count,
        (void*)&out_params,
        (void*)&out_velocity
    };
    int launch_status = omni_cu_launch_kernel(
        omni_tensor_cuda_ml_sgd_f32_function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        kernel_params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        if (out_velocity != NULL) omni_tensor_backend_cuda_free(out_velocity);
        omni_tensor_backend_cuda_free(out_params);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_params_device = out_params;
    *out_velocity_device = out_velocity;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_ml_validate_f32_shape(size_t byte_len, size_t element_count) {
    if (byte_len == 0 ||
        element_count == 0 ||
        element_count > ((size_t)-1) / sizeof(float) ||
        byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static void omni_tensor_backend_cuda_ml_free_three(void* first, void* second, void* third) {
    if (first != NULL) omni_tensor_backend_cuda_free(first);
    if (second != NULL) omni_tensor_backend_cuda_free(second);
    if (third != NULL) omni_tensor_backend_cuda_free(third);
}

int omni_tensor_backend_cuda_ml_adam_f32(
    const void* params_device,
    const void* grads_device,
    const void* first_device,
    const void* second_device,
    size_t byte_len,
    size_t element_count,
    float learning_rate,
    float beta1,
    float beta2,
    float epsilon,
    float weight_decay,
    float first_correction,
    float second_correction,
    unsigned int decoupled_weight_decay,
    unsigned int has_moments,
    void** out_params_device,
    void** out_first_device,
    void** out_second_device
) {
    if (out_params_device == NULL || out_first_device == NULL || out_second_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_params_device = NULL;
    *out_first_device = NULL;
    *out_second_device = NULL;
    if (params_device == NULL || grads_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (has_moments != 0u && (first_device == NULL || second_device == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    int status = omni_tensor_backend_cuda_ml_validate_f32_shape(byte_len, element_count);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;
    if (!isfinite(learning_rate) || !isfinite(beta1) || !isfinite(beta2) || !isfinite(epsilon) ||
        !isfinite(weight_decay) || !isfinite(first_correction) || !isfinite(second_correction) ||
        learning_rate < 0.0f || beta1 < 0.0f || beta1 >= 1.0f || beta2 < 0.0f || beta2 >= 1.0f ||
        epsilon <= 0.0f || weight_decay < 0.0f || first_correction <= 0.0f || second_correction <= 0.0f) {
        return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
    }
    if (!omni_tensor_cuda_ml_optimizer_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_params = NULL;
    status = omni_tensor_backend_cuda_alloc(byte_len, &out_params);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;
    void* out_first = NULL;
    status = omni_tensor_backend_cuda_alloc(byte_len, &out_first);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_ml_free_three(out_params, NULL, NULL);
        return status;
    }
    void* out_second = NULL;
    status = omni_tensor_backend_cuda_alloc(byte_len, &out_second);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_ml_free_three(out_params, out_first, NULL);
        return status;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_ml_free_three(out_params, out_first, out_second);
        return status;
    }
    void* kernel_params[] = {
        (void*)&params_device,
        (void*)&grads_device,
        (void*)&first_device,
        (void*)&second_device,
        (void*)&learning_rate,
        (void*)&beta1,
        (void*)&beta2,
        (void*)&epsilon,
        (void*)&weight_decay,
        (void*)&first_correction,
        (void*)&second_correction,
        (void*)&decoupled_weight_decay,
        (void*)&has_moments,
        (void*)&element_count,
        (void*)&out_params,
        (void*)&out_first,
        (void*)&out_second
    };
    int launch_status = omni_cu_launch_kernel(
        omni_tensor_cuda_ml_adam_f32_function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        kernel_params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_ml_free_three(out_params, out_first, out_second);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_params_device = out_params;
    *out_first_device = out_first;
    *out_second_device = out_second;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_ml_rmsprop_f32(
    const void* params_device,
    const void* grads_device,
    const void* square_device,
    const void* velocity_device,
    size_t byte_len,
    size_t element_count,
    float learning_rate,
    float alpha,
    float epsilon,
    float momentum,
    float weight_decay,
    unsigned int has_square,
    unsigned int has_velocity,
    void** out_params_device,
    void** out_square_device,
    void** out_velocity_device
) {
    if (out_params_device == NULL || out_square_device == NULL || out_velocity_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_params_device = NULL;
    *out_square_device = NULL;
    *out_velocity_device = NULL;
    if (params_device == NULL || grads_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (has_square != 0u && square_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (has_velocity != 0u && velocity_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    int status = omni_tensor_backend_cuda_ml_validate_f32_shape(byte_len, element_count);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;
    if (!isfinite(learning_rate) || !isfinite(alpha) || !isfinite(epsilon) || !isfinite(momentum) || !isfinite(weight_decay) ||
        learning_rate < 0.0f || alpha < 0.0f || alpha >= 1.0f || epsilon <= 0.0f ||
        momentum < 0.0f || momentum >= 1.0f || weight_decay < 0.0f) {
        return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
    }
    if (!omni_tensor_cuda_ml_optimizer_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_params = NULL;
    status = omni_tensor_backend_cuda_alloc(byte_len, &out_params);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) return status;
    void* out_square = NULL;
    status = omni_tensor_backend_cuda_alloc(byte_len, &out_square);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_ml_free_three(out_params, NULL, NULL);
        return status;
    }
    void* out_velocity = NULL;
    if (momentum > 0.0f) {
        status = omni_tensor_backend_cuda_alloc(byte_len, &out_velocity);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            omni_tensor_backend_cuda_ml_free_three(out_params, out_square, NULL);
            return status;
        }
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = 0;
    status = omni_tensor_cuda_grid_dim_1d(element_count, block_dim, &grid_dim);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_ml_free_three(out_params, out_square, out_velocity);
        return status;
    }
    void* kernel_params[] = {
        (void*)&params_device,
        (void*)&grads_device,
        (void*)&square_device,
        (void*)&velocity_device,
        (void*)&learning_rate,
        (void*)&alpha,
        (void*)&epsilon,
        (void*)&momentum,
        (void*)&weight_decay,
        (void*)&has_square,
        (void*)&has_velocity,
        (void*)&element_count,
        (void*)&out_params,
        (void*)&out_square,
        (void*)&out_velocity
    };
    int launch_status = omni_cu_launch_kernel(
        omni_tensor_cuda_ml_rmsprop_f32_function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        kernel_params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_ml_free_three(out_params, out_square, out_velocity);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_params_device = out_params;
    *out_square_device = out_square;
    *out_velocity_device = out_velocity;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_ml_sgd_byte_len_overflow_guard_for_tests(void) {
    void* out_params_device = NULL;
    void* out_velocity_device = NULL;
    size_t overflowing_element_count = ((size_t)-1) / sizeof(float) + 2u;
    return omni_tensor_backend_cuda_ml_sgd_f32(
        (const void*)1,
        (const void*)1,
        NULL,
        sizeof(float),
        overflowing_element_count,
        0.1f,
        0.0f,
        0.0f,
        0u,
        &out_params_device,
        &out_velocity_device
    );
}

int omni_tensor_backend_cuda_ml_stateful_byte_len_overflow_guard_for_tests(void) {
    void* out_params_device = NULL;
    void* out_first_device = NULL;
    void* out_second_device = NULL;
    size_t overflowing_element_count = ((size_t)-1) / sizeof(float) + 2u;
    return omni_tensor_backend_cuda_ml_adam_f32(
        (const void*)1,
        (const void*)1,
        NULL,
        NULL,
        sizeof(float),
        overflowing_element_count,
        0.1f,
        0.9f,
        0.999f,
        0.00000001f,
        0.0f,
        0.1f,
        0.001f,
        0u,
        0u,
        &out_params_device,
        &out_first_device,
        &out_second_device
    );
}
