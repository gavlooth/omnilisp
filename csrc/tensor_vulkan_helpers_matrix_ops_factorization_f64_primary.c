#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_determinant_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_determinant
) {
    if (out_determinant == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_determinant = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) {
        *out_determinant = 1.0;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDeterminantPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_determinant_f64_spv,
        omni_tensor_vulkan_determinant_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double determinant_payload = 0.0;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(determinant_payload), &determinant_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_determinant = determinant_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_solve_f64(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(double) || coefficients_byte_len != coefficient_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(double) || rhs_byte_len != result_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanSolvePushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0
    };
    g_omni_tensor_vulkan_solve_serial_call_count++;
    g_omni_tensor_vulkan_solve_serial_f64_call_count++;
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_solve_f64_spv,
        omni_tensor_vulkan_solve_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        (result_count + coefficient_count) * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

long omni_tensor_backend_vulkan_solve_parallel_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_call_count;
}

long omni_tensor_backend_vulkan_solve_parallel_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_parallel_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_f64_call_count;
}

int omni_tensor_backend_vulkan_solve_parallel_f64(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(double) || coefficients_byte_len != coefficient_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(double) || rhs_byte_len != result_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* output_device = NULL;
    void* status_device = NULL;
    size_t init_work_item_count = coefficient_count > result_count ? coefficient_count : result_count;
    size_t max_pivot_groups = (n + OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE - 1u) /
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE;
    if (max_pivot_groups == 0 || max_pivot_groups > (SIZE_MAX - 4u) / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t status_slots = 4u + max_pivot_groups * 2u;
    if (status_slots > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    g_omni_tensor_vulkan_solve_parallel_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_call_count++;
    g_omni_tensor_vulkan_solve_parallel_f64_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count++;
    int status = omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(double),
        status_slots * sizeof(uint32_t),
        init_work_item_count,
        sizeof(double),
        n,
        rhs_cols,
        result_count,
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE,
        1,
        omni_tensor_vulkan_solve_parallel_init_f64_spv,
        omni_tensor_vulkan_solve_parallel_init_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv_size,
        omni_tensor_vulkan_solve_multi_row_swap_f64_spv,
        omni_tensor_vulkan_solve_multi_row_swap_f64_spv_size,
        omni_tensor_vulkan_solve_multi_factor_f64_spv,
        omni_tensor_vulkan_solve_multi_factor_f64_spv_size,
        omni_tensor_vulkan_solve_multi_eliminate_f64_spv,
        omni_tensor_vulkan_solve_multi_eliminate_f64_spv_size,
        omni_tensor_vulkan_solve_multi_backsolve_f64_spv,
        omni_tensor_vulkan_solve_multi_backsolve_f64_spv_size,
        &g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count,
        &g_omni_tensor_vulkan_solve_factor_stage_f64_call_count,
        &output_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_status_code_u32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_solve_parallel_f32(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(float) || coefficients_byte_len != coefficient_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(float) || rhs_byte_len != result_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* output_device = NULL;
    void* status_device = NULL;
    size_t init_work_item_count = coefficient_count > result_count ? coefficient_count : result_count;
    size_t max_pivot_groups = (n + OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE - 1u) /
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE;
    if (max_pivot_groups == 0 || max_pivot_groups > (SIZE_MAX - 4u) / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t status_slots = 4u + max_pivot_groups * 2u;
    if (status_slots > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    g_omni_tensor_vulkan_solve_parallel_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_call_count++;
    g_omni_tensor_vulkan_solve_parallel_f32_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count++;
    int status = omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(float),
        status_slots * sizeof(uint32_t),
        init_work_item_count,
        sizeof(float),
        n,
        rhs_cols,
        result_count,
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE,
        0,
        omni_tensor_vulkan_solve_parallel_init_f32_spv,
        omni_tensor_vulkan_solve_parallel_init_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv_size,
        omni_tensor_vulkan_solve_multi_row_swap_f32_spv,
        omni_tensor_vulkan_solve_multi_row_swap_f32_spv_size,
        omni_tensor_vulkan_solve_multi_factor_f32_spv,
        omni_tensor_vulkan_solve_multi_factor_f32_spv_size,
        omni_tensor_vulkan_solve_multi_eliminate_f32_spv,
        omni_tensor_vulkan_solve_multi_eliminate_f32_spv_size,
        omni_tensor_vulkan_solve_multi_backsolve_f32_spv,
        omni_tensor_vulkan_solve_multi_backsolve_f32_spv_size,
        &g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count,
        &g_omni_tensor_vulkan_solve_factor_stage_f32_call_count,
        &output_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_status_code_u32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_lu_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((n + 2u) > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_lu_f64_spv,
        omni_tensor_vulkan_lu_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    double* metadata = (double*)malloc(metadata_count * sizeof(double));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * sizeof(double),
        metadata_count * sizeof(double),
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    double swap_payload = metadata[n];
    double status_payload = metadata[n + 1u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f64(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0 || swap_payload > (double)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((double)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }

    for (size_t i = 0; i < n; i++) {
        double pivot_payload = metadata[i];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0 || pivot_payload >= (double)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((double)pivot != pivot_payload) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        out_pivots[i] = pivot;
    }

    free(metadata);
    *out_swap_count = swap_count;
    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_inverse_f64_spv,
        omni_tensor_vulkan_inverse_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        element_count * 2u * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_cholesky_f64_spv,
        omni_tensor_vulkan_cholesky_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        element_count * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / sizeof(double) || byte_len != q_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * sizeof(double),
        r_storage_count * sizeof(double),
        1,
        omni_tensor_vulkan_qr_f64_spv,
        omni_tensor_vulkan_qr_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        r_device,
        r_count * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
