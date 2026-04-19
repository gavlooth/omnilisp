#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double) * 2u,
        1,
        omni_tensor_vulkan_norm_f64_spv,
        omni_tensor_vulkan_norm_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double norm_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0
            ? norm_payload[0] * sqrt(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float) * 2u,
        1,
        omni_tensor_vulkan_norm_f32_spv,
        omni_tensor_vulkan_norm_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float norm_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0f
            ? norm_payload[0] * sqrtf(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double) * 2u,
        1,
        omni_tensor_vulkan_norm_complex128_spv,
        omni_tensor_vulkan_norm_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double norm_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0
            ? norm_payload[0] * sqrt(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float) * 2u,
        1,
        omni_tensor_vulkan_norm_complex64_spv,
        omni_tensor_vulkan_norm_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float norm_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0f
            ? norm_payload[0] * sqrtf(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    double tolerance,
    double* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankPushConstants push = {
        tolerance,
        (uint32_t)rows,
        (uint32_t)cols
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_rank_f64_spv,
        omni_tensor_vulkan_rank_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double rank_payload = 0.0;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), &rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload) || rank_payload < 0.0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    float tolerance,
    float* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0f) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t max_rank = rows < cols ? rows : cols;
    if (max_rank > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankF32PushConstants push = {
        (float)tolerance,
        (uint32_t)rows,
        (uint32_t)cols,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_rank_f32_spv,
        omni_tensor_vulkan_rank_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float rank_payload = 0.0f;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), &rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload) || rank_payload < 0.0f) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        if ((size_t)rank_payload > max_rank) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    double tolerance,
    double* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankPushConstants push = {
        tolerance,
        (uint32_t)rows,
        (uint32_t)cols
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_rank_complex128_spv,
        omni_tensor_vulkan_rank_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double rank_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload[0]) || rank_payload[0] < 0.0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    float tolerance,
    float* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0f) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t max_rank = rows < cols ? rows : cols;
    if (max_rank > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankF32PushConstants push = {
        (float)tolerance,
        (uint32_t)rows,
        (uint32_t)cols,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_rank_complex64_spv,
        omni_tensor_vulkan_rank_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float rank_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload[0]) || rank_payload[0] < 0.0f) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        if ((size_t)rank_payload[0] > max_rank) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload[0];
    }
    return status;
}
