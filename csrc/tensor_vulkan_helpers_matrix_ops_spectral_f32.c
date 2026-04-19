#include "tensor_vulkan_helpers_internal.h"
static int omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (visible_output_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + gram_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(float),
        1u,
        omni_tensor_vulkan_singular_values_f32_spv,
        omni_tensor_vulkan_singular_values_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        output,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(float),
        1u,
        omni_tensor_vulkan_singular_values_complex64_spv,
        omni_tensor_vulkan_singular_values_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        output,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_svd_validate_shape_f32(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_u_count,
    size_t* out_u_storage_count,
    size_t* out_s_storage_count,
    size_t* out_v_count
) {
    if (out_k == NULL || out_u_count == NULL || out_u_storage_count == NULL ||
        out_s_storage_count == NULL || out_v_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_k = 0u;
    *out_u_count = 0u;
    *out_u_storage_count = 0u;
    *out_s_storage_count = 0u;
    *out_v_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rows != 0 && k > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_count = rows * k;
    if (cols != 0 && k > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t v_count = cols * k;
    if (u_count > UINT32_MAX || v_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (u_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_storage_count = u_count + gram_count;
    if (u_storage_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t s_storage_count = k + 1u;
    if (u_storage_count > SIZE_MAX / sizeof(float) ||
        s_storage_count > SIZE_MAX / sizeof(float) ||
        v_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_u_count = u_count;
    *out_u_storage_count = u_storage_count;
    *out_s_storage_count = s_storage_count;
    *out_v_count = v_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_svd_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t u_count = 0u;
    size_t u_storage_count = 0u;
    size_t s_storage_count = 0u;
    size_t v_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_svd_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &u_count,
        &u_storage_count,
        &s_storage_count,
        &v_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || u_count == 0u || u_storage_count == 0u || s_storage_count == 0u || v_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSvdPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)u_count,
        (uint32_t)s_storage_count,
        (uint32_t)v_count
    };
    void* u_device = NULL;
    void* s_device = NULL;
    void* v_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        u_storage_count * sizeof(float),
        s_storage_count * sizeof(float),
        v_count * sizeof(float),
        1u,
        omni_tensor_vulkan_svd_f32_spv,
        omni_tensor_vulkan_svd_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SVD_LOCAL_SIZE,
        &u_device,
        &s_device,
        &v_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        s_device,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)v_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)s_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)u_device);
        return status;
    }

    *out_u_device_ptr = u_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_norm_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_f32(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(float);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(float),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
int omni_tensor_backend_vulkan_singular_norm_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_complex64(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(float);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(float),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
