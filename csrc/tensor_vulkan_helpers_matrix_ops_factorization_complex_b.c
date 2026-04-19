#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_lu_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {(uint32_t)n, 0, 0, 0};
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_lu_complex128_spv,
        omni_tensor_vulkan_lu_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    double* metadata = (double*)malloc(metadata_count * element_size);
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * element_size,
        metadata_count * element_size,
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    double swap_payload = metadata[n * 2u];
    double status_payload = metadata[(n + 1u) * 2u];
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
        double pivot_payload = metadata[i * 2u];
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
int omni_tensor_backend_vulkan_lu_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {(uint32_t)n, 0, 0, 0};
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_lu_complex64_spv,
        omni_tensor_vulkan_lu_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    float* metadata = (float*)malloc(metadata_count * element_size);
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * element_size,
        metadata_count * element_size,
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    float swap_payload = metadata[n * 2u];
    float status_payload = metadata[(n + 1u) * 2u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f32(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0f || swap_payload > (float)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((float)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    for (size_t i = 0; i < n; i++) {
        float pivot_payload = metadata[i * 2u];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0f || pivot_payload >= (float)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((float)pivot != pivot_payload) {
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

int omni_tensor_backend_vulkan_inverse_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_inverse_f32_spv,
        omni_tensor_vulkan_inverse_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        output_device,
        element_count * 2u * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
