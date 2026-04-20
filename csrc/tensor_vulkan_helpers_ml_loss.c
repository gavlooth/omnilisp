#include "tensor_vulkan_helpers_internal.h"
#include <math.h>

static int omni_tensor_backend_vulkan_ml_mse_typed(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    size_t element_size,
    int require_float64,
    const uint32_t* shader_words,
    size_t shader_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (require_float64) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (!omni_tensor_backend_vulkan_float32_available()) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t required_bytes = element_count * element_size;
    if (predictions_byte_len < required_bytes || targets_byte_len < required_bytes) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        0u,
        0u,
        0u
    };
    void* result_device_ptr = NULL;
    int status = require_float64
        ? omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            element_size,
            1,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            1u,
            &result_device_ptr
        )
        : omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            element_size,
            1,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            1u,
            &result_device_ptr
        );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (!require_float64) {
        float result_value = 0.0f;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(
            result_device_ptr,
            sizeof(result_value),
            &result_value
        );
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)result_device_ptr);
            return copy_status;
        }
        if (!isfinite(result_value)) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)result_device_ptr);
            return OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        }
    }

    *out_device_ptr = result_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_mse_f64(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_mse_typed(
        predictions_device_ptr,
        predictions_byte_len,
        targets_device_ptr,
        targets_byte_len,
        element_count,
        sizeof(double),
        1,
        omni_tensor_vulkan_ml_mse_f64_spv,
        omni_tensor_vulkan_ml_mse_f64_spv_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_ml_mse_f32(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_mse_typed(
        predictions_device_ptr,
        predictions_byte_len,
        targets_device_ptr,
        targets_byte_len,
        element_count,
        sizeof(float),
        0,
        omni_tensor_vulkan_ml_mse_f32_spv,
        omni_tensor_vulkan_ml_mse_f32_spv_size,
        out_device_ptr
    );
}
