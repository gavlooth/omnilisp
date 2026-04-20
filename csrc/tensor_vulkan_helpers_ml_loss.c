#include "tensor_vulkan_helpers_internal.h"
#include <math.h>

#define OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE 256u

static size_t omni_tensor_vulkan_mse_chunk_count(size_t count) {
    return (count + OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE;
}

static int omni_tensor_vulkan_mse_validate_f32_result(void* result_device_ptr) {
    float result_value = 0.0f;
    int copy_status = omni_tensor_backend_vulkan_copy_to_host(
        result_device_ptr,
        sizeof(result_value),
        &result_value
    );
    if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) return copy_status;
    if (!isfinite(result_value)) return OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_mse_reduce_partials(
    void* partial_device_ptr,
    size_t partial_count,
    size_t element_count,
    size_t element_size,
    int require_float64,
    const uint32_t* reduce_shader_words,
    size_t reduce_shader_size,
    void** out_device_ptr
) {
    void* current_device_ptr = partial_device_ptr;
    size_t current_count = partial_count;
    while (current_count > 1) {
        size_t next_count = omni_tensor_vulkan_mse_chunk_count(current_count);
        if (next_count == 0 || next_count > UINT32_MAX || next_count > SIZE_MAX / element_size) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        OmniTensorVulkanMapUnaryPushConstants push = {
            (uint32_t)current_count,
            next_count == 1 ? 1u : 0u,
            OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE,
            (uint32_t)element_count
        };
        void* next_device_ptr = NULL;
        int status = require_float64
            ? omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
                current_device_ptr,
                current_count * element_size,
                next_count * element_size,
                next_count,
                reduce_shader_words,
                reduce_shader_size,
                &push,
                sizeof(push),
                OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
                &next_device_ptr
            )
            : omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
                current_device_ptr,
                current_count * element_size,
                next_count * element_size,
                next_count,
                reduce_shader_words,
                reduce_shader_size,
                &push,
                sizeof(push),
                OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
                &next_device_ptr
            );
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        current_device_ptr = next_device_ptr;
        current_count = next_count;
    }

    *out_device_ptr = current_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

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
    const uint32_t* reduce_shader_words,
    size_t reduce_shader_size,
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

    size_t partial_count = omni_tensor_vulkan_mse_chunk_count(element_count);
    if (partial_count == 0 || partial_count > UINT32_MAX || partial_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        partial_count == 1 ? 1u : 0u,
        OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE,
        0u
    };
    void* partial_device_ptr = NULL;
    int status = require_float64
        ? omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            partial_count * element_size,
            partial_count,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
            &partial_device_ptr
        )
        : omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            partial_count * element_size,
            partial_count,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
            &partial_device_ptr
        );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    void* result_device_ptr = NULL;
    if (partial_count == 1) {
        result_device_ptr = partial_device_ptr;
    } else {
        status = omni_tensor_vulkan_mse_reduce_partials(
            partial_device_ptr,
            partial_count,
            element_count,
            element_size,
            require_float64,
            reduce_shader_words,
            reduce_shader_size,
            &result_device_ptr
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    }

    if (!require_float64) {
        status = omni_tensor_vulkan_mse_validate_f32_result(result_device_ptr);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)result_device_ptr);
            return status;
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
        omni_tensor_vulkan_ml_mse_reduce_f64_spv,
        omni_tensor_vulkan_ml_mse_reduce_f64_spv_size,
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
        omni_tensor_vulkan_ml_mse_reduce_f32_spv,
        omni_tensor_vulkan_ml_mse_reduce_f32_spv_size,
        out_device_ptr
    );
}
