#include "tensor_vulkan_helpers_internal.h"

#define OMNI_TENSOR_VULKAN_ML_CLIP_SCALE_LOCAL_SIZE 64u

typedef struct OmniTensorVulkanMlClipSumSquaresPushConstants {
    uint32_t count;
} OmniTensorVulkanMlClipSumSquaresPushConstants;

typedef struct OmniTensorVulkanMlClipScalePushConstants {
    uint32_t count;
    float scale;
} OmniTensorVulkanMlClipScalePushConstants;

typedef struct OmniTensorVulkanKernelSourceBinaryF32PushConstants {
    uint32_t count;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanKernelSourceBinaryF32PushConstants;

int omni_tensor_backend_vulkan_ml_clip_sum_squares_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float* out_sum
) {
    if (out_sum == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_sum = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMlClipSumSquaresPushConstants push = { (uint32_t)element_count };
    void* sum_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        sizeof(float),
        1,
        omni_tensor_vulkan_ml_clip_sum_squares_f32_spv,
        omni_tensor_vulkan_ml_clip_sum_squares_f32_spv_size,
        &push,
        sizeof(push),
        1,
        &sum_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (sum_device == NULL) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;

    status = omni_tensor_backend_vulkan_copy_to_host(sum_device, sizeof(float), out_sum);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)sum_device);
    return status;
}

int omni_tensor_backend_vulkan_ml_clip_scale_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float scale,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMlClipScalePushConstants push = { (uint32_t)element_count, scale };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_ml_clip_scale_f32_spv,
        omni_tensor_vulkan_ml_clip_scale_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_ML_CLIP_SCALE_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_kernel_source_scale_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float scale,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMlClipScalePushConstants push = { (uint32_t)element_count, scale };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_ml_clip_scale_f32_spv,
        omni_tensor_vulkan_ml_clip_scale_f32_spv_size,
        &push,
        sizeof(push),
        local_size == 0 ? OMNI_TENSOR_VULKAN_ML_CLIP_SCALE_LOCAL_SIZE : local_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_kernel_source_scale_f32_spirv(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    float scale,
    uint32_t local_size,
    const uint32_t* shader_words,
    size_t shader_word_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (shader_words == NULL || shader_word_count < 5 || shader_word_count > SIZE_MAX / sizeof(uint32_t)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (shader_words[0] != 0x07230203u || shader_words[1] == 0 || shader_words[3] == 0 || shader_words[4] != 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMlClipScalePushConstants push = { (uint32_t)element_count, scale };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        byte_len,
        element_count,
        shader_words,
        shader_word_count * sizeof(uint32_t),
        &push,
        sizeof(push),
        local_size == 0 ? OMNI_TENSOR_VULKAN_ML_CLIP_SCALE_LOCAL_SIZE : local_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_kernel_source_unary_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (op > 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMapUnaryPushConstants push = { (uint32_t)element_count, op, 0, 0 };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_unary_f32_spv,
        omni_tensor_vulkan_map_unary_f32_spv_size,
        &push,
        sizeof(push),
        local_size == 0 ? OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE : local_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_kernel_source_unary_f32_spirv(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    uint32_t local_size,
    const uint32_t* shader_words,
    size_t shader_word_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (op > 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (shader_words == NULL || shader_word_count < 5 || shader_word_count > SIZE_MAX / sizeof(uint32_t)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (shader_words[0] != 0x07230203u || shader_words[1] == 0 || shader_words[3] == 0 || shader_words[4] != 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (input_device_ptr == NULL || input_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanMapUnaryPushConstants push = { (uint32_t)element_count, op, 0, 0 };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        byte_len,
        element_count,
        shader_words,
        shader_word_count * sizeof(uint32_t),
        &push,
        sizeof(push),
        local_size == 0 ? OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE : local_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_kernel_source_binary_f32_spirv(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t element_count,
    uint32_t local_size,
    const uint32_t* shader_words,
    size_t shader_word_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (shader_words == NULL || shader_word_count < 5 || shader_word_count > SIZE_MAX / sizeof(uint32_t)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (shader_words[0] != 0x07230203u || shader_words[1] == 0 || shader_words[3] == 0 || shader_words[4] != 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (element_count == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t byte_len = element_count * sizeof(float);
    if (left_device_ptr == NULL || right_device_ptr == NULL || left_byte_len < byte_len || right_byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanKernelSourceBinaryF32PushConstants push = { (uint32_t)element_count, 0, 0, 0 };
    return omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        left_device_ptr,
        left_byte_len,
        right_device_ptr,
        right_byte_len,
        byte_len,
        element_count,
        shader_words,
        shader_word_count * sizeof(uint32_t),
        &push,
        sizeof(push),
        local_size == 0 ? OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE : local_size,
        out_device_ptr
    );
}
