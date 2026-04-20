#include "tensor_vulkan_helpers_internal.h"

#define OMNI_TENSOR_VULKAN_ML_POOL2D_LOCAL_SIZE 64u

typedef struct OmniTensorVulkanMlPool2dPushConstants {
    uint32_t batch;
    uint32_t channels;
    uint32_t height;
    uint32_t width;
    uint32_t kernel_height;
    uint32_t kernel_width;
    uint32_t stride_height;
    uint32_t stride_width;
    uint32_t padding_height;
    uint32_t padding_width;
    uint32_t out_height;
    uint32_t out_width;
    uint32_t op;
} OmniTensorVulkanMlPool2dPushConstants;

static int omni_tensor_vulkan_pool_mul_size(size_t left, size_t right, size_t* out) {
    if (out == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (right != 0 && left > SIZE_MAX / right) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    *out = left * right;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_ml_pool2d_validate_dims(
    size_t input_byte_len,
    size_t batch,
    size_t channels,
    size_t height,
    size_t width,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t out_height,
    size_t out_width,
    size_t op,
    size_t* out_output_count
) {
    if (out_output_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_output_count = 0;
    if (input_byte_len == 0 || batch == 0 || channels == 0 || height == 0 || width == 0 ||
        kernel_height == 0 || kernel_width == 0 || stride_height == 0 || stride_width == 0 ||
        out_height == 0 || out_width == 0 || op > 1) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (padding_height >= kernel_height || padding_width >= kernel_width) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t input_count = 0;
    int status = omni_tensor_vulkan_pool_mul_size(batch, channels, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_pool_mul_size(input_count, height, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_pool_mul_size(input_count, width, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (input_count > SIZE_MAX / sizeof(float) || input_byte_len < input_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t output_count = 0;
    status = omni_tensor_vulkan_pool_mul_size(batch, channels, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_pool_mul_size(output_count, out_height, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_pool_mul_size(output_count, out_width, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (output_count > UINT32_MAX || output_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    if (batch > UINT32_MAX || channels > UINT32_MAX || height > UINT32_MAX ||
        width > UINT32_MAX || kernel_height > UINT32_MAX || kernel_width > UINT32_MAX ||
        stride_height > UINT32_MAX || stride_width > UINT32_MAX ||
        padding_height > UINT32_MAX || padding_width > UINT32_MAX ||
        out_height > UINT32_MAX || out_width > UINT32_MAX || op > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    *out_output_count = output_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_pool2d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t batch,
    size_t channels,
    size_t height,
    size_t width,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t out_height,
    size_t out_width,
    size_t op,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = 0;
    int status = omni_tensor_vulkan_ml_pool2d_validate_dims(
        input_byte_len,
        batch,
        channels,
        height,
        width,
        kernel_height,
        kernel_width,
        stride_height,
        stride_width,
        padding_height,
        padding_width,
        out_height,
        out_width,
        op,
        &output_count
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanMlPool2dPushConstants push = {
        (uint32_t)batch,
        (uint32_t)channels,
        (uint32_t)height,
        (uint32_t)width,
        (uint32_t)kernel_height,
        (uint32_t)kernel_width,
        (uint32_t)stride_height,
        (uint32_t)stride_width,
        (uint32_t)padding_height,
        (uint32_t)padding_width,
        (uint32_t)out_height,
        (uint32_t)out_width,
        (uint32_t)op
    };
    size_t output_byte_len = output_count * sizeof(float);
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        output_byte_len,
        output_count,
        omni_tensor_vulkan_ml_pool2d_f32_spv,
        omni_tensor_vulkan_ml_pool2d_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_ML_POOL2D_LOCAL_SIZE,
        out_device_ptr
    );
}
