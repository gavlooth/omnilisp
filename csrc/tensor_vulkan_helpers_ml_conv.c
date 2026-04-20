#include "tensor_vulkan_helpers_internal.h"

#define OMNI_TENSOR_VULKAN_ML_CONV1D_LOCAL_SIZE 64u

typedef struct OmniTensorVulkanMlConv1dPushConstants {
    uint32_t batch;
    uint32_t in_channels;
    uint32_t width;
    uint32_t out_channels;
    uint32_t kernel_width;
    uint32_t stride;
    uint32_t padding;
    uint32_t dilation;
    uint32_t groups;
    uint32_t out_width;
    uint32_t in_channels_per_group;
    uint32_t out_channels_per_group;
} OmniTensorVulkanMlConv1dPushConstants;

static int omni_tensor_vulkan_ml_conv1d_validate_dims(
    size_t input_byte_len,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t width,
    size_t out_channels,
    size_t kernel_width,
    size_t stride,
    size_t padding,
    size_t dilation,
    size_t groups,
    size_t out_width,
    size_t* out_input_count,
    size_t* out_kernel_count,
    size_t* out_output_count
) {
    if (out_input_count == NULL || out_kernel_count == NULL || out_output_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_input_count = 0;
    *out_kernel_count = 0;
    *out_output_count = 0;
    if (input_byte_len == 0 || kernel_byte_len == 0 ||
        batch == 0 || in_channels == 0 || width == 0 || out_channels == 0 || kernel_width == 0 ||
        stride == 0 || dilation == 0 || groups == 0 || out_width == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (in_channels % groups != 0 || out_channels % groups != 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t in_channels_per_group = in_channels / groups;

    size_t input_count = 0;
    if (batch != 0 && in_channels > SIZE_MAX / batch) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    input_count = batch * in_channels;
    if (width != 0 && input_count > SIZE_MAX / width) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    input_count *= width;
    if (input_count > SIZE_MAX / sizeof(float) || input_byte_len < input_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t kernel_count = 0;
    if (in_channels_per_group != 0 && out_channels > SIZE_MAX / in_channels_per_group) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    kernel_count = out_channels * in_channels_per_group;
    if (kernel_width != 0 && kernel_count > SIZE_MAX / kernel_width) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    kernel_count *= kernel_width;
    if (kernel_count > SIZE_MAX / sizeof(float) || kernel_byte_len < kernel_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t output_count = 0;
    if (batch != 0 && out_channels > SIZE_MAX / batch) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count = batch * out_channels;
    if (out_width != 0 && output_count > SIZE_MAX / out_width) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count *= out_width;
    if (output_count > UINT32_MAX || output_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    if (batch > UINT32_MAX || in_channels > UINT32_MAX || width > UINT32_MAX ||
        out_channels > UINT32_MAX || kernel_width > UINT32_MAX || stride > UINT32_MAX ||
        padding > UINT32_MAX || dilation > UINT32_MAX || groups > UINT32_MAX ||
        out_width > UINT32_MAX || in_channels_per_group > UINT32_MAX ||
        (out_channels / groups) > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    *out_input_count = input_count;
    *out_kernel_count = kernel_count;
    *out_output_count = output_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_conv1d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* kernel_device_ptr,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t width,
    size_t out_channels,
    size_t kernel_width,
    size_t stride,
    size_t padding,
    size_t dilation,
    size_t groups,
    size_t out_width,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t input_count = 0;
    size_t kernel_count = 0;
    size_t output_count = 0;
    int status = omni_tensor_vulkan_ml_conv1d_validate_dims(
        input_byte_len,
        kernel_byte_len,
        batch,
        in_channels,
        width,
        out_channels,
        kernel_width,
        stride,
        padding,
        dilation,
        groups,
        out_width,
        &input_count,
        &kernel_count,
        &output_count
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    (void)input_count;
    (void)kernel_count;

    OmniTensorVulkanMlConv1dPushConstants push = {
        (uint32_t)batch,
        (uint32_t)in_channels,
        (uint32_t)width,
        (uint32_t)out_channels,
        (uint32_t)kernel_width,
        (uint32_t)stride,
        (uint32_t)padding,
        (uint32_t)dilation,
        (uint32_t)groups,
        (uint32_t)out_width,
        (uint32_t)(in_channels / groups),
        (uint32_t)(out_channels / groups)
    };
    size_t output_byte_len = output_count * sizeof(float);
    return omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        input_device_ptr,
        input_byte_len,
        kernel_device_ptr,
        kernel_byte_len,
        output_byte_len,
        output_count,
        omni_tensor_vulkan_ml_conv1d_f32_spv,
        omni_tensor_vulkan_ml_conv1d_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_ML_CONV1D_LOCAL_SIZE,
        out_device_ptr
    );
}

#define OMNI_TENSOR_VULKAN_ML_CONV2D_LOCAL_SIZE 64u

typedef struct OmniTensorVulkanMlConv2dPushConstants {
    uint32_t batch;
    uint32_t in_channels;
    uint32_t height;
    uint32_t width;
    uint32_t out_channels;
    uint32_t kernel_height;
    uint32_t kernel_width;
    uint32_t stride_height;
    uint32_t stride_width;
    uint32_t padding_height;
    uint32_t padding_width;
    uint32_t dilation_height;
    uint32_t dilation_width;
    uint32_t groups;
    uint32_t out_height;
    uint32_t out_width;
    uint32_t in_channels_per_group;
    uint32_t out_channels_per_group;
} OmniTensorVulkanMlConv2dPushConstants;

static int omni_tensor_vulkan_mul_size(size_t left, size_t right, size_t* out) {
    if (out == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (right != 0 && left > SIZE_MAX / right) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    *out = left * right;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_ml_conv2d_validate_dims(
    size_t input_byte_len,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t height,
    size_t width,
    size_t out_channels,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t dilation_height,
    size_t dilation_width,
    size_t groups,
    size_t out_height,
    size_t out_width,
    size_t* out_output_count
) {
    if (out_output_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_output_count = 0;
    if (input_byte_len == 0 || kernel_byte_len == 0 ||
        batch == 0 || in_channels == 0 || height == 0 || width == 0 ||
        out_channels == 0 || kernel_height == 0 || kernel_width == 0 ||
        stride_height == 0 || stride_width == 0 || dilation_height == 0 ||
        dilation_width == 0 || groups == 0 || out_height == 0 || out_width == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (in_channels % groups != 0 || out_channels % groups != 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t in_channels_per_group = in_channels / groups;
    size_t input_count = 0;
    int status = omni_tensor_vulkan_mul_size(batch, in_channels, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(input_count, height, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(input_count, width, &input_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (input_count > SIZE_MAX / sizeof(float) || input_byte_len < input_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t kernel_count = 0;
    status = omni_tensor_vulkan_mul_size(out_channels, in_channels_per_group, &kernel_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(kernel_count, kernel_height, &kernel_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(kernel_count, kernel_width, &kernel_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (kernel_count > SIZE_MAX / sizeof(float) || kernel_byte_len < kernel_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t output_count = 0;
    status = omni_tensor_vulkan_mul_size(batch, out_channels, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(output_count, out_height, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    status = omni_tensor_vulkan_mul_size(output_count, out_width, &output_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (output_count > UINT32_MAX || output_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    if (batch > UINT32_MAX || in_channels > UINT32_MAX || height > UINT32_MAX ||
        width > UINT32_MAX || out_channels > UINT32_MAX || kernel_height > UINT32_MAX ||
        kernel_width > UINT32_MAX || stride_height > UINT32_MAX || stride_width > UINT32_MAX ||
        padding_height > UINT32_MAX || padding_width > UINT32_MAX ||
        dilation_height > UINT32_MAX || dilation_width > UINT32_MAX ||
        groups > UINT32_MAX || out_height > UINT32_MAX || out_width > UINT32_MAX ||
        in_channels_per_group > UINT32_MAX || (out_channels / groups) > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    *out_output_count = output_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_conv2d_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* kernel_device_ptr,
    size_t kernel_byte_len,
    size_t batch,
    size_t in_channels,
    size_t height,
    size_t width,
    size_t out_channels,
    size_t kernel_height,
    size_t kernel_width,
    size_t stride_height,
    size_t stride_width,
    size_t padding_height,
    size_t padding_width,
    size_t dilation_height,
    size_t dilation_width,
    size_t groups,
    size_t out_height,
    size_t out_width,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = 0;
    int status = omni_tensor_vulkan_ml_conv2d_validate_dims(
        input_byte_len,
        kernel_byte_len,
        batch,
        in_channels,
        height,
        width,
        out_channels,
        kernel_height,
        kernel_width,
        stride_height,
        stride_width,
        padding_height,
        padding_width,
        dilation_height,
        dilation_width,
        groups,
        out_height,
        out_width,
        &output_count
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanMlConv2dPushConstants push = {
        (uint32_t)batch,
        (uint32_t)in_channels,
        (uint32_t)height,
        (uint32_t)width,
        (uint32_t)out_channels,
        (uint32_t)kernel_height,
        (uint32_t)kernel_width,
        (uint32_t)stride_height,
        (uint32_t)stride_width,
        (uint32_t)padding_height,
        (uint32_t)padding_width,
        (uint32_t)dilation_height,
        (uint32_t)dilation_width,
        (uint32_t)groups,
        (uint32_t)out_height,
        (uint32_t)out_width,
        (uint32_t)(in_channels / groups),
        (uint32_t)(out_channels / groups)
    };
    size_t output_byte_len = output_count * sizeof(float);
    return omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        input_device_ptr,
        input_byte_len,
        kernel_device_ptr,
        kernel_byte_len,
        output_byte_len,
        output_count,
        omni_tensor_vulkan_ml_conv2d_f32_spv,
        omni_tensor_vulkan_ml_conv2d_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_ML_CONV2D_LOCAL_SIZE,
        out_device_ptr
    );
}
