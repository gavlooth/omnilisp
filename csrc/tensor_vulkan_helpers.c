#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);

int omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
long g_omni_tensor_vulkan_solve_parallel_call_count = 0;
long g_omni_tensor_vulkan_solve_multi_dispatch_call_count = 0;
long g_omni_tensor_vulkan_solve_pivot_reduce_call_count = 0;
long g_omni_tensor_vulkan_solve_factor_stage_call_count = 0;
long g_omni_tensor_vulkan_solve_serial_call_count = 0;
long g_omni_tensor_vulkan_solve_parallel_f32_call_count = 0;
long g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count = 0;
long g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count = 0;
long g_omni_tensor_vulkan_solve_factor_stage_f32_call_count = 0;
long g_omni_tensor_vulkan_solve_serial_f32_call_count = 0;
long g_omni_tensor_vulkan_solve_parallel_f64_call_count = 0;
long g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count = 0;
long g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count = 0;
long g_omni_tensor_vulkan_solve_factor_stage_f64_call_count = 0;
long g_omni_tensor_vulkan_solve_serial_f64_call_count = 0;
int omni_tensor_backend_vulkan_copy_range_to_host(
    const void* device_ptr,
    size_t offset,
    size_t byte_len,
    void* host_ptr
) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || offset > handle->byte_len || byte_len > handle->byte_len - offset) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)handle->byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(host_ptr, (const unsigned char*)mapped + offset, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_unary_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (op > 4 && op != 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_unary_f64_spv,
        omni_tensor_vulkan_map_unary_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_unary_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (op > 16 && op != 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_unary_f32_spv,
        omni_tensor_vulkan_map_unary_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex128_unary(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(double) * 2u) || byte_len != element_count * sizeof(double) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_complex128_unary_spv,
        omni_tensor_vulkan_map_complex128_unary_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64_unary(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(float) * 2u) || byte_len != element_count * sizeof(float) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_complex64_unary_spv,
        omni_tensor_vulkan_map_complex64_unary_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex128_to_real(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op != 0u && op != 3u && op != 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(double) * 2u) ||
        input_byte_len != element_count * sizeof(double) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        input_byte_len,
        element_count * sizeof(double),
        element_count,
        omni_tensor_vulkan_map_complex128_to_real_spv,
        omni_tensor_vulkan_map_complex128_to_real_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64_to_real(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op != 0u && op != 3u && op != 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(float) * 2u) ||
        input_byte_len != element_count * sizeof(float) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        element_count * sizeof(float),
        element_count,
        omni_tensor_vulkan_map_complex64_to_real_spv,
        omni_tensor_vulkan_map_complex64_to_real_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}
