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

static unsigned char omni_tensor_vulkan_copy_range_test_bytes[32];
static OmniVulkanDeviceSize omni_tensor_vulkan_copy_range_test_offset = OMNI_VULKAN_WHOLE_SIZE;
static OmniVulkanDeviceSize omni_tensor_vulkan_copy_range_test_size = OMNI_VULKAN_WHOLE_SIZE;

static OmniVulkanResult omni_tensor_vulkan_copy_range_test_map(
    OmniVulkanDevice device,
    OmniVulkanDeviceMemory memory,
    OmniVulkanDeviceSize offset,
    OmniVulkanDeviceSize size,
    OmniVulkanFlags flags,
    void** data
) {
    (void)device;
    (void)memory;
    (void)flags;
    omni_tensor_vulkan_copy_range_test_offset = offset;
    omni_tensor_vulkan_copy_range_test_size = size;
    if (data == NULL || offset > sizeof(omni_tensor_vulkan_copy_range_test_bytes) ||
        size > sizeof(omni_tensor_vulkan_copy_range_test_bytes) - offset) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *data = omni_tensor_vulkan_copy_range_test_bytes + offset;
    return OMNI_VULKAN_SUCCESS;
}

static void omni_tensor_vulkan_copy_range_test_unmap(OmniVulkanDevice device, OmniVulkanDeviceMemory memory) {
    (void)device;
    (void)memory;
}

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
    if (omni_vulkan_map_memory(device, handle->memory, (OmniVulkanDeviceSize)offset, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(host_ptr, mapped, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_range_subrange_map_for_tests(void) {
    for (size_t i = 0; i < sizeof(omni_tensor_vulkan_copy_range_test_bytes); i++) {
        omni_tensor_vulkan_copy_range_test_bytes[i] = (unsigned char)(i + 1u);
    }
    omni_tensor_vulkan_copy_range_test_offset = OMNI_VULKAN_WHOLE_SIZE;
    omni_tensor_vulkan_copy_range_test_size = OMNI_VULKAN_WHOLE_SIZE;

    omni_vulkan_map_memory_fn saved_map = omni_vulkan_map_memory;
    omni_vulkan_unmap_memory_fn saved_unmap = omni_vulkan_unmap_memory;
    omni_vulkan_map_memory = omni_tensor_vulkan_copy_range_test_map;
    omni_vulkan_unmap_memory = omni_tensor_vulkan_copy_range_test_unmap;

    OmniTensorVulkanContext context = {0};
    context.device = (OmniVulkanDevice)(uintptr_t)1u;
    OmniTensorVulkanBuffer handle = {0};
    handle.context = &context;
    handle.memory = (OmniVulkanDeviceMemory)(uintptr_t)2u;
    handle.byte_len = sizeof(omni_tensor_vulkan_copy_range_test_bytes);
    atomic_init(&handle.ref_count, 1u);

    unsigned char out[5] = {0};
    int status = omni_tensor_backend_vulkan_copy_range_to_host(&handle, 17u, sizeof(out), out);

    omni_vulkan_map_memory = saved_map;
    omni_vulkan_unmap_memory = saved_unmap;

    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (omni_tensor_vulkan_copy_range_test_offset != 17u ||
        omni_tensor_vulkan_copy_range_test_size != sizeof(out)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    for (size_t i = 0; i < sizeof(out); i++) {
        if (out[i] != omni_tensor_vulkan_copy_range_test_bytes[17u + i]) return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
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
    if (op > 4 && op != 19u && op != 20u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
    if (op > 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
    if (element_count != 0 && input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
    if (element_count != 0 && input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
    if (element_count != 0 && input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
    if (element_count != 0 && input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
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
