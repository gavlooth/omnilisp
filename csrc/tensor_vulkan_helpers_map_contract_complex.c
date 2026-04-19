#include "tensor_vulkan_helpers_internal.h"
static int omni_tensor_backend_vulkan_map_complex_launch(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    int requires_float64,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (requires_float64 && !omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_size == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / element_size ||
        byte_len != element_count * element_size || mode > 2u || op > 3u || shader_words == NULL ||
        shader_size == 0 || push_data == NULL || push_size == 0 || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (out_rank > UINT32_MAX || left_rank > UINT32_MAX || right_rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;

    const void* primary_device_ptr = mode == 1u ? right_device_ptr : left_device_ptr;
    if (primary_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* primary = (OmniTensorVulkanBuffer*)primary_device_ptr;
    OmniTensorVulkanBuffer* left = left_device_ptr != NULL ? (OmniTensorVulkanBuffer*)left_device_ptr : primary;
    OmniTensorVulkanBuffer* right = right_device_ptr != NULL ? (OmniTensorVulkanBuffer*)right_device_ptr : primary;
    OmniTensorVulkanContext* context = primary->context;
    if (context == NULL || context->device == NULL ||
        left->context != context || right->context != context ||
        left->buffer == NULL || right->buffer == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t left_required = 0;
    size_t right_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_required > SIZE_MAX / element_size || right_required > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left->byte_len < left_required * element_size || right->byte_len < right_required * element_size) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDevice device = context->device;
    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    result = omni_tensor_backend_vulkan_copy_to_existing_device(metadata, metadata_words * sizeof(uint32_t), metadata_buffer);
    free(metadata);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[5] = {
        { left->buffer, (OmniVulkanDeviceSize)left->byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right->byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
        { metadata_buffer->buffer, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) },
        { status_buffer->buffer, (OmniVulkanDeviceSize)sizeof(uint32_t) }
    };

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS ||
        pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
        uint32_t status_host = 0u;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(status_buffer, sizeof(status_host), &status_host);
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            result = copy_status;
        } else if (status_host == 1u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
    }
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_complex128(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    double scalar_real,
    double scalar_imag,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    OmniTensorVulkanMapComplex128PushConstants push = {
        scalar_real,
        scalar_imag,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank
    };
    return omni_tensor_backend_vulkan_map_complex_launch(
        left_device_ptr,
        right_device_ptr,
        byte_len,
        element_count,
        sizeof(double) * 2u,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_vulkan_map_complex128_spv,
        omni_tensor_vulkan_map_complex128_spv_size,
        &push,
        sizeof(push),
        1,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    float scalar_real,
    float scalar_imag,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    OmniTensorVulkanMapComplex64PushConstants push = {
        scalar_real,
        scalar_imag,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        0
    };
    return omni_tensor_backend_vulkan_map_complex_launch(
        left_device_ptr,
        right_device_ptr,
        byte_len,
        element_count,
        sizeof(float) * 2u,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_vulkan_map_complex64_spv,
        omni_tensor_vulkan_map_complex64_spv_size,
        &push,
        sizeof(push),
        0,
        out_device_ptr
    );
}
