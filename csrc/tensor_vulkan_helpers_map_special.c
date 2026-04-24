#include "tensor_vulkan_helpers_internal.h"

int omni_tensor_backend_vulkan_map_normal_quantile_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        0,
        0,
        0
    };
    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(push)
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
        omni_tensor_vulkan_normal_quantile_f32_spv,
        omni_tensor_vulkan_normal_quantile_f32_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(element_count, OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_normal_quantile_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        0,
        0,
        0
    };
    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(push)
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
        omni_tensor_vulkan_normal_quantile_f64_spv,
        omni_tensor_vulkan_normal_quantile_f64_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(element_count, OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_round_i64_launch(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    const uint32_t* shader_words,
    size_t shader_size,
    int require_float64,
    int64_t* host_out
) {
    if (op < 1 || op > 4) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_int64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (require_float64 && !omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_INVALID;
    if (input_device_ptr == NULL || host_out == NULL || shader_words == NULL || shader_size == 0) return OMNI_TENSOR_VULKAN_INVALID;
    if (element_size == 0 || element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(int64_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;
    OmniVulkanDevice device = context->device;
    size_t out_byte_len = element_count * sizeof(int64_t);

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)out_byte_len },
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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = { (uint32_t)element_count, op, 0, 0 };
    OmniVulkanPushConstantRange push_range = { OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push) };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, pipeline_layout, shader_words, shader_size, &shader_module, &pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(element_count, OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
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
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        }
    }
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_backend_vulkan_copy_to_host(output, out_byte_len, host_out);
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    omni_tensor_backend_vulkan_destroy_buffer_handle(output);
    return result;
}

int omni_tensor_backend_vulkan_round_i64_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    return omni_tensor_backend_vulkan_round_i64_launch(
        input_device_ptr,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_vulkan_round_i64_f64_spv,
        omni_tensor_vulkan_round_i64_f64_spv_size,
        1,
        host_out
    );
}

int omni_tensor_backend_vulkan_round_i64_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    return omni_tensor_backend_vulkan_round_i64_launch(
        input_device_ptr,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_vulkan_round_i64_f32_spv,
        omni_tensor_vulkan_round_i64_f32_spv_size,
        0,
        host_out
    );
}
