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
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 || shader_words == NULL || shader_size == 0 || push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[2] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

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
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(output_element_count, local_size, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

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
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 || shader_words == NULL || shader_size == 0 || push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[2] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

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
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(output_element_count, local_size, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { left->buffer, (OmniVulkanDeviceSize)left_byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[2].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

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
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(output_element_count, local_size, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { left->buffer, (OmniVulkanDeviceSize)left_byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[2].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

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
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(output_element_count, local_size, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
