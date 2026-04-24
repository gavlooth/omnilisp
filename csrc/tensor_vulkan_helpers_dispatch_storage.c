#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) {
    if (out_first_device_ptr == NULL || out_second_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)first_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)second_output_byte_len }
    };
    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = first_output->buffer;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[2].buffer = second_output->buffer;

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
    result = omni_tensor_vulkan_group_count_1d(work_item_count, local_size, &group_count);
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
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t third_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_first_device_ptr,
    void** out_second_device_ptr,
    void** out_third_device_ptr
) {
    if (out_first_device_ptr == NULL || out_second_device_ptr == NULL || out_third_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    *out_third_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0 || third_output_byte_len == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (input_device_ptr == NULL || input_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[4] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)first_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)second_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)third_output_byte_len }
    };
    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = first_output->buffer;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[2].buffer = second_output->buffer;

    OmniTensorVulkanBuffer* third_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, third_output_byte_len, &third_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[3].buffer = third_output->buffer;

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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 4, &descriptors);
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

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 4, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(work_item_count, local_size, &group_count);
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
        omni_tensor_backend_vulkan_destroy_buffer_handle(third_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    *out_third_device_ptr = third_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

void omni_tensor_vulkan_destroy_storage_descriptor_resources(
    OmniVulkanDevice device,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (device == NULL || resources == NULL) return;
    if (resources->pool != NULL) omni_vulkan_destroy_descriptor_pool(device, resources->pool, NULL);
    if (resources->layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, resources->layout, NULL);
    resources->layout = NULL;
    resources->pool = NULL;
    resources->set = NULL;
}

int omni_tensor_vulkan_create_storage_descriptor_set_layout(
    OmniVulkanDevice device,
    uint32_t binding_count,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (resources == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    resources->layout = NULL;
    resources->pool = NULL;
    resources->set = NULL;
    if (device == NULL || binding_count == 0 || binding_count > OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    for (uint32_t i = 0; i < binding_count; i++) {
        bindings[i] = (OmniVulkanDescriptorSetLayoutBinding){
            i,
            OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            1,
            OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
            NULL
        };
    }

    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        binding_count,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &resources->layout) != OMNI_VULKAN_SUCCESS ||
        resources->layout == NULL) {
        omni_tensor_vulkan_destroy_storage_descriptor_resources(device, resources);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_vulkan_allocate_storage_descriptor_set(
    OmniVulkanDevice device,
    const OmniTensorVulkanStorageBufferDescriptor* descriptors,
    uint32_t descriptor_count,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (device == NULL || descriptors == NULL || resources == NULL || resources->layout == NULL ||
        descriptor_count == 0 || descriptor_count > OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDescriptorBufferInfo buffer_infos[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    OmniVulkanWriteDescriptorSet writes[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    for (uint32_t i = 0; i < descriptor_count; i++) {
        if (descriptors[i].buffer == NULL || descriptors[i].range == 0) {
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        buffer_infos[i] = (OmniVulkanDescriptorBufferInfo){
            descriptors[i].buffer,
            0,
            descriptors[i].range
        };
    }

    OmniVulkanDescriptorPoolSize pool_size = {
        OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
        descriptor_count
    };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &resources->pool) != OMNI_VULKAN_SUCCESS ||
        resources->pool == NULL) {
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        resources->pool,
        1,
        &resources->layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &resources->set) != OMNI_VULKAN_SUCCESS ||
        resources->set == NULL) {
        if (resources->pool != NULL) omni_vulkan_destroy_descriptor_pool(device, resources->pool, NULL);
        resources->pool = NULL;
        resources->set = NULL;
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    for (uint32_t i = 0; i < descriptor_count; i++) {
        writes[i] = (OmniVulkanWriteDescriptorSet){
            OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
            NULL,
            resources->set,
            i,
            0,
            1,
            OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            NULL,
            &buffer_infos[i],
            NULL
        };
    }
    omni_vulkan_update_descriptor_sets(device, descriptor_count, writes, 0, NULL);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_vulkan_record_submit_single_dispatch(
    OmniVulkanDevice device,
    OmniVulkanQueue queue,
    uint32_t queue_family_index,
    OmniVulkanPipeline pipeline,
    OmniVulkanPipelineLayout pipeline_layout,
    OmniVulkanDescriptorSet descriptor_set,
    const void* push_data,
    uint32_t push_size,
    uint32_t group_count
) {
    if (device == NULL || queue == NULL || pipeline == NULL || pipeline_layout == NULL ||
        descriptor_set == NULL || push_data == NULL || push_size == 0 || group_count == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS ||
        command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS ||
        command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, push_size, push_data);
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    return result;
}

int omni_tensor_vulkan_create_compute_pipeline_for_shader(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const uint32_t* shader_words,
    size_t shader_size,
    OmniVulkanShaderModule* out_shader_module,
    OmniVulkanPipeline* out_pipeline
) {
    if (device == NULL || pipeline_layout == NULL ||
        shader_words == NULL || shader_size == 0 ||
        out_shader_module == NULL || out_pipeline == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_shader_module = NULL;
    *out_pipeline = NULL;

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        shader_size,
        shader_words
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, out_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || *out_shader_module == NULL) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        *out_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, out_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || *out_pipeline == NULL) {
        omni_vulkan_destroy_shader_module(device, *out_shader_module, NULL);
        *out_shader_module = NULL;
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
