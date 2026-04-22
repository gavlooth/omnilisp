#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_map_f32(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    float scalar,
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
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_count > UINT32_MAX || byte_len != element_count * sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 2u || op > 7u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
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
    if (left_required > SIZE_MAX / sizeof(float) || right_required > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left->byte_len < left_required * sizeof(float) || right->byte_len < right_required * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return status;
    }

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
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanMapF32PushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_map_f32_spv_size,
        omni_tensor_vulkan_map_f32_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
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
        0
    };
    if (omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline) != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = {
        OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
        4
    };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo alloc_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &alloc_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo buffer_infos[4] = {
        { left->buffer, 0, left->byte_len },
        { right->buffer, 0, right->byte_len },
        { output->buffer, 0, byte_len },
        { metadata_buffer->buffer, 0, metadata_words * sizeof(uint32_t) }
    };
    OmniVulkanWriteDescriptorSet writes[4];
    for (uint32_t i = 0; i < 4; i++) {
        writes[i].sType = OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        writes[i].pNext = NULL;
        writes[i].dstSet = descriptor_set;
        writes[i].dstBinding = i;
        writes[i].dstArrayElement = 0;
        writes[i].descriptorCount = 1;
        writes[i].descriptorType = OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        writes[i].pImageInfo = NULL;
        writes[i].pBufferInfo = &buffer_infos[i];
        writes[i].pTexelBufferView = NULL;
    }
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
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
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
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

    OmniTensorVulkanMapF32PushConstants push = {
        scalar,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        0
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
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
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
