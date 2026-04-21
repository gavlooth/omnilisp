#include "tensor_vulkan_helpers_internal.h"

long g_omni_tensor_vulkan_map_chain2_dispatch_call_count = 0;

long omni_tensor_backend_vulkan_map_chain2_dispatch_call_count(void) {
    return g_omni_tensor_vulkan_map_chain2_dispatch_call_count;
}

static void omni_tensor_vulkan_cmd_barrier_one_buffer(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* buffer,
    size_t byte_len
) {
    OmniVulkanBufferMemoryBarrier barrier = {
        OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
        NULL,
        OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
        OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
        OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
        OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
        buffer->buffer,
        0,
        (OmniVulkanDeviceSize)byte_len
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        1,
        &barrier,
        0,
        NULL
    );
}

static int omni_tensor_vulkan_map_chain2_write_metadata(
    OmniVulkanDevice device,
    OmniTensorVulkanContext* context,
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    OmniTensorVulkanBuffer** out_metadata_buffer
) {
    if (out_metadata_buffer == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_metadata_buffer = NULL;
    if (rank > SIZE_MAX / 6u || rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rank > 0 && (shape == NULL || strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t metadata_words_actual = rank * 6u;
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    if (metadata_words > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    size_t cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, strides, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, strides, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, shape, rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, strides, rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }
    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);
    *out_metadata_buffer = metadata_buffer;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static void omni_tensor_vulkan_map_chain2_write_descriptor_set(
    OmniVulkanDevice device,
    OmniVulkanDescriptorSet set,
    OmniTensorVulkanBuffer* input,
    OmniTensorVulkanBuffer* output,
    OmniTensorVulkanBuffer* metadata,
    size_t byte_len,
    size_t metadata_byte_len
) {
    OmniVulkanDescriptorBufferInfo infos[4] = {
        { input->buffer, 0, (OmniVulkanDeviceSize)input->byte_len },
        { input->buffer, 0, (OmniVulkanDeviceSize)input->byte_len },
        { output->buffer, 0, (OmniVulkanDeviceSize)byte_len },
        { metadata->buffer, 0, (OmniVulkanDeviceSize)metadata_byte_len }
    };
    OmniVulkanWriteDescriptorSet writes[4];
    for (uint32_t i = 0; i < 4; i++) {
        writes[i].sType = OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        writes[i].pNext = NULL;
        writes[i].dstSet = set;
        writes[i].dstBinding = i;
        writes[i].dstArrayElement = 0;
        writes[i].descriptorCount = 1;
        writes[i].descriptorType = OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        writes[i].pImageInfo = NULL;
        writes[i].pBufferInfo = &infos[i];
        writes[i].pTexelBufferView = NULL;
    }
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);
}

int omni_tensor_backend_vulkan_map_chain2_scalar_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    float first_scalar,
    uint32_t first_mode,
    uint32_t first_op,
    float second_scalar,
    uint32_t second_mode,
    uint32_t second_op,
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || byte_len != element_count * sizeof(float) || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((first_mode != 0u && first_mode != 1u) || (second_mode != 0u && second_mode != 1u) || first_op > 5u || second_op > 5u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t required = 0;
    int status = omni_tensor_vulkan_dense_required_elements(rank, shape, strides, &required);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (required > SIZE_MAX / sizeof(float) || required * sizeof(float) > byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* intermediate = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &intermediate);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    OmniTensorVulkanBuffer* output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(intermediate);
        return status;
    }
    OmniTensorVulkanBuffer* metadata = NULL;
    status = omni_tensor_vulkan_map_chain2_write_metadata(device, context, rank, shape, strides, &metadata);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(intermediate);
        return status;
    }
    size_t metadata_byte_len = (rank == 0 ? 1 : rank * 6u) * sizeof(uint32_t);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_sets[2] = { NULL, NULL };
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
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO, NULL, 0, 4, bindings };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = { OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(OmniTensorVulkanMapF32PushConstants) };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = { OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO, NULL, 0, 1, &descriptor_set_layout, 1, &push_range };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, pipeline_layout, omni_tensor_vulkan_map_f32_spv, omni_tensor_vulkan_map_f32_spv_size, &shader_module, &pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 8 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO, NULL, 0, 2, 1, &pool_size };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanDescriptorSetLayout set_layouts[2] = { descriptor_set_layout, descriptor_set_layout };
    OmniVulkanDescriptorSetAllocateInfo alloc_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO, NULL, descriptor_pool, 2, set_layouts };
    if (omni_vulkan_allocate_descriptor_sets(device, &alloc_info, descriptor_sets) != OMNI_VULKAN_SUCCESS || descriptor_sets[0] == NULL || descriptor_sets[1] == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    omni_tensor_vulkan_map_chain2_write_descriptor_set(device, descriptor_sets[0], input, intermediate, metadata, byte_len, metadata_byte_len);
    omni_tensor_vulkan_map_chain2_write_descriptor_set(device, descriptor_sets[1], intermediate, output, metadata, byte_len, metadata_byte_len);

    OmniVulkanCommandPoolCreateInfo command_pool_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO, NULL, 0, context->queue_family_index };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferAllocateInfo command_buffer_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO, NULL, command_pool, OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY, 1 };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferBeginInfo begin_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO, NULL, OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, NULL };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    uint32_t first_left_rank = first_mode == 1u ? 0u : (uint32_t)rank;
    uint32_t first_right_rank = first_mode == 0u ? 0u : (uint32_t)rank;
    OmniTensorVulkanMapF32PushConstants first_push = { first_scalar, (uint32_t)element_count, first_op, first_mode, (uint32_t)rank, first_left_rank, first_right_rank, 0 };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_sets[0], 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(first_push), &first_push);
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    omni_tensor_vulkan_cmd_barrier_one_buffer(command_buffer, intermediate, byte_len);
    uint32_t second_left_rank = second_mode == 1u ? 0u : (uint32_t)rank;
    uint32_t second_right_rank = second_mode == 0u ? 0u : (uint32_t)rank;
    OmniTensorVulkanMapF32PushConstants second_push = { second_scalar, (uint32_t)element_count, second_op, second_mode, (uint32_t)rank, second_left_rank, second_right_rank, 0 };
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_sets[1], 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(second_push), &second_push);
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = { OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO, NULL, 0, NULL, NULL, 1, &command_buffer, 0, NULL };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    g_omni_tensor_vulkan_map_chain2_dispatch_call_count++;

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata);
    if (intermediate != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(intermediate);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
