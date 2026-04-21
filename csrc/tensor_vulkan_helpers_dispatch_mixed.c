#include "tensor_vulkan_helpers_internal.h"

long g_omni_tensor_vulkan_map_view_tensor_scalar_chain_dispatch_call_count = 0;

long omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_dispatch_call_count(void) {
    return g_omni_tensor_vulkan_map_view_tensor_scalar_chain_dispatch_call_count;
}

static void omni_tensor_vulkan_mixed_barrier(
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
    omni_vulkan_cmd_pipeline_barrier(command_buffer, OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT, OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT, 0, 0, NULL, 1, &barrier, 0, NULL);
}

static int omni_tensor_vulkan_mixed_dense_strides(size_t rank, const size_t* shape, size_t* strides, size_t* out_count) {
    if (out_count == NULL || (rank > 0 && (shape == NULL || strides == NULL))) return OMNI_TENSOR_VULKAN_INVALID;
    size_t count = 1;
    for (size_t cursor = rank; cursor > 0; cursor--) {
        size_t axis = cursor - 1u;
        strides[axis] = count;
        if (shape[axis] != 0 && count > SIZE_MAX / shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        count *= shape[axis];
    }
    *out_count = count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_mixed_metadata(
    OmniVulkanDevice device,
    OmniTensorVulkanContext* context,
    size_t out_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    size_t left_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    size_t right_rank,
    const size_t* right_shape,
    const size_t* right_strides,
    OmniTensorVulkanBuffer** out_buffer,
    size_t* out_byte_len
) {
    if (out_buffer == NULL || out_byte_len == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_buffer = NULL;
    *out_byte_len = 0;
    if (out_rank > UINT32_MAX || left_rank > UINT32_MAX || right_rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((out_rank > 0 && (out_shape == NULL || out_strides == NULL)) ||
        (left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t words_actual = out_rank * 2u;
    if (words_actual > SIZE_MAX - left_rank * 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    words_actual += left_rank * 2u;
    if (words_actual > SIZE_MAX - right_rank * 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    words_actual += right_rank * 2u;
    size_t words = words_actual == 0 ? 1 : words_actual;
    if (words > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t byte_len = words * sizeof(uint32_t);
    uint32_t* metadata = (uint32_t*)calloc(words, sizeof(uint32_t));
    if (metadata == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    size_t cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &cursor, right_strides, right_rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }
    OmniTensorVulkanBuffer* buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }
    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, buffer->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(buffer);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, metadata, byte_len);
    omni_vulkan_unmap_memory(device, buffer->memory);
    free(metadata);
    *out_buffer = buffer;
    *out_byte_len = byte_len;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static void omni_tensor_vulkan_mixed_write_set(
    OmniVulkanDevice device,
    OmniVulkanDescriptorSet set,
    OmniTensorVulkanBuffer* left,
    OmniTensorVulkanBuffer* right,
    OmniTensorVulkanBuffer* output,
    OmniTensorVulkanBuffer* metadata,
    size_t output_byte_len,
    size_t metadata_byte_len
) {
    if (right == NULL) right = left;
    OmniVulkanDescriptorBufferInfo infos[4] = {
        { left->buffer, 0, (OmniVulkanDeviceSize)left->byte_len },
        { right->buffer, 0, (OmniVulkanDeviceSize)right->byte_len },
        { output->buffer, 0, (OmniVulkanDeviceSize)output_byte_len },
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

int omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_f32(
    const void* view_device_ptr,
    const void* right_device_ptr,
    size_t view_byte_len,
    size_t right_byte_len,
    size_t element_count,
    uint32_t first_op,
    size_t scalar_op_count,
    const float* scalars,
    const uint32_t* modes,
    const uint32_t* ops,
    size_t rank,
    const size_t* shape,
    const size_t* view_strides,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (element_count == 0 || view_byte_len == 0 || right_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (view_device_ptr == NULL || right_device_ptr == NULL || element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_byte_len = element_count * sizeof(float);
    if (first_op > 5u || scalar_op_count < 1 || scalar_op_count > UINT32_MAX || scalars == NULL || modes == NULL || ops == NULL) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (scalar_op_count + 1u < scalar_op_count || scalar_op_count + 1u > UINT32_MAX / 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rank == 0 || rank > UINT32_MAX || shape == NULL || view_strides == NULL || right_strides == NULL) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    for (size_t i = 0; i < scalar_op_count; i++) {
        if ((modes[i] != 0u && modes[i] != 1u) || ops[i] > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t* dense_strides = (size_t*)calloc(rank, sizeof(size_t));
    if (dense_strides == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    size_t dense_count = 0;
    int status = omni_tensor_vulkan_mixed_dense_strides(rank, shape, dense_strides, &dense_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(dense_strides);
        return status;
    }
    if (dense_count != element_count) {
        free(dense_strides);
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    for (size_t i = 0; i < rank; i++) {
        if (right_strides[i] != dense_strides[i]) {
            free(dense_strides);
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
    }
    size_t left_required = 0;
    status = omni_tensor_vulkan_dense_required_elements(rank, shape, view_strides, &left_required);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        size_t right_required = 0;
        status = omni_tensor_vulkan_dense_required_elements(rank, shape, right_strides, &right_required);
        if (status == OMNI_TENSOR_VULKAN_SUCCESS && right_required != element_count) status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(dense_strides);
        return status;
    }
    if (left_required > SIZE_MAX / sizeof(float) || left_required * sizeof(float) > view_byte_len || right_byte_len < output_byte_len) {
        free(dense_strides);
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)view_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL || left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < view_byte_len || right->byte_len < right_byte_len || right->context != context) {
        free(dense_strides);
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;
    size_t dispatch_count = scalar_op_count + 1u;
    if (dispatch_count > SIZE_MAX / sizeof(OmniTensorVulkanBuffer*) || dispatch_count > SIZE_MAX / sizeof(OmniVulkanDescriptorSet)) {
        free(dense_strides);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer** outputs = (OmniTensorVulkanBuffer**)calloc(dispatch_count, sizeof(OmniTensorVulkanBuffer*));
    OmniTensorVulkanBuffer** metadata = (OmniTensorVulkanBuffer**)calloc(dispatch_count, sizeof(OmniTensorVulkanBuffer*));
    size_t* metadata_lens = (size_t*)calloc(dispatch_count, sizeof(size_t));
    if (outputs == NULL || metadata == NULL || metadata_lens == NULL) {
        free(dense_strides);
        free(outputs);
        free(metadata);
        free(metadata_lens);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    for (size_t i = 0; i < dispatch_count; i++) {
        status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &outputs[i]);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) goto early_cleanup;
        size_t left_rank = rank;
        size_t right_rank = rank;
        const size_t* left_strides = i == 0 ? view_strides : dense_strides;
        const size_t* operand_right_strides = i == 0 ? right_strides : dense_strides;
        if (i > 0 && modes[i - 1u] == 1u) left_rank = 0;
        if (i > 0 && modes[i - 1u] == 0u) right_rank = 0;
        status = omni_tensor_vulkan_mixed_metadata(device, context, rank, shape, dense_strides, left_rank, left_rank == 0 ? NULL : shape, left_rank == 0 ? NULL : left_strides, right_rank, right_rank == 0 ? NULL : shape, right_rank == 0 ? NULL : operand_right_strides, &metadata[i], &metadata_lens[i]);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) goto early_cleanup;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet* descriptor_sets = (OmniVulkanDescriptorSet*)calloc(dispatch_count, sizeof(OmniVulkanDescriptorSet));
    OmniVulkanDescriptorSetLayout* set_layouts = (OmniVulkanDescriptorSetLayout*)calloc(dispatch_count, sizeof(OmniVulkanDescriptorSetLayout));
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;
    OmniTensorVulkanBuffer* transferred_output = NULL;
    if (descriptor_sets == NULL || set_layouts == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
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
    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, (uint32_t)(dispatch_count * 4u) };
    OmniVulkanDescriptorPoolCreateInfo pool_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO, NULL, 0, (uint32_t)dispatch_count, 1, &pool_size };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    for (size_t i = 0; i < dispatch_count; i++) set_layouts[i] = descriptor_set_layout;
    OmniVulkanDescriptorSetAllocateInfo alloc_info = { OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO, NULL, descriptor_pool, (uint32_t)dispatch_count, set_layouts };
    if (omni_vulkan_allocate_descriptor_sets(device, &alloc_info, descriptor_sets) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    for (size_t i = 0; i < dispatch_count; i++) {
        if (descriptor_sets[i] == NULL) {
            result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
            goto cleanup;
        }
        OmniTensorVulkanBuffer* dispatch_left = i == 0 ? left : outputs[i - 1u];
        OmniTensorVulkanBuffer* dispatch_right = i == 0 ? right : NULL;
        omni_tensor_vulkan_mixed_write_set(device, descriptor_sets[i], dispatch_left, dispatch_right, outputs[i], metadata[i], output_byte_len, metadata_lens[i]);
    }
    OmniVulkanCommandPoolCreateInfo pool_cmd = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO, NULL, 0, context->queue_family_index };
    if (omni_vulkan_create_command_pool(device, &pool_cmd, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferAllocateInfo cmd_info = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO, NULL, command_pool, OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY, 1 };
    if (omni_vulkan_allocate_command_buffers(device, &cmd_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    OmniVulkanCommandBufferBeginInfo begin = { OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO, NULL, OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, NULL };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    uint32_t groups = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    for (size_t i = 0; i < dispatch_count; i++) {
        uint32_t mode = i == 0 ? 2u : modes[i - 1u];
        uint32_t op = i == 0 ? first_op : ops[i - 1u];
        float scalar = i == 0 ? 0.0f : scalars[i - 1u];
        uint32_t left_rank = mode == 1u ? 0u : (uint32_t)rank;
        uint32_t right_rank = mode == 0u ? 0u : (uint32_t)rank;
        OmniTensorVulkanMapF32PushConstants push = { scalar, (uint32_t)element_count, op, mode, (uint32_t)rank, left_rank, right_rank, 0 };
        omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_sets[i], 0, NULL);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
        omni_vulkan_cmd_dispatch(command_buffer, groups, 1, 1);
        if (i + 1u < dispatch_count) omni_tensor_vulkan_mixed_barrier(command_buffer, outputs[i], output_byte_len);
    }
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    OmniVulkanSubmitInfo submit = { OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO, NULL, 0, NULL, NULL, 1, &command_buffer, 0, NULL };
    if (omni_vulkan_queue_submit(queue, 1, &submit, NULL) != OMNI_VULKAN_SUCCESS || omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }
    g_omni_tensor_vulkan_map_view_tensor_scalar_chain_dispatch_call_count++;

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (outputs != NULL) {
        if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
            transferred_output = outputs[dispatch_count - 1u];
            outputs[dispatch_count - 1u] = NULL;
        }
        for (size_t i = 0; i < dispatch_count; i++) {
            if (outputs[i] != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(outputs[i]);
        }
    }
    if (metadata != NULL) {
        for (size_t i = 0; i < dispatch_count; i++) {
            if (metadata[i] != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata[i]);
        }
    }
    free(outputs);
    free(metadata);
    free(metadata_lens);
    free(descriptor_sets);
    free(set_layouts);
    free(dense_strides);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;
    *out_device_ptr = transferred_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;

early_cleanup:
    for (size_t i = 0; i < dispatch_count; i++) {
        if (outputs[i] != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(outputs[i]);
        if (metadata[i] != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata[i]);
    }
    free(outputs);
    free(metadata);
    free(metadata_lens);
    free(dense_strides);
    return status;
}
