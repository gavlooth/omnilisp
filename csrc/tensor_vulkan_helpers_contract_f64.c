#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_contract_f64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX ||
        axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_count > SIZE_MAX / sizeof(double) || right_count > SIZE_MAX / sizeof(double) ||
        out_element_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t contracted_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[left_axis] != 0 && contracted_count > SIZE_MAX / left_shape[left_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        contracted_count *= left_shape[left_axis];
    }
    if (contracted_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    if (out_rank != expected_rank) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t result_axis = 0;
    size_t expected_count = 1;
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (left_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (right_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t left_byte_len = left_count * sizeof(double);
    size_t right_byte_len = right_count * sizeof(double);
    size_t out_byte_len = out_element_count * sizeof(double);

    if (out_element_count == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (contracted_count == 0) {
        OmniTensorVulkanBuffer* output = NULL;
        int status = omni_tensor_backend_vulkan_create_buffer(out_byte_len, &output);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        if (output == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        OmniVulkanDevice zero_device = output->context != NULL ? output->context->device : NULL;
        if (zero_device == NULL || output->memory == NULL || output->byte_len < out_byte_len) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        void* mapped_zero = NULL;
        if (omni_vulkan_map_memory(zero_device, output->memory, 0, (OmniVulkanDeviceSize)out_byte_len, 0, &mapped_zero) != OMNI_VULKAN_SUCCESS || mapped_zero == NULL) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_COPY_FAILED;
        }
        memset(mapped_zero, 0, out_byte_len);
        omni_vulkan_unmap_memory(zero_device, output->memory);
        *out_device_ptr = output;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

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

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
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
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_axes, axis_count);
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
        (uint32_t)sizeof(OmniTensorVulkanContractPushConstants)
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
        omni_tensor_vulkan_contract_f64_spv_size,
        omni_tensor_vulkan_contract_f64_spv
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
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
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

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
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

    OmniTensorVulkanContractPushConstants push = {
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE;
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
