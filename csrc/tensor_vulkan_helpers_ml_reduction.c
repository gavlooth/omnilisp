#include "tensor_vulkan_helpers_internal.h"

static int omni_tensor_vulkan_axis_is_listed(size_t axis, const size_t* axes, size_t axis_count) {
    for (size_t i = 0; i < axis_count; i++) {
        if (axes[i] == axis) return 1;
    }
    return 0;
}

static int omni_tensor_backend_vulkan_ml_reduction_typed(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis_count,
    const size_t* axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    uint32_t op,
    size_t element_size,
    int require_float64,
    const uint32_t* shader_words,
    size_t shader_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (require_float64) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (!omni_tensor_backend_vulkan_float32_available()) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (require_float64) {
        if (op > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (op > 4u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (axis_count == 0 || axis_count > input_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axes == NULL) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_rank > 0 && (input_shape == NULL || input_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (input_rank > UINT32_MAX || axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t input_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(input_rank, input_shape, input_strides, &input_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (input_required > SIZE_MAX / element_size || input_element_count > SIZE_MAX / element_size ||
        out_element_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (input_required > input_element_count || input_byte_len < input_required * element_size) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t reduction_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t axis = axes[axis_pos];
        if (axis >= input_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (axes[previous] == axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (input_shape[axis] == 0) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (reduction_count > SIZE_MAX / input_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        reduction_count *= input_shape[axis];
    }
    if (reduction_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = input_rank - axis_count;
    if (out_rank != expected_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t expected_count = expected_rank == 0 ? 1 : 1;
    size_t result_axis = 0;
    for (size_t axis = 0; axis < input_rank; axis++) {
        if (omni_tensor_vulkan_axis_is_listed(axis, axes, axis_count)) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != input_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (input_shape[axis] != 0 && expected_count > SIZE_MAX / input_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= input_shape[axis];
        result_axis++;
    }
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t out_byte_len = out_element_count * element_size;
    if (out_element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || input_rank > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (input_rank * 2u) + axis_count;
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
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_shape, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_strides, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, axes, axis_count);
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

    OmniVulkanDescriptorSetLayoutBinding bindings[3] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        3,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanMlReductionPushConstants)
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
        shader_size,
        shader_words
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
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 3 };
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

    OmniVulkanDescriptorBufferInfo input_info = { input->buffer, 0, (OmniVulkanDeviceSize)input_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[3] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &input_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 3, writes, 0, NULL);

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

    OmniTensorVulkanMlReductionPushConstants push = {
        (uint32_t)input_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank,
        op,
        (uint32_t)reduction_count
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = 0u;
    result = omni_tensor_vulkan_group_count_1d(out_element_count, OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE, &group_count);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }
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

int omni_tensor_backend_vulkan_ml_reduction_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis_count,
    const size_t* axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    uint32_t op,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_reduction_typed(
        input_device_ptr,
        input_byte_len,
        input_element_count,
        input_rank,
        axis_count,
        axes,
        out_element_count,
        out_rank,
        input_shape,
        input_strides,
        out_shape,
        out_strides,
        op,
        sizeof(double),
        1,
        omni_tensor_vulkan_ml_reduction_f64_spv,
        omni_tensor_vulkan_ml_reduction_f64_spv_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_ml_reduction_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis_count,
    const size_t* axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    uint32_t op,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_reduction_typed(
        input_device_ptr,
        input_byte_len,
        input_element_count,
        input_rank,
        axis_count,
        axes,
        out_element_count,
        out_rank,
        input_shape,
        input_strides,
        out_shape,
        out_strides,
        op,
        sizeof(float),
        0,
        omni_tensor_vulkan_ml_reduction_f32_spv,
        omni_tensor_vulkan_ml_reduction_f32_spv_size,
        out_device_ptr
    );
}
