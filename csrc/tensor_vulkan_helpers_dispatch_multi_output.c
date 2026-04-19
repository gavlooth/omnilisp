#include "tensor_vulkan_helpers_internal.h"
static void omni_tensor_vulkan_cmd_barrier_two_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[2] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        2,
        barriers,
        0,
        NULL
    );
}

static void omni_tensor_vulkan_cmd_barrier_three_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len,
    OmniTensorVulkanBuffer* third_output,
    size_t third_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[3] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            third_output->buffer,
            0,
            (OmniVulkanDeviceSize)third_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        3,
        barriers,
        0,
        NULL
    );
}

void omni_tensor_vulkan_cmd_barrier_four_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len,
    OmniTensorVulkanBuffer* third_output,
    size_t third_output_byte_len,
    OmniTensorVulkanBuffer* fourth_output,
    size_t fourth_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[4] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            third_output->buffer,
            0,
            (OmniVulkanDeviceSize)third_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            fourth_output->buffer,
            0,
            (OmniVulkanDeviceSize)fourth_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        4,
        barriers,
        0,
        NULL
    );
}

static int omni_tensor_backend_vulkan_dispatch_two_inputs_two_outputs_f64(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t first_work_item_count,
    size_t second_work_item_count,
    const uint32_t* first_shader_words,
    size_t first_shader_size,
    const uint32_t* second_shader_words,
    size_t second_shader_size,
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
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (first_work_item_count == 0 || second_work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        first_shader_words == NULL || first_shader_size == 0 ||
        second_shader_words == NULL || second_shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (first_work_item_count > UINT32_MAX || second_work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
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

    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule first_shader_module = NULL;
    OmniVulkanShaderModule second_shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline first_pipeline = NULL;
    OmniVulkanPipeline second_pipeline = NULL;
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
        (uint32_t)push_size
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

    OmniVulkanShaderModuleCreateInfo first_shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        first_shader_size,
        first_shader_words
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &first_shader_info, NULL, &first_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || first_shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo second_shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        second_shader_size,
        second_shader_words
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    shader_result = omni_vulkan_create_shader_module(device, &second_shader_info, NULL, &second_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || second_shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo first_stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        first_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo first_pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        first_stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &first_pipeline_info, NULL, &first_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || first_pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo second_stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        second_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo second_pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        second_stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &second_pipeline_info, NULL, &second_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || second_pipeline == NULL) {
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
    OmniVulkanDescriptorBufferInfo first_output_info = { first_output->buffer, 0, (OmniVulkanDeviceSize)first_output_byte_len };
    OmniVulkanDescriptorBufferInfo second_output_info = { second_output->buffer, 0, (OmniVulkanDeviceSize)second_output_byte_len };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &first_output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &second_output_info, NULL }
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

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, first_pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)push_size, push_data);
    uint32_t first_group_count = ((uint32_t)first_work_item_count + local_size - 1u) / local_size;
    omni_vulkan_cmd_dispatch(command_buffer, first_group_count, 1, 1);

    OmniVulkanBufferMemoryBarrier barriers[2] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        2,
        barriers,
        0,
        NULL
    );

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, second_pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)push_size, push_data);
    uint32_t second_group_count = ((uint32_t)second_work_item_count + local_size - 1u) / local_size;
    omni_vulkan_cmd_dispatch(command_buffer, second_group_count, 1, 1);
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
    if (second_pipeline != NULL) omni_vulkan_destroy_pipeline(device, second_pipeline, NULL);
    if (first_pipeline != NULL) omni_vulkan_destroy_pipeline(device, first_pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (second_shader_module != NULL) omni_vulkan_destroy_shader_module(device, second_shader_module, NULL);
    if (first_shader_module != NULL) omni_vulkan_destroy_shader_module(device, first_shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
