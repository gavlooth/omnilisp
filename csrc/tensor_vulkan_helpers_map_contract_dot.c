#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_contract_dot_f64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(double);
    if (byte_len / sizeof(double) != element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < byte_len || right->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(double), &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

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
        (uint32_t)sizeof(OmniTensorVulkanDotPushConstants)
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
        omni_tensor_vulkan_contract_dot_f64_spv_size,
        omni_tensor_vulkan_contract_dot_f64_spv
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

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)sizeof(double) };
    OmniVulkanWriteDescriptorSet writes[3] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL }
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

    OmniTensorVulkanDotPushConstants push = { (uint32_t)element_count };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    omni_vulkan_cmd_dispatch(command_buffer, 1, 1, 1);
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
