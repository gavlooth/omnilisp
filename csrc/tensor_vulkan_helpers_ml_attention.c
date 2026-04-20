#include "tensor_vulkan_helpers_internal.h"

int omni_tensor_backend_vulkan_ml_attention_f32(
    const void* query_device_ptr,
    size_t query_byte_len,
    const void* key_device_ptr,
    size_t key_byte_len,
    const void* value_device_ptr,
    size_t value_byte_len,
    const void* mask_device_ptr,
    size_t mask_byte_len,
    size_t batch_count,
    size_t query_len,
    size_t key_len,
    size_t head_dim,
    size_t value_dim,
    unsigned int mask_kind,
    float scale,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (query_device_ptr == NULL || key_device_ptr == NULL || value_device_ptr == NULL || mask_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (batch_count == 0 || query_len == 0 || key_len == 0 || head_dim == 0 || value_dim == 0) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mask_kind > 2u || !isfinite(scale) || scale <= 0.0f) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    if (batch_count > UINT32_MAX || query_len > UINT32_MAX || key_len > UINT32_MAX ||
        head_dim > UINT32_MAX || value_dim > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t query_count = 0;
    size_t key_count = 0;
    size_t value_count = 0;
    size_t output_count = 0;
    if (batch_count > SIZE_MAX / query_len) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    query_count = batch_count * query_len;
    if (query_count > SIZE_MAX / head_dim) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    query_count *= head_dim;
    if (query_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (batch_count > SIZE_MAX / key_len) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    key_count = batch_count * key_len;
    if (key_count > SIZE_MAX / head_dim) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    key_count *= head_dim;
    if (key_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    value_count = batch_count * key_len;
    if (value_count > SIZE_MAX / value_dim) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    value_count *= value_dim;
    if (value_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count = batch_count * query_len;
    if (output_count > SIZE_MAX / value_dim || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count *= value_dim;
    if (output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    if (query_count > SIZE_MAX / sizeof(float) || key_count > SIZE_MAX / sizeof(float) ||
        value_count > SIZE_MAX / sizeof(float) || output_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t query_required = query_count * sizeof(float);
    size_t key_required = key_count * sizeof(float);
    size_t value_required = value_count * sizeof(float);
    size_t output_byte_len = output_count * sizeof(float);
    if (query_byte_len < query_required || key_byte_len < key_required || value_byte_len < value_required) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t mask_required = sizeof(float);
    if (mask_kind == 1u) {
        if (query_len > SIZE_MAX / key_len) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        size_t mask_count = query_len * key_len;
        if (mask_count > UINT32_MAX || mask_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        mask_required = mask_count * sizeof(float);
    } else if (mask_kind == 2u) {
        size_t mask_count = batch_count;
        if (mask_count > SIZE_MAX / query_len) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        mask_count *= query_len;
        if (mask_count > SIZE_MAX / key_len) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        mask_count *= key_len;
        if (mask_count > UINT32_MAX || mask_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        mask_required = mask_count * sizeof(float);
    }
    if (mask_byte_len < mask_required) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* query = (OmniTensorVulkanBuffer*)query_device_ptr;
    OmniTensorVulkanBuffer* key = (OmniTensorVulkanBuffer*)key_device_ptr;
    OmniTensorVulkanBuffer* value = (OmniTensorVulkanBuffer*)value_device_ptr;
    OmniTensorVulkanBuffer* mask = (OmniTensorVulkanBuffer*)mask_device_ptr;
    OmniTensorVulkanContext* context = query->context;
    if (context == NULL || context->device == NULL ||
        key->context != context || value->context != context || mask->context != context ||
        query->buffer == NULL || key->buffer == NULL || value->buffer == NULL || mask->buffer == NULL ||
        query->byte_len < query_byte_len || key->byte_len < key_byte_len ||
        value->byte_len < value_byte_len || mask->byte_len < mask_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanMlAttentionPushConstants push = {
        (uint32_t)batch_count,
        (uint32_t)query_len,
        (uint32_t)key_len,
        (uint32_t)head_dim,
        (uint32_t)value_dim,
        (uint32_t)output_count,
        (uint32_t)mask_kind,
        scale
    };

    OmniVulkanDevice device = context->device;
    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[5] = {
        { query->buffer, (OmniVulkanDeviceSize)query_byte_len },
        { key->buffer, (OmniVulkanDeviceSize)key_byte_len },
        { value->buffer, (OmniVulkanDeviceSize)value_byte_len },
        { mask->buffer, (OmniVulkanDeviceSize)mask_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[4].buffer = output->buffer;

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
    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

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
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        omni_tensor_vulkan_ml_attention_f32_spv,
        omni_tensor_vulkan_ml_attention_f32_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    size_t group_count_size = (output_count + OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE;
    if (group_count_size > UINT32_MAX) {
        result = OMNI_TENSOR_VULKAN_UNSUPPORTED;
        goto cleanup;
    }
    uint32_t group_count = (uint32_t)group_count_size;
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
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
