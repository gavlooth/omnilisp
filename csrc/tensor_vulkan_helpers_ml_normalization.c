#include "tensor_vulkan_helpers_internal.h"

int omni_tensor_backend_vulkan_ml_layer_norm_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t axis,
    float epsilon,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_rank == 0 || axis >= input_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_shape == NULL || input_strides == NULL || out_strides == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (!isfinite(epsilon) || epsilon <= 0.0f) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    if (input_rank > UINT32_MAX || input_element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t input_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(input_rank, input_shape, input_strides, &input_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (input_required > SIZE_MAX / sizeof(float) || input_required > input_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_byte_len < input_required * sizeof(float)) return OMNI_TENSOR_VULKAN_INVALID;
    if (input_shape[axis] == 0) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_shape[axis] > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_count = 1;
    for (size_t i = 0; i < input_rank; i++) {
        if (input_shape[i] > UINT32_MAX || input_strides[i] > UINT32_MAX || out_strides[i] > UINT32_MAX) {
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (input_shape[i] != 0 && expected_count > SIZE_MAX / input_shape[i]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= input_shape[i];
    }
    if (expected_count != input_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t out_byte_len = input_element_count * sizeof(float);
    if (input_element_count == 0 || out_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    if (input_rank > SIZE_MAX / 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_words = input_rank * 3u;
    if (metadata_words > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;

    size_t metadata_cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_shape, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_strides, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, input_rank);
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

    status = omni_tensor_backend_vulkan_copy_to_existing_device(metadata, metadata_words * sizeof(uint32_t), metadata_buffer);
    free(metadata);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        return status;
    }

    OmniTensorVulkanMlLayerNormPushConstants push = {
        (uint32_t)input_rank,
        (uint32_t)input_element_count,
        (uint32_t)axis,
        (uint32_t)input_shape[axis],
        epsilon
    };

    status = omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        input_device_ptr,
        input_byte_len,
        metadata_buffer,
        metadata_words * sizeof(uint32_t),
        out_byte_len,
        input_element_count,
        omni_tensor_vulkan_ml_layer_norm_f32_spv,
        omni_tensor_vulkan_ml_layer_norm_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE,
        out_device_ptr
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    return status;
}

static int omni_tensor_backend_vulkan_dispatch_batch_norm_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    const void* scale_device_ptr,
    size_t scale_byte_len,
    const void* bias_device_ptr,
    size_t bias_byte_len,
    const void* mean_device_ptr,
    size_t mean_byte_len,
    const void* variance_device_ptr,
    size_t variance_byte_len,
    const void* metadata_device_ptr,
    size_t metadata_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const void* push_data,
    size_t push_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || scale_device_ptr == NULL || bias_device_ptr == NULL ||
        mean_device_ptr == NULL || variance_device_ptr == NULL || metadata_device_ptr == NULL ||
        input_byte_len == 0 || scale_byte_len == 0 || bias_byte_len == 0 ||
        mean_byte_len == 0 || variance_byte_len == 0 || metadata_byte_len == 0 ||
        push_data == NULL || push_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanBuffer* scale = (OmniTensorVulkanBuffer*)scale_device_ptr;
    OmniTensorVulkanBuffer* bias = (OmniTensorVulkanBuffer*)bias_device_ptr;
    OmniTensorVulkanBuffer* mean = (OmniTensorVulkanBuffer*)mean_device_ptr;
    OmniTensorVulkanBuffer* variance = (OmniTensorVulkanBuffer*)variance_device_ptr;
    OmniTensorVulkanBuffer* metadata = (OmniTensorVulkanBuffer*)metadata_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL ||
        scale->context != context || bias->context != context ||
        mean->context != context || variance->context != context ||
        metadata->context != context ||
        input->buffer == NULL || scale->buffer == NULL || bias->buffer == NULL ||
        mean->buffer == NULL || variance->buffer == NULL || metadata->buffer == NULL ||
        input->byte_len < input_byte_len || scale->byte_len < scale_byte_len ||
        bias->byte_len < bias_byte_len || mean->byte_len < mean_byte_len ||
        variance->byte_len < variance_byte_len || metadata->byte_len < metadata_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDevice device = context->device;
    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[7] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { scale->buffer, (OmniVulkanDeviceSize)scale_byte_len },
        { bias->buffer, (OmniVulkanDeviceSize)bias_byte_len },
        { mean->buffer, (OmniVulkanDeviceSize)mean_byte_len },
        { variance->buffer, (OmniVulkanDeviceSize)variance_byte_len },
        { metadata->buffer, (OmniVulkanDeviceSize)metadata_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[6].buffer = output->buffer;

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
    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 7, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

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
        omni_tensor_vulkan_ml_batch_norm_f32_spv,
        omni_tensor_vulkan_ml_batch_norm_f32_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 7, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)output_element_count + OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE;
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

int omni_tensor_backend_vulkan_ml_batch_norm_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t input_element_count,
    size_t input_rank,
    size_t channel_axis,
    float epsilon,
    const size_t* input_shape,
    const size_t* input_strides,
    const size_t* out_strides,
    const void* scale_device_ptr,
    size_t scale_byte_len,
    const void* bias_device_ptr,
    size_t bias_byte_len,
    const void* mean_device_ptr,
    size_t mean_byte_len,
    const void* variance_device_ptr,
    size_t variance_byte_len,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_rank == 0 || channel_axis >= input_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_shape == NULL || input_strides == NULL || out_strides == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (!isfinite(epsilon) || epsilon <= 0.0f) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    if (input_rank > UINT32_MAX || input_element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_shape[channel_axis] == 0 || input_shape[channel_axis] > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t input_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(input_rank, input_shape, input_strides, &input_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (input_required > SIZE_MAX / sizeof(float) || input_required > input_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (input_byte_len < input_required * sizeof(float)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t channel_dim = input_shape[channel_axis];
    size_t param_byte_len = channel_dim * sizeof(float);
    if (scale_byte_len < param_byte_len || bias_byte_len < param_byte_len ||
        mean_byte_len < param_byte_len || variance_byte_len < param_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t expected_count = 1;
    for (size_t i = 0; i < input_rank; i++) {
        if (input_shape[i] > UINT32_MAX || input_strides[i] > UINT32_MAX || out_strides[i] > UINT32_MAX) {
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (input_shape[i] != 0 && expected_count > SIZE_MAX / input_shape[i]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= input_shape[i];
    }
    if (expected_count != input_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t out_byte_len = input_element_count * sizeof(float);
    if (input_element_count == 0 || out_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    if (input == NULL || input->context == NULL || input->context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    if (input_rank > SIZE_MAX / 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_words = input_rank * 3u;
    if (metadata_words > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;

    size_t metadata_cursor = 0;
    int status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_shape, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_strides, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, input_rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(input->context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        return status;
    }
    status = omni_tensor_backend_vulkan_copy_to_existing_device(metadata, metadata_words * sizeof(uint32_t), metadata_buffer);
    free(metadata);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        return status;
    }

    OmniTensorVulkanMlBatchNormPushConstants push = {
        (uint32_t)input_rank,
        (uint32_t)input_element_count,
        (uint32_t)channel_axis,
        (uint32_t)channel_dim,
        epsilon
    };
    status = omni_tensor_backend_vulkan_dispatch_batch_norm_f32(
        input_device_ptr,
        input_byte_len,
        scale_device_ptr,
        scale_byte_len,
        bias_device_ptr,
        bias_byte_len,
        mean_device_ptr,
        mean_byte_len,
        variance_device_ptr,
        variance_byte_len,
        metadata_buffer,
        metadata_words * sizeof(uint32_t),
        out_byte_len,
        input_element_count,
        &push,
        sizeof(push),
        out_device_ptr
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    return status;
}
