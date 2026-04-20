#include "tensor_vulkan_helpers_internal.h"

#define OMNI_TENSOR_VULKAN_ML_SGD_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_ML_ADAM_LOCAL_SIZE 64u

typedef struct OmniTensorVulkanMlSgdPushConstants {
    uint32_t count;
    float learning_rate;
    float momentum;
    float weight_decay;
} OmniTensorVulkanMlSgdPushConstants;

typedef struct OmniTensorVulkanMlAdamPushConstants {
    uint32_t count;
    float learning_rate;
    float beta1;
    float beta2;
    float epsilon;
    float weight_decay;
    float first_correction;
    float second_correction;
    uint32_t decoupled_weight_decay;
} OmniTensorVulkanMlAdamPushConstants;

static int omni_tensor_vulkan_sgd_dispatch_two_outputs_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* velocity_device_ptr,
    size_t velocity_byte_len,
    size_t element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const OmniTensorVulkanMlSgdPushConstants* push,
    void** out_params_device_ptr,
    void** out_velocity_device_ptr
) {
    if (out_params_device_ptr == NULL || out_velocity_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_params_device_ptr = NULL;
    *out_velocity_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (params_device_ptr == NULL || grads_device_ptr == NULL ||
        params_byte_len == 0 || grads_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 || push == NULL ||
        element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t byte_len = element_count * sizeof(float);
    if (params_byte_len < byte_len || grads_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* params = (OmniTensorVulkanBuffer*)params_device_ptr;
    OmniTensorVulkanBuffer* grads = (OmniTensorVulkanBuffer*)grads_device_ptr;
    OmniTensorVulkanBuffer* velocity = (OmniTensorVulkanBuffer*)velocity_device_ptr;
    OmniTensorVulkanContext* context = params->context;
    if (context == NULL || context->device == NULL ||
        grads->context != context ||
        params->buffer == NULL || grads->buffer == NULL ||
        params->byte_len < params_byte_len || grads->byte_len < grads_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (velocity_device_ptr != NULL &&
        (velocity_byte_len < byte_len || velocity->context != context || velocity->buffer == NULL || velocity->byte_len < velocity_byte_len)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanBuffer* output_params = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output_params);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    OmniTensorVulkanBuffer* output_velocity = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output_velocity);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_params);
        return status;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[5] = {
        { params->buffer, (OmniVulkanDeviceSize)params_byte_len },
        { grads->buffer, (OmniVulkanDeviceSize)grads_byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len }
    };
    uint32_t descriptor_count = velocity_device_ptr != NULL ? 5u : 4u;
    if (velocity_device_ptr != NULL) {
        buffer_descriptors[2] = (OmniTensorVulkanStorageBufferDescriptor){ velocity->buffer, (OmniVulkanDeviceSize)velocity_byte_len };
        buffer_descriptors[3].buffer = output_params->buffer;
        buffer_descriptors[4].buffer = output_velocity->buffer;
    } else {
        buffer_descriptors[2].buffer = output_params->buffer;
        buffer_descriptors[3].buffer = output_velocity->buffer;
    }

    OmniVulkanDevice device = context->device;
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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, descriptor_count, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(*push)
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

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, pipeline_layout, shader_words, shader_size, &shader_module, &pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, descriptor_count, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_ML_SGD_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_SGD_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push,
        (uint32_t)sizeof(*push),
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_velocity);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_params);
        return result;
    }

    *out_params_device_ptr = output_params;
    *out_velocity_device_ptr = output_velocity;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_adam_dispatch_three_outputs_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* first_device_ptr,
    size_t first_byte_len,
    const void* second_device_ptr,
    size_t second_byte_len,
    size_t element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const OmniTensorVulkanMlAdamPushConstants* push,
    int has_moments,
    void** out_params_device_ptr,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) {
    if (out_params_device_ptr == NULL || out_first_device_ptr == NULL || out_second_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_params_device_ptr = NULL;
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (params_device_ptr == NULL || grads_device_ptr == NULL ||
        params_byte_len == 0 || grads_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 || push == NULL ||
        element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t byte_len = element_count * sizeof(float);
    if (params_byte_len < byte_len || grads_byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;
    if (has_moments && (first_device_ptr == NULL || second_device_ptr == NULL || first_byte_len < byte_len || second_byte_len < byte_len)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanBuffer* params = (OmniTensorVulkanBuffer*)params_device_ptr;
    OmniTensorVulkanBuffer* grads = (OmniTensorVulkanBuffer*)grads_device_ptr;
    OmniTensorVulkanBuffer* first = (OmniTensorVulkanBuffer*)first_device_ptr;
    OmniTensorVulkanBuffer* second = (OmniTensorVulkanBuffer*)second_device_ptr;
    OmniTensorVulkanContext* context = params->context;
    if (context == NULL || context->device == NULL ||
        grads->context != context ||
        params->buffer == NULL || grads->buffer == NULL ||
        params->byte_len < params_byte_len || grads->byte_len < grads_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (has_moments &&
        (first->context != context || second->context != context ||
         first->buffer == NULL || second->buffer == NULL ||
         first->byte_len < first_byte_len || second->byte_len < second_byte_len)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanBuffer* output_params = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output_params);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    OmniTensorVulkanBuffer* output_first = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output_first);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_params);
        return status;
    }
    OmniTensorVulkanBuffer* output_second = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output_second);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_first);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_params);
        return status;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[7] = {
        { params->buffer, (OmniVulkanDeviceSize)params_byte_len },
        { grads->buffer, (OmniVulkanDeviceSize)grads_byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len },
        { NULL, (OmniVulkanDeviceSize)byte_len }
    };
    uint32_t descriptor_count = has_moments ? 7u : 5u;
    if (has_moments) {
        buffer_descriptors[2] = (OmniTensorVulkanStorageBufferDescriptor){ first->buffer, (OmniVulkanDeviceSize)first_byte_len };
        buffer_descriptors[3] = (OmniTensorVulkanStorageBufferDescriptor){ second->buffer, (OmniVulkanDeviceSize)second_byte_len };
        buffer_descriptors[4].buffer = output_params->buffer;
        buffer_descriptors[5].buffer = output_first->buffer;
        buffer_descriptors[6].buffer = output_second->buffer;
    } else {
        buffer_descriptors[2].buffer = output_params->buffer;
        buffer_descriptors[3].buffer = output_first->buffer;
        buffer_descriptors[4].buffer = output_second->buffer;
    }

    OmniVulkanDevice device = context->device;
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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, descriptor_count, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(*push)
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

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, pipeline_layout, shader_words, shader_size, &shader_module, &pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, descriptor_count, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_ML_ADAM_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_ADAM_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push,
        (uint32_t)sizeof(*push),
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_second);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_first);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output_params);
        return result;
    }

    *out_params_device_ptr = output_params;
    *out_first_device_ptr = output_first;
    *out_second_device_ptr = output_second;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_sgd_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* velocity_device_ptr,
    size_t velocity_byte_len,
    size_t element_count,
    float learning_rate,
    float momentum,
    float weight_decay,
    int has_velocity,
    void** out_params_device_ptr,
    void** out_velocity_device_ptr
) {
    if (out_params_device_ptr == NULL || out_velocity_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_params_device_ptr = NULL;
    *out_velocity_device_ptr = NULL;
    if (!isfinite(learning_rate) || !isfinite(momentum) || !isfinite(weight_decay) || learning_rate <= 0.0f || momentum < 0.0f) {
        return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    }
    if (element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanMlSgdPushConstants push = {
        (uint32_t)element_count,
        learning_rate,
        momentum,
        weight_decay
    };

    if (momentum <= 0.0f) {
        return omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
            params_device_ptr,
            params_byte_len,
            grads_device_ptr,
            grads_byte_len,
            element_count * sizeof(float),
            element_count,
            omni_tensor_vulkan_ml_sgd_f32_spv,
            omni_tensor_vulkan_ml_sgd_f32_spv_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_SGD_LOCAL_SIZE,
            out_params_device_ptr
        );
    }

    if (has_velocity && velocity_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    return omni_tensor_vulkan_sgd_dispatch_two_outputs_f32(
        params_device_ptr,
        params_byte_len,
        grads_device_ptr,
        grads_byte_len,
        has_velocity ? velocity_device_ptr : NULL,
        has_velocity ? velocity_byte_len : 0,
        element_count,
        has_velocity ? omni_tensor_vulkan_ml_sgd_momentum_f32_spv : omni_tensor_vulkan_ml_sgd_init_momentum_f32_spv,
        has_velocity ? omni_tensor_vulkan_ml_sgd_momentum_f32_spv_size : omni_tensor_vulkan_ml_sgd_init_momentum_f32_spv_size,
        &push,
        out_params_device_ptr,
        out_velocity_device_ptr
    );
}

int omni_tensor_backend_vulkan_ml_adam_f32(
    const void* params_device_ptr,
    size_t params_byte_len,
    const void* grads_device_ptr,
    size_t grads_byte_len,
    const void* first_device_ptr,
    size_t first_byte_len,
    const void* second_device_ptr,
    size_t second_byte_len,
    size_t element_count,
    float learning_rate,
    float beta1,
    float beta2,
    float epsilon,
    float weight_decay,
    float first_correction,
    float second_correction,
    int decoupled_weight_decay,
    int has_moments,
    void** out_params_device_ptr,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) {
    if (out_params_device_ptr == NULL || out_first_device_ptr == NULL || out_second_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_params_device_ptr = NULL;
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    if (!isfinite(learning_rate) || !isfinite(beta1) || !isfinite(beta2) || !isfinite(epsilon) ||
        !isfinite(weight_decay) || !isfinite(first_correction) || !isfinite(second_correction) ||
        learning_rate < 0.0f || beta1 < 0.0f || beta1 >= 1.0f || beta2 < 0.0f || beta2 >= 1.0f ||
        epsilon <= 0.0f || weight_decay < 0.0f || first_correction <= 0.0f || second_correction <= 0.0f) {
        return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    }
    if (element_count > UINT32_MAX || element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanMlAdamPushConstants push = {
        (uint32_t)element_count,
        learning_rate,
        beta1,
        beta2,
        epsilon,
        weight_decay,
        first_correction,
        second_correction,
        decoupled_weight_decay ? 1u : 0u
    };

    return omni_tensor_vulkan_adam_dispatch_three_outputs_f32(
        params_device_ptr,
        params_byte_len,
        grads_device_ptr,
        grads_byte_len,
        has_moments ? first_device_ptr : NULL,
        has_moments ? first_byte_len : 0,
        has_moments ? second_device_ptr : NULL,
        has_moments ? second_byte_len : 0,
        element_count,
        has_moments ? omni_tensor_vulkan_ml_adam_f32_spv : omni_tensor_vulkan_ml_adam_init_f32_spv,
        has_moments ? omni_tensor_vulkan_ml_adam_f32_spv_size : omni_tensor_vulkan_ml_adam_init_f32_spv_size,
        &push,
        has_moments,
        out_params_device_ptr,
        out_first_device_ptr,
        out_second_device_ptr
    );
}
