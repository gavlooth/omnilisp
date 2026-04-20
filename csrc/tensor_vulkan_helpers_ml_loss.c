#include "tensor_vulkan_helpers_internal.h"
#include <math.h>

#define OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE 256u
#define OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_CHUNK_SIZE 256u

static size_t omni_tensor_vulkan_mse_chunk_count(size_t count) {
    return (count + OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE;
}

static size_t omni_tensor_vulkan_cross_entropy_chunk_count(size_t count) {
    return (count + OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_CHUNK_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_CHUNK_SIZE;
}

static int omni_tensor_vulkan_mse_validate_f32_result(void* result_device_ptr) {
    float result_value = 0.0f;
    int copy_status = omni_tensor_backend_vulkan_copy_to_host(
        result_device_ptr,
        sizeof(result_value),
        &result_value
    );
    if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) return copy_status;
    if (!isfinite(result_value)) return OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_cross_entropy_validate_f32_result(void* result_device_ptr) {
    float status_value = 0.0f;
    int status_copy = omni_tensor_backend_vulkan_copy_range_to_host(
        result_device_ptr,
        sizeof(float),
        sizeof(status_value),
        &status_value
    );
    if (status_copy != OMNI_TENSOR_VULKAN_SUCCESS) return status_copy;
    if (status_value < 0.0f) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;

    float result_value = 0.0f;
    int copy_status = omni_tensor_backend_vulkan_copy_to_host(
        result_device_ptr,
        sizeof(result_value),
        &result_value
    );
    if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) return copy_status;
    if (!isfinite(result_value)) return OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_dispatch_cross_entropy_f32(
    const OmniTensorVulkanBuffer* logits,
    size_t logits_byte_len,
    const OmniTensorVulkanBuffer* targets,
    size_t targets_byte_len,
    const OmniTensorVulkanBuffer* metadata,
    size_t metadata_byte_len,
    const void* push_data,
    size_t push_size,
    size_t output_byte_len,
    size_t output_element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (logits == NULL || targets == NULL || metadata == NULL ||
        logits_byte_len == 0 || targets_byte_len == 0 || metadata_byte_len == 0 ||
        push_data == NULL || push_size == 0 || push_size > UINT32_MAX ||
        output_byte_len == 0 || output_element_count == 0 || output_element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > SIZE_MAX / (2u * sizeof(float))) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_byte_len < output_element_count * 2u * sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanContext* context = logits->context;
    if (context == NULL || context->device == NULL ||
        targets->context != context || metadata->context != context ||
        logits->buffer == NULL || targets->buffer == NULL || metadata->buffer == NULL ||
        logits->byte_len < logits_byte_len ||
        targets->byte_len < targets_byte_len ||
        metadata->byte_len < metadata_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDevice device = context->device;
    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[4] = {
        { logits->buffer, (OmniVulkanDeviceSize)logits_byte_len },
        { targets->buffer, (OmniVulkanDeviceSize)targets_byte_len },
        { metadata->buffer, (OmniVulkanDeviceSize)metadata_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[3].buffer = output->buffer;

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

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 4, &descriptors);
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
        omni_tensor_vulkan_ml_cross_entropy_f32_spv,
        omni_tensor_vulkan_ml_cross_entropy_f32_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 4, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)output_element_count + OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_LOCAL_SIZE;
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

static int omni_tensor_vulkan_cross_entropy_reduce_partials(
    void* partial_device_ptr,
    size_t partial_count,
    size_t slice_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (partial_device_ptr == NULL || partial_count == 0 || slice_count == 0 || slice_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    void* current_device_ptr = partial_device_ptr;
    size_t current_count = partial_count;
    while (current_count > 1) {
        size_t next_count = omni_tensor_vulkan_cross_entropy_chunk_count(current_count);
        if (next_count == 0 || next_count > UINT32_MAX ||
            current_count > SIZE_MAX / (2u * sizeof(float)) ||
            next_count > SIZE_MAX / (2u * sizeof(float))) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }

        OmniTensorVulkanMapUnaryPushConstants push = {
            (uint32_t)current_count,
            next_count == 1 ? 1u : 0u,
            OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_CHUNK_SIZE,
            (uint32_t)slice_count
        };
        void* next_device_ptr = NULL;
        int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
            current_device_ptr,
            current_count * 2u * sizeof(float),
            next_count * 2u * sizeof(float),
            next_count,
            omni_tensor_vulkan_ml_cross_entropy_reduce_f32_spv,
            omni_tensor_vulkan_ml_cross_entropy_reduce_f32_spv_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_CROSS_ENTROPY_LOCAL_SIZE,
            &next_device_ptr
        );
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        current_device_ptr = next_device_ptr;
        current_count = next_count;
    }

    *out_device_ptr = current_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_mse_reduce_partials(
    void* partial_device_ptr,
    size_t partial_count,
    size_t element_count,
    size_t element_size,
    int require_float64,
    const uint32_t* reduce_shader_words,
    size_t reduce_shader_size,
    void** out_device_ptr
) {
    void* current_device_ptr = partial_device_ptr;
    size_t current_count = partial_count;
    while (current_count > 1) {
        size_t next_count = omni_tensor_vulkan_mse_chunk_count(current_count);
        if (next_count == 0 || next_count > UINT32_MAX || next_count > SIZE_MAX / element_size) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        OmniTensorVulkanMapUnaryPushConstants push = {
            (uint32_t)current_count,
            next_count == 1 ? 1u : 0u,
            OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE,
            (uint32_t)element_count
        };
        void* next_device_ptr = NULL;
        int status = require_float64
            ? omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
                current_device_ptr,
                current_count * element_size,
                next_count * element_size,
                next_count,
                reduce_shader_words,
                reduce_shader_size,
                &push,
                sizeof(push),
                OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
                &next_device_ptr
            )
            : omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
                current_device_ptr,
                current_count * element_size,
                next_count * element_size,
                next_count,
                reduce_shader_words,
                reduce_shader_size,
                &push,
                sizeof(push),
                OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
                &next_device_ptr
            );
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)current_device_ptr);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        current_device_ptr = next_device_ptr;
        current_count = next_count;
    }

    *out_device_ptr = current_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_ml_mse_typed(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    size_t element_size,
    int require_float64,
    const uint32_t* shader_words,
    size_t shader_size,
    const uint32_t* reduce_shader_words,
    size_t reduce_shader_size,
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
    if (element_count == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t required_bytes = element_count * element_size;
    if (predictions_byte_len < required_bytes || targets_byte_len < required_bytes) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t partial_count = omni_tensor_vulkan_mse_chunk_count(element_count);
    if (partial_count == 0 || partial_count > UINT32_MAX || partial_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        partial_count == 1 ? 1u : 0u,
        OMNI_TENSOR_VULKAN_ML_MSE_CHUNK_SIZE,
        0u
    };
    void* partial_device_ptr = NULL;
    int status = require_float64
        ? omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            partial_count * element_size,
            partial_count,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
            &partial_device_ptr
        )
        : omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
            predictions_device_ptr,
            predictions_byte_len,
            targets_device_ptr,
            targets_byte_len,
            partial_count * element_size,
            partial_count,
            shader_words,
            shader_size,
            &push,
            sizeof(push),
            OMNI_TENSOR_VULKAN_ML_MSE_LOCAL_SIZE,
            &partial_device_ptr
        );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    void* result_device_ptr = NULL;
    if (partial_count == 1) {
        result_device_ptr = partial_device_ptr;
    } else {
        status = omni_tensor_vulkan_mse_reduce_partials(
            partial_device_ptr,
            partial_count,
            element_count,
            element_size,
            require_float64,
            reduce_shader_words,
            reduce_shader_size,
            &result_device_ptr
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    }

    if (!require_float64) {
        status = omni_tensor_vulkan_mse_validate_f32_result(result_device_ptr);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)result_device_ptr);
            return status;
        }
    }

    *out_device_ptr = result_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_ml_mse_f64(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_mse_typed(
        predictions_device_ptr,
        predictions_byte_len,
        targets_device_ptr,
        targets_byte_len,
        element_count,
        sizeof(double),
        1,
        omni_tensor_vulkan_ml_mse_f64_spv,
        omni_tensor_vulkan_ml_mse_f64_spv_size,
        omni_tensor_vulkan_ml_mse_reduce_f64_spv,
        omni_tensor_vulkan_ml_mse_reduce_f64_spv_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_ml_mse_f32(
    const void* predictions_device_ptr,
    size_t predictions_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    return omni_tensor_backend_vulkan_ml_mse_typed(
        predictions_device_ptr,
        predictions_byte_len,
        targets_device_ptr,
        targets_byte_len,
        element_count,
        sizeof(float),
        0,
        omni_tensor_vulkan_ml_mse_f32_spv,
        omni_tensor_vulkan_ml_mse_f32_spv_size,
        omni_tensor_vulkan_ml_mse_reduce_f32_spv,
        omni_tensor_vulkan_ml_mse_reduce_f32_spv_size,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_ml_cross_entropy_f32(
    const void* logits_device_ptr,
    size_t logits_byte_len,
    const void* targets_device_ptr,
    size_t targets_byte_len,
    size_t element_count,
    size_t input_rank,
    size_t axis,
    const size_t* input_shape,
    const size_t* logits_strides,
    const size_t* targets_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (logits_device_ptr == NULL || targets_device_ptr == NULL ||
        input_shape == NULL || logits_strides == NULL || targets_strides == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (input_rank == 0 || axis >= input_rank || input_rank > UINT32_MAX || element_count == 0 || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_count = 1;
    for (size_t i = 0; i < input_rank; i++) {
        if (input_shape[i] > UINT32_MAX || logits_strides[i] > UINT32_MAX || targets_strides[i] > UINT32_MAX) {
            return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (input_shape[i] != 0 && expected_count > SIZE_MAX / input_shape[i]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= input_shape[i];
    }
    if (expected_count != element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t reduction_count = input_shape[axis];
    if (reduction_count == 0 || reduction_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t slice_count = element_count / reduction_count;
    if (slice_count == 0 || slice_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t logits_required = 0;
    int status = omni_tensor_vulkan_dense_required_elements(input_rank, input_shape, logits_strides, &logits_required);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    size_t targets_required = 0;
    status = omni_tensor_vulkan_dense_required_elements(input_rank, input_shape, targets_strides, &targets_required);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (logits_required > SIZE_MAX / sizeof(float) || targets_required > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (logits_byte_len < logits_required * sizeof(float) ||
        targets_byte_len < targets_required * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniTensorVulkanBuffer* logits = (OmniTensorVulkanBuffer*)logits_device_ptr;
    OmniTensorVulkanBuffer* targets = (OmniTensorVulkanBuffer*)targets_device_ptr;
    OmniTensorVulkanContext* context = logits->context;
    if (context == NULL || context->device == NULL ||
        targets->context != context ||
        logits->buffer == NULL || targets->buffer == NULL ||
        logits->byte_len < logits_byte_len || targets->byte_len < targets_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    if (input_rank > SIZE_MAX / 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_words = input_rank * 3u;
    if (metadata_words > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;

    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, input_shape, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, logits_strides, input_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, targets_strides, input_rank);
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

    OmniTensorVulkanMlReductionPushConstants push = {
        (uint32_t)input_rank,
        1u,
        (uint32_t)slice_count,
        0u,
        (uint32_t)axis,
        (uint32_t)reduction_count
    };

    if (slice_count > SIZE_MAX / (2u * sizeof(float))) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    void* partial_device_ptr = NULL;
    status = omni_tensor_vulkan_dispatch_cross_entropy_f32(
        logits,
        logits_byte_len,
        targets,
        targets_byte_len,
        metadata_buffer,
        metadata_words * sizeof(uint32_t),
        &push,
        sizeof(push),
        slice_count * 2u * sizeof(float),
        slice_count,
        &partial_device_ptr
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    void* result_device_ptr = NULL;
    if (slice_count == 1) {
        result_device_ptr = partial_device_ptr;
    } else {
        status = omni_tensor_vulkan_cross_entropy_reduce_partials(
            partial_device_ptr,
            slice_count,
            slice_count,
            &result_device_ptr
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    }

    status = omni_tensor_vulkan_cross_entropy_validate_f32_result(result_device_ptr);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)result_device_ptr);
        return status;
    }

    *out_device_ptr = result_device_ptr;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
