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
