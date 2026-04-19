#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_singular_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_f64(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(double);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(double),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
int omni_tensor_backend_vulkan_singular_norm_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(double),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_complex128(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(double);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(double),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
