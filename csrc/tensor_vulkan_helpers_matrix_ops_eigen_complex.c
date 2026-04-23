#include "tensor_vulkan_helpers_internal.h"

static int omni_tensor_backend_vulkan_hermitian_eigen_validate_shape_complex(
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    size_t* out_value_storage_count,
    size_t* out_vector_count,
    size_t* out_vector_storage_count
) {
    if (out_value_storage_count == NULL || out_vector_count == NULL ||
        out_vector_storage_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_value_storage_count = 0u;
    *out_vector_count = 0u;
    *out_vector_storage_count = 0u;
    if (scalar_size != sizeof(double) && scalar_size != sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t component_count = element_count * 2u;
    if (component_count > SIZE_MAX / scalar_size || byte_len != component_count * scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX / 64u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t value_storage_count = n + 1u;
    if (element_count > SIZE_MAX / 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t vector_storage_count = element_count * 4u;
    if (value_storage_count > UINT32_MAX || vector_storage_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (value_storage_count > SIZE_MAX / scalar_size ||
        vector_storage_count > SIZE_MAX / scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_value_storage_count = value_storage_count;
    *out_vector_count = element_count;
    *out_vector_storage_count = vector_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_general_eigen_validate_shape_complex(
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    size_t* out_value_count,
    size_t* out_vector_count,
    size_t* out_vector_storage_count,
    size_t* out_max_iterations
) {
    if (out_value_count == NULL || out_vector_count == NULL ||
        out_vector_storage_count == NULL || out_max_iterations == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_value_count = 0u;
    *out_vector_count = 0u;
    *out_vector_storage_count = 0u;
    *out_max_iterations = 0u;
    if (scalar_size != sizeof(double) && scalar_size != sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / 2u || n > SIZE_MAX / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t component_count = element_count * 2u;
    if (component_count > SIZE_MAX / scalar_size ||
        byte_len != component_count * scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / 5u || element_count > (UINT32_MAX - 256u) / 128u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t vector_storage_count = element_count * 5u;
    if (vector_storage_count > UINT32_MAX || vector_storage_count > SIZE_MAX / 2u ||
        n > SIZE_MAX / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t value_component_count = n * 2u;
    size_t vector_component_count = vector_storage_count * 2u;
    if (value_component_count > SIZE_MAX / scalar_size ||
        vector_component_count > SIZE_MAX / scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_value_count = n;
    *out_vector_count = element_count;
    *out_vector_storage_count = vector_storage_count;
    *out_max_iterations = element_count * 256u + 512u;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_general_eigen_complex_dispatch(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    const uint32_t* shader_words,
    size_t shader_size,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    if (out_values_device_ptr == NULL || out_vectors_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_values_device_ptr = NULL;
    *out_vectors_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (scalar_size == sizeof(double)) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (scalar_size == sizeof(float)) {
        if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t value_count = 0u;
    size_t vector_count = 0u;
    size_t vector_storage_count = 0u;
    size_t max_iterations = 0u;
    int validation_status = omni_tensor_backend_vulkan_general_eigen_validate_shape_complex(
        byte_len,
        n,
        scalar_size,
        &value_count,
        &vector_count,
        &vector_storage_count,
        &max_iterations
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (n == 0 || value_count == 0u || vector_count == 0u || vector_storage_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanGeneralEigenPushConstants push = {
        (uint32_t)n,
        (uint32_t)value_count,
        (uint32_t)vector_count,
        (uint32_t)vector_storage_count,
        (uint32_t)max_iterations,
        0u
    };
    void* values_device = NULL;
    void* vectors_device = NULL;
    void* status_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        value_count * 2u * scalar_size,
        vector_storage_count * 2u * scalar_size,
        scalar_size,
        1u,
        shader_words,
        shader_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_GENERAL_EIGEN_LOCAL_SIZE,
        &values_device,
        &vectors_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = scalar_size == sizeof(double)
        ? omni_tensor_backend_vulkan_read_general_eigen_status_f64(status_device)
        : omni_tensor_backend_vulkan_read_general_eigen_status_f32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)vectors_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)values_device);
        return status;
    }

    *out_values_device_ptr = values_device;
    *out_vectors_device_ptr = vectors_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_hermitian_eigen_complex_dispatch(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    const uint32_t* shader_words,
    size_t shader_size,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    if (out_values_device_ptr == NULL || out_vectors_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_values_device_ptr = NULL;
    *out_vectors_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (scalar_size == sizeof(double)) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (scalar_size == sizeof(float)) {
        if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t value_storage_count = 0u;
    size_t vector_count = 0u;
    size_t vector_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_hermitian_eigen_validate_shape_complex(
        byte_len,
        n,
        scalar_size,
        &value_storage_count,
        &vector_count,
        &vector_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (n == 0 || value_storage_count == 0u || vector_count == 0u || vector_storage_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSymmetricEigenPushConstants push = {
        (uint32_t)n,
        (uint32_t)value_storage_count,
        (uint32_t)vector_count,
        0u
    };
    void* values_device = NULL;
    void* vectors_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        value_storage_count * scalar_size,
        vector_storage_count * scalar_size,
        1u,
        shader_words,
        shader_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SYMMETRIC_EIGEN_LOCAL_SIZE,
        &values_device,
        &vectors_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t status_offset = n * scalar_size;
    status = scalar_size == sizeof(double)
        ? omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(values_device, status_offset)
        : omni_tensor_backend_vulkan_read_symmetric_eigen_status_f32(values_device, status_offset);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)vectors_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)values_device);
        return status;
    }

    *out_values_device_ptr = values_device;
    *out_vectors_device_ptr = vectors_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_hermitian_eigen_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_hermitian_eigen_complex_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(double),
        omni_tensor_vulkan_hermitian_eigen_complex128_spv,
        omni_tensor_vulkan_hermitian_eigen_complex128_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}

int omni_tensor_backend_vulkan_hermitian_eigen_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_hermitian_eigen_complex_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(float),
        omni_tensor_vulkan_hermitian_eigen_complex64_spv,
        omni_tensor_vulkan_hermitian_eigen_complex64_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}

int omni_tensor_backend_vulkan_general_eigen_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_general_eigen_complex_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(double),
        omni_tensor_vulkan_general_eigen_complex128_spv,
        omni_tensor_vulkan_general_eigen_complex128_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}

int omni_tensor_backend_vulkan_general_eigen_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_general_eigen_complex_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(float),
        omni_tensor_vulkan_general_eigen_complex64_spv,
        omni_tensor_vulkan_general_eigen_complex64_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}

static int omni_tensor_backend_vulkan_general_eigen_validate_shape_real(
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    size_t* out_value_count,
    size_t* out_vector_count,
    size_t* out_vector_storage_count,
    size_t* out_max_iterations
) {
    if (out_value_count == NULL || out_vector_count == NULL ||
        out_vector_storage_count == NULL || out_max_iterations == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_value_count = 0u;
    *out_vector_count = 0u;
    *out_vector_storage_count = 0u;
    *out_max_iterations = 0u;
    if (scalar_size != sizeof(double) && scalar_size != sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / 2u || n > SIZE_MAX / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX / scalar_size ||
        byte_len != element_count * scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / 5u || element_count > (UINT32_MAX - 256u) / 128u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t vector_storage_count = element_count * 5u;
    if (vector_storage_count > UINT32_MAX || vector_storage_count > SIZE_MAX / 2u ||
        n > SIZE_MAX / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t value_component_count = n * 2u;
    size_t vector_component_count = vector_storage_count * 2u;
    if (value_component_count > SIZE_MAX / scalar_size ||
        vector_component_count > SIZE_MAX / scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_value_count = n;
    *out_vector_count = element_count;
    *out_vector_storage_count = vector_storage_count;
    *out_max_iterations = element_count * 256u + 512u;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_general_eigen_real_dispatch(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t scalar_size,
    const uint32_t* shader_words,
    size_t shader_size,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    if (out_values_device_ptr == NULL || out_vectors_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_values_device_ptr = NULL;
    *out_vectors_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (scalar_size == sizeof(double)) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (scalar_size == sizeof(float)) {
        if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t value_count = 0u;
    size_t vector_count = 0u;
    size_t vector_storage_count = 0u;
    size_t max_iterations = 0u;
    int validation_status = omni_tensor_backend_vulkan_general_eigen_validate_shape_real(
        byte_len,
        n,
        scalar_size,
        &value_count,
        &vector_count,
        &vector_storage_count,
        &max_iterations
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (n == 0 || value_count == 0u || vector_count == 0u || vector_storage_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanGeneralEigenPushConstants push = {
        (uint32_t)n,
        (uint32_t)value_count,
        (uint32_t)vector_count,
        (uint32_t)vector_storage_count,
        (uint32_t)max_iterations,
        0u
    };
    void* values_device = NULL;
    void* vectors_device = NULL;
    void* status_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        value_count * 2u * scalar_size,
        vector_storage_count * 2u * scalar_size,
        scalar_size,
        1u,
        shader_words,
        shader_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_GENERAL_EIGEN_LOCAL_SIZE,
        &values_device,
        &vectors_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = scalar_size == sizeof(double)
        ? omni_tensor_backend_vulkan_read_general_eigen_status_f64(status_device)
        : omni_tensor_backend_vulkan_read_general_eigen_status_f32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)vectors_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)values_device);
        return status;
    }

    *out_values_device_ptr = values_device;
    *out_vectors_device_ptr = vectors_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_general_eigen_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_general_eigen_real_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(double),
        omni_tensor_vulkan_general_eigen_f64_spv,
        omni_tensor_vulkan_general_eigen_f64_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}

int omni_tensor_backend_vulkan_general_eigen_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    return omni_tensor_backend_vulkan_general_eigen_real_dispatch(
        input_device_ptr,
        byte_len,
        n,
        sizeof(float),
        omni_tensor_vulkan_general_eigen_f32_spv,
        omni_tensor_vulkan_general_eigen_f32_spv_size,
        out_values_device_ptr,
        out_vectors_device_ptr
    );
}
