#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_cholesky_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_cholesky_f32_spv,
        omni_tensor_vulkan_cholesky_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        output_device,
        element_count * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / sizeof(float) || byte_len != q_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * sizeof(float),
        r_storage_count * sizeof(float),
        1,
        omni_tensor_vulkan_qr_f32_spv,
        omni_tensor_vulkan_qr_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        r_device,
        r_count * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (visible_output_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + gram_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_oversized_index_guard_for_tests(void) {
    size_t rows = (size_t)UINT32_MAX;
    size_t cols = 2u;
    size_t element_count = rows * cols;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    return omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
        element_count * sizeof(double),
        rows,
        cols,
        &k,
        &output_storage_count
    );
}

int omni_tensor_backend_vulkan_singular_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
);

int omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests(void) {
    double out_norm = 123.0;
    int expected_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) {
        expected_status = OMNI_TENSOR_VULKAN_UNAVAILABLE;
    } else if (!omni_tensor_backend_vulkan_float64_available()) {
        expected_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    int status = omni_tensor_backend_vulkan_singular_norm_f64(
        NULL,
        0u,
        65u,
        65u,
        0u,
        &out_norm
    );
    if (status != expected_status) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
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

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_singular_values_f64_spv,
        omni_tensor_vulkan_singular_values_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        output,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t scalar_size,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL || scalar_size == 0u) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t scalar_count = element_count * 2u;
    if (scalar_count > SIZE_MAX / scalar_size || byte_len != scalar_count * scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX / 2u || cols > UINT32_MAX / 2u || k > UINT32_MAX / 2u || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t real_k = k * 2u;
    if (real_k != 0u && real_k > ((size_t)UINT32_MAX / 64u) / real_k) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (real_k != 0u && real_k > SIZE_MAX / real_k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = real_k * real_k;
    if (real_k > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t hidden_count = real_k + gram_count;
    if (visible_output_count > SIZE_MAX - hidden_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + hidden_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_complex_iteration_guard_for_tests(void) {
    size_t k = 0u;
    size_t output_storage_count = 0u;
    size_t rows = 4096u;
    size_t cols = 4096u;
    size_t element_count = rows * cols;
    int status128 = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        element_count * 2u * sizeof(double),
        rows,
        cols,
        sizeof(double),
        &k,
        &output_storage_count
    );
    int status64 = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        element_count * 2u * sizeof(float),
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (status128 == OMNI_TENSOR_VULKAN_UNSUPPORTED && status64 == OMNI_TENSOR_VULKAN_UNSUPPORTED) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

int omni_tensor_backend_vulkan_singular_values_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
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

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_singular_values_complex128_spv,
        omni_tensor_vulkan_singular_values_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        output,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_svd_validate_shape_f64(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_u_count,
    size_t* out_u_storage_count,
    size_t* out_s_storage_count,
    size_t* out_v_count
) {
    if (out_k == NULL || out_u_count == NULL || out_u_storage_count == NULL ||
        out_s_storage_count == NULL || out_v_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_k = 0u;
    *out_u_count = 0u;
    *out_u_storage_count = 0u;
    *out_s_storage_count = 0u;
    *out_v_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rows != 0 && k > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_count = rows * k;
    if (cols != 0 && k > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t v_count = cols * k;
    if (u_count > UINT32_MAX || v_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (u_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_storage_count = u_count + gram_count;
    if (u_storage_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t s_storage_count = k + 1u;
    if (u_storage_count > SIZE_MAX / sizeof(double) ||
        s_storage_count > SIZE_MAX / sizeof(double) ||
        v_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_u_count = u_count;
    *out_u_storage_count = u_storage_count;
    *out_s_storage_count = s_storage_count;
    *out_v_count = v_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_svd_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t u_count = 0u;
    size_t u_storage_count = 0u;
    size_t s_storage_count = 0u;
    size_t v_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_svd_validate_shape_f64(
        byte_len,
        rows,
        cols,
        &k,
        &u_count,
        &u_storage_count,
        &s_storage_count,
        &v_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || u_count == 0u || u_storage_count == 0u || s_storage_count == 0u || v_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSvdPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)u_count,
        (uint32_t)s_storage_count,
        (uint32_t)v_count
    };
    void* u_device = NULL;
    void* s_device = NULL;
    void* v_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        u_storage_count * sizeof(double),
        s_storage_count * sizeof(double),
        v_count * sizeof(double),
        1u,
        omni_tensor_vulkan_svd_f64_spv,
        omni_tensor_vulkan_svd_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SVD_LOCAL_SIZE,
        &u_device,
        &s_device,
        &v_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        s_device,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)v_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)s_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)u_device);
        return status;
    }

    *out_u_device_ptr = u_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_symmetric_eigen_validate_shape_f64(
    size_t byte_len,
    size_t n,
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
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX / 64u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t value_storage_count = n + 1u;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t vector_storage_count = element_count + element_count;
    if (value_storage_count > UINT32_MAX || vector_storage_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (value_storage_count > SIZE_MAX / sizeof(double) ||
        vector_storage_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_value_storage_count = value_storage_count;
    *out_vector_count = element_count;
    *out_vector_storage_count = vector_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_symmetric_eigen_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    if (out_values_device_ptr == NULL || out_vectors_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_values_device_ptr = NULL;
    *out_vectors_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t value_storage_count = 0u;
    size_t vector_count = 0u;
    size_t vector_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_symmetric_eigen_validate_shape_f64(
        byte_len,
        n,
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
        value_storage_count * sizeof(double),
        vector_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_symmetric_eigen_f64_spv,
        omni_tensor_vulkan_symmetric_eigen_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SYMMETRIC_EIGEN_LOCAL_SIZE,
        &values_device,
        &vectors_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(
        values_device,
        n * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)vectors_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)values_device);
        return status;
    }

    *out_values_device_ptr = values_device;
    *out_vectors_device_ptr = vectors_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}
