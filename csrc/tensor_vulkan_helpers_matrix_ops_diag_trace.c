#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_diagonal_f64(
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
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        diagonal_count * sizeof(double),
        diagonal_count,
        omni_tensor_vulkan_diagonal_f64_spv,
        omni_tensor_vulkan_diagonal_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        diagonal_count * sizeof(float),
        diagonal_count,
        omni_tensor_vulkan_diagonal_f32_spv,
        omni_tensor_vulkan_diagonal_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    if (n > SIZE_MAX / sizeof(double) || byte_len != n * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        output_count,
        omni_tensor_vulkan_diagonal_matrix_f64_spv,
        omni_tensor_vulkan_diagonal_matrix_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_f32(
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
    size_t output_count = n * n;
    if (n > SIZE_MAX / sizeof(float) || byte_len != n * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        output_count,
        omni_tensor_vulkan_diagonal_matrix_f32_spv,
        omni_tensor_vulkan_diagonal_matrix_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_trace_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_trace
) {
    if (out_trace == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_trace = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double),
        1,
        omni_tensor_vulkan_trace_f64_spv,
        omni_tensor_vulkan_trace_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(double), out_trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    return status;
}

int omni_tensor_backend_vulkan_trace_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_trace
) {
    if (out_trace == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_trace = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float),
        1,
        omni_tensor_vulkan_trace_f32_spv,
        omni_tensor_vulkan_trace_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(float), out_trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    return status;
}

int omni_tensor_backend_vulkan_diagonal_complex128(
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
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX ||
        diagonal_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        diagonal_count * element_size,
        diagonal_count,
        omni_tensor_vulkan_diagonal_complex128_spv,
        omni_tensor_vulkan_diagonal_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX ||
        diagonal_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        diagonal_count * element_size,
        diagonal_count,
        omni_tensor_vulkan_diagonal_complex64_spv,
        omni_tensor_vulkan_diagonal_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (n > SIZE_MAX / element_size || byte_len != n * element_size ||
        output_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        output_count,
        omni_tensor_vulkan_diagonal_matrix_complex128_spv,
        omni_tensor_vulkan_diagonal_matrix_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_complex64(
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
    size_t output_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (n > SIZE_MAX / element_size || byte_len != n * element_size ||
        output_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        output_count,
        omni_tensor_vulkan_diagonal_matrix_complex64_spv,
        omni_tensor_vulkan_diagonal_matrix_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_trace_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_real,
    double* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0;
    *out_imag = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        element_size,
        1,
        omni_tensor_vulkan_trace_complex128_spv,
        omni_tensor_vulkan_trace_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double trace[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(trace), trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = trace[0];
        *out_imag = trace[1];
    }
    return status;
}
int omni_tensor_backend_vulkan_trace_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_real,
    float* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0f;
    *out_imag = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        element_size,
        1,
        omni_tensor_vulkan_trace_complex64_spv,
        omni_tensor_vulkan_trace_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float trace[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(trace), trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = trace[0];
        *out_imag = trace[1];
    }
    return status;
}
