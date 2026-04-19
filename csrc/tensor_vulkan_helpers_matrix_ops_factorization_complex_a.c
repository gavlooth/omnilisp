#include "tensor_vulkan_helpers_internal.h"
int omni_tensor_backend_vulkan_inverse_complex128(
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
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_inverse_complex128_spv,
        omni_tensor_vulkan_inverse_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        output_device,
        element_count * 2u * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_complex64(
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
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_inverse_complex64_spv,
        omni_tensor_vulkan_inverse_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        output_device,
        element_count * 2u * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_complex128(
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
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_cholesky_complex128_spv,
        omni_tensor_vulkan_cholesky_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        output_device,
        element_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_complex64(
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
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

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
        output_count * element_size,
        1,
        omni_tensor_vulkan_cholesky_complex64_spv,
        omni_tensor_vulkan_cholesky_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        output_device,
        element_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_complex128(
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
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    const size_t element_size = sizeof(double) * 2u;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / element_size || byte_len != q_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
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
        q_count * element_size,
        r_storage_count * element_size,
        1,
        omni_tensor_vulkan_qr_complex128_spv,
        omni_tensor_vulkan_qr_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        r_device,
        r_count * element_size
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
int omni_tensor_backend_vulkan_qr_complex64(
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
    const size_t element_size = sizeof(float) * 2u;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / element_size || byte_len != q_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
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
        q_count * element_size,
        r_storage_count * element_size,
        1,
        omni_tensor_vulkan_qr_complex64_spv,
        omni_tensor_vulkan_qr_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        r_device,
        r_count * element_size
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
