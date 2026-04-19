#include "tensor_vulkan_helpers_internal.h"

static int omni_tensor_backend_vulkan_read_status_payload_f64(
    void* output_device,
    size_t status_offset_bytes,
    double* out_status_payload
) {
    if (out_status_payload == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_status_payload = 0.0;
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        status_offset_bytes,
        sizeof(*out_status_payload),
        out_status_payload
    );
    return status;
}

static int omni_tensor_backend_vulkan_read_mapped_status_f64(
    void* output_device,
    size_t status_offset_bytes,
    OmniTensorVulkanStatusMapperF64 mapper
) {
    if (mapper == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    double status_payload = 0.0;
    int status = omni_tensor_backend_vulkan_read_status_payload_f64(
        output_device,
        status_offset_bytes,
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return mapper(status_payload);
}

static int omni_tensor_backend_vulkan_read_status_payload_f32(
    void* output_device,
    size_t status_offset_bytes,
    float* out_status_payload
) {
    if (out_status_payload == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_status_payload = 0.0f;
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        status_offset_bytes,
        sizeof(*out_status_payload),
        out_status_payload
    );
    return status;
}

static int omni_tensor_backend_vulkan_read_mapped_status_f32(
    void* output_device,
    size_t status_offset_bytes,
    OmniTensorVulkanStatusMapperF32 mapper
) {
    if (mapper == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    float status_payload = 0.0f;
    int status = omni_tensor_backend_vulkan_read_status_payload_f32(
        output_device,
        status_offset_bytes,
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return mapper(status_payload);
}

int omni_tensor_backend_vulkan_tail_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return status_payload == 0.0 ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_SINGULAR;
}

int omni_tensor_backend_vulkan_tail_status_from_payload_f32(float status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return status_payload == 0.0f ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_SINGULAR;
}

int omni_tensor_backend_vulkan_read_tail_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_tail_status_from_payload_f64
    );
}

int omni_tensor_backend_vulkan_read_tail_status_f32(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f32(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_tail_status_from_payload_f32
    );
}

static int omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

static int omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(float status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0f) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0f) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

static int omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    if (status_payload == 3.0) return OMNI_TENSOR_VULKAN_NOT_SYMMETRIC;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

int omni_tensor_backend_vulkan_read_singular_values_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_singular_values_status_from_payload_f64
    );
}

int omni_tensor_backend_vulkan_read_singular_values_status_f32(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f32(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_singular_values_status_from_payload_f32
    );
}

int omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64
    );
}

int omni_tensor_backend_vulkan_singular_values_status_payload_probe_for_tests(void) {
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_SINGULAR) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(2.0) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(INFINITY) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(0.0f) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(1.0f) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(2.0f) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(INFINITY) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(2.0) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(3.0) != OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_read_status_code_u32(void* status_device) {
    uint32_t status_payload = 1u;
    int status = omni_tensor_backend_vulkan_copy_to_host(
        status_device,
        sizeof(status_payload),
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (status_payload == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 1u) return OMNI_TENSOR_VULKAN_SINGULAR;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}
