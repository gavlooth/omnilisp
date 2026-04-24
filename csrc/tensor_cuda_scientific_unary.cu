#include <math.h>

#include "tensor_cuda_status_priority.inc"

static __device__ double omni_cuda_standard_normal_quantile(double p) {
    return normcdfinv(p);
}

static __device__ double omni_cuda_checked_standard_normal_quantile(double p, unsigned int* status) {
    if (!isfinite(p)) {
        omni_cuda_status_set(status, 2u);
        return 0.0;
    }
    if (!(p > 0.0 && p < 1.0)) {
        omni_cuda_status_set(status, 1u);
        return 0.0;
    }
    return omni_cuda_standard_normal_quantile(p);
}

static __device__ double omni_cuda_checked_asin(double value, unsigned int* status) {
    if (!isfinite(value)) {
        omni_cuda_status_set(status, 2u);
        return 0.0;
    }
    if (value < -1.0 || value > 1.0) {
        omni_cuda_status_set(status, 1u);
        return 0.0;
    }
    return asin(value);
}

static __device__ double omni_cuda_checked_acos(double value, unsigned int* status) {
    if (!isfinite(value)) {
        omni_cuda_status_set(status, 2u);
        return 0.0;
    }
    if (value < -1.0 || value > 1.0) {
        omni_cuda_status_set(status, 1u);
        return 0.0;
    }
    return acos(value);
}

static __device__ double omni_cuda_checked_log(double value, unsigned int* status) {
    if (!isfinite(value)) {
        omni_cuda_status_set(status, 2u);
        return 0.0;
    }
    if (!(value > 0.0)) {
        omni_cuda_status_set(status, 1u);
        return 0.0;
    }
    return log(value);
}

static __device__ double omni_cuda_checked_log10(double value, unsigned int* status) {
    if (!isfinite(value)) {
        omni_cuda_status_set(status, 2u);
        return 0.0;
    }
    if (!(value > 0.0)) {
        omni_cuda_status_set(status, 1u);
        return 0.0;
    }
    return log10(value);
}

extern "C" __global__ void omni_cuda_map_scientific_f64(
    const double* input,
    unsigned int op,
    unsigned long long element_count,
    double* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    double value = input[index];
    double result = value;
    switch (op) {
        case 5: result = sin(value); break;
        case 6: result = cos(value); break;
        case 7: result = tan(value); break;
        case 8: result = omni_cuda_checked_asin(value, status); break;
        case 9: result = omni_cuda_checked_acos(value, status); break;
        case 10: result = atan(value); break;
        case 11: result = sinh(value); break;
        case 12: result = cosh(value); break;
        case 13: result = tanh(value); break;
        case 14: result = exp(value); break;
        case 15: result = omni_cuda_checked_log(value, status); break;
        case 16: result = omni_cuda_checked_log10(value, status); break;
        case 17: result = erf(value); break;
        case 18: result = erfc(value); break;
        case 19: result = 0.5 * erfc(-value * 0.70710678118654752440084436210484903928); break;
        case 20: result = omni_cuda_checked_standard_normal_quantile(value, status); break;
        default: result = value; break;
    }
    out[index] = result;
}

extern "C" __global__ void omni_cuda_map_scientific_f32(
    const float* input,
    unsigned int op,
    unsigned long long element_count,
    float* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    float value = input[index];
    float result = value;
    switch (op) {
        case 5: result = sinf(value); break;
        case 6: result = cosf(value); break;
        case 7: result = tanf(value); break;
        case 8: result = (float)omni_cuda_checked_asin((double)value, status); break;
        case 9: result = (float)omni_cuda_checked_acos((double)value, status); break;
        case 10: result = atanf(value); break;
        case 11: result = sinhf(value); break;
        case 12: result = coshf(value); break;
        case 13: result = tanhf(value); break;
        case 14: result = expf(value); break;
        case 15: result = (float)omni_cuda_checked_log((double)value, status); break;
        case 16: result = (float)omni_cuda_checked_log10((double)value, status); break;
        case 17: result = erff(value); break;
        case 18: result = erfcf(value); break;
        case 19: result = 0.5f * erfcf(-value * 0.70710678118f); break;
        case 20: result = (float)omni_cuda_checked_standard_normal_quantile((double)value, status); break;
        default: result = value; break;
    }
    out[index] = result;
}
