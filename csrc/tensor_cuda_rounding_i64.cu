#include <math.h>
#include <stdint.h>

static __device__ int omni_cuda_rounding_status(double value) {
    if (!isfinite(value)) return 2;
    double max_i64 = 9223372036854775807.0;
    double min_i64 = -9223372036854775808.0;
    if (value < min_i64 || value > max_i64) return 1;
    return 0;
}

static __device__ double omni_cuda_apply_rounding(double input, unsigned int op) {
    if (op == 1u) return floor(input);
    if (op == 2u) return ceil(input);
    if (op == 3u) return round(input);
    if (op == 4u) return trunc(input);
    return input;
}

extern "C" __global__ void omni_cuda_round_i64_f64(
    const double* input,
    unsigned int op,
    unsigned long long element_count,
    long long* out,
    unsigned int* status
) {
    unsigned long long index = (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x + (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    if (op < 1u || op > 4u) {
        atomicCAS(status, 0u, 2u);
        return;
    }

    double rounded = omni_cuda_apply_rounding(input[index], op);
    int status_code = omni_cuda_rounding_status(rounded);
    if (status_code != 0) {
        atomicCAS(status, 0u, (unsigned int)status_code);
        return;
    }
    out[index] = (long long)rounded;
}

extern "C" __global__ void omni_cuda_round_i64_f32(
    const float* input,
    unsigned int op,
    unsigned long long element_count,
    long long* out,
    unsigned int* status
) {
    unsigned long long index = (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x + (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    if (op < 1u || op > 4u) {
        atomicCAS(status, 0u, 2u);
        return;
    }

    double rounded = omni_cuda_apply_rounding((double)input[index], op);
    int status_code = omni_cuda_rounding_status(rounded);
    if (status_code != 0) {
        atomicCAS(status, 0u, (unsigned int)status_code);
        return;
    }
    out[index] = (long long)rounded;
}
