#include <float.h>
#include <math.h>
#include <stdint.h>

struct OmniCudaComplex128 {
    double real;
    double imag;
};

struct OmniCudaComplex64 {
    float real;
    float imag;
};

static __device__ void omni_cuda_status_set(unsigned int* status, unsigned int code) {
    if (status != 0 && code != 0u) {
        atomicCAS(status, 0u, code);
    }
}

static __device__ unsigned long long omni_cuda_operand_index(
    unsigned long long index,
    const unsigned long long* offsets
) {
    return offsets == 0 ? index : offsets[index];
}

static __device__ int omni_cuda_complex128_valid(OmniCudaComplex128 value) {
    return isfinite(value.real) && isfinite(value.imag);
}

static __device__ int omni_cuda_complex64_valid(double real, double imag) {
    return isfinite(real) && isfinite(imag) && fabs(real) <= (double)FLT_MAX && fabs(imag) <= (double)FLT_MAX;
}

static __device__ OmniCudaComplex128 omni_cuda_complex128_binary(
    OmniCudaComplex128 left,
    OmniCudaComplex128 right,
    unsigned int op,
    unsigned int* status
) {
    OmniCudaComplex128 result;
    result.real = 0.0;
    result.imag = 0.0;

    switch (op) {
        case 0:
            result.real = left.real + right.real;
            result.imag = left.imag + right.imag;
            break;
        case 1:
            result.real = left.real - right.real;
            result.imag = left.imag - right.imag;
            break;
        case 2:
            result.real = left.real * right.real - left.imag * right.imag;
            result.imag = left.real * right.imag + left.imag * right.real;
            break;
        case 3: {
            double denom = right.real * right.real + right.imag * right.imag;
            if (!(denom > 0.0) || !isfinite(denom)) {
                omni_cuda_status_set(status, 1u);
                return result;
            }
            result.real = (left.real * right.real + left.imag * right.imag) / denom;
            result.imag = (left.imag * right.real - left.real * right.imag) / denom;
            break;
        }
        default:
            omni_cuda_status_set(status, 2u);
            return result;
    }

    if (!omni_cuda_complex128_valid(result)) {
        omni_cuda_status_set(status, 2u);
    }
    return result;
}

static __device__ OmniCudaComplex128 omni_cuda_complex128_unary(
    OmniCudaComplex128 input,
    unsigned int op,
    unsigned int* status
) {
    OmniCudaComplex128 result;
    result.real = input.real;
    result.imag = input.imag;

    switch (op) {
        case 0:
            result.real = hypot(input.real, input.imag);
            result.imag = 0.0;
            break;
        case 1:
            result.real = -input.real;
            result.imag = -input.imag;
            break;
        case 2:
            break;
        case 3:
            result.real = input.real;
            result.imag = 0.0;
            break;
        case 4:
            result.real = input.imag;
            result.imag = 0.0;
            break;
        case 5:
            result.real = input.real;
            result.imag = -input.imag;
            break;
        default:
            omni_cuda_status_set(status, 2u);
            result.real = 0.0;
            result.imag = 0.0;
            break;
    }

    if (!omni_cuda_complex128_valid(result)) {
        omni_cuda_status_set(status, 2u);
    }
    return result;
}

static __device__ OmniCudaComplex64 omni_cuda_complex64_from_double(
    double real,
    double imag,
    unsigned int* status
) {
    OmniCudaComplex64 result;
    result.real = 0.0f;
    result.imag = 0.0f;
    if (!omni_cuda_complex64_valid(real, imag)) {
        omni_cuda_status_set(status, 2u);
        return result;
    }
    result.real = (float)real;
    result.imag = (float)imag;
    return result;
}

static __device__ OmniCudaComplex64 omni_cuda_complex64_binary(
    OmniCudaComplex64 left,
    OmniCudaComplex64 right,
    unsigned int op,
    unsigned int* status
) {
    OmniCudaComplex128 wide_left;
    wide_left.real = (double)left.real;
    wide_left.imag = (double)left.imag;
    OmniCudaComplex128 wide_right;
    wide_right.real = (double)right.real;
    wide_right.imag = (double)right.imag;
    OmniCudaComplex128 wide = omni_cuda_complex128_binary(wide_left, wide_right, op, status);
    return omni_cuda_complex64_from_double(wide.real, wide.imag, status);
}

static __device__ OmniCudaComplex64 omni_cuda_complex64_unary(
    OmniCudaComplex64 input,
    unsigned int op,
    unsigned int* status
) {
    OmniCudaComplex128 wide_input;
    wide_input.real = (double)input.real;
    wide_input.imag = (double)input.imag;
    OmniCudaComplex128 wide = omni_cuda_complex128_unary(wide_input, op, status);
    return omni_cuda_complex64_from_double(wide.real, wide.imag, status);
}

extern "C" __global__ void omni_cuda_map_complex128(
    const OmniCudaComplex128* left,
    const OmniCudaComplex128* right,
    OmniCudaComplex128 scalar,
    unsigned int mode,
    unsigned int op,
    unsigned long long element_count,
    const unsigned long long* left_offsets,
    const unsigned long long* right_offsets,
    OmniCudaComplex128* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    OmniCudaComplex128 left_value = scalar;
    OmniCudaComplex128 right_value = scalar;
    if (mode == 0u) {
        left_value = left[omni_cuda_operand_index(index, left_offsets)];
    } else if (mode == 1u) {
        right_value = right[omni_cuda_operand_index(index, right_offsets)];
    } else {
        left_value = left[omni_cuda_operand_index(index, left_offsets)];
        right_value = right[omni_cuda_operand_index(index, right_offsets)];
    }
    out[index] = omni_cuda_complex128_binary(left_value, right_value, op, status);
}

extern "C" __global__ void omni_cuda_map_complex64(
    const OmniCudaComplex64* left,
    const OmniCudaComplex64* right,
    OmniCudaComplex64 scalar,
    unsigned int mode,
    unsigned int op,
    unsigned long long element_count,
    const unsigned long long* left_offsets,
    const unsigned long long* right_offsets,
    OmniCudaComplex64* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    OmniCudaComplex64 left_value = scalar;
    OmniCudaComplex64 right_value = scalar;
    if (mode == 0u) {
        left_value = left[omni_cuda_operand_index(index, left_offsets)];
    } else if (mode == 1u) {
        right_value = right[omni_cuda_operand_index(index, right_offsets)];
    } else {
        left_value = left[omni_cuda_operand_index(index, left_offsets)];
        right_value = right[omni_cuda_operand_index(index, right_offsets)];
    }
    out[index] = omni_cuda_complex64_binary(left_value, right_value, op, status);
}

extern "C" __global__ void omni_cuda_map_complex128_unary(
    const OmniCudaComplex128* input,
    unsigned int op,
    unsigned long long element_count,
    OmniCudaComplex128* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;
    out[index] = omni_cuda_complex128_unary(input[index], op, status);
}

extern "C" __global__ void omni_cuda_map_complex64_unary(
    const OmniCudaComplex64* input,
    unsigned int op,
    unsigned long long element_count,
    OmniCudaComplex64* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;
    out[index] = omni_cuda_complex64_unary(input[index], op, status);
}

extern "C" __global__ void omni_cuda_map_complex128_to_real(
    const OmniCudaComplex128* input,
    unsigned int op,
    unsigned long long element_count,
    double* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    OmniCudaComplex128 value = input[index];
    double result = 0.0;
    if (op == 0u) {
        result = hypot(value.real, value.imag);
    } else if (op == 3u) {
        result = value.real;
    } else if (op == 4u) {
        result = value.imag;
    } else {
        omni_cuda_status_set(status, 2u);
        return;
    }
    if (!isfinite(result)) {
        omni_cuda_status_set(status, 2u);
        return;
    }
    out[index] = result;
}

extern "C" __global__ void omni_cuda_map_complex64_to_real(
    const OmniCudaComplex64* input,
    unsigned int op,
    unsigned long long element_count,
    float* out,
    unsigned int* status
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= element_count) return;

    OmniCudaComplex64 value = input[index];
    double result = 0.0;
    if (op == 0u) {
        result = hypot((double)value.real, (double)value.imag);
    } else if (op == 3u) {
        result = (double)value.real;
    } else if (op == 4u) {
        result = (double)value.imag;
    } else {
        omni_cuda_status_set(status, 2u);
        return;
    }
    if (!isfinite(result) || fabs(result) > (double)FLT_MAX) {
        omni_cuda_status_set(status, 2u);
        return;
    }
    out[index] = (float)result;
}
