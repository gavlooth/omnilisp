#include <stdint.h>

struct OmniCudaComplex128 {
    double real;
    double imag;
};

struct OmniCudaComplex64 {
    float real;
    float imag;
};

extern "C" __global__ void omni_cuda_transpose_complex128(
    const OmniCudaComplex128* input,
    unsigned long long rows,
    unsigned long long cols,
    unsigned long long element_count,
    OmniCudaComplex128* out
) {
    unsigned long long out_index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (out_index >= element_count) return;

    unsigned long long out_row = out_index / rows;
    unsigned long long out_col = out_index % rows;
    unsigned long long input_index = out_col * cols + out_row;
    out[out_index] = input[input_index];
}

extern "C" __global__ void omni_cuda_transpose_complex64(
    const OmniCudaComplex64* input,
    unsigned long long rows,
    unsigned long long cols,
    unsigned long long element_count,
    OmniCudaComplex64* out
) {
    unsigned long long out_index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (out_index >= element_count) return;

    unsigned long long out_row = out_index / rows;
    unsigned long long out_col = out_index % rows;
    unsigned long long input_index = out_col * cols + out_row;
    out[out_index] = input[input_index];
}

extern "C" __global__ void omni_cuda_diagonal_complex128(
    const OmniCudaComplex128* input,
    unsigned long long rows,
    unsigned long long cols,
    unsigned long long diagonal_count,
    OmniCudaComplex128* out
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= diagonal_count) return;
    out[index] = input[index * cols + index];
}

extern "C" __global__ void omni_cuda_diagonal_complex64(
    const OmniCudaComplex64* input,
    unsigned long long rows,
    unsigned long long cols,
    unsigned long long diagonal_count,
    OmniCudaComplex64* out
) {
    unsigned long long index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (index >= diagonal_count) return;
    out[index] = input[index * cols + index];
}

extern "C" __global__ void omni_cuda_diagonal_matrix_complex128(
    const OmniCudaComplex128* input,
    unsigned long long n,
    unsigned long long element_count,
    OmniCudaComplex128* out
) {
    unsigned long long out_index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (out_index >= element_count) return;

    unsigned long long row = out_index / n;
    unsigned long long col = out_index % n;
    OmniCudaComplex128 zero;
    zero.real = 0.0;
    zero.imag = 0.0;
    out[out_index] = row == col ? input[row] : zero;
}

extern "C" __global__ void omni_cuda_diagonal_matrix_complex64(
    const OmniCudaComplex64* input,
    unsigned long long n,
    unsigned long long element_count,
    OmniCudaComplex64* out
) {
    unsigned long long out_index =
        (unsigned long long)blockIdx.x * (unsigned long long)blockDim.x +
        (unsigned long long)threadIdx.x;
    if (out_index >= element_count) return;

    unsigned long long row = out_index / n;
    unsigned long long col = out_index % n;
    OmniCudaComplex64 zero;
    zero.real = 0.0f;
    zero.imag = 0.0f;
    out[out_index] = row == col ? input[row] : zero;
}

extern "C" __global__ void omni_cuda_trace_complex128(
    const OmniCudaComplex128* input,
    unsigned long long n,
    OmniCudaComplex128* out
) {
    OmniCudaComplex128 sum;
    sum.real = 0.0;
    sum.imag = 0.0;
    for (unsigned long long i = 0; i < n; i++) {
        OmniCudaComplex128 value = input[i * n + i];
        sum.real += value.real;
        sum.imag += value.imag;
    }
    out[0] = sum;
}

extern "C" __global__ void omni_cuda_trace_complex64(
    const OmniCudaComplex64* input,
    unsigned long long n,
    OmniCudaComplex64* out
) {
    OmniCudaComplex64 sum;
    sum.real = 0.0f;
    sum.imag = 0.0f;
    for (unsigned long long i = 0; i < n; i++) {
        OmniCudaComplex64 value = input[i * n + i];
        sum.real += value.real;
        sum.imag += value.imag;
    }
    out[0] = sum;
}
