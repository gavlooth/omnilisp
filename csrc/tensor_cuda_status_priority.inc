#ifndef OMNI_CUDA_HOST_DEVICE
#ifdef __CUDACC__
#define OMNI_CUDA_HOST_DEVICE __host__ __device__
#else
#define OMNI_CUDA_HOST_DEVICE
#endif
#endif

static OMNI_CUDA_HOST_DEVICE void omni_cuda_status_set(unsigned int* status, unsigned int code) {
    if (status == 0 || code == 0u) return;
#if defined(__CUDA_ARCH__)
    atomicMax(status, code);
#else
    if (*status < code) *status = code;
#endif
}
