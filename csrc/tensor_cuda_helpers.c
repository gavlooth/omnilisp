#include <dlfcn.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define OMNI_TENSOR_CUDA_UNAVAILABLE 0
#define OMNI_TENSOR_CUDA_SUCCESS 1
#define OMNI_TENSOR_CUDA_INVALID -2
#define OMNI_TENSOR_CUDA_ALLOCATION_FAILED -3
#define OMNI_TENSOR_CUDA_COPY_FAILED -4
#define OMNI_TENSOR_CUDA_EXECUTION_FAILED -5
#define OMNI_TENSOR_CUDA_DOMAIN_ERROR -6
#define OMNI_TENSOR_CUDA_INVALID_ARGUMENT -7

#define OMNI_CUDA_MEMCPY_HOST_TO_DEVICE 1
#define OMNI_CUDA_MEMCPY_DEVICE_TO_HOST 2
#define OMNI_CUDA_MEMCPY_DEVICE_TO_DEVICE 3
#define OMNI_CUBLAS_OP_N 0
#define OMNI_CUBLAS_OP_T 1
#define OMNI_CUBLAS_STATUS_SUCCESS 0

int omni_tensor_backend_cuda_available(void);
void omni_tensor_backend_cuda_free(void* device_ptr);

typedef struct OmniTensorCudaComplex128 {
    double real;
    double imag;
} OmniTensorCudaComplex128;

typedef struct OmniTensorCudaComplex64 {
    float real;
    float imag;
} OmniTensorCudaComplex64;

typedef int (*omni_cuda_malloc_fn)(void** dev_ptr, size_t size);
typedef int (*omni_cuda_free_fn)(void* dev_ptr);
typedef int (*omni_cuda_memcpy_fn)(void* dst, const void* src, size_t count, int kind);
typedef int (*omni_cuda_get_device_count_fn)(int* count);
typedef int (*omni_cu_init_fn)(unsigned int flags);
typedef int (*omni_cu_module_load_data_fn)(void** module, const void* image);
typedef int (*omni_cu_module_get_function_fn)(void** function, void* module, const char* name);
typedef int (*omni_cu_module_unload_fn)(void* module);
typedef int (*omni_cu_launch_kernel_fn)(
    void* function,
    unsigned int grid_dim_x,
    unsigned int grid_dim_y,
    unsigned int grid_dim_z,
    unsigned int block_dim_x,
    unsigned int block_dim_y,
    unsigned int block_dim_z,
    unsigned int shared_mem_bytes,
    void* stream,
    void** kernel_params,
    void** extra
);
typedef int (*omni_cu_ctx_synchronize_fn)(void);
typedef int (*omni_cublas_create_fn)(void** handle);
typedef int (*omni_cublas_destroy_fn)(void* handle);
typedef int (*omni_cublas_dgemm_fn)(
    void* handle,
    int transa,
    int transb,
    int m,
    int n,
    int k,
    const double* alpha,
    const double* a,
    int lda,
    const double* b,
    int ldb,
    const double* beta,
    double* c,
    int ldc
);
typedef int (*omni_cublas_sgemm_fn)(
    void* handle,
    int transa,
    int transb,
    int m,
    int n,
    int k,
    const float* alpha,
    const float* a,
    int lda,
    const float* b,
    int ldb,
    const float* beta,
    float* c,
    int ldc
);
typedef int (*omni_cublas_dgemv_fn)(
    void* handle,
    int trans,
    int m,
    int n,
    const double* alpha,
    const double* a,
    int lda,
    const double* x,
    int incx,
    const double* beta,
    double* y,
    int incy
);
typedef int (*omni_cublas_sgemv_fn)(
    void* handle,
    int trans,
    int m,
    int n,
    const float* alpha,
    const float* a,
    int lda,
    const float* x,
    int incx,
    const float* beta,
    float* y,
    int incy
);
typedef int (*omni_cublas_zgemm_fn)(
    void* handle,
    int transa,
    int transb,
    int m,
    int n,
    int k,
    const OmniTensorCudaComplex128* alpha,
    const OmniTensorCudaComplex128* a,
    int lda,
    const OmniTensorCudaComplex128* b,
    int ldb,
    const OmniTensorCudaComplex128* beta,
    OmniTensorCudaComplex128* c,
    int ldc
);
typedef int (*omni_cublas_cgemm_fn)(
    void* handle,
    int transa,
    int transb,
    int m,
    int n,
    int k,
    const OmniTensorCudaComplex64* alpha,
    const OmniTensorCudaComplex64* a,
    int lda,
    const OmniTensorCudaComplex64* b,
    int ldb,
    const OmniTensorCudaComplex64* beta,
    OmniTensorCudaComplex64* c,
    int ldc
);
typedef int (*omni_cublas_zgemv_fn)(
    void* handle,
    int trans,
    int m,
    int n,
    const OmniTensorCudaComplex128* alpha,
    const OmniTensorCudaComplex128* a,
    int lda,
    const OmniTensorCudaComplex128* x,
    int incx,
    const OmniTensorCudaComplex128* beta,
    OmniTensorCudaComplex128* y,
    int incy
);
typedef int (*omni_cublas_cgemv_fn)(
    void* handle,
    int trans,
    int m,
    int n,
    const OmniTensorCudaComplex64* alpha,
    const OmniTensorCudaComplex64* a,
    int lda,
    const OmniTensorCudaComplex64* x,
    int incx,
    const OmniTensorCudaComplex64* beta,
    OmniTensorCudaComplex64* y,
    int incy
);
static void* omni_tensor_cuda_handle = NULL;
static void* omni_tensor_cuda_driver_handle = NULL;
static void* omni_tensor_cuda_map_module = NULL;
static void* omni_tensor_cuda_map_f64_function = NULL;
static void* omni_tensor_cuda_map_f32_function = NULL;
static void* omni_tensor_cuda_map_unary_f64_function = NULL;
static void* omni_tensor_cuda_map_unary_f32_function = NULL;
static void* omni_tensor_cuda_map_scientific_module = NULL;
static void* omni_tensor_cuda_map_scientific_f64_function = NULL;
static void* omni_tensor_cuda_map_scientific_f32_function = NULL;
static void* omni_tensor_cuda_complex_map_module = NULL;
static void* omni_tensor_cuda_map_complex128_function = NULL;
static void* omni_tensor_cuda_map_complex64_function = NULL;
static void* omni_tensor_cuda_map_complex128_unary_function = NULL;
static void* omni_tensor_cuda_map_complex64_unary_function = NULL;
static void* omni_tensor_cuda_map_complex128_to_real_function = NULL;
static void* omni_tensor_cuda_map_complex64_to_real_function = NULL;
static void* omni_tensor_cuda_complex_matrix_module = NULL;
static void* omni_tensor_cuda_transpose_complex128_function = NULL;
static void* omni_tensor_cuda_transpose_complex64_function = NULL;
static void* omni_tensor_cuda_diagonal_complex128_function = NULL;
static void* omni_tensor_cuda_diagonal_complex64_function = NULL;
static void* omni_tensor_cuda_diagonal_matrix_complex128_function = NULL;
static void* omni_tensor_cuda_diagonal_matrix_complex64_function = NULL;
static void* omni_tensor_cuda_trace_complex128_function = NULL;
static void* omni_tensor_cuda_trace_complex64_function = NULL;
static void* omni_tensor_cuda_round_module = NULL;
static void* omni_tensor_cuda_round_i64_f64_function = NULL;
static void* omni_tensor_cuda_round_i64_f32_function = NULL;
static void* omni_tensor_cublas_handle = NULL;
static omni_cuda_malloc_fn omni_cuda_malloc = NULL;
static omni_cuda_free_fn omni_cuda_free = NULL;
static omni_cuda_memcpy_fn omni_cuda_memcpy = NULL;
static omni_cuda_get_device_count_fn omni_cuda_get_device_count = NULL;
static omni_cu_init_fn omni_cu_init = NULL;
static omni_cu_module_load_data_fn omni_cu_module_load_data = NULL;
static omni_cu_module_get_function_fn omni_cu_module_get_function = NULL;
static omni_cu_module_unload_fn omni_cu_module_unload = NULL;
static omni_cu_launch_kernel_fn omni_cu_launch_kernel = NULL;
static omni_cu_ctx_synchronize_fn omni_cu_ctx_synchronize = NULL;
static omni_cublas_create_fn omni_cublas_create = NULL;
static omni_cublas_destroy_fn omni_cublas_destroy = NULL;
static omni_cublas_dgemm_fn omni_cublas_dgemm = NULL;
static omni_cublas_dgemv_fn omni_cublas_dgemv = NULL;
static omni_cublas_sgemm_fn omni_cublas_sgemm = NULL;
static omni_cublas_sgemv_fn omni_cublas_sgemv = NULL;
static omni_cublas_zgemm_fn omni_cublas_zgemm = NULL;
static omni_cublas_cgemm_fn omni_cublas_cgemm = NULL;
static omni_cublas_zgemv_fn omni_cublas_zgemv = NULL;
static omni_cublas_cgemv_fn omni_cublas_cgemv = NULL;
static int omni_tensor_cuda_resolution_attempted = 0;
static int omni_tensor_cuda_driver_resolution_attempted = 0;
static int omni_tensor_cuda_map_module_attempted = 0;
static int omni_tensor_cuda_map_scientific_module_attempted = 0;
static int omni_tensor_cuda_complex_map_module_attempted = 0;
static int omni_tensor_cuda_complex_matrix_module_attempted = 0;
static int omni_tensor_cuda_round_module_attempted = 0;
static int omni_tensor_cublas_resolution_attempted = 0;
static int omni_tensor_cuda_disabled_for_tests = 0;
static int omni_tensor_cublas_disabled_for_tests = 0;

static const char* omni_tensor_cuda_map_ptx =
#include "tensor_cuda_map_ptx.inc"
;
static const char* omni_tensor_cuda_scientific_ptx =
#include "tensor_cuda_scientific_ptx_part_00.inc"
#include "tensor_cuda_scientific_ptx_part_01.inc"
#include "tensor_cuda_scientific_ptx_part_02.inc"
#include "tensor_cuda_scientific_ptx_part_03.inc"
#include "tensor_cuda_scientific_ptx_part_04.inc"
#include "tensor_cuda_scientific_ptx_part_05.inc"
;
static const char* omni_tensor_cuda_round_ptx =
#include "tensor_cuda_rounding_i64_ptx.inc"
;

static const char* omni_tensor_cuda_complex_map_ptx =
#include "tensor_cuda_complex_map_ptx_part_00.inc"
#include "tensor_cuda_complex_map_ptx_part_01.inc"
;

static const char* omni_tensor_cuda_complex_matrix_ptx =
#include "tensor_cuda_complex_matrix_ptx.inc"
;

static int omni_tensor_cuda_resolve(void) {
    if (omni_tensor_cuda_disabled_for_tests) return 0;
    if (omni_cuda_malloc != NULL &&
        omni_cuda_free != NULL &&
        omni_cuda_memcpy != NULL &&
        omni_cuda_get_device_count != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_resolution_attempted) return 0;
    omni_tensor_cuda_resolution_attempted = 1;

    const char* candidates[] = {
        "libcudart.so",
        "libcudart.so.13",
        "libcudart.so.12",
        "libcudart.so.11",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* malloc_symbol = dlsym(handle, "cudaMalloc");
        void* free_symbol = dlsym(handle, "cudaFree");
        void* memcpy_symbol = dlsym(handle, "cudaMemcpy");
        void* device_count_symbol = dlsym(handle, "cudaGetDeviceCount");
        if (malloc_symbol != NULL &&
            free_symbol != NULL &&
            memcpy_symbol != NULL &&
            device_count_symbol != NULL) {
            omni_tensor_cuda_handle = handle;
            omni_cuda_malloc = (omni_cuda_malloc_fn)malloc_symbol;
            omni_cuda_free = (omni_cuda_free_fn)free_symbol;
            omni_cuda_memcpy = (omni_cuda_memcpy_fn)memcpy_symbol;
            omni_cuda_get_device_count = (omni_cuda_get_device_count_fn)device_count_symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

static int omni_tensor_cuda_driver_resolve(void) {
    if (omni_tensor_cuda_disabled_for_tests) return 0;
    if (omni_cu_init != NULL &&
        omni_cu_module_load_data != NULL &&
        omni_cu_module_get_function != NULL &&
        omni_cu_module_unload != NULL &&
        omni_cu_launch_kernel != NULL &&
        omni_cu_ctx_synchronize != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_driver_resolution_attempted) return 0;
    omni_tensor_cuda_driver_resolution_attempted = 1;

    const char* candidates[] = {
        "libcuda.so",
        "libcuda.so.1",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* init_symbol = dlsym(handle, "cuInit");
        void* module_load_data_symbol = dlsym(handle, "cuModuleLoadData");
        void* module_get_function_symbol = dlsym(handle, "cuModuleGetFunction");
        void* module_unload_symbol = dlsym(handle, "cuModuleUnload");
        void* launch_kernel_symbol = dlsym(handle, "cuLaunchKernel");
        void* ctx_synchronize_symbol = dlsym(handle, "cuCtxSynchronize");
        if (init_symbol != NULL &&
            module_load_data_symbol != NULL &&
            module_get_function_symbol != NULL &&
            module_unload_symbol != NULL &&
            launch_kernel_symbol != NULL &&
            ctx_synchronize_symbol != NULL) {
            omni_tensor_cuda_driver_handle = handle;
            omni_cu_init = (omni_cu_init_fn)init_symbol;
            omni_cu_module_load_data = (omni_cu_module_load_data_fn)module_load_data_symbol;
            omni_cu_module_get_function = (omni_cu_module_get_function_fn)module_get_function_symbol;
            omni_cu_module_unload = (omni_cu_module_unload_fn)module_unload_symbol;
            omni_cu_launch_kernel = (omni_cu_launch_kernel_fn)launch_kernel_symbol;
            omni_cu_ctx_synchronize = (omni_cu_ctx_synchronize_fn)ctx_synchronize_symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

static int omni_tensor_cuda_map_resolve(void) {
    if (omni_tensor_cuda_map_f64_function != NULL &&
        omni_tensor_cuda_map_f32_function != NULL &&
        omni_tensor_cuda_map_unary_f64_function != NULL &&
        omni_tensor_cuda_map_unary_f32_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_map_module_attempted) return 0;
    omni_tensor_cuda_map_module_attempted = 1;

    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_map_ptx) != 0 || module == NULL) {
        return 0;
    }
    void* f64_function = NULL;
    void* f32_function = NULL;
    void* unary_f64_function = NULL;
    void* unary_f32_function = NULL;
    if (omni_cu_module_get_function(&f64_function, module, "omni_cuda_map_f64") != 0 ||
        omni_cu_module_get_function(&f32_function, module, "omni_cuda_map_f32") != 0 ||
        omni_cu_module_get_function(&unary_f64_function, module, "omni_cuda_map_unary_f64") != 0 ||
        omni_cu_module_get_function(&unary_f32_function, module, "omni_cuda_map_unary_f32") != 0 ||
        f64_function == NULL ||
        f32_function == NULL ||
        unary_f64_function == NULL ||
        unary_f32_function == NULL) {
        return 0;
    }

    omni_tensor_cuda_map_module = module;
    omni_tensor_cuda_map_f64_function = f64_function;
    omni_tensor_cuda_map_f32_function = f32_function;
    omni_tensor_cuda_map_unary_f64_function = unary_f64_function;
    omni_tensor_cuda_map_unary_f32_function = unary_f32_function;
    return 1;
}

static int omni_tensor_cuda_map_scientific_resolve(void) {
    if (omni_tensor_cuda_map_scientific_f64_function != NULL &&
        omni_tensor_cuda_map_scientific_f32_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_map_scientific_module_attempted) return 0;
    omni_tensor_cuda_map_scientific_module_attempted = 1;

    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_scientific_ptx) != 0 || module == NULL) {
        return 0;
    }

    void* f64_function = NULL;
    void* f32_function = NULL;
    if (omni_cu_module_get_function(&f64_function, module, "omni_cuda_map_scientific_f64") != 0 ||
        omni_cu_module_get_function(&f32_function, module, "omni_cuda_map_scientific_f32") != 0 ||
        f64_function == NULL ||
        f32_function == NULL) {
        return 0;
    }

    omni_tensor_cuda_map_scientific_module = module;
    omni_tensor_cuda_map_scientific_f64_function = f64_function;
    omni_tensor_cuda_map_scientific_f32_function = f32_function;
    return 1;
}

static int omni_tensor_cuda_complex_map_resolve(void) {
    if (omni_tensor_cuda_map_complex128_function != NULL &&
        omni_tensor_cuda_map_complex64_function != NULL &&
        omni_tensor_cuda_map_complex128_unary_function != NULL &&
        omni_tensor_cuda_map_complex64_unary_function != NULL &&
        omni_tensor_cuda_map_complex128_to_real_function != NULL &&
        omni_tensor_cuda_map_complex64_to_real_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_complex_map_module_attempted) return 0;

    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_complex_map_ptx) != 0 || module == NULL) {
        omni_tensor_cuda_complex_map_module_attempted = 1;
        return 0;
    }

    void* complex128_function = NULL;
    void* complex64_function = NULL;
    void* complex128_unary_function = NULL;
    void* complex64_unary_function = NULL;
    void* complex128_to_real_function = NULL;
    void* complex64_to_real_function = NULL;
    if (omni_cu_module_get_function(&complex128_function, module, "omni_cuda_map_complex128") != 0 ||
        omni_cu_module_get_function(&complex64_function, module, "omni_cuda_map_complex64") != 0 ||
        omni_cu_module_get_function(&complex128_unary_function, module, "omni_cuda_map_complex128_unary") != 0 ||
        omni_cu_module_get_function(&complex64_unary_function, module, "omni_cuda_map_complex64_unary") != 0 ||
        omni_cu_module_get_function(&complex128_to_real_function, module, "omni_cuda_map_complex128_to_real") != 0 ||
        omni_cu_module_get_function(&complex64_to_real_function, module, "omni_cuda_map_complex64_to_real") != 0 ||
        complex128_function == NULL ||
        complex64_function == NULL ||
        complex128_unary_function == NULL ||
        complex64_unary_function == NULL ||
        complex128_to_real_function == NULL ||
        complex64_to_real_function == NULL) {
        omni_cu_module_unload(module);
        omni_tensor_cuda_complex_map_module_attempted = 1;
        return 0;
    }

    omni_tensor_cuda_complex_map_module = module;
    omni_tensor_cuda_map_complex128_function = complex128_function;
    omni_tensor_cuda_map_complex64_function = complex64_function;
    omni_tensor_cuda_map_complex128_unary_function = complex128_unary_function;
    omni_tensor_cuda_map_complex64_unary_function = complex64_unary_function;
    omni_tensor_cuda_map_complex128_to_real_function = complex128_to_real_function;
    omni_tensor_cuda_map_complex64_to_real_function = complex64_to_real_function;
    return 1;
}

static int omni_tensor_cuda_complex_matrix_resolve(void) {
    if (omni_tensor_cuda_transpose_complex128_function != NULL &&
        omni_tensor_cuda_transpose_complex64_function != NULL &&
        omni_tensor_cuda_diagonal_complex128_function != NULL &&
        omni_tensor_cuda_diagonal_complex64_function != NULL &&
        omni_tensor_cuda_diagonal_matrix_complex128_function != NULL &&
        omni_tensor_cuda_diagonal_matrix_complex64_function != NULL &&
        omni_tensor_cuda_trace_complex128_function != NULL &&
        omni_tensor_cuda_trace_complex64_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_complex_matrix_module_attempted) return 0;

    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_complex_matrix_ptx) != 0 || module == NULL) {
        omni_tensor_cuda_complex_matrix_module_attempted = 1;
        return 0;
    }

    void* transpose_complex128_function = NULL;
    void* transpose_complex64_function = NULL;
    void* diagonal_complex128_function = NULL;
    void* diagonal_complex64_function = NULL;
    void* diagonal_matrix_complex128_function = NULL;
    void* diagonal_matrix_complex64_function = NULL;
    void* trace_complex128_function = NULL;
    void* trace_complex64_function = NULL;
    if (omni_cu_module_get_function(&transpose_complex128_function, module, "omni_cuda_transpose_complex128") != 0 ||
        omni_cu_module_get_function(&transpose_complex64_function, module, "omni_cuda_transpose_complex64") != 0 ||
        omni_cu_module_get_function(&diagonal_complex128_function, module, "omni_cuda_diagonal_complex128") != 0 ||
        omni_cu_module_get_function(&diagonal_complex64_function, module, "omni_cuda_diagonal_complex64") != 0 ||
        omni_cu_module_get_function(&diagonal_matrix_complex128_function, module, "omni_cuda_diagonal_matrix_complex128") != 0 ||
        omni_cu_module_get_function(&diagonal_matrix_complex64_function, module, "omni_cuda_diagonal_matrix_complex64") != 0 ||
        omni_cu_module_get_function(&trace_complex128_function, module, "omni_cuda_trace_complex128") != 0 ||
        omni_cu_module_get_function(&trace_complex64_function, module, "omni_cuda_trace_complex64") != 0 ||
        transpose_complex128_function == NULL ||
        transpose_complex64_function == NULL ||
        diagonal_complex128_function == NULL ||
        diagonal_complex64_function == NULL ||
        diagonal_matrix_complex128_function == NULL ||
        diagonal_matrix_complex64_function == NULL ||
        trace_complex128_function == NULL ||
        trace_complex64_function == NULL) {
        omni_cu_module_unload(module);
        omni_tensor_cuda_complex_matrix_module_attempted = 1;
        return 0;
    }

    omni_tensor_cuda_complex_matrix_module = module;
    omni_tensor_cuda_transpose_complex128_function = transpose_complex128_function;
    omni_tensor_cuda_transpose_complex64_function = transpose_complex64_function;
    omni_tensor_cuda_diagonal_complex128_function = diagonal_complex128_function;
    omni_tensor_cuda_diagonal_complex64_function = diagonal_complex64_function;
    omni_tensor_cuda_diagonal_matrix_complex128_function = diagonal_matrix_complex128_function;
    omni_tensor_cuda_diagonal_matrix_complex64_function = diagonal_matrix_complex64_function;
    omni_tensor_cuda_trace_complex128_function = trace_complex128_function;
    omni_tensor_cuda_trace_complex64_function = trace_complex64_function;
    return 1;
}

static int omni_tensor_cuda_round_resolve(void) {
    if (omni_tensor_cuda_round_i64_f64_function != NULL &&
        omni_tensor_cuda_round_i64_f32_function != NULL) {
        return 1;
    }
    if (omni_tensor_cuda_round_module_attempted) return 0;
    omni_tensor_cuda_round_module_attempted = 1;

    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cuda_driver_resolve()) return 0;
    if (omni_cu_init(0) != 0) return 0;

    void* module = NULL;
    if (omni_cu_module_load_data(&module, omni_tensor_cuda_round_ptx) != 0 || module == NULL) {
        return 0;
    }

    void* f64_function = NULL;
    void* f32_function = NULL;
    if (omni_cu_module_get_function(&f64_function, module, "omni_cuda_round_i64_f64") != 0 ||
        omni_cu_module_get_function(&f32_function, module, "omni_cuda_round_i64_f32") != 0 ||
        f64_function == NULL ||
        f32_function == NULL) {
        return 0;
    }

    omni_tensor_cuda_round_module = module;
    omni_tensor_cuda_round_i64_f64_function = f64_function;
    omni_tensor_cuda_round_i64_f32_function = f32_function;
    return 1;
}

static int omni_tensor_cublas_resolve(void) {
    if (omni_tensor_cublas_disabled_for_tests) return 0;
    if (omni_cublas_create != NULL &&
        omni_cublas_destroy != NULL &&
        omni_cublas_dgemm != NULL &&
        omni_cublas_dgemv != NULL) {
        return 1;
    }
    if (omni_tensor_cublas_resolution_attempted) return 0;
    omni_tensor_cublas_resolution_attempted = 1;

    const char* candidates[] = {
        "libcublas.so",
        "libcublas.so.13",
        "libcublas.so.12",
        "libcublas.so.11",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* create_symbol = dlsym(handle, "cublasCreate_v2");
        void* destroy_symbol = dlsym(handle, "cublasDestroy_v2");
        void* dgemm_symbol = dlsym(handle, "cublasDgemm_v2");
        void* dgemv_symbol = dlsym(handle, "cublasDgemv_v2");
        void* sgemm_symbol = dlsym(handle, "cublasSgemm_v2");
        void* sgemv_symbol = dlsym(handle, "cublasSgemv_v2");
        void* zgemm_symbol = dlsym(handle, "cublasZgemm_v2");
        void* cgemm_symbol = dlsym(handle, "cublasCgemm_v2");
        void* zgemv_symbol = dlsym(handle, "cublasZgemv_v2");
        void* cgemv_symbol = dlsym(handle, "cublasCgemv_v2");
        if (create_symbol != NULL &&
            destroy_symbol != NULL &&
            dgemm_symbol != NULL &&
            dgemv_symbol != NULL) {
            omni_tensor_cublas_handle = handle;
            omni_cublas_create = (omni_cublas_create_fn)create_symbol;
            omni_cublas_destroy = (omni_cublas_destroy_fn)destroy_symbol;
            omni_cublas_dgemm = (omni_cublas_dgemm_fn)dgemm_symbol;
            omni_cublas_dgemv = (omni_cublas_dgemv_fn)dgemv_symbol;
            if (sgemm_symbol != NULL) omni_cublas_sgemm = (omni_cublas_sgemm_fn)sgemm_symbol;
            if (sgemv_symbol != NULL) omni_cublas_sgemv = (omni_cublas_sgemv_fn)sgemv_symbol;
            if (zgemm_symbol != NULL) omni_cublas_zgemm = (omni_cublas_zgemm_fn)zgemm_symbol;
            if (cgemm_symbol != NULL) omni_cublas_cgemm = (omni_cublas_cgemm_fn)cgemm_symbol;
            if (zgemv_symbol != NULL) omni_cublas_zgemv = (omni_cublas_zgemv_fn)zgemv_symbol;
            if (cgemv_symbol != NULL) omni_cublas_cgemv = (omni_cublas_cgemv_fn)cgemv_symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

#include "tensor_cuda_helpers_public_memory.inc"
#include "tensor_cuda_helpers_map_binary.inc"
#include "tensor_cuda_helpers_map_unary_round.inc"
#include "tensor_cuda_helpers_complex_matrix.inc"
#include "tensor_cuda_helpers_cublas.inc"
