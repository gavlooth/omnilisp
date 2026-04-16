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
".version 6.4\n"
".target sm_30\n"
".address_size 64\n"
".visible .entry omni_cuda_map_f64(\n"
"    .param .u64 left_ptr,\n"
"    .param .u64 right_ptr,\n"
"    .param .f64 scalar_value,\n"
"    .param .u32 mode_value,\n"
"    .param .u32 op_value,\n"
"    .param .u64 element_count,\n"
"    .param .u64 left_offsets_ptr,\n"
"    .param .u64 right_offsets_ptr,\n"
"    .param .u64 out_ptr\n"
") {\n"
"    .reg .pred %p<14>;\n"
"    .reg .b32 %r<8>;\n"
"    .reg .b64 %rd<22>;\n"
"    .reg .f64 %fd<5>;\n"
"    ld.param.u64 %rd1, [left_ptr];\n"
"    ld.param.u64 %rd2, [right_ptr];\n"
"    ld.param.f64 %fd3, [scalar_value];\n"
"    ld.param.u32 %r5, [mode_value];\n"
"    ld.param.u32 %r6, [op_value];\n"
"    ld.param.u64 %rd4, [element_count];\n"
"    ld.param.u64 %rd11, [left_offsets_ptr];\n"
"    ld.param.u64 %rd12, [right_offsets_ptr];\n"
"    ld.param.u64 %rd5, [out_ptr];\n"
"    mov.u32 %r1, %tid.x;\n"
"    mov.u32 %r2, %ctaid.x;\n"
"    mov.u32 %r3, %ntid.x;\n"
"    mad.lo.u32 %r4, %r2, %r3, %r1;\n"
"    cvt.u64.u32 %rd6, %r4;\n"
"    setp.ge.u64 %p1, %rd6, %rd4;\n"
"    @%p1 bra F64_DONE;\n"
"    mul.lo.u64 %rd7, %rd6, 8;\n"
"    add.u64 %rd10, %rd5, %rd7;\n"
"    setp.eq.u32 %p2, %r5, 1;\n"
"    @%p2 bra F64_LEFT_SCALAR;\n"
"    mov.u64 %rd13, %rd6;\n"
"    setp.eq.u64 %p11, %rd11, 0;\n"
"    @%p11 bra F64_LEFT_OFFSET_READY;\n"
"    add.u64 %rd15, %rd11, %rd7;\n"
"    ld.global.u64 %rd13, [%rd15];\n"
"F64_LEFT_OFFSET_READY:\n"
"    mul.lo.u64 %rd8, %rd13, 8;\n"
"    add.u64 %rd8, %rd1, %rd8;\n"
"    ld.global.f64 %fd1, [%rd8];\n"
"    bra F64_LEFT_READY;\n"
"F64_LEFT_SCALAR:\n"
"    mov.f64 %fd1, %fd3;\n"
"F64_LEFT_READY:\n"
"    setp.eq.u32 %p3, %r5, 0;\n"
"    @%p3 bra F64_RIGHT_SCALAR;\n"
"    mov.u64 %rd14, %rd6;\n"
"    setp.eq.u64 %p12, %rd12, 0;\n"
"    @%p12 bra F64_RIGHT_OFFSET_READY;\n"
"    add.u64 %rd16, %rd12, %rd7;\n"
"    ld.global.u64 %rd14, [%rd16];\n"
"F64_RIGHT_OFFSET_READY:\n"
"    mul.lo.u64 %rd9, %rd14, 8;\n"
"    add.u64 %rd9, %rd2, %rd9;\n"
"    ld.global.f64 %fd2, [%rd9];\n"
"    bra F64_RIGHT_READY;\n"
"F64_RIGHT_SCALAR:\n"
"    mov.f64 %fd2, %fd3;\n"
"F64_RIGHT_READY:\n"
"    setp.eq.u32 %p4, %r6, 0;\n"
"    @%p4 bra F64_ADD;\n"
"    setp.eq.u32 %p5, %r6, 1;\n"
"    @%p5 bra F64_SUB;\n"
"    setp.eq.u32 %p6, %r6, 2;\n"
"    @%p6 bra F64_MUL;\n"
"    setp.eq.u32 %p7, %r6, 3;\n"
"    @%p7 bra F64_DIV;\n"
"    setp.eq.u32 %p8, %r6, 4;\n"
"    @%p8 bra F64_MIN;\n"
"    bra F64_MAX;\n"
"F64_ADD:\n"
"    add.rn.f64 %fd4, %fd1, %fd2;\n"
"    bra F64_STORE;\n"
"F64_SUB:\n"
"    sub.rn.f64 %fd4, %fd1, %fd2;\n"
"    bra F64_STORE;\n"
"F64_MUL:\n"
"    mul.rn.f64 %fd4, %fd1, %fd2;\n"
"    bra F64_STORE;\n"
"F64_DIV:\n"
"    div.rn.f64 %fd4, %fd1, %fd2;\n"
"    bra F64_STORE;\n"
"F64_MIN:\n"
"    setp.lt.f64 %p9, %fd1, %fd2;\n"
"    selp.f64 %fd4, %fd1, %fd2, %p9;\n"
"    bra F64_STORE;\n"
"F64_MAX:\n"
"    setp.gt.f64 %p10, %fd1, %fd2;\n"
"    selp.f64 %fd4, %fd1, %fd2, %p10;\n"
"F64_STORE:\n"
"    st.global.f64 [%rd10], %fd4;\n"
"F64_DONE:\n"
"    ret;\n"
"}\n"
".visible .entry omni_cuda_map_f32(\n"
"    .param .u64 left_ptr,\n"
"    .param .u64 right_ptr,\n"
"    .param .f32 scalar_value,\n"
"    .param .u32 mode_value,\n"
"    .param .u32 op_value,\n"
"    .param .u64 element_count,\n"
"    .param .u64 left_offsets_ptr,\n"
"    .param .u64 right_offsets_ptr,\n"
"    .param .u64 out_ptr\n"
") {\n"
"    .reg .pred %p<14>;\n"
"    .reg .b32 %r<8>;\n"
"    .reg .b64 %rd<22>;\n"
"    .reg .f32 %f<5>;\n"
"    ld.param.u64 %rd1, [left_ptr];\n"
"    ld.param.u64 %rd2, [right_ptr];\n"
"    ld.param.f32 %f3, [scalar_value];\n"
"    ld.param.u32 %r5, [mode_value];\n"
"    ld.param.u32 %r6, [op_value];\n"
"    ld.param.u64 %rd4, [element_count];\n"
"    ld.param.u64 %rd11, [left_offsets_ptr];\n"
"    ld.param.u64 %rd12, [right_offsets_ptr];\n"
"    ld.param.u64 %rd5, [out_ptr];\n"
"    mov.u32 %r1, %tid.x;\n"
"    mov.u32 %r2, %ctaid.x;\n"
"    mov.u32 %r3, %ntid.x;\n"
"    mad.lo.u32 %r4, %r2, %r3, %r1;\n"
"    cvt.u64.u32 %rd6, %r4;\n"
"    setp.ge.u64 %p1, %rd6, %rd4;\n"
"    @%p1 bra F32_DONE;\n"
"    mul.lo.u64 %rd7, %rd6, 4;\n"
"    add.u64 %rd10, %rd5, %rd7;\n"
"    setp.eq.u32 %p2, %r5, 1;\n"
"    @%p2 bra F32_LEFT_SCALAR;\n"
"    mov.u64 %rd13, %rd6;\n"
"    setp.eq.u64 %p11, %rd11, 0;\n"
"    @%p11 bra F32_LEFT_OFFSET_READY;\n"
"    mul.lo.u64 %rd17, %rd6, 8;\n"
"    add.u64 %rd15, %rd11, %rd17;\n"
"    ld.global.u64 %rd13, [%rd15];\n"
"F32_LEFT_OFFSET_READY:\n"
"    mul.lo.u64 %rd8, %rd13, 4;\n"
"    add.u64 %rd8, %rd1, %rd8;\n"
"    ld.global.f32 %f1, [%rd8];\n"
"    bra F32_LEFT_READY;\n"
"F32_LEFT_SCALAR:\n"
"    mov.f32 %f1, %f3;\n"
"F32_LEFT_READY:\n"
"    setp.eq.u32 %p3, %r5, 0;\n"
"    @%p3 bra F32_RIGHT_SCALAR;\n"
"    mov.u64 %rd14, %rd6;\n"
"    setp.eq.u64 %p12, %rd12, 0;\n"
"    @%p12 bra F32_RIGHT_OFFSET_READY;\n"
"    mul.lo.u64 %rd18, %rd6, 8;\n"
"    add.u64 %rd16, %rd12, %rd18;\n"
"    ld.global.u64 %rd14, [%rd16];\n"
"F32_RIGHT_OFFSET_READY:\n"
"    mul.lo.u64 %rd9, %rd14, 4;\n"
"    add.u64 %rd9, %rd2, %rd9;\n"
"    ld.global.f32 %f2, [%rd9];\n"
"    bra F32_RIGHT_READY;\n"
"F32_RIGHT_SCALAR:\n"
"    mov.f32 %f2, %f3;\n"
"F32_RIGHT_READY:\n"
"    setp.eq.u32 %p4, %r6, 0;\n"
"    @%p4 bra F32_ADD;\n"
"    setp.eq.u32 %p5, %r6, 1;\n"
"    @%p5 bra F32_SUB;\n"
"    setp.eq.u32 %p6, %r6, 2;\n"
"    @%p6 bra F32_MUL;\n"
"    setp.eq.u32 %p7, %r6, 3;\n"
"    @%p7 bra F32_DIV;\n"
"    setp.eq.u32 %p8, %r6, 4;\n"
"    @%p8 bra F32_MIN;\n"
"    bra F32_MAX;\n"
"F32_ADD:\n"
"    add.rn.f32 %f4, %f1, %f2;\n"
"    bra F32_STORE;\n"
"F32_SUB:\n"
"    sub.rn.f32 %f4, %f1, %f2;\n"
"    bra F32_STORE;\n"
"F32_MUL:\n"
"    mul.rn.f32 %f4, %f1, %f2;\n"
"    bra F32_STORE;\n"
"F32_DIV:\n"
"    div.rn.f32 %f4, %f1, %f2;\n"
"    bra F32_STORE;\n"
"F32_MIN:\n"
"    setp.lt.f32 %p9, %f1, %f2;\n"
"    selp.f32 %f4, %f1, %f2, %p9;\n"
"    bra F32_STORE;\n"
"F32_MAX:\n"
"    setp.gt.f32 %p10, %f1, %f2;\n"
"    selp.f32 %f4, %f1, %f2, %p10;\n"
"F32_STORE:\n"
"    st.global.f32 [%rd10], %f4;\n"
"F32_DONE:\n"
"    ret;\n"
"}\n"
".visible .entry omni_cuda_map_unary_f64(\n"
"    .param .u64 input_ptr,\n"
"    .param .u32 op_value,\n"
"    .param .u64 element_count,\n"
"    .param .u64 out_ptr\n"
") {\n"
"    .reg .pred %p<8>;\n"
"    .reg .b32 %r<7>;\n"
"    .reg .b64 %rd<8>;\n"
"    .reg .f64 %fd<3>;\n"
"    ld.param.u64 %rd1, [input_ptr];\n"
"    ld.param.u32 %r5, [op_value];\n"
"    ld.param.u64 %rd2, [element_count];\n"
"    ld.param.u64 %rd3, [out_ptr];\n"
"    mov.u32 %r1, %tid.x;\n"
"    mov.u32 %r2, %ctaid.x;\n"
"    mov.u32 %r3, %ntid.x;\n"
"    mad.lo.u32 %r4, %r2, %r3, %r1;\n"
"    cvt.u64.u32 %rd4, %r4;\n"
"    setp.ge.u64 %p1, %rd4, %rd2;\n"
"    @%p1 bra F64_UNARY_DONE;\n"
"    mul.lo.u64 %rd5, %rd4, 8;\n"
"    add.u64 %rd6, %rd1, %rd5;\n"
"    add.u64 %rd7, %rd3, %rd5;\n"
"    ld.global.f64 %fd1, [%rd6];\n"
"    setp.eq.u32 %p2, %r5, 0;\n"
"    @%p2 bra F64_UNARY_ABS;\n"
"    setp.eq.u32 %p3, %r5, 1;\n"
"    @%p3 bra F64_UNARY_NEG;\n"
"    setp.eq.u32 %p4, %r5, 2;\n"
"    @%p4 bra F64_UNARY_SQRT;\n"
"    setp.eq.u32 %p5, %r5, 3;\n"
"    @%p5 bra F64_UNARY_IDENTITY;\n"
"    bra F64_UNARY_ZERO;\n"
"F64_UNARY_ABS:\n"
"    abs.f64 %fd2, %fd1;\n"
"    bra F64_UNARY_STORE;\n"
"F64_UNARY_NEG:\n"
"    neg.f64 %fd2, %fd1;\n"
"    bra F64_UNARY_STORE;\n"
"F64_UNARY_SQRT:\n"
"    sqrt.rn.f64 %fd2, %fd1;\n"
"    bra F64_UNARY_STORE;\n"
"F64_UNARY_IDENTITY:\n"
"    mov.f64 %fd2, %fd1;\n"
"    bra F64_UNARY_STORE;\n"
"F64_UNARY_ZERO:\n"
"    mov.f64 %fd2, 0d0000000000000000;\n"
"F64_UNARY_STORE:\n"
"    st.global.f64 [%rd7], %fd2;\n"
"F64_UNARY_DONE:\n"
"    ret;\n"
"}\n"
".visible .entry omni_cuda_map_unary_f32(\n"
"    .param .u64 input_ptr,\n"
"    .param .u32 op_value,\n"
"    .param .u64 element_count,\n"
"    .param .u64 out_ptr\n"
") {\n"
"    .reg .pred %p<8>;\n"
"    .reg .b32 %r<7>;\n"
"    .reg .b64 %rd<8>;\n"
"    .reg .f32 %f<3>;\n"
"    ld.param.u64 %rd1, [input_ptr];\n"
"    ld.param.u32 %r5, [op_value];\n"
"    ld.param.u64 %rd2, [element_count];\n"
"    ld.param.u64 %rd3, [out_ptr];\n"
"    mov.u32 %r1, %tid.x;\n"
"    mov.u32 %r2, %ctaid.x;\n"
"    mov.u32 %r3, %ntid.x;\n"
"    mad.lo.u32 %r4, %r2, %r3, %r1;\n"
"    cvt.u64.u32 %rd4, %r4;\n"
"    setp.ge.u64 %p1, %rd4, %rd2;\n"
"    @%p1 bra F32_UNARY_DONE;\n"
"    mul.lo.u64 %rd5, %rd4, 4;\n"
"    add.u64 %rd6, %rd1, %rd5;\n"
"    add.u64 %rd7, %rd3, %rd5;\n"
"    ld.global.f32 %f1, [%rd6];\n"
"    setp.eq.u32 %p2, %r5, 0;\n"
"    @%p2 bra F32_UNARY_ABS;\n"
"    setp.eq.u32 %p3, %r5, 1;\n"
"    @%p3 bra F32_UNARY_NEG;\n"
"    setp.eq.u32 %p4, %r5, 2;\n"
"    @%p4 bra F32_UNARY_SQRT;\n"
"    setp.eq.u32 %p5, %r5, 3;\n"
"    @%p5 bra F32_UNARY_IDENTITY;\n"
"    bra F32_UNARY_ZERO;\n"
"F32_UNARY_ABS:\n"
"    abs.f32 %f2, %f1;\n"
"    bra F32_UNARY_STORE;\n"
"F32_UNARY_NEG:\n"
"    neg.f32 %f2, %f1;\n"
"    bra F32_UNARY_STORE;\n"
"F32_UNARY_SQRT:\n"
"    sqrt.rn.f32 %f2, %f1;\n"
"    bra F32_UNARY_STORE;\n"
"F32_UNARY_IDENTITY:\n"
"    mov.f32 %f2, %f1;\n"
"    bra F32_UNARY_STORE;\n"
"F32_UNARY_ZERO:\n"
"    mov.f32 %f2, 0f00000000;\n"
"F32_UNARY_STORE:\n"
"    st.global.f32 [%rd7], %f2;\n"
"F32_UNARY_DONE:\n"
"    ret;\n"
"}\n";

static const char* omni_tensor_cuda_scientific_ptx =
/* Generated from csrc/tensor_cuda_scientific_unary.cu with CUDA 13 nvcc -arch=compute_75. */
"//\n"
"// Generated by NVIDIA NVVM Compiler\n"
"//\n"
"// Compiler Build ID: CL-36424714\n"
"// Cuda compilation tools, release 13.0, V13.0.88\n"
"// Based on NVVM 7.0.1\n"
"//\n"
"\n"
".version 9.0\n"
".target sm_75\n"
".address_size 64\n"
"\n"
"	// .globl	omni_cuda_map_scientific_f64\n"
".func  (.param .align 8 .b8 func_retval0[16]) __internal_trig_reduction_slowpathd\n"
"(\n"
"	.param .b64 __internal_trig_reduction_slowpathd_param_0\n"
")\n"
";\n"
".global .align 4 .b8 __cudart_i2opi_f[24] = {65, 144, 67, 60, 153, 149, 98, 219, 192, 221, 52, 245, 209, 87, 39, 252, 41, 21, 68, 78, 110, 131, 249, 162};\n"
".global .align 16 .b8 __cudart_sin_cos_coeffs[128] = {70, 210, 176, 44, 241, 229, 90, 190, 146, 227, 172, 105, 227, 29, 199, 62, 161, 98, 219, 25, 160, 1, 42, 191, 24, 8, 17, 17, 17, 17, 129, 63, 84, 85, 85, 85, 85, 85, 197, 191, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 129, 253, 32, 131, 255, 168, 189, 40, 133, 239, 193, 167, 238, 33, 62, 217, 230, 6, 142, 79, 126, 146, 190, 233, 188, 221, 25, 160, 1, 250, 62, 71, 93, 193, 22, 108, 193, 86, 191, 81, 85, 85, 85, 85, 85, 165, 63, 0, 0, 0, 0, 0, 0, 224, 191, 0, 0, 0, 0, 0, 0, 240, 63};\n"
".global .align 8 .b8 __cudart_i2opi_d[144] = {8, 93, 141, 31, 177, 95, 251, 107, 234, 146, 82, 138, 247, 57, 7, 61, 123, 241, 229, 235, 199, 186, 39, 117, 45, 234, 95, 158, 102, 63, 70, 79, 183, 9, 203, 39, 207, 126, 54, 109, 31, 109, 10, 90, 139, 17, 47, 239, 15, 152, 5, 222, 255, 151, 248, 31, 59, 40, 249, 189, 139, 95, 132, 156, 244, 57, 83, 131, 57, 214, 145, 57, 65, 126, 95, 180, 38, 112, 156, 233, 132, 68, 187, 46, 245, 53, 130, 232, 62, 167, 41, 177, 28, 235, 29, 254, 28, 146, 209, 9, 234, 46, 73, 6, 224, 210, 77, 66, 58, 110, 36, 183, 97, 197, 187, 222, 171, 99, 81, 254, 65, 144, 67, 60, 153, 149, 98, 219, 192, 221, 52, 245, 209, 87, 39, 252, 41, 21, 68, 78, 110, 131, 249, 162};\n"
"\n"
".visible .entry omni_cuda_map_scientific_f64(\n"
"	.param .u64 omni_cuda_map_scientific_f64_param_0,\n"
"	.param .u32 omni_cuda_map_scientific_f64_param_1,\n"
"	.param .u64 omni_cuda_map_scientific_f64_param_2,\n"
"	.param .u64 omni_cuda_map_scientific_f64_param_3,\n"
"	.param .u64 omni_cuda_map_scientific_f64_param_4\n"
")\n"
"{\n"
"	.reg .pred 	%p<99>;\n"
"	.reg .f32 	%f<17>;\n"
"	.reg .b32 	%r<221>;\n"
"	.reg .f64 	%fd<1320>;\n"
"	.reg .b64 	%rd<26>;\n"
"\n"
"\n"
"	ld.param.u64 	%rd3, [omni_cuda_map_scientific_f64_param_0];\n"
"	ld.param.u32 	%r54, [omni_cuda_map_scientific_f64_param_1];\n"
"	ld.param.u64 	%rd5, [omni_cuda_map_scientific_f64_param_2];\n"
"	ld.param.u64 	%rd6, [omni_cuda_map_scientific_f64_param_4];\n"
"	cvta.to.global.u64 	%rd1, %rd6;\n"
"	mov.u32 	%r55, %ctaid.x;\n"
"	mov.u32 	%r56, %ntid.x;\n"
"	mul.wide.u32 	%rd7, %r55, %r56;\n"
"	mov.u32 	%r57, %tid.x;\n"
"	cvt.u64.u32 	%rd8, %r57;\n"
"	add.s64 	%rd2, %rd7, %rd8;\n"
"	setp.ge.u64 	%p1, %rd2, %rd5;\n"
"	@%p1 bra 	$L__BB0_116;\n"
"\n"
"	cvta.to.global.u64 	%rd9, %rd3;\n"
"	shl.b64 	%rd10, %rd2, 3;\n"
"	add.s64 	%rd11, %rd9, %rd10;\n"
"	ld.global.f64 	%fd1, [%rd11];\n"
"	setp.gt.s32 	%p2, %r54, 12;\n"
"	@%p2 bra 	$L__BB0_23;\n"
"\n"
"	setp.gt.s32 	%p14, %r54, 8;\n"
"	@%p14 bra 	$L__BB0_13;\n"
"\n"
"	setp.gt.s32 	%p20, %r54, 6;\n"
"	@%p20 bra 	$L__BB0_9;\n"
"\n"
"	setp.eq.s32 	%p23, %r54, 5;\n"
"	@%p23 bra 	$L__BB0_107;\n"
"\n"
"	setp.eq.s32 	%p24, %r54, 6;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p24 bra 	$L__BB0_6;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_6:\n"
"	abs.f64 	%fd86, %fd1;\n"
"	setp.eq.f64 	%p91, %fd86, 0d7FF0000000000000;\n"
"	@%p91 bra 	$L__BB0_102;\n"
"	bra.uni 	$L__BB0_7;\n"
"\n"
"$L__BB0_102:\n"
"	mov.f64 	%fd1236, 0d0000000000000000;\n"
"	mul.rn.f64 	%fd1315, %fd1, %fd1236;\n"
"	mov.u32 	%r219, 0;\n"
"	bra.uni 	$L__BB0_103;\n"
"\n"
"$L__BB0_23:\n"
"	setp.gt.s32 	%p3, %r54, 16;\n"
"	@%p3 bra 	$L__BB0_36;\n"
"\n"
"	setp.gt.s32 	%p9, %r54, 14;\n"
"	@%p9 bra 	$L__BB0_30;\n"
"\n"
"	setp.eq.s32 	%p12, %r54, 13;\n"
"	@%p12 bra 	$L__BB0_77;\n"
"\n"
"	setp.eq.s32 	%p13, %r54, 14;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p13 bra 	$L__BB0_27;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_27:\n"
"	mov.f64 	%fd773, 0d4338000000000000;\n"
"	mov.f64 	%fd774, 0d3FF71547652B82FE;\n"
"	fma.rn.f64 	%fd775, %fd1, %fd774, %fd773;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r38, %temp}, %fd775;\n"
"	}\n"
"	mov.f64 	%fd776, 0dC338000000000000;\n"
"	add.rn.f64 	%fd777, %fd775, %fd776;\n"
"	mov.f64 	%fd778, 0dBFE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd779, %fd777, %fd778, %fd1;\n"
"	mov.f64 	%fd780, 0dBC7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd781, %fd777, %fd780, %fd779;\n"
"	mov.f64 	%fd782, 0d3E928AF3FCA213EA;\n"
"	mov.f64 	%fd783, 0d3E5ADE1569CE2BDF;\n"
"	fma.rn.f64 	%fd784, %fd783, %fd781, %fd782;\n"
"	mov.f64 	%fd785, 0d3EC71DEE62401315;\n"
"	fma.rn.f64 	%fd786, %fd784, %fd781, %fd785;\n"
"	mov.f64 	%fd787, 0d3EFA01997C89EB71;\n"
"	fma.rn.f64 	%fd788, %fd786, %fd781, %fd787;\n"
"	mov.f64 	%fd789, 0d3F2A01A014761F65;\n"
"	fma.rn.f64 	%fd790, %fd788, %fd781, %fd789;\n"
"	mov.f64 	%fd791, 0d3F56C16C1852B7AF;\n"
"	fma.rn.f64 	%fd792, %fd790, %fd781, %fd791;\n"
"	mov.f64 	%fd793, 0d3F81111111122322;\n"
"	fma.rn.f64 	%fd794, %fd792, %fd781, %fd793;\n"
"	mov.f64 	%fd795, 0d3FA55555555502A1;\n"
"	fma.rn.f64 	%fd796, %fd794, %fd781, %fd795;\n"
"	mov.f64 	%fd797, 0d3FC5555555555511;\n"
"	fma.rn.f64 	%fd798, %fd796, %fd781, %fd797;\n"
"	mov.f64 	%fd799, 0d3FE000000000000B;\n"
"	fma.rn.f64 	%fd800, %fd798, %fd781, %fd799;\n"
"	mov.f64 	%fd801, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd802, %fd800, %fd781, %fd801;\n"
"	fma.rn.f64 	%fd803, %fd802, %fd781, %fd801;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r39, %temp}, %fd803;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r40}, %fd803;\n"
"	}\n"
"	shl.b32 	%r139, %r38, 20;\n"
"	add.s32 	%r140, %r40, %r139;\n"
"	mov.b64 	%fd1319, {%r39, %r140};\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r141}, %fd1;\n"
"	}\n"
"	mov.b32 	%f10, %r141;\n"
"	abs.f32 	%f1, %f10;\n"
"	setp.lt.f32 	%p62, %f1, 0f4086232B;\n"
"	@%p62 bra 	$L__BB0_115;\n"
"\n"
"	setp.lt.f64 	%p63, %fd1, 0d0000000000000000;\n"
"	add.f64 	%fd804, %fd1, 0d7FF0000000000000;\n"
"	selp.f64 	%fd1319, 0d0000000000000000, %fd804, %p63;\n"
"	setp.geu.f32 	%p64, %f1, 0f40874800;\n"
"	@%p64 bra 	$L__BB0_115;\n"
"\n"
"	shr.u32 	%r142, %r38, 31;\n"
"	add.s32 	%r143, %r38, %r142;\n"
"	shr.s32 	%r144, %r143, 1;\n"
"	shl.b32 	%r145, %r144, 20;\n"
"	add.s32 	%r146, %r40, %r145;\n"
"	mov.b64 	%fd805, {%r39, %r146};\n"
"	sub.s32 	%r147, %r38, %r144;\n"
"	shl.b32 	%r148, %r147, 20;\n"
"	add.s32 	%r149, %r148, 1072693248;\n"
"	mov.u32 	%r150, 0;\n"
"	mov.b64 	%fd806, {%r150, %r149};\n"
"	mul.f64 	%fd1319, %fd805, %fd806;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_13:\n"
"	setp.gt.s32 	%p15, %r54, 10;\n"
"	@%p15 bra 	$L__BB0_19;\n"
"\n"
"	setp.eq.s32 	%p18, %r54, 9;\n"
"	@%p18 bra 	$L__BB0_84;\n"
"\n"
"	setp.eq.s32 	%p19, %r54, 10;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p19 bra 	$L__BB0_16;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_16:\n"
"	abs.f64 	%fd61, %fd1;\n"
"	setp.leu.f64 	%p74, %fd61, 0d3FF0000000000000;\n"
"	mov.f64 	%fd1311, %fd61;\n"
"	@%p74 bra 	$L__BB0_18;\n"
"\n"
"	rcp.approx.ftz.f64 	%fd970, %fd61;\n"
"	neg.f64 	%fd971, %fd61;\n"
"	mov.f64 	%fd972, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd973, %fd971, %fd970, %fd972;\n"
"	fma.rn.f64 	%fd974, %fd973, %fd973, %fd973;\n"
"	fma.rn.f64 	%fd975, %fd974, %fd970, %fd970;\n"
"	setp.eq.f64 	%p75, %fd61, 0d7FF0000000000000;\n"
"	selp.f64 	%fd1311, 0d0000000000000000, %fd975, %p75;\n"
"\n"
"$L__BB0_18:\n"
"	mul.f64 	%fd976, %fd1311, %fd1311;\n"
"	mov.f64 	%fd977, 0d3F2D3B63DBB65B49;\n"
"	mov.f64 	%fd978, 0dBEF53E1D2A25FF7E;\n"
"	fma.rn.f64 	%fd979, %fd978, %fd976, %fd977;\n"
"	mov.f64 	%fd980, 0dBF5312788DDE082E;\n"
"	fma.rn.f64 	%fd981, %fd979, %fd976, %fd980;\n"
"	mov.f64 	%fd982, 0d3F6F9690C8249315;\n"
"	fma.rn.f64 	%fd983, %fd981, %fd976, %fd982;\n"
"	mov.f64 	%fd984, 0dBF82CF5AABC7CF0D;\n"
"	fma.rn.f64 	%fd985, %fd983, %fd976, %fd984;\n"
"	mov.f64 	%fd986, 0d3F9162B0B2A3BFDE;\n"
"	fma.rn.f64 	%fd987, %fd985, %fd976, %fd986;\n"
"	mov.f64 	%fd988, 0dBF9A7256FEB6FC6B;\n"
"	fma.rn.f64 	%fd989, %fd987, %fd976, %fd988;\n"
"	mov.f64 	%fd990, 0d3FA171560CE4A489;\n"
"	fma.rn.f64 	%fd991, %fd989, %fd976, %fd990;\n"
"	mov.f64 	%fd992, 0dBFA4F44D841450E4;\n"
"	fma.rn.f64 	%fd993, %fd991, %fd976, %fd992;\n"
"	mov.f64 	%fd994, 0d3FA7EE3D3F36BB95;\n"
"	fma.rn.f64 	%fd995, %fd993, %fd976, %fd994;\n"
"	mov.f64 	%fd996, 0dBFAAD32AE04A9FD1;\n"
"	fma.rn.f64 	%fd997, %fd995, %fd976, %fd996;\n"
"	mov.f64 	%fd998, 0d3FAE17813D66954F;\n"
"	fma.rn.f64 	%fd999, %fd997, %fd976, %fd998;\n"
"	mov.f64 	%fd1000, 0dBFB11089CA9A5BCD;\n"
"	fma.rn.f64 	%fd1001, %fd999, %fd976, %fd1000;\n"
"	mov.f64 	%fd1002, 0d3FB3B12B2DB51738;\n"
"	fma.rn.f64 	%fd1003, %fd1001, %fd976, %fd1002;\n"
"	mov.f64 	%fd1004, 0dBFB745D022F8DC5C;\n"
"	fma.rn.f64 	%fd1005, %fd1003, %fd976, %fd1004;\n"
"	mov.f64 	%fd1006, 0d3FBC71C709DFE927;\n"
"	fma.rn.f64 	%fd1007, %fd1005, %fd976, %fd1006;\n"
"	mov.f64 	%fd1008, 0dBFC2492491FA1744;\n"
"	fma.rn.f64 	%fd1009, %fd1007, %fd976, %fd1008;\n"
"	mov.f64 	%fd1010, 0d3FC99999999840D2;\n"
"	fma.rn.f64 	%fd1011, %fd1009, %fd976, %fd1010;\n"
"	mov.f64 	%fd1012, 0dBFD555555555544C;\n"
"	fma.rn.f64 	%fd1013, %fd1011, %fd976, %fd1012;\n"
"	mul.f64 	%fd1014, %fd976, %fd1013;\n"
"	fma.rn.f64 	%fd1015, %fd1014, %fd1311, %fd1311;\n"
"	mov.f64 	%fd1016, 0d3FF921FB54442D18;\n"
"	sub.f64 	%fd1017, %fd1016, %fd1015;\n"
"	setp.gt.f64 	%p76, %fd61, 0d3FF0000000000000;\n"
"	selp.f64 	%fd1018, %fd1017, %fd1015, %p76;\n"
"	copysign.f64 	%fd1319, %fd1, %fd1018;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_36:\n"
"	setp.gt.s32 	%p4, %r54, 18;\n"
"	@%p4 bra 	$L__BB0_41;\n"
"\n"
"	setp.eq.s32 	%p7, %r54, 17;\n"
"	@%p7 bra 	$L__BB0_65;\n"
"\n"
"	setp.eq.s32 	%p8, %r54, 18;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p8 bra 	$L__BB0_39;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_39:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r15}, %fd1;\n"
"	}\n"
"	and.b32  	%r16, %r15, 2147483647;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r17, %temp}, %fd1;\n"
"	}\n"
"	setp.lt.u32 	%p46, %r16, 2146435072;\n"
"	@%p46 bra 	$L__BB0_64;\n"
"	bra.uni 	$L__BB0_40;\n"
"\n"
"$L__BB0_64:\n"
"	mov.b64 	%fd479, {%r17, %r16};\n"
"	add.f64 	%fd480, %fd479, 0dC010000000000000;\n"
"	mov.f64 	%fd481, 0dC010000000000000;\n"
"	add.f64 	%fd482, %fd479, 0d4010000000000000;\n"
"	rcp.approx.ftz.f64 	%fd483, %fd482;\n"
"	neg.f64 	%fd484, %fd482;\n"
"	mov.f64 	%fd485, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd486, %fd484, %fd483, %fd485;\n"
"	fma.rn.f64 	%fd487, %fd486, %fd486, %fd486;\n"
"	fma.rn.f64 	%fd488, %fd487, %fd483, %fd483;\n"
"	mul.f64 	%fd489, %fd480, %fd488;\n"
"	add.rn.f64 	%fd490, %fd489, %fd485;\n"
"	fma.rn.f64 	%fd491, %fd481, %fd490, %fd479;\n"
"	neg.f64 	%fd492, %fd489;\n"
"	fma.rn.f64 	%fd493, %fd492, %fd479, %fd491;\n"
"	fma.rn.f64 	%fd494, %fd488, %fd493, %fd489;\n"
"	mov.f64 	%fd495, 0dBE44E1C6FD03D328;\n"
"	mov.f64 	%fd496, 0dBDF8774AD4E0BFD7;\n"
"	fma.rn.f64 	%fd497, %fd496, %fd494, %fd495;\n"
"	mov.f64 	%fd498, 0dBE4330149F7A56B6;\n"
"	fma.rn.f64 	%fd499, %fd497, %fd494, %fd498;\n"
"	mov.f64 	%fd500, 0d3E7BEDDED8376273;\n"
"	fma.rn.f64 	%fd501, %fd499, %fd494, %fd500;\n"
"	mov.f64 	%fd502, 0d3E6F9254C3ABF22B;\n"
"	fma.rn.f64 	%fd503, %fd501, %fd494, %fd502;\n"
"	mov.f64 	%fd504, 0dBEAB9068C2148CF0;\n"
"	fma.rn.f64 	%fd505, %fd503, %fd494, %fd504;\n"
"	mov.f64 	%fd506, 0d3E94C6454DB34009;\n"
"	fma.rn.f64 	%fd507, %fd505, %fd494, %fd506;\n"
"	mov.f64 	%fd508, 0d3ED7F1C378F2311D;\n"
"	fma.rn.f64 	%fd509, %fd507, %fd494, %fd508;\n"
"	mov.f64 	%fd510, 0dBEE78E051C6D5C58;\n"
"	fma.rn.f64 	%fd511, %fd509, %fd494, %fd510;\n"
"	mov.f64 	%fd512, 0dBEF995B4EAD14A90;\n"
"	fma.rn.f64 	%fd513, %fd511, %fd494, %fd512;\n"
"	mov.f64 	%fd514, 0d3F23BE27CF0A29B2;\n"
"	fma.rn.f64 	%fd515, %fd513, %fd494, %fd514;\n"
"	mov.f64 	%fd516, 0dBF2A1DEF3E81672E;\n"
"	fma.rn.f64 	%fd517, %fd515, %fd494, %fd516;\n"
"	mov.f64 	%fd518, 0dBF48D4ABE68C1713;\n"
"	fma.rn.f64 	%fd519, %fd517, %fd494, %fd518;\n"
"	mov.f64 	%fd520, 0d3F749C67210DD6B4;\n"
"	fma.rn.f64 	%fd521, %fd519, %fd494, %fd520;\n"
"	mov.f64 	%fd522, 0dBF9096238568E357;\n"
"	fma.rn.f64 	%fd523, %fd521, %fd494, %fd522;\n"
"	mov.f64 	%fd524, 0d3FA3079EDF8C2DC9;\n"
"	fma.rn.f64 	%fd525, %fd523, %fd494, %fd524;\n"
"	mov.f64 	%fd526, 0dBFB0FB06DFF601FC;\n"
"	fma.rn.f64 	%fd527, %fd525, %fd494, %fd526;\n"
"	mov.f64 	%fd528, 0d3FB7FEE004DFBCDC;\n"
"	fma.rn.f64 	%fd529, %fd527, %fd494, %fd528;\n"
"	mov.f64 	%fd530, 0dBFB9DDB23C3DB8C6;\n"
"	fma.rn.f64 	%fd531, %fd529, %fd494, %fd530;\n"
"	mov.f64 	%fd532, 0d3FB16ECEFCFA5FDA;\n"
"	fma.rn.f64 	%fd533, %fd531, %fd494, %fd532;\n"
"	mov.f64 	%fd534, 0d3F8F7F5DF66FB6D6;\n"
"	fma.rn.f64 	%fd535, %fd533, %fd494, %fd534;\n"
"	mov.f64 	%fd536, 0dBFC1DF1AD154A29D;\n"
"	fma.rn.f64 	%fd537, %fd535, %fd494, %fd536;\n"
"	mov.f64 	%fd538, 0d3FF3BA5916E9FD7F;\n"
"	fma.rn.f64 	%fd539, %fd537, %fd494, %fd538;\n"
"	mov.f64 	%fd540, 0d4000000000000000;\n"
"	fma.rn.f64 	%fd541, %fd540, %fd479, %fd485;\n"
"	rcp.approx.ftz.f64 	%fd542, %fd541;\n"
"	neg.f64 	%fd543, %fd541;\n"
"	fma.rn.f64 	%fd544, %fd543, %fd542, %fd485;\n"
"	fma.rn.f64 	%fd545, %fd544, %fd544, %fd544;\n"
"	fma.rn.f64 	%fd546, %fd545, %fd542, %fd542;\n"
"	mul.f64 	%fd547, %fd539, %fd546;\n"
"	mul.f64 	%fd548, %fd547, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd549, %fd479, %fd548, %fd539;\n"
"	neg.f64 	%fd550, %fd547;\n"
"	add.rn.f64 	%fd551, %fd549, %fd550;\n"
"	fma.rn.f64 	%fd552, %fd551, %fd546, %fd547;\n"
"	mul.f64 	%fd553, %fd479, %fd479;\n"
"	neg.f64 	%fd554, %fd553;\n"
"	mov.f64 	%fd555, 0d4338000000000000;\n"
"	mov.f64 	%fd556, 0d3FF71547652B82FE;\n"
"	fma.rn.f64 	%fd557, %fd554, %fd556, %fd555;\n"
"	mov.f64 	%fd558, 0dC338000000000000;\n"
"	add.rn.f64 	%fd559, %fd557, %fd558;\n"
"	mov.f64 	%fd560, 0dBFE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd561, %fd559, %fd560, %fd554;\n"
"	mov.f64 	%fd562, 0dBC7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd563, %fd559, %fd562, %fd561;\n"
"	mov.f64 	%fd564, 0d3E928AF3FCA213EA;\n"
"	mov.f64 	%fd565, 0d3E5ADE1569CE2BDF;\n"
"	fma.rn.f64 	%fd566, %fd565, %fd563, %fd564;\n"
"	mov.f64 	%fd567, 0d3EC71DEE62401315;\n"
"	fma.rn.f64 	%fd568, %fd566, %fd563, %fd567;\n"
"	mov.f64 	%fd569, 0d3EFA01997C89EB71;\n"
"	fma.rn.f64 	%fd570, %fd568, %fd563, %fd569;\n"
"	mov.f64 	%fd571, 0d3F2A01A014761F65;\n"
"	fma.rn.f64 	%fd572, %fd570, %fd563, %fd571;\n"
"	mov.f64 	%fd573, 0d3F56C16C1852B7AF;\n"
"	fma.rn.f64 	%fd574, %fd572, %fd563, %fd573;\n"
"	mov.f64 	%fd575, 0d3F81111111122322;\n"
"	fma.rn.f64 	%fd576, %fd574, %fd563, %fd575;\n"
"	mov.f64 	%fd577, 0d3FA55555555502A1;\n"
"	fma.rn.f64 	%fd578, %fd576, %fd563, %fd577;\n"
"	mov.f64 	%fd579, 0d3FC5555555555511;\n"
"	fma.rn.f64 	%fd580, %fd578, %fd563, %fd579;\n"
"	mov.f64 	%fd581, 0d3FE000000000000B;\n"
"	fma.rn.f64 	%fd582, %fd580, %fd563, %fd581;\n"
"	fma.rn.f64 	%fd583, %fd582, %fd563, %fd485;\n"
"	fma.rn.f64 	%fd584, %fd583, %fd563, %fd485;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r101, %temp}, %fd557;\n"
"	}\n"
"	shr.u32 	%r102, %r101, 31;\n"
"	add.s32 	%r103, %r101, %r102;\n"
"	shr.s32 	%r104, %r103, 1;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r105, %temp}, %fd584;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r106}, %fd584;\n"
"	}\n"
"	shl.b32 	%r107, %r104, 20;\n"
"	add.s32 	%r108, %r106, %r107;\n"
"	mov.b64 	%fd585, {%r105, %r108};\n"
"	sub.s32 	%r109, %r101, %r104;\n"
"	shl.b32 	%r110, %r109, 20;\n"
"	add.s32 	%r111, %r110, 1072693248;\n"
"	mov.u32 	%r112, 0;\n"
"	mov.b64 	%fd586, {%r112, %r111};\n"
"	mul.f64 	%fd587, %fd585, %fd586;\n"
"	neg.f64 	%fd588, %fd479;\n"
"	fma.rn.f64 	%fd589, %fd588, %fd479, %fd553;\n"
"	fma.rn.f64 	%fd590, %fd587, %fd589, %fd587;\n"
"	mul.f64 	%fd591, %fd552, %fd590;\n"
"	setp.gt.u32 	%p51, %r16, 1077624832;\n"
"	selp.f64 	%fd592, 0d0000000000000000, %fd591, %p51;\n"
"	sub.f64 	%fd593, %fd540, %fd592;\n"
"	setp.lt.s32 	%p52, %r15, 0;\n"
"	selp.f64 	%fd1319, %fd593, %fd592, %p52;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_9:\n"
"	setp.eq.s32 	%p21, %r54, 7;\n"
"	@%p21 bra 	$L__BB0_96;\n"
"\n"
"	setp.eq.s32 	%p22, %r54, 8;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p22 bra 	$L__BB0_11;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_11:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r186}, %fd1;\n"
"	}\n"
"	mov.b32 	%f15, %r186;\n"
"	abs.f32 	%f16, %f15;\n"
"	setp.lt.f32 	%p82, %f16, 0f3FE26666;\n"
"	@%p82 bra 	$L__BB0_95;\n"
"	bra.uni 	$L__BB0_12;\n"
"\n"
"$L__BB0_95:\n"
"	mul.f64 	%fd1150, %fd1, %fd1;\n"
"	mov.f64 	%fd1151, 0dBFB3823B180754AF;\n"
"	mov.f64 	%fd1152, 0d3FB0066BDC1895E9;\n"
"	fma.rn.f64 	%fd1153, %fd1152, %fd1150, %fd1151;\n"
"	mov.f64 	%fd1154, 0d3FB11E52CC2F79AE;\n"
"	fma.rn.f64 	%fd1155, %fd1153, %fd1150, %fd1154;\n"
"	mov.f64 	%fd1156, 0dBF924EAF3526861B;\n"
"	fma.rn.f64 	%fd1157, %fd1155, %fd1150, %fd1156;\n"
"	mov.f64 	%fd1158, 0d3F91DF02A31E6CB7;\n"
"	fma.rn.f64 	%fd1159, %fd1157, %fd1150, %fd1158;\n"
"	mov.f64 	%fd1160, 0d3F847D18B0EEC6CC;\n"
"	fma.rn.f64 	%fd1161, %fd1159, %fd1150, %fd1160;\n"
"	mov.f64 	%fd1162, 0d3F8D0AF961BA53B0;\n"
"	fma.rn.f64 	%fd1163, %fd1161, %fd1150, %fd1162;\n"
"	mov.f64 	%fd1164, 0d3F91BF7734CF1C48;\n"
"	fma.rn.f64 	%fd1165, %fd1163, %fd1150, %fd1164;\n"
"	mov.f64 	%fd1166, 0d3F96E91483144EF7;\n"
"	fma.rn.f64 	%fd1167, %fd1165, %fd1150, %fd1166;\n"
"	mov.f64 	%fd1168, 0d3F9F1C6E0A4F9F81;\n"
"	fma.rn.f64 	%fd1169, %fd1167, %fd1150, %fd1168;\n"
"	mov.f64 	%fd1170, 0d3FA6DB6DC27FA92B;\n"
"	fma.rn.f64 	%fd1171, %fd1169, %fd1150, %fd1170;\n"
"	mov.f64 	%fd1172, 0d3FB333333320F91B;\n"
"	fma.rn.f64 	%fd1173, %fd1171, %fd1150, %fd1172;\n"
"	mov.f64 	%fd1174, 0d3FC5555555555F4D;\n"
"	fma.rn.f64 	%fd1175, %fd1173, %fd1150, %fd1174;\n"
"	mul.f64 	%fd1176, %fd1150, %fd1175;\n"
"	fma.rn.f64 	%fd1319, %fd1176, %fd1, %fd1;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_30:\n"
"	setp.eq.s32 	%p10, %r54, 15;\n"
"	@%p10 bra 	$L__BB0_70;\n"
"\n"
"	setp.eq.s32 	%p11, %r54, 16;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p11 bra 	$L__BB0_32;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_32:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r210}, %fd1;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r211, %temp}, %fd1;\n"
"	}\n"
"	setp.gt.s32 	%p54, %r210, 1048575;\n"
"	mov.u32 	%r212, -1023;\n"
"	@%p54 bra 	$L__BB0_34;\n"
"\n"
"	mul.f64 	%fd1, %fd1, 0d4350000000000000;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r210}, %fd1;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r211, %temp}, %fd1;\n"
"	}\n"
"	mov.u32 	%r212, -1077;\n"
"\n"
"$L__BB0_34:\n"
"	add.s32 	%r115, %r210, -1;\n"
"	setp.lt.u32 	%p55, %r115, 2146435071;\n"
"	@%p55 bra 	$L__BB0_66;\n"
"	bra.uni 	$L__BB0_35;\n"
"\n"
"$L__BB0_66:\n"
"	shr.u32 	%r117, %r210, 20;\n"
"	add.s32 	%r213, %r212, %r117;\n"
"	and.b32  	%r118, %r210, -2146435073;\n"
"	or.b32  	%r119, %r118, 1072693248;\n"
"	mov.b64 	%fd1307, {%r211, %r119};\n"
"	setp.lt.s32 	%p57, %r119, 1073127583;\n"
"	@%p57 bra 	$L__BB0_68;\n"
"\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r120, %temp}, %fd1307;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r121}, %fd1307;\n"
"	}\n"
"	add.s32 	%r122, %r121, -1048576;\n"
"	mov.b64 	%fd1307, {%r120, %r122};\n"
"	add.s32 	%r213, %r213, 1;\n"
"\n"
"$L__BB0_68:\n"
"	add.f64 	%fd681, %fd1307, 0d3FF0000000000000;\n"
"	mov.f64 	%fd682, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd683, %fd681;\n"
"	neg.f64 	%fd684, %fd681;\n"
"	fma.rn.f64 	%fd685, %fd684, %fd683, %fd682;\n"
"	fma.rn.f64 	%fd686, %fd685, %fd685, %fd685;\n"
"	fma.rn.f64 	%fd687, %fd686, %fd683, %fd683;\n"
"	add.f64 	%fd688, %fd1307, 0dBFF0000000000000;\n"
"	mul.f64 	%fd689, %fd688, %fd687;\n"
"	fma.rn.f64 	%fd690, %fd688, %fd687, %fd689;\n"
"	mul.f64 	%fd691, %fd690, %fd690;\n"
"	mov.f64 	%fd692, 0d3ED0EE258B7A8B04;\n"
"	mov.f64 	%fd693, 0d3EB1380B3AE80F1E;\n"
"	fma.rn.f64 	%fd694, %fd693, %fd691, %fd692;\n"
"	mov.f64 	%fd695, 0d3EF3B2669F02676F;\n"
"	fma.rn.f64 	%fd696, %fd694, %fd691, %fd695;\n"
"	mov.f64 	%fd697, 0d3F1745CBA9AB0956;\n"
"	fma.rn.f64 	%fd698, %fd696, %fd691, %fd697;\n"
"	mov.f64 	%fd699, 0d3F3C71C72D1B5154;\n"
"	fma.rn.f64 	%fd700, %fd698, %fd691, %fd699;\n"
"	mov.f64 	%fd701, 0d3F624924923BE72D;\n"
"	fma.rn.f64 	%fd702, %fd700, %fd691, %fd701;\n"
"	mov.f64 	%fd703, 0d3F8999999999A3C4;\n"
"	fma.rn.f64 	%fd704, %fd702, %fd691, %fd703;\n"
"	mov.f64 	%fd705, 0d3FB5555555555554;\n"
"	fma.rn.f64 	%fd706, %fd704, %fd691, %fd705;\n"
"	sub.f64 	%fd707, %fd688, %fd690;\n"
"	add.f64 	%fd708, %fd707, %fd707;\n"
"	neg.f64 	%fd709, %fd690;\n"
"	fma.rn.f64 	%fd710, %fd709, %fd688, %fd708;\n"
"	mul.f64 	%fd711, %fd687, %fd710;\n"
"	mul.f64 	%fd712, %fd691, %fd706;\n"
"	fma.rn.f64 	%fd713, %fd712, %fd690, %fd711;\n"
"	xor.b32  	%r123, %r213, -2147483648;\n"
"	mov.u32 	%r124, -2147483648;\n"
"	mov.u32 	%r125, 1127219200;\n"
"	mov.b64 	%fd714, {%r123, %r125};\n"
"	mov.b64 	%fd715, {%r124, %r125};\n"
"	sub.f64 	%fd716, %fd714, %fd715;\n"
"	mov.f64 	%fd717, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd718, %fd716, %fd717, %fd690;\n"
"	neg.f64 	%fd719, %fd716;\n"
"	fma.rn.f64 	%fd720, %fd719, %fd717, %fd718;\n"
"	sub.f64 	%fd721, %fd720, %fd690;\n"
"	sub.f64 	%fd722, %fd713, %fd721;\n"
"	mov.f64 	%fd723, 0d3C7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd724, %fd716, %fd723, %fd722;\n"
"	add.f64 	%fd1308, %fd718, %fd724;\n"
"	bra.uni 	$L__BB0_69;\n"
"\n"
"$L__BB0_19:\n"
"	setp.eq.s32 	%p16, %r54, 11;\n"
"	@%p16 bra 	$L__BB0_81;\n"
"\n"
"	setp.eq.s32 	%p17, %r54, 12;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p17 bra 	$L__BB0_21;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_21:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r153}, %fd1;\n"
"	}\n"
"	and.b32  	%r154, %r153, 2147483647;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r155, %temp}, %fd1;\n"
"	}\n"
"	mov.b64 	%fd51, {%r155, %r154};\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r156}, %fd51;\n"
"	}\n"
"	setp.lt.u32 	%p67, %r156, 1082536911;\n"
"	@%p67 bra 	$L__BB0_80;\n"
"	bra.uni 	$L__BB0_22;\n"
"\n"
"$L__BB0_80:\n"
"	mov.f64 	%fd872, 0d4338000000000000;\n"
"	mov.f64 	%fd873, 0d3FF71547652B82FE;\n"
"	fma.rn.f64 	%fd874, %fd51, %fd873, %fd872;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r157, %temp}, %fd874;\n"
"	}\n"
"	mov.f64 	%fd875, 0dC338000000000000;\n"
"	add.rn.f64 	%fd876, %fd874, %fd875;\n"
"	mov.f64 	%fd877, 0dBFE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd878, %fd876, %fd877, %fd51;\n"
"	mov.f64 	%fd879, 0dBC7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd880, %fd876, %fd879, %fd878;\n"
"	mov.f64 	%fd881, 0d3E928AF3FCA213EA;\n"
"	mov.f64 	%fd882, 0d3E5ADE1569CE2BDF;\n"
"	fma.rn.f64 	%fd883, %fd882, %fd880, %fd881;\n"
"	mov.f64 	%fd884, 0d3EC71DEE62401315;\n"
"	fma.rn.f64 	%fd885, %fd883, %fd880, %fd884;\n"
"	mov.f64 	%fd886, 0d3EFA01997C89EB71;\n"
"	fma.rn.f64 	%fd887, %fd885, %fd880, %fd886;\n"
"	mov.f64 	%fd888, 0d3F2A01A014761F65;\n"
"	fma.rn.f64 	%fd889, %fd887, %fd880, %fd888;\n"
"	mov.f64 	%fd890, 0d3F56C16C1852B7AF;\n"
"	fma.rn.f64 	%fd891, %fd889, %fd880, %fd890;\n"
"	mov.f64 	%fd892, 0d3F81111111122322;\n"
"	fma.rn.f64 	%fd893, %fd891, %fd880, %fd892;\n"
"	mov.f64 	%fd894, 0d3FA55555555502A1;\n"
"	fma.rn.f64 	%fd895, %fd893, %fd880, %fd894;\n"
"	mov.f64 	%fd896, 0d3FC5555555555511;\n"
"	fma.rn.f64 	%fd897, %fd895, %fd880, %fd896;\n"
"	mov.f64 	%fd898, 0d3FE000000000000B;\n"
"	fma.rn.f64 	%fd899, %fd897, %fd880, %fd898;\n"
"	mov.f64 	%fd900, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd901, %fd899, %fd880, %fd900;\n"
"	fma.rn.f64 	%fd902, %fd901, %fd880, %fd900;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r158, %temp}, %fd902;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r159}, %fd902;\n"
"	}\n"
"	shl.b32 	%r160, %r157, 20;\n"
"	add.s32 	%r161, %r160, %r159;\n"
"	add.s32 	%r162, %r161, -2097152;\n"
"	mov.b64 	%fd903, {%r158, %r162};\n"
"	rcp.approx.ftz.f64 	%fd904, %fd903;\n"
"	neg.f64 	%fd905, %fd903;\n"
"	fma.rn.f64 	%fd906, %fd905, %fd904, %fd900;\n"
"	fma.rn.f64 	%fd907, %fd906, %fd906, %fd906;\n"
"	fma.rn.f64 	%fd908, %fd907, %fd904, %fd904;\n"
"	mov.f64 	%fd909, 0d3FB0000000000000;\n"
"	fma.rn.f64 	%fd53, %fd908, %fd909, %fd903;\n"
"	add.f64 	%fd1319, %fd53, %fd53;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_41:\n"
"	setp.eq.s32 	%p5, %r54, 19;\n"
"	@%p5 bra 	$L__BB0_61;\n"
"\n"
"	setp.ne.s32 	%p6, %r54, 20;\n"
"	mov.f64 	%fd1319, %fd1;\n"
"	@%p6 bra 	$L__BB0_115;\n"
"\n"
"	abs.f64 	%fd109, %fd1;\n"
"	setp.geu.f64 	%p25, %fd109, 0d7FF0000000000000;\n"
"	mov.f64 	%fd1319, 0d0000000000000000;\n"
"	@%p25 bra 	$L__BB0_60;\n"
"	bra.uni 	$L__BB0_44;\n"
"\n"
"$L__BB0_60:\n"
"	atom.global.max.u32 	%r88, [%rd1], 2;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_107:\n"
"	abs.f64 	%fd97, %fd1;\n"
"	setp.eq.f64 	%p95, %fd97, 0d7FF0000000000000;\n"
"	@%p95 bra 	$L__BB0_110;\n"
"	bra.uni 	$L__BB0_108;\n"
"\n"
"$L__BB0_110:\n"
"	mov.f64 	%fd1266, 0d0000000000000000;\n"
"	mul.rn.f64 	%fd1317, %fd1, %fd1266;\n"
"	mov.u32 	%r220, 0;\n"
"	bra.uni 	$L__BB0_111;\n"
"\n"
"$L__BB0_77:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r151}, %fd1;\n"
"	}\n"
"	and.b32  	%r41, %r151, 2147483647;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r152, %temp}, %fd1;\n"
"	}\n"
"	mov.b64 	%fd48, {%r152, %r41};\n"
"	setp.ltu.f64 	%p65, %fd48, 0d3FE4F92224DD2F1A;\n"
"	@%p65 bra 	$L__BB0_79;\n"
"	bra.uni 	$L__BB0_78;\n"
"\n"
"$L__BB0_79:\n"
"	mul.f64 	%fd848, %fd1, %fd1;\n"
"	mov.f64 	%fd849, 0d3F14359F420AFC3D;\n"
"	mov.f64 	%fd850, 0dBEF0BC46E2F5E964;\n"
"	fma.rn.f64 	%fd851, %fd850, %fd848, %fd849;\n"
"	mov.f64 	%fd852, 0dBF2DF9F0728C5D84;\n"
"	fma.rn.f64 	%fd853, %fd851, %fd848, %fd852;\n"
"	mov.f64 	%fd854, 0d3F4337D1CEC4F033;\n"
"	fma.rn.f64 	%fd855, %fd853, %fd848, %fd854;\n"
"	mov.f64 	%fd856, 0dBF57D6E9674335B3;\n"
"	fma.rn.f64 	%fd857, %fd855, %fd848, %fd856;\n"
"	mov.f64 	%fd858, 0d3F6D6D000D7AAD3D;\n"
"	fma.rn.f64 	%fd859, %fd857, %fd848, %fd858;\n"
"	mov.f64 	%fd860, 0dBF8226E1F3CF1EF5;\n"
"	fma.rn.f64 	%fd861, %fd859, %fd848, %fd860;\n"
"	mov.f64 	%fd862, 0d3F9664F47EC0C8CF;\n"
"	fma.rn.f64 	%fd863, %fd861, %fd848, %fd862;\n"
"	mov.f64 	%fd864, 0dBFABA1BA1B80AB40;\n"
"	fma.rn.f64 	%fd865, %fd863, %fd848, %fd864;\n"
"	mov.f64 	%fd866, 0d3FC111111110FA4A;\n"
"	fma.rn.f64 	%fd867, %fd865, %fd848, %fd866;\n"
"	mov.f64 	%fd868, 0dBFD5555555555550;\n"
"	fma.rn.f64 	%fd869, %fd867, %fd848, %fd868;\n"
"	mov.f64 	%fd870, 0d0000000000000000;\n"
"	fma.rn.f64 	%fd871, %fd869, %fd848, %fd870;\n"
"	fma.rn.f64 	%fd1319, %fd871, %fd1, %fd1;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_84:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r42}, %fd1;\n"
"	}\n"
"	abs.f64 	%fd65, %fd1;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r177}, %fd65;\n"
"	}\n"
"	setp.lt.s32 	%p77, %r177, 1071801958;\n"
"	@%p77 bra 	$L__BB0_92;\n"
"	bra.uni 	$L__BB0_85;\n"
"\n"
"$L__BB0_92:\n"
"	mul.f64 	%fd1066, %fd65, %fd65;\n"
"	mov.f64 	%fd1067, 0dBFB3823B180754AF;\n"
"	mov.f64 	%fd1068, 0d3FB0066BDC1895E9;\n"
"	fma.rn.f64 	%fd1069, %fd1068, %fd1066, %fd1067;\n"
"	mov.f64 	%fd1070, 0d3FB11E52CC2F79AE;\n"
"	fma.rn.f64 	%fd1071, %fd1069, %fd1066, %fd1070;\n"
"	mov.f64 	%fd1072, 0dBF924EAF3526861B;\n"
"	fma.rn.f64 	%fd1073, %fd1071, %fd1066, %fd1072;\n"
"	mov.f64 	%fd1074, 0d3F91DF02A31E6CB7;\n"
"	fma.rn.f64 	%fd1075, %fd1073, %fd1066, %fd1074;\n"
"	mov.f64 	%fd1076, 0d3F847D18B0EEC6CC;\n"
"	fma.rn.f64 	%fd1077, %fd1075, %fd1066, %fd1076;\n"
"	mov.f64 	%fd1078, 0d3F8D0AF961BA53B0;\n"
"	fma.rn.f64 	%fd1079, %fd1077, %fd1066, %fd1078;\n"
"	mov.f64 	%fd1080, 0d3F91BF7734CF1C48;\n"
"	fma.rn.f64 	%fd1081, %fd1079, %fd1066, %fd1080;\n"
"	mov.f64 	%fd1082, 0d3F96E91483144EF7;\n"
"	fma.rn.f64 	%fd1083, %fd1081, %fd1066, %fd1082;\n"
"	mov.f64 	%fd1084, 0d3F9F1C6E0A4F9F81;\n"
"	fma.rn.f64 	%fd1085, %fd1083, %fd1066, %fd1084;\n"
"	mov.f64 	%fd1086, 0d3FA6DB6DC27FA92B;\n"
"	fma.rn.f64 	%fd1087, %fd1085, %fd1066, %fd1086;\n"
"	mov.f64 	%fd1088, 0d3FB333333320F91B;\n"
"	fma.rn.f64 	%fd1089, %fd1087, %fd1066, %fd1088;\n"
"	mov.f64 	%fd1090, 0d3FC5555555555F4D;\n"
"	fma.rn.f64 	%fd1091, %fd1089, %fd1066, %fd1090;\n"
"	mul.f64 	%fd1092, %fd1066, %fd1091;\n"
"	fma.rn.f64 	%fd73, %fd1092, %fd65, %fd65;\n"
"	setp.lt.s32 	%p81, %r42, 0;\n"
"	@%p81 bra 	$L__BB0_94;\n"
"\n"
"	mov.f64 	%fd1093, 0dBC91A62633145C07;\n"
"	add.rn.f64 	%fd1094, %fd73, %fd1093;\n"
"	neg.f64 	%fd1095, %fd1094;\n"
"	mov.f64 	%fd1096, 0d3FF921FB54442D18;\n"
"	add.rn.f64 	%fd1319, %fd1096, %fd1095;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_65:\n"
"	abs.f64 	%fd594, %fd1;\n"
"	mov.f64 	%fd595, 0d3D47088FDB46FA5F;\n"
"	mov.f64 	%fd596, 0dBCF0679AFBA6F279;\n"
"	fma.rn.f64 	%fd597, %fd596, %fd594, %fd595;\n"
"	mov.f64 	%fd598, 0dBD8DF9F9B976A9B2;\n"
"	fma.rn.f64 	%fd599, %fd597, %fd594, %fd598;\n"
"	mov.f64 	%fd600, 0d3DC7F1F5590CC332;\n"
"	fma.rn.f64 	%fd601, %fd599, %fd594, %fd600;\n"
"	mov.f64 	%fd602, 0dBDFA28A3CD2D56C4;\n"
"	fma.rn.f64 	%fd603, %fd601, %fd594, %fd602;\n"
"	mov.f64 	%fd604, 0d3E2485EE67835925;\n"
"	fma.rn.f64 	%fd605, %fd603, %fd594, %fd604;\n"
"	mov.f64 	%fd606, 0dBE476DB45919F583;\n"
"	fma.rn.f64 	%fd607, %fd605, %fd594, %fd606;\n"
"	mov.f64 	%fd608, 0d3E62D698D98C8D71;\n"
"	fma.rn.f64 	%fd609, %fd607, %fd594, %fd608;\n"
"	mov.f64 	%fd610, 0dBE720A2C7155D5C6;\n"
"	fma.rn.f64 	%fd611, %fd609, %fd594, %fd610;\n"
"	mov.f64 	%fd612, 0dBE41D29B37CA1397;\n"
"	fma.rn.f64 	%fd613, %fd611, %fd594, %fd612;\n"
"	mov.f64 	%fd614, 0d3EA2EF6CC0F67A49;\n"
"	fma.rn.f64 	%fd615, %fd613, %fd594, %fd614;\n"
"	mov.f64 	%fd616, 0dBEC102B892333B6F;\n"
"	fma.rn.f64 	%fd617, %fd615, %fd594, %fd616;\n"
"	mov.f64 	%fd618, 0d3ECA30375BA9A84E;\n"
"	fma.rn.f64 	%fd619, %fd617, %fd594, %fd618;\n"
"	mov.f64 	%fd620, 0d3ECAAD18DEDEA43E;\n"
"	fma.rn.f64 	%fd621, %fd619, %fd594, %fd620;\n"
"	mov.f64 	%fd622, 0dBEFF05355BC5B225;\n"
"	fma.rn.f64 	%fd623, %fd621, %fd594, %fd622;\n"
"	mov.f64 	%fd624, 0d3F10E37A3108BC8B;\n"
"	fma.rn.f64 	%fd625, %fd623, %fd594, %fd624;\n"
"	mov.f64 	%fd626, 0d3EFB292D828E5CB2;\n"
"	fma.rn.f64 	%fd627, %fd625, %fd594, %fd626;\n"
"	mov.f64 	%fd628, 0dBF4356626EBF9BFA;\n"
"	fma.rn.f64 	%fd629, %fd627, %fd594, %fd628;\n"
"	mov.f64 	%fd630, 0d3F5BCA68F73D6AFC;\n"
"	fma.rn.f64 	%fd631, %fd629, %fd594, %fd630;\n"
"	mov.f64 	%fd632, 0dBF2B6B69EBBC280B;\n"
"	fma.rn.f64 	%fd633, %fd631, %fd594, %fd632;\n"
"	mov.f64 	%fd634, 0dBF9396685912A453;\n"
"	fma.rn.f64 	%fd635, %fd633, %fd594, %fd634;\n"
"	mov.f64 	%fd636, 0d3FBA4F4E2A1ABEF8;\n"
"	fma.rn.f64 	%fd637, %fd635, %fd594, %fd636;\n"
"	mov.f64 	%fd638, 0d3FE45F306DC9C8BB;\n"
"	fma.rn.f64 	%fd639, %fd637, %fd594, %fd638;\n"
"	mov.f64 	%fd640, 0d3FC06EBA8214DB69;\n"
"	fma.rn.f64 	%fd641, %fd639, %fd594, %fd640;\n"
"	fma.rn.f64 	%fd642, %fd641, %fd594, %fd594;\n"
"	sub.f64 	%fd643, %fd594, %fd642;\n"
"	fma.rn.f64 	%fd644, %fd641, %fd594, %fd643;\n"
"	neg.f64 	%fd645, %fd642;\n"
"	neg.f64 	%fd646, %fd644;\n"
"	cvt.rn.f32.f64 	%f4, %fd642;\n"
"	mul.f32 	%f5, %f4, 0fBFB8AA3B;\n"
"	cvt.rni.f32.f32 	%f6, %f5;\n"
"	cvt.f64.f32 	%fd647, %f6;\n"
"	neg.f64 	%fd648, %fd647;\n"
"	mov.f64 	%fd649, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd650, %fd648, %fd649, %fd645;\n"
"	mov.f64 	%fd651, 0d3E928A27F89B6999;\n"
"	mov.f64 	%fd652, 0d3E5AE904A4741B81;\n"
"	fma.rn.f64 	%fd653, %fd652, %fd650, %fd651;\n"
"	mov.f64 	%fd654, 0d3EC71DE715FF7E07;\n"
"	fma.rn.f64 	%fd655, %fd653, %fd650, %fd654;\n"
"	mov.f64 	%fd656, 0d3EFA019A6B0AC45A;\n"
"	fma.rn.f64 	%fd657, %fd655, %fd650, %fd656;\n"
"	mov.f64 	%fd658, 0d3F2A01A017EED94F;\n"
"	fma.rn.f64 	%fd659, %fd657, %fd650, %fd658;\n"
"	mov.f64 	%fd660, 0d3F56C16C17F2A71B;\n"
"	fma.rn.f64 	%fd661, %fd659, %fd650, %fd660;\n"
"	mov.f64 	%fd662, 0d3F811111111173C4;\n"
"	fma.rn.f64 	%fd663, %fd661, %fd650, %fd662;\n"
"	mov.f64 	%fd664, 0d3FA555555555211A;\n"
"	fma.rn.f64 	%fd665, %fd663, %fd650, %fd664;\n"
"	mov.f64 	%fd666, 0d3FC5555555555540;\n"
"	fma.rn.f64 	%fd667, %fd665, %fd650, %fd666;\n"
"	mov.f64 	%fd668, 0d3FE0000000000005;\n"
"	fma.rn.f64 	%fd669, %fd667, %fd650, %fd668;\n"
"	mul.f64 	%fd670, %fd650, %fd669;\n"
"	fma.rn.f64 	%fd671, %fd670, %fd650, %fd646;\n"
"	add.f64 	%fd672, %fd650, %fd671;\n"
"	ex2.approx.ftz.f32 	%f7, %f6;\n"
"	cvt.f64.f32 	%fd673, %f7;\n"
"	mov.f64 	%fd674, 0d3FF0000000000000;\n"
"	sub.f64 	%fd675, %fd674, %fd673;\n"
"	neg.f64 	%fd676, %fd672;\n"
"	fma.rn.f64 	%fd677, %fd676, %fd673, %fd675;\n"
"	setp.ge.f64 	%p53, %fd594, 0d4017AFB48DC96626;\n"
"	selp.f64 	%fd678, 0d3FF0000000000000, %fd677, %p53;\n"
"	copysign.f64 	%fd1319, %fd1, %fd678;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_96:\n"
"	abs.f64 	%fd78, %fd1;\n"
"	setp.eq.f64 	%p85, %fd78, 0d7FF0000000000000;\n"
"	@%p85 bra 	$L__BB0_99;\n"
"	bra.uni 	$L__BB0_97;\n"
"\n"
"$L__BB0_99:\n"
"	mov.f64 	%fd1185, 0d0000000000000000;\n"
"	mul.rn.f64 	%fd1314, %fd1, %fd1185;\n"
"	mov.u32 	%r218, 0;\n"
"	bra.uni 	$L__BB0_100;\n"
"\n"
"$L__BB0_70:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r214}, %fd1;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r215, %temp}, %fd1;\n"
"	}\n"
"	setp.gt.s32 	%p58, %r214, 1048575;\n"
"	mov.u32 	%r216, -1023;\n"
"	@%p58 bra 	$L__BB0_72;\n"
"\n"
"	mul.f64 	%fd1, %fd1, 0d4350000000000000;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r214}, %fd1;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r215, %temp}, %fd1;\n"
"	}\n"
"	mov.u32 	%r216, -1077;\n"
"\n"
"$L__BB0_72:\n"
"	add.s32 	%r128, %r214, -1;\n"
"	setp.lt.u32 	%p59, %r128, 2146435071;\n"
"	@%p59 bra 	$L__BB0_74;\n"
"	bra.uni 	$L__BB0_73;\n"
"\n"
"$L__BB0_74:\n"
"	shr.u32 	%r130, %r214, 20;\n"
"	add.s32 	%r217, %r216, %r130;\n"
"	and.b32  	%r131, %r214, -2146435073;\n"
"	or.b32  	%r132, %r131, 1072693248;\n"
"	mov.b64 	%fd1310, {%r215, %r132};\n"
"	setp.lt.s32 	%p61, %r132, 1073127583;\n"
"	@%p61 bra 	$L__BB0_76;\n"
"\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r133, %temp}, %fd1310;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r134}, %fd1310;\n"
"	}\n"
"	add.s32 	%r135, %r134, -1048576;\n"
"	mov.b64 	%fd1310, {%r133, %r135};\n"
"	add.s32 	%r217, %r217, 1;\n"
"\n"
"$L__BB0_76:\n"
"	add.f64 	%fd729, %fd1310, 0d3FF0000000000000;\n"
"	mov.f64 	%fd730, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd731, %fd729;\n"
"	neg.f64 	%fd732, %fd729;\n"
"	fma.rn.f64 	%fd733, %fd732, %fd731, %fd730;\n"
"	fma.rn.f64 	%fd734, %fd733, %fd733, %fd733;\n"
"	fma.rn.f64 	%fd735, %fd734, %fd731, %fd731;\n"
"	add.f64 	%fd736, %fd1310, 0dBFF0000000000000;\n"
"	mul.f64 	%fd737, %fd736, %fd735;\n"
"	fma.rn.f64 	%fd738, %fd736, %fd735, %fd737;\n"
"	mul.f64 	%fd739, %fd738, %fd738;\n"
"	mov.f64 	%fd740, 0d3ED0EE258B7A8B04;\n"
"	mov.f64 	%fd741, 0d3EB1380B3AE80F1E;\n"
"	fma.rn.f64 	%fd742, %fd741, %fd739, %fd740;\n"
"	mov.f64 	%fd743, 0d3EF3B2669F02676F;\n"
"	fma.rn.f64 	%fd744, %fd742, %fd739, %fd743;\n"
"	mov.f64 	%fd745, 0d3F1745CBA9AB0956;\n"
"	fma.rn.f64 	%fd746, %fd744, %fd739, %fd745;\n"
"	mov.f64 	%fd747, 0d3F3C71C72D1B5154;\n"
"	fma.rn.f64 	%fd748, %fd746, %fd739, %fd747;\n"
"	mov.f64 	%fd749, 0d3F624924923BE72D;\n"
"	fma.rn.f64 	%fd750, %fd748, %fd739, %fd749;\n"
"	mov.f64 	%fd751, 0d3F8999999999A3C4;\n"
"	fma.rn.f64 	%fd752, %fd750, %fd739, %fd751;\n"
"	mov.f64 	%fd753, 0d3FB5555555555554;\n"
"	fma.rn.f64 	%fd754, %fd752, %fd739, %fd753;\n"
"	sub.f64 	%fd755, %fd736, %fd738;\n"
"	add.f64 	%fd756, %fd755, %fd755;\n"
"	neg.f64 	%fd757, %fd738;\n"
"	fma.rn.f64 	%fd758, %fd757, %fd736, %fd756;\n"
"	mul.f64 	%fd759, %fd735, %fd758;\n"
"	mul.f64 	%fd760, %fd739, %fd754;\n"
"	fma.rn.f64 	%fd761, %fd760, %fd738, %fd759;\n"
"	xor.b32  	%r136, %r217, -2147483648;\n"
"	mov.u32 	%r137, -2147483648;\n"
"	mov.u32 	%r138, 1127219200;\n"
"	mov.b64 	%fd762, {%r136, %r138};\n"
"	mov.b64 	%fd763, {%r137, %r138};\n"
"	sub.f64 	%fd764, %fd762, %fd763;\n"
"	mov.f64 	%fd765, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd766, %fd764, %fd765, %fd738;\n"
"	neg.f64 	%fd767, %fd764;\n"
"	fma.rn.f64 	%fd768, %fd767, %fd765, %fd766;\n"
"	sub.f64 	%fd769, %fd768, %fd738;\n"
"	sub.f64 	%fd770, %fd761, %fd769;\n"
"	mov.f64 	%fd771, 0d3C7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd772, %fd764, %fd771, %fd770;\n"
"	add.f64 	%fd1319, %fd766, %fd772;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_81:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r163}, %fd1;\n"
"	}\n"
"	and.b32  	%r164, %r163, 2147483647;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r165, %temp}, %fd1;\n"
"	}\n"
"	mov.b64 	%fd56, {%r165, %r164};\n"
"	setp.lt.u32 	%p69, %r164, 1072693248;\n"
"	@%p69 bra 	$L__BB0_83;\n"
"	bra.uni 	$L__BB0_82;\n"
"\n"
"$L__BB0_83:\n"
"	mul.f64 	%fd955, %fd56, %fd56;\n"
"	mov.f64 	%fd956, 0d3DE611A561D87DEF;\n"
"	mov.f64 	%fd957, 0d3D6B4C75AB274C53;\n"
"	fma.rn.f64 	%fd958, %fd957, %fd955, %fd956;\n"
"	mov.f64 	%fd959, 0d3E5AE64671B18F5C;\n"
"	fma.rn.f64 	%fd960, %fd958, %fd955, %fd959;\n"
"	mov.f64 	%fd961, 0d3EC71DE3A465B1E4;\n"
"	fma.rn.f64 	%fd962, %fd960, %fd955, %fd961;\n"
"	mov.f64 	%fd963, 0d3F2A01A01A02899D;\n"
"	fma.rn.f64 	%fd964, %fd962, %fd955, %fd963;\n"
"	mov.f64 	%fd965, 0d3F811111111110A6;\n"
"	fma.rn.f64 	%fd966, %fd964, %fd955, %fd965;\n"
"	mov.f64 	%fd967, 0d3FC5555555555556;\n"
"	fma.rn.f64 	%fd968, %fd966, %fd955, %fd967;\n"
"	mul.f64 	%fd969, %fd955, %fd968;\n"
"	fma.rn.f64 	%fd58, %fd969, %fd56, %fd56;\n"
"	copysign.f64 	%fd1319, %fd1, %fd58;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_61:\n"
"	mul.f64 	%fd21, %fd1, 0dBFE6A09E667F3BCD;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r12}, %fd21;\n"
"	}\n"
"	and.b32  	%r13, %r12, 2147483647;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r14, %temp}, %fd21;\n"
"	}\n"
"	setp.lt.u32 	%p39, %r13, 2146435072;\n"
"	@%p39 bra 	$L__BB0_63;\n"
"	bra.uni 	$L__BB0_62;\n"
"\n"
"$L__BB0_63:\n"
"	mov.b64 	%fd362, {%r14, %r13};\n"
"	add.f64 	%fd363, %fd362, 0dC010000000000000;\n"
"	mov.f64 	%fd364, 0dC010000000000000;\n"
"	add.f64 	%fd365, %fd362, 0d4010000000000000;\n"
"	rcp.approx.ftz.f64 	%fd366, %fd365;\n"
"	neg.f64 	%fd367, %fd365;\n"
"	mov.f64 	%fd368, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd369, %fd367, %fd366, %fd368;\n"
"	fma.rn.f64 	%fd370, %fd369, %fd369, %fd369;\n"
"	fma.rn.f64 	%fd371, %fd370, %fd366, %fd366;\n"
"	mul.f64 	%fd372, %fd363, %fd371;\n"
"	add.rn.f64 	%fd373, %fd372, %fd368;\n"
"	fma.rn.f64 	%fd374, %fd364, %fd373, %fd362;\n"
"	neg.f64 	%fd375, %fd372;\n"
"	fma.rn.f64 	%fd376, %fd375, %fd362, %fd374;\n"
"	fma.rn.f64 	%fd377, %fd371, %fd376, %fd372;\n"
"	mov.f64 	%fd378, 0dBE44E1C6FD03D328;\n"
"	mov.f64 	%fd379, 0dBDF8774AD4E0BFD7;\n"
"	fma.rn.f64 	%fd380, %fd379, %fd377, %fd378;\n"
"	mov.f64 	%fd381, 0dBE4330149F7A56B6;\n"
"	fma.rn.f64 	%fd382, %fd380, %fd377, %fd381;\n"
"	mov.f64 	%fd383, 0d3E7BEDDED8376273;\n"
"	fma.rn.f64 	%fd384, %fd382, %fd377, %fd383;\n"
"	mov.f64 	%fd385, 0d3E6F9254C3ABF22B;\n"
"	fma.rn.f64 	%fd386, %fd384, %fd377, %fd385;\n"
"	mov.f64 	%fd387, 0dBEAB9068C2148CF0;\n"
"	fma.rn.f64 	%fd388, %fd386, %fd377, %fd387;\n"
"	mov.f64 	%fd389, 0d3E94C6454DB34009;\n"
"	fma.rn.f64 	%fd390, %fd388, %fd377, %fd389;\n"
"	mov.f64 	%fd391, 0d3ED7F1C378F2311D;\n"
"	fma.rn.f64 	%fd392, %fd390, %fd377, %fd391;\n"
"	mov.f64 	%fd393, 0dBEE78E051C6D5C58;\n"
"	fma.rn.f64 	%fd394, %fd392, %fd377, %fd393;\n"
"	mov.f64 	%fd395, 0dBEF995B4EAD14A90;\n"
"	fma.rn.f64 	%fd396, %fd394, %fd377, %fd395;\n"
"	mov.f64 	%fd397, 0d3F23BE27CF0A29B2;\n"
"	fma.rn.f64 	%fd398, %fd396, %fd377, %fd397;\n"
"	mov.f64 	%fd399, 0dBF2A1DEF3E81672E;\n"
"	fma.rn.f64 	%fd400, %fd398, %fd377, %fd399;\n"
"	mov.f64 	%fd401, 0dBF48D4ABE68C1713;\n"
"	fma.rn.f64 	%fd402, %fd400, %fd377, %fd401;\n"
"	mov.f64 	%fd403, 0d3F749C67210DD6B4;\n"
"	fma.rn.f64 	%fd404, %fd402, %fd377, %fd403;\n"
"	mov.f64 	%fd405, 0dBF9096238568E357;\n"
"	fma.rn.f64 	%fd406, %fd404, %fd377, %fd405;\n"
"	mov.f64 	%fd407, 0d3FA3079EDF8C2DC9;\n"
"	fma.rn.f64 	%fd408, %fd406, %fd377, %fd407;\n"
"	mov.f64 	%fd409, 0dBFB0FB06DFF601FC;\n"
"	fma.rn.f64 	%fd410, %fd408, %fd377, %fd409;\n"
"	mov.f64 	%fd411, 0d3FB7FEE004DFBCDC;\n"
"	fma.rn.f64 	%fd412, %fd410, %fd377, %fd411;\n"
"	mov.f64 	%fd413, 0dBFB9DDB23C3DB8C6;\n"
"	fma.rn.f64 	%fd414, %fd412, %fd377, %fd413;\n"
"	mov.f64 	%fd415, 0d3FB16ECEFCFA5FDA;\n"
"	fma.rn.f64 	%fd416, %fd414, %fd377, %fd415;\n"
"	mov.f64 	%fd417, 0d3F8F7F5DF66FB6D6;\n"
"	fma.rn.f64 	%fd418, %fd416, %fd377, %fd417;\n"
"	mov.f64 	%fd419, 0dBFC1DF1AD154A29D;\n"
"	fma.rn.f64 	%fd420, %fd418, %fd377, %fd419;\n"
"	mov.f64 	%fd421, 0d3FF3BA5916E9FD7F;\n"
"	fma.rn.f64 	%fd422, %fd420, %fd377, %fd421;\n"
"	mov.f64 	%fd423, 0d4000000000000000;\n"
"	fma.rn.f64 	%fd424, %fd423, %fd362, %fd368;\n"
"	rcp.approx.ftz.f64 	%fd425, %fd424;\n"
"	neg.f64 	%fd426, %fd424;\n"
"	fma.rn.f64 	%fd427, %fd426, %fd425, %fd368;\n"
"	fma.rn.f64 	%fd428, %fd427, %fd427, %fd427;\n"
"	fma.rn.f64 	%fd429, %fd428, %fd425, %fd425;\n"
"	mul.f64 	%fd430, %fd422, %fd429;\n"
"	mul.f64 	%fd431, %fd430, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd432, %fd362, %fd431, %fd422;\n"
"	neg.f64 	%fd433, %fd430;\n"
"	add.rn.f64 	%fd434, %fd432, %fd433;\n"
"	fma.rn.f64 	%fd435, %fd434, %fd429, %fd430;\n"
"	mul.f64 	%fd436, %fd362, %fd362;\n"
"	neg.f64 	%fd437, %fd436;\n"
"	mov.f64 	%fd438, 0d4338000000000000;\n"
"	mov.f64 	%fd439, 0d3FF71547652B82FE;\n"
"	fma.rn.f64 	%fd440, %fd437, %fd439, %fd438;\n"
"	mov.f64 	%fd441, 0dC338000000000000;\n"
"	add.rn.f64 	%fd442, %fd440, %fd441;\n"
"	mov.f64 	%fd443, 0dBFE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd444, %fd442, %fd443, %fd437;\n"
"	mov.f64 	%fd445, 0dBC7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd446, %fd442, %fd445, %fd444;\n"
"	mov.f64 	%fd447, 0d3E928AF3FCA213EA;\n"
"	mov.f64 	%fd448, 0d3E5ADE1569CE2BDF;\n"
"	fma.rn.f64 	%fd449, %fd448, %fd446, %fd447;\n"
"	mov.f64 	%fd450, 0d3EC71DEE62401315;\n"
"	fma.rn.f64 	%fd451, %fd449, %fd446, %fd450;\n"
"	mov.f64 	%fd452, 0d3EFA01997C89EB71;\n"
"	fma.rn.f64 	%fd453, %fd451, %fd446, %fd452;\n"
"	mov.f64 	%fd454, 0d3F2A01A014761F65;\n"
"	fma.rn.f64 	%fd455, %fd453, %fd446, %fd454;\n"
"	mov.f64 	%fd456, 0d3F56C16C1852B7AF;\n"
"	fma.rn.f64 	%fd457, %fd455, %fd446, %fd456;\n"
"	mov.f64 	%fd458, 0d3F81111111122322;\n"
"	fma.rn.f64 	%fd459, %fd457, %fd446, %fd458;\n"
"	mov.f64 	%fd460, 0d3FA55555555502A1;\n"
"	fma.rn.f64 	%fd461, %fd459, %fd446, %fd460;\n"
"	mov.f64 	%fd462, 0d3FC5555555555511;\n"
"	fma.rn.f64 	%fd463, %fd461, %fd446, %fd462;\n"
"	mov.f64 	%fd464, 0d3FE000000000000B;\n"
"	fma.rn.f64 	%fd465, %fd463, %fd446, %fd464;\n"
"	fma.rn.f64 	%fd466, %fd465, %fd446, %fd368;\n"
"	fma.rn.f64 	%fd467, %fd466, %fd446, %fd368;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r89, %temp}, %fd440;\n"
"	}\n"
"	shr.u32 	%r90, %r89, 31;\n"
"	add.s32 	%r91, %r89, %r90;\n"
"	shr.s32 	%r92, %r91, 1;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r93, %temp}, %fd467;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r94}, %fd467;\n"
"	}\n"
"	shl.b32 	%r95, %r92, 20;\n"
"	add.s32 	%r96, %r94, %r95;\n"
"	mov.b64 	%fd468, {%r93, %r96};\n"
"	sub.s32 	%r97, %r89, %r92;\n"
"	shl.b32 	%r98, %r97, 20;\n"
"	add.s32 	%r99, %r98, 1072693248;\n"
"	mov.u32 	%r100, 0;\n"
"	mov.b64 	%fd469, {%r100, %r99};\n"
"	mul.f64 	%fd470, %fd468, %fd469;\n"
"	neg.f64 	%fd471, %fd362;\n"
"	fma.rn.f64 	%fd472, %fd471, %fd362, %fd436;\n"
"	fma.rn.f64 	%fd473, %fd470, %fd472, %fd470;\n"
"	mul.f64 	%fd474, %fd435, %fd473;\n"
"	setp.gt.u32 	%p44, %r13, 1077624832;\n"
"	selp.f64 	%fd475, 0d0000000000000000, %fd474, %p44;\n"
"	sub.f64 	%fd476, %fd423, %fd475;\n"
"	setp.lt.s32 	%p45, %r12, 0;\n"
"	selp.f64 	%fd23, %fd476, %fd475, %p45;\n"
"	mul.f64 	%fd1319, %fd23, 0d3FE0000000000000;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_40:\n"
"	setp.eq.s32 	%p47, %r16, 2146435072;\n"
"	setp.eq.s32 	%p48, %r17, 0;\n"
"	and.pred  	%p49, %p47, %p48;\n"
"	setp.lt.s32 	%p50, %r15, 0;\n"
"	selp.f64 	%fd477, 0d4000000000000000, 0d0000000000000000, %p50;\n"
"	add.f64 	%fd478, %fd1, %fd1;\n"
"	selp.f64 	%fd1319, %fd477, %fd478, %p49;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_12:\n"
"	abs.f64 	%fd1100, %fd1;\n"
"	mov.f64 	%fd1101, 0d3FE0000000000000;\n"
"	mov.f64 	%fd1102, 0dBFE0000000000000;\n"
"	fma.rn.f64 	%fd1103, %fd1102, %fd1100, %fd1101;\n"
"	rsqrt.approx.ftz.f64 	%fd1104, %fd1103;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r187, %temp}, %fd1104;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r188}, %fd1104;\n"
"	}\n"
"	add.s32 	%r189, %r188, -1048576;\n"
"	mov.b64 	%fd1105, {%r187, %r189};\n"
"	mul.f64 	%fd1106, %fd1103, %fd1104;\n"
"	neg.f64 	%fd1107, %fd1106;\n"
"	fma.rn.f64 	%fd1108, %fd1106, %fd1107, %fd1103;\n"
"	fma.rn.f64 	%fd1109, %fd1108, %fd1105, %fd1106;\n"
"	neg.f64 	%fd1110, %fd1109;\n"
"	mov.f64 	%fd1111, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd1112, %fd1104, %fd1110, %fd1111;\n"
"	fma.rn.f64 	%fd1113, %fd1112, %fd1105, %fd1105;\n"
"	fma.rn.f64 	%fd1114, %fd1109, %fd1110, %fd1103;\n"
"	fma.rn.f64 	%fd1115, %fd1114, %fd1113, %fd1109;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r190}, %fd1103;\n"
"	}\n"
"	setp.lt.s32 	%p83, %r190, 0;\n"
"	selp.f64 	%fd1116, 0dFFF8000000000000, %fd1115, %p83;\n"
"	setp.ne.f64 	%p84, %fd1103, 0d0000000000000000;\n"
"	selp.f64 	%fd1117, %fd1116, %fd1103, %p84;\n"
"	mov.f64 	%fd1118, 0dBFB3823B180754AF;\n"
"	mov.f64 	%fd1119, 0d3FB0066BDC1895E9;\n"
"	fma.rn.f64 	%fd1120, %fd1119, %fd1103, %fd1118;\n"
"	mov.f64 	%fd1121, 0d3FB11E52CC2F79AE;\n"
"	fma.rn.f64 	%fd1122, %fd1120, %fd1103, %fd1121;\n"
"	mov.f64 	%fd1123, 0dBF924EAF3526861B;\n"
"	fma.rn.f64 	%fd1124, %fd1122, %fd1103, %fd1123;\n"
"	mov.f64 	%fd1125, 0d3F91DF02A31E6CB7;\n"
"	fma.rn.f64 	%fd1126, %fd1124, %fd1103, %fd1125;\n"
"	mov.f64 	%fd1127, 0d3F847D18B0EEC6CC;\n"
"	fma.rn.f64 	%fd1128, %fd1126, %fd1103, %fd1127;\n"
"	mov.f64 	%fd1129, 0d3F8D0AF961BA53B0;\n"
"	fma.rn.f64 	%fd1130, %fd1128, %fd1103, %fd1129;\n"
"	mov.f64 	%fd1131, 0d3F91BF7734CF1C48;\n"
"	fma.rn.f64 	%fd1132, %fd1130, %fd1103, %fd1131;\n"
"	mov.f64 	%fd1133, 0d3F96E91483144EF7;\n"
"	fma.rn.f64 	%fd1134, %fd1132, %fd1103, %fd1133;\n"
"	mov.f64 	%fd1135, 0d3F9F1C6E0A4F9F81;\n"
"	fma.rn.f64 	%fd1136, %fd1134, %fd1103, %fd1135;\n"
"	mov.f64 	%fd1137, 0d3FA6DB6DC27FA92B;\n"
"	fma.rn.f64 	%fd1138, %fd1136, %fd1103, %fd1137;\n"
"	mov.f64 	%fd1139, 0d3FB333333320F91B;\n"
"	fma.rn.f64 	%fd1140, %fd1138, %fd1103, %fd1139;\n"
"	mov.f64 	%fd1141, 0d3FC5555555555F4D;\n"
"	fma.rn.f64 	%fd1142, %fd1140, %fd1103, %fd1141;\n"
"	mul.f64 	%fd1143, %fd1103, %fd1142;\n"
"	mul.f64 	%fd1144, %fd1117, 0dC000000000000000;\n"
"	mov.f64 	%fd1145, 0d3C91A62633145C07;\n"
"	fma.rn.f64 	%fd1146, %fd1144, %fd1143, %fd1145;\n"
"	add.f64 	%fd1147, %fd1144, 0d3FE921FB54442D18;\n"
"	add.f64 	%fd1148, %fd1147, %fd1146;\n"
"	add.f64 	%fd1149, %fd1148, 0d3FE921FB54442D18;\n"
"	copysign.f64 	%fd1319, %fd1, %fd1149;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_35:\n"
"	mov.f64 	%fd679, 0d7FF0000000000000;\n"
"	fma.rn.f64 	%fd680, %fd1, %fd679, %fd679;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r116}, %fd1;\n"
"	}\n"
"	mov.b32 	%f8, %r116;\n"
"	setp.eq.f32 	%p56, %f8, 0f00000000;\n"
"	selp.f64 	%fd1308, 0dFFF0000000000000, %fd680, %p56;\n"
"\n"
"$L__BB0_69:\n"
"	mul.f64 	%fd725, %fd1308, 0d3C695355BAAAFAD3;\n"
"	mov.f64 	%fd726, 0d3FDBCB7B1526E50E;\n"
"	fma.rn.f64 	%fd1319, %fd1308, %fd726, %fd725;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_22:\n"
"	setp.le.f64 	%p68, %fd1, 0d7FF0000000000000;\n"
"	selp.f64 	%fd52, 0d7FF0000000000000, %fd1, %p68;\n"
"	add.f64 	%fd1319, %fd52, %fd52;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_44:\n"
"	setp.leu.f64 	%p26, %fd1, 0d0000000000000000;\n"
"	setp.geu.f64 	%p27, %fd1, 0d3FF0000000000000;\n"
"	or.pred  	%p28, %p27, %p26;\n"
"	@%p28 bra 	$L__BB0_59;\n"
"	bra.uni 	$L__BB0_45;\n"
"\n"
"$L__BB0_59:\n"
"	atom.global.max.u32 	%r87, [%rd1], 1;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_78:\n"
"	add.f64 	%fd807, %fd48, %fd48;\n"
"	mov.f64 	%fd808, 0d4000000000000000;\n"
"	cvt.rn.f32.f64 	%f11, %fd807;\n"
"	mul.f32 	%f12, %f11, 0f3FB8AA3B;\n"
"	cvt.rni.f32.f32 	%f13, %f12;\n"
"	cvt.f64.f32 	%fd809, %f13;\n"
"	neg.f64 	%fd810, %fd809;\n"
"	mov.f64 	%fd811, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd812, %fd810, %fd811, %fd807;\n"
"	mov.f64 	%fd813, 0d3E928A27F89B6999;\n"
"	mov.f64 	%fd814, 0d3E5AE904A4741B81;\n"
"	fma.rn.f64 	%fd815, %fd814, %fd812, %fd813;\n"
"	mov.f64 	%fd816, 0d3EC71DE715FF7E07;\n"
"	fma.rn.f64 	%fd817, %fd815, %fd812, %fd816;\n"
"	mov.f64 	%fd818, 0d3EFA019A6B0AC45A;\n"
"	fma.rn.f64 	%fd819, %fd817, %fd812, %fd818;\n"
"	mov.f64 	%fd820, 0d3F2A01A017EED94F;\n"
"	fma.rn.f64 	%fd821, %fd819, %fd812, %fd820;\n"
"	mov.f64 	%fd822, 0d3F56C16C17F2A71B;\n"
"	fma.rn.f64 	%fd823, %fd821, %fd812, %fd822;\n"
"	mov.f64 	%fd824, 0d3F811111111173C4;\n"
"	fma.rn.f64 	%fd825, %fd823, %fd812, %fd824;\n"
"	mov.f64 	%fd826, 0d3FA555555555211A;\n"
"	fma.rn.f64 	%fd827, %fd825, %fd812, %fd826;\n"
"	mov.f64 	%fd828, 0d3FC5555555555540;\n"
"	fma.rn.f64 	%fd829, %fd827, %fd812, %fd828;\n"
"	mov.f64 	%fd830, 0d3FE0000000000005;\n"
"	fma.rn.f64 	%fd831, %fd829, %fd812, %fd830;\n"
"	mul.f64 	%fd832, %fd812, %fd831;\n"
"	fma.rn.f64 	%fd833, %fd832, %fd812, %fd812;\n"
"	ex2.approx.ftz.f32 	%f14, %f13;\n"
"	cvt.f64.f32 	%fd834, %f14;\n"
"	mov.f64 	%fd835, 0d3FF0000000000000;\n"
"	sub.f64 	%fd836, %fd835, %fd834;\n"
"	neg.f64 	%fd837, %fd833;\n"
"	fma.rn.f64 	%fd838, %fd837, %fd834, %fd836;\n"
"	sub.f64 	%fd839, %fd808, %fd838;\n"
"	rcp.approx.ftz.f64 	%fd840, %fd839;\n"
"	neg.f64 	%fd841, %fd839;\n"
"	fma.rn.f64 	%fd842, %fd841, %fd840, %fd835;\n"
"	fma.rn.f64 	%fd843, %fd842, %fd842, %fd842;\n"
"	fma.rn.f64 	%fd844, %fd843, %fd840, %fd840;\n"
"	neg.f64 	%fd845, %fd844;\n"
"	fma.rn.f64 	%fd846, %fd808, %fd845, %fd835;\n"
"	setp.gt.u32 	%p66, %r41, 1077088193;\n"
"	selp.f64 	%fd847, 0d3FF0000000000000, %fd846, %p66;\n"
"	copysign.f64 	%fd1319, %fd1, %fd847;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_85:\n"
"	mov.f64 	%fd1019, 0d3FF0000000000000;\n"
"	sub.f64 	%fd66, %fd1019, %fd65;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r43}, %fd66;\n"
"	}\n"
"	setp.lt.s32 	%p78, %r43, 1;\n"
"	@%p78 bra 	$L__BB0_87;\n"
"\n"
"	add.s32 	%r178, %r43, -1048576;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r179, %temp}, %fd66;\n"
"	}\n"
"	mov.b64 	%fd1020, {%r179, %r178};\n"
"	rsqrt.approx.ftz.f64 	%fd1021, %fd1020;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r180}, %fd1021;\n"
"	}\n"
"	add.s32 	%r181, %r180, -1048576;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r182, %temp}, %fd1021;\n"
"	}\n"
"	mov.b64 	%fd1022, {%r182, %r181};\n"
"	mul.f64 	%fd1023, %fd1020, %fd1021;\n"
"	neg.f64 	%fd1024, %fd1023;\n"
"	fma.rn.f64 	%fd1025, %fd1023, %fd1024, %fd1020;\n"
"	fma.rn.f64 	%fd1026, %fd1025, %fd1022, %fd1023;\n"
"	neg.f64 	%fd1027, %fd1026;\n"
"	fma.rn.f64 	%fd1028, %fd1026, %fd1027, %fd1020;\n"
"	fma.rn.f64 	%fd1030, %fd1021, %fd1027, %fd1019;\n"
"	fma.rn.f64 	%fd1031, %fd1030, %fd1022, %fd1022;\n"
"	fma.rn.f64 	%fd1032, %fd1028, %fd1031, %fd1026;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r183}, %fd1032;\n"
"	}\n"
"	add.s32 	%r184, %r183, 1048576;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r185, %temp}, %fd1032;\n"
"	}\n"
"	mov.b64 	%fd1033, {%r185, %r184};\n"
"	mov.f64 	%fd1034, 0dBEBAC2FE66FAAC4B;\n"
"	mov.f64 	%fd1035, 0d3EC715B371155F70;\n"
"	fma.rn.f64 	%fd1036, %fd1035, %fd66, %fd1034;\n"
"	mov.f64 	%fd1037, 0d3ED9A9B88EFCD9B8;\n"
"	fma.rn.f64 	%fd1038, %fd1036, %fd66, %fd1037;\n"
"	mov.f64 	%fd1039, 0d3EDD0F40A8A0C4C3;\n"
"	fma.rn.f64 	%fd1040, %fd1038, %fd66, %fd1039;\n"
"	mov.f64 	%fd1041, 0d3EF46D4CFA9E0E1F;\n"
"	fma.rn.f64 	%fd1042, %fd1040, %fd66, %fd1041;\n"
"	mov.f64 	%fd1043, 0d3F079C168D1E2422;\n"
"	fma.rn.f64 	%fd1044, %fd1042, %fd66, %fd1043;\n"
"	mov.f64 	%fd1045, 0d3F1C9A88C3BCA540;\n"
"	fma.rn.f64 	%fd1046, %fd1044, %fd66, %fd1045;\n"
"	mov.f64 	%fd1047, 0d3F31C4E64BD476DF;\n"
"	fma.rn.f64 	%fd1048, %fd1046, %fd66, %fd1047;\n"
"	mov.f64 	%fd1049, 0d3F46E8BA60009C8F;\n"
"	fma.rn.f64 	%fd1050, %fd1048, %fd66, %fd1049;\n"
"	mov.f64 	%fd1051, 0d3F5F1C71C62B05A2;\n"
"	fma.rn.f64 	%fd1052, %fd1050, %fd66, %fd1051;\n"
"	mov.f64 	%fd1053, 0d3F76DB6DB6DC9F2C;\n"
"	fma.rn.f64 	%fd1054, %fd1052, %fd66, %fd1053;\n"
"	mov.f64 	%fd1055, 0d3F9333333333329C;\n"
"	fma.rn.f64 	%fd1056, %fd1054, %fd66, %fd1055;\n"
"	mov.f64 	%fd1057, 0d3FB5555555555555;\n"
"	fma.rn.f64 	%fd1058, %fd1056, %fd66, %fd1057;\n"
"	mul.f64 	%fd1059, %fd66, %fd1058;\n"
"	fma.rn.f64 	%fd1319, %fd1059, %fd1033, %fd1033;\n"
"	bra.uni 	$L__BB0_88;\n"
"\n"
"$L__BB0_73:\n"
"	mov.f64 	%fd727, 0d7FF0000000000000;\n"
"	fma.rn.f64 	%fd728, %fd1, %fd727, %fd727;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r129}, %fd1;\n"
"	}\n"
"	mov.b32 	%f9, %r129;\n"
"	setp.eq.f32 	%p60, %f9, 0f00000000;\n"
"	selp.f64 	%fd1319, 0dFFF0000000000000, %fd728, %p60;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_82:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r166}, %fd56;\n"
"	}\n"
"	mov.f64 	%fd910, 0d4338000000000000;\n"
"	mov.f64 	%fd911, 0d3FF71547652B82FE;\n"
"	fma.rn.f64 	%fd912, %fd56, %fd911, %fd910;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r167, %temp}, %fd912;\n"
"	}\n"
"	add.s32 	%r168, %r167, -1;\n"
"	mov.f64 	%fd913, 0dC338000000000000;\n"
"	add.rn.f64 	%fd914, %fd912, %fd913;\n"
"	mov.f64 	%fd915, 0dBFE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd916, %fd914, %fd915, %fd56;\n"
"	mov.f64 	%fd917, 0dBC7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd918, %fd914, %fd917, %fd916;\n"
"	shl.b32 	%r169, %r166, 1;\n"
"	setp.lt.u32 	%p70, %r169, 2142496327;\n"
"	selp.f64 	%fd919, %fd56, %fd918, %p70;\n"
"	selp.b32 	%r170, 0, %r168, %p70;\n"
"	mov.u32 	%r171, 0;\n"
"	mov.f64 	%fd920, 0d3E5AF86D8EBD13CD;\n"
"	mov.f64 	%fd921, 0d3E21F4076ACD15B6;\n"
"	fma.rn.f64 	%fd922, %fd921, %fd919, %fd920;\n"
"	mov.f64 	%fd923, 0d3E927E5092BA033D;\n"
"	fma.rn.f64 	%fd924, %fd922, %fd919, %fd923;\n"
"	mov.f64 	%fd925, 0d3EC71DDE6C5F9DA1;\n"
"	fma.rn.f64 	%fd926, %fd924, %fd919, %fd925;\n"
"	mov.f64 	%fd927, 0d3EFA01A018D034E6;\n"
"	fma.rn.f64 	%fd928, %fd926, %fd919, %fd927;\n"
"	mov.f64 	%fd929, 0d3F2A01A01B3B6940;\n"
"	fma.rn.f64 	%fd930, %fd928, %fd919, %fd929;\n"
"	mov.f64 	%fd931, 0d3F56C16C16C1B5DD;\n"
"	fma.rn.f64 	%fd932, %fd930, %fd919, %fd931;\n"
"	mov.f64 	%fd933, 0d3F8111111110F74D;\n"
"	fma.rn.f64 	%fd934, %fd932, %fd919, %fd933;\n"
"	mov.f64 	%fd935, 0d3FA555555555554D;\n"
"	fma.rn.f64 	%fd936, %fd934, %fd919, %fd935;\n"
"	mov.f64 	%fd937, 0d3FC5555555555557;\n"
"	fma.rn.f64 	%fd938, %fd936, %fd919, %fd937;\n"
"	mov.f64 	%fd939, 0d3FE0000000000000;\n"
"	fma.rn.f64 	%fd940, %fd938, %fd919, %fd939;\n"
"	mul.f64 	%fd941, %fd919, %fd940;\n"
"	fma.rn.f64 	%fd942, %fd941, %fd919, %fd919;\n"
"	setp.eq.s32 	%p71, %r170, 1024;\n"
"	selp.b32 	%r172, -1, 0, %p71;\n"
"	add.s32 	%r173, %r170, %r172;\n"
"	shl.b32 	%r174, %r173, 20;\n"
"	add.s32 	%r175, %r174, 1072693248;\n"
"	mov.b64 	%fd943, {%r171, %r175};\n"
"	mov.u32 	%r176, 1071644672;\n"
"	mov.b64 	%fd944, {%r171, %r176};\n"
"	sub.f64 	%fd945, %fd943, %fd944;\n"
"	fma.rn.f64 	%fd946, %fd942, %fd943, %fd945;\n"
"	add.f64 	%fd947, %fd946, %fd946;\n"
"	selp.f64 	%fd948, %fd947, %fd946, %p71;\n"
"	setp.eq.s32 	%p72, %r169, 0;\n"
"	selp.f64 	%fd949, %fd919, %fd948, %p72;\n"
"	mov.f64 	%fd950, 0d3FF0000000000000;\n"
"	mov.f64 	%fd951, 0d4000000000000000;\n"
"	fma.rn.f64 	%fd952, %fd951, %fd949, %fd950;\n"
"	div.rn.f64 	%fd953, %fd949, %fd952;\n"
"	add.f64 	%fd954, %fd949, %fd953;\n"
"	setp.ge.f64 	%p73, %fd56, 0d408633CE8FB9F87E;\n"
"	selp.f64 	%fd57, 0d7FF0000000000000, %fd954, %p73;\n"
"	copysign.f64 	%fd1319, %fd1, %fd57;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_62:\n"
"	setp.eq.s32 	%p40, %r13, 2146435072;\n"
"	setp.eq.s32 	%p41, %r14, 0;\n"
"	and.pred  	%p42, %p40, %p41;\n"
"	setp.lt.s32 	%p43, %r12, 0;\n"
"	selp.f64 	%fd360, 0d4000000000000000, 0d0000000000000000, %p43;\n"
"	add.f64 	%fd361, %fd21, %fd21;\n"
"	selp.f64 	%fd22, %fd360, %fd361, %p42;\n"
"	mul.f64 	%fd1319, %fd22, 0d3FE0000000000000;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_7:\n"
"	mul.f64 	%fd1228, %fd1, 0d3FE45F306DC9C883;\n"
"	cvt.rni.s32.f64 	%r219, %fd1228;\n"
"	cvt.rn.f64.s32 	%fd1229, %r219;\n"
"	neg.f64 	%fd1230, %fd1229;\n"
"	mov.f64 	%fd1231, 0d3FF921FB54442D18;\n"
"	fma.rn.f64 	%fd1232, %fd1230, %fd1231, %fd1;\n"
"	mov.f64 	%fd1233, 0d3C91A62633145C00;\n"
"	fma.rn.f64 	%fd1234, %fd1230, %fd1233, %fd1232;\n"
"	mov.f64 	%fd1235, 0d397B839A252049C0;\n"
"	fma.rn.f64 	%fd1315, %fd1230, %fd1235, %fd1234;\n"
"	setp.ltu.f64 	%p92, %fd86, 0d41E0000000000000;\n"
"	@%p92 bra 	$L__BB0_103;\n"
"\n"
"	{ // callseq 1, 0\n"
"	.reg .b32 temp_param_reg;\n"
"	.param .b64 param0;\n"
"	st.param.f64 	[param0+0], %fd1;\n"
"	.param .align 8 .b8 retval0[16];\n"
"	call.uni (retval0), \n"
"	__internal_trig_reduction_slowpathd, \n"
"	(\n"
"	param0\n"
"	);\n"
"	ld.param.f64 	%fd1315, [retval0+0];\n"
"	ld.param.b32 	%r219, [retval0+8];\n"
"	} // callseq 1\n"
"\n"
"$L__BB0_103:\n"
"	add.s32 	%r50, %r219, 1;\n"
"	and.b32  	%r194, %r50, 1;\n"
"	shl.b32 	%r195, %r50, 3;\n"
"	and.b32  	%r196, %r195, 8;\n"
"	mul.wide.u32 	%rd12, %r196, 8;\n"
"	mov.u64 	%rd13, __cudart_sin_cos_coeffs;\n"
"	add.s64 	%rd14, %rd13, %rd12;\n"
"	setp.eq.s32 	%p93, %r194, 0;\n"
"	selp.f64 	%fd1237, 0d3DE5DB65F9785EBA, 0dBDA8FF8320FD8164, %p93;\n"
"	ld.global.nc.v2.f64 	{%fd1238, %fd1239}, [%rd14];\n"
"	mul.rn.f64 	%fd91, %fd1315, %fd1315;\n"
"	fma.rn.f64 	%fd1242, %fd1237, %fd91, %fd1238;\n"
"	fma.rn.f64 	%fd1243, %fd1242, %fd91, %fd1239;\n"
"	ld.global.nc.v2.f64 	{%fd1244, %fd1245}, [%rd14+16];\n"
"	fma.rn.f64 	%fd1248, %fd1243, %fd91, %fd1244;\n"
"	fma.rn.f64 	%fd1249, %fd1248, %fd91, %fd1245;\n"
"	ld.global.nc.v2.f64 	{%fd1250, %fd1251}, [%rd14+32];\n"
"	fma.rn.f64 	%fd1254, %fd1249, %fd91, %fd1250;\n"
"	fma.rn.f64 	%fd92, %fd1254, %fd91, %fd1251;\n"
"	fma.rn.f64 	%fd1319, %fd92, %fd1315, %fd1315;\n"
"	@%p93 bra 	$L__BB0_105;\n"
"\n"
"	mov.f64 	%fd1255, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd1319, %fd92, %fd91, %fd1255;\n"
"\n"
"$L__BB0_105:\n"
"	and.b32  	%r197, %r50, 2;\n"
"	setp.eq.s32 	%p94, %r197, 0;\n"
"	@%p94 bra 	$L__BB0_115;\n"
"\n"
"	mov.f64 	%fd1256, 0d0000000000000000;\n"
"	mov.f64 	%fd1257, 0dBFF0000000000000;\n"
"	fma.rn.f64 	%fd1319, %fd1319, %fd1257, %fd1256;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_108:\n"
"	mul.f64 	%fd1258, %fd1, 0d3FE45F306DC9C883;\n"
"	cvt.rni.s32.f64 	%r220, %fd1258;\n"
"	cvt.rn.f64.s32 	%fd1259, %r220;\n"
"	neg.f64 	%fd1260, %fd1259;\n"
"	mov.f64 	%fd1261, 0d3FF921FB54442D18;\n"
"	fma.rn.f64 	%fd1262, %fd1260, %fd1261, %fd1;\n"
"	mov.f64 	%fd1263, 0d3C91A62633145C00;\n"
"	fma.rn.f64 	%fd1264, %fd1260, %fd1263, %fd1262;\n"
"	mov.f64 	%fd1265, 0d397B839A252049C0;\n"
"	fma.rn.f64 	%fd1317, %fd1260, %fd1265, %fd1264;\n"
"	setp.ltu.f64 	%p96, %fd97, 0d41E0000000000000;\n"
"	@%p96 bra 	$L__BB0_111;\n"
"\n"
"	{ // callseq 2, 0\n"
"	.reg .b32 temp_param_reg;\n"
"	.param .b64 param0;\n"
"	st.param.f64 	[param0+0], %fd1;\n"
"	.param .align 8 .b8 retval0[16];\n"
"	call.uni (retval0), \n"
"	__internal_trig_reduction_slowpathd, \n"
"	(\n"
"	param0\n"
"	);\n"
"	ld.param.f64 	%fd1317, [retval0+0];\n"
"	ld.param.b32 	%r220, [retval0+8];\n"
"	} // callseq 2\n"
"\n"
"$L__BB0_111:\n"
"	and.b32  	%r199, %r220, 1;\n"
"	shl.b32 	%r200, %r220, 3;\n"
"	and.b32  	%r201, %r200, 8;\n"
"	mul.wide.u32 	%rd15, %r201, 8;\n"
"	mov.u64 	%rd16, __cudart_sin_cos_coeffs;\n"
"	add.s64 	%rd17, %rd16, %rd15;\n"
"	setp.eq.s32 	%p97, %r199, 0;\n"
"	selp.f64 	%fd1267, 0d3DE5DB65F9785EBA, 0dBDA8FF8320FD8164, %p97;\n"
"	ld.global.nc.v2.f64 	{%fd1268, %fd1269}, [%rd17];\n"
"	mul.rn.f64 	%fd102, %fd1317, %fd1317;\n"
"	fma.rn.f64 	%fd1272, %fd1267, %fd102, %fd1268;\n"
"	fma.rn.f64 	%fd1273, %fd1272, %fd102, %fd1269;\n"
"	ld.global.nc.v2.f64 	{%fd1274, %fd1275}, [%rd17+16];\n"
"	fma.rn.f64 	%fd1278, %fd1273, %fd102, %fd1274;\n"
"	fma.rn.f64 	%fd1279, %fd1278, %fd102, %fd1275;\n"
"	ld.global.nc.v2.f64 	{%fd1280, %fd1281}, [%rd17+32];\n"
"	fma.rn.f64 	%fd1284, %fd1279, %fd102, %fd1280;\n"
"	fma.rn.f64 	%fd103, %fd1284, %fd102, %fd1281;\n"
"	fma.rn.f64 	%fd1319, %fd103, %fd1317, %fd1317;\n"
"	@%p97 bra 	$L__BB0_113;\n"
"\n"
"	mov.f64 	%fd1285, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd1319, %fd103, %fd102, %fd1285;\n"
"\n"
"$L__BB0_113:\n"
"	and.b32  	%r202, %r220, 2;\n"
"	setp.eq.s32 	%p98, %r202, 0;\n"
"	@%p98 bra 	$L__BB0_115;\n"
"\n"
"	mov.f64 	%fd1286, 0d0000000000000000;\n"
"	mov.f64 	%fd1287, 0dBFF0000000000000;\n"
"	fma.rn.f64 	%fd1319, %fd1319, %fd1287, %fd1286;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_97:\n"
"	mul.f64 	%fd1177, %fd1, 0d3FE45F306DC9C883;\n"
"	cvt.rni.s32.f64 	%r218, %fd1177;\n"
"	cvt.rn.f64.s32 	%fd1178, %r218;\n"
"	neg.f64 	%fd1179, %fd1178;\n"
"	mov.f64 	%fd1180, 0d3FF921FB54442D18;\n"
"	fma.rn.f64 	%fd1181, %fd1179, %fd1180, %fd1;\n"
"	mov.f64 	%fd1182, 0d3C91A62633145C00;\n"
"	fma.rn.f64 	%fd1183, %fd1179, %fd1182, %fd1181;\n"
"	mov.f64 	%fd1184, 0d397B839A252049C0;\n"
"	fma.rn.f64 	%fd1314, %fd1179, %fd1184, %fd1183;\n"
"	setp.ltu.f64 	%p86, %fd78, 0d41E0000000000000;\n"
"	@%p86 bra 	$L__BB0_100;\n"
"\n"
"	{ // callseq 0, 0\n"
"	.reg .b32 temp_param_reg;\n"
"	.param .b64 param0;\n"
"	st.param.f64 	[param0+0], %fd1;\n"
"	.param .align 8 .b8 retval0[16];\n"
"	call.uni (retval0), \n"
"	__internal_trig_reduction_slowpathd, \n"
"	(\n"
"	param0\n"
"	);\n"
"	ld.param.f64 	%fd1314, [retval0+0];\n"
"	ld.param.b32 	%r218, [retval0+8];\n"
"	} // callseq 0\n"
"\n"
"$L__BB0_100:\n"
"	mul.f64 	%fd1186, %fd1314, %fd1314;\n"
"	mov.f64 	%fd1187, 0dBEF9757C5B27EBB1;\n"
"	mov.f64 	%fd1188, 0d3EE48DAC2799BCB9;\n"
"	fma.rn.f64 	%fd1189, %fd1188, %fd1186, %fd1187;\n"
"	mov.f64 	%fd1190, 0d3F0980E90FD91E04;\n"
"	fma.rn.f64 	%fd1191, %fd1189, %fd1186, %fd1190;\n"
"	mov.f64 	%fd1192, 0dBEFAE2B0417D7E1D;\n"
"	fma.rn.f64 	%fd1193, %fd1191, %fd1186, %fd1192;\n"
"	mov.f64 	%fd1194, 0d3F119F5341BFBA57;\n"
"	fma.rn.f64 	%fd1195, %fd1193, %fd1186, %fd1194;\n"
"	mov.f64 	%fd1196, 0d3F15E791A00F6919;\n"
"	fma.rn.f64 	%fd1197, %fd1195, %fd1186, %fd1196;\n"
"	mov.f64 	%fd1198, 0d3F2FF2E7FADEC73A;\n"
"	fma.rn.f64 	%fd1199, %fd1197, %fd1186, %fd1198;\n"
"	mov.f64 	%fd1200, 0d3F434BC1B206DA62;\n"
"	fma.rn.f64 	%fd1201, %fd1199, %fd1186, %fd1200;\n"
"	mov.f64 	%fd1202, 0d3F57DB18EF2F83F9;\n"
"	fma.rn.f64 	%fd1203, %fd1201, %fd1186, %fd1202;\n"
"	mov.f64 	%fd1204, 0d3F6D6D2E7AE49FBC;\n"
"	fma.rn.f64 	%fd1205, %fd1203, %fd1186, %fd1204;\n"
"	mov.f64 	%fd1206, 0d3F8226E3A816A776;\n"
"	fma.rn.f64 	%fd1207, %fd1205, %fd1186, %fd1206;\n"
"	mov.f64 	%fd1208, 0d3F9664F485D25660;\n"
"	fma.rn.f64 	%fd1209, %fd1207, %fd1186, %fd1208;\n"
"	mov.f64 	%fd1210, 0d3FABA1BA1BABF31D;\n"
"	fma.rn.f64 	%fd1211, %fd1209, %fd1186, %fd1210;\n"
"	mov.f64 	%fd1212, 0d3FC11111111105D2;\n"
"	fma.rn.f64 	%fd1213, %fd1211, %fd1186, %fd1212;\n"
"	mov.f64 	%fd1214, 0d3FD555555555555E;\n"
"	fma.rn.f64 	%fd1215, %fd1213, %fd1186, %fd1214;\n"
"	mul.f64 	%fd83, %fd1186, %fd1215;\n"
"	fma.rn.f64 	%fd1319, %fd83, %fd1314, %fd1314;\n"
"	and.b32  	%r192, %r218, 1;\n"
"	setp.eq.b32 	%p87, %r192, 1;\n"
"	mov.pred 	%p88, 0;\n"
"	xor.pred  	%p89, %p87, %p88;\n"
"	not.pred 	%p90, %p89;\n"
"	@%p90 bra 	$L__BB0_115;\n"
"\n"
"	sub.f64 	%fd1216, %fd1319, %fd1314;\n"
"	neg.f64 	%fd1217, %fd1216;\n"
"	fma.rn.f64 	%fd1218, %fd83, %fd1314, %fd1217;\n"
"	neg.f64 	%fd1219, %fd1319;\n"
"	rcp.approx.ftz.f64 	%fd1220, %fd1319;\n"
"	mov.f64 	%fd1221, 0d3FF0000000000000;\n"
"	fma.rn.f64 	%fd1222, %fd1219, %fd1220, %fd1221;\n"
"	fma.rn.f64 	%fd1223, %fd1222, %fd1222, %fd1222;\n"
"	fma.rn.f64 	%fd1224, %fd1223, %fd1220, %fd1220;\n"
"	neg.f64 	%fd1225, %fd1224;\n"
"	fma.rn.f64 	%fd1226, %fd1319, %fd1225, %fd1221;\n"
"	fma.rn.f64 	%fd1227, %fd1225, %fd1218, %fd1226;\n"
"	fma.rn.f64 	%fd1319, %fd1227, %fd1225, %fd1225;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_45:\n"
"	add.f64 	%fd2, %fd1, %fd1;\n"
"	neg.f64 	%fd3, %fd2;\n"
"	mov.f64 	%fd110, 0d4000000000000000;\n"
"	add.rn.f64 	%fd4, %fd110, %fd3;\n"
"	setp.ge.f64 	%p29, %fd2, 0d3F4FA4D2AD8F904D;\n"
"	setp.le.f64 	%p30, %fd2, 0d3FFFFC0B65AA4E0E;\n"
"	and.pred  	%p31, %p29, %p30;\n"
"	@%p31 bra 	$L__BB0_57;\n"
"	bra.uni 	$L__BB0_46;\n"
"\n"
"$L__BB0_57:\n"
"	mul.rn.f64 	%fd263, %fd4, %fd2;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r78}, %fd263;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r79, %temp}, %fd263;\n"
"	}\n"
"	shr.u32 	%r80, %r78, 20;\n"
"	and.b32  	%r81, %r80, 2046;\n"
"	add.s32 	%r82, %r81, 2147482626;\n"
"	mov.u32 	%r83, 1127219200;\n"
"	mov.b64 	%fd264, {%r82, %r83};\n"
"	mov.u32 	%r84, -2147483648;\n"
"	mov.b64 	%fd265, {%r84, %r83};\n"
"	sub.f64 	%fd266, %fd264, %fd265;\n"
"	and.b32  	%r85, %r78, -2145386497;\n"
"	or.b32  	%r86, %r85, 1071644672;\n"
"	mov.b64 	%fd267, {%r79, %r86};\n"
"	add.f64 	%fd268, %fd267, 0dBFF0000000000000;\n"
"	add.f64 	%fd269, %fd267, 0d3FF0000000000000;\n"
"	mov.f64 	%fd270, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd271, %fd269;\n"
"	neg.f64 	%fd272, %fd269;\n"
"	fma.rn.f64 	%fd273, %fd272, %fd271, %fd270;\n"
"	fma.rn.f64 	%fd274, %fd273, %fd273, %fd273;\n"
"	fma.rn.f64 	%fd275, %fd274, %fd271, %fd271;\n"
"	mul.f64 	%fd276, %fd268, %fd275;\n"
"	mov.f64 	%fd277, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd278, %fd277, %fd276, %fd268;\n"
"	neg.f64 	%fd279, %fd276;\n"
"	fma.rn.f64 	%fd280, %fd279, %fd268, %fd278;\n"
"	fma.rn.f64 	%fd281, %fd280, %fd275, %fd276;\n"
"	mul.f64 	%fd282, %fd281, %fd281;\n"
"	mov.f64 	%fd283, 0d3FA55CF59CDC5D89;\n"
"	mov.f64 	%fd284, 0d3FB5C5C218C775C9;\n"
"	fma.rn.f64 	%fd285, %fd284, %fd282, %fd283;\n"
"	mov.f64 	%fd286, 0d3FAEFD18CF6EBB9C;\n"
"	fma.rn.f64 	%fd287, %fd285, %fd282, %fd286;\n"
"	mov.f64 	%fd288, 0d3FB10682EDCB8D1B;\n"
"	fma.rn.f64 	%fd289, %fd287, %fd282, %fd288;\n"
"	mov.f64 	%fd290, 0d3FB3B1DD3AC7FC96;\n"
"	fma.rn.f64 	%fd291, %fd289, %fd282, %fd290;\n"
"	mov.f64 	%fd292, 0d3FB745CB459B54A6;\n"
"	fma.rn.f64 	%fd293, %fd291, %fd282, %fd292;\n"
"	mov.f64 	%fd294, 0d3FBC71C741A0669F;\n"
"	fma.rn.f64 	%fd295, %fd293, %fd282, %fd294;\n"
"	mov.f64 	%fd296, 0d3FC249249209112E;\n"
"	fma.rn.f64 	%fd297, %fd295, %fd282, %fd296;\n"
"	mov.f64 	%fd298, 0d3FC99999999A06C1;\n"
"	fma.rn.f64 	%fd299, %fd297, %fd282, %fd298;\n"
"	mov.f64 	%fd300, 0d3FD5555555555535;\n"
"	fma.rn.f64 	%fd301, %fd299, %fd282, %fd300;\n"
"	mul.f64 	%fd302, %fd282, %fd301;\n"
"	fma.rn.f64 	%fd303, %fd302, %fd281, %fd281;\n"
"	add.f64 	%fd304, %fd303, %fd303;\n"
"	mov.f64 	%fd305, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd306, %fd266, %fd305, %fd304;\n"
"	mov.f64 	%fd307, 0dC009000000000000;\n"
"	sub.f64 	%fd308, %fd307, %fd306;\n"
"	mov.f64 	%fd309, 0dBC08DDF93324D327;\n"
"	mov.f64 	%fd310, 0dBBB135D2E746E627;\n"
"	fma.rn.f64 	%fd311, %fd310, %fd308, %fd309;\n"
"	mov.f64 	%fd312, 0d3C37B83EEF0B7C9F;\n"
"	fma.rn.f64 	%fd313, %fd311, %fd308, %fd312;\n"
"	mov.f64 	%fd314, 0d3C69BA72CD589B91;\n"
"	fma.rn.f64 	%fd315, %fd313, %fd308, %fd314;\n"
"	mov.f64 	%fd316, 0dBCA33689090A6B96;\n"
"	fma.rn.f64 	%fd317, %fd315, %fd308, %fd316;\n"
"	mov.f64 	%fd318, 0d3C782E11898132E0;\n"
"	fma.rn.f64 	%fd319, %fd317, %fd308, %fd318;\n"
"	mov.f64 	%fd320, 0d3CFDE4ACFD9E26BA;\n"
"	fma.rn.f64 	%fd321, %fd319, %fd308, %fd320;\n"
"	mov.f64 	%fd322, 0dBD26D33EED66C487;\n"
"	fma.rn.f64 	%fd323, %fd321, %fd308, %fd322;\n"
"	mov.f64 	%fd324, 0dBD36F2167040D8E2;\n"
"	fma.rn.f64 	%fd325, %fd323, %fd308, %fd324;\n"
"	mov.f64 	%fd326, 0d3D872A22C2D77E20;\n"
"	fma.rn.f64 	%fd327, %fd325, %fd308, %fd326;\n"
"	mov.f64 	%fd328, 0dBDAC8859C4E5C0AF;\n"
"	fma.rn.f64 	%fd329, %fd327, %fd308, %fd328;\n"
"	mov.f64 	%fd330, 0dBDCDC583D118A561;\n"
"	fma.rn.f64 	%fd331, %fd329, %fd308, %fd330;\n"
"	mov.f64 	%fd332, 0d3E120F47CCF46B3C;\n"
"	fma.rn.f64 	%fd333, %fd331, %fd308, %fd332;\n"
"	mov.f64 	%fd334, 0dBE31A9E38DC84D60;\n"
"	fma.rn.f64 	%fd335, %fd333, %fd308, %fd334;\n"
"	mov.f64 	%fd336, 0dBE5F36CD6D3D46A9;\n"
"	fma.rn.f64 	%fd337, %fd335, %fd308, %fd336;\n"
"	mov.f64 	%fd338, 0d3E9C6B4F5D03B787;\n"
"	fma.rn.f64 	%fd339, %fd337, %fd308, %fd338;\n"
"	mov.f64 	%fd340, 0dBEB6E8A5434AE8A2;\n"
"	fma.rn.f64 	%fd341, %fd339, %fd308, %fd340;\n"
"	mov.f64 	%fd342, 0dBEED1D1F7B8736F6;\n"
"	fma.rn.f64 	%fd343, %fd341, %fd308, %fd342;\n"
"	mov.f64 	%fd344, 0d3F2879C2A212F024;\n"
"	fma.rn.f64 	%fd345, %fd343, %fd308, %fd344;\n"
"	mov.f64 	%fd346, 0dBF4845769484FCA8;\n"
"	fma.rn.f64 	%fd347, %fd345, %fd308, %fd346;\n"
"	mov.f64 	%fd348, 0dBF78B6C33114F909;\n"
"	fma.rn.f64 	%fd349, %fd347, %fd308, %fd348;\n"
"	mov.f64 	%fd350, 0d3FCEBD80D9B13E28;\n"
"	fma.rn.f64 	%fd351, %fd349, %fd308, %fd350;\n"
"	mov.f64 	%fd352, 0d3FFA755E7C99AE86;\n"
"	fma.rn.f64 	%fd353, %fd351, %fd308, %fd352;\n"
"	fma.rn.f64 	%fd1305, %fd353, %fd3, %fd353;\n"
"	bra.uni 	$L__BB0_58;\n"
"\n"
"$L__BB0_94:\n"
"	mov.f64 	%fd1097, 0d3C91A62633145C07;\n"
"	add.rn.f64 	%fd1098, %fd73, %fd1097;\n"
"	mov.f64 	%fd1099, 0d3FF921FB54442D18;\n"
"	add.rn.f64 	%fd1319, %fd1099, %fd1098;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_87:\n"
"	mov.f64 	%fd1060, 0d0000000000000000;\n"
"	mul.rn.f64 	%fd1319, %fd65, %fd1060;\n"
"\n"
"$L__BB0_88:\n"
"	setp.gt.s32 	%p79, %r43, -1;\n"
"	@%p79 bra 	$L__BB0_90;\n"
"\n"
"	mov.f64 	%fd1061, 0d7FF0000000000000;\n"
"	mul.rn.f64 	%fd1319, %fd1319, %fd1061;\n"
"\n"
"$L__BB0_90:\n"
"	setp.gt.s32 	%p80, %r42, -1;\n"
"	@%p80 bra 	$L__BB0_115;\n"
"\n"
"	mov.f64 	%fd1062, 0dBCA1A62633145C07;\n"
"	add.rn.f64 	%fd1063, %fd1319, %fd1062;\n"
"	neg.f64 	%fd1064, %fd1063;\n"
"	mov.f64 	%fd1065, 0d400921FB54442D18;\n"
"	add.rn.f64 	%fd1319, %fd1065, %fd1064;\n"
"	bra.uni 	$L__BB0_115;\n"
"\n"
"$L__BB0_46:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r1}, %fd2;\n"
"	}\n"
"	setp.gt.s32 	%p32, %r1, 1072693247;\n"
"	selp.f64 	%fd1301, %fd4, %fd2, %p32;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r206}, %fd1301;\n"
"	}\n"
"	mov.b32 	%f2, %r206;\n"
"	setp.ltu.f32 	%p33, %f2, 0f2B2BFF2F;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r207, %temp}, %fd1301;\n"
"	}\n"
"	@%p33 bra 	$L__BB0_48;\n"
"	bra.uni 	$L__BB0_47;\n"
"\n"
"$L__BB0_48:\n"
"	setp.gt.s32 	%p34, %r206, 1048575;\n"
"	mov.u32 	%r208, -1023;\n"
"	@%p34 bra 	$L__BB0_50;\n"
"\n"
"	mul.f64 	%fd1301, %fd1301, 0d4350000000000000;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r206}, %fd1301;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r207, %temp}, %fd1301;\n"
"	}\n"
"	mov.u32 	%r208, -1077;\n"
"\n"
"$L__BB0_50:\n"
"	add.s32 	%r67, %r206, -1;\n"
"	setp.lt.u32 	%p35, %r67, 2146435071;\n"
"	@%p35 bra 	$L__BB0_52;\n"
"	bra.uni 	$L__BB0_51;\n"
"\n"
"$L__BB0_52:\n"
"	shr.u32 	%r69, %r206, 20;\n"
"	add.s32 	%r209, %r208, %r69;\n"
"	and.b32  	%r70, %r206, -2146435073;\n"
"	or.b32  	%r71, %r70, 1072693248;\n"
"	mov.b64 	%fd1302, {%r207, %r71};\n"
"	setp.lt.s32 	%p37, %r71, 1073127583;\n"
"	@%p37 bra 	$L__BB0_54;\n"
"\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r72, %temp}, %fd1302;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r73}, %fd1302;\n"
"	}\n"
"	add.s32 	%r74, %r73, -1048576;\n"
"	mov.b64 	%fd1302, {%r72, %r74};\n"
"	add.s32 	%r209, %r209, 1;\n"
"\n"
"$L__BB0_54:\n"
"	add.f64 	%fd197, %fd1302, 0d3FF0000000000000;\n"
"	mov.f64 	%fd198, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd199, %fd197;\n"
"	neg.f64 	%fd200, %fd197;\n"
"	fma.rn.f64 	%fd201, %fd200, %fd199, %fd198;\n"
"	fma.rn.f64 	%fd202, %fd201, %fd201, %fd201;\n"
"	fma.rn.f64 	%fd203, %fd202, %fd199, %fd199;\n"
"	add.f64 	%fd204, %fd1302, 0dBFF0000000000000;\n"
"	mul.f64 	%fd205, %fd204, %fd203;\n"
"	fma.rn.f64 	%fd206, %fd204, %fd203, %fd205;\n"
"	mul.f64 	%fd207, %fd206, %fd206;\n"
"	mov.f64 	%fd208, 0d3ED0EE258B7A8B04;\n"
"	mov.f64 	%fd209, 0d3EB1380B3AE80F1E;\n"
"	fma.rn.f64 	%fd210, %fd209, %fd207, %fd208;\n"
"	mov.f64 	%fd211, 0d3EF3B2669F02676F;\n"
"	fma.rn.f64 	%fd212, %fd210, %fd207, %fd211;\n"
"	mov.f64 	%fd213, 0d3F1745CBA9AB0956;\n"
"	fma.rn.f64 	%fd214, %fd212, %fd207, %fd213;\n"
"	mov.f64 	%fd215, 0d3F3C71C72D1B5154;\n"
"	fma.rn.f64 	%fd216, %fd214, %fd207, %fd215;\n"
"	mov.f64 	%fd217, 0d3F624924923BE72D;\n"
"	fma.rn.f64 	%fd218, %fd216, %fd207, %fd217;\n"
"	mov.f64 	%fd219, 0d3F8999999999A3C4;\n"
"	fma.rn.f64 	%fd220, %fd218, %fd207, %fd219;\n"
"	mov.f64 	%fd221, 0d3FB5555555555554;\n"
"	fma.rn.f64 	%fd222, %fd220, %fd207, %fd221;\n"
"	sub.f64 	%fd223, %fd204, %fd206;\n"
"	add.f64 	%fd224, %fd223, %fd223;\n"
"	neg.f64 	%fd225, %fd206;\n"
"	fma.rn.f64 	%fd226, %fd225, %fd204, %fd224;\n"
"	mul.f64 	%fd227, %fd203, %fd226;\n"
"	mul.f64 	%fd228, %fd207, %fd222;\n"
"	fma.rn.f64 	%fd229, %fd228, %fd206, %fd227;\n"
"	xor.b32  	%r75, %r209, -2147483648;\n"
"	mov.u32 	%r76, -2147483648;\n"
"	mov.u32 	%r77, 1127219200;\n"
"	mov.b64 	%fd230, {%r75, %r77};\n"
"	mov.b64 	%fd231, {%r76, %r77};\n"
"	sub.f64 	%fd232, %fd230, %fd231;\n"
"	mov.f64 	%fd233, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd234, %fd232, %fd233, %fd206;\n"
"	neg.f64 	%fd235, %fd232;\n"
"	fma.rn.f64 	%fd236, %fd235, %fd233, %fd234;\n"
"	sub.f64 	%fd237, %fd236, %fd206;\n"
"	sub.f64 	%fd238, %fd229, %fd237;\n"
"	mov.f64 	%fd239, 0d3C7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd240, %fd232, %fd239, %fd238;\n"
"	add.f64 	%fd1303, %fd234, %fd240;\n"
"	bra.uni 	$L__BB0_55;\n"
"\n"
"$L__BB0_47:\n"
"	shr.u32 	%r58, %r206, 20;\n"
"	and.b32  	%r59, %r58, 2046;\n"
"	add.s32 	%r60, %r59, 2147482626;\n"
"	mov.u32 	%r61, 1127219200;\n"
"	mov.b64 	%fd111, {%r60, %r61};\n"
"	mov.u32 	%r62, -2147483648;\n"
"	mov.b64 	%fd112, {%r62, %r61};\n"
"	sub.f64 	%fd113, %fd111, %fd112;\n"
"	and.b32  	%r63, %r206, -2145386497;\n"
"	or.b32  	%r64, %r63, 1071644672;\n"
"	mov.b64 	%fd114, {%r207, %r64};\n"
"	add.f64 	%fd115, %fd114, 0dBFF0000000000000;\n"
"	add.f64 	%fd116, %fd114, 0d3FF0000000000000;\n"
"	mov.f64 	%fd117, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd118, %fd116;\n"
"	neg.f64 	%fd119, %fd116;\n"
"	fma.rn.f64 	%fd120, %fd119, %fd118, %fd117;\n"
"	fma.rn.f64 	%fd121, %fd120, %fd120, %fd120;\n"
"	fma.rn.f64 	%fd122, %fd121, %fd118, %fd118;\n"
"	mul.f64 	%fd123, %fd115, %fd122;\n"
"	mov.f64 	%fd124, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd125, %fd124, %fd123, %fd115;\n"
"	neg.f64 	%fd126, %fd123;\n"
"	fma.rn.f64 	%fd127, %fd126, %fd115, %fd125;\n"
"	fma.rn.f64 	%fd128, %fd127, %fd122, %fd123;\n"
"	mul.f64 	%fd129, %fd128, %fd128;\n"
"	mov.f64 	%fd130, 0d3FA55CF59CDC5D89;\n"
"	mov.f64 	%fd131, 0d3FB5C5C218C775C9;\n"
"	fma.rn.f64 	%fd132, %fd131, %fd129, %fd130;\n"
"	mov.f64 	%fd133, 0d3FAEFD18CF6EBB9C;\n"
"	fma.rn.f64 	%fd134, %fd132, %fd129, %fd133;\n"
"	mov.f64 	%fd135, 0d3FB10682EDCB8D1B;\n"
"	fma.rn.f64 	%fd136, %fd134, %fd129, %fd135;\n"
"	mov.f64 	%fd137, 0d3FB3B1DD3AC7FC96;\n"
"	fma.rn.f64 	%fd138, %fd136, %fd129, %fd137;\n"
"	mov.f64 	%fd139, 0d3FB745CB459B54A6;\n"
"	fma.rn.f64 	%fd140, %fd138, %fd129, %fd139;\n"
"	mov.f64 	%fd141, 0d3FBC71C741A0669F;\n"
"	fma.rn.f64 	%fd142, %fd140, %fd129, %fd141;\n"
"	mov.f64 	%fd143, 0d3FC249249209112E;\n"
"	fma.rn.f64 	%fd144, %fd142, %fd129, %fd143;\n"
"	mov.f64 	%fd145, 0d3FC99999999A06C1;\n"
"	fma.rn.f64 	%fd146, %fd144, %fd129, %fd145;\n"
"	mov.f64 	%fd147, 0d3FD5555555555535;\n"
"	fma.rn.f64 	%fd148, %fd146, %fd129, %fd147;\n"
"	mul.f64 	%fd149, %fd129, %fd148;\n"
"	fma.rn.f64 	%fd150, %fd149, %fd128, %fd128;\n"
"	add.f64 	%fd151, %fd150, %fd150;\n"
"	mov.f64 	%fd152, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd153, %fd113, %fd152, %fd151;\n"
"	neg.f64 	%fd154, %fd153;\n"
"	rsqrt.approx.ftz.f64 	%fd155, %fd154;\n"
"	mul.rn.f64 	%fd156, %fd155, %fd155;\n"
"	neg.f64 	%fd157, %fd156;\n"
"	fma.rn.f64 	%fd158, %fd154, %fd157, %fd117;\n"
"	mov.f64 	%fd159, 0d3FE0000000000000;\n"
"	mov.f64 	%fd160, 0d3FD8000000000000;\n"
"	fma.rn.f64 	%fd161, %fd160, %fd158, %fd159;\n"
"	mul.rn.f64 	%fd162, %fd158, %fd155;\n"
"	fma.rn.f64 	%fd163, %fd161, %fd162, %fd155;\n"
"	mov.f64 	%fd164, 0d4000A0E7333839AA;\n"
"	mov.f64 	%fd165, 0d3FEBE9222591AFAB;\n"
"	fma.rn.f64 	%fd166, %fd165, %fd163, %fd164;\n"
"	mov.f64 	%fd167, 0d4008768CF7E57D5C;\n"
"	fma.rn.f64 	%fd168, %fd166, %fd163, %fd167;\n"
"	mov.f64 	%fd169, 0d400B77E7E28DA583;\n"
"	fma.rn.f64 	%fd170, %fd168, %fd163, %fd169;\n"
"	mov.f64 	%fd171, 0d3FF34F26A4F99CF9;\n"
"	fma.rn.f64 	%fd172, %fd170, %fd163, %fd171;\n"
"	mov.f64 	%fd173, 0d3FC1F674ADB019ED;\n"
"	fma.rn.f64 	%fd174, %fd172, %fd163, %fd173;\n"
"	mov.f64 	%fd175, 0d3F75DDAE9506431D;\n"
"	fma.rn.f64 	%fd176, %fd174, %fd163, %fd175;\n"
"	mov.f64 	%fd177, 0d3F0ADA49AA32489C;\n"
"	fma.rn.f64 	%fd178, %fd176, %fd163, %fd177;\n"
"	add.f64 	%fd179, %fd163, 0d4001E90FF51C2197;\n"
"	mov.f64 	%fd180, 0d40111EA3A7CF3820;\n"
"	fma.rn.f64 	%fd181, %fd179, %fd163, %fd180;\n"
"	mov.f64 	%fd182, 0d4011A0E4A4749594;\n"
"	fma.rn.f64 	%fd183, %fd181, %fd163, %fd182;\n"
"	mov.f64 	%fd184, 0d400D4E977D38C14D;\n"
"	fma.rn.f64 	%fd185, %fd183, %fd163, %fd184;\n"
"	mov.f64 	%fd186, 0d3FF37FD567EC0D5F;\n"
"	fma.rn.f64 	%fd187, %fd185, %fd163, %fd186;\n"
"	mov.f64 	%fd188, 0d3FC1FB9D7F676033;\n"
"	fma.rn.f64 	%fd189, %fd187, %fd163, %fd188;\n"
"	mov.f64 	%fd190, 0d3F75DDCDF98946E4;\n"
"	fma.rn.f64 	%fd191, %fd189, %fd163, %fd190;\n"
"	mov.f64 	%fd192, 0d3F0ADA42D79D8DBB;\n"
"	fma.rn.f64 	%fd193, %fd191, %fd163, %fd192;\n"
"	mul.f64 	%fd194, %fd163, %fd193;\n"
"	div.rn.f64 	%fd1304, %fd178, %fd194;\n"
"	bra.uni 	$L__BB0_56;\n"
"\n"
"$L__BB0_51:\n"
"	mov.f64 	%fd195, 0d7FF0000000000000;\n"
"	fma.rn.f64 	%fd196, %fd1301, %fd195, %fd195;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r68}, %fd1301;\n"
"	}\n"
"	mov.b32 	%f3, %r68;\n"
"	setp.eq.f32 	%p36, %f3, 0f00000000;\n"
"	selp.f64 	%fd1303, 0dFFF0000000000000, %fd196, %p36;\n"
"\n"
"$L__BB0_55:\n"
"	neg.f64 	%fd241, %fd1303;\n"
"	rsqrt.approx.f64 	%fd242, %fd241;\n"
"	mov.f64 	%fd243, 0d3FFA2013964E259C;\n"
"	mov.f64 	%fd244, 0d3FE8E2101C71B0BF;\n"
"	fma.rn.f64 	%fd245, %fd244, %fd242, %fd243;\n"
"	mov.f64 	%fd246, 0d3FDABFE90921BE68;\n"
"	fma.rn.f64 	%fd247, %fd245, %fd242, %fd246;\n"
"	mov.f64 	%fd248, 0d3F97E41314DE00D4;\n"
"	fma.rn.f64 	%fd249, %fd247, %fd242, %fd248;\n"
"	mov.f64 	%fd250, 0d3F311BD487102E94;\n"
"	fma.rn.f64 	%fd251, %fd249, %fd242, %fd250;\n"
"	add.f64 	%fd252, %fd242, 0d3FF59895C30BAA54;\n"
"	mov.f64 	%fd253, 0d3FFAE8E5956A143F;\n"
"	fma.rn.f64 	%fd254, %fd252, %fd242, %fd253;\n"
"	mov.f64 	%fd255, 0d3FDACCE85FF7383D;\n"
"	fma.rn.f64 	%fd256, %fd254, %fd242, %fd255;\n"
"	mov.f64 	%fd257, 0d3F97E43B6CAC34FE;\n"
"	fma.rn.f64 	%fd258, %fd256, %fd242, %fd257;\n"
"	mov.f64 	%fd259, 0d3F311BD08289EB12;\n"
"	fma.rn.f64 	%fd260, %fd258, %fd242, %fd259;\n"
"	mul.f64 	%fd261, %fd242, %fd260;\n"
"	div.rn.f64 	%fd1304, %fd251, %fd261;\n"
"\n"
"$L__BB0_56:\n"
"	neg.f64 	%fd262, %fd1304;\n"
"	selp.f64 	%fd1305, %fd262, %fd1304, %p32;\n"
"\n"
"$L__BB0_58:\n"
"	mul.f64 	%fd354, %fd1305, 0dBCA21165F626CDD5;\n"
"	mov.f64 	%fd355, 0dBFF6A09E667F3BCC;\n"
"	fma.rn.f64 	%fd356, %fd355, %fd1305, %fd354;\n"
"	mov.f64 	%fd357, 0d0000000000000000;\n"
"	add.rn.f64 	%fd1319, %fd356, %fd357;\n"
"\n"
"$L__BB0_115:\n"
"	mov.u32 	%r205, %ntid.x;\n"
"	mov.u32 	%r204, %ctaid.x;\n"
"	mov.u32 	%r203, %tid.x;\n"
"	cvt.u64.u32 	%rd25, %r203;\n"
"	mul.wide.u32 	%rd24, %r204, %r205;\n"
"	add.s64 	%rd23, %rd24, %rd25;\n"
"	shl.b64 	%rd22, %rd23, 3;\n"
"	ld.param.u64 	%rd21, [omni_cuda_map_scientific_f64_param_3];\n"
"	cvta.to.global.u64 	%rd18, %rd21;\n"
"	add.s64 	%rd20, %rd18, %rd22;\n"
"	st.global.f64 	[%rd20], %fd1319;\n"
"\n"
"$L__BB0_116:\n"
"	ret;\n"
"\n"
"}\n"
"	// .globl	omni_cuda_map_scientific_f32\n"
".visible .entry omni_cuda_map_scientific_f32(\n"
"	.param .u64 omni_cuda_map_scientific_f32_param_0,\n"
"	.param .u32 omni_cuda_map_scientific_f32_param_1,\n"
"	.param .u64 omni_cuda_map_scientific_f32_param_2,\n"
"	.param .u64 omni_cuda_map_scientific_f32_param_3,\n"
"	.param .u64 omni_cuda_map_scientific_f32_param_4\n"
")\n"
"{\n"
"	.local .align 4 .b8 	__local_depot1[28];\n"
"	.reg .b64 	%SP;\n"
"	.reg .b64 	%SPL;\n"
"	.reg .pred 	%p<96>;\n"
"	.reg .f32 	%f<530>;\n"
"	.reg .b32 	%r<231>;\n"
"	.reg .f64 	%fd<281>;\n"
"	.reg .b64 	%rd<80>;\n"
"\n"
"\n"
"	mov.u64 	%SPL, __local_depot1;\n"
"	ld.param.u64 	%rd23, [omni_cuda_map_scientific_f32_param_0];\n"
"	ld.param.u32 	%r66, [omni_cuda_map_scientific_f32_param_1];\n"
"	ld.param.u64 	%rd25, [omni_cuda_map_scientific_f32_param_2];\n"
"	ld.param.u64 	%rd26, [omni_cuda_map_scientific_f32_param_4];\n"
"	cvta.to.global.u64 	%rd1, %rd26;\n"
"	add.u64 	%rd2, %SPL, 0;\n"
"	mov.u32 	%r67, %ctaid.x;\n"
"	mov.u32 	%r68, %ntid.x;\n"
"	mul.wide.u32 	%rd28, %r67, %r68;\n"
"	mov.u32 	%r69, %tid.x;\n"
"	cvt.u64.u32 	%rd29, %r69;\n"
"	add.s64 	%rd3, %rd28, %rd29;\n"
"	setp.ge.u64 	%p1, %rd3, %rd25;\n"
"	@%p1 bra 	$L__BB1_95;\n"
"\n"
"	cvta.to.global.u64 	%rd30, %rd23;\n"
"	shl.b64 	%rd31, %rd3, 2;\n"
"	add.s64 	%rd32, %rd30, %rd31;\n"
"	ld.global.f32 	%f1, [%rd32];\n"
"	add.s64 	%rd4, %rd2, 24;\n"
"	setp.gt.s32 	%p2, %r66, 12;\n"
"	@%p2 bra 	$L__BB1_25;\n"
"\n"
"	setp.gt.s32 	%p14, %r66, 8;\n"
"	@%p14 bra 	$L__BB1_16;\n"
"\n"
"	setp.gt.s32 	%p20, %r66, 6;\n"
"	@%p20 bra 	$L__BB1_13;\n"
"\n"
"	setp.eq.s32 	%p23, %r66, 5;\n"
"	@%p23 bra 	$L__BB1_82;\n"
"\n"
"	setp.eq.s32 	%p24, %r66, 6;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p24 bra 	$L__BB1_6;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_6:\n"
"	mul.f32 	%f476, %f1, 0f3F22F983;\n"
"	cvt.rni.s32.f32 	%r226, %f476;\n"
"	cvt.rn.f32.s32 	%f477, %r226;\n"
"	mov.f32 	%f478, 0fBFC90FDA;\n"
"	fma.rn.f32 	%f479, %f477, %f478, %f1;\n"
"	mov.f32 	%f480, 0fB3A22168;\n"
"	fma.rn.f32 	%f481, %f477, %f480, %f479;\n"
"	mov.f32 	%f482, 0fA7C234C5;\n"
"	fma.rn.f32 	%f523, %f477, %f482, %f481;\n"
"	abs.f32 	%f35, %f1;\n"
"	setp.ltu.f32 	%p76, %f35, 0f47CE4780;\n"
"	@%p76 bra 	$L__BB1_78;\n"
"\n"
"	setp.eq.f32 	%p77, %f35, 0f7F800000;\n"
"	@%p77 bra 	$L__BB1_77;\n"
"	bra.uni 	$L__BB1_8;\n"
"\n"
"$L__BB1_77:\n"
"	mov.f32 	%f485, 0f00000000;\n"
"	mul.rn.f32 	%f523, %f1, %f485;\n"
"	mov.u32 	%r226, 0;\n"
"	bra.uni 	$L__BB1_78;\n"
"\n"
"$L__BB1_25:\n"
"	setp.gt.s32 	%p3, %r66, 16;\n"
"	@%p3 bra 	$L__BB1_35;\n"
"\n"
"	setp.gt.s32 	%p9, %r66, 14;\n"
"	@%p9 bra 	$L__BB1_30;\n"
"\n"
"	setp.eq.s32 	%p12, %r66, 13;\n"
"	@%p12 bra 	$L__BB1_65;\n"
"\n"
"	setp.eq.s32 	%p13, %r66, 14;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p13 bra 	$L__BB1_29;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_29:\n"
"	mov.f32 	%f273, 0f3F000000;\n"
"	mov.f32 	%f274, 0f3BBB989D;\n"
"	fma.rn.f32 	%f275, %f1, %f274, %f273;\n"
"	cvt.sat.f32.f32 	%f276, %f275;\n"
"	mov.f32 	%f277, 0f4B400001;\n"
"	mov.f32 	%f278, 0f437C0000;\n"
"	fma.rm.f32 	%f279, %f276, %f278, %f277;\n"
"	add.f32 	%f280, %f279, 0fCB40007F;\n"
"	neg.f32 	%f281, %f280;\n"
"	mov.f32 	%f282, 0f3FB8AA3B;\n"
"	fma.rn.f32 	%f283, %f1, %f282, %f281;\n"
"	mov.f32 	%f284, 0f32A57060;\n"
"	fma.rn.f32 	%f285, %f1, %f284, %f283;\n"
"	mov.b32 	%r113, %f279;\n"
"	shl.b32 	%r114, %r113, 23;\n"
"	mov.b32 	%f286, %r114;\n"
"	ex2.approx.ftz.f32 	%f287, %f285;\n"
"	mul.f32 	%f529, %f287, %f286;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_16:\n"
"	setp.gt.s32 	%p15, %r66, 10;\n"
"	@%p15 bra 	$L__BB1_22;\n"
"\n"
"	setp.eq.s32 	%p18, %r66, 9;\n"
"	@%p18 bra 	$L__BB1_67;\n"
"\n"
"	setp.eq.s32 	%p19, %r66, 10;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p19 bra 	$L__BB1_19;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_19:\n"
"	abs.f32 	%f21, %f1;\n"
"	setp.leu.f32 	%p58, %f21, 0f3F800000;\n"
"	setp.gt.f32 	%p59, %f21, 0f3F800000;\n"
"	rcp.approx.ftz.f32 	%f361, %f21;\n"
"	selp.f32 	%f362, %f361, %f21, %p59;\n"
"	mul.f32 	%f363, %f362, %f362;\n"
"	mov.f32 	%f364, 0fBC6BE14F;\n"
"	mov.f32 	%f365, 0f3B2090AA;\n"
"	fma.rn.f32 	%f366, %f365, %f363, %f364;\n"
"	mov.f32 	%f367, 0f3D23397E;\n"
"	fma.rn.f32 	%f368, %f366, %f363, %f367;\n"
"	mov.f32 	%f369, 0fBD948A7A;\n"
"	fma.rn.f32 	%f370, %f368, %f363, %f369;\n"
"	mov.f32 	%f371, 0f3DD76B21;\n"
"	fma.rn.f32 	%f372, %f370, %f363, %f371;\n"
"	mov.f32 	%f373, 0fBE111E88;\n"
"	fma.rn.f32 	%f374, %f372, %f363, %f373;\n"
"	mov.f32 	%f375, 0f3E4CAF60;\n"
"	fma.rn.f32 	%f376, %f374, %f363, %f375;\n"
"	mov.f32 	%f377, 0fBEAAAA27;\n"
"	fma.rn.f32 	%f378, %f376, %f363, %f377;\n"
"	mul.f32 	%f379, %f363, %f378;\n"
"	fma.rn.f32 	%f521, %f379, %f362, %f362;\n"
"	@%p58 bra 	$L__BB1_21;\n"
"\n"
"	neg.f32 	%f380, %f521;\n"
"	mov.f32 	%f381, 0f3FD774EB;\n"
"	mov.f32 	%f382, 0f3F6EE581;\n"
"	fma.rn.f32 	%f521, %f382, %f381, %f380;\n"
"\n"
"$L__BB1_21:\n"
"	copysign.f32 	%f383, %f1, %f521;\n"
"	setp.le.f32 	%p60, %f21, 0f7F800000;\n"
"	selp.f32 	%f529, %f383, %f521, %p60;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_35:\n"
"	setp.gt.s32 	%p4, %r66, 18;\n"
"	@%p4 bra 	$L__BB1_39;\n"
"\n"
"	setp.eq.s32 	%p7, %r66, 17;\n"
"	@%p7 bra 	$L__BB1_60;\n"
"\n"
"	setp.eq.s32 	%p8, %r66, 18;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p8 bra 	$L__BB1_38;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_38:\n"
"	abs.f32 	%f130, %f1;\n"
"	add.f32 	%f131, %f130, 0fC0800000;\n"
"	mov.f32 	%f132, 0fC0800000;\n"
"	add.f32 	%f133, %f130, 0f40800000;\n"
"	rcp.approx.ftz.f32 	%f134, %f133;\n"
"	mul.rn.f32 	%f135, %f131, %f134;\n"
"	add.f32 	%f136, %f135, 0f3F800000;\n"
"	mov.f32 	%f137, 0f3F800000;\n"
"	fma.rn.f32 	%f138, %f132, %f136, %f130;\n"
"	neg.f32 	%f139, %f135;\n"
"	fma.rn.f32 	%f140, %f139, %f130, %f138;\n"
"	fma.rn.f32 	%f141, %f134, %f140, %f135;\n"
"	mov.f32 	%f142, 0f3BE6E05B;\n"
"	mov.f32 	%f143, 0f3A69A091;\n"
"	fma.rn.f32 	%f144, %f143, %f141, %f142;\n"
"	mov.f32 	%f145, 0fBC81FB4B;\n"
"	fma.rn.f32 	%f146, %f144, %f141, %f145;\n"
"	mov.f32 	%f147, 0f3D15373B;\n"
"	fma.rn.f32 	%f148, %f146, %f141, %f147;\n"
"	mov.f32 	%f149, 0fBD887C5A;\n"
"	fma.rn.f32 	%f150, %f148, %f141, %f149;\n"
"	mov.f32 	%f151, 0f3DC021D5;\n"
"	fma.rn.f32 	%f152, %f150, %f141, %f151;\n"
"	mov.f32 	%f153, 0fBDCED424;\n"
"	fma.rn.f32 	%f154, %f152, %f141, %f153;\n"
"	mov.f32 	%f155, 0f3D8B74DE;\n"
"	fma.rn.f32 	%f156, %f154, %f141, %f155;\n"
"	mov.f32 	%f157, 0f3C7BF170;\n"
"	fma.rn.f32 	%f158, %f156, %f141, %f157;\n"
"	mov.f32 	%f159, 0fBE0EF8D4;\n"
"	fma.rn.f32 	%f160, %f158, %f141, %f159;\n"
"	mov.f32 	%f161, 0f3F9DD2C9;\n"
"	fma.rn.f32 	%f162, %f160, %f141, %f161;\n"
"	mov.f32 	%f163, 0f40000000;\n"
"	fma.rn.f32 	%f164, %f163, %f130, %f137;\n"
"	rcp.approx.ftz.f32 	%f165, %f164;\n"
"	mul.rn.f32 	%f166, %f162, %f165;\n"
"	mul.f32 	%f167, %f166, 0fC0000000;\n"
"	fma.rn.f32 	%f168, %f130, %f167, %f162;\n"
"	sub.f32 	%f169, %f168, %f166;\n"
"	fma.rn.f32 	%f170, %f169, %f165, %f166;\n"
"	mul.f32 	%f171, %f130, %f130;\n"
"	neg.f32 	%f172, %f171;\n"
"	mov.f32 	%f173, 0f3FB8AA3B;\n"
"	mul.rn.f32 	%f174, %f172, %f173;\n"
"	cvt.rzi.f32.f32 	%f175, %f174;\n"
"	abs.f32 	%f176, %f175;\n"
"	setp.gt.f32 	%p42, %f176, 0f42FC0000;\n"
"	mov.f32 	%f177, 0f42FC0000;\n"
"	copysign.f32 	%f178, %f175, %f177;\n"
"	selp.f32 	%f179, %f178, %f175, %p42;\n"
"	mov.f32 	%f180, 0fBF317218;\n"
"	fma.rn.f32 	%f181, %f179, %f180, %f172;\n"
"	mov.f32 	%f182, 0f3102E308;\n"
"	fma.rn.f32 	%f183, %f179, %f182, %f181;\n"
"	mul.f32 	%f184, %f183, 0f3FB8AA3B;\n"
"	add.f32 	%f185, %f179, 0f4B40007F;\n"
"	mov.b32 	%r103, %f185;\n"
"	shl.b32 	%r104, %r103, 23;\n"
"	mov.b32 	%f186, %r104;\n"
"	ex2.approx.ftz.f32 	%f187, %f184;\n"
"	mul.f32 	%f188, %f187, %f186;\n"
"	neg.f32 	%f189, %f130;\n"
"	fma.rn.f32 	%f190, %f189, %f130, %f171;\n"
"	fma.rn.f32 	%f191, %f188, %f190, %f188;\n"
"	mul.f32 	%f192, %f170, %f191;\n"
"	setp.gt.f32 	%p43, %f130, 0f4120E148;\n"
"	selp.f32 	%f193, 0f00000000, %f192, %p43;\n"
"	sub.f32 	%f194, %f163, %f193;\n"
"	setp.lt.f32 	%p44, %f1, 0f00000000;\n"
"	selp.f32 	%f529, %f194, %f193, %p44;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_13:\n"
"	setp.eq.s32 	%p21, %r66, 7;\n"
"	@%p21 bra 	$L__BB1_68;\n"
"\n"
"	setp.eq.s32 	%p22, %r66, 8;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p22 bra 	$L__BB1_15;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_15:\n"
"	abs.f32 	%f418, %f1;\n"
"	neg.f32 	%f419, %f418;\n"
"	mov.f32 	%f420, 0f3F000000;\n"
"	fma.rn.f32 	%f421, %f420, %f419, %f420;\n"
"	rsqrt.approx.ftz.f32 	%f422, %f421;\n"
"	mul.f32 	%f423, %f421, %f422;\n"
"	mul.f32 	%f424, %f422, 0f3F000000;\n"
"	neg.f32 	%f425, %f423;\n"
"	fma.rn.f32 	%f426, %f425, %f424, %f420;\n"
"	fma.rn.f32 	%f427, %f423, %f426, %f423;\n"
"	setp.eq.f32 	%p64, %f418, 0f3F800000;\n"
"	selp.f32 	%f428, 0f00000000, %f427, %p64;\n"
"	setp.gt.f32 	%p65, %f418, 0f3F0F5C29;\n"
"	selp.f32 	%f429, %f428, %f418, %p65;\n"
"	mul.f32 	%f430, %f429, %f429;\n"
"	mov.f32 	%f431, 0f3C99CA97;\n"
"	mov.f32 	%f432, 0f3D4DD2F7;\n"
"	fma.rn.f32 	%f433, %f432, %f430, %f431;\n"
"	mov.f32 	%f434, 0f3D3F90E8;\n"
"	fma.rn.f32 	%f435, %f433, %f430, %f434;\n"
"	mov.f32 	%f436, 0f3D993CCF;\n"
"	fma.rn.f32 	%f437, %f435, %f430, %f436;\n"
"	mov.f32 	%f438, 0f3E2AAC04;\n"
"	fma.rn.f32 	%f439, %f437, %f430, %f438;\n"
"	mul.f32 	%f440, %f430, %f439;\n"
"	fma.rn.f32 	%f441, %f440, %f429, %f429;\n"
"	mul.f32 	%f442, %f441, 0fC0000000;\n"
"	mov.f32 	%f443, 0f3FD774EB;\n"
"	mov.f32 	%f444, 0f3F6EE581;\n"
"	fma.rn.f32 	%f445, %f444, %f443, %f442;\n"
"	selp.f32 	%f446, %f445, %f441, %p65;\n"
"	setp.le.f32 	%p66, %f446, 0f7F800000;\n"
"	copysign.f32 	%f447, %f1, %f446;\n"
"	selp.f32 	%f529, %f447, %f446, %p66;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_30:\n"
"	setp.eq.s32 	%p10, %r66, 15;\n"
"	@%p10 bra 	$L__BB1_62;\n"
"\n"
"	setp.eq.s32 	%p11, %r66, 16;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p11 bra 	$L__BB1_32;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_32:\n"
"	setp.lt.f32 	%p47, %f1, 0f00800000;\n"
"	mul.f32 	%f216, %f1, 0f4B000000;\n"
"	selp.f32 	%f7, %f216, %f1, %p47;\n"
"	selp.f32 	%f217, 0fC1B80000, 0f00000000, %p47;\n"
"	mov.b32 	%r105, %f7;\n"
"	add.s32 	%r106, %r105, -1059760811;\n"
"	and.b32  	%r107, %r106, -8388608;\n"
"	sub.s32 	%r108, %r105, %r107;\n"
"	mov.b32 	%f218, %r108;\n"
"	cvt.rn.f32.s32 	%f219, %r107;\n"
"	mov.f32 	%f220, 0f34000000;\n"
"	fma.rn.f32 	%f221, %f219, %f220, %f217;\n"
"	add.f32 	%f222, %f218, 0fBF800000;\n"
"	mov.f32 	%f223, 0f3E1039F6;\n"
"	mov.f32 	%f224, 0fBE055027;\n"
"	fma.rn.f32 	%f225, %f224, %f222, %f223;\n"
"	mov.f32 	%f226, 0fBDF8CDCC;\n"
"	fma.rn.f32 	%f227, %f225, %f222, %f226;\n"
"	mov.f32 	%f228, 0f3E0F2955;\n"
"	fma.rn.f32 	%f229, %f227, %f222, %f228;\n"
"	mov.f32 	%f230, 0fBE2AD8B9;\n"
"	fma.rn.f32 	%f231, %f229, %f222, %f230;\n"
"	mov.f32 	%f232, 0f3E4CED0B;\n"
"	fma.rn.f32 	%f233, %f231, %f222, %f232;\n"
"	mov.f32 	%f234, 0fBE7FFF22;\n"
"	fma.rn.f32 	%f235, %f233, %f222, %f234;\n"
"	mov.f32 	%f236, 0f3EAAAA78;\n"
"	fma.rn.f32 	%f237, %f235, %f222, %f236;\n"
"	mov.f32 	%f238, 0fBF000000;\n"
"	fma.rn.f32 	%f239, %f237, %f222, %f238;\n"
"	mul.f32 	%f240, %f222, %f239;\n"
"	fma.rn.f32 	%f241, %f240, %f222, %f222;\n"
"	mov.f32 	%f242, 0f3F317218;\n"
"	fma.rn.f32 	%f519, %f221, %f242, %f241;\n"
"	setp.lt.u32 	%p48, %r105, 2139095040;\n"
"	@%p48 bra 	$L__BB1_34;\n"
"\n"
"	mov.f32 	%f243, 0f7F800000;\n"
"	fma.rn.f32 	%f519, %f7, %f243, %f243;\n"
"\n"
"$L__BB1_34:\n"
"	mul.f32 	%f244, %f519, 0f3EDE5BD9;\n"
"	setp.eq.f32 	%p49, %f7, 0f00000000;\n"
"	selp.f32 	%f529, 0fFF800000, %f244, %p49;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_22:\n"
"	setp.eq.s32 	%p16, %r66, 11;\n"
"	@%p16 bra 	$L__BB1_66;\n"
"\n"
"	setp.eq.s32 	%p17, %r66, 12;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p17 bra 	$L__BB1_24;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_24:\n"
"	abs.f32 	%f309, %f1;\n"
"	mov.f32 	%f310, 0f3FB8AA3B;\n"
"	mul.rn.f32 	%f311, %f309, %f310;\n"
"	cvt.rzi.f32.f32 	%f312, %f311;\n"
"	abs.f32 	%f313, %f312;\n"
"	setp.gt.f32 	%p55, %f313, 0f42FC0000;\n"
"	mov.f32 	%f314, 0f42FC0000;\n"
"	copysign.f32 	%f315, %f312, %f314;\n"
"	selp.f32 	%f316, %f315, %f312, %p55;\n"
"	mov.f32 	%f317, 0fBF317218;\n"
"	fma.rn.f32 	%f318, %f316, %f317, %f309;\n"
"	mov.f32 	%f319, 0f3102E308;\n"
"	fma.rn.f32 	%f320, %f316, %f319, %f318;\n"
"	mul.f32 	%f321, %f320, 0f3FB8AA3B;\n"
"	add.f32 	%f322, %f316, 0f4B40007D;\n"
"	mov.b32 	%r115, %f322;\n"
"	shl.b32 	%r116, %r115, 23;\n"
"	mov.b32 	%f323, %r116;\n"
"	ex2.approx.ftz.f32 	%f324, %f321;\n"
"	mul.f32 	%f325, %f324, %f323;\n"
"	mov.f32 	%f326, 0f3E000000;\n"
"	div.approx.ftz.f32 	%f327, %f326, %f325;\n"
"	mov.f32 	%f328, 0f40000000;\n"
"	fma.rn.f32 	%f529, %f328, %f325, %f327;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_39:\n"
"	setp.eq.s32 	%p5, %r66, 19;\n"
"	@%p5 bra 	$L__BB1_59;\n"
"\n"
"	setp.ne.s32 	%p6, %r66, 20;\n"
"	mov.f32 	%f529, %f1;\n"
"	@%p6 bra 	$L__BB1_94;\n"
"\n"
"	cvt.f64.f32 	%fd1, %f1;\n"
"	abs.f64 	%fd20, %fd1;\n"
"	setp.geu.f64 	%p25, %fd20, 0d7FF0000000000000;\n"
"	mov.f32 	%f529, 0f00000000;\n"
"	@%p25 bra 	$L__BB1_58;\n"
"	bra.uni 	$L__BB1_42;\n"
"\n"
"$L__BB1_58:\n"
"	atom.global.max.u32 	%r100, [%rd1], 2;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_82:\n"
"	mul.f32 	%f497, %f1, 0f3F22F983;\n"
"	cvt.rni.s32.f32 	%r230, %f497;\n"
"	cvt.rn.f32.s32 	%f498, %r230;\n"
"	mov.f32 	%f499, 0fBFC90FDA;\n"
"	fma.rn.f32 	%f500, %f498, %f499, %f1;\n"
"	mov.f32 	%f501, 0fB3A22168;\n"
"	fma.rn.f32 	%f502, %f498, %f501, %f500;\n"
"	mov.f32 	%f503, 0fA7C234C5;\n"
"	fma.rn.f32 	%f526, %f498, %f503, %f502;\n"
"	abs.f32 	%f47, %f1;\n"
"	setp.ltu.f32 	%p86, %f47, 0f47CE4780;\n"
"	@%p86 bra 	$L__BB1_90;\n"
"\n"
"	setp.eq.f32 	%p87, %f47, 0f7F800000;\n"
"	@%p87 bra 	$L__BB1_89;\n"
"	bra.uni 	$L__BB1_84;\n"
"\n"
"$L__BB1_89:\n"
"	mov.f32 	%f506, 0f00000000;\n"
"	mul.rn.f32 	%f526, %f1, %f506;\n"
"	mov.u32 	%r230, 0;\n"
"	bra.uni 	$L__BB1_90;\n"
"\n"
"$L__BB1_65:\n"
"	abs.f32 	%f288, %f1;\n"
"	mul.f32 	%f289, %f288, 0f4038AA3B;\n"
"	ex2.approx.ftz.f32 	%f290, %f289;\n"
"	add.f32 	%f291, %f290, 0f3F800000;\n"
"	mov.f32 	%f292, 0f3F800000;\n"
"	rcp.approx.ftz.f32 	%f293, %f291;\n"
"	mov.f32 	%f294, 0fC0000000;\n"
"	fma.rn.f32 	%f295, %f293, %f294, %f292;\n"
"	setp.ge.f32 	%p53, %f288, 0f41102CB4;\n"
"	selp.f32 	%f296, 0f3F800000, %f295, %p53;\n"
"	copysign.f32 	%f297, %f1, %f296;\n"
"	mul.f32 	%f298, %f1, %f1;\n"
"	mov.f32 	%f299, 0fBD563CAE;\n"
"	mov.f32 	%f300, 0f3C80F082;\n"
"	fma.rn.f32 	%f301, %f300, %f298, %f299;\n"
"	mov.f32 	%f302, 0f3E085941;\n"
"	fma.rn.f32 	%f303, %f301, %f298, %f302;\n"
"	mov.f32 	%f304, 0fBEAAA9ED;\n"
"	fma.rn.f32 	%f305, %f303, %f298, %f304;\n"
"	mov.f32 	%f306, 0f00000000;\n"
"	fma.rn.f32 	%f307, %f305, %f298, %f306;\n"
"	fma.rn.f32 	%f308, %f307, %f1, %f1;\n"
"	setp.ge.f32 	%p54, %f288, 0f3F19999A;\n"
"	selp.f32 	%f529, %f297, %f308, %p54;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_67:\n"
"	abs.f32 	%f384, %f1;\n"
"	neg.f32 	%f385, %f384;\n"
"	mov.f32 	%f386, 0f3F000000;\n"
"	fma.rn.f32 	%f387, %f386, %f385, %f386;\n"
"	rsqrt.approx.ftz.f32 	%f388, %f387;\n"
"	mul.f32 	%f389, %f387, %f388;\n"
"	mul.f32 	%f390, %f388, 0f3F000000;\n"
"	neg.f32 	%f391, %f389;\n"
"	fma.rn.f32 	%f392, %f391, %f390, %f386;\n"
"	fma.rn.f32 	%f393, %f389, %f392, %f389;\n"
"	setp.eq.f32 	%p61, %f384, 0f3F800000;\n"
"	selp.f32 	%f394, 0f00000000, %f393, %p61;\n"
"	setp.gt.f32 	%p62, %f384, 0f3F0F5C29;\n"
"	selp.f32 	%f395, %f394, %f384, %p62;\n"
"	copysign.f32 	%f396, %f1, %f395;\n"
"	mul.f32 	%f397, %f396, %f396;\n"
"	mov.f32 	%f398, 0f3C8B1ABB;\n"
"	mov.f32 	%f399, 0f3D10ECEF;\n"
"	fma.rn.f32 	%f400, %f399, %f397, %f398;\n"
"	mov.f32 	%f401, 0f3CFC028C;\n"
"	fma.rn.f32 	%f402, %f400, %f397, %f401;\n"
"	mov.f32 	%f403, 0f3D372139;\n"
"	fma.rn.f32 	%f404, %f402, %f397, %f403;\n"
"	mov.f32 	%f405, 0f3D9993DB;\n"
"	fma.rn.f32 	%f406, %f404, %f397, %f405;\n"
"	mov.f32 	%f407, 0f3E2AAAC6;\n"
"	fma.rn.f32 	%f408, %f406, %f397, %f407;\n"
"	mul.f32 	%f409, %f397, %f408;\n"
"	fma.rn.f32 	%f410, %f409, %f396, %f396;\n"
"	neg.f32 	%f411, %f410;\n"
"	selp.f32 	%f412, %f410, %f411, %p62;\n"
"	mov.f32 	%f413, 0f3FD774EB;\n"
"	mov.f32 	%f414, 0f3F6EE581;\n"
"	fma.rn.f32 	%f415, %f414, %f413, %f412;\n"
"	setp.gt.f32 	%p63, %f1, 0f3F0F5C29;\n"
"	selp.f32 	%f416, %f410, %f415, %p63;\n"
"	add.f32 	%f417, %f416, %f416;\n"
"	selp.f32 	%f529, %f417, %f416, %p62;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_60:\n"
"	abs.f32 	%f195, %f1;\n"
"	setp.ltu.f32 	%p45, %f195, 0f3F8060FE;\n"
"	setp.ge.f32 	%p46, %f195, 0f3F8060FE;\n"
"	mul.f32 	%f196, %f1, %f1;\n"
"	selp.f32 	%f197, %f195, %f196, %p46;\n"
"	selp.f32 	%f198, 0f38EB4C3A, 0f38B1E96A, %p46;\n"
"	selp.f32 	%f199, 0fBAAE005B, 0fBA574D20, %p46;\n"
"	fma.rn.f32 	%f200, %f198, %f197, %f199;\n"
"	selp.f32 	%f201, 0f3C09919F, 0f3BAAD5EA, %p46;\n"
"	fma.rn.f32 	%f202, %f200, %f197, %f201;\n"
"	selp.f32 	%f203, 0fBD24D99A, 0fBCDC1BE7, %p46;\n"
"	fma.rn.f32 	%f204, %f202, %f197, %f203;\n"
"	selp.f32 	%f205, 0f3E235519, 0f3DE718AF, %p46;\n"
"	fma.rn.f32 	%f206, %f204, %f197, %f205;\n"
"	selp.f32 	%f207, 0f3F69B4F9, 0fBEC093AC, %p46;\n"
"	fma.rn.f32 	%f208, %f206, %f197, %f207;\n"
"	selp.f32 	%f209, 0f3F210A14, 0f3E0375D3, %p46;\n"
"	fma.rn.f32 	%f210, %f208, %f197, %f209;\n"
"	neg.f32 	%f211, %f195;\n"
"	selp.f32 	%f212, %f211, %f1, %p46;\n"
"	fma.rn.f32 	%f529, %f210, %f212, %f212;\n"
"	@%p45 bra 	$L__BB1_94;\n"
"\n"
"	ex2.approx.ftz.f32 	%f213, %f529;\n"
"	mov.f32 	%f214, 0f3F800000;\n"
"	sub.f32 	%f215, %f214, %f213;\n"
"	copysign.f32 	%f529, %f1, %f215;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_68:\n"
"	mul.f32 	%f448, %f1, 0f3F22F983;\n"
"	cvt.rni.s32.f32 	%r222, %f448;\n"
"	cvt.rn.f32.s32 	%f449, %r222;\n"
"	mov.f32 	%f450, 0fBFC90FDA;\n"
"	fma.rn.f32 	%f451, %f449, %f450, %f1;\n"
"	mov.f32 	%f452, 0fB3A22168;\n"
"	fma.rn.f32 	%f453, %f449, %f452, %f451;\n"
"	mov.f32 	%f454, 0fA7C234C5;\n"
"	fma.rn.f32 	%f522, %f449, %f454, %f453;\n"
"	abs.f32 	%f29, %f1;\n"
"	setp.ltu.f32 	%p67, %f29, 0f47CE4780;\n"
"	@%p67 bra 	$L__BB1_76;\n"
"\n"
"	setp.eq.f32 	%p68, %f29, 0f7F800000;\n"
"	@%p68 bra 	$L__BB1_75;\n"
"	bra.uni 	$L__BB1_70;\n"
"\n"
"$L__BB1_75:\n"
"	mov.f32 	%f457, 0f00000000;\n"
"	mul.rn.f32 	%f522, %f1, %f457;\n"
"	mov.u32 	%r222, 0;\n"
"	bra.uni 	$L__BB1_76;\n"
"\n"
"$L__BB1_62:\n"
"	setp.lt.f32 	%p50, %f1, 0f00800000;\n"
"	mul.f32 	%f245, %f1, 0f4B000000;\n"
"	selp.f32 	%f12, %f245, %f1, %p50;\n"
"	selp.f32 	%f246, 0fC1B80000, 0f00000000, %p50;\n"
"	mov.b32 	%r109, %f12;\n"
"	add.s32 	%r110, %r109, -1059760811;\n"
"	and.b32  	%r111, %r110, -8388608;\n"
"	sub.s32 	%r112, %r109, %r111;\n"
"	mov.b32 	%f247, %r112;\n"
"	cvt.rn.f32.s32 	%f248, %r111;\n"
"	mov.f32 	%f249, 0f34000000;\n"
"	fma.rn.f32 	%f250, %f248, %f249, %f246;\n"
"	add.f32 	%f251, %f247, 0fBF800000;\n"
"	mov.f32 	%f252, 0f3E1039F6;\n"
"	mov.f32 	%f253, 0fBE055027;\n"
"	fma.rn.f32 	%f254, %f253, %f251, %f252;\n"
"	mov.f32 	%f255, 0fBDF8CDCC;\n"
"	fma.rn.f32 	%f256, %f254, %f251, %f255;\n"
"	mov.f32 	%f257, 0f3E0F2955;\n"
"	fma.rn.f32 	%f258, %f256, %f251, %f257;\n"
"	mov.f32 	%f259, 0fBE2AD8B9;\n"
"	fma.rn.f32 	%f260, %f258, %f251, %f259;\n"
"	mov.f32 	%f261, 0f3E4CED0B;\n"
"	fma.rn.f32 	%f262, %f260, %f251, %f261;\n"
"	mov.f32 	%f263, 0fBE7FFF22;\n"
"	fma.rn.f32 	%f264, %f262, %f251, %f263;\n"
"	mov.f32 	%f265, 0f3EAAAA78;\n"
"	fma.rn.f32 	%f266, %f264, %f251, %f265;\n"
"	mov.f32 	%f267, 0fBF000000;\n"
"	fma.rn.f32 	%f268, %f266, %f251, %f267;\n"
"	mul.f32 	%f269, %f251, %f268;\n"
"	fma.rn.f32 	%f270, %f269, %f251, %f251;\n"
"	mov.f32 	%f271, 0f3F317218;\n"
"	fma.rn.f32 	%f520, %f250, %f271, %f270;\n"
"	setp.lt.u32 	%p51, %r109, 2139095040;\n"
"	@%p51 bra 	$L__BB1_64;\n"
"\n"
"	mov.f32 	%f272, 0f7F800000;\n"
"	fma.rn.f32 	%f520, %f12, %f272, %f272;\n"
"\n"
"$L__BB1_64:\n"
"	setp.eq.f32 	%p52, %f12, 0f00000000;\n"
"	selp.f32 	%f529, 0fFF800000, %f520, %p52;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_66:\n"
"	abs.f32 	%f329, %f1;\n"
"	mov.f32 	%f330, 0f3FB8AA3B;\n"
"	mul.rn.f32 	%f331, %f329, %f330;\n"
"	cvt.rzi.f32.f32 	%f332, %f331;\n"
"	abs.f32 	%f333, %f332;\n"
"	setp.gt.f32 	%p56, %f333, 0f42FC0000;\n"
"	mov.f32 	%f334, 0f42FC0000;\n"
"	copysign.f32 	%f335, %f332, %f334;\n"
"	selp.f32 	%f336, %f335, %f332, %p56;\n"
"	mov.f32 	%f337, 0fBF317218;\n"
"	fma.rn.f32 	%f338, %f336, %f337, %f329;\n"
"	mov.f32 	%f339, 0f3102E308;\n"
"	fma.rn.f32 	%f340, %f336, %f339, %f338;\n"
"	mul.f32 	%f341, %f340, 0f3FB8AA3B;\n"
"	add.f32 	%f342, %f336, 0f4B40007D;\n"
"	mov.b32 	%r117, %f342;\n"
"	shl.b32 	%r118, %r117, 23;\n"
"	mov.b32 	%f343, %r118;\n"
"	ex2.approx.ftz.f32 	%f344, %f341;\n"
"	mul.f32 	%f345, %f344, %f343;\n"
"	mov.f32 	%f346, 0fBE000000;\n"
"	div.approx.ftz.f32 	%f347, %f346, %f345;\n"
"	mov.f32 	%f348, 0f40000000;\n"
"	fma.rn.f32 	%f349, %f348, %f345, %f347;\n"
"	mul.f32 	%f350, %f1, %f1;\n"
"	mov.f32 	%f351, 0f394FFF49;\n"
"	mov.f32 	%f352, 0f363D0ADA;\n"
"	fma.rn.f32 	%f353, %f352, %f350, %f351;\n"
"	mov.f32 	%f354, 0f3C08889A;\n"
"	fma.rn.f32 	%f355, %f353, %f350, %f354;\n"
"	mov.f32 	%f356, 0f3E2AAAAB;\n"
"	fma.rn.f32 	%f357, %f355, %f350, %f356;\n"
"	mul.f32 	%f358, %f350, %f357;\n"
"	fma.rn.f32 	%f359, %f358, %f1, %f1;\n"
"	setp.ge.f32 	%p57, %f329, 0f3F800000;\n"
"	copysign.f32 	%f360, %f1, %f349;\n"
"	selp.f32 	%f529, %f360, %f359, %p57;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_59:\n"
"	mul.f32 	%f63, %f1, 0fBF3504F3;\n"
"	abs.f32 	%f64, %f63;\n"
"	add.f32 	%f65, %f64, 0fC0800000;\n"
"	mov.f32 	%f66, 0fC0800000;\n"
"	add.f32 	%f67, %f64, 0f40800000;\n"
"	rcp.approx.ftz.f32 	%f68, %f67;\n"
"	mul.rn.f32 	%f69, %f65, %f68;\n"
"	add.f32 	%f70, %f69, 0f3F800000;\n"
"	mov.f32 	%f71, 0f3F800000;\n"
"	fma.rn.f32 	%f72, %f66, %f70, %f64;\n"
"	neg.f32 	%f73, %f69;\n"
"	fma.rn.f32 	%f74, %f73, %f64, %f72;\n"
"	fma.rn.f32 	%f75, %f68, %f74, %f69;\n"
"	mov.f32 	%f76, 0f3BE6E05B;\n"
"	mov.f32 	%f77, 0f3A69A091;\n"
"	fma.rn.f32 	%f78, %f77, %f75, %f76;\n"
"	mov.f32 	%f79, 0fBC81FB4B;\n"
"	fma.rn.f32 	%f80, %f78, %f75, %f79;\n"
"	mov.f32 	%f81, 0f3D15373B;\n"
"	fma.rn.f32 	%f82, %f80, %f75, %f81;\n"
"	mov.f32 	%f83, 0fBD887C5A;\n"
"	fma.rn.f32 	%f84, %f82, %f75, %f83;\n"
"	mov.f32 	%f85, 0f3DC021D5;\n"
"	fma.rn.f32 	%f86, %f84, %f75, %f85;\n"
"	mov.f32 	%f87, 0fBDCED424;\n"
"	fma.rn.f32 	%f88, %f86, %f75, %f87;\n"
"	mov.f32 	%f89, 0f3D8B74DE;\n"
"	fma.rn.f32 	%f90, %f88, %f75, %f89;\n"
"	mov.f32 	%f91, 0f3C7BF170;\n"
"	fma.rn.f32 	%f92, %f90, %f75, %f91;\n"
"	mov.f32 	%f93, 0fBE0EF8D4;\n"
"	fma.rn.f32 	%f94, %f92, %f75, %f93;\n"
"	mov.f32 	%f95, 0f3F9DD2C9;\n"
"	fma.rn.f32 	%f96, %f94, %f75, %f95;\n"
"	mov.f32 	%f97, 0f40000000;\n"
"	fma.rn.f32 	%f98, %f97, %f64, %f71;\n"
"	rcp.approx.ftz.f32 	%f99, %f98;\n"
"	mul.rn.f32 	%f100, %f96, %f99;\n"
"	mul.f32 	%f101, %f100, 0fC0000000;\n"
"	fma.rn.f32 	%f102, %f64, %f101, %f96;\n"
"	sub.f32 	%f103, %f102, %f100;\n"
"	fma.rn.f32 	%f104, %f103, %f99, %f100;\n"
"	mul.f32 	%f105, %f64, %f64;\n"
"	neg.f32 	%f106, %f105;\n"
"	mov.f32 	%f107, 0f3FB8AA3B;\n"
"	mul.rn.f32 	%f108, %f106, %f107;\n"
"	cvt.rzi.f32.f32 	%f109, %f108;\n"
"	abs.f32 	%f110, %f109;\n"
"	setp.gt.f32 	%p39, %f110, 0f42FC0000;\n"
"	mov.f32 	%f111, 0f42FC0000;\n"
"	copysign.f32 	%f112, %f109, %f111;\n"
"	selp.f32 	%f113, %f112, %f109, %p39;\n"
"	mov.f32 	%f114, 0fBF317218;\n"
"	fma.rn.f32 	%f115, %f113, %f114, %f106;\n"
"	mov.f32 	%f116, 0f3102E308;\n"
"	fma.rn.f32 	%f117, %f113, %f116, %f115;\n"
"	mul.f32 	%f118, %f117, 0f3FB8AA3B;\n"
"	add.f32 	%f119, %f113, 0f4B40007F;\n"
"	mov.b32 	%r101, %f119;\n"
"	shl.b32 	%r102, %r101, 23;\n"
"	mov.b32 	%f120, %r102;\n"
"	ex2.approx.ftz.f32 	%f121, %f118;\n"
"	mul.f32 	%f122, %f121, %f120;\n"
"	neg.f32 	%f123, %f64;\n"
"	fma.rn.f32 	%f124, %f123, %f64, %f105;\n"
"	fma.rn.f32 	%f125, %f122, %f124, %f122;\n"
"	mul.f32 	%f126, %f104, %f125;\n"
"	setp.gt.f32 	%p40, %f64, 0f4120E148;\n"
"	selp.f32 	%f127, 0f00000000, %f126, %p40;\n"
"	setp.lt.f32 	%p41, %f63, 0f00000000;\n"
"	sub.f32 	%f128, %f97, %f127;\n"
"	selp.f32 	%f129, %f128, %f127, %p41;\n"
"	mul.f32 	%f529, %f129, 0f3F000000;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_42:\n"
"	setp.leu.f32 	%p26, %f1, 0f00000000;\n"
"	setp.geu.f32 	%p27, %f1, 0f3F800000;\n"
"	or.pred  	%p28, %p27, %p26;\n"
"	@%p28 bra 	$L__BB1_57;\n"
"	bra.uni 	$L__BB1_43;\n"
"\n"
"$L__BB1_57:\n"
"	atom.global.max.u32 	%r99, [%rd1], 1;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_43:\n"
"	add.f64 	%fd2, %fd1, %fd1;\n"
"	neg.f64 	%fd3, %fd2;\n"
"	mov.f64 	%fd21, 0d4000000000000000;\n"
"	add.rn.f64 	%fd4, %fd21, %fd3;\n"
"	setp.ge.f64 	%p29, %fd2, 0d3F4FA4D2AD8F904D;\n"
"	setp.le.f64 	%p30, %fd2, 0d3FFFFC0B65AA4E0E;\n"
"	and.pred  	%p31, %p29, %p30;\n"
"	@%p31 bra 	$L__BB1_55;\n"
"	bra.uni 	$L__BB1_44;\n"
"\n"
"$L__BB1_55:\n"
"	mul.rn.f64 	%fd174, %fd4, %fd2;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r90}, %fd174;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r91, %temp}, %fd174;\n"
"	}\n"
"	shr.u32 	%r92, %r90, 20;\n"
"	and.b32  	%r93, %r92, 2046;\n"
"	add.s32 	%r94, %r93, 2147482626;\n"
"	mov.u32 	%r95, 1127219200;\n"
"	mov.b64 	%fd175, {%r94, %r95};\n"
"	mov.u32 	%r96, -2147483648;\n"
"	mov.b64 	%fd176, {%r96, %r95};\n"
"	sub.f64 	%fd177, %fd175, %fd176;\n"
"	and.b32  	%r97, %r90, -2145386497;\n"
"	or.b32  	%r98, %r97, 1071644672;\n"
"	mov.b64 	%fd178, {%r91, %r98};\n"
"	add.f64 	%fd179, %fd178, 0dBFF0000000000000;\n"
"	add.f64 	%fd180, %fd178, 0d3FF0000000000000;\n"
"	mov.f64 	%fd181, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd182, %fd180;\n"
"	neg.f64 	%fd183, %fd180;\n"
"	fma.rn.f64 	%fd184, %fd183, %fd182, %fd181;\n"
"	fma.rn.f64 	%fd185, %fd184, %fd184, %fd184;\n"
"	fma.rn.f64 	%fd186, %fd185, %fd182, %fd182;\n"
"	mul.f64 	%fd187, %fd179, %fd186;\n"
"	mov.f64 	%fd188, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd189, %fd188, %fd187, %fd179;\n"
"	neg.f64 	%fd190, %fd187;\n"
"	fma.rn.f64 	%fd191, %fd190, %fd179, %fd189;\n"
"	fma.rn.f64 	%fd192, %fd191, %fd186, %fd187;\n"
"	mul.f64 	%fd193, %fd192, %fd192;\n"
"	mov.f64 	%fd194, 0d3FA55CF59CDC5D89;\n"
"	mov.f64 	%fd195, 0d3FB5C5C218C775C9;\n"
"	fma.rn.f64 	%fd196, %fd195, %fd193, %fd194;\n"
"	mov.f64 	%fd197, 0d3FAEFD18CF6EBB9C;\n"
"	fma.rn.f64 	%fd198, %fd196, %fd193, %fd197;\n"
"	mov.f64 	%fd199, 0d3FB10682EDCB8D1B;\n"
"	fma.rn.f64 	%fd200, %fd198, %fd193, %fd199;\n"
"	mov.f64 	%fd201, 0d3FB3B1DD3AC7FC96;\n"
"	fma.rn.f64 	%fd202, %fd200, %fd193, %fd201;\n"
"	mov.f64 	%fd203, 0d3FB745CB459B54A6;\n"
"	fma.rn.f64 	%fd204, %fd202, %fd193, %fd203;\n"
"	mov.f64 	%fd205, 0d3FBC71C741A0669F;\n"
"	fma.rn.f64 	%fd206, %fd204, %fd193, %fd205;\n"
"	mov.f64 	%fd207, 0d3FC249249209112E;\n"
"	fma.rn.f64 	%fd208, %fd206, %fd193, %fd207;\n"
"	mov.f64 	%fd209, 0d3FC99999999A06C1;\n"
"	fma.rn.f64 	%fd210, %fd208, %fd193, %fd209;\n"
"	mov.f64 	%fd211, 0d3FD5555555555535;\n"
"	fma.rn.f64 	%fd212, %fd210, %fd193, %fd211;\n"
"	mul.f64 	%fd213, %fd193, %fd212;\n"
"	fma.rn.f64 	%fd214, %fd213, %fd192, %fd192;\n"
"	add.f64 	%fd215, %fd214, %fd214;\n"
"	mov.f64 	%fd216, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd217, %fd177, %fd216, %fd215;\n"
"	mov.f64 	%fd218, 0dC009000000000000;\n"
"	sub.f64 	%fd219, %fd218, %fd217;\n"
"	mov.f64 	%fd220, 0dBC08DDF93324D327;\n"
"	mov.f64 	%fd221, 0dBBB135D2E746E627;\n"
"	fma.rn.f64 	%fd222, %fd221, %fd219, %fd220;\n"
"	mov.f64 	%fd223, 0d3C37B83EEF0B7C9F;\n"
"	fma.rn.f64 	%fd224, %fd222, %fd219, %fd223;\n"
"	mov.f64 	%fd225, 0d3C69BA72CD589B91;\n"
"	fma.rn.f64 	%fd226, %fd224, %fd219, %fd225;\n"
"	mov.f64 	%fd227, 0dBCA33689090A6B96;\n"
"	fma.rn.f64 	%fd228, %fd226, %fd219, %fd227;\n"
"	mov.f64 	%fd229, 0d3C782E11898132E0;\n"
"	fma.rn.f64 	%fd230, %fd228, %fd219, %fd229;\n"
"	mov.f64 	%fd231, 0d3CFDE4ACFD9E26BA;\n"
"	fma.rn.f64 	%fd232, %fd230, %fd219, %fd231;\n"
"	mov.f64 	%fd233, 0dBD26D33EED66C487;\n"
"	fma.rn.f64 	%fd234, %fd232, %fd219, %fd233;\n"
"	mov.f64 	%fd235, 0dBD36F2167040D8E2;\n"
"	fma.rn.f64 	%fd236, %fd234, %fd219, %fd235;\n"
"	mov.f64 	%fd237, 0d3D872A22C2D77E20;\n"
"	fma.rn.f64 	%fd238, %fd236, %fd219, %fd237;\n"
"	mov.f64 	%fd239, 0dBDAC8859C4E5C0AF;\n"
"	fma.rn.f64 	%fd240, %fd238, %fd219, %fd239;\n"
"	mov.f64 	%fd241, 0dBDCDC583D118A561;\n"
"	fma.rn.f64 	%fd242, %fd240, %fd219, %fd241;\n"
"	mov.f64 	%fd243, 0d3E120F47CCF46B3C;\n"
"	fma.rn.f64 	%fd244, %fd242, %fd219, %fd243;\n"
"	mov.f64 	%fd245, 0dBE31A9E38DC84D60;\n"
"	fma.rn.f64 	%fd246, %fd244, %fd219, %fd245;\n"
"	mov.f64 	%fd247, 0dBE5F36CD6D3D46A9;\n"
"	fma.rn.f64 	%fd248, %fd246, %fd219, %fd247;\n"
"	mov.f64 	%fd249, 0d3E9C6B4F5D03B787;\n"
"	fma.rn.f64 	%fd250, %fd248, %fd219, %fd249;\n"
"	mov.f64 	%fd251, 0dBEB6E8A5434AE8A2;\n"
"	fma.rn.f64 	%fd252, %fd250, %fd219, %fd251;\n"
"	mov.f64 	%fd253, 0dBEED1D1F7B8736F6;\n"
"	fma.rn.f64 	%fd254, %fd252, %fd219, %fd253;\n"
"	mov.f64 	%fd255, 0d3F2879C2A212F024;\n"
"	fma.rn.f64 	%fd256, %fd254, %fd219, %fd255;\n"
"	mov.f64 	%fd257, 0dBF4845769484FCA8;\n"
"	fma.rn.f64 	%fd258, %fd256, %fd219, %fd257;\n"
"	mov.f64 	%fd259, 0dBF78B6C33114F909;\n"
"	fma.rn.f64 	%fd260, %fd258, %fd219, %fd259;\n"
"	mov.f64 	%fd261, 0d3FCEBD80D9B13E28;\n"
"	fma.rn.f64 	%fd262, %fd260, %fd219, %fd261;\n"
"	mov.f64 	%fd263, 0d3FFA755E7C99AE86;\n"
"	fma.rn.f64 	%fd264, %fd262, %fd219, %fd263;\n"
"	fma.rn.f64 	%fd280, %fd264, %fd3, %fd264;\n"
"	bra.uni 	$L__BB1_56;\n"
"\n"
"$L__BB1_8:\n"
"	mov.b32 	%r30, %f1;\n"
"	shr.u32 	%r151, %r30, 23;\n"
"	and.b32  	%r152, %r151, 255;\n"
"	add.s32 	%r31, %r152, -128;\n"
"	shl.b32 	%r153, %r30, 8;\n"
"	or.b32  	%r32, %r153, -2147483648;\n"
"	shr.u32 	%r33, %r31, 5;\n"
"	mov.u64 	%rd76, 0;\n"
"	mov.u32 	%r223, 0;\n"
"	mov.u64 	%rd75, __cudart_i2opi_f;\n"
"	mov.u64 	%rd74, %rd2;\n"
"\n"
"$L__BB1_9:\n"
"	.pragma \"nounroll\";\n"
"	ld.global.nc.u32 	%r154, [%rd75];\n"
"	mad.wide.u32 	%rd45, %r154, %r32, %rd76;\n"
"	shr.u64 	%rd76, %rd45, 32;\n"
"	st.local.u32 	[%rd74], %rd45;\n"
"	add.s64 	%rd75, %rd75, 4;\n"
"	add.s64 	%rd74, %rd74, 4;\n"
"	add.s32 	%r223, %r223, 1;\n"
"	setp.ne.s32 	%p78, %r223, 6;\n"
"	@%p78 bra 	$L__BB1_9;\n"
"\n"
"	st.local.u32 	[%rd4], %rd76;\n"
"	mov.u32 	%r155, 4;\n"
"	sub.s32 	%r36, %r155, %r33;\n"
"	mov.u32 	%r156, 6;\n"
"	sub.s32 	%r157, %r156, %r33;\n"
"	mul.wide.s32 	%rd46, %r157, 4;\n"
"	add.s64 	%rd47, %rd2, %rd46;\n"
"	ld.local.u32 	%r224, [%rd47];\n"
"	ld.local.u32 	%r225, [%rd47+-4];\n"
"	and.b32  	%r39, %r31, 31;\n"
"	setp.eq.s32 	%p79, %r39, 0;\n"
"	@%p79 bra 	$L__BB1_12;\n"
"\n"
"	mov.u32 	%r158, 32;\n"
"	sub.s32 	%r159, %r158, %r39;\n"
"	shr.u32 	%r160, %r225, %r159;\n"
"	shl.b32 	%r161, %r224, %r39;\n"
"	add.s32 	%r224, %r160, %r161;\n"
"	mul.wide.s32 	%rd48, %r36, 4;\n"
"	add.s64 	%rd49, %rd2, %rd48;\n"
"	ld.local.u32 	%r162, [%rd49];\n"
"	shr.u32 	%r163, %r162, %r159;\n"
"	shl.b32 	%r164, %r225, %r39;\n"
"	add.s32 	%r225, %r163, %r164;\n"
"\n"
"$L__BB1_12:\n"
"	and.b32  	%r165, %r30, -2147483648;\n"
"	shr.u32 	%r166, %r225, 30;\n"
"	shl.b32 	%r167, %r224, 2;\n"
"	or.b32  	%r168, %r166, %r167;\n"
"	shr.u32 	%r169, %r168, 31;\n"
"	shr.u32 	%r170, %r224, 30;\n"
"	add.s32 	%r171, %r169, %r170;\n"
"	neg.s32 	%r172, %r171;\n"
"	setp.eq.s32 	%p80, %r165, 0;\n"
"	selp.b32 	%r226, %r171, %r172, %p80;\n"
"	setp.ne.s32 	%p81, %r169, 0;\n"
"	xor.b32  	%r173, %r165, -2147483648;\n"
"	selp.b32 	%r174, %r173, %r165, %p81;\n"
"	selp.b32 	%r175, -1, 0, %p81;\n"
"	xor.b32  	%r176, %r168, %r175;\n"
"	shl.b32 	%r177, %r225, 2;\n"
"	xor.b32  	%r178, %r177, %r175;\n"
"	cvt.u64.u32 	%rd50, %r176;\n"
"	cvt.u64.u32 	%rd51, %r178;\n"
"	bfi.b64 	%rd52, %rd50, %rd51, 32, 32;\n"
"	cvt.rn.f64.s64 	%fd272, %rd52;\n"
"	mul.f64 	%fd273, %fd272, 0d3BF921FB54442D19;\n"
"	cvt.rn.f32.f64 	%f483, %fd273;\n"
"	setp.eq.s32 	%p82, %r174, 0;\n"
"	neg.f32 	%f484, %f483;\n"
"	selp.f32 	%f523, %f483, %f484, %p82;\n"
"\n"
"$L__BB1_78:\n"
"	add.s32 	%r46, %r226, 1;\n"
"	and.b32  	%r47, %r46, 1;\n"
"	setp.eq.s32 	%p83, %r47, 0;\n"
"	selp.f32 	%f39, %f523, 0f3F800000, %p83;\n"
"	mul.rn.f32 	%f40, %f523, %f523;\n"
"	mov.f32 	%f524, 0fB94D4153;\n"
"	@%p83 bra 	$L__BB1_80;\n"
"\n"
"	mov.f32 	%f487, 0fBAB607ED;\n"
"	mov.f32 	%f488, 0f37CBAC00;\n"
"	fma.rn.f32 	%f524, %f488, %f40, %f487;\n"
"\n"
"$L__BB1_80:\n"
"	selp.f32 	%f489, 0f3C0885E4, 0f3D2AAABB, %p83;\n"
"	fma.rn.f32 	%f490, %f524, %f40, %f489;\n"
"	selp.f32 	%f491, 0fBE2AAAA8, 0fBEFFFFFF, %p83;\n"
"	fma.rn.f32 	%f492, %f490, %f40, %f491;\n"
"	mov.f32 	%f493, 0f00000000;\n"
"	fma.rn.f32 	%f494, %f40, %f39, %f493;\n"
"	fma.rn.f32 	%f529, %f492, %f494, %f39;\n"
"	and.b32  	%r180, %r46, 2;\n"
"	setp.eq.s32 	%p85, %r180, 0;\n"
"	@%p85 bra 	$L__BB1_94;\n"
"\n"
"	mov.f32 	%f496, 0fBF800000;\n"
"	fma.rn.f32 	%f529, %f529, %f496, %f493;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_84:\n"
"	mov.b32 	%r49, %f1;\n"
"	shr.u32 	%r182, %r49, 23;\n"
"	and.b32  	%r183, %r182, 255;\n"
"	add.s32 	%r50, %r183, -128;\n"
"	shl.b32 	%r184, %r49, 8;\n"
"	or.b32  	%r51, %r184, -2147483648;\n"
"	shr.u32 	%r52, %r50, 5;\n"
"	mov.u64 	%rd79, 0;\n"
"	mov.u32 	%r227, 0;\n"
"	mov.u64 	%rd78, __cudart_i2opi_f;\n"
"	mov.u64 	%rd77, %rd2;\n"
"\n"
"$L__BB1_85:\n"
"	.pragma \"nounroll\";\n"
"	ld.global.nc.u32 	%r185, [%rd78];\n"
"	mad.wide.u32 	%rd55, %r185, %r51, %rd79;\n"
"	shr.u64 	%rd79, %rd55, 32;\n"
"	st.local.u32 	[%rd77], %rd55;\n"
"	add.s64 	%rd78, %rd78, 4;\n"
"	add.s64 	%rd77, %rd77, 4;\n"
"	add.s32 	%r227, %r227, 1;\n"
"	setp.ne.s32 	%p88, %r227, 6;\n"
"	@%p88 bra 	$L__BB1_85;\n"
"\n"
"	st.local.u32 	[%rd4], %rd79;\n"
"	mov.u32 	%r186, 4;\n"
"	sub.s32 	%r55, %r186, %r52;\n"
"	mov.u32 	%r187, 6;\n"
"	sub.s32 	%r188, %r187, %r52;\n"
"	mul.wide.s32 	%rd56, %r188, 4;\n"
"	add.s64 	%rd57, %rd2, %rd56;\n"
"	ld.local.u32 	%r228, [%rd57];\n"
"	ld.local.u32 	%r229, [%rd57+-4];\n"
"	and.b32  	%r58, %r50, 31;\n"
"	setp.eq.s32 	%p89, %r58, 0;\n"
"	@%p89 bra 	$L__BB1_88;\n"
"\n"
"	mov.u32 	%r189, 32;\n"
"	sub.s32 	%r190, %r189, %r58;\n"
"	shr.u32 	%r191, %r229, %r190;\n"
"	shl.b32 	%r192, %r228, %r58;\n"
"	add.s32 	%r228, %r191, %r192;\n"
"	mul.wide.s32 	%rd58, %r55, 4;\n"
"	add.s64 	%rd59, %rd2, %rd58;\n"
"	ld.local.u32 	%r193, [%rd59];\n"
"	shr.u32 	%r194, %r193, %r190;\n"
"	shl.b32 	%r195, %r229, %r58;\n"
"	add.s32 	%r229, %r194, %r195;\n"
"\n"
"$L__BB1_88:\n"
"	and.b32  	%r196, %r49, -2147483648;\n"
"	shr.u32 	%r197, %r229, 30;\n"
"	shl.b32 	%r198, %r228, 2;\n"
"	or.b32  	%r199, %r197, %r198;\n"
"	shr.u32 	%r200, %r199, 31;\n"
"	shr.u32 	%r201, %r228, 30;\n"
"	add.s32 	%r202, %r200, %r201;\n"
"	neg.s32 	%r203, %r202;\n"
"	setp.eq.s32 	%p90, %r196, 0;\n"
"	selp.b32 	%r230, %r202, %r203, %p90;\n"
"	setp.ne.s32 	%p91, %r200, 0;\n"
"	xor.b32  	%r204, %r196, -2147483648;\n"
"	selp.b32 	%r205, %r204, %r196, %p91;\n"
"	selp.b32 	%r206, -1, 0, %p91;\n"
"	xor.b32  	%r207, %r199, %r206;\n"
"	shl.b32 	%r208, %r229, 2;\n"
"	xor.b32  	%r209, %r208, %r206;\n"
"	cvt.u64.u32 	%rd60, %r207;\n"
"	cvt.u64.u32 	%rd61, %r209;\n"
"	bfi.b64 	%rd62, %rd60, %rd61, 32, 32;\n"
"	cvt.rn.f64.s64 	%fd274, %rd62;\n"
"	mul.f64 	%fd275, %fd274, 0d3BF921FB54442D19;\n"
"	cvt.rn.f32.f64 	%f504, %fd275;\n"
"	setp.eq.s32 	%p92, %r205, 0;\n"
"	neg.f32 	%f505, %f504;\n"
"	selp.f32 	%f526, %f504, %f505, %p92;\n"
"\n"
"$L__BB1_90:\n"
"	and.b32  	%r65, %r230, 1;\n"
"	setp.eq.s32 	%p93, %r65, 0;\n"
"	selp.f32 	%f51, %f526, 0f3F800000, %p93;\n"
"	mul.rn.f32 	%f52, %f526, %f526;\n"
"	mov.f32 	%f527, 0fB94D4153;\n"
"	@%p93 bra 	$L__BB1_92;\n"
"\n"
"	mov.f32 	%f508, 0fBAB607ED;\n"
"	mov.f32 	%f509, 0f37CBAC00;\n"
"	fma.rn.f32 	%f527, %f509, %f52, %f508;\n"
"\n"
"$L__BB1_92:\n"
"	selp.f32 	%f510, 0f3C0885E4, 0f3D2AAABB, %p93;\n"
"	fma.rn.f32 	%f511, %f527, %f52, %f510;\n"
"	selp.f32 	%f512, 0fBE2AAAA8, 0fBEFFFFFF, %p93;\n"
"	fma.rn.f32 	%f513, %f511, %f52, %f512;\n"
"	mov.f32 	%f514, 0f00000000;\n"
"	fma.rn.f32 	%f515, %f52, %f51, %f514;\n"
"	fma.rn.f32 	%f529, %f513, %f515, %f51;\n"
"	and.b32  	%r211, %r230, 2;\n"
"	setp.eq.s32 	%p95, %r211, 0;\n"
"	@%p95 bra 	$L__BB1_94;\n"
"\n"
"	mov.f32 	%f517, 0fBF800000;\n"
"	fma.rn.f32 	%f529, %f529, %f517, %f514;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"$L__BB1_70:\n"
"	mov.b32 	%r13, %f1;\n"
"	shr.u32 	%r120, %r13, 23;\n"
"	and.b32  	%r121, %r120, 255;\n"
"	add.s32 	%r14, %r121, -128;\n"
"	shl.b32 	%r122, %r13, 8;\n"
"	or.b32  	%r15, %r122, -2147483648;\n"
"	shr.u32 	%r16, %r14, 5;\n"
"	mov.u64 	%rd73, 0;\n"
"	mov.u32 	%r219, 0;\n"
"	mov.u64 	%rd72, __cudart_i2opi_f;\n"
"	mov.u64 	%rd71, %rd2;\n"
"\n"
"$L__BB1_71:\n"
"	.pragma \"nounroll\";\n"
"	ld.global.nc.u32 	%r123, [%rd72];\n"
"	mad.wide.u32 	%rd35, %r123, %r15, %rd73;\n"
"	shr.u64 	%rd73, %rd35, 32;\n"
"	st.local.u32 	[%rd71], %rd35;\n"
"	add.s64 	%rd72, %rd72, 4;\n"
"	add.s64 	%rd71, %rd71, 4;\n"
"	add.s32 	%r219, %r219, 1;\n"
"	setp.ne.s32 	%p69, %r219, 6;\n"
"	@%p69 bra 	$L__BB1_71;\n"
"\n"
"	st.local.u32 	[%rd4], %rd73;\n"
"	mov.u32 	%r124, 4;\n"
"	sub.s32 	%r19, %r124, %r16;\n"
"	mov.u32 	%r125, 6;\n"
"	sub.s32 	%r126, %r125, %r16;\n"
"	mul.wide.s32 	%rd36, %r126, 4;\n"
"	add.s64 	%rd37, %rd2, %rd36;\n"
"	ld.local.u32 	%r220, [%rd37];\n"
"	ld.local.u32 	%r221, [%rd37+-4];\n"
"	and.b32  	%r22, %r14, 31;\n"
"	setp.eq.s32 	%p70, %r22, 0;\n"
"	@%p70 bra 	$L__BB1_74;\n"
"\n"
"	mov.u32 	%r127, 32;\n"
"	sub.s32 	%r128, %r127, %r22;\n"
"	shr.u32 	%r129, %r221, %r128;\n"
"	shl.b32 	%r130, %r220, %r22;\n"
"	add.s32 	%r220, %r129, %r130;\n"
"	mul.wide.s32 	%rd38, %r19, 4;\n"
"	add.s64 	%rd39, %rd2, %rd38;\n"
"	ld.local.u32 	%r131, [%rd39];\n"
"	shr.u32 	%r132, %r131, %r128;\n"
"	shl.b32 	%r133, %r221, %r22;\n"
"	add.s32 	%r221, %r132, %r133;\n"
"\n"
"$L__BB1_74:\n"
"	and.b32  	%r134, %r13, -2147483648;\n"
"	shr.u32 	%r135, %r221, 30;\n"
"	shl.b32 	%r136, %r220, 2;\n"
"	or.b32  	%r137, %r135, %r136;\n"
"	shr.u32 	%r138, %r137, 31;\n"
"	shr.u32 	%r139, %r220, 30;\n"
"	add.s32 	%r140, %r138, %r139;\n"
"	neg.s32 	%r141, %r140;\n"
"	setp.eq.s32 	%p71, %r134, 0;\n"
"	selp.b32 	%r222, %r140, %r141, %p71;\n"
"	setp.ne.s32 	%p72, %r138, 0;\n"
"	xor.b32  	%r142, %r134, -2147483648;\n"
"	selp.b32 	%r143, %r142, %r134, %p72;\n"
"	selp.b32 	%r144, -1, 0, %p72;\n"
"	xor.b32  	%r145, %r137, %r144;\n"
"	shl.b32 	%r146, %r221, 2;\n"
"	xor.b32  	%r147, %r146, %r144;\n"
"	cvt.u64.u32 	%rd40, %r145;\n"
"	cvt.u64.u32 	%rd41, %r147;\n"
"	bfi.b64 	%rd42, %rd40, %rd41, 32, 32;\n"
"	cvt.rn.f64.s64 	%fd270, %rd42;\n"
"	mul.f64 	%fd271, %fd270, 0d3BF921FB54442D19;\n"
"	cvt.rn.f32.f64 	%f455, %fd271;\n"
"	setp.eq.s32 	%p73, %r143, 0;\n"
"	neg.f32 	%f456, %f455;\n"
"	selp.f32 	%f522, %f455, %f456, %p73;\n"
"\n"
"$L__BB1_76:\n"
"	mul.f32 	%f458, %f522, %f522;\n"
"	mov.f32 	%f459, 0f3B560000;\n"
"	mov.f32 	%f460, 0f3C190000;\n"
"	fma.rn.f32 	%f461, %f460, %f458, %f459;\n"
"	mov.f32 	%f462, 0f3CC70000;\n"
"	fma.rn.f32 	%f463, %f461, %f458, %f462;\n"
"	mov.f32 	%f464, 0f3D5B0000;\n"
"	fma.rn.f32 	%f465, %f463, %f458, %f464;\n"
"	mov.f32 	%f466, 0f3E089438;\n"
"	fma.rn.f32 	%f467, %f465, %f458, %f466;\n"
"	mov.f32 	%f468, 0f3EAAAA88;\n"
"	fma.rn.f32 	%f469, %f467, %f458, %f468;\n"
"	mul.rn.f32 	%f470, %f458, %f522;\n"
"	fma.rn.f32 	%f471, %f469, %f470, %f522;\n"
"	abs.f32 	%f472, %f522;\n"
"	setp.eq.f32 	%p74, %f472, 0f3A00B43C;\n"
"	selp.f32 	%f473, %f522, %f471, %p74;\n"
"	and.b32  	%r149, %r222, 1;\n"
"	setp.eq.b32 	%p75, %r149, 1;\n"
"	neg.f32 	%f474, %f473;\n"
"	rcp.approx.ftz.f32 	%f475, %f474;\n"
"	selp.f32 	%f529, %f475, %f473, %p75;\n"
"\n"
"$L__BB1_94:\n"
"	mov.u32 	%r214, %ntid.x;\n"
"	mov.u32 	%r213, %ctaid.x;\n"
"	mov.u32 	%r212, %tid.x;\n"
"	cvt.u64.u32 	%rd70, %r212;\n"
"	mul.wide.u32 	%rd69, %r213, %r214;\n"
"	add.s64 	%rd68, %rd69, %rd70;\n"
"	shl.b64 	%rd67, %rd68, 2;\n"
"	ld.param.u64 	%rd66, [omni_cuda_map_scientific_f32_param_3];\n"
"	cvta.to.global.u64 	%rd63, %rd66;\n"
"	add.s64 	%rd65, %rd63, %rd67;\n"
"	st.global.f32 	[%rd65], %f529;\n"
"\n"
"$L__BB1_95:\n"
"	ret;\n"
"\n"
"$L__BB1_44:\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r1}, %fd2;\n"
"	}\n"
"	setp.gt.s32 	%p32, %r1, 1072693247;\n"
"	selp.f64 	%fd276, %fd4, %fd2, %p32;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r215}, %fd276;\n"
"	}\n"
"	mov.b32 	%f59, %r215;\n"
"	setp.ltu.f32 	%p33, %f59, 0f2B2BFF2F;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r216, %temp}, %fd276;\n"
"	}\n"
"	@%p33 bra 	$L__BB1_46;\n"
"	bra.uni 	$L__BB1_45;\n"
"\n"
"$L__BB1_46:\n"
"	setp.gt.s32 	%p34, %r215, 1048575;\n"
"	mov.u32 	%r217, -1023;\n"
"	@%p34 bra 	$L__BB1_48;\n"
"\n"
"	mul.f64 	%fd276, %fd276, 0d4350000000000000;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r215}, %fd276;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r216, %temp}, %fd276;\n"
"	}\n"
"	mov.u32 	%r217, -1077;\n"
"\n"
"$L__BB1_48:\n"
"	add.s32 	%r79, %r215, -1;\n"
"	setp.lt.u32 	%p35, %r79, 2146435071;\n"
"	@%p35 bra 	$L__BB1_50;\n"
"	bra.uni 	$L__BB1_49;\n"
"\n"
"$L__BB1_50:\n"
"	shr.u32 	%r81, %r215, 20;\n"
"	add.s32 	%r218, %r217, %r81;\n"
"	and.b32  	%r82, %r215, -2146435073;\n"
"	or.b32  	%r83, %r82, 1072693248;\n"
"	mov.b64 	%fd277, {%r216, %r83};\n"
"	setp.lt.s32 	%p37, %r83, 1073127583;\n"
"	@%p37 bra 	$L__BB1_52;\n"
"\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%r84, %temp}, %fd277;\n"
"	}\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r85}, %fd277;\n"
"	}\n"
"	add.s32 	%r86, %r85, -1048576;\n"
"	mov.b64 	%fd277, {%r84, %r86};\n"
"	add.s32 	%r218, %r218, 1;\n"
"\n"
"$L__BB1_52:\n"
"	add.f64 	%fd108, %fd277, 0d3FF0000000000000;\n"
"	mov.f64 	%fd109, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd110, %fd108;\n"
"	neg.f64 	%fd111, %fd108;\n"
"	fma.rn.f64 	%fd112, %fd111, %fd110, %fd109;\n"
"	fma.rn.f64 	%fd113, %fd112, %fd112, %fd112;\n"
"	fma.rn.f64 	%fd114, %fd113, %fd110, %fd110;\n"
"	add.f64 	%fd115, %fd277, 0dBFF0000000000000;\n"
"	mul.f64 	%fd116, %fd115, %fd114;\n"
"	fma.rn.f64 	%fd117, %fd115, %fd114, %fd116;\n"
"	mul.f64 	%fd118, %fd117, %fd117;\n"
"	mov.f64 	%fd119, 0d3ED0EE258B7A8B04;\n"
"	mov.f64 	%fd120, 0d3EB1380B3AE80F1E;\n"
"	fma.rn.f64 	%fd121, %fd120, %fd118, %fd119;\n"
"	mov.f64 	%fd122, 0d3EF3B2669F02676F;\n"
"	fma.rn.f64 	%fd123, %fd121, %fd118, %fd122;\n"
"	mov.f64 	%fd124, 0d3F1745CBA9AB0956;\n"
"	fma.rn.f64 	%fd125, %fd123, %fd118, %fd124;\n"
"	mov.f64 	%fd126, 0d3F3C71C72D1B5154;\n"
"	fma.rn.f64 	%fd127, %fd125, %fd118, %fd126;\n"
"	mov.f64 	%fd128, 0d3F624924923BE72D;\n"
"	fma.rn.f64 	%fd129, %fd127, %fd118, %fd128;\n"
"	mov.f64 	%fd130, 0d3F8999999999A3C4;\n"
"	fma.rn.f64 	%fd131, %fd129, %fd118, %fd130;\n"
"	mov.f64 	%fd132, 0d3FB5555555555554;\n"
"	fma.rn.f64 	%fd133, %fd131, %fd118, %fd132;\n"
"	sub.f64 	%fd134, %fd115, %fd117;\n"
"	add.f64 	%fd135, %fd134, %fd134;\n"
"	neg.f64 	%fd136, %fd117;\n"
"	fma.rn.f64 	%fd137, %fd136, %fd115, %fd135;\n"
"	mul.f64 	%fd138, %fd114, %fd137;\n"
"	mul.f64 	%fd139, %fd118, %fd133;\n"
"	fma.rn.f64 	%fd140, %fd139, %fd117, %fd138;\n"
"	xor.b32  	%r87, %r218, -2147483648;\n"
"	mov.u32 	%r88, -2147483648;\n"
"	mov.u32 	%r89, 1127219200;\n"
"	mov.b64 	%fd141, {%r87, %r89};\n"
"	mov.b64 	%fd142, {%r88, %r89};\n"
"	sub.f64 	%fd143, %fd141, %fd142;\n"
"	mov.f64 	%fd144, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd145, %fd143, %fd144, %fd117;\n"
"	neg.f64 	%fd146, %fd143;\n"
"	fma.rn.f64 	%fd147, %fd146, %fd144, %fd145;\n"
"	sub.f64 	%fd148, %fd147, %fd117;\n"
"	sub.f64 	%fd149, %fd140, %fd148;\n"
"	mov.f64 	%fd150, 0d3C7ABC9E3B39803F;\n"
"	fma.rn.f64 	%fd151, %fd143, %fd150, %fd149;\n"
"	add.f64 	%fd278, %fd145, %fd151;\n"
"	bra.uni 	$L__BB1_53;\n"
"\n"
"$L__BB1_45:\n"
"	shr.u32 	%r70, %r215, 20;\n"
"	and.b32  	%r71, %r70, 2046;\n"
"	add.s32 	%r72, %r71, 2147482626;\n"
"	mov.u32 	%r73, 1127219200;\n"
"	mov.b64 	%fd22, {%r72, %r73};\n"
"	mov.u32 	%r74, -2147483648;\n"
"	mov.b64 	%fd23, {%r74, %r73};\n"
"	sub.f64 	%fd24, %fd22, %fd23;\n"
"	and.b32  	%r75, %r215, -2145386497;\n"
"	or.b32  	%r76, %r75, 1071644672;\n"
"	mov.b64 	%fd25, {%r216, %r76};\n"
"	add.f64 	%fd26, %fd25, 0dBFF0000000000000;\n"
"	add.f64 	%fd27, %fd25, 0d3FF0000000000000;\n"
"	mov.f64 	%fd28, 0d3FF0000000000000;\n"
"	rcp.approx.ftz.f64 	%fd29, %fd27;\n"
"	neg.f64 	%fd30, %fd27;\n"
"	fma.rn.f64 	%fd31, %fd30, %fd29, %fd28;\n"
"	fma.rn.f64 	%fd32, %fd31, %fd31, %fd31;\n"
"	fma.rn.f64 	%fd33, %fd32, %fd29, %fd29;\n"
"	mul.f64 	%fd34, %fd26, %fd33;\n"
"	mov.f64 	%fd35, 0dC000000000000000;\n"
"	fma.rn.f64 	%fd36, %fd35, %fd34, %fd26;\n"
"	neg.f64 	%fd37, %fd34;\n"
"	fma.rn.f64 	%fd38, %fd37, %fd26, %fd36;\n"
"	fma.rn.f64 	%fd39, %fd38, %fd33, %fd34;\n"
"	mul.f64 	%fd40, %fd39, %fd39;\n"
"	mov.f64 	%fd41, 0d3FA55CF59CDC5D89;\n"
"	mov.f64 	%fd42, 0d3FB5C5C218C775C9;\n"
"	fma.rn.f64 	%fd43, %fd42, %fd40, %fd41;\n"
"	mov.f64 	%fd44, 0d3FAEFD18CF6EBB9C;\n"
"	fma.rn.f64 	%fd45, %fd43, %fd40, %fd44;\n"
"	mov.f64 	%fd46, 0d3FB10682EDCB8D1B;\n"
"	fma.rn.f64 	%fd47, %fd45, %fd40, %fd46;\n"
"	mov.f64 	%fd48, 0d3FB3B1DD3AC7FC96;\n"
"	fma.rn.f64 	%fd49, %fd47, %fd40, %fd48;\n"
"	mov.f64 	%fd50, 0d3FB745CB459B54A6;\n"
"	fma.rn.f64 	%fd51, %fd49, %fd40, %fd50;\n"
"	mov.f64 	%fd52, 0d3FBC71C741A0669F;\n"
"	fma.rn.f64 	%fd53, %fd51, %fd40, %fd52;\n"
"	mov.f64 	%fd54, 0d3FC249249209112E;\n"
"	fma.rn.f64 	%fd55, %fd53, %fd40, %fd54;\n"
"	mov.f64 	%fd56, 0d3FC99999999A06C1;\n"
"	fma.rn.f64 	%fd57, %fd55, %fd40, %fd56;\n"
"	mov.f64 	%fd58, 0d3FD5555555555535;\n"
"	fma.rn.f64 	%fd59, %fd57, %fd40, %fd58;\n"
"	mul.f64 	%fd60, %fd40, %fd59;\n"
"	fma.rn.f64 	%fd61, %fd60, %fd39, %fd39;\n"
"	add.f64 	%fd62, %fd61, %fd61;\n"
"	mov.f64 	%fd63, 0d3FE62E42FEFA39EF;\n"
"	fma.rn.f64 	%fd64, %fd24, %fd63, %fd62;\n"
"	neg.f64 	%fd65, %fd64;\n"
"	rsqrt.approx.ftz.f64 	%fd66, %fd65;\n"
"	mul.rn.f64 	%fd67, %fd66, %fd66;\n"
"	neg.f64 	%fd68, %fd67;\n"
"	fma.rn.f64 	%fd69, %fd65, %fd68, %fd28;\n"
"	mov.f64 	%fd70, 0d3FE0000000000000;\n"
"	mov.f64 	%fd71, 0d3FD8000000000000;\n"
"	fma.rn.f64 	%fd72, %fd71, %fd69, %fd70;\n"
"	mul.rn.f64 	%fd73, %fd69, %fd66;\n"
"	fma.rn.f64 	%fd74, %fd72, %fd73, %fd66;\n"
"	mov.f64 	%fd75, 0d4000A0E7333839AA;\n"
"	mov.f64 	%fd76, 0d3FEBE9222591AFAB;\n"
"	fma.rn.f64 	%fd77, %fd76, %fd74, %fd75;\n"
"	mov.f64 	%fd78, 0d4008768CF7E57D5C;\n"
"	fma.rn.f64 	%fd79, %fd77, %fd74, %fd78;\n"
"	mov.f64 	%fd80, 0d400B77E7E28DA583;\n"
"	fma.rn.f64 	%fd81, %fd79, %fd74, %fd80;\n"
"	mov.f64 	%fd82, 0d3FF34F26A4F99CF9;\n"
"	fma.rn.f64 	%fd83, %fd81, %fd74, %fd82;\n"
"	mov.f64 	%fd84, 0d3FC1F674ADB019ED;\n"
"	fma.rn.f64 	%fd85, %fd83, %fd74, %fd84;\n"
"	mov.f64 	%fd86, 0d3F75DDAE9506431D;\n"
"	fma.rn.f64 	%fd87, %fd85, %fd74, %fd86;\n"
"	mov.f64 	%fd88, 0d3F0ADA49AA32489C;\n"
"	fma.rn.f64 	%fd89, %fd87, %fd74, %fd88;\n"
"	add.f64 	%fd90, %fd74, 0d4001E90FF51C2197;\n"
"	mov.f64 	%fd91, 0d40111EA3A7CF3820;\n"
"	fma.rn.f64 	%fd92, %fd90, %fd74, %fd91;\n"
"	mov.f64 	%fd93, 0d4011A0E4A4749594;\n"
"	fma.rn.f64 	%fd94, %fd92, %fd74, %fd93;\n"
"	mov.f64 	%fd95, 0d400D4E977D38C14D;\n"
"	fma.rn.f64 	%fd96, %fd94, %fd74, %fd95;\n"
"	mov.f64 	%fd97, 0d3FF37FD567EC0D5F;\n"
"	fma.rn.f64 	%fd98, %fd96, %fd74, %fd97;\n"
"	mov.f64 	%fd99, 0d3FC1FB9D7F676033;\n"
"	fma.rn.f64 	%fd100, %fd98, %fd74, %fd99;\n"
"	mov.f64 	%fd101, 0d3F75DDCDF98946E4;\n"
"	fma.rn.f64 	%fd102, %fd100, %fd74, %fd101;\n"
"	mov.f64 	%fd103, 0d3F0ADA42D79D8DBB;\n"
"	fma.rn.f64 	%fd104, %fd102, %fd74, %fd103;\n"
"	mul.f64 	%fd105, %fd74, %fd104;\n"
"	div.rn.f64 	%fd279, %fd89, %fd105;\n"
"	bra.uni 	$L__BB1_54;\n"
"\n"
"$L__BB1_49:\n"
"	mov.f64 	%fd106, 0d7FF0000000000000;\n"
"	fma.rn.f64 	%fd107, %fd276, %fd106, %fd106;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r80}, %fd276;\n"
"	}\n"
"	mov.b32 	%f60, %r80;\n"
"	setp.eq.f32 	%p36, %f60, 0f00000000;\n"
"	selp.f64 	%fd278, 0dFFF0000000000000, %fd107, %p36;\n"
"\n"
"$L__BB1_53:\n"
"	neg.f64 	%fd152, %fd278;\n"
"	rsqrt.approx.f64 	%fd153, %fd152;\n"
"	mov.f64 	%fd154, 0d3FFA2013964E259C;\n"
"	mov.f64 	%fd155, 0d3FE8E2101C71B0BF;\n"
"	fma.rn.f64 	%fd156, %fd155, %fd153, %fd154;\n"
"	mov.f64 	%fd157, 0d3FDABFE90921BE68;\n"
"	fma.rn.f64 	%fd158, %fd156, %fd153, %fd157;\n"
"	mov.f64 	%fd159, 0d3F97E41314DE00D4;\n"
"	fma.rn.f64 	%fd160, %fd158, %fd153, %fd159;\n"
"	mov.f64 	%fd161, 0d3F311BD487102E94;\n"
"	fma.rn.f64 	%fd162, %fd160, %fd153, %fd161;\n"
"	add.f64 	%fd163, %fd153, 0d3FF59895C30BAA54;\n"
"	mov.f64 	%fd164, 0d3FFAE8E5956A143F;\n"
"	fma.rn.f64 	%fd165, %fd163, %fd153, %fd164;\n"
"	mov.f64 	%fd166, 0d3FDACCE85FF7383D;\n"
"	fma.rn.f64 	%fd167, %fd165, %fd153, %fd166;\n"
"	mov.f64 	%fd168, 0d3F97E43B6CAC34FE;\n"
"	fma.rn.f64 	%fd169, %fd167, %fd153, %fd168;\n"
"	mov.f64 	%fd170, 0d3F311BD08289EB12;\n"
"	fma.rn.f64 	%fd171, %fd169, %fd153, %fd170;\n"
"	mul.f64 	%fd172, %fd153, %fd171;\n"
"	div.rn.f64 	%fd279, %fd162, %fd172;\n"
"\n"
"$L__BB1_54:\n"
"	neg.f64 	%fd173, %fd279;\n"
"	selp.f64 	%fd280, %fd173, %fd279, %p32;\n"
"\n"
"$L__BB1_56:\n"
"	mul.f64 	%fd265, %fd280, 0dBCA21165F626CDD5;\n"
"	mov.f64 	%fd266, 0dBFF6A09E667F3BCC;\n"
"	fma.rn.f64 	%fd267, %fd266, %fd280, %fd265;\n"
"	mov.f64 	%fd268, 0d0000000000000000;\n"
"	add.rn.f64 	%fd269, %fd267, %fd268;\n"
"	cvt.rn.f32.f64 	%f529, %fd269;\n"
"	bra.uni 	$L__BB1_94;\n"
"\n"
"}\n"
".func  (.param .align 8 .b8 func_retval0[16]) __internal_trig_reduction_slowpathd(\n"
"	.param .b64 __internal_trig_reduction_slowpathd_param_0\n"
")\n"
"{\n"
"	.local .align 8 .b8 	__local_depot2[40];\n"
"	.reg .b64 	%SP;\n"
"	.reg .b64 	%SPL;\n"
"	.reg .pred 	%p<10>;\n"
"	.reg .b32 	%r<38>;\n"
"	.reg .f64 	%fd<5>;\n"
"	.reg .b64 	%rd<77>;\n"
"\n"
"\n"
"	mov.u64 	%SPL, __local_depot2;\n"
"	ld.param.f64 	%fd4, [__internal_trig_reduction_slowpathd_param_0];\n"
"	add.u64 	%rd1, %SPL, 0;\n"
"	{\n"
"	.reg .b32 %temp; \n"
"	mov.b64 	{%temp, %r1}, %fd4;\n"
"	}\n"
"	shr.u32 	%r12, %r1, 20;\n"
"	and.b32  	%r2, %r12, 2047;\n"
"	setp.eq.s32 	%p1, %r2, 2047;\n"
"	mov.u32 	%r37, 0;\n"
"	@%p1 bra 	$L__BB2_7;\n"
"\n"
"	add.s32 	%r13, %r2, -1024;\n"
"	shr.u32 	%r14, %r13, 6;\n"
"	mov.u32 	%r15, 16;\n"
"	sub.s32 	%r16, %r15, %r14;\n"
"	mov.u32 	%r17, 15;\n"
"	sub.s32 	%r3, %r17, %r14;\n"
"	mov.u32 	%r18, 19;\n"
"	sub.s32 	%r19, %r18, %r14;\n"
"	setp.gt.s32 	%p2, %r16, 14;\n"
"	selp.b32 	%r4, 18, %r19, %p2;\n"
"	setp.gt.s32 	%p3, %r16, %r4;\n"
"	mov.u64 	%rd74, 0;\n"
"	mov.u32 	%r36, %r3;\n"
"	@%p3 bra 	$L__BB2_4;\n"
"\n"
"	mul.wide.s32 	%rd21, %r3, 8;\n"
"	mov.u64 	%rd22, __cudart_i2opi_d;\n"
"	add.s64 	%rd72, %rd22, %rd21;\n"
"	mov.b64 	%rd23, %fd4;\n"
"	shl.b64 	%rd24, %rd23, 11;\n"
"	or.b64  	%rd3, %rd24, -9223372036854775808;\n"
"	mov.u64 	%rd74, 0;\n"
"	mov.u64 	%rd71, %rd1;\n"
"	mov.u32 	%r36, %r3;\n"
"\n"
"$L__BB2_3:\n"
"	.pragma \"nounroll\";\n"
"	ld.global.nc.u64 	%rd25, [%rd72];\n"
"	{\n"
"	.reg .u32 %r0, %r1, %r2, %r3, %alo, %ahi, %blo, %bhi, %clo, %chi;\n"
"	mov.b64 	{%alo,%ahi}, %rd25;\n"
"	mov.b64 	{%blo,%bhi}, %rd3;\n"
"	mov.b64 	{%clo,%chi}, %rd74;\n"
"	mad.lo.cc.u32 	%r0, %alo, %blo, %clo;\n"
"	madc.hi.cc.u32 	%r1, %alo, %blo, %chi;\n"
"	madc.hi.u32 	%r2, %alo, %bhi, 0;\n"
"	mad.lo.cc.u32 	%r1, %alo, %bhi, %r1;\n"
"	madc.hi.cc.u32 	%r2, %ahi, %blo, %r2;\n"
"	madc.hi.u32 	%r3, %ahi, %bhi, 0;\n"
"	mad.lo.cc.u32 	%r1, %ahi, %blo, %r1;\n"
"	madc.lo.cc.u32 	%r2, %ahi, %bhi, %r2;\n"
"	addc.u32 	%r3, %r3, 0;\n"
"	mov.b64 	%rd26, {%r0,%r1};\n"
"	mov.b64 	%rd74, {%r2,%r3};\n"
"	}\n"
"	st.local.u64 	[%rd71], %rd26;\n"
"	add.s64 	%rd72, %rd72, 8;\n"
"	add.s64 	%rd71, %rd71, 8;\n"
"	add.s32 	%r36, %r36, 1;\n"
"	setp.lt.s32 	%p4, %r36, %r4;\n"
"	@%p4 bra 	$L__BB2_3;\n"
"\n"
"$L__BB2_4:\n"
"	sub.s32 	%r20, %r36, %r3;\n"
"	mul.wide.s32 	%rd27, %r20, 8;\n"
"	add.s64 	%rd28, %rd1, %rd27;\n"
"	st.local.u64 	[%rd28], %rd74;\n"
"	add.s32 	%r22, %r12, -1024;\n"
"	and.b32  	%r8, %r22, 63;\n"
"	ld.local.u64 	%rd76, [%rd1+16];\n"
"	ld.local.u64 	%rd75, [%rd1+24];\n"
"	setp.eq.s32 	%p5, %r8, 0;\n"
"	@%p5 bra 	$L__BB2_6;\n"
"\n"
"	mov.u32 	%r23, 64;\n"
"	sub.s32 	%r24, %r23, %r8;\n"
"	shl.b64 	%rd29, %rd75, %r8;\n"
"	shr.u64 	%rd30, %rd76, %r24;\n"
"	or.b64  	%rd75, %rd29, %rd30;\n"
"	shl.b64 	%rd31, %rd76, %r8;\n"
"	ld.local.u64 	%rd32, [%rd1+8];\n"
"	shr.u64 	%rd33, %rd32, %r24;\n"
"	or.b64  	%rd76, %rd33, %rd31;\n"
"\n"
"$L__BB2_6:\n"
"	shr.u64 	%rd34, %rd75, 62;\n"
"	cvt.u32.u64 	%r25, %rd34;\n"
"	shr.u64 	%rd35, %rd76, 62;\n"
"	shl.b64 	%rd36, %rd75, 2;\n"
"	or.b64  	%rd37, %rd35, %rd36;\n"
"	shr.u64 	%rd38, %rd75, 61;\n"
"	cvt.u32.u64 	%r26, %rd38;\n"
"	and.b32  	%r27, %r26, 1;\n"
"	add.s32 	%r28, %r27, %r25;\n"
"	and.b32  	%r29, %r1, -2147483648;\n"
"	setp.eq.s32 	%p6, %r29, 0;\n"
"	neg.s32 	%r30, %r28;\n"
"	selp.b32 	%r37, %r28, %r30, %p6;\n"
"	setp.eq.s32 	%p7, %r27, 0;\n"
"	shl.b64 	%rd39, %rd76, 2;\n"
"	mov.u64 	%rd40, 0;\n"
"	{\n"
"	.reg .u32 %r0, %r1, %r2, %r3, %a0, %a1, %a2, %a3, %b0, %b1, %b2, %b3;\n"
"	mov.b64 	{%a0,%a1}, %rd40;\n"
"	mov.b64 	{%a2,%a3}, %rd40;\n"
"	mov.b64 	{%b0,%b1}, %rd39;\n"
"	mov.b64 	{%b2,%b3}, %rd37;\n"
"	sub.cc.u32 	%r0, %a0, %b0;\n"
"	subc.cc.u32 	%r1, %a1, %b1;\n"
"	subc.cc.u32 	%r2, %a2, %b2;\n"
"	subc.u32 	%r3, %a3, %b3;\n"
"	mov.b64 	%rd41, {%r0,%r1};\n"
"	mov.b64 	%rd42, {%r2,%r3};\n"
"	}\n"
"	xor.b32  	%r31, %r29, -2147483648;\n"
"	selp.b64 	%rd43, %rd37, %rd42, %p7;\n"
"	selp.b64 	%rd44, %rd39, %rd41, %p7;\n"
"	selp.b32 	%r32, %r29, %r31, %p7;\n"
"	clz.b64 	%r33, %rd43;\n"
"	cvt.u64.u32 	%rd45, %r33;\n"
"	setp.eq.s64 	%p8, %rd45, 0;\n"
"	shl.b64 	%rd46, %rd43, %r33;\n"
"	mov.u64 	%rd47, 64;\n"
"	sub.s64 	%rd48, %rd47, %rd45;\n"
"	cvt.u32.u64 	%r34, %rd48;\n"
"	shr.u64 	%rd49, %rd44, %r34;\n"
"	or.b64  	%rd50, %rd49, %rd46;\n"
"	selp.b64 	%rd51, %rd43, %rd50, %p8;\n"
"	mov.u64 	%rd52, -3958705157555305931;\n"
"	{\n"
"	.reg .u32 %r0, %r1, %r2, %r3, %alo, %ahi, %blo, %bhi;\n"
"	mov.b64 	{%alo,%ahi}, %rd51;\n"
"	mov.b64 	{%blo,%bhi}, %rd52;\n"
"	mul.lo.u32 	%r0, %alo, %blo;\n"
"	mul.hi.u32 	%r1, %alo, %blo;\n"
"	mad.lo.cc.u32 	%r1, %alo, %bhi, %r1;\n"
"	madc.hi.u32 	%r2, %alo, %bhi, 0;\n"
"	mad.lo.cc.u32 	%r1, %ahi, %blo, %r1;\n"
"	madc.hi.cc.u32 	%r2, %ahi, %blo, %r2;\n"
"	madc.hi.u32 	%r3, %ahi, %bhi, 0;\n"
"	mad.lo.cc.u32 	%r2, %ahi, %bhi, %r2;\n"
"	addc.u32 	%r3, %r3, 0;\n"
"	mov.b64 	%rd53, {%r0,%r1};\n"
"	mov.b64 	%rd54, {%r2,%r3};\n"
"	}\n"
"	setp.gt.s64 	%p9, %rd54, 0;\n"
"	{\n"
"	.reg .u32 %r0, %r1, %r2, %r3, %a0, %a1, %a2, %a3, %b0, %b1, %b2, %b3;\n"
"	mov.b64 	{%a0,%a1}, %rd53;\n"
"	mov.b64 	{%a2,%a3}, %rd54;\n"
"	mov.b64 	{%b0,%b1}, %rd53;\n"
"	mov.b64 	{%b2,%b3}, %rd54;\n"
"	add.cc.u32 	%r0, %a0, %b0;\n"
"	addc.cc.u32 	%r1, %a1, %b1;\n"
"	addc.cc.u32 	%r2, %a2, %b2;\n"
"	addc.u32 	%r3, %a3, %b3;\n"
"	mov.b64 	%rd55, {%r0,%r1};\n"
"	mov.b64 	%rd56, {%r2,%r3};\n"
"	}\n"
"	selp.b64 	%rd57, %rd56, %rd54, %p9;\n"
"	selp.u64 	%rd58, 1, 0, %p9;\n"
"	add.s64 	%rd59, %rd45, %rd58;\n"
"	cvt.u64.u32 	%rd60, %r32;\n"
"	shl.b64 	%rd61, %rd60, 32;\n"
"	shl.b64 	%rd62, %rd59, 52;\n"
"	mov.u64 	%rd63, 4602678819172646912;\n"
"	sub.s64 	%rd64, %rd63, %rd62;\n"
"	add.s64 	%rd65, %rd57, 1;\n"
"	shr.u64 	%rd66, %rd65, 10;\n"
"	add.s64 	%rd67, %rd66, 1;\n"
"	shr.u64 	%rd68, %rd67, 1;\n"
"	add.s64 	%rd69, %rd64, %rd68;\n"
"	or.b64  	%rd70, %rd69, %rd61;\n"
"	mov.b64 	%fd4, %rd70;\n"
"\n"
"$L__BB2_7:\n"
"	st.param.f64 	[func_retval0+0], %fd4;\n"
"	st.param.b32 	[func_retval0+8], %r37;\n"
"	ret;\n"
"\n"
"}\n"
"\n"
;

static const char* omni_tensor_cuda_round_ptx =
#include "tensor_cuda_rounding_i64_ptx.inc"
;

static const char* omni_tensor_cuda_complex_map_ptx =
#include "tensor_cuda_complex_map_ptx.inc"
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

int omni_tensor_backend_cuda_available(void) {
    if (!omni_tensor_cuda_resolve()) return 0;
    int count = 0;
    if (omni_cuda_get_device_count(&count) != 0) return 0;
    if (count <= 0) return 0;

    void* probe = NULL;
    if (omni_cuda_malloc(&probe, 1) != 0 || probe == NULL) return 0;
    omni_cuda_free(probe);
    return 1;
}

void omni_tensor_backend_cuda_disable_for_tests(int disabled) {
    omni_tensor_cuda_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_cublas_disable_for_tests(int disabled) {
    omni_tensor_cublas_disabled_for_tests = disabled != 0;
}

int omni_tensor_backend_cublas_available(void) {
    if (!omni_tensor_backend_cuda_available()) return 0;
    if (!omni_tensor_cublas_resolve()) return 0;
    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) return 0;
    int status = omni_cublas_destroy(handle);
    return status == OMNI_CUBLAS_STATUS_SUCCESS ? 1 : 0;
}

int omni_tensor_backend_cublas_float32_available(void) {
    if (!omni_tensor_backend_cublas_available()) return 0;
    return omni_cublas_sgemm != NULL && omni_cublas_sgemv != NULL ? 1 : 0;
}

int omni_tensor_backend_cublas_complex_available(void) {
    if (!omni_tensor_backend_cublas_available()) return 0;
    return omni_cublas_zgemm != NULL &&
        omni_cublas_cgemm != NULL &&
        omni_cublas_zgemv != NULL &&
        omni_cublas_cgemv != NULL ? 1 : 0;
}

int omni_tensor_backend_cuda_map_available(void) {
    return omni_tensor_cuda_map_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_scientific_map_available(void) {
    return omni_tensor_cuda_map_scientific_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_complex_map_available(void) {
    return omni_tensor_cuda_complex_map_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_complex_matrix_available(void) {
    return omni_tensor_cuda_complex_matrix_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_rounding_i64_available(void) {
    return omni_tensor_cuda_round_resolve() ? 1 : 0;
}

int omni_tensor_backend_cuda_alloc(size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    void* device_ptr = NULL;
    if (omni_cuda_malloc(&device_ptr, byte_len) != 0 || device_ptr == NULL) {
        return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    }
    *out_device_ptr = device_ptr;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_device(const void* host_ptr, size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (host_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;

    void* device_ptr = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &device_ptr);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;
    if (omni_cuda_memcpy(device_ptr, host_ptr, byte_len, OMNI_CUDA_MEMCPY_HOST_TO_DEVICE) != 0) {
        omni_cuda_free(device_ptr);
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    *out_device_ptr = device_ptr;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_host(const void* device_ptr, size_t byte_len, void* host_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(host_ptr, device_ptr, byte_len, OMNI_CUDA_MEMCPY_DEVICE_TO_HOST) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_to_existing_device(const void* host_ptr, size_t byte_len, void* device_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (host_ptr == NULL || device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(device_ptr, host_ptr, byte_len, OMNI_CUDA_MEMCPY_HOST_TO_DEVICE) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_copy_device_to_existing_device(const void* source_device_ptr, size_t byte_len, void* dest_device_ptr) {
    if (byte_len == 0 || source_device_ptr == dest_device_ptr) return OMNI_TENSOR_CUDA_SUCCESS;
    if (source_device_ptr == NULL || dest_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_backend_cuda_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (omni_cuda_memcpy(dest_device_ptr, source_device_ptr, byte_len, OMNI_CUDA_MEMCPY_DEVICE_TO_DEVICE) != 0) {
        return OMNI_TENSOR_CUDA_COPY_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_fill_f64(void* device_ptr, size_t element_count, double value) {
    if (element_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || element_count > ((size_t)-1) / sizeof(double)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(double);
    double* host_values = (double*)malloc(byte_len);
    if (host_values == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    for (size_t i = 0; i < element_count; i++) {
        host_values[i] = value;
    }
    int status = omni_tensor_backend_cuda_copy_to_existing_device(host_values, byte_len, device_ptr);
    free(host_values);
    return status;
}

int omni_tensor_backend_cuda_fill_f32(void* device_ptr, size_t element_count, float value) {
    if (element_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (device_ptr == NULL || element_count > ((size_t)-1) / sizeof(float)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(float);
    float* host_values = (float*)malloc(byte_len);
    if (host_values == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;
    for (size_t i = 0; i < element_count; i++) {
        host_values[i] = value;
    }
    int status = omni_tensor_backend_cuda_copy_to_existing_device(host_values, byte_len, device_ptr);
    free(host_values);
    return status;
}

static int omni_tensor_cuda_operand_matches_broadcast(
    size_t out_rank,
    size_t operand_rank,
    const size_t* out_shape,
    const size_t* operand_shape
) {
    if (operand_rank > out_rank) return 0;
    if (operand_rank == 0) return 1;
    if (out_shape == NULL || operand_shape == NULL) return 0;
    size_t axis_delta = out_rank - operand_rank;
    for (size_t out_axis = 0; out_axis < out_rank; out_axis++) {
        if (out_axis < axis_delta) continue;
        size_t operand_axis = out_axis - axis_delta;
        size_t operand_dim = operand_shape[operand_axis];
        size_t out_dim = out_shape[out_axis];
        if (operand_dim != out_dim && operand_dim != 1) return 0;
    }
    return 1;
}

static int omni_tensor_cuda_operand_needs_offsets(
    size_t out_rank,
    size_t operand_rank,
    size_t element_count,
    const size_t* out_shape,
    const size_t* operand_shape
) {
    if (!omni_tensor_cuda_operand_matches_broadcast(out_rank, operand_rank, out_shape, operand_shape)) return -1;
    if (element_count <= 1) return 0;
    if (operand_rank == 0) return 1;
    if (operand_rank != out_rank) return 1;
    for (size_t axis = 0; axis < out_rank; axis++) {
        if (operand_shape[axis] != out_shape[axis]) return 1;
    }
    return 0;
}

static int omni_tensor_cuda_build_operand_offsets(
    size_t out_rank,
    size_t operand_rank,
    size_t element_count,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* operand_shape,
    const size_t* operand_strides,
    void** out_offsets_device
) {
    if (out_offsets_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_offsets_device = NULL;
    int needs_offsets = omni_tensor_cuda_operand_needs_offsets(out_rank, operand_rank, element_count, out_shape, operand_shape);
    if (needs_offsets < 0) return OMNI_TENSOR_CUDA_INVALID;
    if (!needs_offsets) return OMNI_TENSOR_CUDA_SUCCESS;
    if (element_count > ((size_t)-1) / sizeof(uint64_t)) return OMNI_TENSOR_CUDA_INVALID;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (operand_rank > 0 && (operand_shape == NULL || operand_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    size_t byte_len = element_count * sizeof(uint64_t);
    uint64_t* offsets = (uint64_t*)calloc(element_count, sizeof(uint64_t));
    if (offsets == NULL) return OMNI_TENSOR_CUDA_ALLOCATION_FAILED;

    size_t axis_delta = out_rank - operand_rank;
    for (size_t index = 0; index < element_count; index++) {
        size_t offset = 0;
        for (size_t out_axis = 0; out_axis < out_rank; out_axis++) {
            if (out_axis < axis_delta) continue;
            size_t operand_axis = out_axis - axis_delta;
            size_t operand_dim = operand_rank == 0 ? 1 : operand_shape[operand_axis];
            if (operand_dim == 1) continue;
            size_t out_dim = out_shape[out_axis];
            size_t out_stride = out_strides[out_axis];
            if (out_dim == 0 || out_stride == 0) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            size_t coord = (index / out_stride) % out_dim;
            if (operand_strides[operand_axis] != 0 && coord > ((size_t)-1) / operand_strides[operand_axis]) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            size_t axis_offset = coord * operand_strides[operand_axis];
            if (axis_offset > ((size_t)-1) - offset) {
                free(offsets);
                return OMNI_TENSOR_CUDA_INVALID;
            }
            offset += axis_offset;
        }
        offsets[index] = (uint64_t)offset;
    }

    int status = omni_tensor_backend_cuda_copy_to_device(offsets, byte_len, out_offsets_device);
    free(offsets);
    return status;
}

static int omni_tensor_backend_cuda_map_launch(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    void* scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (op > 5 || mode > 2 || element_size == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if ((mode == 0 && left_device == NULL) ||
        (mode == 1 && right_device == NULL) ||
        (mode == 2 && (left_device == NULL || right_device == NULL)) ||
        scalar == NULL ||
        function == NULL) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 1 && left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 0 && right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    void* left_offsets_device = NULL;
    void* right_offsets_device = NULL;
    int offset_status = OMNI_TENSOR_CUDA_SUCCESS;
    if (mode != 1) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            left_rank,
            element_count,
            out_shape,
            out_strides,
            left_shape,
            left_strides,
            &left_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) return offset_status;
    }
    if (mode != 0) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            right_rank,
            element_count,
            out_shape,
            out_strides,
            right_shape,
            right_strides,
            &right_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
            return offset_status;
        }
    }

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        return alloc_status;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&left_device,
        (void*)&right_device,
        scalar,
        (void*)&mode,
        (void*)&op,
        (void*)&element_count,
        (void*)&left_offsets_device,
        (void*)&right_offsets_device,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
    if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_map_launch(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    void* scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (op > 3 || mode > 2 || element_size == 0 || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if ((mode == 0 && left_device == NULL) ||
        (mode == 1 && right_device == NULL) ||
        (mode == 2 && (left_device == NULL || right_device == NULL)) ||
        scalar == NULL ||
        function == NULL) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 1 && left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;
    if (mode != 0 && right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_CUDA_INVALID;

    void* left_offsets_device = NULL;
    void* right_offsets_device = NULL;
    int offset_status = OMNI_TENSOR_CUDA_SUCCESS;
    if (mode != 1) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            left_rank,
            element_count,
            out_shape,
            out_strides,
            left_shape,
            left_strides,
            &left_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) return offset_status;
    }
    if (mode != 0) {
        offset_status = omni_tensor_cuda_build_operand_offsets(
            out_rank,
            right_rank,
            element_count,
            out_shape,
            out_strides,
            right_shape,
            right_strides,
            &right_offsets_device
        );
        if (offset_status != OMNI_TENSOR_CUDA_SUCCESS) {
            if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
            return offset_status;
        }
    }

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        return alloc_status;
    }

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
        if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&left_device,
        (void*)&right_device,
        scalar,
        (void*)&mode,
        (void*)&op,
        (void*)&element_count,
        (void*)&left_offsets_device,
        (void*)&right_offsets_device,
        (void*)&out_device,
        (void*)&status_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (left_offsets_device != NULL) omni_tensor_backend_cuda_free(left_offsets_device);
    if (right_offsets_device != NULL) omni_tensor_backend_cuda_free(right_offsets_device);
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return copy_status;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_map_f64(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    double scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(double),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_f32(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    float scalar,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(float),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    double scalar_real,
    double scalar_imag,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    OmniTensorCudaComplex128 scalar = { scalar_real, scalar_imag };
    return omni_tensor_backend_cuda_complex_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64(
    const void* left_device,
    const void* right_device,
    size_t byte_len,
    size_t element_count,
    float scalar_real,
    float scalar_imag,
    unsigned int mode,
    unsigned int op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    OmniTensorCudaComplex64 scalar = { scalar_real, scalar_imag };
    return omni_tensor_backend_cuda_complex_map_launch(
        left_device,
        right_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        &scalar,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_cuda_map_complex64_function,
        out_device_ptr
    );
}

static int omni_tensor_backend_cuda_map_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    unsigned int min_op,
    unsigned int max_op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < min_op ||
        op > max_op ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_map_scientific_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < 5 ||
        op > 20 ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return copy_status;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_unary_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t input_element_size,
    size_t output_element_size,
    unsigned int op,
    unsigned int min_op,
    unsigned int max_op,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < min_op ||
        op > max_op ||
        input_element_size == 0 ||
        output_element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / input_element_size || byte_len != element_count * input_element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / output_element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    size_t out_byte_len = element_count * output_element_size;
    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return copy_status;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        if (status_host == 1u) return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
        if (status_host == 2u) return OMNI_TENSOR_CUDA_INVALID_ARGUMENT;
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_round_i64_launch(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    void* function,
    int64_t* host_out
) {
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        op < 1 ||
        op > 4 ||
        element_size == 0 ||
        element_count > UINT32_MAX) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (host_out == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (element_count > ((size_t)-1) / sizeof(int64_t)) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    size_t out_byte_len = element_count * sizeof(int64_t);
    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int status_host = 0u;
    void* status_device = NULL;
    int status_alloc = omni_tensor_backend_cuda_copy_to_device(&status_host, sizeof(status_host), &status_device);
    if (status_alloc != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_alloc;
    }

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((element_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&op,
        (void*)&element_count,
        (void*)&out_device,
        (void*)&status_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(status_device);
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int status_copy = omni_tensor_backend_cuda_copy_to_host(status_device, sizeof(status_host), &status_host);
    omni_tensor_backend_cuda_free(status_device);
    if (status_copy != OMNI_TENSOR_CUDA_SUCCESS) {
        omni_tensor_backend_cuda_free(out_device);
        return status_copy;
    }
    if (status_host != 0u) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_DOMAIN_ERROR;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(out_device, out_byte_len, host_out);
    omni_tensor_backend_cuda_free(out_device);
    if (copy_status != OMNI_TENSOR_CUDA_SUCCESS) return copy_status;

    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cuda_map_unary_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        0,
        4,
        omni_tensor_cuda_map_unary_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_unary_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        0,
        4,
        omni_tensor_cuda_map_unary_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128_unary(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        sizeof(OmniTensorCudaComplex128),
        op,
        0,
        5,
        omni_tensor_cuda_map_complex128_unary_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64_unary(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        sizeof(OmniTensorCudaComplex64),
        op,
        0,
        5,
        omni_tensor_cuda_map_complex64_unary_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex128_to_real(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex128),
        sizeof(double),
        op,
        0,
        4,
        omni_tensor_cuda_map_complex128_to_real_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_complex64_to_real(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_complex_map_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(OmniTensorCudaComplex64),
        sizeof(float),
        op,
        0,
        4,
        omni_tensor_cuda_map_complex64_to_real_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_scientific_unary_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_scientific_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_cuda_map_scientific_f64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_map_scientific_unary_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    void** out_device_ptr
) {
    if (!omni_tensor_cuda_map_scientific_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_map_scientific_unary_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_cuda_map_scientific_f32_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_round_i64_f64(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_round_i64_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_cuda_round_i64_f64_function,
        host_out
    );
}

int omni_tensor_backend_cuda_round_i64_f32(
    const void* input_device,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    if (!omni_tensor_cuda_round_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_round_i64_launch(
        input_device,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_cuda_round_i64_f32_function,
        host_out
    );
}

static int omni_tensor_backend_cuda_complex_matrix_launch_5(
    const void* input_device,
    size_t out_byte_len,
    size_t launch_count,
    size_t element_size,
    unsigned long long arg0,
    unsigned long long arg1,
    unsigned long long arg2,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (launch_count == 0) return out_byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        element_size == 0 ||
        launch_count > UINT32_MAX ||
        out_byte_len == 0 ||
        out_byte_len % element_size != 0) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((launch_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&arg0,
        (void*)&arg1,
        (void*)&arg2,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_matrix_launch_4(
    const void* input_device,
    size_t out_byte_len,
    size_t launch_count,
    size_t element_size,
    unsigned long long arg0,
    unsigned long long arg1,
    void* function,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (launch_count == 0) return out_byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (input_device == NULL ||
        function == NULL ||
        element_size == 0 ||
        launch_count > UINT32_MAX ||
        out_byte_len == 0 ||
        out_byte_len % element_size != 0) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(out_byte_len, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    unsigned int block_dim = 256;
    unsigned int grid_dim = (unsigned int)((launch_count + block_dim - 1) / block_dim);
    if (grid_dim == 0) grid_dim = 1;
    void* params[] = {
        (void*)&input_device,
        (void*)&arg0,
        (void*)&arg1,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        grid_dim,
        1,
        1,
        block_dim,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    *out_device_ptr = out_device;
    return OMNI_TENSOR_CUDA_SUCCESS;
}

static int omni_tensor_backend_cuda_complex_matrix_trace_launch(
    const void* input_device,
    size_t element_size,
    unsigned long long n,
    void* function,
    void* host_out
) {
    if (host_out == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (input_device == NULL || function == NULL || element_size == 0) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* out_device = NULL;
    int alloc_status = omni_tensor_backend_cuda_alloc(element_size, &out_device);
    if (alloc_status != OMNI_TENSOR_CUDA_SUCCESS) return alloc_status;

    void* params[] = {
        (void*)&input_device,
        (void*)&n,
        (void*)&out_device
    };
    int launch_status = omni_cu_launch_kernel(
        function,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        NULL,
        params,
        NULL
    );
    int sync_status = launch_status == 0 ? omni_cu_ctx_synchronize() : launch_status;
    if (launch_status != 0 || sync_status != 0) {
        omni_tensor_backend_cuda_free(out_device);
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }

    int copy_status = omni_tensor_backend_cuda_copy_to_host(out_device, element_size, host_out);
    omni_tensor_backend_cuda_free(out_device);
    return copy_status;
}

int omni_tensor_backend_cuda_transpose_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > UINT32_MAX ||
        element_count > ((size_t)-1) / element_size ||
        byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_transpose_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_transpose_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > UINT32_MAX ||
        element_count > ((size_t)-1) / element_size ||
        byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        byte_len,
        element_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)element_count,
        omni_tensor_cuda_transpose_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_complex128(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (diagonal_count > UINT32_MAX || diagonal_count > ((size_t)-1) / element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        diagonal_count * element_size,
        diagonal_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)diagonal_count,
        omni_tensor_cuda_diagonal_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_complex64(
    const void* input_device,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    if (rows != 0 && cols > ((size_t)-1) / rows) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (diagonal_count > UINT32_MAX || diagonal_count > ((size_t)-1) / element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_5(
        input_device,
        diagonal_count * element_size,
        diagonal_count,
        element_size,
        (unsigned long long)rows,
        (unsigned long long)cols,
        (unsigned long long)diagonal_count,
        omni_tensor_cuda_diagonal_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_matrix_complex128(
    const void* input_device,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t output_count = n * n;
    if (n > ((size_t)-1) / element_size || byte_len != n * element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (output_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (output_count > UINT32_MAX || output_count > ((size_t)-1) / element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_4(
        input_device,
        output_count * element_size,
        output_count,
        element_size,
        (unsigned long long)n,
        (unsigned long long)output_count,
        omni_tensor_cuda_diagonal_matrix_complex128_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_diagonal_matrix_complex64(
    const void* input_device,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_device_ptr = NULL;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t output_count = n * n;
    if (n > ((size_t)-1) / element_size || byte_len != n * element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (output_count == 0) return byte_len == 0 ? OMNI_TENSOR_CUDA_SUCCESS : OMNI_TENSOR_CUDA_INVALID;
    if (output_count > UINT32_MAX || output_count > ((size_t)-1) / element_size) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;
    return omni_tensor_backend_cuda_complex_matrix_launch_4(
        input_device,
        output_count * element_size,
        output_count,
        element_size,
        (unsigned long long)n,
        (unsigned long long)output_count,
        omni_tensor_cuda_diagonal_matrix_complex64_function,
        out_device_ptr
    );
}

int omni_tensor_backend_cuda_trace_complex128(
    const void* input_device,
    size_t byte_len,
    size_t n,
    double* out_real,
    double* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_real = 0.0;
    *out_imag = 0.0;
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(OmniTensorCudaComplex128);
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    OmniTensorCudaComplex128 trace = { 0.0, 0.0 };
    int status = omni_tensor_backend_cuda_complex_matrix_trace_launch(
        input_device,
        element_size,
        (unsigned long long)n,
        omni_tensor_cuda_trace_complex128_function,
        &trace
    );
    if (status == OMNI_TENSOR_CUDA_SUCCESS) {
        *out_real = trace.real;
        *out_imag = trace.imag;
    }
    return status;
}

int omni_tensor_backend_cuda_trace_complex64(
    const void* input_device,
    size_t byte_len,
    size_t n,
    float* out_real,
    float* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_CUDA_INVALID;
    *out_real = 0.0f;
    *out_imag = 0.0f;
    if (n != 0 && n > ((size_t)-1) / n) return OMNI_TENSOR_CUDA_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(OmniTensorCudaComplex64);
    if (element_count > ((size_t)-1) / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (n == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_CUDA_INVALID;
    if (!omni_tensor_cuda_complex_matrix_resolve()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    OmniTensorCudaComplex64 trace = { 0.0f, 0.0f };
    int status = omni_tensor_backend_cuda_complex_matrix_trace_launch(
        input_device,
        element_size,
        (unsigned long long)n,
        omni_tensor_cuda_trace_complex64_function,
        &trace
    );
    if (status == OMNI_TENSOR_CUDA_SUCCESS) {
        *out_real = trace.real;
        *out_imag = trace.imag;
    }
    return status;
}

int omni_tensor_backend_cublas_dgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const double alpha = 1.0;
    const double beta = 0.0;
    int status = omni_cublas_dgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const double*)matrix_device,
        (int)matrix_leading_dim,
        (const double*)vector_device,
        1,
        &beta,
        (double*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_sgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_float32_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const float alpha = 1.0f;
    const float beta = 0.0f;
    int status = omni_cublas_sgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const float*)matrix_device,
        (int)matrix_leading_dim,
        (const float*)vector_device,
        1,
        &beta,
        (float*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_zgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex128 alpha = { 1.0, 0.0 };
    const OmniTensorCudaComplex128 beta = { 0.0, 0.0 };
    int status = omni_cublas_zgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const OmniTensorCudaComplex128*)matrix_device,
        (int)matrix_leading_dim,
        (const OmniTensorCudaComplex128*)vector_device,
        1,
        &beta,
        (OmniTensorCudaComplex128*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_cgemv_rowmajor(
    size_t rows,
    size_t cols,
    int trans_matrix,
    const void* matrix_device,
    size_t matrix_leading_dim,
    const void* vector_device,
    void* out_device
) {
    if (rows == 0 || cols == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (matrix_device == NULL || vector_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (rows > 2147483647u || cols > 2147483647u || matrix_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex64 alpha = { 1.0f, 0.0f };
    const OmniTensorCudaComplex64 beta = { 0.0f, 0.0f };
    int status = omni_cublas_cgemv(
        handle,
        trans_matrix ? OMNI_CUBLAS_OP_N : OMNI_CUBLAS_OP_T,
        (int)cols,
        (int)rows,
        &alpha,
        (const OmniTensorCudaComplex64*)matrix_device,
        (int)matrix_leading_dim,
        (const OmniTensorCudaComplex64*)vector_device,
        1,
        &beta,
        (OmniTensorCudaComplex64*)out_device,
        1
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

void omni_tensor_backend_cuda_free(void* device_ptr) {
    if (device_ptr == NULL) return;
    if (!omni_tensor_cuda_resolve()) return;
    omni_cuda_free(device_ptr);
}

int omni_tensor_backend_cublas_dgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const double alpha = 1.0;
    const double beta = 0.0;
    int status = omni_cublas_dgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const double*)right_device,
        (int)right_leading_dim,
        (const double*)left_device,
        (int)left_leading_dim,
        &beta,
        (double*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_sgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_float32_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const float alpha = 1.0f;
    const float beta = 0.0f;
    int status = omni_cublas_sgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const float*)right_device,
        (int)right_leading_dim,
        (const float*)left_device,
        (int)left_leading_dim,
        &beta,
        (float*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_zgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex128 alpha = { 1.0, 0.0 };
    const OmniTensorCudaComplex128 beta = { 0.0, 0.0 };
    int status = omni_cublas_zgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const OmniTensorCudaComplex128*)right_device,
        (int)right_leading_dim,
        (const OmniTensorCudaComplex128*)left_device,
        (int)left_leading_dim,
        &beta,
        (OmniTensorCudaComplex128*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}

int omni_tensor_backend_cublas_cgemm_rowmajor(
    size_t m,
    size_t n,
    size_t k,
    int trans_left,
    int trans_right,
    const void* left_device,
    size_t left_leading_dim,
    const void* right_device,
    size_t right_leading_dim,
    void* out_device,
    size_t out_leading_dim
) {
    if (m == 0 || n == 0 || k == 0) return OMNI_TENSOR_CUDA_SUCCESS;
    if (left_device == NULL || right_device == NULL || out_device == NULL) return OMNI_TENSOR_CUDA_INVALID;
    if (m > 2147483647u || n > 2147483647u || k > 2147483647u ||
        left_leading_dim > 2147483647u ||
        right_leading_dim > 2147483647u ||
        out_leading_dim > 2147483647u) {
        return OMNI_TENSOR_CUDA_INVALID;
    }
    if (!omni_tensor_backend_cublas_complex_available()) return OMNI_TENSOR_CUDA_UNAVAILABLE;

    void* handle = NULL;
    if (omni_cublas_create(&handle) != OMNI_CUBLAS_STATUS_SUCCESS || handle == NULL) {
        return OMNI_TENSOR_CUDA_UNAVAILABLE;
    }

    const OmniTensorCudaComplex64 alpha = { 1.0f, 0.0f };
    const OmniTensorCudaComplex64 beta = { 0.0f, 0.0f };
    int status = omni_cublas_cgemm(
        handle,
        trans_right ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        trans_left ? OMNI_CUBLAS_OP_T : OMNI_CUBLAS_OP_N,
        (int)n,
        (int)m,
        (int)k,
        &alpha,
        (const OmniTensorCudaComplex64*)right_device,
        (int)right_leading_dim,
        (const OmniTensorCudaComplex64*)left_device,
        (int)left_leading_dim,
        &beta,
        (OmniTensorCudaComplex64*)out_device,
        (int)out_leading_dim
    );
    int destroy_status = omni_cublas_destroy(handle);
    if (status != OMNI_CUBLAS_STATUS_SUCCESS || destroy_status != OMNI_CUBLAS_STATUS_SUCCESS) {
        return OMNI_TENSOR_CUDA_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_CUDA_SUCCESS;
}
