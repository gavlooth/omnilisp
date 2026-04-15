module lisp;

extern fn int omni_tensor_backend_blas_available() @extern("omni_tensor_backend_blas_available");
extern fn int omni_tensor_backend_blas_dgemv_available() @extern("omni_tensor_backend_blas_dgemv_available");
extern fn int omni_tensor_backend_blas_ddot_available() @extern("omni_tensor_backend_blas_ddot_available");
extern fn long omni_tensor_backend_blas_dgemm_call_count() @extern("omni_tensor_backend_blas_dgemm_call_count");
extern fn long omni_tensor_backend_blas_dgemv_call_count() @extern("omni_tensor_backend_blas_dgemv_call_count");
extern fn long omni_tensor_backend_blas_ddot_call_count() @extern("omni_tensor_backend_blas_ddot_call_count");

extern fn int omni_tensor_backend_blas_dgemm(
    usz m,
    usz n,
    usz k,
    int trans_a,
    int trans_b,
    double* a,
    usz lda,
    double* b,
    usz ldb,
    double* c,
    usz ldc
) @extern("omni_tensor_backend_blas_dgemm");

extern fn int omni_tensor_backend_blas_dgemv(
    usz m,
    usz n,
    int trans_a,
    double* a,
    usz lda,
    double* x,
    double* y
) @extern("omni_tensor_backend_blas_dgemv");

extern fn int omni_tensor_backend_blas_ddot(
    usz n,
    double* x,
    double* y,
    double* out
) @extern("omni_tensor_backend_blas_ddot");
