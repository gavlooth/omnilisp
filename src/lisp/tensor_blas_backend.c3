module lisp;

extern fn int omni_tensor_backend_blas_available() @extern("omni_tensor_backend_blas_available");
extern fn long omni_tensor_backend_blas_dgemm_call_count() @extern("omni_tensor_backend_blas_dgemm_call_count");

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
