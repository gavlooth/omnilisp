module lisp;

const int OMNI_TENSOR_LAPACK_UNAVAILABLE = 0;
const int OMNI_TENSOR_LAPACK_SUCCESS = 1;
const int OMNI_TENSOR_LAPACK_SINGULAR = -1;
const int OMNI_TENSOR_LAPACK_INVALID = -2;
const int OMNI_TENSOR_LAPACK_OUT_OF_MEMORY = -3;
const int OMNI_TENSOR_LAPACK_NO_CONVERGENCE = -4;

extern fn int omni_tensor_backend_lapack_dgesv_available() @extern("omni_tensor_backend_lapack_dgesv_available");
extern fn long omni_tensor_backend_lapack_dgesv_call_count() @extern("omni_tensor_backend_lapack_dgesv_call_count");
extern fn int omni_tensor_backend_lapack_dgetrf_available() @extern("omni_tensor_backend_lapack_dgetrf_available");
extern fn long omni_tensor_backend_lapack_dgetrf_call_count() @extern("omni_tensor_backend_lapack_dgetrf_call_count");
extern fn int omni_tensor_backend_lapack_dpotrf_available() @extern("omni_tensor_backend_lapack_dpotrf_available");
extern fn long omni_tensor_backend_lapack_dpotrf_call_count() @extern("omni_tensor_backend_lapack_dpotrf_call_count");
extern fn int omni_tensor_backend_lapack_dgeqrf_available() @extern("omni_tensor_backend_lapack_dgeqrf_available");
extern fn long omni_tensor_backend_lapack_dgeqrf_call_count() @extern("omni_tensor_backend_lapack_dgeqrf_call_count");
extern fn int omni_tensor_backend_lapack_dsyev_available() @extern("omni_tensor_backend_lapack_dsyev_available");
extern fn long omni_tensor_backend_lapack_dsyev_call_count() @extern("omni_tensor_backend_lapack_dsyev_call_count");
extern fn int omni_tensor_backend_lapack_dgesvd_available() @extern("omni_tensor_backend_lapack_dgesvd_available");
extern fn long omni_tensor_backend_lapack_dgesvd_call_count() @extern("omni_tensor_backend_lapack_dgesvd_call_count");
extern fn int omni_tensor_backend_lapack_dgeev_available() @extern("omni_tensor_backend_lapack_dgeev_available");
extern fn long omni_tensor_backend_lapack_dgeev_call_count() @extern("omni_tensor_backend_lapack_dgeev_call_count");
extern fn void omni_tensor_backend_lapack_dgesv_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dgesv_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dgetrf_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dgetrf_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dpotrf_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dpotrf_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dgeqrf_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dgeqrf_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dsyev_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dsyev_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dgesvd_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dgesvd_disable_for_tests");
extern fn void omni_tensor_backend_lapack_dgeev_disable_for_tests(int disabled) @extern("omni_tensor_backend_lapack_dgeev_disable_for_tests");

extern fn int omni_tensor_backend_lapack_dgesv(
    usz n,
    usz nrhs,
    double* a,
    double* b
) @extern("omni_tensor_backend_lapack_dgesv");

extern fn int omni_tensor_backend_lapack_dgetrf(
    usz n,
    double* a,
    usz* pivots,
    usz* swap_count
) @extern("omni_tensor_backend_lapack_dgetrf");

extern fn int omni_tensor_backend_lapack_dpotrf(
    usz n,
    double* a
) @extern("omni_tensor_backend_lapack_dpotrf");

extern fn int omni_tensor_backend_lapack_dgeqrf(
    usz rows,
    usz cols,
    double* q,
    double* r
) @extern("omni_tensor_backend_lapack_dgeqrf");

extern fn int omni_tensor_backend_lapack_dsyev(
    usz n,
    double* eigenvectors,
    double* eigenvalues
) @extern("omni_tensor_backend_lapack_dsyev");

extern fn int omni_tensor_backend_lapack_dgesvd(
    usz rows,
    usz cols,
    double* a,
    double* u,
    double* s,
    double* v
) @extern("omni_tensor_backend_lapack_dgesvd");

extern fn int omni_tensor_backend_lapack_dgeev(
    usz n,
    double* a,
    double* wr,
    double* wi,
    double* vr
) @extern("omni_tensor_backend_lapack_dgeev");
