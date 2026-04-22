#include <dlfcn.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef int omni_lapack_int;
typedef int (*omni_lapacke_dgesv_fn)(
    int matrix_layout,
    omni_lapack_int n,
    omni_lapack_int nrhs,
    double* a,
    omni_lapack_int lda,
    omni_lapack_int* ipiv,
    double* b,
    omni_lapack_int ldb
);
typedef int (*omni_lapacke_dgetrf_fn)(
    int matrix_layout,
    omni_lapack_int m,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda,
    omni_lapack_int* ipiv
);
typedef int (*omni_lapacke_dpotrf_fn)(
    int matrix_layout,
    char uplo,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda
);
typedef int (*omni_lapacke_dgeqrf_fn)(
    int matrix_layout,
    omni_lapack_int m,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda,
    double* tau
);
typedef int (*omni_lapacke_dorgqr_fn)(
    int matrix_layout,
    omni_lapack_int m,
    omni_lapack_int n,
    omni_lapack_int k,
    double* a,
    omni_lapack_int lda,
    const double* tau
);
typedef int (*omni_lapacke_dsyev_fn)(
    int matrix_layout,
    char jobz,
    char uplo,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda,
    double* w
);
typedef int (*omni_lapacke_dgesvd_fn)(
    int matrix_layout,
    char jobu,
    char jobvt,
    omni_lapack_int m,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda,
    double* s,
    double* u,
    omni_lapack_int ldu,
    double* vt,
    omni_lapack_int ldvt,
    double* superb
);
typedef int (*omni_lapacke_dgeev_fn)(
    int matrix_layout,
    char jobvl,
    char jobvr,
    omni_lapack_int n,
    double* a,
    omni_lapack_int lda,
    double* wr,
    double* wi,
    double* vl,
    omni_lapack_int ldvl,
    double* vr,
    omni_lapack_int ldvr
);
typedef int (*omni_lapacke_sgeev_fn)(
    int matrix_layout,
    char jobvl,
    char jobvr,
    omni_lapack_int n,
    float* a,
    omni_lapack_int lda,
    float* wr,
    float* wi,
    float* vl,
    omni_lapack_int ldvl,
    float* vr,
    omni_lapack_int ldvr
);

typedef struct {
    double real;
    double imag;
} omni_lapack_complex128;

typedef struct {
    float real;
    float imag;
} omni_lapack_complex64;

typedef int (*omni_lapacke_zgeev_fn)(
    int matrix_layout,
    char jobvl,
    char jobvr,
    omni_lapack_int n,
    omni_lapack_complex128* a,
    omni_lapack_int lda,
    omni_lapack_complex128* w,
    omni_lapack_complex128* vl,
    omni_lapack_int ldvl,
    omni_lapack_complex128* vr,
    omni_lapack_int ldvr
);
typedef int (*omni_lapacke_cgeev_fn)(
    int matrix_layout,
    char jobvl,
    char jobvr,
    omni_lapack_int n,
    omni_lapack_complex64* a,
    omni_lapack_int lda,
    omni_lapack_complex64* w,
    omni_lapack_complex64* vl,
    omni_lapack_int ldvl,
    omni_lapack_complex64* vr,
    omni_lapack_int ldvr
);

enum {
    OMNI_LAPACK_ROW_MAJOR = 101,
    OMNI_TENSOR_LAPACK_UNAVAILABLE = 0,
    OMNI_TENSOR_LAPACK_SUCCESS = 1,
    OMNI_TENSOR_LAPACK_SINGULAR = -1,
    OMNI_TENSOR_LAPACK_INVALID = -2,
    OMNI_TENSOR_LAPACK_OUT_OF_MEMORY = -3,
    OMNI_TENSOR_LAPACK_NO_CONVERGENCE = -4
};

static void* omni_tensor_lapack_handle = NULL;
static omni_lapacke_dgesv_fn omni_tensor_lapacke_dgesv = NULL;
static omni_lapacke_dgetrf_fn omni_tensor_lapacke_dgetrf = NULL;
static omni_lapacke_dpotrf_fn omni_tensor_lapacke_dpotrf = NULL;
static omni_lapacke_dgeqrf_fn omni_tensor_lapacke_dgeqrf = NULL;
static omni_lapacke_dorgqr_fn omni_tensor_lapacke_dorgqr = NULL;
static omni_lapacke_dsyev_fn omni_tensor_lapacke_dsyev = NULL;
static omni_lapacke_dgesvd_fn omni_tensor_lapacke_dgesvd = NULL;
static omni_lapacke_dgeev_fn omni_tensor_lapacke_dgeev = NULL;
static omni_lapacke_sgeev_fn omni_tensor_lapacke_sgeev = NULL;
static omni_lapacke_zgeev_fn omni_tensor_lapacke_zgeev = NULL;
static omni_lapacke_cgeev_fn omni_tensor_lapacke_cgeev = NULL;
static int omni_tensor_lapack_resolution_attempted = 0;
static long omni_tensor_lapack_dgesv_calls = 0;
static long omni_tensor_lapack_dgetrf_calls = 0;
static long omni_tensor_lapack_dpotrf_calls = 0;
static long omni_tensor_lapack_dgeqrf_calls = 0;
static long omni_tensor_lapack_dsyev_calls = 0;
static long omni_tensor_lapack_dgesvd_calls = 0;
static long omni_tensor_lapack_dgeev_calls = 0;
static long omni_tensor_lapack_sgeev_calls = 0;
static long omni_tensor_lapack_zgeev_calls = 0;
static long omni_tensor_lapack_cgeev_calls = 0;
static int omni_tensor_lapack_dgesv_disabled_for_tests = 0;
static int omni_tensor_lapack_dgetrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dpotrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dgeqrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dsyev_disabled_for_tests = 0;
static int omni_tensor_lapack_dgesvd_disabled_for_tests = 0;
static int omni_tensor_lapack_dgeev_disabled_for_tests = 0;
static int omni_tensor_lapack_sgeev_disabled_for_tests = 0;
static int omni_tensor_lapack_zgeev_disabled_for_tests = 0;
static int omni_tensor_lapack_cgeev_disabled_for_tests = 0;

static int omni_tensor_backend_lapack_dgesv_disabled(void) {
    return omni_tensor_lapack_dgesv_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DGESV") != NULL;
}

static int omni_tensor_backend_lapack_dgetrf_disabled(void) {
    return omni_tensor_lapack_dgetrf_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DGETRF") != NULL;
}

static int omni_tensor_backend_lapack_dpotrf_disabled(void) {
    return omni_tensor_lapack_dpotrf_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DPOTRF") != NULL;
}

static int omni_tensor_backend_lapack_dgeqrf_disabled(void) {
    return omni_tensor_lapack_dgeqrf_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DGEQRF") != NULL;
}

static int omni_tensor_backend_lapack_dsyev_disabled(void) {
    return omni_tensor_lapack_dsyev_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DSYEV") != NULL;
}

static int omni_tensor_backend_lapack_dgesvd_disabled(void) {
    return omni_tensor_lapack_dgesvd_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DGESVD") != NULL;
}

static int omni_tensor_backend_lapack_dgeev_disabled(void) {
    return omni_tensor_lapack_dgeev_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_DGEEV") != NULL;
}

static int omni_tensor_backend_lapack_sgeev_disabled(void) {
    return omni_tensor_lapack_sgeev_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_SGEEV") != NULL;
}

static int omni_tensor_backend_lapack_zgeev_disabled(void) {
    return omni_tensor_lapack_zgeev_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_ZGEEV") != NULL;
}

static int omni_tensor_backend_lapack_cgeev_disabled(void) {
    return omni_tensor_lapack_cgeev_disabled_for_tests ||
        getenv("OMNI_TENSOR_DISABLE_LAPACK_CGEEV") != NULL;
}

static double omni_tensor_backend_lapack_abs(double value) {
    return value < 0.0 ? -value : value;
}

static void omni_tensor_backend_lapack_swap_columns(double* matrix, size_t n, size_t left, size_t right) {
    if (left == right) return;
    for (size_t row = 0; row < n; row++) {
        double tmp = matrix[row * n + left];
        matrix[row * n + left] = matrix[row * n + right];
        matrix[row * n + right] = tmp;
    }
}

static void omni_tensor_backend_lapack_normalize_eigenvector_signs(double* eigenvectors, size_t n) {
    for (size_t col = 0; col < n; col++) {
        size_t pivot_row = 0;
        double pivot_abs = 0.0;
        for (size_t row = 0; row < n; row++) {
            double candidate_abs = omni_tensor_backend_lapack_abs(eigenvectors[row * n + col]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (pivot_abs > 0.0 && eigenvectors[pivot_row * n + col] < 0.0) {
            for (size_t row = 0; row < n; row++) {
                eigenvectors[row * n + col] = -eigenvectors[row * n + col];
            }
        }
    }
}

static void omni_tensor_backend_lapack_sort_eigenpairs_desc(double* eigenvectors, double* eigenvalues, size_t n) {
    for (size_t i = 0; i < n; i++) {
        size_t best = i;
        double best_value = eigenvalues[i];
        for (size_t j = i + 1; j < n; j++) {
            if (eigenvalues[j] > best_value) {
                best = j;
                best_value = eigenvalues[j];
            }
        }
        if (best != i) {
            double tmp = eigenvalues[i];
            eigenvalues[i] = eigenvalues[best];
            eigenvalues[best] = tmp;
            omni_tensor_backend_lapack_swap_columns(eigenvectors, n, i, best);
        }
    }
}

static int omni_tensor_lapack_resolve(void) {
    if (omni_tensor_lapack_handle != NULL) return 1;
    if (omni_tensor_lapack_resolution_attempted) return 0;
    omni_tensor_lapack_resolution_attempted = 1;

    const char* candidates[] = {
        "liblapacke.so.3",
        "liblapacke.so",
        NULL
    };
    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;
        void* dgesv_symbol = dlsym(handle, "LAPACKE_dgesv");
        void* dgetrf_symbol = dlsym(handle, "LAPACKE_dgetrf");
        void* dpotrf_symbol = dlsym(handle, "LAPACKE_dpotrf");
        void* dgeqrf_symbol = dlsym(handle, "LAPACKE_dgeqrf");
        void* dorgqr_symbol = dlsym(handle, "LAPACKE_dorgqr");
        void* dsyev_symbol = dlsym(handle, "LAPACKE_dsyev");
        void* dgesvd_symbol = dlsym(handle, "LAPACKE_dgesvd");
        void* dgeev_symbol = dlsym(handle, "LAPACKE_dgeev");
        void* sgeev_symbol = dlsym(handle, "LAPACKE_sgeev");
        void* zgeev_symbol = dlsym(handle, "LAPACKE_zgeev");
        void* cgeev_symbol = dlsym(handle, "LAPACKE_cgeev");
        if (
            dgesv_symbol != NULL ||
            dgetrf_symbol != NULL ||
            dpotrf_symbol != NULL ||
            (dgeqrf_symbol != NULL && dorgqr_symbol != NULL) ||
            dsyev_symbol != NULL ||
            dgesvd_symbol != NULL ||
            dgeev_symbol != NULL ||
            sgeev_symbol != NULL ||
            zgeev_symbol != NULL ||
            cgeev_symbol != NULL
        ) {
            omni_tensor_lapack_handle = handle;
            omni_tensor_lapacke_dgesv = (omni_lapacke_dgesv_fn)dgesv_symbol;
            omni_tensor_lapacke_dgetrf = (omni_lapacke_dgetrf_fn)dgetrf_symbol;
            omni_tensor_lapacke_dpotrf = (omni_lapacke_dpotrf_fn)dpotrf_symbol;
            omni_tensor_lapacke_dgeqrf = (omni_lapacke_dgeqrf_fn)dgeqrf_symbol;
            omni_tensor_lapacke_dorgqr = (omni_lapacke_dorgqr_fn)dorgqr_symbol;
            omni_tensor_lapacke_dsyev = (omni_lapacke_dsyev_fn)dsyev_symbol;
            omni_tensor_lapacke_dgesvd = (omni_lapacke_dgesvd_fn)dgesvd_symbol;
            omni_tensor_lapacke_dgeev = (omni_lapacke_dgeev_fn)dgeev_symbol;
            omni_tensor_lapacke_sgeev = (omni_lapacke_sgeev_fn)sgeev_symbol;
            omni_tensor_lapacke_zgeev = (omni_lapacke_zgeev_fn)zgeev_symbol;
            omni_tensor_lapacke_cgeev = (omni_lapacke_cgeev_fn)cgeev_symbol;
            return 1;
        }
        dlclose(handle);
    }
    return 0;
}

int omni_tensor_backend_lapack_dgesv_available(void) {
    if (omni_tensor_backend_lapack_dgesv_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dgesv != NULL;
}

long omni_tensor_backend_lapack_dgesv_call_count(void) {
    return omni_tensor_lapack_dgesv_calls;
}

int omni_tensor_backend_lapack_dgetrf_available(void) {
    if (omni_tensor_backend_lapack_dgetrf_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dgetrf != NULL;
}

long omni_tensor_backend_lapack_dgetrf_call_count(void) {
    return omni_tensor_lapack_dgetrf_calls;
}

int omni_tensor_backend_lapack_dpotrf_available(void) {
    if (omni_tensor_backend_lapack_dpotrf_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dpotrf != NULL;
}

long omni_tensor_backend_lapack_dpotrf_call_count(void) {
    return omni_tensor_lapack_dpotrf_calls;
}

int omni_tensor_backend_lapack_dgeqrf_available(void) {
    if (omni_tensor_backend_lapack_dgeqrf_disabled()) return 0;
    return omni_tensor_lapack_resolve() &&
        omni_tensor_lapacke_dgeqrf != NULL &&
        omni_tensor_lapacke_dorgqr != NULL;
}

long omni_tensor_backend_lapack_dgeqrf_call_count(void) {
    return omni_tensor_lapack_dgeqrf_calls;
}

int omni_tensor_backend_lapack_dsyev_available(void) {
    if (omni_tensor_backend_lapack_dsyev_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dsyev != NULL;
}

long omni_tensor_backend_lapack_dsyev_call_count(void) {
    return omni_tensor_lapack_dsyev_calls;
}

int omni_tensor_backend_lapack_dgesvd_available(void) {
    if (omni_tensor_backend_lapack_dgesvd_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dgesvd != NULL;
}

long omni_tensor_backend_lapack_dgesvd_call_count(void) {
    return omni_tensor_lapack_dgesvd_calls;
}

int omni_tensor_backend_lapack_dgeev_available(void) {
    if (omni_tensor_backend_lapack_dgeev_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_dgeev != NULL;
}

long omni_tensor_backend_lapack_dgeev_call_count(void) {
    return omni_tensor_lapack_dgeev_calls;
}

int omni_tensor_backend_lapack_sgeev_available(void) {
    if (omni_tensor_backend_lapack_sgeev_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_sgeev != NULL;
}

long omni_tensor_backend_lapack_sgeev_call_count(void) {
    return omni_tensor_lapack_sgeev_calls;
}

int omni_tensor_backend_lapack_zgeev_available(void) {
    if (omni_tensor_backend_lapack_zgeev_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_zgeev != NULL;
}

long omni_tensor_backend_lapack_zgeev_call_count(void) {
    return omni_tensor_lapack_zgeev_calls;
}

int omni_tensor_backend_lapack_cgeev_available(void) {
    if (omni_tensor_backend_lapack_cgeev_disabled()) return 0;
    return omni_tensor_lapack_resolve() && omni_tensor_lapacke_cgeev != NULL;
}

long omni_tensor_backend_lapack_cgeev_call_count(void) {
    return omni_tensor_lapack_cgeev_calls;
}

void omni_tensor_backend_lapack_dgeev_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgeev_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_sgeev_disable_for_tests(int disabled) {
    omni_tensor_lapack_sgeev_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_zgeev_disable_for_tests(int disabled) {
    omni_tensor_lapack_zgeev_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_cgeev_disable_for_tests(int disabled) {
    omni_tensor_lapack_cgeev_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dgesv_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgesv_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dgetrf_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgetrf_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dpotrf_disable_for_tests(int disabled) {
    omni_tensor_lapack_dpotrf_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dgeqrf_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgeqrf_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dgesvd_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgesvd_disabled_for_tests = disabled != 0;
}

void omni_tensor_backend_lapack_dsyev_disable_for_tests(int disabled) {
    omni_tensor_lapack_dsyev_disabled_for_tests = disabled != 0;
}

int omni_tensor_backend_lapack_dgesv(
    size_t n,
    size_t nrhs,
    double* a,
    double* b
) {
    if (omni_tensor_backend_lapack_dgesv_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dgesv == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || b == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX || nrhs > (size_t)INT_MAX) {
        return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    }

    size_t ipiv_bytes = n * sizeof(omni_lapack_int);
    if (n != 0 && ipiv_bytes / sizeof(omni_lapack_int) != n) {
        return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }
    omni_lapack_int* ipiv = NULL;
    if (n > 0) {
        ipiv = (omni_lapack_int*)malloc(ipiv_bytes);
        if (ipiv == NULL) return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    omni_lapack_int lapack_nrhs = (omni_lapack_int)nrhs;
    int info = omni_tensor_lapacke_dgesv(
        OMNI_LAPACK_ROW_MAJOR,
        lapack_n,
        lapack_nrhs,
        a,
        lapack_n,
        ipiv,
        b,
        lapack_nrhs
    );
    free(ipiv);

    if (info == 0) {
        omni_tensor_lapack_dgesv_calls++;
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_SINGULAR;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_dgeqrf(
    size_t rows,
    size_t cols,
    double* q,
    double* r
) {
    if (omni_tensor_backend_lapack_dgeqrf_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dgeqrf == NULL || omni_tensor_lapacke_dorgqr == NULL) {
        return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    }
    if (q == NULL || r == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (cols > rows) return OMNI_TENSOR_LAPACK_INVALID;
    if (rows > (size_t)INT_MAX || cols > (size_t)INT_MAX) {
        return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    }

    size_t tau_bytes = cols * sizeof(double);
    size_t sign_bytes = cols * sizeof(double);
    if (cols != 0 && (tau_bytes / sizeof(double) != cols || sign_bytes / sizeof(double) != cols)) {
        return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }
    double* tau = NULL;
    double* signs = NULL;
    if (cols > 0) {
        tau = (double*)malloc(tau_bytes);
        if (tau == NULL) return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
        signs = (double*)malloc(sign_bytes);
        if (signs == NULL) {
            free(tau);
            return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
        }
    }

    omni_lapack_int lapack_rows = (omni_lapack_int)rows;
    omni_lapack_int lapack_cols = (omni_lapack_int)cols;
    int info = omni_tensor_lapacke_dgeqrf(
        OMNI_LAPACK_ROW_MAJOR,
        lapack_rows,
        lapack_cols,
        q,
        lapack_cols,
        tau
    );
    if (info != 0) {
        free(signs);
        free(tau);
        return info < 0 ? OMNI_TENSOR_LAPACK_INVALID : OMNI_TENSOR_LAPACK_SINGULAR;
    }

    for (size_t i = 0; i < cols * cols; i++) {
        r[i] = 0.0;
    }
    for (size_t row = 0; row < cols; row++) {
        double diagonal = q[row * cols + row];
        if (omni_tensor_backend_lapack_abs(diagonal) <= 1e-12) {
            free(signs);
            free(tau);
            return OMNI_TENSOR_LAPACK_SINGULAR;
        }
        signs[row] = diagonal < 0.0 ? -1.0 : 1.0;
        for (size_t col = row; col < cols; col++) {
            r[row * cols + col] = q[row * cols + col] * signs[row];
        }
    }

    info = omni_tensor_lapacke_dorgqr(
        OMNI_LAPACK_ROW_MAJOR,
        lapack_rows,
        lapack_cols,
        lapack_cols,
        q,
        lapack_cols,
        tau
    );
    free(tau);
    if (info != 0) {
        free(signs);
        return OMNI_TENSOR_LAPACK_INVALID;
    }

    for (size_t col = 0; col < cols; col++) {
        if (signs[col] < 0.0) {
            for (size_t row = 0; row < rows; row++) {
                q[row * cols + col] = -q[row * cols + col];
            }
        }
    }
    free(signs);

    omni_tensor_lapack_dgeqrf_calls++;
    return OMNI_TENSOR_LAPACK_SUCCESS;
}


#include "tensor_lapack_helpers_factorization.inc"
