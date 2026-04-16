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
static int omni_tensor_lapack_resolution_attempted = 0;
static long omni_tensor_lapack_dgesv_calls = 0;
static long omni_tensor_lapack_dgetrf_calls = 0;
static long omni_tensor_lapack_dpotrf_calls = 0;
static long omni_tensor_lapack_dgeqrf_calls = 0;
static long omni_tensor_lapack_dsyev_calls = 0;
static long omni_tensor_lapack_dgesvd_calls = 0;
static long omni_tensor_lapack_dgeev_calls = 0;
static int omni_tensor_lapack_dgesv_disabled_for_tests = 0;
static int omni_tensor_lapack_dgetrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dpotrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dgeqrf_disabled_for_tests = 0;
static int omni_tensor_lapack_dsyev_disabled_for_tests = 0;
static int omni_tensor_lapack_dgesvd_disabled_for_tests = 0;
static int omni_tensor_lapack_dgeev_disabled_for_tests = 0;

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
        if (
            dgesv_symbol != NULL ||
            dgetrf_symbol != NULL ||
            dpotrf_symbol != NULL ||
            (dgeqrf_symbol != NULL && dorgqr_symbol != NULL) ||
            dsyev_symbol != NULL ||
            dgesvd_symbol != NULL ||
            dgeev_symbol != NULL
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

void omni_tensor_backend_lapack_dgeev_disable_for_tests(int disabled) {
    omni_tensor_lapack_dgeev_disabled_for_tests = disabled != 0;
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

int omni_tensor_backend_lapack_dsyev(
    size_t n,
    double* eigenvectors,
    double* eigenvalues
) {
    if (omni_tensor_backend_lapack_dsyev_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dsyev == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (eigenvectors == NULL || eigenvalues == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_dsyev(
        OMNI_LAPACK_ROW_MAJOR,
        'V',
        'U',
        lapack_n,
        eigenvectors,
        lapack_n,
        eigenvalues
    );
    if (info == 0) {
        omni_tensor_backend_lapack_sort_eigenpairs_desc(eigenvectors, eigenvalues, n);
        omni_tensor_backend_lapack_normalize_eigenvector_signs(eigenvectors, n);
        for (size_t i = 0; i < n; i++) {
            if (eigenvalues[i] > -1e-12 && eigenvalues[i] < 1e-12) eigenvalues[i] = 0.0;
        }
        omni_tensor_lapack_dsyev_calls++;
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_NO_CONVERGENCE;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_dgesvd(
    size_t rows,
    size_t cols,
    double* a,
    double* u,
    double* s,
    double* v
) {
    if (omni_tensor_backend_lapack_dgesvd_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dgesvd == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || u == NULL || s == NULL || v == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (rows > (size_t)INT_MAX || cols > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;

    size_t k = rows < cols ? rows : cols;
    if (k == 0) return OMNI_TENSOR_LAPACK_SUCCESS;

    size_t vt_count = 0;
    if (k != 0 && cols > SIZE_MAX / k) return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    vt_count = k * cols;
    size_t vt_bytes = vt_count * sizeof(double);
    if (vt_count != 0 && vt_bytes / sizeof(double) != vt_count) {
        return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }
    size_t superb_count = k > 1 ? k - 1 : 1;
    size_t superb_bytes = superb_count * sizeof(double);
    if (superb_count != 0 && superb_bytes / sizeof(double) != superb_count) {
        return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }

    double* vt = (double*)malloc(vt_bytes);
    if (vt == NULL) return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    double* superb = (double*)malloc(superb_bytes);
    if (superb == NULL) {
        free(vt);
        return OMNI_TENSOR_LAPACK_OUT_OF_MEMORY;
    }

    omni_lapack_int lapack_rows = (omni_lapack_int)rows;
    omni_lapack_int lapack_cols = (omni_lapack_int)cols;
    omni_lapack_int lapack_k = (omni_lapack_int)k;
    int info = omni_tensor_lapacke_dgesvd(
        OMNI_LAPACK_ROW_MAJOR,
        'S',
        'S',
        lapack_rows,
        lapack_cols,
        a,
        lapack_cols,
        s,
        u,
        lapack_k,
        vt,
        lapack_cols,
        superb
    );
    free(superb);
    if (info != 0) {
        free(vt);
        return info > 0 ? OMNI_TENSOR_LAPACK_NO_CONVERGENCE : OMNI_TENSOR_LAPACK_INVALID;
    }

    for (size_t row = 0; row < cols; row++) {
        for (size_t col = 0; col < k; col++) {
            v[row * k + col] = vt[col * cols + row];
        }
    }
    free(vt);

    for (size_t col = 0; col < k; col++) {
        size_t pivot_row = 0;
        double pivot_abs = 0.0;
        for (size_t row = 0; row < rows; row++) {
            double candidate_abs = omni_tensor_backend_lapack_abs(u[row * k + col]);
            if (candidate_abs > pivot_abs) {
                pivot_abs = candidate_abs;
                pivot_row = row;
            }
        }
        if (pivot_abs > 0.0 && u[pivot_row * k + col] < 0.0) {
            for (size_t row = 0; row < rows; row++) {
                u[row * k + col] = -u[row * k + col];
            }
            for (size_t row = 0; row < cols; row++) {
                v[row * k + col] = -v[row * k + col];
            }
        }
    }

    omni_tensor_lapack_dgesvd_calls++;
    return OMNI_TENSOR_LAPACK_SUCCESS;
}

int omni_tensor_backend_lapack_dgeev(
    size_t n,
    double* a,
    double* wr,
    double* wi,
    double* vr
) {
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dgeev == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || wr == NULL || wi == NULL || vr == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (n == 0) return OMNI_TENSOR_LAPACK_SUCCESS;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_dgeev(
        OMNI_LAPACK_ROW_MAJOR,
        'N',
        'V',
        lapack_n,
        a,
        lapack_n,
        wr,
        wi,
        NULL,
        1,
        vr,
        lapack_n
    );
    if (info == 0) {
        omni_tensor_lapack_dgeev_calls++;
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_NO_CONVERGENCE;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_dgetrf(
    size_t n,
    double* a,
    size_t* pivots,
    size_t* swap_count
) {
    if (omni_tensor_backend_lapack_dgetrf_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dgetrf == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || pivots == NULL || swap_count == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;

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
    int info = omni_tensor_lapacke_dgetrf(
        OMNI_LAPACK_ROW_MAJOR,
        lapack_n,
        lapack_n,
        a,
        lapack_n,
        ipiv
    );
    if (info < 0) {
        free(ipiv);
        return OMNI_TENSOR_LAPACK_INVALID;
    }

    for (size_t i = 0; i < n; i++) {
        pivots[i] = i;
    }
    *swap_count = 0;
    for (size_t i = 0; i < n; i++) {
        if (ipiv[i] < 1 || ipiv[i] > lapack_n) {
            free(ipiv);
            return OMNI_TENSOR_LAPACK_INVALID;
        }
        size_t pivot_row = (size_t)(ipiv[i] - 1);
        if (pivot_row != i) {
            size_t tmp = pivots[i];
            pivots[i] = pivots[pivot_row];
            pivots[pivot_row] = tmp;
            *swap_count += 1;
        }
    }
    free(ipiv);

    if (info == 0) {
        omni_tensor_lapack_dgetrf_calls++;
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    return OMNI_TENSOR_LAPACK_SINGULAR;
}

int omni_tensor_backend_lapack_dpotrf(
    size_t n,
    double* a
) {
    if (omni_tensor_backend_lapack_dpotrf_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_dpotrf == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_dpotrf(
        OMNI_LAPACK_ROW_MAJOR,
        'L',
        lapack_n,
        a,
        lapack_n
    );
    if (info == 0) {
        for (size_t row = 0; row < n; row++) {
            for (size_t col = row + 1; col < n; col++) {
                a[row * n + col] = 0.0;
            }
        }
        omni_tensor_lapack_dpotrf_calls++;
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_SINGULAR;
    return OMNI_TENSOR_LAPACK_INVALID;
}
