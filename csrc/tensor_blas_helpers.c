#include <dlfcn.h>
#include <limits.h>
#include <stddef.h>

#define OMNI_CBLAS_ROW_MAJOR 101
#define OMNI_CBLAS_NO_TRANS 111
#define OMNI_CBLAS_TRANS 112

typedef void (*omni_cblas_dgemm_fn)(
    int order,
    int trans_a,
    int trans_b,
    int m,
    int n,
    int k,
    double alpha,
    const double* a,
    int lda,
    const double* b,
    int ldb,
    double beta,
    double* c,
    int ldc
);

typedef void (*omni_cblas_dgemv_fn)(
    int order,
    int trans_a,
    int m,
    int n,
    double alpha,
    const double* a,
    int lda,
    const double* x,
    int incx,
    double beta,
    double* y,
    int incy
);

typedef double (*omni_cblas_ddot_fn)(
    int n,
    const double* x,
    int incx,
    const double* y,
    int incy
);

typedef void (*omni_cblas_dger_fn)(
    int order,
    int m,
    int n,
    double alpha,
    const double* x,
    int incx,
    const double* y,
    int incy,
    double* a,
    int lda
);

static void* omni_tensor_blas_handle = NULL;
static omni_cblas_dgemm_fn omni_tensor_cblas_dgemm = NULL;
static omni_cblas_dgemv_fn omni_tensor_cblas_dgemv = NULL;
static omni_cblas_ddot_fn omni_tensor_cblas_ddot = NULL;
static omni_cblas_dger_fn omni_tensor_cblas_dger = NULL;
static int omni_tensor_blas_resolution_attempted = 0;
static long omni_tensor_blas_dgemm_calls = 0;
static long omni_tensor_blas_dgemv_calls = 0;
static long omni_tensor_blas_ddot_calls = 0;
static long omni_tensor_blas_dger_calls = 0;

static int omni_tensor_blas_resolve(void) {
    if (omni_tensor_cblas_dgemm != NULL) return 1;
    if (omni_tensor_blas_resolution_attempted) return 0;
    omni_tensor_blas_resolution_attempted = 1;

    const char* candidates[] = {
        "libopenblas.so.0",
        "libopenblas.so",
        "libblas.so.3",
        "libblas.so",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* dgemm_symbol = dlsym(handle, "cblas_dgemm");
        void* dgemv_symbol = dlsym(handle, "cblas_dgemv");
        void* ddot_symbol = dlsym(handle, "cblas_ddot");
        void* dger_symbol = dlsym(handle, "cblas_dger");
        if (dgemm_symbol != NULL) {
            omni_tensor_blas_handle = handle;
            omni_tensor_cblas_dgemm = (omni_cblas_dgemm_fn)dgemm_symbol;
            if (dgemv_symbol != NULL) {
                omni_tensor_cblas_dgemv = (omni_cblas_dgemv_fn)dgemv_symbol;
            }
            if (ddot_symbol != NULL) {
                omni_tensor_cblas_ddot = (omni_cblas_ddot_fn)ddot_symbol;
            }
            if (dger_symbol != NULL) {
                omni_tensor_cblas_dger = (omni_cblas_dger_fn)dger_symbol;
            }
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

int omni_tensor_backend_blas_available(void) {
    return omni_tensor_blas_resolve();
}

int omni_tensor_backend_blas_dgemv_available(void) {
    return omni_tensor_blas_resolve() && omni_tensor_cblas_dgemv != NULL;
}

int omni_tensor_backend_blas_ddot_available(void) {
    return omni_tensor_blas_resolve() && omni_tensor_cblas_ddot != NULL;
}

int omni_tensor_backend_blas_dger_available(void) {
    return omni_tensor_blas_resolve() && omni_tensor_cblas_dger != NULL;
}

long omni_tensor_backend_blas_dgemm_call_count(void) {
    return omni_tensor_blas_dgemm_calls;
}

long omni_tensor_backend_blas_dgemv_call_count(void) {
    return omni_tensor_blas_dgemv_calls;
}

long omni_tensor_backend_blas_ddot_call_count(void) {
    return omni_tensor_blas_ddot_calls;
}

long omni_tensor_backend_blas_dger_call_count(void) {
    return omni_tensor_blas_dger_calls;
}

int omni_tensor_backend_blas_dgemm(
    size_t m,
    size_t n,
    size_t k,
    int trans_a,
    int trans_b,
    double* a,
    size_t lda,
    double* b,
    size_t ldb,
    double* c,
    size_t ldc
) {
    if (a == NULL || b == NULL || c == NULL) return 0;
    if (m == 0 || n == 0 || k == 0) return 0;
    if (m > INT_MAX || n > INT_MAX || k > INT_MAX ||
        lda > INT_MAX || ldb > INT_MAX || ldc > INT_MAX) {
        return 0;
    }
    if (!omni_tensor_blas_resolve()) return 0;

    omni_tensor_cblas_dgemm(
        OMNI_CBLAS_ROW_MAJOR,
        trans_a != 0 ? OMNI_CBLAS_TRANS : OMNI_CBLAS_NO_TRANS,
        trans_b != 0 ? OMNI_CBLAS_TRANS : OMNI_CBLAS_NO_TRANS,
        (int)m,
        (int)n,
        (int)k,
        1.0,
        a,
        (int)lda,
        b,
        (int)ldb,
        0.0,
        c,
        (int)ldc
    );
    omni_tensor_blas_dgemm_calls++;
    return 1;
}

int omni_tensor_backend_blas_dgemv(
    size_t m,
    size_t n,
    int trans_a,
    double* a,
    size_t lda,
    double* x,
    double* y
) {
    if (a == NULL || x == NULL || y == NULL) return 0;
    if (m == 0 || n == 0) return 0;
    if (m > INT_MAX || n > INT_MAX || lda > INT_MAX) return 0;
    if (!omni_tensor_blas_resolve() || omni_tensor_cblas_dgemv == NULL) return 0;

    omni_tensor_cblas_dgemv(
        OMNI_CBLAS_ROW_MAJOR,
        trans_a != 0 ? OMNI_CBLAS_TRANS : OMNI_CBLAS_NO_TRANS,
        (int)m,
        (int)n,
        1.0,
        a,
        (int)lda,
        x,
        1,
        0.0,
        y,
        1
    );
    omni_tensor_blas_dgemv_calls++;
    return 1;
}

int omni_tensor_backend_blas_ddot(
    size_t n,
    double* x,
    double* y,
    double* out
) {
    if (x == NULL || y == NULL || out == NULL) return 0;
    if (n == 0 || n > INT_MAX) return 0;
    if (!omni_tensor_blas_resolve() || omni_tensor_cblas_ddot == NULL) return 0;

    *out = omni_tensor_cblas_ddot((int)n, x, 1, y, 1);
    omni_tensor_blas_ddot_calls++;
    return 1;
}

int omni_tensor_backend_blas_dger(
    size_t m,
    size_t n,
    double* x,
    double* y,
    double* out,
    size_t lda
) {
    if (x == NULL || y == NULL || out == NULL) return 0;
    if (m == 0 || n == 0) return 0;
    if (m > INT_MAX || n > INT_MAX || lda > INT_MAX) return 0;
    if (!omni_tensor_blas_resolve() || omni_tensor_cblas_dger == NULL) return 0;

    size_t count = m * lda;
    for (size_t i = 0; i < count; i++) out[i] = 0.0;
    omni_tensor_cblas_dger(
        OMNI_CBLAS_ROW_MAJOR,
        (int)m,
        (int)n,
        1.0,
        x,
        1,
        y,
        1,
        out,
        (int)lda
    );
    omni_tensor_blas_dger_calls++;
    return 1;
}
