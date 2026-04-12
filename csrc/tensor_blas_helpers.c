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

static void* omni_tensor_blas_handle = NULL;
static omni_cblas_dgemm_fn omni_tensor_cblas_dgemm = NULL;
static int omni_tensor_blas_resolution_attempted = 0;
static long omni_tensor_blas_dgemm_calls = 0;

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

        void* symbol = dlsym(handle, "cblas_dgemm");
        if (symbol != NULL) {
            omni_tensor_blas_handle = handle;
            omni_tensor_cblas_dgemm = (omni_cblas_dgemm_fn)symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

int omni_tensor_backend_blas_available(void) {
    return omni_tensor_blas_resolve();
}

long omni_tensor_backend_blas_dgemm_call_count(void) {
    return omni_tensor_blas_dgemm_calls;
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
