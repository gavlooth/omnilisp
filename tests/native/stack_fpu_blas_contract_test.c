#include <stdint.h>
#include <stdio.h>

void fpu_save(uint32_t* mxcsr, uint32_t* x87cw);
int omni_tensor_backend_blas_dger(
    size_t m,
    size_t n,
    double* x,
    double* y,
    double* out,
    size_t lda
);
int omni_tensor_backend_blas_partial_symbol_table_for_tests(void);
int omni_tensor_backend_lapack_partial_symbol_table_for_tests(void);
void omni_context_switch(void* old_ctx, void* new_ctx) {
    (void)old_ctx;
    (void)new_ctx;
}

static int test_fpu_save_allows_null_outputs(void) {
    uint32_t mxcsr = 0;
    uint32_t x87cw = 0;

    fpu_save(NULL, NULL);
    fpu_save(&mxcsr, NULL);
    fpu_save(NULL, &x87cw);

    return 1;
}

static int test_blas_dger_rejects_padded_lda_without_writing(void) {
    double x[2] = { 1.0, 2.0 };
    double y[3] = { 10.0, 20.0, 30.0 };
    double out[6] = { -1.0, -2.0, -3.0, -4.0, -5.0, -6.0 };

    int status = omni_tensor_backend_blas_dger(2, 3, x, y, out, 4);
    if (status != 0) {
        fprintf(stderr, "dger accepted padded lda without output capacity\n");
        return 0;
    }

    for (size_t i = 0; i < 6; i++) {
        if (out[i] != -1.0 - (double)i) {
            fprintf(stderr, "dger wrote output[%zu] before rejecting padded lda\n", i);
            return 0;
        }
    }
    return 1;
}

int main(void) {
    if (!test_fpu_save_allows_null_outputs()) return 1;
    if (!test_blas_dger_rejects_padded_lda_without_writing()) return 1;
    if (omni_tensor_backend_blas_partial_symbol_table_for_tests() != 1) {
        fprintf(stderr, "partial BLAS symbol table did not expose optional symbols\n");
        return 1;
    }
    if (omni_tensor_backend_lapack_partial_symbol_table_for_tests() != 1) {
        fprintf(stderr, "partial LAPACK symbol table did not expose independent symbols\n");
        return 1;
    }
    return 0;
}
