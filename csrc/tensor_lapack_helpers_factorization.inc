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
        atomic_fetch_add_explicit(&omni_tensor_lapack_dsyev_calls, 1, memory_order_relaxed);
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

    atomic_fetch_add_explicit(&omni_tensor_lapack_dgesvd_calls, 1, memory_order_relaxed);
    return OMNI_TENSOR_LAPACK_SUCCESS;
}

int omni_tensor_backend_lapack_dgeev(
    size_t n,
    double* a,
    double* wr,
    double* wi,
    double* vr
) {
    if (omni_tensor_backend_lapack_dgeev_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
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
        atomic_fetch_add_explicit(&omni_tensor_lapack_dgeev_calls, 1, memory_order_relaxed);
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_NO_CONVERGENCE;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_sgeev(
    size_t n,
    float* a,
    float* wr,
    float* wi,
    float* vr
) {
    if (omni_tensor_backend_lapack_sgeev_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_sgeev == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || wr == NULL || wi == NULL || vr == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (n == 0) return OMNI_TENSOR_LAPACK_SUCCESS;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_sgeev(
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
        atomic_fetch_add_explicit(&omni_tensor_lapack_sgeev_calls, 1, memory_order_relaxed);
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_NO_CONVERGENCE;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_zgeev(
    size_t n,
    omni_lapack_complex128* a,
    omni_lapack_complex128* w,
    omni_lapack_complex128* vr
) {
    if (omni_tensor_backend_lapack_zgeev_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_zgeev == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || w == NULL || vr == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (n == 0) return OMNI_TENSOR_LAPACK_SUCCESS;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_zgeev(
        OMNI_LAPACK_ROW_MAJOR,
        'N',
        'V',
        lapack_n,
        a,
        lapack_n,
        w,
        NULL,
        1,
        vr,
        lapack_n
    );
    if (info == 0) {
        atomic_fetch_add_explicit(&omni_tensor_lapack_zgeev_calls, 1, memory_order_relaxed);
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_NO_CONVERGENCE;
    return OMNI_TENSOR_LAPACK_INVALID;
}

int omni_tensor_backend_lapack_cgeev(
    size_t n,
    omni_lapack_complex64* a,
    omni_lapack_complex64* w,
    omni_lapack_complex64* vr
) {
    if (omni_tensor_backend_lapack_cgeev_disabled()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (!omni_tensor_lapack_resolve()) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (omni_tensor_lapacke_cgeev == NULL) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (a == NULL || w == NULL || vr == NULL) return OMNI_TENSOR_LAPACK_INVALID;
    if (n > (size_t)INT_MAX) return OMNI_TENSOR_LAPACK_UNAVAILABLE;
    if (n == 0) return OMNI_TENSOR_LAPACK_SUCCESS;

    omni_lapack_int lapack_n = (omni_lapack_int)n;
    int info = omni_tensor_lapacke_cgeev(
        OMNI_LAPACK_ROW_MAJOR,
        'N',
        'V',
        lapack_n,
        a,
        lapack_n,
        w,
        NULL,
        1,
        vr,
        lapack_n
    );
    if (info == 0) {
        atomic_fetch_add_explicit(&omni_tensor_lapack_cgeev_calls, 1, memory_order_relaxed);
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
        atomic_fetch_add_explicit(&omni_tensor_lapack_dgetrf_calls, 1, memory_order_relaxed);
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
        atomic_fetch_add_explicit(&omni_tensor_lapack_dpotrf_calls, 1, memory_order_relaxed);
        return OMNI_TENSOR_LAPACK_SUCCESS;
    }
    if (info > 0) return OMNI_TENSOR_LAPACK_SINGULAR;
    return OMNI_TENSOR_LAPACK_INVALID;
}
