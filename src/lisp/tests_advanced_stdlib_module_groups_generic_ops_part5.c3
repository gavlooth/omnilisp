module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part5(Interp* interp, int* pass, int* fail) {
    test_double(interp, "matrix norm Complex128 max",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'max)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex64 default frobenius",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0)]))",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex64 one",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'one)",
        7.0, pass, fail);
    test_double(interp, "matrix norm Complex64 infinity",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'infinity)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex64 max",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'max)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex128 spectral",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'spectral)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex128 nuclear",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'nuclear)",
        7.0, pass, fail);
    test_double(interp, "matrix norm Complex64 spectral",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'spectral)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex64 nuclear",
        "(matrix/norm (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'nuclear)",
        7.0, pass, fail);
    test_truthy(interp, "matrix singular-values CUDA Complex128 when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-singular-values-complex128) (let (x (to-device (Tensor Complex128 [2 2] [(Complex128 3 0) 0 0 (Complex128 2 0)]) 'cuda) s (matrix/singular-values x) cpu (to-device s 'cpu)) (and (= (device s) 'cuda) (= (String (dtype s)) \"Float64\") (= (ref (shape s) 0) 2) (= (ref cpu [0]) 3.0) (= (ref cpu [1]) 2.0))) true))", pass, fail);
    test_truthy(interp, "matrix norm CUDA Complex128 spectral nuclear when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-singular-values-complex128) (let (x (to-device (Tensor Complex128 [2 2] [(Complex128 3 0) 0 0 (Complex128 2 0)]) 'cuda)) (and (= (matrix/norm x 'spectral) 3.0) (= (matrix/norm x 'nuclear) 5.0))) true))", pass, fail);
    test_truthy(interp, "matrix svd CUDA Complex64 wide when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-svd-complex64) (let (x (to-device (Tensor Complex64 [2 3] [(Complex64 3 0) 0 0 0 (Complex64 2 0) 0]) 'cuda) svd (matrix/svd x) u (ref svd 'u) s (ref svd 's) v (ref svd 'v) scpu (to-device s 'cpu)) (and (= (device u) 'cuda) (= (device s) 'cuda) (= (device v) 'cuda) (= (String (dtype u)) \"Complex64\") (= (String (dtype s)) \"Float32\") (= (ref (shape u) 0) 2) (= (ref (shape u) 1) 2) (= (ref (shape v) 0) 3) (= (ref (shape v) 1) 2) (= (ref scpu [0]) (Float32 3.0)) (= (ref scpu [1]) (Float32 2.0)))) true))", pass, fail);
    {
        long z_calls_before = omni_tensor_backend_cusolver_zgesvd_call_count();
        long c_calls_before = omni_tensor_backend_cusolver_cgesvd_call_count();
        test_truthy(interp, "matrix CUDA complex SVD uses cuSOLVER counters when available",
            "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-singular-values-complex128) (let (s (matrix/singular-values (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) 0 0 (Complex128 1 0)]) 'cuda))) (= (device s) 'cuda)) true))",
            pass, fail);
        bool cuda_svd_available = omni_tensor_backend_cusolver_complex_svd_available() != 0 &&
            omni_tensor_backend_cuda_complex_matrix_available() != 0;
        bool counter_ok = !cuda_svd_available ||
            omni_tensor_backend_cusolver_zgesvd_call_count() > z_calls_before ||
            omni_tensor_backend_cusolver_cgesvd_call_count() > c_calls_before;
        if (counter_ok) {
            emit_pass_literal("[PASS] tensor CUDA complex SVD cuSOLVER counter path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CUDA complex SVD cuSOLVER counter path");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm CPU fixed complex reducers do not call Float64 LAPACK",
            "(and (= (matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)])) 5.0) (= (matrix/norm (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'one) 7.0))", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU fixed complex matrix norm reducers did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU fixed complex matrix norm reducers did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm CPU fixed complex spectral nuclear do not call Float64 LAPACK",
            "(and (= (matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'spectral) 5.0) (= (matrix/norm (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'nuclear) 7.0))", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU fixed complex matrix norm spectral/nuclear did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU fixed complex matrix norm spectral/nuclear did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm CPU Float32 spectral nuclear do not call Float64 LAPACK",
            "(and (= (matrix/norm (Tensor Float32 [2 2] [3 0 0 2]) 'spectral) 3.0) (= (matrix/norm (Tensor Float32 [2 2] [3 0 0 2]) 'nuclear) 5.0))", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU Float32 spectral/nuclear norms did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU Float32 spectral/nuclear norms did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix norm vulkan default frobenius when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 2] [3 4 0 0]) 'vulkan)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan one when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'one) 9.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan infinity when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'infinity) 15.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan max when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'max) 6.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (map + (to-device (Tensor Float64 [2 2] [3 4 0 0]) 'vulkan) 0.0)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 default frobenius when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 2] [3 4 0 0]) 'vulkan)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 one when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'one) 9.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 infinity when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'infinity) 15.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 max when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'max) 6.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (map + (to-device (Tensor Float32 [2 2] [3 4 0 0]) 'vulkan) 0.0)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 spectral when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 'spectral) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 nuclear when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 'nuclear) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 spectral realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (map + (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 0.0) 'spectral) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 nuclear wide when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [2 3] [3 0 0 0 2 0]) 'vulkan) 'nuclear) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 spectral empty rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [0 3] 0.0) 'vulkan) 'spectral) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 default copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 one copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'vulkan) 'one) 7.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 infinity copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'vulkan) 'infinity) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 max copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'vulkan) 'max) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 default copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 one copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'vulkan) 'one) 7.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 infinity copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'vulkan) 'infinity) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 max copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 0 0) (Complex64 3 4)]) 'vulkan) 'max) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 spectral when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan) 'spectral) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 nuclear when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan) 'nuclear) 7.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 spectral when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan) 'spectral) 5.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 nuclear when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan) 'nuclear) 7.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 spectral non diagonal lazy when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (< (abs (- (matrix/norm (map + (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 1) (Complex128 0 1) (Complex128 1 0)]) 'vulkan) (Complex128 0 0)) 'spectral) 1.4142135623730951)) 0.000001) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 nuclear wide when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 3] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0) (Complex64 0 0)]) 'vulkan) 'nuclear) 7.0) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex128 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex128) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (= (handle (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Complex64 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'matrix-numerical-complex64) false)) (= (handle (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 &&
            (omni_tensor_backend_vulkan_float64_available() != 0 || omni_tensor_backend_vulkan_float32_available() != 0);
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm vulkan fixed complex reducers do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/norm (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan) 'spectral) 5.0) true) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/norm (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan) 'nuclear) 7.0) true)))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan fixed complex matrix norm reducers do not call LAPACK"
                : "[PASS] tensor Vulkan fixed complex matrix norm reducers unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex matrix norm reducers do not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm vulkan Float32 reducers do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (if (= (matrix/norm (to-device (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'vulkan) 'max) 6.0) (= (matrix/norm (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 'spectral) 3.0) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix norm reducers do not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix norm reducers unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix norm reducers do not call LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix norm vulkan spectral when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 'spectral) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan nuclear when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 'nuclear) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0) 'spectral) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral wide when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 3] [3 0 0 0 2 0]) 'vulkan) 'spectral) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan nuclear wide when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 3] [3 0 0 0 2 0]) 'vulkan) 'nuclear) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral rank deficient zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [2 2] 0.0) 'vulkan) 'spectral) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral empty rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) 'spectral) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan nuclear empty rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) 'nuclear) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral empty columns when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [3 0] 0.0) 'vulkan) 'spectral) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan nuclear empty columns when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [3 0] 0.0) 'vulkan) 'nuclear) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan spectral over previous max k when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (matrix/norm (to-device (Tensor Float64 [65 65] 1.0) 'vulkan) 'spectral) 65.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan nuclear over previous max k when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (matrix/norm (to-device (Tensor Float64 [65 65] 1.0) 'vulkan) 'nuclear) 65.0)) 0.00001) true) true))", pass, fail);
    {
        bool zero_size_validation_ok = omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests() == OMNI_TENSOR_VULKAN_SUCCESS;
        if (zero_size_validation_ok) {
            emit_pass_literal("[PASS] tensor Vulkan matrix singular-norm zero-size validation");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix singular-norm zero-size validation");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm vulkan spectral nuclear do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (= (matrix/norm (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 'spectral) 3.0) (= (matrix/norm (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 'nuclear) 5.0) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix norm spectral/nuclear does not call LAPACK"
                : "[PASS] tensor Vulkan matrix norm spectral/nuclear unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix norm spectral/nuclear does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm vulkan over previous max k does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (matrix/norm (to-device (Tensor Float64 [65 65] 1.0) 'vulkan) 'spectral) 65.0)) 0.000001) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix norm over previous max k does not call LAPACK"
                : "[PASS] tensor Vulkan matrix norm unavailable; over previous max-k no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix norm over previous max k does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix norm vulkan zero-size spectral nuclear do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (= (matrix/norm (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) 'spectral) 0.0) (if (= (matrix/norm (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) 'nuclear) 0.0) (if (= (matrix/norm (to-device (Tensor Float64 [3 0] 0.0) 'vulkan) 'spectral) 0.0) (= (matrix/norm (to-device (Tensor Float64 [3 0] 0.0) 'vulkan) 'nuclear) 0.0) nil) nil) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan zero-size matrix norm spectral/nuclear does not call LAPACK"
                : "[PASS] tensor Vulkan zero-size matrix norm spectral/nuclear unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan zero-size matrix norm spectral/nuclear does not call LAPACK");
            *fail += 1;
        }
    }
    test_double(interp, "matrix norm spectral",
        "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'spectral)",
        3.0, pass, fail);
    test_double(interp, "matrix norm nuclear",
        "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'nuclear)",
        5.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesvd_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_double(interp, "matrix norm spectral LAPACK candidate",
            "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'spectral)",
            3.0, pass, fail);
        test_double(interp, "matrix norm nuclear LAPACK candidate",
            "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'nuclear)",
            5.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesvd_call_count() >= lapack_calls_before + 2;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesvd matrix norm spectral/nuclear path"
                : "[PASS] tensor LAPACK backend dgesvd unavailable; matrix norm spectral/nuclear fallback retained");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd matrix norm spectral/nuclear path");
            *fail += 1;
        }
    }
    {
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix norm spectral forced pure fallback",
            "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'spectral)",
            3.0, pass, fail);
        test_double(interp, "matrix norm nuclear forced pure fallback",
            "(matrix/norm (Tensor Float64 [2 2] [3 0 0 2]) 'nuclear)",
            5.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesvd_available() == 0;
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesvd disabled; matrix norm spectral/nuclear pure fallback path");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd disabled; matrix norm spectral/nuclear pure fallback path");
            *fail += 1;
        }
    }
    test_double(interp, "matrix norm empty",
        "(matrix/norm (Tensor Float64 [0 3] 0.0))",
        0.0, pass, fail);
    test_double(interp, "matrix norm Float32 empty",
        "(matrix/norm (Tensor Float32 [0 3] 0.0))",
        0.0, pass, fail);
    test_truthy(interp, "matrix norm vulkan empty when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/norm (to-device (Tensor Float64 [0 3] 0.0) 'vulkan)) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 empty rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [0 3] 0.0) 'vulkan)) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix norm vulkan Float32 empty columns when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/norm (to-device (Tensor Float32 [3 0] 0.0) 'vulkan)) 0.0) true) true))", pass, fail);
    test_double(interp, "matrix norm realizes lazy input",
        "(matrix/norm (map + (Tensor Float64 [2 2] [3 4 0 0]) 0.0))",
        5.0, pass, fail);
    test_double(interp, "matrix norm Float32 realizes lazy input",
        "(matrix/norm (map + (Tensor Float32 [2 2] [3 4 0 0]) 0.0))",
        5.0, pass, fail);
    test_error_contains(interp, "matrix norm rejects vector input",
        "(matrix/norm (Tensor Float64 [2] [1 2]))",
        "matrix/norm: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix norm rejects high precision dtype",
        "(matrix/norm (Tensor BigFloat [1 1] [1]))",
        "matrix/norm: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix norm rejects invalid selector",
        "(matrix/norm (Tensor Float64 [1 1] [1]) 'euclidean)",
        "matrix/norm: norm selector must be 'frobenius, 'one, 'infinity, 'max, 'spectral, or 'nuclear", pass, fail);
    test_eq_double(interp, "matrix trace Float64 value",
        "(matrix/trace (Tensor Float64 [2 2] [1 2 3 4]))",
        5.0, pass, fail);
    test_eq_double(interp, "matrix trace realizes lazy input",
        "(matrix/trace (map + (Tensor Float64 [2 2] [1 2 3 4]) 0.0))",
        5.0, pass, fail);
    test_truthy(interp, "matrix trace Float32 returns Float32 scalar",
        "(let (x (matrix/trace (Tensor Float32 [2 2] [1 2 3 4]))) (and (= x 5.0) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "matrix trace vulkan scalar result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/trace (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/trace (map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) 0.0)) 5.0) true) true))", pass, fail);
    test_eq_double(interp, "matrix trace empty square",
        "(matrix/trace (Tensor Float64 [0 0] 0.0))",
        0.0, pass, fail);
    test_truthy(interp, "matrix trace vulkan empty square when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/trace (to-device (Tensor Float64 [0 0] 0.0) 'vulkan)) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Float32 scalar result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (x (matrix/trace (to-device (Tensor Float32 [2 2] [1 2 3 4]) 'vulkan))) (and (= x 5.0) (= (type-of x) 'Float32))) true) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/trace (map + (to-device (Tensor Float32 [2 2] [1 2 3 4]) 'vulkan) 0.0)) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Float32 empty square when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/trace (to-device (Tensor Float32 [0 0] 0.0) 'vulkan)) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Complex128 scalar when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex128) (let (x (matrix/trace (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)]) 'vulkan))) (and (= (type-of x) 'Complex128) (= (String x) \"4.0+6.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Complex64 scalar when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex64) (let (x (matrix/trace (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'vulkan))) (and (= (type-of x) 'Complex64) (= (String x) \"4.0+6.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix trace vulkan Complex128 empty square when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex128) (= (String (matrix/trace (to-device (Tensor Complex128 [0 0] 0.0) 'vulkan))) \"0.0+0.0i\") true))", pass, fail);
    test_str_val(interp, "matrix trace BigInteger scalar",
        "(String (matrix/trace (Tensor BigInteger [2 2] [(BigInteger \"9223372036854775808\") 2 3 4])))",
        "9223372036854775812", pass, fail);
    test_str_val(interp, "matrix trace BigFloat scalar",
        "(String (matrix/trace (Tensor BigFloat [2 2] [(BigFloat \"1.25\") 2 3 (BigFloat \"2.5\")])))",
        "3.75", pass, fail);
    test_str_val(interp, "matrix trace BigComplex scalar",
        "(String (matrix/trace (Tensor BigComplex [2 2] [(BigComplex 1 2) 0 0 (BigComplex 3 4)])))",
        "4+6i", pass, fail);
    test_str_val(interp, "matrix trace Complex128 scalar",
        "(String (matrix/trace (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)])))",
        "4.0+6.0i", pass, fail);
    test_truthy(interp, "matrix transpose cuda Complex128 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex128) (let (result (matrix/transpose (to-device (Tensor Complex128 [2 1] [(Complex128 1 2) (Complex128 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex128\") (= (String (ref cpu [0 1])) \"3.0+4.0i\"))) (if (ref cuda-backend 'complex128) (= (handle (matrix/transpose (to-device (Tensor Complex128 [2 1] [(Complex128 1 2) (Complex128 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix diagonal cuda Complex128 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex128) (let (result (matrix/diagonal (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex128\") (= (String (ref cpu [1])) \"3.0+4.0i\"))) (if (ref cuda-backend 'complex128) (= (handle (matrix/diagonal (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix cuda Complex128 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex128) (let (result (matrix/diagonal-matrix (to-device (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex128\") (= (String (ref cpu [1 1])) \"3.0+4.0i\") (= (String (ref cpu [0 1])) \"0.0+0.0i\"))) (if (ref cuda-backend 'complex128) (= (handle (matrix/diagonal-matrix (to-device (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix trace cuda Complex128 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex128) (let (x (matrix/trace (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)]) 'cuda))) (and (= (type-of x) 'Complex128) (= (String x) \"4.0+6.0i\"))) (if (ref cuda-backend 'complex128) (= (handle (matrix/trace (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) 0 0 (Complex128 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix transpose cuda Complex64 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex64) (let (result (matrix/transpose (to-device (Tensor Complex64 [2 1] [(Complex64 1 2) (Complex64 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex64\") (= (String (ref cpu [0 1])) \"3.0+4.0i\"))) (if (ref cuda-backend 'complex64) (= (handle (matrix/transpose (to-device (Tensor Complex64 [2 1] [(Complex64 1 2) (Complex64 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix diagonal cuda Complex64 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex64) (let (result (matrix/diagonal (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex64\") (= (String (ref cpu [1])) \"3.0+4.0i\"))) (if (ref cuda-backend 'complex64) (= (handle (matrix/diagonal (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix cuda Complex64 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex64) (let (result (matrix/diagonal-matrix (to-device (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)]) 'cuda)) cpu (to-device result 'cpu)) (and (= (device result) 'cuda) (= (String (dtype result)) \"Complex64\") (= (String (ref cpu [1 1])) \"3.0+4.0i\") (= (String (ref cpu [0 1])) \"0.0+0.0i\"))) (if (ref cuda-backend 'complex64) (= (handle (matrix/diagonal-matrix (to-device (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "matrix trace cuda Complex64 follows structural capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'matrix-structural-complex64) (let (x (matrix/trace (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'cuda))) (and (= (type-of x) 'Complex64) (= (String x) \"4.0+6.0i\"))) (if (ref cuda-backend 'complex64) (= (handle (matrix/trace (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_error_contains(interp, "matrix trace rejects vector input",
        "(matrix/trace (Tensor Float64 [2] [1 2]))",
        "matrix/trace: expected a square rank-2 Tensor", pass, fail);
    test_error_contains(interp, "matrix trace rejects non-square input",
        "(matrix/trace (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/trace: expected a square rank-2 Tensor", pass, fail);
    test_eq_double(interp, "matrix solve vector rhs",
        "(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2])) [1])",
        0.6, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesv_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_eq_double(interp, "matrix solve LAPACK candidate",
            "(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2])) [0])",
            0.2, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesv_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesv matrix solve path"
                : "[PASS] tensor LAPACK backend dgesv unavailable; matrix solve fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesv matrix solve path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgesv_disable_for_tests(1);
        int fail_before = *fail;
        test_eq_double(interp, "matrix solve forced pure fallback",
            "(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2])) [0])",
            0.2, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesv_available() == 0;
        omni_tensor_backend_lapack_dgesv_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesv disabled; matrix solve pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesv disabled; matrix solve pure fallback path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix solve matrix rhs",
        "(ref (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2 2] [1 2 2 1])) [0 1])",
        1.0, pass, fail);
    test_eq_double(interp, "matrix solve realizes lazy coefficients",
        "(ref (matrix/solve (map + (Tensor Float64 [2 2] [2 1 1 3]) 0.0) (Tensor Float64 [2] [1 2])) [0])",
        0.2, pass, fail);
    test_str_val(interp, "matrix solve result dtype",
        "(String (dtype (matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [2] [1 2]))))",
        "Float64", pass, fail);
    test_truthy(interp, "matrix solve vulkan vector rhs first value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan vector rhs second value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [1]) 0.6)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan matrix rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2 2] [1 2 2 1]) 'vulkan)) 'cpu) [0 1]) 1.0)) 0.000000001) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long serial_calls_before = omni_tensor_backend_vulkan_solve_serial_call_count();
        long serial_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_call_count();
        long serial_multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_call_count();
        test_truthy(interp, "matrix solve vulkan serial threshold keeps 2x2 off parallel helper when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.000000001) true) true))", pass, fail);
        bool serial_threshold_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_call_count() > serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_call_count() == serial_parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_call_count() == serial_multi_dispatch_calls_before);
        if (serial_threshold_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix solve 2x2 stayed on serial helper"
                : "[PASS] tensor Vulkan matrix solve 2x2 serial threshold unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix solve 2x2 stayed on serial helper");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long serial_calls_before = omni_tensor_backend_vulkan_solve_serial_call_count();
        long parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_call_count();
        long multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_call_count();
        test_truthy(interp, "matrix solve vulkan measured threshold keeps 9x9 on serial helper when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (matrix/identity 9) 'vulkan) (to-device (Tensor Float64 [9] [1 2 3 4 5 6 7 8 9]) 'vulkan)) 'cpu) [8]) 9.0)) 0.000000001) true) true))", pass, fail);
        bool below_threshold_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_call_count() > serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_call_count() == parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_call_count() == multi_dispatch_calls_before);
        if (below_threshold_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix solve 9x9 stayed below measured parallel threshold"
                : "[PASS] tensor Vulkan matrix solve measured threshold unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix solve 9x9 stayed below measured parallel threshold");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long serial_calls_before = omni_tensor_backend_vulkan_solve_serial_call_count();
        long parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_call_count();
        long multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_call_count();
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan measured threshold enters parallel at 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (matrix/identity 65) 'vulkan) (to-device (Tensor Float64 [65] 1.0) 'vulkan)) 'cpu) [64]) 1.0)) 0.000000001) true) true))", pass, fail);
        bool threshold_parallel_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_call_count() == serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_call_count() > parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_call_count() > multi_dispatch_calls_before &&
             omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before);
        if (threshold_parallel_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix solve measured threshold entered parallel helper"
                : "[PASS] tensor Vulkan matrix solve measured threshold parallel entry unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix solve measured threshold entered parallel helper");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_call_count();
        long multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_call_count();
        long pivot_reduce_calls_before = omni_tensor_backend_vulkan_solve_pivot_reduce_call_count();
        long factor_stage_calls_before = omni_tensor_backend_vulkan_solve_factor_stage_call_count();
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan threshold-routed vector rhs when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [3 3] [3 1 0 1 4 1 0 2 5]) 'vulkan) (to-device (Tensor Float64 [3] [4 6 7]) 'vulkan)) 'cpu) [2]) 1.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed staged pivoting when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [3 3] [0 1 0 1 0 0 0 0 1]) 'vulkan) (to-device (Tensor Float64 [3] [2 3 4]) 'vulkan)) 'cpu) [0]) 3.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed vector preserves rank when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (rank (matrix/solve (to-device (Tensor Float64 [3 3] [3 1 0 1 4 1 0 2 5]) 'vulkan) (to-device (Tensor Float64 [3] [4 6 7]) 'vulkan))) 1) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed matrix rhs when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [3 3] [3 1 0 1 4 1 0 2 5]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 1 6 2 7 3]) 'vulkan)) 'cpu) [1 1]) 0.32653061224489793)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed staged copy beyond one workgroup when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (matrix/identity 9) 'vulkan) (to-device (Tensor Float64 [9 2] [1 10 2 11 3 12 4 13 5 14 6 15 7 16 8 17 9 18]) 'vulkan)) 'cpu) [8 1]) 18.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed dense 9x9 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [9 9] [2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 2]) 'vulkan) (to-device (Tensor Float64 [9] [10 10 10 10 10 10 10 10 10]) 'vulkan)) 'cpu) [8]) 1.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan parallel cross-workgroup pivot reduction when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (matrix/identity 65) 'vulkan) (to-device (Tensor Float64 [65] 1.0) 'vulkan)) 'cpu) [64]) 1.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan parallel factor staged dense 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (map + (matrix/identity 65) (Tensor Float64 [65 65] 1.0)) 'vulkan) (to-device (Tensor Float64 [65] 66.0) 'vulkan)) 'cpu) [64]) 1.0)) 0.000000001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed matrix preserves rank and shape when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (result (matrix/solve (to-device (Tensor Float64 [3 3] [3 1 0 1 4 1 0 2 5]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 1 6 2 7 3]) 'vulkan))) (and (= (rank result) 2) (= (ref (shape result) 0) 3) (= (ref (shape result) 1) 2))) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed rejects singular coefficients when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [3 3] [1 2 3 2 4 6 0 1 1]) 'vulkan) (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan threshold-routed rejects post-elimination singular coefficients when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [3 3] [1 2 3 2 4 6 3 6 9]) 'vulkan) (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan parallel rejects singular 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [65 65] 1.0) 'vulkan) (to-device (Tensor Float64 [65] 1.0) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
        bool parallel_path_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_parallel_call_count() > parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_call_count() > multi_dispatch_calls_before &&
             omni_tensor_backend_vulkan_solve_pivot_reduce_call_count() > pivot_reduce_calls_before &&
             omni_tensor_backend_vulkan_solve_factor_stage_call_count() > factor_stage_calls_before &&
             omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before);
        if (parallel_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix solve parallel path without LAPACK"
                : "[PASS] tensor Vulkan matrix solve parallel path unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix solve parallel path without LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix solve vulkan result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan realizes lazy coefficients when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (map + (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) 0.0) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan realizes lazy rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0.0)) 'cpu) [1]) 0.6)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan rejects singular coefficients when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [2 2] [1 2 2 4]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan rejects mismatched rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan rejects mixed rhs device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (Tensor Float64 [2] [1 2])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan empty square returns empty vector when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (matrix/solve (to-device (Tensor Float64 [0 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan))) (and (= (device s) 'vulkan) (= (rank s) 1) (= (ref (shape s) 0) 0))) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan empty matrix rhs returns empty matrix when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (matrix/solve (to-device (Tensor Float64 [0 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0 0] 0.0) 'vulkan))) (and (= (device s) 'vulkan) (= (rank s) 2) (= (ref (shape s) 0) 0) (= (ref (shape s) 1) 0))) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float64 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.000000001) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix solve does not call LAPACK"
                : "[PASS] tensor Vulkan matrix solve unavailable; no-LAPACK path not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix solve does not call LAPACK");
            (*fail)++;
        }
    }
    test_error_contains(interp, "matrix solve rejects singular coefficients",
        "(matrix/solve (Tensor Float64 [2 2] [1 2 2 4]) (Tensor Float64 [2] [1 2]))",
        "matrix/solve: coefficient matrix is singular", pass, fail);
    test_error_contains(interp, "matrix solve rejects mismatched rhs",
        "(matrix/solve (Tensor Float64 [2 2] [2 1 1 3]) (Tensor Float64 [3] [1 2 3]))",
        "matrix/solve: expected square Float64, Float32, Complex128, or Complex64 coefficient Tensor and matching rank-1 or rank-2 right-hand Tensor", pass, fail);
    test_error_contains(interp, "matrix solve rejects BigComplex dtype",
        "(matrix/solve (Tensor BigComplex [2 2] [1 0 0 1]) (Tensor BigComplex [2] [1 2]))",
        "matrix/solve: expected square Float64, Float32, Complex128, or Complex64 coefficient Tensor and matching rank-1 or rank-2 right-hand Tensor", pass, fail);
    test_eq_double(interp, "matrix lu combined lower factor",
        "(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])",
        0.5, pass, fail);
    test_eq_double(interp, "matrix lu combined upper factor",
        "(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 1])",
        -0.5, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgetrf_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_eq_double(interp, "matrix lu LAPACK candidate",
            "(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])",
            0.5, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgetrf_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgetrf matrix lu path"
                : "[PASS] tensor LAPACK backend dgetrf unavailable; matrix lu fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgetrf matrix lu path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgetrf_disable_for_tests(1);
        int fail_before = *fail;
        test_eq_double(interp, "matrix lu forced pure fallback",
            "(ref (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu) [1 0])",
            0.5, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgetrf_available() == 0;
        omni_tensor_backend_lapack_dgetrf_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgetrf disabled; matrix lu pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgetrf disabled; matrix lu pure fallback path");
            (*fail)++;
        }
    }
    test_eq(interp, "matrix lu pivot row order",
        "(ref (ref (matrix/lu (Tensor Float64 [2 2] [0 2 1 3])) 'pivots) 0)",
        1, pass, fail);
    test_eq(interp, "matrix lu swap count",
        "(ref (matrix/lu (Tensor Float64 [2 2] [0 2 1 3])) 'swap-count)",
        1, pass, fail);
    test_eq_double(interp, "matrix lu realizes lazy input",
        "(ref (ref (matrix/lu (map + (Tensor Float64 [2 2] [4 3 2 1]) 0.0)) 'lu) [0 1])",
        3.0, pass, fail);
    test_str_val(interp, "matrix lu result dtype",
        "(String (dtype (ref (matrix/lu (Tensor Float64 [2 2] [4 3 2 1])) 'lu)))",
        "Float64", pass, fail);
    test_truthy(interp, "matrix lu vulkan combined lower factor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (ref (matrix/lu (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) 'lu) 'cpu) [1 0]) 0.5) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan combined upper factor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (ref (matrix/lu (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) 'lu) 'cpu) [1 1]) -0.5) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan pivot row order when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (ref (matrix/lu (to-device (Tensor Float64 [2 2] [0 2 1 3]) 'vulkan)) 'pivots) 0) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan swap count when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (matrix/lu (to-device (Tensor Float64 [2 2] [0 2 1 3]) 'vulkan)) 'swap-count) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (ref (matrix/lu (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) 'lu)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (ref (matrix/lu (map + (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan) 0.0)) 'lu) 'cpu) [0 1]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan empty square stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (and (= (device (ref (matrix/lu (to-device (Tensor Float64 [0 0] 0.0) 'vulkan)) 'lu)) 'vulkan) (= (ref (shape (ref (matrix/lu (to-device (Tensor Float64 [0 0] 0.0) 'vulkan)) 'lu)) 0) 0)) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan rejects singular coefficients when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/lu (to-device (Tensor Float64 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan rejects non-square input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/lu (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_truthy(interp, "matrix lu vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (ref (matrix/lu (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) 'lu) 'cpu) [1 0]) 0.5) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgetrf_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix lu does not call LAPACK"
                : "[PASS] tensor Vulkan matrix lu unavailable; no-LAPACK path not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix lu does not call LAPACK");
            (*fail)++;
        }
    }
    test_error_contains(interp, "matrix lu rejects singular coefficients",
        "(matrix/lu (Tensor Float64 [2 2] [1 2 2 4]))",
        "matrix/lu: coefficient matrix is singular", pass, fail);
    test_error_contains(interp, "matrix lu rejects non-square input",
        "(matrix/lu (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/lu: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix lu rejects BigComplex dtype",
        "(matrix/lu (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/lu: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_eq_double(interp, "matrix determinant from lu factors",
        "(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))",
        -2.0, pass, fail);
    test_truthy(interp, "matrix determinant vulkan from lu factors when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) -2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan pivot parity when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (to-device (Tensor Float64 [2 2] [0 2 1 3]) 'vulkan)) -2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan singular zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (to-device (Tensor Float64 [2 2] [1 2 2 4]) 'vulkan)) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (map + (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan) 0.0)) -2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan empty square identity when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (to-device (Tensor Float64 [0 0] 0.0) 'vulkan)) 1.0) true) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan rejects non-square input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/determinant (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
}
