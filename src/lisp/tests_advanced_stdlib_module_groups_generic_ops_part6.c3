module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part6(Interp* interp, int* pass, int* fail) {
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_truthy(interp, "matrix determinant vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/determinant (to-device (Tensor Float64 [2 2] [4 3 2 1]) 'vulkan)) -2.0) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgetrf_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix determinant does not call LAPACK"
                : "[PASS] tensor Vulkan matrix determinant unavailable; no-LAPACK path not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix determinant does not call LAPACK");
            (*fail)++;
        }
    }
    {
        bool lapack_available = omni_tensor_backend_lapack_dgetrf_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_eq_double(interp, "matrix determinant LAPACK candidate",
            "(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))",
            -2.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgetrf_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgetrf matrix determinant path"
                : "[PASS] tensor LAPACK backend dgetrf unavailable; matrix determinant fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgetrf matrix determinant path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgetrf_disable_for_tests(1);
        int fail_before = *fail;
        test_eq_double(interp, "matrix determinant forced pure fallback",
            "(matrix/determinant (Tensor Float64 [2 2] [4 3 2 1]))",
            -2.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgetrf_available() == 0;
        omni_tensor_backend_lapack_dgetrf_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgetrf disabled; matrix determinant pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgetrf disabled; matrix determinant pure fallback path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix determinant pivot parity",
        "(matrix/determinant (Tensor Float64 [2 2] [0 2 1 3]))",
        -2.0, pass, fail);
    test_eq_double(interp, "matrix determinant singular zero",
        "(matrix/determinant (Tensor Float64 [2 2] [1 2 2 4]))",
        0.0, pass, fail);
    test_eq_double(interp, "matrix determinant realizes lazy input",
        "(matrix/determinant (map + (Tensor Float64 [2 2] [4 3 2 1]) 0.0))",
        -2.0, pass, fail);
    test_eq_double(interp, "matrix determinant empty square identity",
        "(matrix/determinant (Tensor Float64 [0 0] 0.0))",
        1.0, pass, fail);
    test_error_contains(interp, "matrix determinant rejects non-square input",
        "(matrix/determinant (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/determinant: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix determinant rejects BigComplex dtype",
        "(matrix/determinant (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/determinant: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_double(interp, "matrix inverse first entry",
        "(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 0])",
        0.6, pass, fail);
    test_double(interp, "matrix inverse off diagonal",
        "(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [0 1])",
        -0.7, pass, fail);
    test_double(interp, "matrix inverse product identity",
        "(ref (realize (contract (Tensor Float64 [2 2] [4 7 2 6]) (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [1 0])) [1 1])",
        1.0, pass, fail);
    test_truthy(interp, "matrix inverse vulkan first entry when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/inverse (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [0 0]) 0.6)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan off diagonal when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/inverse (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [0 1]) -0.7)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan product identity when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (contract (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan) (matrix/inverse (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan)) [1 0]) 'cpu) [1 1]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/inverse (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/inverse (map + (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan) 0.0)) 'cpu) [1 1]) 0.4)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan empty square stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (and (= (device (matrix/inverse (to-device (Tensor Float64 [0 0] 0.0) 'vulkan))) 'vulkan) (= (ref (shape (matrix/inverse (to-device (Tensor Float64 [0 0] 0.0) 'vulkan))) 0) 0)) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan rejects singular coefficients when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/inverse (to-device (Tensor Float64 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan rejects non-square input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/inverse (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix inverse vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/inverse (to-device (Tensor Float64 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [1 0]) -0.2)) 0.000000001) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix inverse does not call LAPACK"
                : "[PASS] tensor Vulkan matrix inverse unavailable; no-LAPACK path not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix inverse does not call LAPACK");
            (*fail)++;
        }
    }
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesv_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_double(interp, "matrix inverse LAPACK candidate",
            "(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [1 0])",
            -0.2, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesv_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesv matrix inverse path"
                : "[PASS] tensor LAPACK backend dgesv unavailable; matrix inverse fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesv matrix inverse path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgesv_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix inverse forced pure fallback",
            "(ref (matrix/inverse (Tensor Float64 [2 2] [4 7 2 6])) [1 0])",
            -0.2, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesv_available() == 0;
        omni_tensor_backend_lapack_dgesv_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesv disabled; matrix inverse pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesv disabled; matrix inverse pure fallback path");
            (*fail)++;
        }
    }
    test_double(interp, "matrix inverse lazy input",
        "(ref (matrix/inverse (map + (Tensor Float64 [2 2] [4 7 2 6]) 0.0)) [1 1])",
        0.4, pass, fail);
    test_double(interp, "matrix inverse empty square shape",
        "(ref (shape (matrix/inverse (Tensor Float64 [0 0] 0.0))) 0)",
        0.0, pass, fail);
    test_error_contains(interp, "matrix inverse rejects singular",
        "(matrix/inverse (Tensor Float64 [2 2] [1 2 2 4]))",
        "matrix/inverse: input matrix is singular", pass, fail);
    test_error_contains(interp, "matrix inverse rejects non-square",
        "(matrix/inverse (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/inverse: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix inverse rejects complex dtype",
        "(matrix/inverse (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/inverse: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_eq_double(interp, "matrix qr q first column",
        "(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [0 0])",
        1.0, pass, fail);
    test_eq_double(interp, "matrix qr q second column",
        "(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [1 1])",
        1.0, pass, fail);
    test_eq_double(interp, "matrix qr r projection",
        "(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r) [0 1])",
        1.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgeqrf_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        test_eq_double(interp, "matrix qr LAPACK candidate",
            "(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [0 0])",
            1.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgeqrf_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgeqrf matrix qr path"
                : "[PASS] tensor LAPACK backend dgeqrf unavailable; matrix qr fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgeqrf matrix qr path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgeqrf_disable_for_tests(1);
        int fail_before = *fail;
        test_eq_double(interp, "matrix qr forced pure fallback",
            "(ref (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q) [0 0])",
            1.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgeqrf_available() == 0;
        omni_tensor_backend_lapack_dgeqrf_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgeqrf disabled; matrix qr pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgeqrf disabled; matrix qr pure fallback path");
            (*fail)++;
        }
    }
    test_eq(interp, "matrix qr q row count",
        "(ref (shape (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'q)) 0)",
        3, pass, fail);
    test_eq(interp, "matrix qr r column count",
        "(ref (shape (ref (matrix/qr (Tensor Float64 [3 2] [1 1 0 1 0 0])) 'r)) 1)",
        2, pass, fail);
    test_eq_double(interp, "matrix qr realizes lazy input",
        "(ref (ref (matrix/qr (map + (Tensor Float64 [3 2] [1 1 0 1 0 0]) 0.0)) 'r) [1 1])",
        1.0, pass, fail);
    test_error_contains(interp, "matrix qr rejects wide input",
        "(matrix/qr (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/qr: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns", pass, fail);
    test_error_contains(interp, "matrix qr rejects rank deficient input",
        "(matrix/qr (Tensor Float64 [2 2] [1 2 2 4]))",
        "matrix/qr: input matrix is rank deficient", pass, fail);
    test_error_contains(interp, "matrix qr rejects complex dtype",
        "(matrix/qr (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/qr: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor with rows >= columns", pass, fail);
    test_truthy(interp, "matrix qr empty shape",
        "(let (qr (matrix/qr (Tensor Float64 [0 0] 0.0))) (and (= (ref (shape (ref qr 'q)) 0) 0) (= (ref (shape (ref qr 'r)) 0) 0)))", pass, fail);
    test_truthy(interp, "matrix qr vulkan q first column when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'q) 'cpu) [0 0]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan q second column when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'q) 'cpu) [1 1]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan r projection when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'r) 'cpu) [0 1]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (and (= (device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'q)) 'vulkan) (= (device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'r)) 'vulkan)) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (qr (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan))) (and (= (ref (shape (ref qr 'q)) 0) 3) (= (ref (shape (ref qr 'q)) 1) 2) (= (ref (shape (ref qr 'r)) 0) 2) (= (ref (shape (ref qr 'r)) 1) 2))) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/qr (map + (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan) 0.0)) 'r) 'cpu) [1 1]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan empty result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (qr (matrix/qr (to-device (Tensor Float64 [0 0] 0.0) 'vulkan))) (and (= (device (ref qr 'q)) 'vulkan) (= (device (ref qr 'r)) 'vulkan) (= (ref (shape (ref qr 'q)) 0) 0) (= (ref (shape (ref qr 'r)) 0) 0))) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan rejects wide input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/qr (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan rejects rank deficient input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/qr (to-device (Tensor Float64 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        test_truthy(interp, "matrix qr vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float64 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'r) 'cpu) [1 1]) 1.0)) 0.000000001) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dgeqrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix qr path did not call LAPACK"
                : "[PASS] tensor Vulkan matrix qr path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix qr path did not call LAPACK");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix cholesky diagonal",
        "(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])",
        2.0, pass, fail);
    test_eq_double(interp, "matrix cholesky lower entry",
        "(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [1 0])",
        1.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dpotrf_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_eq_double(interp, "matrix cholesky LAPACK candidate",
            "(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])",
            2.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dpotrf_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dpotrf matrix cholesky path"
                : "[PASS] tensor LAPACK backend dpotrf unavailable; matrix cholesky fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dpotrf matrix cholesky path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dpotrf_disable_for_tests(1);
        int fail_before = *fail;
        test_eq_double(interp, "matrix cholesky forced pure fallback",
            "(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 0])",
            2.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dpotrf_available() == 0;
        omni_tensor_backend_lapack_dpotrf_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dpotrf disabled; matrix cholesky pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dpotrf disabled; matrix cholesky pure fallback path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix cholesky upper zero",
        "(ref (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2])) [0 1])",
        0.0, pass, fail);
    test_eq_double(interp, "matrix cholesky realizes lazy input",
        "(ref (matrix/cholesky (map + (Tensor Float64 [2 2] [4 2 2 2]) 0.0)) [1 1])",
        1.0, pass, fail);
    test_eq(interp, "matrix cholesky shape",
        "(ref (shape (matrix/cholesky (Tensor Float64 [2 2] [4 2 2 2]))) 0)",
        2, pass, fail);
    test_eq(interp, "matrix cholesky empty shape",
        "(ref (shape (matrix/cholesky (Tensor Float64 [0 0] 0.0))) 0)",
        0, pass, fail);
    test_error_contains(interp, "matrix cholesky rejects non-square input",
        "(matrix/cholesky (Tensor Float64 [2 3] [1 2 3 4 5 6]))",
        "matrix/cholesky: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix cholesky rejects nonsymmetric input",
        "(matrix/cholesky (Tensor Float64 [2 2] [1 2 3 4]))",
        "matrix/cholesky: input matrix is not symmetric positive definite", pass, fail);
    test_error_contains(interp, "matrix cholesky rejects indefinite input",
        "(matrix/cholesky (Tensor Float64 [2 2] [1 2 2 1]))",
        "matrix/cholesky: input matrix is not symmetric positive definite", pass, fail);
    test_error_contains(interp, "matrix cholesky rejects complex dtype",
        "(matrix/cholesky (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/cholesky: expected a square rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan lower entry when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/cholesky (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [1 0]) 1.0) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan diagonal when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/cholesky (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [0 0]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan upper zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/cholesky (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [0 1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/cholesky (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/cholesky (map + (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan empty result stays on device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/cholesky (to-device (Tensor Float64 [0 0] 0.0) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan rejects non-square input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/cholesky (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan rejects nonsymmetric input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/cholesky (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan rejects indefinite input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/cholesky (to-device (Tensor Float64 [2 2] [1 2 2 1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_truthy(interp, "matrix cholesky vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/cholesky (to-device (Tensor Float64 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [1 1]) 1.0) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dpotrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix cholesky path did not call LAPACK"
                : "[PASS] tensor Vulkan matrix cholesky path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix cholesky path did not call LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix lu CPU Float32 values and dtype",
        "(let (lu (matrix/lu (Tensor Float32 [2 2] [4 3 2 1]))) (and (< (abs (- (ref (ref lu 'lu) [1 0]) 0.5)) 0.0001) (< (abs (- (ref (ref lu 'lu) [1 1]) -0.5)) 0.0001) (= (String (dtype (ref lu 'lu))) \"Float32\")))", pass, fail);
    test_truthy(interp, "matrix lu CPU Float32 pivot metadata",
        "(let (lu (matrix/lu (Tensor Float32 [2 2] [0 2 1 3]))) (and (= (ref (ref lu 'pivots) 0) 1) (= (ref lu 'swap-count) 1)))", pass, fail);
    test_truthy(interp, "matrix solve CPU Float32 vector rhs",
        "(let (s (matrix/solve (Tensor Float32 [2 2] [2 1 1 3]) (Tensor Float32 [2] [1 2]))) (and (< (abs (- (ref s [0]) 0.2)) 0.0001) (< (abs (- (ref s [1]) 0.6)) 0.0001) (= (String (dtype s)) \"Float32\")))", pass, fail);
    test_truthy(interp, "matrix determinant CPU Float32 value and singular zero",
        "(and (< (abs (- (matrix/determinant (Tensor Float32 [2 2] [4 3 2 1])) -2.0)) 0.0001) (< (abs (matrix/determinant (Tensor Float32 [2 2] [1 2 2 4]))) 0.0001))", pass, fail);
    test_truthy(interp, "matrix inverse CPU Float32 values and dtype",
        "(let (inv (matrix/inverse (Tensor Float32 [2 2] [4 7 2 6]))) (and (< (abs (- (ref inv [0 0]) 0.6)) 0.0001) (< (abs (- (ref inv [0 1]) -0.7)) 0.0001) (= (String (dtype inv)) \"Float32\")))", pass, fail);
    test_truthy(interp, "matrix qr CPU Float32 values and dtype",
        "(let (qr (matrix/qr (Tensor Float32 [3 2] [1 1 0 1 0 0]))) (and (< (abs (- (ref (ref qr 'q) [0 0]) 1.0)) 0.0001) (< (abs (- (ref (ref qr 'q) [1 1]) 1.0)) 0.0001) (< (abs (- (ref (ref qr 'r) [0 1]) 1.0)) 0.0001) (= (String (dtype (ref qr 'q))) \"Float32\") (= (String (dtype (ref qr 'r))) \"Float32\")))", pass, fail);
    test_truthy(interp, "matrix cholesky CPU Float32 values and dtype",
        "(let (ch (matrix/cholesky (Tensor Float32 [2 2] [4 2 2 2]))) (and (< (abs (- (ref ch [0 0]) 2.0)) 0.0001) (< (abs (- (ref ch [1 0]) 1.0)) 0.0001) (< (abs (ref ch [0 1])) 0.0001) (= (String (dtype ch)) \"Float32\")))", pass, fail);
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_truthy(interp, "matrix lu determinant CPU Float32 do not call Float64 LAPACK",
            "(and (< (abs (- (matrix/determinant (Tensor Float32 [2 2] [4 3 2 1])) -2.0)) 0.0001) (< (abs (- (ref (ref (matrix/lu (Tensor Float32 [2 2] [4 3 2 1])) 'lu) [1 0]) 0.5)) 0.0001))", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgetrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU Float32 LU/determinant path did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU Float32 LU/determinant path did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve inverse CPU Float32 do not call Float64 LAPACK",
            "(and (< (abs (- (ref (matrix/solve (Tensor Float32 [2 2] [2 1 1 3]) (Tensor Float32 [2] [1 2])) [0]) 0.2)) 0.0001) (< (abs (- (ref (matrix/inverse (Tensor Float32 [2 2] [4 7 2 6])) [0 0]) 0.6)) 0.0001))", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU Float32 solve/inverse path did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU Float32 solve/inverse path did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        test_truthy(interp, "matrix qr CPU Float32 does not call Float64 LAPACK",
            "(< (abs (- (ref (ref (matrix/qr (Tensor Float32 [3 2] [1 1 0 1 0 0])) 'r) [1 1]) 1.0)) 0.0001)", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dgeqrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU Float32 QR path did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU Float32 QR path did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_truthy(interp, "matrix cholesky CPU Float32 does not call Float64 LAPACK",
            "(< (abs (- (ref (matrix/cholesky (Tensor Float32 [2 2] [4 2 2 2])) [1 1]) 1.0)) 0.0001)", pass, fail);
        bool lapack_unchanged = omni_tensor_backend_lapack_dpotrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal("[PASS] tensor CPU Float32 Cholesky path did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU Float32 Cholesky path did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix lu CPU Complex128 values and dtype",
        "(let (lu (matrix/lu (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 3 0) (Complex128 2 0) (Complex128 1 0)]))) (and (= (String (ref (ref lu 'lu) [1 0])) \"0.5+0.0i\") (= (String (ref (ref lu 'lu) [1 1])) \"-0.5+0.0i\") (= (String (dtype (ref lu 'lu))) \"Complex128\")))", pass, fail);
    test_truthy(interp, "matrix lu CPU Complex64 values and dtype",
        "(let (lu (matrix/lu (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 3 0) (Complex64 2 0) (Complex64 1 0)]))) (and (= (String (ref (ref lu 'lu) [1 0])) \"0.5+0.0i\") (= (String (ref (ref lu 'lu) [1 1])) \"-0.5+0.0i\") (= (String (dtype (ref lu 'lu))) \"Complex64\")))", pass, fail);
    test_truthy(interp, "matrix lu CPU Complex128 pivot metadata",
        "(let (lu (matrix/lu (Tensor Complex128 [2 2] [(Complex128 0 0) (Complex128 2 0) (Complex128 1 0) (Complex128 3 0)]))) (and (= (ref (ref lu 'pivots) 0) 1) (= (ref lu 'swap-count) 1)))", pass, fail);
    test_truthy(interp, "matrix determinant CPU Complex128 value and singular zero",
        "(and (= (String (matrix/determinant (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]))) \"-1.0+3.0i\") (= (String (matrix/determinant (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)]))) \"0.0+0.0i\"))", pass, fail);
    test_truthy(interp, "matrix determinant CPU Complex64 value and singular zero",
        "(let (det (matrix/determinant (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)])) zero (matrix/determinant (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)]))) (and (< (abs (- (real-part det) -1.0)) 0.001) (< (abs (- (imag-part det) 3.0)) 0.001) (< (abs (real-part zero)) 0.001) (< (abs (imag-part zero)) 0.001)))", pass, fail);
    test_truthy(interp, "matrix solve CPU Complex128 vector rhs",
        "(let (s (matrix/solve (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) (Tensor Complex128 [2] [(Complex128 1 1) (Complex128 3 -1)]))) (and (= (String (ref s [0])) \"0.5+0.5i\") (= (String (ref s [1])) \"1.5-0.5i\") (= (String (dtype s)) \"Complex128\")))", pass, fail);
    test_truthy(interp, "matrix solve CPU Complex64 vector rhs",
        "(let (s (matrix/solve (Tensor Complex64 [2 2] [(Complex64 2 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) (Tensor Complex64 [2] [(Complex64 1 1) (Complex64 3 -1)]))) (and (= (String (ref s [0])) \"0.5+0.5i\") (= (String (ref s [1])) \"1.5-0.5i\") (= (String (dtype s)) \"Complex64\")))", pass, fail);
    test_truthy(interp, "matrix solve CPU Complex128 matrix rhs shape",
        "(let (s (matrix/solve (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 4 0) (Complex128 6 0) (Complex128 8 0)]))) (and (= (rank s) 2) (= (ref (shape s) 1) 2) (= (String (ref s [1 1])) \"4.0+0.0i\")))", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex128 values and dtype",
        "(let (inv (matrix/inverse (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)])) a00 (ref inv [0 0]) a01 (ref inv [0 1]) a10 (ref inv [1 0]) a11 (ref inv [1 1])) (and (< (abs (- (real-part a00) -0.7)) 0.000000001) (< (abs (- (imag-part a00) -1.1)) 0.000000001) (< (abs (- (real-part a01) 0.2)) 0.000000001) (< (abs (- (imag-part a01) 0.6)) 0.000000001) (< (abs (- (real-part a10) 0.3)) 0.000000001) (< (abs (- (imag-part a10) 0.9)) 0.000000001) (< (abs (- (real-part a11) 0.2)) 0.000000001) (< (abs (- (imag-part a11) -0.4)) 0.000000001) (= (String (dtype inv)) \"Complex128\")))", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex128 product oracle",
        "(let (a (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) prod (contract a (matrix/inverse a) [1 0]) d0 (ref prod [0 0]) d1 (ref prod [1 1]) off (ref prod [0 1])) (and (< (abs (- (real-part d0) 1.0)) 0.000000001) (< (abs (imag-part d0)) 0.000000001) (< (abs (- (real-part d1) 1.0)) 0.000000001) (< (abs (imag-part d1)) 0.000000001) (< (abs (real-part off)) 0.000000001) (< (abs (imag-part off)) 0.000000001)))", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex64 values and dtype",
        "(let (inv (matrix/inverse (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)])) a00 (ref inv [0 0]) a01 (ref inv [0 1]) a10 (ref inv [1 0]) a11 (ref inv [1 1])) (and (< (abs (- (real-part a00) -0.7)) 0.001) (< (abs (- (imag-part a00) -1.1)) 0.001) (< (abs (- (real-part a01) 0.2)) 0.001) (< (abs (- (imag-part a01) 0.6)) 0.001) (< (abs (- (real-part a10) 0.3)) 0.001) (< (abs (- (imag-part a10) 0.9)) 0.001) (< (abs (- (real-part a11) 0.2)) 0.001) (< (abs (- (imag-part a11) -0.4)) 0.001) (= (String (dtype inv)) \"Complex64\")))", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex64 product oracle",
        "(let (a (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)]) prod (contract a (matrix/inverse a) [1 0]) d0 (ref prod [0 0]) d1 (ref prod [1 1]) off (ref prod [0 1])) (and (< (abs (- (real-part d0) 1.0)) 0.001) (< (abs (imag-part d0)) 0.001) (< (abs (- (real-part d1) 1.0)) 0.001) (< (abs (imag-part d1)) 0.001) (< (abs (real-part off)) 0.001) (< (abs (imag-part off)) 0.001)))", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex128 values dtype shapes and reconstruction",
        "(let (a (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) qr (matrix/qr a) q (ref qr 'q) r (ref qr 'r) recon (contract q r [1 0]) q00 (ref q [0 0]) r11 (ref r [1 1]) x00 (ref recon [0 0]) x11 (ref recon [1 1]) x20 (ref recon [2 0])) (and (= (ref (shape q) 0) 3) (= (ref (shape q) 1) 2) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 2) (= (String (dtype q)) \"Complex128\") (= (String (dtype r)) \"Complex128\") (< (abs (- (real-part q00) 1.0)) 0.000000001) (< (abs (imag-part q00)) 0.000000001) (< (abs (- (real-part r11) 1.0)) 0.000000001) (< (abs (imag-part r11)) 0.000000001) (< (abs (- (real-part x00) 1.0)) 0.000000001) (< (abs (- (real-part x11) 1.0)) 0.000000001) (< (abs (real-part x20)) 0.000000001) (< (abs (imag-part x20)) 0.000000001)))", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex64 values dtype shapes and reconstruction",
        "(let (a (Tensor Complex64 [3 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) qr (matrix/qr a) q (ref qr 'q) r (ref qr 'r) recon (contract q r [1 0]) q00 (ref q [0 0]) r11 (ref r [1 1]) x00 (ref recon [0 0]) x11 (ref recon [1 1]) x20 (ref recon [2 0])) (and (= (ref (shape q) 0) 3) (= (ref (shape q) 1) 2) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 2) (= (String (dtype q)) \"Complex64\") (= (String (dtype r)) \"Complex64\") (< (abs (- (real-part q00) 1.0)) 0.001) (< (abs (imag-part q00)) 0.001) (< (abs (- (real-part r11) 1.0)) 0.001) (< (abs (imag-part r11)) 0.001) (< (abs (- (real-part x00) 1.0)) 0.001) (< (abs (- (real-part x11) 1.0)) 0.001) (< (abs (real-part x20)) 0.001) (< (abs (imag-part x20)) 0.001)))", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex128 uses Hermitian projection for non-real columns",
        "(let (qr (matrix/qr (Tensor Complex128 [3 2] [(Complex128 1 1) (Complex128 1 -1) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)])) r01 (ref (ref qr 'r) [0 1]) r11 (ref (ref qr 'r) [1 1])) (and (< (abs (real-part r01)) 0.000000001) (< (abs (- (imag-part r01) -1.4142135623730951)) 0.000000001) (< (abs (- (real-part r11) 1.0)) 0.000000001) (< (abs (imag-part r11)) 0.000000001)))", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex64 uses Hermitian projection for non-real columns",
        "(let (qr (matrix/qr (Tensor Complex64 [3 2] [(Complex64 1 1) (Complex64 1 -1) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)])) r01 (ref (ref qr 'r) [0 1]) r11 (ref (ref qr 'r) [1 1])) (and (< (abs (real-part r01)) 0.001) (< (abs (- (imag-part r01) -1.4142135623730951)) 0.001) (< (abs (- (real-part r11) 1.0)) 0.001) (< (abs (imag-part r11)) 0.001)))", pass, fail);
    test_truthy(interp, "matrix cholesky CPU Complex128 Hermitian values dtype and upper zero",
        "(let (ch (matrix/cholesky (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 2) (Complex128 2 -2) (Complex128 6 0)])) a00 (ref ch [0 0]) a10 (ref ch [1 0]) a01 (ref ch [0 1]) a11 (ref ch [1 1])) (and (= (String (dtype ch)) \"Complex128\") (< (abs (- (real-part a00) 2.0)) 0.000000001) (< (abs (imag-part a00)) 0.000000001) (< (abs (- (real-part a10) 1.0)) 0.000000001) (< (abs (- (imag-part a10) -1.0)) 0.000000001) (< (abs (real-part a01)) 0.000000001) (< (abs (imag-part a01)) 0.000000001) (< (abs (- (real-part a11) 2.0)) 0.000000001) (< (abs (imag-part a11)) 0.000000001)))", pass, fail);
    test_truthy(interp, "matrix cholesky CPU Complex64 Hermitian values dtype and upper zero",
        "(let (ch (matrix/cholesky (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)])) a00 (ref ch [0 0]) a10 (ref ch [1 0]) a01 (ref ch [0 1]) a11 (ref ch [1 1])) (and (= (String (dtype ch)) \"Complex64\") (< (abs (- (real-part a00) 2.0)) 0.001) (< (abs (imag-part a00)) 0.001) (< (abs (- (real-part a10) 1.0)) 0.001) (< (abs (- (imag-part a10) -1.0)) 0.001) (< (abs (real-part a01)) 0.001) (< (abs (imag-part a01)) 0.001) (< (abs (- (real-part a11) 2.0)) 0.001) (< (abs (imag-part a11)) 0.001)))", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex128 rejects rank deficient",
        "(= (handle (matrix/qr (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix qr CPU Complex64 rejects rank deficient",
        "(= (handle (matrix/qr (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix cholesky CPU Complex128 rejects non-Hermitian",
        "(= (handle (matrix/cholesky (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 1) (Complex128 2 2) (Complex128 6 0)])) (raise msg (ref msg 'code))) 'tensor/not-positive-definite)", pass, fail);
    test_truthy(interp, "matrix cholesky CPU Complex64 rejects non-HPD",
        "(= (handle (matrix/cholesky (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 1 0)])) (raise msg (ref msg 'code))) 'tensor/not-positive-definite)", pass, fail);
    {
        long qr_lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        long cholesky_lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_truthy(interp, "matrix qr cholesky CPU fixed complex do not call Float64 LAPACK",
            "(let (qr (matrix/qr (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)])) ch (matrix/cholesky (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)]))) (and (= (String (ref (ref qr 'r) [1 1])) \"1.0+0.0i\") (< (abs (- (real-part (ref ch [1 1])) 2.0)) 0.001)))", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgeqrf_call_count() == qr_lapack_calls_before &&
            omni_tensor_backend_lapack_dpotrf_call_count() == cholesky_lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor CPU fixed complex QR/Cholesky paths did not call Float64 LAPACK");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor CPU fixed complex QR/Cholesky paths did not call Float64 LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix lu CPU Complex128 rejects singular",
        "(= (handle (matrix/lu (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix solve CPU Complex64 rejects singular",
        "(= (handle (matrix/solve (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)]) (Tensor Complex64 [2] [(Complex64 1 0) (Complex64 2 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex128 rejects singular",
        "(= (handle (matrix/inverse (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix inverse CPU Complex64 rejects singular",
        "(= (handle (matrix/inverse (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)])) (raise msg (ref msg 'code))) 'tensor/singular-matrix)", pass, fail);
    test_truthy(interp, "matrix lu vulkan Complex128 factor value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (String (ref (to-device (ref (matrix/lu (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 3 0) (Complex128 2 0) (Complex128 1 0)]) 'vulkan)) 'lu) 'cpu) [1 0])) \"0.5+0.0i\") true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Complex64 factor value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (String (ref (to-device (ref (matrix/lu (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 3 0) (Complex64 2 0) (Complex64 1 0)]) 'vulkan)) 'lu) 'cpu) [1 1])) \"-0.5+0.0i\") true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Complex128 preserves dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (lu (ref (matrix/lu (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 3 0) (Complex128 2 0) (Complex128 1 0)]) 'vulkan)) 'lu)) (and (= (device lu) 'vulkan) (= (String (dtype lu)) \"Complex128\"))) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Complex64 pivot metadata when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (lu (matrix/lu (to-device (Tensor Complex64 [2 2] [(Complex64 0 0) (Complex64 2 0) (Complex64 1 0) (Complex64 3 0)]) 'vulkan))) (and (= (ref (ref lu 'pivots) 0) 1) (= (ref lu 'swap-count) 1))) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan Complex128 value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (String (matrix/determinant (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) 'vulkan))) \"-1.0+3.0i\") true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan Complex64 value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (det (matrix/determinant (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)]) 'vulkan))) (and (< (abs (- (real-part det) -1.0)) 0.001) (< (abs (- (imag-part det) 3.0)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix determinant vulkan Complex128 lazy and singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (and (= (String (matrix/determinant (map + (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) 'vulkan) (Complex128 0 0)))) \"-1.0+3.0i\") (= (String (matrix/determinant (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)]) 'vulkan))) \"0.0+0.0i\")) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Complex128 vector rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (s (matrix/solve (to-device (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan) (to-device (Tensor Complex128 [2] [(Complex128 1 1) (Complex128 3 -1)]) 'vulkan))) (and (= (device s) 'vulkan) (= (String (dtype s)) \"Complex128\") (= (String (ref (to-device s 'cpu) [0])) \"0.5+0.5i\") (= (String (ref (to-device s 'cpu) [1])) \"1.5-0.5i\"))) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Complex64 vector rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (s (matrix/solve (to-device (Tensor Complex64 [2 2] [(Complex64 2 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan) (to-device (Tensor Complex64 [2] [(Complex64 1 1) (Complex64 3 -1)]) 'vulkan))) (and (= (device s) 'vulkan) (= (String (dtype s)) \"Complex64\") (= (String (ref (to-device s 'cpu) [0])) \"0.5+0.5i\") (= (String (ref (to-device s 'cpu) [1])) \"1.5-0.5i\"))) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Complex128 matrix rhs shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (s (matrix/solve (to-device (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan) (to-device (Tensor Complex128 [2 2] [(Complex128 2 0) (Complex128 4 0) (Complex128 6 0) (Complex128 8 0)]) 'vulkan))) (and (= (rank s) 2) (= (ref (shape s) 1) 2) (= (String (ref (to-device s 'cpu) [1 1])) \"4.0+0.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Complex64 lazy rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (String (ref (to-device (matrix/solve (to-device (Tensor Complex64 [2 2] [(Complex64 2 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan) (map + (to-device (Tensor Complex64 [2] [(Complex64 1 1) (Complex64 3 -1)]) 'vulkan) (Complex64 0 0))) 'cpu) [1])) \"1.5-0.5i\") true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex128 placement dtype and value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (inv (matrix/inverse (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) 'vulkan)) cpu (to-device inv 'cpu) a00 (ref cpu [0 0]) a11 (ref cpu [1 1])) (and (= (device inv) 'vulkan) (= (String (dtype inv)) \"Complex128\") (< (abs (- (real-part a00) -0.7)) 0.000000001) (< (abs (- (imag-part a00) -1.1)) 0.000000001) (< (abs (- (real-part a11) 0.2)) 0.000000001) (< (abs (- (imag-part a11) -0.4)) 0.000000001))) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex64 placement dtype and value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (inv (matrix/inverse (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)]) 'vulkan)) cpu (to-device inv 'cpu) a00 (ref cpu [0 0]) a11 (ref cpu [1 1])) (and (= (device inv) 'vulkan) (= (String (dtype inv)) \"Complex64\") (< (abs (- (real-part a00) -0.7)) 0.001) (< (abs (- (imag-part a00) -1.1)) 0.001) (< (abs (- (real-part a11) 0.2)) 0.001) (< (abs (- (imag-part a11) -0.4)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex128 product oracle when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (a (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) 'vulkan) prod (contract a (matrix/inverse a) [1 0]) cpu (to-device prod 'cpu) d0 (ref cpu [0 0]) d1 (ref cpu [1 1])) (and (= (device prod) 'vulkan) (< (abs (- (real-part d0) 1.0)) 0.000000001) (< (abs (imag-part d0)) 0.000000001) (< (abs (- (real-part d1) 1.0)) 0.000000001) (< (abs (imag-part d1)) 0.000000001))) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex64 product oracle when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (a (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)]) 'vulkan) prod (contract a (matrix/inverse a) [1 0]) cpu (to-device prod 'cpu) d0 (ref cpu [0 0]) d1 (ref cpu [1 1])) (and (= (device prod) 'vulkan) (< (abs (- (real-part d0) 1.0)) 0.001) (< (abs (imag-part d0)) 0.001) (< (abs (- (real-part d1) 1.0)) 0.001) (< (abs (imag-part d1)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Complex128 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (handle (matrix/lu (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Complex64 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (handle (matrix/solve (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)]) 'vulkan) (to-device (Tensor Complex64 [2] [(Complex64 1 0) (Complex64 2 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex128 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (handle (matrix/inverse (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex64 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (handle (matrix/inverse (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex128 values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (qr (matrix/qr (to-device (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) q (to-device (ref qr 'q) 'cpu) r (to-device (ref qr 'r) 'cpu) q00 (ref q [0 0]) r11 (ref r [1 1])) (and (< (abs (- (real-part q00) 1.0)) 0.000000001) (< (abs (imag-part q00)) 0.000000001) (< (abs (- (real-part r11) 1.0)) 0.000000001) (< (abs (imag-part r11)) 0.000000001))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex64 values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (qr (matrix/qr (to-device (Tensor Complex64 [3 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) q (to-device (ref qr 'q) 'cpu) r (to-device (ref qr 'r) 'cpu) q00 (ref q [0 0]) r11 (ref r [1 1])) (and (< (abs (- (real-part q00) 1.0)) 0.001) (< (abs (imag-part q00)) 0.001) (< (abs (- (real-part r11) 1.0)) 0.001) (< (abs (imag-part r11)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex128 uses Hermitian projection when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (qr (matrix/qr (to-device (Tensor Complex128 [3 2] [(Complex128 1 1) (Complex128 1 -1) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) r (to-device (ref qr 'r) 'cpu) r01 (ref r [0 1]) r11 (ref r [1 1])) (and (< (abs (real-part r01)) 0.000000001) (< (abs (- (imag-part r01) -1.4142135623730951)) 0.000000001) (< (abs (- (real-part r11) 1.0)) 0.000000001) (< (abs (imag-part r11)) 0.000000001))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex64 uses Hermitian projection when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (qr (matrix/qr (to-device (Tensor Complex64 [3 2] [(Complex64 1 1) (Complex64 1 -1) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) r (to-device (ref qr 'r) 'cpu) r01 (ref r [0 1]) r11 (ref r [1 1])) (and (< (abs (real-part r01)) 0.001) (< (abs (- (imag-part r01) -1.4142135623730951)) 0.001) (< (abs (- (real-part r11) 1.0)) 0.001) (< (abs (imag-part r11)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex128 dtype device shapes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (qr (matrix/qr (to-device (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) q (ref qr 'q) r (ref qr 'r)) (and (= (device q) 'vulkan) (= (device r) 'vulkan) (= (String (dtype q)) \"Complex128\") (= (String (dtype r)) \"Complex128\") (= (ref (shape q) 0) 3) (= (ref (shape q) 1) 2) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 2))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex64 dtype device shapes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (qr (matrix/qr (to-device (Tensor Complex64 [3 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) q (ref qr 'q) r (ref qr 'r)) (and (= (device q) 'vulkan) (= (device r) 'vulkan) (= (String (dtype q)) \"Complex64\") (= (String (dtype r)) \"Complex64\") (= (ref (shape q) 0) 3) (= (ref (shape q) 1) 2) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 2))) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex128 lazy and rank deficient when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (and (= (String (ref (to-device (ref (matrix/qr (map + (to-device (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan) (Complex128 0 0))) 'r) 'cpu) [1 1])) \"1.0+0.0i\") (= (handle (matrix/qr (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 4 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix)) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex64 lazy and rank deficient when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (and (= (String (ref (to-device (ref (matrix/qr (map + (to-device (Tensor Complex64 [3 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan) (Complex64 0 0))) 'r) 'cpu) [1 1])) \"1.0+0.0i\") (= (handle (matrix/qr (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 4 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix)) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex128 values dtype device and upper zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (ch (matrix/cholesky (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 2) (Complex128 2 -2) (Complex128 6 0)]) 'vulkan)) cpu (to-device ch 'cpu) a00 (ref cpu [0 0]) a10 (ref cpu [1 0]) a01 (ref cpu [0 1]) a11 (ref cpu [1 1])) (and (= (device ch) 'vulkan) (= (String (dtype ch)) \"Complex128\") (< (abs (- (real-part a00) 2.0)) 0.000000001) (< (abs (- (real-part a10) 1.0)) 0.000000001) (< (abs (- (imag-part a10) -1.0)) 0.000000001) (< (abs (real-part a01)) 0.000000001) (< (abs (imag-part a01)) 0.000000001) (< (abs (- (real-part a11) 2.0)) 0.000000001))) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex64 values dtype device and upper zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (ch (matrix/cholesky (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)]) 'vulkan)) cpu (to-device ch 'cpu) a00 (ref cpu [0 0]) a10 (ref cpu [1 0]) a01 (ref cpu [0 1]) a11 (ref cpu [1 1])) (and (= (device ch) 'vulkan) (= (String (dtype ch)) \"Complex64\") (< (abs (- (real-part a00) 2.0)) 0.001) (< (abs (- (real-part a10) 1.0)) 0.001) (< (abs (- (imag-part a10) -1.0)) 0.001) (< (abs (real-part a01)) 0.001) (< (abs (imag-part a01)) 0.001) (< (abs (- (real-part a11) 2.0)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex128 lazy non-Hermitian and non-HPD when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (and (= (String (ref (to-device (matrix/cholesky (map + (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 2) (Complex128 2 -2) (Complex128 6 0)]) 'vulkan) (Complex128 0 0))) 'cpu) [1 1])) \"2.0+0.0i\") (= (handle (matrix/cholesky (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 1) (Complex128 2 2) (Complex128 6 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite) (= (handle (matrix/cholesky (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 2 0) (Complex128 2 0) (Complex128 1 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite)) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex64 lazy non-Hermitian and non-HPD when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (and (= (String (ref (to-device (matrix/cholesky (map + (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)]) 'vulkan) (Complex64 0 0))) 'cpu) [1 1])) \"2.0+0.0i\") (= (handle (matrix/cholesky (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 1) (Complex64 2 2) (Complex64 6 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite) (= (handle (matrix/cholesky (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 2 0) (Complex64 2 0) (Complex64 1 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite)) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0;
        long qr_lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        long cholesky_lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_truthy(interp, "matrix qr cholesky vulkan fixed complex do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-numerical-complex128) (= (String (ref (to-device (ref (matrix/qr (to-device (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) 'r) 'cpu) [1 1])) \"1.0+0.0i\") true) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (String (ref (to-device (matrix/cholesky (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)]) 'vulkan)) 'cpu) [1 1])) \"2.0+0.0i\") true)))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available ||
            (omni_tensor_backend_lapack_dgeqrf_call_count() == qr_lapack_calls_before &&
             omni_tensor_backend_lapack_dpotrf_call_count() == cholesky_lapack_calls_before);
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan fixed complex QR/Cholesky paths did not call LAPACK"
                : "[PASS] tensor Vulkan fixed complex QR/Cholesky unavailable; no-LAPACK path not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex QR/Cholesky paths did not call LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix qr vulkan Complex128 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex128) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (= (handle (matrix/qr (to-device (Tensor Complex128 [3 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0) (Complex128 0 0) (Complex128 0 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Complex64 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'matrix-numerical-complex64) false)) (= (handle (matrix/qr (to-device (Tensor Complex64 [3 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0) (Complex64 0 0) (Complex64 0 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex128 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex128) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (= (handle (matrix/cholesky (to-device (Tensor Complex128 [2 2] [(Complex128 4 0) (Complex128 2 2) (Complex128 2 -2) (Complex128 6 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Complex64 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'matrix-numerical-complex64) false)) (= (handle (matrix/cholesky (to-device (Tensor Complex64 [2 2] [(Complex64 4 0) (Complex64 2 2) (Complex64 2 -2) (Complex64 6 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex128 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex128) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (= (handle (matrix/inverse (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 0) (Complex128 4 -1)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Complex64 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'matrix-numerical-complex64) false)) (= (handle (matrix/inverse (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 0) (Complex64 4 -1)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Float32 factor value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/lu (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan)) 'lu) 'cpu) [1 0]) 0.5)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Float32 preserves dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (lu (ref (matrix/lu (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan)) 'lu)) (and (= (device lu) 'vulkan) (= (String (dtype lu)) \"Float32\"))) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Float32 pivot metadata when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (lu (matrix/lu (to-device (Tensor Float32 [2 2] [0 2 1 3]) 'vulkan))) (and (= (ref (ref lu 'pivots) 0) 1) (= (ref lu 'swap-count) 1))) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Float32 lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/lu (map + (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan) 0.0)) 'lu) 'cpu) [1 1]) -0.5)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix lu vulkan Float32 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (matrix/lu (to-device (Tensor Float32 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Float32 vector rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float32 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float32 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Float32 preserves dtype device and rank when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (matrix/solve (to-device (Tensor Float32 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float32 [2] [1 2]) 'vulkan))) (and (= (device s) 'vulkan) (= (String (dtype s)) \"Float32\") (= (rank s) 1))) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Float32 matrix rhs shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (shape (matrix/solve (to-device (Tensor Float32 [3 3] [3 1 0 1 4 1 0 2 5]) 'vulkan) (to-device (Tensor Float32 [3 2] [4 1 6 2 7 3]) 'vulkan))) 1) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Float32 lazy rhs when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float32 [2 2] [2 1 1 3]) 'vulkan) (map + (to-device (Tensor Float32 [2] [1 2]) 'vulkan) 0.0)) 'cpu) [1]) 0.6)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix solve vulkan Float32 rejects singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (matrix/solve (to-device (Tensor Float32 [2 2] [1 2 2 4]) 'vulkan) (to-device (Tensor Float32 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long f32_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f32_call_count();
        long f32_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f32_call_count();
        long f32_multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count();
        long f64_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f64_call_count();
        long f64_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f64_call_count();
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan Float32 2x2 stays serial when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/solve (to-device (Tensor Float32 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float32 [2] [1 2]) 'vulkan)) 'cpu) [0]) 0.2)) 0.0001) true) true))", pass, fail);
        bool f32_serial_threshold_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_f32_call_count() > f32_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f32_call_count() == f32_parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count() == f32_multi_dispatch_calls_before &&
             omni_tensor_backend_vulkan_solve_serial_f64_call_count() == f64_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f64_call_count() == f64_parallel_calls_before &&
             omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before);
        if (f32_serial_threshold_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix solve 2x2 stayed on serial helper"
                : "[PASS] tensor Vulkan Float32 matrix solve 2x2 serial threshold unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix solve 2x2 stayed on serial helper");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long f32_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f32_call_count();
        long f32_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f32_call_count();
        long f32_multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count();
        long f64_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f64_call_count();
        long f64_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f64_call_count();
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan Float32 9x9 stays serial when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/solve (to-device (matrix/identity 9 Float32) 'vulkan) (to-device (Tensor Float32 [9] [1 2 3 4 5 6 7 8 9]) 'vulkan)) 'cpu) [8]) 9.0)) 0.0001) true) true))", pass, fail);
        bool f32_serial_threshold_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_f32_call_count() > f32_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f32_call_count() == f32_parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count() == f32_multi_dispatch_calls_before &&
             omni_tensor_backend_vulkan_solve_serial_f64_call_count() == f64_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f64_call_count() == f64_parallel_calls_before &&
             omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before);
        if (f32_serial_threshold_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix solve 9x9 stayed below staged threshold"
                : "[PASS] tensor Vulkan Float32 matrix solve 9x9 serial threshold unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix solve 9x9 stayed below staged threshold");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long f32_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f32_call_count();
        long f32_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f32_call_count();
        long f32_multi_dispatch_calls_before = omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count();
        long f32_pivot_reduce_calls_before = omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count();
        long f32_factor_stage_calls_before = omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count();
        long f64_serial_calls_before = omni_tensor_backend_vulkan_solve_serial_f64_call_count();
        long f64_parallel_calls_before = omni_tensor_backend_vulkan_solve_parallel_f64_call_count();
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve vulkan Float32 staged identity 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (result (matrix/solve (to-device (matrix/identity 65 Float32) 'vulkan) (to-device (Tensor Float32 [65] 1.0) 'vulkan))) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Float32\") (< (abs (- (ref (to-device result 'cpu) [64]) 1.0)) 0.0001))) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan Float32 staged dense 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/solve (to-device (map + (matrix/identity 65 Float32) (Tensor Float32 [65 65] 1.0)) 'vulkan) (to-device (Tensor Float32 [65] 66.0) 'vulkan)) 'cpu) [64]) 1.0)) 0.0001) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan Float32 staged matrix rhs shape when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (result (matrix/solve (to-device (matrix/identity 65 Float32) 'vulkan) (to-device (Tensor Float32 [65 2] 1.0) 'vulkan))) (and (= (rank result) 2) (= (ref (shape result) 0) 65) (= (ref (shape result) 1) 2))) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan Float32 staged rejects singular 65 when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (matrix/solve (to-device (Tensor Float32 [65 65] 1.0) 'vulkan) (to-device (Tensor Float32 [65] 1.0) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
        test_truthy(interp, "matrix solve vulkan Float32 staged rejects mixed dtype when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (and (ref vulkan-backend 'float32) (ref vulkan-backend 'float64)) (= (handle (matrix/solve (to-device (matrix/identity 65 Float32) 'vulkan) (to-device (Tensor Float64 [65] 1.0) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
        bool f32_staged_ok = !vulkan_available ||
            (omni_tensor_backend_vulkan_solve_serial_f32_call_count() == f32_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f32_call_count() > f32_parallel_calls_before &&
             omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count() > f32_multi_dispatch_calls_before &&
             omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count() > f32_pivot_reduce_calls_before &&
             omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count() > f32_factor_stage_calls_before &&
             omni_tensor_backend_vulkan_solve_serial_f64_call_count() == f64_serial_calls_before &&
             omni_tensor_backend_vulkan_solve_parallel_f64_call_count() == f64_parallel_calls_before &&
             omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before);
        if (f32_staged_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix solve staged parallel path without Float64 or LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix solve staged parallel path unavailable; not exercised");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix solve staged parallel path without Float64 or LAPACK");
            (*fail)++;
        }
    }
    test_truthy(interp, "matrix determinant vulkan Float32 value when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (matrix/determinant (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan)) -2.0)) 0.0001) true) true))", pass, fail);
}
