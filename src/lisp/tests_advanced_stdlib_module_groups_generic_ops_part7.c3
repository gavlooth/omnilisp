module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part7(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "matrix determinant vulkan Float32 lazy and singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (matrix/determinant (map + (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan) 0.0)) -2.0)) 0.0001) (< (abs (matrix/determinant (to-device (Tensor Float32 [2 2] [1 2 2 4]) 'vulkan))) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Float32 values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (matrix/inverse (to-device (Tensor Float32 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [0 0]) 0.6)) 0.0001) (< (abs (- (ref (to-device (matrix/inverse (to-device (Tensor Float32 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [0 1]) -0.7)) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Float32 preserves dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (inv (matrix/inverse (to-device (Tensor Float32 [2 2] [4 7 2 6]) 'vulkan))) (and (= (device inv) 'vulkan) (= (String (dtype inv)) \"Float32\"))) true) true))", pass, fail);
    test_truthy(interp, "matrix inverse vulkan Float32 lazy and singular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (matrix/inverse (map + (to-device (Tensor Float32 [2 2] [4 7 2 6]) 'vulkan) 0.0)) 'cpu) [1 1]) 0.4)) 0.0001) (= (handle (matrix/inverse (to-device (Tensor Float32 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix)) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Float32 values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float32 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'q) 'cpu) [0 0]) 1.0)) 0.0001) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float32 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'r) 'cpu) [0 1]) 1.0)) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Float32 preserves dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (qr (matrix/qr (to-device (Tensor Float32 [3 2] [1 1 0 1 0 0]) 'vulkan))) (and (= (device (ref qr 'q)) 'vulkan) (= (device (ref qr 'r)) 'vulkan) (= (String (dtype (ref qr 'q))) \"Float32\") (= (String (dtype (ref qr 'r))) \"Float32\"))) true) true))", pass, fail);
    test_truthy(interp, "matrix qr vulkan Float32 lazy and rank deficient when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (ref (matrix/qr (map + (to-device (Tensor Float32 [3 2] [1 1 0 1 0 0]) 'vulkan) 0.0)) 'r) 'cpu) [1 1]) 1.0)) 0.0001) (= (handle (matrix/qr (to-device (Tensor Float32 [2 2] [1 2 2 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix)) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Float32 values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (matrix/cholesky (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [0 0]) 2.0)) 0.0001) (< (abs (- (ref (to-device (matrix/cholesky (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [1 0]) 1.0)) 0.0001) (< (abs (ref (to-device (matrix/cholesky (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [0 1])) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Float32 preserves dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (ch (matrix/cholesky (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan))) (and (= (device ch) 'vulkan) (= (String (dtype ch)) \"Float32\"))) true) true))", pass, fail);
    test_truthy(interp, "matrix cholesky vulkan Float32 lazy and rejects nonsymmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (- (ref (to-device (matrix/cholesky (map + (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan) 0.0)) 'cpu) [1 1]) 1.0)) 0.0001) (= (handle (matrix/cholesky (to-device (Tensor Float32 [2 2] [1 2 3 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-positive-definite)) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgetrf_call_count();
        test_truthy(interp, "matrix lu determinant vulkan Float32 do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (List (matrix/determinant (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan)) (ref (to-device (ref (matrix/lu (to-device (Tensor Float32 [2 2] [4 3 2 1]) 'vulkan)) 'lu) 'cpu) [1 0])) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dgetrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 LU/determinant path did not call LAPACK"
                : "[PASS] tensor Vulkan Float32 LU/determinant path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 LU/determinant path did not call LAPACK");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesv_call_count();
        test_truthy(interp, "matrix solve inverse vulkan Float32 do not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (List (ref (to-device (matrix/solve (to-device (Tensor Float32 [2 2] [2 1 1 3]) 'vulkan) (to-device (Tensor Float32 [2] [1 2]) 'vulkan)) 'cpu) [0]) (ref (to-device (matrix/inverse (to-device (Tensor Float32 [2 2] [4 7 2 6]) 'vulkan)) 'cpu) [0 0])) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dgesv_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 solve/inverse path did not call LAPACK"
                : "[PASS] tensor Vulkan Float32 solve/inverse path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 solve/inverse path did not call LAPACK");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgeqrf_call_count();
        test_truthy(interp, "matrix qr vulkan Float32 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/qr (to-device (Tensor Float32 [3 2] [1 1 0 1 0 0]) 'vulkan)) 'r) 'cpu) [1 1]) 1.0)) 0.0001) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dgeqrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 QR path did not call LAPACK"
                : "[PASS] tensor Vulkan Float32 QR path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 QR path did not call LAPACK");
            (*fail)++;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dpotrf_call_count();
        test_truthy(interp, "matrix cholesky vulkan Float32 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (matrix/cholesky (to-device (Tensor Float32 [2 2] [4 2 2 2]) 'vulkan)) 'cpu) [1 1]) 1.0)) 0.0001) true) true))", pass, fail);
        bool lapack_unchanged = !vulkan_available || omni_tensor_backend_lapack_dpotrf_call_count() == lapack_calls_before;
        if (lapack_unchanged) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 Cholesky path did not call LAPACK"
                : "[PASS] tensor Vulkan Float32 Cholesky path unavailable; no-LAPACK check skipped");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 Cholesky path did not call LAPACK");
            (*fail)++;
        }
    }
    test_double(interp, "matrix singular-values first",
        "(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [0])",
        3.0, pass, fail);
    test_double(interp, "matrix singular-values second",
        "(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [1])",
        2.0, pass, fail);
    test_eq(interp, "matrix singular-values wide length",
        "(ref (shape (matrix/singular-values (Tensor Float64 [2 3] [3 0 0 0 2 0]))) 0)",
        2, pass, fail);
    test_eq(interp, "matrix singular-values empty length",
        "(ref (shape (matrix/singular-values (Tensor Float64 [0 3] 0.0))) 0)",
        0, pass, fail);
    test_double(interp, "matrix singular-values realizes lazy input",
        "(ref (matrix/singular-values (map + (Tensor Float64 [2 2] [3 0 0 2]) 0.0)) [1])",
        2.0, pass, fail);
    test_double(interp, "matrix singular-values rank deficient zero",
        "(ref (matrix/singular-values (Tensor Float64 [2 2] [0 0 0 0])) [0])",
        0.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesvd_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_double(interp, "matrix singular-values LAPACK candidate",
            "(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [0])",
            3.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesvd_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesvd matrix singular-values path"
                : "[PASS] tensor LAPACK backend dgesvd unavailable; matrix singular-values fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd matrix singular-values path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix singular-values forced pure fallback first",
            "(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [0])",
            3.0, pass, fail);
        test_double(interp, "matrix singular-values forced pure fallback second",
            "(ref (matrix/singular-values (Tensor Float64 [2 2] [3 0 0 2])) [1])",
            2.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesvd_available() == 0;
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesvd disabled; matrix singular-values pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd disabled; matrix singular-values pure fallback path");
            (*fail)++;
        }
    }
    test_error_contains(interp, "matrix singular-values rejects vector input",
        "(matrix/singular-values (Tensor Float64 [2] [1 2]))",
        "matrix/singular-values: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix singular-values rejects BigComplex dtype",
        "(matrix/singular-values (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/singular-values: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_truthy(interp, "matrix singular-values CPU Float32 values and dtype",
        "(let (s (matrix/singular-values (Tensor Float32 [2 2] [3 0 0 2]))) (and (= (String (dtype s)) \"Float32\") (= (ref s [0]) 3.0) (= (ref s [1]) 2.0)))", pass, fail);
    test_truthy(interp, "matrix singular-values CPU Float32 empty length",
        "(= (length (matrix/singular-values (Tensor Float32 [0 3] 0.0))) 0)", pass, fail);
    test_truthy(interp, "matrix singular-values CPU Complex128 values and dtype",
        "(let (s (matrix/singular-values (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]))) (and (= (String (dtype s)) \"Float64\") (= (ref s [0]) 5.0) (= (ref s [1]) 2.0)))", pass, fail);
    test_truthy(interp, "matrix singular-values CPU Complex64 values and dtype",
        "(let (s (matrix/singular-values (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]))) (and (= (String (dtype s)) \"Float32\") (= (ref s [0]) 5.0) (= (ref s [1]) 2.0)))", pass, fail);
    test_truthy(interp, "matrix singular-values CPU Complex128 empty length",
        "(= (length (matrix/singular-values (Tensor Complex128 [0 3] (Complex128 0 0)))) 0)", pass, fail);
    test_double(interp, "matrix singular-values Complex128 realizes lazy input",
        "(ref (matrix/singular-values (map + (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) (Complex128 0 0))) [1])",
        2.0, pass, fail);
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values CPU Float32 does not call Float64 LAPACK",
            "(= (ref (matrix/singular-values (Tensor Float32 [2 2] [3 0 0 2])) [0]) 3.0)", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor CPU Float32 matrix singular-values does not call Float64 LAPACK");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor CPU Float32 matrix singular-values does not call Float64 LAPACK");
            *fail += 1;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values CPU fixed complex does not call Float64 LAPACK",
            "(and (= (ref (matrix/singular-values (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)])) [0]) 5.0) (= (ref (matrix/singular-values (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)])) [1]) 2.0))", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor CPU fixed complex matrix singular-values does not call Float64 LAPACK");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor CPU fixed complex matrix singular-values does not call Float64 LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix singular-values vulkan Float32 first when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 second when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [1]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (matrix/singular-values (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/singular-values (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/singular-values (map + (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 'cpu) [1]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 empty length when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (length (to-device (matrix/singular-values (to-device (Tensor Float32 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 over previous max k succeeds when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (Tensor Float32 [65 65] 0.0) 'vulkan)) 'cpu)) (if (= (length s) 65) (if (= (ref s [0]) 0.0) (= (ref s [64]) 0.0) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 identity 65 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (matrix/identity 65 Float32) 'vulkan)) 'cpu)) (if (= (length s) 65) (if (< (abs (- (ref s [0]) 1.0)) 0.0001) (< (abs (- (ref s [64]) 1.0)) 0.0001) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Float32 all-ones 65 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (Tensor Float32 [65 65] 1.0) 'vulkan)) 'cpu)) (if (= (length s) 65) (if (< (abs (- (ref s [0]) 65.0)) 0.001) (< (abs (ref s [64])) 0.001) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Complex128 values dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (s (matrix/singular-values (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan))) (let (cpu (to-device s 'cpu)) (and (= (device s) 'vulkan) (= (String (dtype s)) \"Float64\") (= (ref cpu [0]) 5.0) (= (ref cpu [1]) 2.0)))) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Complex64 values dtype and device when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (s (matrix/singular-values (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan))) (let (cpu (to-device s 'cpu)) (and (= (device s) 'vulkan) (= (String (dtype s)) \"Float32\") (= (ref cpu [0]) 5.0) (= (ref cpu [1]) 2.0)))) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Complex128 non diagonal lazy when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (let (s (to-device (matrix/singular-values (map + (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 1) (Complex128 0 1) (Complex128 1 0)]) 'vulkan) (Complex128 0 0))) 'cpu)) (and (< (abs (- (ref s [0]) 1.4142135623730951)) 0.000001) (< (abs (- (ref s [1]) 1.4142135623730951)) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Complex64 wide values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (let (s (to-device (matrix/singular-values (to-device (Tensor Complex64 [2 3] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0) (Complex64 0 0)]) 'vulkan)) 'cpu)) (and (= (ref s [0]) 5.0) (= (ref s [1]) 2.0))) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan Complex128 empty length when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (length (to-device (matrix/singular-values (to-device (Tensor Complex128 [0 3] (Complex128 0 0)) 'vulkan)) 'cpu)) 0) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 &&
            (omni_tensor_backend_vulkan_float64_available() != 0 || omni_tensor_backend_vulkan_float32_available() != 0);
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values vulkan fixed complex does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-numerical-complex128) (= (ref (to-device (matrix/singular-values (to-device (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 2 0)]) 'vulkan)) 'cpu) [0]) 5.0) true) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (ref (to-device (matrix/singular-values (to-device (Tensor Complex64 [2 2] [(Complex64 3 4) (Complex64 0 0) (Complex64 0 0) (Complex64 2 0)]) 'vulkan)) 'cpu) [1]) 2.0) true)))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan fixed complex matrix singular-values does not call LAPACK"
                : "[PASS] tensor Vulkan fixed complex matrix singular-values unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex matrix singular-values does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values vulkan Float32 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix singular-values does not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix singular-values unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix singular-values does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values vulkan Float32 zero 65 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (Tensor Float32 [65 65] 0.0) 'vulkan)) 'cpu)) (if (= (length s) 65) (< (abs (ref s [64])) 0.0001) nil)) true) true))", pass, fail);
        test_truthy(interp, "matrix singular-values vulkan Float32 identity 65 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (matrix/identity 65 Float32) 'vulkan)) 'cpu)) (if (= (length s) 65) (< (abs (- (ref s [64]) 1.0)) 0.0001) nil)) true) true))", pass, fail);
        test_truthy(interp, "matrix singular-values vulkan Float32 all-ones 65 does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (s (to-device (matrix/singular-values (to-device (Tensor Float32 [65 65] 1.0) 'vulkan)) 'cpu)) (if (= (length s) 65) (< (abs (- (ref s [0]) 65.0)) 0.001) nil)) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix singular-values 65x65 cases do not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix singular-values unavailable; 65x65 no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix singular-values 65x65 cases do not call LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix singular-values vulkan first when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan second when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [1]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/singular-values (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan realize into cpu destination fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (let (dst (Tensor Float64 [2] 0.0)) (realize (matrix/singular-values (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "realize scalar fill into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (dst (to-device (Tensor Float64 [2] 0.0) 'vulkan)) (block (realize 1.0 dst) (= (ref (to-device dst 'cpu) [1]) 1.0))) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/singular-values (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 'cpu) [1]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan empty length when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (matrix/singular-values (to-device (Tensor Float64 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan wide values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (to-device (matrix/singular-values (to-device (Tensor Float64 [2 3] [3 0 0 0 2 0]) 'vulkan)) 'cpu)) (if (= (ref (shape s) 0) 2) (if (= (ref s [0]) 3.0) (= (ref s [1]) 2.0) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan rank deficient zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float64 [2 2] 0.0) 'vulkan)) 'cpu) [0]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan max k boundary when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (to-device (matrix/singular-values (to-device (Tensor Float64 [64 64] 0.0) 'vulkan)) 'cpu)) (if (= (length s) 64) (= (ref s [0]) 0.0) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan rectangular over max axis supported when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (to-device (matrix/singular-values (to-device (Tensor Float64 [2 65] 0.0) 'vulkan)) 'cpu)) (if (= (length s) 2) (= (ref s [1]) 0.0) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan empty wide length when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (matrix/singular-values (to-device (Tensor Float64 [0 65] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix singular-values vulkan over previous max k succeeds when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (to-device (matrix/singular-values (to-device (Tensor Float64 [65 65] 1.0) 'vulkan)) 'cpu)) (if (= (length s) 65) (if (< (abs (- (ref s [0]) 65.0)) 0.000001) (< (abs (- (ref s [64]) 0.0)) 0.000001) nil) nil)) true) true))", pass, fail);
    {
        bool status_probe_ok = omni_tensor_backend_vulkan_singular_values_status_payload_probe_for_tests() == OMNI_TENSOR_VULKAN_SUCCESS &&
            tensor_vulkan_status_code_name(OMNI_TENSOR_VULKAN_NO_CONVERGENCE) == "tensor/no-convergence" &&
            tensor_vulkan_status_code_name(OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) == "tensor/not-symmetric";
        if (status_probe_ok) {
            emit_pass_literal("[PASS] tensor Vulkan matrix singular-values status payload mapping");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix singular-values status payload mapping");
            *fail += 1;
        }
    }
    {
        bool index_guard_ok = omni_tensor_backend_vulkan_singular_values_oversized_index_guard_for_tests() == OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (index_guard_ok) {
            emit_pass_literal("[PASS] tensor Vulkan matrix singular-values oversized index guard");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix singular-values oversized index guard");
            *fail += 1;
        }
    }
    {
        bool complex_iteration_guard_ok = omni_tensor_backend_vulkan_singular_values_complex_iteration_guard_for_tests() == OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (complex_iteration_guard_ok) {
            emit_pass_literal("[PASS] tensor Vulkan fixed complex matrix singular-values iteration guard");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex matrix singular-values iteration guard");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values vulkan does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/singular-values (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix singular-values does not call LAPACK"
                : "[PASS] tensor Vulkan matrix singular-values unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix singular-values does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix singular-values vulkan over previous max k does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (s (to-device (matrix/singular-values (to-device (Tensor Float64 [65 65] 1.0) 'vulkan)) 'cpu)) (< (abs (- (ref s [0]) 65.0)) 0.000001)) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix singular-values over previous max k does not call LAPACK"
                : "[PASS] tensor Vulkan matrix singular-values unavailable; over previous max-k no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix singular-values over previous max k does not call LAPACK");
            *fail += 1;
        }
    }
    test_double(interp, "matrix svd singular value 0",
        "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])",
        3.0, pass, fail);
    test_double(interp, "matrix svd singular value 1",
        "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [1])",
        2.0, pass, fail);
    test_eq_double(interp, "matrix svd u column",
        "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'u) [0 0])",
        1.0, pass, fail);
    test_eq_double(interp, "matrix svd v column",
        "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 'v) [1 1])",
        1.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesvd_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_double(interp, "matrix svd LAPACK candidate",
            "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])",
            3.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesvd_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesvd matrix svd path"
                : "[PASS] tensor LAPACK backend dgesvd unavailable; matrix svd fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd matrix svd path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix svd forced pure fallback singular value 0",
            "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [0])",
            3.0, pass, fail);
        test_double(interp, "matrix svd forced pure fallback singular value 1",
            "(ref (ref (matrix/svd (Tensor Float64 [2 2] [3 0 0 2])) 's) [1])",
            2.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesvd_available() == 0;
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesvd disabled; matrix svd pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd disabled; matrix svd pure fallback path");
            (*fail)++;
        }
    }
    test_eq(interp, "matrix svd tall u rows",
        "(ref (shape (ref (matrix/svd (Tensor Float64 [3 2] [3 0 0 2 0 0])) 'u)) 0)",
        3, pass, fail);
    test_eq(interp, "matrix svd wide v rows",
        "(ref (shape (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v)) 0)",
        3, pass, fail);
    test_eq_double(interp, "matrix svd wide v column",
        "(ref (ref (matrix/svd (Tensor Float64 [2 3] [3 0 0 0 2 0])) 'v) [1 1])",
        1.0, pass, fail);
    test_double(interp, "matrix svd realizes lazy input",
        "(ref (ref (matrix/svd (map + (Tensor Float64 [2 2] [3 0 0 2]) 0.0)) 's) [1])",
        2.0, pass, fail);
    test_double(interp, "matrix svd rank deficient singular value",
        "(ref (ref (matrix/svd (Tensor Float64 [2 2] [0 0 0 0])) 's) [0])",
        0.0, pass, fail);
    test_error_contains(interp, "matrix svd rejects BigComplex dtype",
        "(matrix/svd (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/svd: expected a rank-2 Float64 Tensor or Vulkan Float32 Tensor", pass, fail);
    test_error_contains(interp, "matrix svd rejects Complex128 pending factor contract",
        "(matrix/svd (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0)]))",
        "matrix/svd: expected a rank-2 Float64 Tensor or Vulkan Float32 Tensor", pass, fail);
    test_error_contains(interp, "matrix svd rejects Complex64 pending factor contract",
        "(matrix/svd (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0)]))",
        "matrix/svd: expected a rank-2 Float64 Tensor or Vulkan Float32 Tensor", pass, fail);
    test_truthy(interp, "matrix svd CPU Float32 singular values and dtype",
        "(let (r (matrix/svd (Tensor Float32 [2 2] [3 0 0 2]))) (and (= (String (dtype (ref r 'u))) \"Float32\") (= (String (dtype (ref r 's))) \"Float32\") (= (String (dtype (ref r 'v))) \"Float32\") (= (ref (ref r 's) [0]) 3.0) (= (ref (ref r 's) [1]) 2.0)))", pass, fail);
    test_truthy(interp, "matrix svd CPU Float32 tall and wide shapes",
        "(and (= (ref (shape (ref (matrix/svd (Tensor Float32 [3 2] [3 0 0 2 0 0])) 'u)) 0) 3) (= (ref (shape (ref (matrix/svd (Tensor Float32 [2 3] [3 0 0 0 2 0])) 'v)) 0) 3))", pass, fail);
    test_truthy(interp, "matrix svd CPU Float32 zero singular value",
        "(= (ref (ref (matrix/svd (Tensor Float32 [2 2] [0 0 0 0])) 's) [0]) 0.0)", pass, fail);
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd CPU Float32 does not call Float64 LAPACK",
            "(= (ref (ref (matrix/svd (Tensor Float32 [2 2] [3 0 0 2])) 's) [0]) 3.0)", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor CPU Float32 matrix svd does not call Float64 LAPACK");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor CPU Float32 matrix svd does not call Float64 LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix svd vulkan Float32 singular value 0 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [0]) 3.0)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan Float32 singular value 1 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [1]) 2.0)) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan Float32 outputs remain vulkan when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan))) (if (= (device (ref r 'u)) 'vulkan) (if (= (device (ref r 's)) 'vulkan) (= (device (ref r 'v)) 'vulkan) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan Float32 outputs preserve dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan))) (if (= (String (dtype (ref r 'u))) \"Float32\") (if (= (String (dtype (ref r 's))) \"Float32\") (= (String (dtype (ref r 'v))) \"Float32\") nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan Float32 lazy succeeds when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/svd (map + (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 's) 'cpu) [1]) 2.0)) 0.0001) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd vulkan Float32 path does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [0]) 3.0)) 0.0001) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix svd does not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix svd unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix svd does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd vulkan Float32 over previous max k succeeds without LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (Tensor Float32 [65 65] 0.0) 'vulkan))) (let (s (to-device (ref r 's) 'cpu)) (if (= (length s) 65) (< (abs (- (ref s [0]) 0.0)) 0.0001) nil))) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix svd over previous max-k path does not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix svd unavailable; over previous max-k no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix svd over previous max-k path does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd vulkan Float32 identity 65 succeeds without LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (matrix/identity 65 Float32) 'vulkan))) (let (s (to-device (ref r 's) 'cpu)) (if (= (length s) 65) (if (< (abs (- (ref s [0]) 1.0)) 0.0001) (< (abs (- (ref s [64]) 1.0)) 0.0001) nil) nil))) true) true))", pass, fail);
        test_truthy(interp, "matrix svd vulkan Float32 all-ones 65 succeeds without LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (Tensor Float32 [65 65] 1.0) 'vulkan))) (let (s (to-device (ref r 's) 'cpu)) (if (= (device (ref r 'u)) 'vulkan) (if (= (device (ref r 's)) 'vulkan) (if (= (device (ref r 'v)) 'vulkan) (if (= (String (dtype (ref r 's))) \"Float32\") (if (= (ref (shape (ref r 'u)) 0) 65) (if (= (ref (shape (ref r 'v)) 0) 65) (if (< (abs (- (ref s [0]) 65.0)) 0.001) (< (abs (ref s [64])) 0.001) nil) nil) nil) nil) nil) nil) nil))) true) true))", pass, fail);
        test_truthy(interp, "matrix svd vulkan Float32 all-ones 65 reconstructs when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (r (matrix/svd (to-device (Tensor Float32 [65 65] 1.0) 'vulkan))) (let (recon (contract (contract (ref r 'u) (matrix/diagonal-matrix (ref r 's)) [1 0]) (matrix/transpose (ref r 'v)) [1 0])) (let (cpu (to-device recon 'cpu)) (if (= (device recon) 'vulkan) (if (= (String (dtype recon)) \"Float32\") (if (< (abs (- (ref cpu [0 0]) 1.0)) 0.01) (< (abs (- (ref cpu [64 64]) 1.0)) 0.01) nil) nil) nil)))) true) true))", pass, fail);
        test_truthy(interp, "matrix norm vulkan Float32 all-ones 65 SVD-backed values when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (if (< (abs (- (matrix/norm (to-device (Tensor Float32 [65 65] 1.0) 'vulkan) 'spectral) 65.0)) 0.001) (< (abs (- (matrix/norm (to-device (Tensor Float32 [65 65] 1.0) 'vulkan) 'nuclear) 65.0)) 0.2) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix svd all-ones 65x65 path does not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix svd unavailable; all-ones 65x65 no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix svd all-ones 65x65 path does not call LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix svd vulkan singular value 0 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [0]) 3.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan singular value 1 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [1]) 2.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan u column when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'u) 'cpu) [0 0]) 1.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan v column when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'v) 'cpu) [1 1]) 1.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan outputs remain vulkan when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (r (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan))) (if (= (device (ref r 'u)) 'vulkan) (if (= (device (ref r 's)) 'vulkan) (= (device (ref r 'v)) 'vulkan) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix svd lazy vulkan succeeds when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/svd (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 's) 'cpu) [1]) 2.0)) 0.000001) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan wide v rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (shape (ref (matrix/svd (to-device (Tensor Float64 [2 3] [3 0 0 0 2 0]) 'vulkan)) 'v)) 0) 3) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan empty rows shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (r (matrix/svd (to-device (Tensor Float64 [0 3] 0.0) 'vulkan))) (if (= (length (to-device (ref r 's) 'cpu)) 0) (= (ref (shape (ref r 'v)) 0) 3) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan empty cols shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (r (matrix/svd (to-device (Tensor Float64 [3 0] 0.0) 'vulkan))) (if (= (length (to-device (ref r 's) 'cpu)) 0) (= (ref (shape (ref r 'u)) 0) 3) nil)) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd vulkan over previous max k succeeds without LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (r (matrix/svd (to-device (Tensor Float64 [65 65] 0.0) 'vulkan))) (let (s (to-device (ref r 's) 'cpu)) (if (= (length s) 65) (< (abs (- (ref s [0]) 0.0)) 0.000001) nil))) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix svd over previous max-k path does not call LAPACK"
                : "[PASS] tensor Vulkan matrix svd unavailable; over previous max-k no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix svd over previous max-k path does not call LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix svd vulkan over previous max k outputs remain vulkan when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (r (matrix/svd (to-device (Tensor Float64 [65 65] 0.0) 'vulkan))) (if (= (device (ref r 'u)) 'vulkan) (if (= (device (ref r 's)) 'vulkan) (= (device (ref r 'v)) 'vulkan) nil) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix svd vulkan wrong shape fails shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/svd (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix svd lazy vulkan wrong shape fails shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/svd (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix svd vulkan path does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (< (abs (- (ref (to-device (ref (matrix/svd (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 's) 'cpu) [0]) 3.0)) 0.000001) (< (abs (- (ref (to-device (ref (matrix/svd (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 's) 'cpu) [1]) 2.0)) 0.000001) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix svd does not call LAPACK"
                : "[PASS] tensor Vulkan matrix svd unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix svd does not call LAPACK");
            *fail += 1;
        }
    }
	    test_double(interp, "matrix eigenvalues diagonal first",
	        "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [0])",
        3.0, pass, fail);
    test_double(interp, "matrix eigenvalues diagonal second",
        "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [1])",
        2.0, pass, fail);
    test_double(interp, "matrix eigenvalues symmetric offdiag first",
        "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [2 1 1 2])) [0])",
        3.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dsyev_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dsyev_call_count();
        test_double(interp, "matrix eigenvalues LAPACK candidate",
            "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [0])",
            3.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dsyev_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dsyev matrix eigenvalues path"
                : "[PASS] tensor LAPACK backend dsyev unavailable; matrix eigenvalues fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dsyev matrix eigenvalues path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dsyev_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix eigenvalues forced pure fallback first",
            "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [0])",
            3.0, pass, fail);
        test_double(interp, "matrix eigenvalues forced pure fallback second",
            "(ref (matrix/eigenvalues (Tensor Float64 [2 2] [3 0 0 2])) [1])",
            2.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dsyev_available() == 0;
        omni_tensor_backend_lapack_dsyev_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dsyev disabled; matrix eigenvalues pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dsyev disabled; matrix eigenvalues pure fallback path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix eigenvectors values aligned",
        "(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])",
        3.0, pass, fail);
    test_eq_double(interp, "matrix eigenvectors vector column",
        "(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])",
        1.0, pass, fail);
    {
        bool lapack_available = omni_tensor_backend_lapack_dsyev_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dsyev_call_count();
        test_eq_double(interp, "matrix eigenvectors LAPACK candidate",
            "(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])",
            1.0, pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dsyev_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dsyev matrix eigenvectors path"
                : "[PASS] tensor LAPACK backend dsyev unavailable; matrix eigenvectors fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dsyev matrix eigenvectors path");
            (*fail)++;
        }
    }
    {
        omni_tensor_backend_lapack_dsyev_disable_for_tests(1);
        int fail_before = *fail;
        test_double(interp, "matrix eigenvectors forced pure fallback value",
            "(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])",
            3.0, pass, fail);
        test_eq_double(interp, "matrix eigenvectors forced pure fallback vector column",
            "(ref (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])",
            1.0, pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dsyev_available() == 0;
        omni_tensor_backend_lapack_dsyev_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dsyev disabled; matrix eigenvectors pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dsyev disabled; matrix eigenvectors pure fallback path");
            (*fail)++;
        }
    }
    test_eq(interp, "matrix eigenvectors vector shape",
        "(ref (shape (ref (matrix/eigenvectors (Tensor Float64 [2 2] [3 0 0 2])) 'vectors)) 0)",
        2, pass, fail);
    test_double(interp, "matrix eigenvalues realizes lazy input",
        "(ref (matrix/eigenvalues (map + (Tensor Float64 [2 2] [3 0 0 2]) 0.0)) [1])",
        2.0, pass, fail);
    test_error_contains(interp, "matrix eigenvalues rejects nonsymmetric input",
        "(matrix/eigenvalues (Tensor Float64 [2 2] [1 2 3 4]))",
        "matrix/eigenvalues: input matrix is not symmetric", pass, fail);
    test_error_contains(interp, "matrix eigenvectors rejects nonsymmetric input",
        "(matrix/eigenvectors (Tensor Float64 [2 2] [1 2 3 4]))",
        "matrix/eigenvectors: input matrix is not symmetric", pass, fail);
    test_error_contains(interp, "matrix eigenvalues rejects complex dtype",
        "(matrix/eigenvalues (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/eigenvalues: expected a square rank-2 Float64 Tensor", pass, fail);
    test_truthy(interp, "matrix eigenvalues vulkan computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/eigenvalues (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues vulkan result stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/eigenvalues (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors vulkan computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/eigenvectors (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'vectors) 'cpu) [0 0]) 1.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors vulkan outputs stay on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (= (device (ref (matrix/eigenvectors (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'values)) 'vulkan) (= (device (ref (matrix/eigenvectors (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'vectors)) 'vulkan) nil) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues lazy vulkan computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (matrix/eigenvalues (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 'cpu) [1]) 2.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors lazy vulkan computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (< (abs (- (ref (to-device (ref (matrix/eigenvectors (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 'values) 'cpu) [0]) 3.0)) 0.000000001) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues nonsymmetric vulkan reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenvalues (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-symmetric) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors nonsymmetric vulkan reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenvectors (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-symmetric) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues lazy nonsymmetric vulkan reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenvalues (map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/not-symmetric) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors lazy nonsymmetric vulkan reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenvectors (map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/not-symmetric) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues vulkan n>64 computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (vals (to-device (matrix/eigenvalues (to-device (matrix/identity 65) 'vulkan)) 'cpu)) (if (= (length vals) 65) (if (< (abs (- (ref vals [0]) 1.0)) 0.000000001) (< (abs (- (ref vals [64]) 1.0)) 0.000000001) nil) nil)) true) true))", pass, fail);
}
