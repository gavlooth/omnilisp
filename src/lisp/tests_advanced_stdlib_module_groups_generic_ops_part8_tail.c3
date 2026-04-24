module lisp;

import std::io;

fn void run_advanced_collections_generic_ops_part8_tail(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "matrix eigenpairs Vulkan Float64 computes diagonal values",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (ep (matrix/eigenpairs (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (= (String (dtype (ref ep 'values))) \"Complex128\") (= (String (dtype (ref ep 'vectors))) \"Complex128\") (< (abs (- (real-part (ref vals [0])) 3.0)) 0.000001) (< (abs (imag-part (ref vals [0]))) 0.000001) (< (abs (- (real-part (ref vals [1])) 2.0)) 0.000001) (< (abs (imag-part (ref vals [1]))) 0.000001) (< (abs (- (real-part (ref vecs [0 0])) 1.0)) 0.000001) (< (abs (- (real-part (ref vecs [1 1])) 1.0)) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Float32 computes diagonal values",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float32) (let (ep (matrix/eigenpairs (to-device (Tensor Float32 [2 2] [3 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (= (String (dtype (ref ep 'values))) \"Complex64\") (= (String (dtype (ref ep 'vectors))) \"Complex64\") (< (abs (- (real-part (ref vals [0])) 3.0)) 0.001) (< (abs (imag-part (ref vals [0]))) 0.001) (< (abs (- (real-part (ref vals [1])) 2.0)) 0.001) (< (abs (imag-part (ref vals [1]))) 0.001) (< (abs (- (real-part (ref vecs [0 0])) 1.0)) 0.001) (< (abs (- (real-part (ref vecs [1 1])) 1.0)) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs lazy Vulkan Float64 computes on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (ep (matrix/eigenpairs (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) vals (to-device (ref ep 'values) 'cpu)) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (< (abs (- (real-part (ref vals [0])) 3.0)) 0.000001) (< (abs (imag-part (ref vals [0]))) 0.000001) (< (abs (- (real-part (ref vals [1])) 2.0)) 0.000001) (< (abs (imag-part (ref vals [1]))) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Float64 exact complex shift residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (a (Tensor Float64 [2 2] [0 -1 1 0]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (= (device (ref ep 'values)) 'vulkan) (< (abs (- (abs (imag-part l0)) 1.0)) 0.000001) (< (+ (abs (- (* (Complex128 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Float32 exact complex shift residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float32) (let (a (Tensor Float32 [2 2] [0 -1 1 0]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (= (device (ref ep 'values)) 'vulkan) (< (abs (- (abs (imag-part l0)) 1.0)) 0.001) (< (+ (abs (- (* (Complex64 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Float64 mixed block deflation residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (ep (matrix/eigenpairs (to-device (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (block (define (res k) (let (l (ref vals [k]) v0 (ref vecs [0 k]) v1 (ref vecs [1 k]) v2 (ref vecs [2 k])) (+ (+ (abs (- (* (Complex128 -1 0) v1) (* l v0))) (abs (- v0 (* l v1)))) (abs (- (* (Complex128 2 0) v2) (* l v2)))))) (and (= (device (ref ep 'values)) 'vulkan) (< (res 0) 0.000001) (< (res 1) 0.000001) (< (res 2) 0.000001)))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Float32 mixed block deflation residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-float32) (let (ep (matrix/eigenpairs (to-device (Tensor Float32 [3 3] [0 -1 0 1 0 0 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (block (define (res k) (let (l (ref vals [k]) v0 (ref vecs [0 k]) v1 (ref vecs [1 k]) v2 (ref vecs [2 k])) (+ (+ (abs (- (* (Complex64 -1 0) v1) (* l v0))) (abs (- v0 (* l v1)))) (abs (- (* (Complex64 2 0) v2) (* l v2)))))) (and (= (device (ref ep 'values)) 'vulkan) (< (res 0) 0.001) (< (res 1) 0.001) (< (res 2) 0.001)))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs vulkan wrong shape fails backend before shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs lazy vulkan wrong shape fails backend before shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex128 computes general triangular values",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex128) (let (ep (matrix/eigenpairs (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) 0 (Complex128 3 -2)]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (= (String (dtype (ref ep 'values))) \"Complex128\") (= (String (dtype (ref ep 'vectors))) \"Complex128\") (= (String (ref vals [0])) \"3.0-2.0i\") (= (String (ref vals [1])) \"1.0+1.0i\") (= (ref (shape vecs) 0) 2) (= (ref (shape vecs) 1) 2))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex64 computes general triangular values",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex64) (let (ep (matrix/eigenpairs (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) 0 (Complex64 3 -2)]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (= (String (dtype (ref ep 'values))) \"Complex64\") (= (String (dtype (ref ep 'vectors))) \"Complex64\") (= (String (ref vals [0])) \"3.0-2.0i\") (= (String (ref vals [1])) \"1.0+1.0i\") (= (ref (shape vecs) 0) 2) (= (ref (shape vecs) 1) 2))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex128 general triangular residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex128) (let (a (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) 0 (Complex128 3 -2)]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0]) r00 (+ (* (Complex128 1 1) v00) (* (Complex128 2 0) v10)) r10 (* (Complex128 3 -2) v10)) (and (< (abs (- r00 (* l0 v00))) 0.000001) (< (abs (- r10 (* l0 v10))) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex64 general triangular residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex64) (let (a (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) 0 (Complex64 3 -2)]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0]) r00 (+ (* (Complex64 1 1) v00) (* (Complex64 2 0) v10)) r10 (* (Complex64 3 -2) v10)) (and (< (abs (- r00 (* l0 v00))) 0.001) (< (abs (- r10 (* l0 v10))) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex128 exact complex shift residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex128) (let (a (Tensor Complex128 [2 2] [0 -1 1 0]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (< (abs (- (abs (imag-part l0)) 1.0)) 0.000001) (< (+ (abs (- (* (Complex128 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.000001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex64 exact complex shift residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex64) (let (a (Tensor Complex64 [2 2] [0 -1 1 0]) ep (matrix/eigenpairs (to-device a 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu) l0 (ref vals [0]) v00 (ref vecs [0 0]) v10 (ref vecs [1 0])) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (< (abs (- (abs (imag-part l0)) 1.0)) 0.001) (< (+ (abs (- (* (Complex64 -1 0) v10) (* l0 v00))) (abs (- v00 (* l0 v10)))) 0.001))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex128 mixed block deflation residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex128) (let (ep (matrix/eigenpairs (to-device (Tensor Complex128 [3 3] [0 -1 0 1 0 0 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (block (define (res k) (let (l (ref vals [k]) v0 (ref vecs [0 k]) v1 (ref vecs [1 k]) v2 (ref vecs [2 k])) (+ (+ (abs (- (* (Complex128 -1 0) v1) (* l v0))) (abs (- v0 (* l v1)))) (abs (- (* (Complex128 2 0) v2) (* l v2)))))) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (< (res 0) 0.000001) (< (res 1) 0.000001) (< (res 2) 0.000001)))) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs Vulkan Complex64 mixed block deflation residual",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-eigenpairs-complex64) (let (ep (matrix/eigenpairs (to-device (Tensor Complex64 [3 3] [0 -1 0 1 0 0 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu) vecs (to-device (ref ep 'vectors) 'cpu)) (block (define (res k) (let (l (ref vals [k]) v0 (ref vecs [0 k]) v1 (ref vecs [1 k]) v2 (ref vecs [2 k])) (+ (+ (abs (- (* (Complex64 -1 0) v1) (* l v0))) (abs (- v0 (* l v1)))) (abs (- (* (Complex64 2 0) v2) (* l v2)))))) (and (= (device (ref ep 'values)) 'vulkan) (= (device (ref ep 'vectors)) 'vulkan) (< (res 0) 0.001) (< (res 1) 0.001) (< (res 2) 0.001)))) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgeev_call_count();
        test_truthy(interp, "matrix eigenpairs Vulkan real native path does not call LAPACK",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (ep (matrix/eigenpairs (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu)) (and (= (device (ref ep 'vectors)) 'vulkan) (< (abs (- (real-part (ref vals [0])) 3.0)) 0.000001))) true) (if (ref vulkan-backend 'matrix-eigenpairs-float64) (let (ep (matrix/eigenpairs (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) vals (to-device (ref ep 'values) 'cpu)) (and (= (device (ref ep 'vectors)) 'vulkan) (< (abs (- (real-part (ref vals [0])) 3.0)) 0.000001))) true)))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgeev_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan real matrix eigenpairs native path does not call LAPACK"
                : "[PASS] tensor Vulkan real matrix eigenpairs unavailable; no-LAPACK native path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan real matrix eigenpairs native path does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_complex_available = omni_tensor_backend_vulkan_available() != 0 &&
            (omni_tensor_backend_vulkan_float64_available() != 0 || omni_tensor_backend_vulkan_float32_available() != 0);
        long zgeev_calls_before = omni_tensor_backend_lapack_zgeev_call_count();
        long cgeev_calls_before = omni_tensor_backend_lapack_cgeev_call_count();
        test_truthy(interp, "matrix eigenpairs Vulkan fixed complex native path does not call LAPACK",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-eigenpairs-complex128) (let (ep (matrix/eigenpairs (to-device (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) 0 (Complex128 3 -2)]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu)) (and (= (device (ref ep 'vectors)) 'vulkan) (= (String (ref vals [0])) \"3.0-2.0i\"))) true) (if (ref vulkan-backend 'matrix-eigenpairs-complex64) (let (ep (matrix/eigenpairs (to-device (Tensor Complex64 [2 2] [(Complex64 1 1) (Complex64 2 0) 0 (Complex64 3 -2)]) 'vulkan)) vals (to-device (ref ep 'values) 'cpu)) (and (= (device (ref ep 'vectors)) 'vulkan) (= (String (ref vals [0])) \"3.0-2.0i\"))) true)))",
            pass, fail);
        bool no_complex_lapack_path_ok = !vulkan_complex_available ||
            (omni_tensor_backend_lapack_zgeev_call_count() == zgeev_calls_before &&
                omni_tensor_backend_lapack_cgeev_call_count() == cgeev_calls_before);
        if (no_complex_lapack_path_ok) {
            emit_pass_literal(vulkan_complex_available
                ? "[PASS] tensor Vulkan fixed complex matrix eigenpairs native path does not call LAPACK"
                : "[PASS] tensor Vulkan fixed complex matrix eigenpairs unavailable; no-LAPACK native path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex matrix eigenpairs native path does not call LAPACK");
            *fail += 1;
        }
    }
    test_eq_double(interp, "contract multi-axis",
        "(ref (contract (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) (Tensor Float64 [2 3 2] 1.0) [1 2] [0 2]) [1 2])",
        26.0, pass, fail);
    test_eq_double(interp, "contract multi-axis pair shorthand",
        "(ref (contract (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) (Tensor Float64 [2 3 2] 1.0) [[1 0] [2 2]]) [1 2])",
        26.0, pass, fail);
    test_eq_double(interp, "realize contracted tensor into destination",
        "(let (src (contract (Tensor Float64 [2 2] [1 2 3 4]) (Tensor Float64 [2 2] [5 6 7 8]) [1] [0]) dst (Tensor Float64 [2 2] 0.0)) (block (realize src dst) (ref dst [0 1])))",
        22.0, pass, fail);
    test_eq_double(interp, "realize contracted expression directly into destination",
        "(let (dst (Tensor Float64 [2 2] 0.0)) (block (realize (contract (Tensor Float64 [2 2] [1 2 3 4]) (Tensor Float64 [2 2] [5 6 7 8]) [1] [0]) dst) (ref dst [0 1])))",
        22.0, pass, fail);
    test_eq_double(interp, "failed realize contracted expression leaves destination unchanged",
        "(let (dst (Tensor Float64 [] 99.0)) (block (handle (realize (contract (map (lambda (x) (if (= x 2) 'bad x)) (Tensor Float64 [2] [1 2])) (Tensor Float64 [2] [10 20]) [0] [0]) dst) (raise msg nil)) (ref dst [])))",
        99.0, pass, fail);
    test_eq_double(interp, "lazy contract tensor survives function return boundary",
        "(block (define (mk-contract) (contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [10 20]) [0] [0])) (ref (mk-contract) []))",
        50.0, pass, fail);
    test_eq_double(interp, "lazy contract tensor survives closure capture",
        "(let (src (contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [10 20]) [0] [0])) (let (f (lambda () (ref src []))) (f)))",
        50.0, pass, fail);
    test_error_contains(interp, "realize contracted expression rejects aliased destination",
        "(let (dst (Tensor Float64 [2 2] [1 2 3 4])) (realize (contract dst dst [1] [0]) dst))",
        "contract: destination aliases source tensor", pass, fail);
    test_error_contains(interp, "realize nested contracted expression rejects aliased destination",
        "(let (dst (Tensor Float64 [2 2] [1 2 3 4])) (let (src (map + dst 0)) (realize (contract src dst [1] [0]) dst)))",
        "contract: destination aliases source tensor", pass, fail);
    test_eq(interp, "realize zero-size contracted expression into zero-size destination",
        "(let (dst (Tensor Float64 [0] 0.0)) (let (src (contract (Tensor Float64 [0 1] 0.0) (Tensor Float64 [1] [10]) [1] [0])) (block (realize src dst) (length dst))))",
        0, pass, fail);
    test_error_contains(interp, "contract axis length mismatch",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) [0] [])",
        "contract: axis list length mismatch", pass, fail);
    test_error_contains(interp, "contract left axis out of range",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) [1] [0])",
        "contract: left axis out of range", pass, fail);
    test_error_contains(interp, "contract right axis out of range",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) [0] [1])",
        "contract: right axis out of range", pass, fail);
    test_error_contains(interp, "contract duplicate axis",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2 2] 1.0) [0 0] [0 1])",
        "contract: duplicate contracted axis", pass, fail);
    test_error_contains(interp, "contract duplicate axis with negative index normalization",
        "(contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2 3] [7 8 9 10 11 12]) [1 -1] [0 1])",
        "contract: duplicate contracted axis", pass, fail);
    test_error_contains(interp, "contract dimension mismatch",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [3] [1 2 3]) [0] [0])",
        "contract: contracted dimensions must match", pass, fail);
    test_error_contains(interp, "contract rejects non-tensor operand",
        "(contract [1 2] (Tensor Float64 [2] [1 2]) [0] [0])",
        "contract: expected Tensor operands", pass, fail);
    test_error_contains(interp, "contract rejects scalar axes",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) 0 [0])",
        "contract: axes must be arrays or proper lists of integers", pass, fail);
    test_error_contains(interp, "contract rejects malformed axis pair shorthand",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) [0])",
        "contract: axis pairs must contain two-integer arrays or proper lists", pass, fail);
    test_error_contains(interp, "contract rejects non-integer axes",
        "(contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [1 2]) ['bad] [0])",
        "contract: axes must be integers", pass, fail);
    // ref on lists (walks cons chain, not just pair 0/1)
    test_eq(interp, "ref list 0", "(ref '(10 20 30) 0)", 10, pass, fail);
    test_eq(interp, "ref list 2", "(ref '(10 20 30) 2)", 30, pass, fail);
    test_eq(interp, "ref list -1", "(ref '(10 20 30) -1)", 30, pass, fail);
    test_error(interp, "ref list oob", "(ref '(10 20 30) 5)", pass, fail);
    test_eq(interp, ".[-1] array", "(let (a [10 20 30]) a.[-1])", 30, pass, fail);
    test_eq(interp, ".[-1] list", "(let (l '(10 20 30)) l.[-1])", 30, pass, fail);
    test_eq(interp, ".[-2] list", "(let (l '(10 20 30)) l.[-2])", 20, pass, fail);
    test_error(interp, ".[-4] list oob", "(let (l '(10 20 30)) l.[-4])", pass, fail);
    test_str(interp, "char-at -1", "(char-at \"abc\" -1)", pass, fail);
    setup(interp, "(define neg-arr [1 2 3])");
    test_tag_interp(interp, "set! array returns void", "(set! neg-arr -1 99)", VOID, pass, fail);
    test_eq(interp, "set! array -1", "(ref neg-arr 2)", 99, pass, fail);
    // length - generic
    test_eq(interp, "length array", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "length dict", "(length {'a 1 'b 2})", 2, pass, fail);
    test_eq(interp, "length string", "(length \"hello\")", 5, pass, fail);
    test_eq(interp, "length utf8 string", "(length \"caf\xc3\xa9\")", 4, pass, fail);
    test_str_val(interp, "ref utf8 string", "(ref \"caf\xc3\xa9\" 3)", "\xc3\xa9", pass, fail);
    test_str_val(interp, ".[ ] utf8 string", "(let (s \"\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e\") s.[1])", "\xe6\x9c\xac", pass, fail);
    test_eq(interp, "length list", "(length '(1 2 3 4))", 4, pass, fail);
    test_eq(interp, "length dotted cons", "(length (cons 1 2))", 2, pass, fail);
    test_eq(interp, "length dotted cons chain", "(length (cons 1 (cons 2 3)))", 3, pass, fail);
    test_error_contains(interp, "length rejects trailing arg", "(length [1 2] 99)", "arity mismatch", pass, fail);
    test_error_contains(interp, "ref rejects trailing arg", "(ref [10 20] 0 99)", "arity mismatch", pass, fail);
    // push!
    setup(interp, "(define ga [1 2])");
    test_tag_interp(interp, "push! returns void", "(push! ga 3)", VOID, pass, fail);
    test_eq(interp, "push! generic", "(length ga)", 3, pass, fail);
    test_eq(interp, "push! val", "(ref ga 2)", 3, pass, fail);
    // keys / values / has? / remove!
    setup(interp, "(define gd {'x 10 'y 20})");
    test_eq(interp, "keys len", "(length (keys gd))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values gd))", 2, pass, fail);
    test_truthy(interp, "has? yes", "(has? gd 'x)", pass, fail);
    test_nil(interp, "has? no", "(has? gd 'z)", pass, fail);
    test_tag_interp(interp, "remove! returns void", "(remove! gd 'x)", VOID, pass, fail);
    test_nil(interp, "remove! gone", "(has? gd 'x)", pass, fail);
    test_eq(interp, "remove! count", "(length gd)", 1, pass, fail);
}
