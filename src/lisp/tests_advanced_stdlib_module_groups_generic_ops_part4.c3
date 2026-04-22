module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part4(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp, "map mixed BigComplex Float64 tensors rejected",
        "(map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (Tensor Float64 [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "contract BigComplex vector dot",
        "(String (ref (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0]) []))",
        "11+3i", pass, fail);
    test_str_val(interp, "contract BigComplex matrix product",
        "(String (ref (contract (Tensor BigComplex [2 2] [(BigComplex 1 1) 2 3 4]) (Tensor BigComplex [2 2] [5 6 7 8]) [1] [0]) [0 1]))",
        "22+6i", pass, fail);
    test_str_val(interp, "contract BigComplex zero-size identity",
        "(String (ref (contract (Tensor BigComplex [0] []) (Tensor BigComplex [0] []) [0] [0]) []))",
        "0+0i", pass, fail);
    test_str_val(interp, "realize BigComplex contract into destination",
        "(let (dst (Tensor BigComplex [] 0)) (block (realize (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0]) dst) (String (ref dst []))))",
        "11+3i", pass, fail);
    test_str_val(interp, "lazy BigComplex contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0])) (String (ref (mk) [])))",
        "11+3i", pass, fail);
    test_str_val(interp, "lazy BigComplex contract survives closure capture",
        "(let (src (contract (Tensor BigComplex [2] [(BigComplex 1 1) 2]) (Tensor BigComplex [2] [3 4]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "11+3i", pass, fail);
    test_error_contains(interp, "contract mixed BigComplex Float64 tensors rejected",
        "(contract (Tensor BigComplex [1] [(BigComplex 1 2)]) (Tensor Float64 [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "map Complex128 scalar left",
        "(String (ref (map + (Complex128 2 1) (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)])) [1]))",
        "5.0+5.0i", pass, fail);
    test_str_val(interp, "map Complex64 tensor broadcast",
        "(String (ref (map + (Tensor Complex64 [2 1] [(Complex64 1 1) (Complex64 2 2)]) (Tensor Complex64 [1 2] [(Complex64 10 10) (Complex64 20 20)])) [1 1]))",
        "22.0+22.0i", pass, fail);
    test_str_val(interp, "map Complex64 tensor plus Complex128 scalar widens",
        "(String (dtype (map + (Tensor Complex64 [1] [(Complex64 1 2)]) (Complex128 3 4))))",
        "Complex128", pass, fail);
    test_str_val(interp, "map Complex64 tensor plus Complex128 tensor widens",
        "(String (ref (map + (Tensor Complex64 [1] [(Complex64 1 2)]) (Tensor Complex128 [1] [(Complex128 3 4)])) [0]))",
        "4.0+6.0i", pass, fail);
    test_str_val(interp, "map Float64 tensor plus Complex64 scalar returns Complex64",
        "(String (ref (map + (Tensor Float64 [1] [2]) (Complex64 1 3)) [0]))",
        "3.0+3.0i", pass, fail);
    test_str_val(interp, "realize mapped Complex128 expression into destination",
        "(let (dst (Tensor Complex128 [1] 0)) (block (realize (map + (Tensor Complex128 [1] [(Complex128 1 2)]) (Complex128 3 4)) dst) (String (ref dst [0]))))",
        "4.0+6.0i", pass, fail);
    test_str_val(interp, "contract Complex128 vector dot",
        "(String (ref (contract (Tensor Complex128 [2] [(Complex128 1 1) (Complex128 2 0)]) (Tensor Complex128 [2] [(Complex128 1 0) (Complex128 0 1)]) [0] [0]) []))",
        "1.0+3.0i", pass, fail);
    test_str_val(interp, "contract Complex64 matrix product",
        "(String (ref (contract (Tensor Complex64 [1 2] [(Complex64 1 1) (Complex64 2 0)]) (Tensor Complex64 [2 1] [(Complex64 1 0) (Complex64 0 1)]) [1] [0]) [0 0]))",
        "1.0+3.0i", pass, fail);
    test_error_contains(interp, "pow Complex128 tensor rejected",
        "(pow (Tensor Complex128 [1] [(Complex128 1 2)]) 2)",
        "pow: fixed-width complex numbers are not supported", pass, fail);
    test_error_contains(interp, "map tensor broadcast shape mismatch",
        "(map + (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2 2] [1 2 3 4]))",
        "map: tensor shape mismatch", pass, fail);
    test_error_contains(interp, "map tensor shape mismatch",
        "(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [3] [1 2 3]))",
        "map: tensor shape mismatch", pass, fail);
    test_error_contains(interp, "map tensor function dtype mismatch",
        "(realize (map (lambda (x) 'bad) (Tensor Float64 [1] [1])))",
        "map: function result dtype mismatch", pass, fail);
    test_error_contains(interp, "map tensor rejects nonnumeric source",
        "(map + (Tensor Float64 [1] [1]) 'bad)",
        "map: tensor inputs must be Tensors or numbers", pass, fail);
    test_error_contains(interp, "map tensor rejects noncallable",
        "(map nil (Tensor Float64 [1] [1]))",
        "map: function must be callable", pass, fail);
    // contract - tensor summed-index lazy expression surface
    test_eq_double(interp, "contract matrix product rank-2",
        "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [1] [0]) [1 1])",
        154.0, pass, fail);
    test_eq(interp, "contract matrix product rank",
        "(rank (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [1] [0]))",
        2, pass, fail);
    test_eq_double(interp, "contract vector dot scalar",
        "(ref (contract (Tensor Float64 [3] [1 2 3]) (Tensor Float64 [3] [10 20 30]) [0] [0]) [])",
        140.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_ddot_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_ddot_call_count();
        test_eq_double(interp, "contract vector dot BLAS candidate",
            "(ref (contract (Tensor Float64 [3] [1 2 3]) (Tensor Float64 [3] [10 20 30]) [0 0]) [])",
            140.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_ddot_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend ddot contract path"
                : "[PASS] tensor BLAS backend ddot unavailable; contract fallback path retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend ddot contract path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "contract list axes",
        "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) '(1) '(0)) [1 1])",
        154.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemm_call_count();
        test_eq_double(interp, "contract single axis pair shorthand",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [1 0]) [1 1])",
            154.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemm_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemm contract path"
                : "[PASS] tensor BLAS backend unavailable; contract fallback path retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemm contract path");
            (*fail)++;
        }
    }
    {
        bool blas_available = omni_tensor_backend_blas_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemm_call_count();
        test_eq_double(interp, "contract rank-2 transpose left axis pair",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2 2] [7 8 9 10]) [0 0]) [2 1])",
            84.0, pass, fail);
        test_eq_double(interp, "contract rank-2 transpose right axis pair",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2 3] [7 8 9 10 11 12]) [1 1]) [0 1])",
            68.0, pass, fail);
        test_eq_double(interp, "contract rank-2 transpose both axis pair",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [4 2] [7 8 9 10 11 12 13 14]) [0 1]) [2 3])",
            123.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemm_call_count() >= blas_calls_before + 3;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemm transpose contract paths"
                : "[PASS] tensor BLAS backend unavailable; transpose contract fallback paths retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemm transpose contract paths");
            (*fail)++;
        }
    }
    {
        bool blas_available = omni_tensor_backend_blas_dgemv_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dgemv_call_count();
        test_eq_double(interp, "contract matrix vector BLAS candidate",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3] [10 20 30]) [1 0]) [1])",
            320.0, pass, fail);
        test_eq_double(interp, "contract transposed matrix vector BLAS candidate",
            "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2] [10 20]) [0 0]) [2])",
            150.0, pass, fail);
        test_eq_double(interp, "contract vector matrix BLAS candidate",
            "(ref (contract (Tensor Float64 [3] [10 20 30]) (Tensor Float64 [3 2] [1 2 3 4 5 6]) [0 0]) [1])",
            280.0, pass, fail);
        test_eq_double(interp, "contract vector transposed matrix BLAS candidate",
            "(ref (contract (Tensor Float64 [3] [10 20 30]) (Tensor Float64 [2 3] [1 2 3 4 5 6]) [0 1]) [1])",
            320.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dgemv_call_count() >= blas_calls_before + 4;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dgemv contract paths"
                : "[PASS] tensor BLAS backend dgemv unavailable; contract fallback paths retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dgemv contract paths");
            (*fail)++;
        }
    }
    test_eq_double(interp, "contract nested axis pair shorthand",
        "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [[1 0]]) [1 1])",
        154.0, pass, fail);
    test_eq_double(interp, "contract negative axes",
        "(ref (contract (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [-1] [-2]) [1 1])",
        154.0, pass, fail);
    test_eq_double(interp, "contract zero contracted dimension scalar",
        "(ref (contract (Tensor Float64 [0] 0.0) (Tensor Float64 [0] 0.0) [0] [0]) [])",
        0.0, pass, fail);
    test_eq_double(interp, "contract rank-0 scalar",
        "(ref (contract (Tensor Float64 [] 3.0) (Tensor Float64 [] 4.0) [] []) [])",
        12.0, pass, fail);
    test_eq_double(interp, "contract zero contracted dimension non-scalar",
        "(ref (contract (Tensor Float64 [2 0 1] 0.0) (Tensor Float64 [2 0 1] 0.0) [1] [1]) [1 0 1 0])",
        0.0, pass, fail);
    {
        bool blas_available = omni_tensor_backend_blas_dger_available() != 0;
        long blas_calls_before = omni_tensor_backend_blas_dger_call_count();
        test_eq_double(interp, "contract zero axes outer product",
            "(ref (contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [3] [10 20 30]) [] []) [1 2])",
            60.0, pass, fail);
        bool blas_path_ok = !blas_available || omni_tensor_backend_blas_dger_call_count() > blas_calls_before;
        if (blas_path_ok) {
            emit_pass_literal(blas_available
                ? "[PASS] tensor BLAS backend dger outer-product path"
                : "[PASS] tensor BLAS backend dger unavailable; outer-product fallback retained");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor BLAS backend dger outer-product path");
            (*fail)++;
        }
    }
    test_eq_double(interp, "matrix transpose Float64 value",
        "(ref (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6])) [2 1])",
        6.0, pass, fail);
    test_str_val(interp, "matrix transpose row shape",
        "(String (ref (shape (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6]))) 0))",
        "3", pass, fail);
    test_str_val(interp, "matrix transpose column shape",
        "(String (ref (shape (matrix/transpose (Tensor Float64 [2 3] [1 2 3 4 5 6]))) 1))",
        "2", pass, fail);
    test_eq_double(interp, "matrix transpose realizes lazy input",
        "(ref (matrix/transpose (map + (Tensor Float64 [2 2] [1 2 3 4]) 0.0)) [1 0])",
        2.0, pass, fail);
    test_truthy(interp, "matrix transpose Float32 value",
        "(let (x (ref (matrix/transpose (Tensor Float32 [2 3] [1 2 3 4 5 6])) [2 1])) (and (= x 6.0) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/transpose (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) 'cpu) [2 1]) 6.0) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/transpose (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/transpose (map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (matrix/transpose (to-device (Tensor Float64 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float32 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/transpose (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan)) 'cpu) [2 1]) 6.0) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float32 preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/transpose (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (matrix/transpose (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan)))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/transpose (map + (to-device (Tensor Float32 [2 2] [1 2 3 4]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float32 zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (length (to-device (matrix/transpose (to-device (Tensor Float32 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Float64 dtype remains Float64 when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (String (dtype (matrix/transpose (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)))) \"Float64\") true) true))", pass, fail);
    test_truthy(interp, "matrix transpose-view rejects nested Vulkan views",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (matrix/transpose-view (matrix/transpose-view (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Complex128 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex128) (let (result (matrix/transpose (to-device (Tensor Complex128 [2 1] [(Complex128 1 2) (Complex128 3 4)]) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex128\") (= (String (ref cpu [0 1])) \"3.0+4.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix transpose vulkan Complex64 zero-size when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex64) (let (result (matrix/transpose (to-device (Tensor Complex64 [0 3] 0.0) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex64\") (= (length cpu) 0))) true))", pass, fail);
    test_str_val(interp, "matrix transpose BigInteger clone",
        "(String (ref (matrix/transpose (Tensor BigInteger [2 2] [(BigInteger \"9223372036854775808\") 2 3 4])) [0 0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "matrix transpose BigFloat clone",
        "(String (ref (matrix/transpose (Tensor BigFloat [1 2] [(BigFloat \"1.25\") (BigFloat \"2.5\")])) [1 0]))",
        "2.5", pass, fail);
    test_str_val(interp, "matrix transpose BigComplex clone",
        "(String (ref (matrix/transpose (Tensor BigComplex [2 1] [(BigComplex 1 2) (BigComplex 3 4)])) [0 1]))",
        "3+4i", pass, fail);
    test_str_val(interp, "matrix transpose Complex128 clone",
        "(String (ref (matrix/transpose (Tensor Complex128 [2 1] [(Complex128 1 2) (Complex128 3 4)])) [0 1]))",
        "3.0+4.0i", pass, fail);
    test_str_val(interp, "matrix transpose preserves dtype",
        "(String (dtype (matrix/transpose (Tensor BigFloat [1 1] [(BigFloat \"1.25\")]))))",
        "BigFloat", pass, fail);
    test_error_contains(interp, "matrix transpose rejects non-rank-2 input",
        "(matrix/transpose (Tensor Float64 [2] [1 2]))",
        "matrix/transpose: expected a rank-2 Tensor", pass, fail);
    test_eq_double(interp, "matrix diagonal rectangular Float64 value",
        "(ref (matrix/diagonal (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1])",
        5.0, pass, fail);
    test_str_val(interp, "matrix diagonal rectangular shape",
        "(String (ref (shape (matrix/diagonal (Tensor Float64 [2 3] [1 2 3 4 5 6]))) 0))",
        "2", pass, fail);
    test_eq_double(interp, "matrix diagonal realizes lazy input",
        "(ref (matrix/diagonal (map + (Tensor Float64 [2 2] [1 2 3 4]) 0.0)) [1])",
        4.0, pass, fail);
    test_truthy(interp, "matrix diagonal Float32 value",
        "(let (x (ref (matrix/diagonal (Tensor Float32 [2 3] [1 2 3 4 5 6])) [1])) (and (= x 5.0) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/diagonal (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) 'cpu) [1]) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/diagonal (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/diagonal (map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_str_val(interp, "matrix diagonal empty rank-1 shape",
        "(String (ref (shape (matrix/diagonal (Tensor Float64 [0 3] 0.0))) 0))",
        "0", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (matrix/diagonal (to-device (Tensor Float64 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Float32 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/diagonal (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan)) 'cpu) [1]) 5.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Float32 preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/diagonal (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (matrix/diagonal (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan)))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/diagonal (map + (to-device (Tensor Float32 [2 2] [1 2 3 4]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Float32 zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (length (to-device (matrix/diagonal (to-device (Tensor Float32 [0 3] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_str_val(interp, "matrix diagonal BigInteger clone",
        "(String (ref (matrix/diagonal (Tensor BigInteger [2 2] [(BigInteger \"9223372036854775808\") 2 3 4])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "matrix diagonal BigFloat clone",
        "(String (ref (matrix/diagonal (Tensor BigFloat [2 2] [(BigFloat \"1.25\") 2 3 (BigFloat \"2.5\")])) [1]))",
        "2.5", pass, fail);
    test_str_val(interp, "matrix diagonal BigComplex clone",
        "(String (ref (matrix/diagonal (Tensor BigComplex [2 2] [(BigComplex 1 2) 0 0 (BigComplex 3 4)])) [1]))",
        "3+4i", pass, fail);
    test_str_val(interp, "matrix diagonal Complex64 clone",
        "(String (ref (matrix/diagonal (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)])) [1]))",
        "3.0+4.0i", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Complex64 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex64) (let (result (matrix/diagonal (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) 0 0 (Complex64 3 4)]) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex64\") (= (String (ref cpu [1])) \"3.0+4.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix diagonal vulkan Complex128 zero-size when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex128) (let (result (matrix/diagonal (to-device (Tensor Complex128 [0 3] 0.0) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex128\") (= (length cpu) 0))) true))", pass, fail);
    test_str_val(interp, "matrix diagonal preserves dtype",
        "(String (dtype (matrix/diagonal (Tensor BigComplex [1 1] [(BigComplex 1 2)]))))",
        "BigComplex", pass, fail);
    test_error_contains(interp, "matrix diagonal rejects non-rank-2 input",
        "(matrix/diagonal (Tensor Float64 [2] [1 2]))",
        "matrix/diagonal: expected a rank-2 Tensor", pass, fail);
    test_eq_double(interp, "matrix diagonal-matrix Float64 diagonal value",
        "(ref (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3])) [2 2])",
        3.0, pass, fail);
    test_eq_double(interp, "matrix diagonal-matrix Float64 off diagonal zero",
        "(ref (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3])) [0 2])",
        0.0, pass, fail);
    test_str_val(interp, "matrix diagonal-matrix row shape",
        "(String (ref (shape (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3]))) 0))",
        "3", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix column shape",
        "(String (ref (shape (matrix/diagonal-matrix (Tensor Float64 [3] [1 2 3]))) 1))",
        "3", pass, fail);
    test_eq_double(interp, "matrix diagonal-matrix realizes lazy input",
        "(ref (matrix/diagonal-matrix (map + (Tensor Float64 [2] [1 2]) 0.0)) [1 1])",
        2.0, pass, fail);
    test_truthy(interp, "matrix diagonal-matrix Float32 diagonal value",
        "(let (x (ref (matrix/diagonal-matrix (Tensor Float32 [3] [1 2 3])) [2 2])) (and (= x 3.0) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan roundtrips diagonal when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/diagonal-matrix (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)) 'cpu) [2 2]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan roundtrips zero fill when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (matrix/diagonal-matrix (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)) 'cpu) [0 2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/diagonal-matrix (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/diagonal-matrix (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix empty square shape",
        "(String (ref (shape (matrix/diagonal-matrix (Tensor Float64 [0] 0.0))) 0))",
        "0", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (matrix/diagonal-matrix (to-device (Tensor Float64 [0] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 roundtrips diagonal when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/diagonal-matrix (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan)) 'cpu) [2 2]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 roundtrips zero fill when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (matrix/diagonal-matrix (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan)) 'cpu) [0 2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/diagonal-matrix (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (matrix/diagonal-matrix (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan)))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (matrix/diagonal-matrix (map + (to-device (Tensor Float32 [2] [1 2]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Float32 zero-size preserves empty result when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (length (to-device (matrix/diagonal-matrix (to-device (Tensor Float32 [0] 0.0) 'vulkan)) 'cpu)) 0) true) true))", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix BigInteger clone",
        "(String (ref (matrix/diagonal-matrix (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 4])) [0 0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix BigInteger off diagonal zero",
        "(String (ref (matrix/diagonal-matrix (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 4])) [0 1]))",
        "0", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix BigFloat clone",
        "(String (ref (matrix/diagonal-matrix (Tensor BigFloat [2] [(BigFloat \"1.25\") (BigFloat \"2.5\")])) [1 1]))",
        "2.5", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix BigComplex clone",
        "(String (ref (matrix/diagonal-matrix (Tensor BigComplex [2] [(BigComplex 1 2) (BigComplex 3 4)])) [1 1]))",
        "3+4i", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix Complex128 off diagonal zero",
        "(String (ref (matrix/diagonal-matrix (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)])) [0 1]))",
        "0.0+0.0i", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Complex128 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex128) (let (result (matrix/diagonal-matrix (to-device (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)]) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex128\") (= (String (ref cpu [1 1])) \"3.0+4.0i\") (= (String (ref cpu [0 1])) \"0.0+0.0i\"))) true))", pass, fail);
    test_truthy(interp, "matrix diagonal-matrix vulkan Complex64 zero-size when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-structural-complex64) (let (result (matrix/diagonal-matrix (to-device (Tensor Complex64 [0] 0.0) 'vulkan)) cpu (to-device result 'cpu)) (and (= (device result) 'vulkan) (= (String (dtype result)) \"Complex64\") (= (length cpu) 0))) true))", pass, fail);
    test_str_val(interp, "matrix diagonal-matrix preserves dtype",
        "(String (dtype (matrix/diagonal-matrix (Tensor BigComplex [1] [(BigComplex 1 2)]))))",
        "BigComplex", pass, fail);
    test_error_contains(interp, "matrix diagonal-matrix rejects rank-2 input",
        "(matrix/diagonal-matrix (Tensor Float64 [1 1] [1]))",
        "matrix/diagonal-matrix: expected a rank-1 Tensor", pass, fail);
    test_eq_double(interp, "matrix identity default Float64 diagonal",
        "(ref (matrix/identity 3) [2 2])",
        1.0, pass, fail);
    test_eq_double(interp, "matrix identity default Float64 off diagonal",
        "(ref (matrix/identity 3) [0 2])",
        0.0, pass, fail);
    test_str_val(interp, "matrix identity row shape",
        "(String (ref (shape (matrix/identity 3)) 0))",
        "3", pass, fail);
    test_str_val(interp, "matrix identity column shape",
        "(String (ref (shape (matrix/identity 3)) 1))",
        "3", pass, fail);
    test_str_val(interp, "matrix identity empty square shape",
        "(String (ref (shape (matrix/identity 0)) 0))",
        "0", pass, fail);
    test_str_val(interp, "matrix identity BigInteger one",
        "(String (ref (matrix/identity 2 BigInteger) [1 1]))",
        "1", pass, fail);
    test_str_val(interp, "matrix identity BigInteger zero",
        "(String (ref (matrix/identity 2 BigInteger) [0 1]))",
        "0", pass, fail);
    test_truthy(interp, "matrix identity Float32 diagonal",
        "(let (x (ref (matrix/identity 2 Float32) [1 1])) (and (= x 1.0) (= (type-of x) 'Float32)))", pass, fail);
    test_str_val(interp, "matrix identity BigFloat one",
        "(String (ref (matrix/identity 2 BigFloat) [1 1]))",
        "1", pass, fail);
    test_str_val(interp, "matrix identity BigComplex one",
        "(String (ref (matrix/identity 2 BigComplex) [1 1]))",
        "1+0i", pass, fail);
    test_str_val(interp, "matrix identity Complex64 one",
        "(String (ref (matrix/identity 2 Complex64) [1 1]))",
        "1.0+0.0i", pass, fail);
    test_str_val(interp, "matrix identity preserves dtype",
        "(String (dtype (matrix/identity 2 BigComplex)))",
        "BigComplex", pass, fail);
    test_error_contains(interp, "matrix identity rejects negative size",
        "(matrix/identity -1)",
        "matrix/identity: size must be a non-negative Integer", pass, fail);
    test_error_contains(interp, "matrix identity rejects invalid dtype",
        "(matrix/identity 2 \"Float64\")",
        "matrix/identity: dtype must be Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex", pass, fail);
    test_str_val(interp, "matrix rank full rectangular",
        "(String (matrix/rank (Tensor Float64 [2 3] [1 0 0 0 1 0])))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank deficient rectangular",
        "(String (matrix/rank (Tensor Float64 [2 3] [1 2 3 2 4 6])))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank empty rows",
        "(String (matrix/rank (Tensor Float64 [0 3] 0.0)))",
        "0", pass, fail);
    test_str_val(interp, "matrix rank realizes lazy input",
        "(String (matrix/rank (map + (Tensor Float64 [2 2] [1 0 0 1]) 0.0)))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank tolerance suppresses small pivot",
        "(String (matrix/rank (Tensor Float64 [2 2] [1 0 0 0.0000000000001]) 0.000000000001))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank zero tolerance keeps small pivot",
        "(String (matrix/rank (Tensor Float64 [2 2] [1 0 0 0.0000000000001]) 0.0))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Float32 full rectangular",
        "(String (matrix/rank (Tensor Float32 [2 3] [1 0 0 0 1 0])))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Float32 deficient rectangular",
        "(String (matrix/rank (Tensor Float32 [2 3] [1 2 3 2 4 6])))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Float32 empty rows",
        "(String (matrix/rank (Tensor Float32 [0 3] 0.0)))",
        "0", pass, fail);
    test_str_val(interp, "matrix rank Float32 empty columns",
        "(String (matrix/rank (Tensor Float32 [3 0] 0.0)))",
        "0", pass, fail);
    test_str_val(interp, "matrix rank Float32 realizes lazy input",
        "(String (matrix/rank (map + (Tensor Float32 [2 2] [1 0 0 1]) 0.0)))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Float32 tolerance suppresses small pivot",
        "(String (matrix/rank (Tensor Float32 [2 2] [1 0 0 0.0001]) 0.001))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Float32 zero tolerance keeps small pivot",
        "(String (matrix/rank (Tensor Float32 [2 2] [1 0 0 0.0001]) 0.0))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Complex128 full rectangular",
        "(String (matrix/rank (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)])))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Complex128 deficient rectangular",
        "(String (matrix/rank (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 -1) (Complex128 2 2) (Complex128 4 0) (Complex128 6 -2)])))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Complex128 tolerance suppresses small pivot",
        "(String (matrix/rank (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0.0000000000001)]) 0.000000000001))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Complex128 zero tolerance keeps small pivot",
        "(String (matrix/rank (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0.0000000000001)]) 0.0))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Complex64 full rectangular",
        "(String (matrix/rank (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 -1) (Complex64 0 0)])))",
        "2", pass, fail);
    test_str_val(interp, "matrix rank Complex64 deficient rectangular",
        "(String (matrix/rank (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 -1) (Complex64 2 2) (Complex64 4 0) (Complex64 6 -2)])))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Complex64 tolerance suppresses small pivot",
        "(String (matrix/rank (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0.0001)]) 0.001))",
        "1", pass, fail);
    test_str_val(interp, "matrix rank Complex64 zero tolerance keeps small pivot",
        "(String (matrix/rank (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0.0001)]) 0.0))",
        "2", pass, fail);
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix rank CPU fixed complex does not call Float64 LAPACK",
            "(and (= (matrix/rank (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)])) 2) (= (matrix/rank (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 -1) (Complex64 2 2) (Complex64 4 0) (Complex64 6 -2)])) 1))", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor fixed complex matrix rank CPU path does not call Float64 LAPACK");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor fixed complex matrix rank CPU path does not call Float64 LAPACK");
            *fail += 1;
        }
    }
    {
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_str_val(interp, "matrix rank Float32 does not call LAPACK",
            "(String (matrix/rank (Tensor Float32 [2 3] [1 0 0 0 1 0])))",
            "2", pass, fail);
        bool no_lapack_path_ok = omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal("[PASS] tensor Float32 matrix rank CPU path does not call LAPACK");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Float32 matrix rank CPU path does not call LAPACK");
            *fail += 1;
        }
    }
    test_truthy(interp, "matrix rank vulkan full rectangular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [2 3] [1 0 0 0 1 0]) 'vulkan)) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan deficient rectangular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [2 3] [1 2 3 2 4 6]) 'vulkan)) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (map + (to-device (Tensor Float64 [2 2] [1 0 0 1]) 'vulkan) 0.0)) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan tolerance suppresses small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [2 2] [1 0 0 0.0000000000001]) 'vulkan) 0.000000000001) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan zero tolerance keeps small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [2 2] [1 0 0 0.0000000000001]) 'vulkan) 0.0) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan empty when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [0 3] 0.0) 'vulkan)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan empty columns when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [3 0] 0.0) 'vulkan)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan rejects vector input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/rank (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 full rectangular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [2 3] [1 0 0 0 1 0]) 'vulkan)) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 deficient rectangular when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [2 3] [1 2 3 2 4 6]) 'vulkan)) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 realizes lazy input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (map + (to-device (Tensor Float32 [2 2] [1 0 0 1]) 'vulkan) 0.0)) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 tolerance suppresses small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [2 2] [1 0 0 0.0001]) 'vulkan) 0.001) 1) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 zero tolerance keeps small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [2 2] [1 0 0 0.0001]) 'vulkan) 0.0) 2) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 empty rows when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [0 3] 0.0) 'vulkan)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 empty columns when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [3 0] 0.0) 'vulkan)) 0) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Float32 rejects vector input when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (matrix/rank (to-device (Tensor Float32 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex128 full rank copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/rank (to-device (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)]) 'vulkan)) 2) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex128 deficient copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/rank (to-device (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 2 0) (Complex128 3 -1) (Complex128 2 2) (Complex128 4 0) (Complex128 6 -2)]) 'vulkan)) 1) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex128 tolerance suppresses small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/rank (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 0.0000000000001 0)]) 'vulkan) 0.000000000001) 1) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex128 zero tolerance keeps small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/rank (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 0.0000000000001 0)]) 'vulkan) 0.0) 2) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex64 full rank copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/rank (to-device (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 0 0) (Complex64 0 0) (Complex64 0 0) (Complex64 2 -1) (Complex64 0 0)]) 'vulkan)) 2) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex64 deficient copyback when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/rank (to-device (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 -1) (Complex64 2 2) (Complex64 4 0) (Complex64 6 -2)]) 'vulkan)) 1) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex64 tolerance suppresses small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/rank (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 0.0001 0)]) 'vulkan) 0.001) 1) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex64 zero tolerance keeps small pivot when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/rank (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 0.0001 0)]) 'vulkan) 0.0) 2) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex128 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex128) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (= (handle (matrix/rank (to-device (Tensor Complex128 [2 2] [(Complex128 1 0) (Complex128 0 0) (Complex128 0 0) (Complex128 1 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "matrix rank vulkan Complex64 fails closed when storage only",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'matrix-numerical-complex64) false)) (= (handle (matrix/rank (to-device (Tensor Complex64 [2 2] [(Complex64 1 0) (Complex64 0 0) (Complex64 0 0) (Complex64 1 0)]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 &&
            (omni_tensor_backend_vulkan_float64_available() != 0 || omni_tensor_backend_vulkan_float32_available() != 0);
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix rank vulkan fixed complex does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'matrix-numerical-complex128) (= (matrix/rank (to-device (Tensor Complex128 [2 3] [(Complex128 1 1) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0) (Complex128 2 -1) (Complex128 0 0)]) 'vulkan)) 2) true) (if (ref vulkan-backend 'matrix-numerical-complex64) (= (matrix/rank (to-device (Tensor Complex64 [2 3] [(Complex64 1 1) (Complex64 2 0) (Complex64 3 -1) (Complex64 2 2) (Complex64 4 0) (Complex64 6 -2)]) 'vulkan)) 1) true)))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan fixed complex matrix rank does not call LAPACK"
                : "[PASS] tensor Vulkan fixed complex matrix rank unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan fixed complex matrix rank does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix rank vulkan does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (matrix/rank (to-device (Tensor Float64 [2 3] [1 0 0 0 1 0]) 'vulkan)) 2) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix rank does not call LAPACK"
                : "[PASS] tensor Vulkan matrix rank unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix rank does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float32_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_truthy(interp, "matrix rank vulkan Float32 does not need LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (matrix/rank (to-device (Tensor Float32 [2 3] [1 0 0 0 1 0]) 'vulkan)) 2) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgesvd_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan Float32 matrix rank does not call LAPACK"
                : "[PASS] tensor Vulkan Float32 matrix rank unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan Float32 matrix rank does not call LAPACK");
            *fail += 1;
        }
    }
    {
        bool lapack_available = omni_tensor_backend_lapack_dgesvd_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgesvd_call_count();
        test_str_val(interp, "matrix rank LAPACK candidate",
            "(String (matrix/rank (Tensor Float64 [2 3] [1 0 0 0 1 0])))",
            "2", pass, fail);
        bool lapack_path_ok = !lapack_available || omni_tensor_backend_lapack_dgesvd_call_count() > lapack_calls_before;
        if (lapack_path_ok) {
            emit_pass_literal(lapack_available
                ? "[PASS] tensor LAPACK backend dgesvd matrix rank path"
                : "[PASS] tensor LAPACK backend dgesvd unavailable; matrix rank fallback retained");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd matrix rank path");
            *fail += 1;
        }
    }
    {
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(1);
        int fail_before = *fail;
        test_str_val(interp, "matrix rank forced pure fallback",
            "(String (matrix/rank (Tensor Float64 [2 3] [1 0 0 0 1 0])))",
            "2", pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgesvd_available() == 0;
        omni_tensor_backend_lapack_dgesvd_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgesvd disabled; matrix rank pure fallback path");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgesvd disabled; matrix rank pure fallback path");
            *fail += 1;
        }
    }
    test_error_contains(interp, "matrix rank rejects vector input",
        "(matrix/rank (Tensor Float64 [2] [1 2]))",
        "matrix/rank: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix rank rejects high precision dtype",
        "(matrix/rank (Tensor BigInteger [1 1] [1]))",
        "matrix/rank: expected a rank-2 Float64, Float32, Complex128, or Complex64 Tensor", pass, fail);
    test_error_contains(interp, "matrix rank rejects negative tolerance",
        "(matrix/rank (Tensor Float64 [1 1] [1]) -1.0)",
        "matrix/rank: tolerance must be a non-negative finite number", pass, fail);
    test_double(interp, "matrix norm default frobenius",
        "(matrix/norm (Tensor Float64 [2 2] [3 4 0 0]))",
        5.0, pass, fail);
    test_double(interp, "matrix norm explicit frobenius",
        "(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'frobenius)",
        9.539392014169456, pass, fail);
    test_double(interp, "matrix norm one",
        "(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'one)",
        9.0, pass, fail);
    test_double(interp, "matrix norm infinity",
        "(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'infinity)",
        15.0, pass, fail);
    test_double(interp, "matrix norm max",
        "(matrix/norm (Tensor Float64 [2 3] [1 -2 3 -4 5 -6]) 'max)",
        6.0, pass, fail);
    test_double(interp, "matrix norm Float32 default frobenius",
        "(matrix/norm (Tensor Float32 [2 2] [3 4 0 0]))",
        5.0, pass, fail);
    test_double(interp, "matrix norm Float32 explicit frobenius",
        "(matrix/norm (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'frobenius)",
        9.539392014169456, pass, fail);
    test_double(interp, "matrix norm Float32 one",
        "(matrix/norm (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'one)",
        9.0, pass, fail);
    test_double(interp, "matrix norm Float32 infinity",
        "(matrix/norm (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'infinity)",
        15.0, pass, fail);
    test_double(interp, "matrix norm Float32 max",
        "(matrix/norm (Tensor Float32 [2 3] [1 -2 3 -4 5 -6]) 'max)",
        6.0, pass, fail);
    test_double(interp, "matrix norm Float32 spectral",
        "(matrix/norm (Tensor Float32 [2 2] [3 0 0 2]) 'spectral)",
        3.0, pass, fail);
    test_double(interp, "matrix norm Float32 nuclear",
        "(matrix/norm (Tensor Float32 [2 2] [3 0 0 2]) 'nuclear)",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex128 default frobenius",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 3 4) (Complex128 0 0) (Complex128 0 0) (Complex128 0 0)]))",
        5.0, pass, fail);
    test_double(interp, "matrix norm Complex128 one",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'one)",
        7.0, pass, fail);
    test_double(interp, "matrix norm Complex128 infinity",
        "(matrix/norm (Tensor Complex128 [2 2] [(Complex128 1 1) (Complex128 2 0) (Complex128 0 0) (Complex128 3 4)]) 'infinity)",
        5.0, pass, fail);
}
