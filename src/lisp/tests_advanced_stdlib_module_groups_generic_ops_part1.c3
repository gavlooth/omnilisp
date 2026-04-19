module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part1(Interp* interp, int* pass, int* fail) {
    // === GENERIC OPERATIONS TESTS ===
    // ref - array
    test_eq(interp, "ref array", "(ref [10 20 30] 1)", 20, pass, fail);
    test_error(interp, "ref array oob", "(ref [1] 5)", pass, fail);
    // ref - dict
    test_eq(interp, "ref dict", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_nil(interp, "ref dict missing", "(ref {'a 1} 'b)", pass, fail);
    // ref - cons
    test_eq(interp, "ref cons car", "(ref (cons 10 20) 0)", 10, pass, fail);
    test_eq(interp, "ref cons cdr", "(ref (cons 10 20) 1)", 20, pass, fail);
    test_eq(interp, "ref cons negative cdr", "(ref (cons 10 20) -1)", 20, pass, fail);
    // ref - string
    test_str_val(interp, "ref string", "(ref \"hello\" 0)", "h", pass, fail);
    // negative indexing (Python-style: -1 = last, -2 = second-to-last)
    test_eq(interp, "ref array -1", "(ref [10 20 30] -1)", 30, pass, fail);
    test_eq(interp, "ref array -2", "(ref [10 20 30] -2)", 20, pass, fail);
    test_eq(interp, "ref array -3", "(ref [10 20 30] -3)", 10, pass, fail);
    test_error(interp, "ref array -4 oob", "(ref [10 20 30] -4)", pass, fail);
    test_str_val(interp, "ref string -1", "(ref \"hello\" -1)", "o", pass, fail);
    test_str_val(interp, "ref string -5", "(ref \"hello\" -5)", "h", pass, fail);
    test_error(interp, "ref string -6 oob", "(ref \"hello\" -6)", pass, fail);
    // ref - tensor
    test_eq_double(interp, "ref tensor rank-0 scalar",
        "(ref (Tensor Float64 [] 3.0) [])",
        3.0, pass, fail);
    test_error_contains(interp, "ref tensor rank-0 scalar with vector index",
        "(ref (Tensor Float64 [] 3.0) [0])",
        "ref: tensor index rank mismatch", pass, fail);
    test_error_contains(interp, "ref tensor rank-0 scalar with scalar index",
        "(ref (Tensor Float64 [] 3.0) 0)",
        "ref: tensor requires array or proper list index", pass, fail);
    test_eq_double(interp, "ref tensor rank-2", "(ref (Tensor Float64 [2 3] [1 2 3 4 5 6]) [1 2])", 6.0, pass, fail);
    test_eq_double(interp, "ref tensor negative axis", "(ref (Tensor Float64 [2 3] [1 2 3 4 5 6]) [1 -1])", 6.0, pass, fail);
    test_eq_double(interp, "ref tensor list index", "(ref (Tensor Float64 '(2 3) '(1 2 3 4 5 6)) '(1 2))", 6.0, pass, fail);
    test_eq_double(interp, "Tensor inferred vector ref",
        "(ref (Tensor [1 2 3]) [2])", 3.0, pass, fail);
    test_str_val(interp, "Tensor inferred matrix shape",
        "(format \"%s\" (shape (Tensor [[1 2] [3 4]])))", "[2 2]", pass, fail);
    test_eq_double(interp, "Tensor inferred matrix row-major ref",
        "(ref (Tensor [[1 2] [3 4]]) [1 0])", 3.0, pass, fail);
    test_eq_double(interp, "Tensor inferred scalar rank-0",
        "(ref (Tensor 7) [])", 7.0, pass, fail);
    test_eq_double(interp, "Tensor inferred data dtype suffix",
        "(ref (Tensor [1 2] Float64) [1])", 2.0, pass, fail);
    test_eq_double(interp, "Tensor inferred data dtype prefix",
        "(ref (Tensor Float64 [1 2]) [1])", 2.0, pass, fail);
    test_eq_double(interp, "Tensor inferred quoted dtype suffix",
        "(ref (Tensor [1 2] 'Float64) [1])", 2.0, pass, fail);
    test_str_val(interp, "Tensor Float32 explicit dtype",
        "(String (dtype (Tensor Float32 [2] [1.25 2.5])))", "Float32", pass, fail);
    test_truthy(interp, "Tensor Float32 ref returns Float32 scalar",
        "(let (x (ref (Tensor Float32 [2] [1.25 2.5]) [1])) (and (= x 2.5) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "Tensor Float32 dtype suffix ref returns Float32 scalar",
        "(let (x (ref (Tensor [1.25 2.5] Float32) [0])) (and (= x 1.25) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "Tensor Float32 dtype prefix inferred shape ref returns Float32 scalar",
        "(let (x (ref (Tensor Float32 [1.25 2.5]) [1])) (and (= x 2.5) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "Tensor Float32 scalar fill returns Float32 scalar",
        "(let (x (ref (Tensor Float32 [2] 2.5) [1])) (and (= x 2.5) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "Array from Tensor Float32 preserves Float32 elements",
        "(let (x (ref (Array (Tensor Float32 [2] [1.25 2.5])) 1)) (and (= x 2.5) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "List from Tensor Float32 preserves Float32 elements",
        "(let (x (ref (List (Tensor Float32 [2] [1.25 2.5])) 0)) (and (= x 1.25) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "Iterator from Tensor Float32 preserves Float32 elements",
        "(let (x (ref (Array (Iterator (Tensor Float32 [2] [1.25 2.5]))) 1)) (and (= x 2.5) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "to-device cpu preserves Tensor Float32 scalar extraction",
        "(let (x (ref (to-device (Tensor Float32 [2] [1.25 2.5]) 'cpu) [0])) (and (= x 1.25) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "realize Tensor Float32 into destination returns Float32 scalar",
        "(let (dst (Tensor Float32 [2] 0.0)) (block (realize (Tensor Float32 [2] [1.25 2.5]) dst) (let (x (ref dst [1])) (and (= x 2.5) (= (type-of x) 'Float32)))))", pass, fail);
    test_truthy(interp, "map Tensor Float32 scalar preserves CPU oracle and scalar extraction",
        "(let (x (ref (map + (Tensor Float32 [2] [1.25 2.5]) 0.5) [1])) (and (= x 3.0) (= (type-of x) 'Float32)))", pass, fail);
    test_str_val(interp, "map Tensor Float32 preserves dtype",
        "(String (dtype (map + (Tensor Float32 [2] [1.25 2.5]) 0.5)))", "Float32", pass, fail);
    test_truthy(interp, "contract Tensor Float32 dot returns Float32 scalar",
        "(let (x (ref (contract (Tensor Float32 [2] [1 2]) (Tensor Float32 [2] [10 20]) [0 0]) [])) (and (= x 50.0) (= (type-of x) 'Float32)))", pass, fail);
    test_truthy(interp, "contract Tensor Float32 zero-size identity returns Float32 scalar",
        "(let (x (ref (contract (Tensor Float32 [0] 0.0) (Tensor Float32 [0] 0.0) [0 0]) [])) (and (= x 0.0) (= (type-of x) 'Float32)))", pass, fail);
    test_error_contains(interp, "Tensor Float32 rejects out-of-range input",
        "(Tensor Float32 [1] [(BigFloat \"1e39\")])",
        "representable as Float32", pass, fail);
    test_error_contains(interp, "map Tensor Float32 rejects mixed Tensor dtype",
        "(map + (Tensor Float32 [1] [1.25]) (Tensor Float64 [1] [2.0]))",
        "map: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "Tensor default device is cpu",
        "(String (device (Tensor Float64 [1] [2])))", "cpu", pass, fail);
    test_truthy(interp, "tensor-layout reports CPU dense row-major metadata",
        "(let (layout (tensor-layout (Tensor Float64 [2 3] [1 2 3 4 5 6]))) (and (= (ref layout 'dtype) 'Float64) (= (ref layout 'device) 'cpu) (= (ref layout 'payload) 'concrete) (= (ref layout 'layout) 'dense-row-major) (= (ref layout 'dense-row-major) true) (= (ref (ref layout 'shape) 0) 2) (= (ref (ref layout 'shape) 1) 3) (= (ref (ref layout 'strides) 0) 3) (= (ref (ref layout 'strides) 1) 1) (= (ref layout 'storage-offset) 0) (= (ref layout 'element-count) 6) (= (ref layout 'storage-elements) 6) (= (ref layout 'byte-length) 48) (= (ref layout 'storage-bytes) 48) (= (ref layout 'is-view) false) (= (ref layout 'owns-storage) true) (= (ref layout 'owner) 'self) (= (ref layout 'write-policy) 'mutable)))", pass, fail);
    test_truthy(interp, "tensor-layout reports rank-0 dense metadata",
        "(let (layout (tensor-layout (Tensor Float64 [] 3.0))) (and (= (length (ref layout 'shape)) 0) (= (length (ref layout 'strides)) 0) (= (ref layout 'rank) 0) (= (ref layout 'element-count) 1) (= (ref layout 'storage-elements) 1) (= (ref layout 'storage-offset) 0) (= (ref layout 'layout) 'dense-row-major)))", pass, fail);
    test_truthy(interp, "tensor-layout reports zero-size row-major strides and backing size",
        "(let (layout (tensor-layout (Tensor Float64 [0 3] 0.0))) (and (= (ref (ref layout 'strides) 0) 3) (= (ref (ref layout 'strides) 1) 1) (= (ref layout 'element-count) 0) (= (ref layout 'storage-elements) 0) (= (ref layout 'byte-length) 0) (= (ref layout 'storage-bytes) 0) (= (ref layout 'dense-row-major) true)))", pass, fail);
    test_truthy(interp, "tensor-layout reports lazy expression metadata without storage ownership",
        "(let (layout (tensor-layout (map + (Tensor Float64 [2] [1 2]) 1.0))) (and (= (ref layout 'payload) 'map) (= (ref layout 'layout) 'dense-row-major) (= (ref (ref layout 'strides) 0) 1) (= (ref layout 'storage-offset) 0) (= (ref layout 'element-count) 2) (= (ref layout 'storage-elements) 0) (= (ref layout 'storage-bytes) 0) (= (ref layout 'owns-storage) false) (= (ref layout 'owner) 'expression) (= (ref layout 'write-policy) 'immutable)))", pass, fail);
    test_truthy(interp, "tensor-layout reports CUDA copied dense metadata when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (layout (tensor-layout (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda))) (and (= (ref layout 'device) 'cuda) (= (ref layout 'payload) 'concrete) (= (ref layout 'layout) 'dense-row-major) (= (ref (ref layout 'strides) 0) 3) (= (ref (ref layout 'strides) 1) 1) (= (ref layout 'storage-offset) 0) (= (ref layout 'storage-elements) 6) (= (ref layout 'owns-storage) true))) true))", pass, fail);
    test_truthy(interp, "tensor-layout reports CUDA Complex128 copied dense metadata when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (let (layout (tensor-layout (to-device (Tensor Complex128 [2 2] [(Complex128 1 2) (Complex128 3 4) (Complex128 5 6) (Complex128 7 8)]) 'cuda))) (and (= (ref layout 'device) 'cuda) (= (ref layout 'dtype) 'Complex128) (= (ref layout 'payload) 'concrete) (= (ref layout 'layout) 'dense-row-major) (= (ref layout 'element-count) 4) (= (ref layout 'storage-elements) 4) (= (ref layout 'byte-length) 64) (= (ref layout 'storage-bytes) 64))) true))", pass, fail);
    test_truthy(interp, "tensor-layout reports Vulkan copied dense metadata when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (layout (tensor-layout (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (ref layout 'device) 'vulkan) (= (ref layout 'payload) 'concrete) (= (ref layout 'layout) 'dense-row-major) (= (ref (ref layout 'strides) 0) 3) (= (ref (ref layout 'strides) 1) 1) (= (ref layout 'storage-offset) 0) (= (ref layout 'storage-elements) 6) (= (ref layout 'owns-storage) true))) true) true))", pass, fail);
    test_truthy(interp, "tensor-layout reports Vulkan Complex64 copied dense metadata when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (let (layout (tensor-layout (to-device (Tensor Complex64 [2 2] [(Complex64 1 2) (Complex64 3 4) (Complex64 5 6) (Complex64 7 8)]) 'vulkan))) (and (= (ref layout 'device) 'vulkan) (= (ref layout 'dtype) 'Complex64) (= (ref layout 'payload) 'concrete) (= (ref layout 'layout) 'dense-row-major) (= (ref layout 'element-count) 4) (= (ref layout 'storage-elements) 4) (= (ref layout 'byte-length) 32) (= (ref layout 'storage-bytes) 32))) true))", pass, fail);
    test_truthy(interp, "matrix/transpose-view reports read-only strided view metadata",
        "(let (view (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) layout (tensor-layout view)) (and (= (ref layout 'payload) 'view) (= (ref layout 'layout) 'strided) (= (ref layout 'dense-row-major) false) (= (ref (ref layout 'shape) 0) 3) (= (ref (ref layout 'shape) 1) 2) (= (ref (ref layout 'strides) 0) 1) (= (ref (ref layout 'strides) 1) 3) (= (ref layout 'storage-offset) 0) (= (ref layout 'storage-elements) 6) (= (ref layout 'storage-bytes) 48) (= (ref layout 'is-view) true) (= (ref layout 'owns-storage) false) (= (ref layout 'owner) 'view-source) (= (ref layout 'write-policy) 'read-only-view)))", pass, fail);
    test_truthy(interp, "matrix/transpose-view reports Vulkan read-only view metadata when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (view (matrix/transpose-view (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) layout (tensor-layout view)) (and (= (ref layout 'device) 'vulkan) (= (ref layout 'payload) 'view) (= (ref layout 'layout) 'strided) (= (ref layout 'dense-row-major) false) (= (ref (ref layout 'shape) 0) 3) (= (ref (ref layout 'shape) 1) 2) (= (ref (ref layout 'strides) 0) 1) (= (ref (ref layout 'strides) 1) 3) (= (ref layout 'is-view) true) (= (ref layout 'owns-storage) false) (= (ref layout 'write-policy) 'read-only-view))) true))", pass, fail);
    test_truthy(interp, "matrix/transpose-view supports CPU ref and collection conversion order",
        "(let (view (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) arr (Array view) lst (List view)) (and (= (ref view [2 1]) 6.0) (= (ref view [-1 -1]) 6.0) (= (ref arr 0) 1.0) (= (ref arr 1) 4.0) (= (ref arr 2) 2.0) (= (ref arr 5) 6.0) (= (ref lst 0) 1.0) (= (ref lst 1) 4.0) (= (ref lst 5) 6.0)))", pass, fail);
    test_truthy(interp, "matrix/transpose-view survives return and closure capture",
        "(let (make-view (lambda () (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6]))) view (make-view) getter (let (captured view) (lambda () (ref captured [2 1])))) (and (= (ref view [2 1]) 6.0) (= (getter) 6.0)))", pass, fail);
    test_truthy(interp, "realize materializes transpose view into dense tensor",
        "(let (realized (realize (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6]))) layout (tensor-layout realized)) (and (= (ref realized [2 1]) 6.0) (= (ref layout 'payload) 'concrete) (= (ref layout 'dense-row-major) true) (= (ref (ref layout 'strides) 0) 2) (= (ref (ref layout 'strides) 1) 1)))", pass, fail);
    test_truthy(interp, "realize materializes Vulkan transpose view into Vulkan dense tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (realized (realize (matrix/transpose-view (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) layout (tensor-layout realized) cpu (to-device realized 'cpu)) (and (= (device realized) 'vulkan) (= (ref cpu [2 1]) 6.0) (= (ref layout 'payload) 'concrete) (= (ref layout 'dense-row-major) true) (= (ref (ref layout 'strides) 0) 2) (= (ref (ref layout 'strides) 1) 1))) true))", pass, fail);
    test_truthy(interp, "to-device cpu copyback materializes Vulkan transpose view when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (cpu (to-device (matrix/transpose-view (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) 'cpu)) (and (= (device cpu) 'cpu) (= (ref cpu [0 1]) 4.0) (= (ref cpu [2 1]) 6.0))) true))", pass, fail);
    test_error_contains(interp, "realize into transpose view fails read-only",
        "(realize (Tensor Float64 [3 2] 0.0) (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])))",
        "realize: destination Tensor is immutable", pass, fail);
    test_truthy(interp, "matrix/transpose composes transpose views structurally",
        "(let (view (matrix/transpose (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6]))) layout (tensor-layout view)) (and (= (ref view [1 2]) 6.0) (= (ref layout 'payload) 'view) (= (ref layout 'dense-row-major) true) (= (ref layout 'is-view) true)))", pass, fail);
    test_truthy(interp, "to-device rejects strided and dense transpose views for CUDA copy",
        "(and (= (handle (to-device (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (to-device (matrix/transpose (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6]))) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unsupported))", pass, fail);
    test_truthy(interp, "to-device Vulkan materializes CPU transpose views when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (copied (to-device (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) 'vulkan) dense-copied (to-device (matrix/transpose (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6]))) 'vulkan)) (and (= (device copied) 'vulkan) (= (ref (to-device copied 'cpu) [2 1]) 6.0) (= (device dense-copied) 'vulkan) (= (ref (to-device dense-copied 'cpu) [1 2]) 6.0))) true))", pass, fail);
    test_truthy(interp, "realize into CUDA destination rejects transpose view source when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (let (dst (to-device (Tensor Float64 [3 2] 0.0) 'cuda)) (realize (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize into Vulkan destination materializes transpose view source when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (dst (to-device (Tensor Float64 [3 2] 0.0) 'vulkan)) (block (realize (matrix/transpose-view (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) dst) (and (= (device dst) 'vulkan) (= (ref (to-device dst 'cpu) [2 1]) 6.0)))) true))", pass, fail);
    test_truthy(interp, "matrix/transpose-view keeps lazy materialized source alive",
        "(let (make-view (lambda () (matrix/transpose-view (map + (Tensor Float64 [2 3] [1 2 3 4 5 6]) 0.0))) view (make-view) getter (let (captured view) (lambda () (ref captured [2 1])))) (and (= (ref view [2 1]) 6.0) (= (getter) 6.0)))", pass, fail);
    test_eq(interp, "tensor-backends reports cpu cuda cublas and vulkan entries",
        "(length (tensor-backends))", 4, pass, fail);
    test_truthy(interp, "tensor-backends cpu available",
        "(= (ref (ref (tensor-backends) 0) 'available) true)", pass, fail);
    test_truthy(interp, "tensor-backends cpu structural complex matrix capability",
        "(let (cpu-backend (ref (tensor-backends) 0)) (and (= (ref cpu-backend 'matrix-structural-complex128) true) (= (ref cpu-backend 'matrix-structural-complex64) true) (= (ref cpu-backend 'matrix-numerical-complex128) true) (= (ref cpu-backend 'matrix-numerical-complex64) true)))", pass, fail);
    test_truthy(interp, "tensor-backends cuda unavailable reason",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) true (= (ref cuda-backend 'reason) 'backend-unavailable)))", pass, fail);
    test_truthy(interp, "tensor-backends cuda Float32 capability entry",
        "(let (cuda-backend (ref (tensor-backends) 1)) (and (= (ref cuda-backend 'name) 'cuda) (or (= (ref cuda-backend 'float64) true) (= (ref cuda-backend 'float64) false)) (or (= (ref cuda-backend 'float32) true) (= (ref cuda-backend 'float32) false)) (or (= (ref cuda-backend 'complex128) true) (= (ref cuda-backend 'complex128) false)) (or (= (ref cuda-backend 'complex64) true) (= (ref cuda-backend 'complex64) false)) (or (= (ref cuda-backend 'elementwise-map-float64) true) (= (ref cuda-backend 'elementwise-map-float64) false)) (or (= (ref cuda-backend 'elementwise-map-float32) true) (= (ref cuda-backend 'elementwise-map-float32) false)) (or (= (ref cuda-backend 'elementwise-map-complex128) true) (= (ref cuda-backend 'elementwise-map-complex128) false)) (or (= (ref cuda-backend 'elementwise-map-complex64) true) (= (ref cuda-backend 'elementwise-map-complex64) false)) (or (= (ref cuda-backend 'contract-complex128) true) (= (ref cuda-backend 'contract-complex128) false)) (or (= (ref cuda-backend 'contract-complex64) true) (= (ref cuda-backend 'contract-complex64) false)) (or (= (ref cuda-backend 'matrix-structural-complex128) true) (= (ref cuda-backend 'matrix-structural-complex128) false)) (or (= (ref cuda-backend 'matrix-structural-complex64) true) (= (ref cuda-backend 'matrix-structural-complex64) false)) (or (= (ref cuda-backend 'scientific-map-float64) true) (= (ref cuda-backend 'scientific-map-float64) false)) (or (= (ref cuda-backend 'scientific-map-float32) true) (= (ref cuda-backend 'scientific-map-float32) false)) (or (= (ref cuda-backend 'rounding-big-integer) true) (= (ref cuda-backend 'rounding-big-integer) false))))", pass, fail);
    test_truthy(interp, "tensor-backends cublas Float32 capability entry",
        "(let (cublas-backend (ref (tensor-backends) 2)) (and (= (ref cublas-backend 'name) 'cublas) (or (= (ref cublas-backend 'float64) true) (= (ref cublas-backend 'float64) false)) (or (= (ref cublas-backend 'float32) true) (= (ref cublas-backend 'float32) false))))", pass, fail);
    test_truthy(interp, "tensor-backends vulkan capability entry",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (ref vulkan-backend 'name) 'vulkan) (or (= (ref vulkan-backend 'float64) true) (= (ref vulkan-backend 'float64) false))))", pass, fail);
    test_truthy(interp, "tensor-backends vulkan Float32 capability entry",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (ref vulkan-backend 'name) 'vulkan) (or (= (ref vulkan-backend 'float32) true) (= (ref vulkan-backend 'float32) false)) (or (= (ref vulkan-backend 'complex128) true) (= (ref vulkan-backend 'complex128) false)) (or (= (ref vulkan-backend 'complex64) true) (= (ref vulkan-backend 'complex64) false)) (or (= (ref vulkan-backend 'elementwise-map-complex128) true) (= (ref vulkan-backend 'elementwise-map-complex128) false)) (or (= (ref vulkan-backend 'elementwise-map-complex64) true) (= (ref vulkan-backend 'elementwise-map-complex64) false)) (or (= (ref vulkan-backend 'contract-complex128) true) (= (ref vulkan-backend 'contract-complex128) false)) (or (= (ref vulkan-backend 'contract-complex64) true) (= (ref vulkan-backend 'contract-complex64) false)) (or (= (ref vulkan-backend 'matrix-structural-complex128) true) (= (ref vulkan-backend 'matrix-structural-complex128) false)) (or (= (ref vulkan-backend 'matrix-structural-complex64) true) (= (ref vulkan-backend 'matrix-structural-complex64) false)) (or (= (ref vulkan-backend 'matrix-numerical-complex128) true) (= (ref vulkan-backend 'matrix-numerical-complex128) false)) (or (= (ref vulkan-backend 'matrix-numerical-complex64) true) (= (ref vulkan-backend 'matrix-numerical-complex64) false))))", pass, fail);
    test_truthy(interp, "tensor-backends vulkan rounding capability is absent or bool",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (ref vulkan-backend 'name) 'vulkan) (or (= (ref vulkan-backend 'rounding-big-integer) nil) (or (= (ref vulkan-backend 'rounding-big-integer) true) (= (ref vulkan-backend 'rounding-big-integer) false)))))", pass, fail);
    test_eq_double(interp, "to-device cpu realizes lazy tensor",
        "(ref (to-device (map + (Tensor Float64 [1] [2]) 1.0) 'cpu) [0])", 3.0, pass, fail);
    test_truthy(interp, "to-device cuda follows backend availability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (device (to-device (Tensor Float64 [1] [2]) 'cuda)) 'cuda) (= (handle (to-device (Tensor Float64 [1] [2]) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device cuda Float32 follows backend availability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (device (to-device (Tensor Float32 [1] [2]) 'cuda)) 'cuda) (= (handle (to-device (Tensor Float32 [1] [2]) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device cuda Complex128 roundtrips storage when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (let (cpu (to-device (to-device (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 -3 4)]) 'cuda) 'cpu)) (and (= (device cpu) 'cpu) (= (String (dtype cpu)) \"Complex128\") (= (String (ref cpu [0])) \"1.0+2.0i\") (= (String (ref cpu [1])) \"-3.0+4.0i\"))) (= (handle (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device cuda Complex64 roundtrips storage when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex64) (let (cpu (to-device (to-device (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 -3 4)]) 'cuda) 'cpu)) (and (= (device cpu) 'cpu) (= (String (dtype cpu)) \"Complex64\") (= (String (ref cpu [0])) \"1.0+2.0i\") (= (String (ref cpu [1])) \"-3.0+4.0i\"))) (= (handle (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device vulkan Complex128 roundtrips storage when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex128) (let (cpu (to-device (to-device (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 -3 4)]) 'vulkan) 'cpu)) (and (= (device cpu) 'cpu) (= (String (dtype cpu)) \"Complex128\") (= (String (ref cpu [0])) \"1.0+2.0i\") (= (String (ref cpu [1])) \"-3.0+4.0i\"))) (= (handle (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device vulkan Complex64 roundtrips storage when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (let (cpu (to-device (to-device (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 -3 4)]) 'vulkan) 'cpu)) (and (= (device cpu) 'cpu) (= (String (dtype cpu)) \"Complex64\") (= (String (ref cpu [0])) \"1.0+2.0i\") (= (String (ref cpu [1])) \"-3.0+4.0i\"))) (= (handle (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 tensor scalar add roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (out (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (Complex128 3 4))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex128\") (= (String (ref cpu [0])) \"4.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "map vulkan Complex64 tensor scalar add roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex64) (let (out (map + (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan) (Complex64 3 4))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex64\") (= (String (ref cpu [0])) \"4.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 scalar tensor subtract roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (String (ref (to-device (map - (Complex128 10 1) (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan)) 'cpu) [0])) \"9.0-1.0i\") true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 tensor tensor multiply roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (String (ref (to-device (map * (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (to-device (Tensor Complex128 [1] [(Complex128 3 -1)]) 'vulkan)) 'cpu) [0])) \"5.0+5.0i\") true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 broadcast add roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (out (map + (to-device (Tensor Complex128 [2 1] [(Complex128 1 1) (Complex128 2 2)]) 'vulkan) (to-device (Tensor Complex128 [1 2] [(Complex128 10 0) (Complex128 0 10)]) 'vulkan))) (= (String (ref (to-device out 'cpu) [1 1])) \"2.0+12.0i\")) true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 conjugate stays complex when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (out (map conjugate (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [0])) \"1.0-2.0i\"))) true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 real-part keeps map dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (out (map real-part (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [0])) \"1.0+0.0i\"))) true))", pass, fail);
    test_truthy(interp, "real-part vulkan Complex128 returns Float64 tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (out (real-part (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [0]) 1.0))) true))", pass, fail);
    test_truthy(interp, "imag-part vulkan Complex64 returns Float32 tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex64) (let (out (imag-part (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 2.0)))) true))", pass, fail);
    test_truthy(interp, "abs vulkan Complex128 returns Float64 tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (ref (to-device (abs (to-device (Tensor Complex128 [1] [(Complex128 3 4)]) 'vulkan)) 'cpu) [0]) 5.0) true))", pass, fail);
    test_truthy(interp, "unary minus vulkan Complex128 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (String (ref (to-device (- (to-device (Tensor Complex128 [1] [(Complex128 1 -2)]) 'vulkan)) 'cpu) [0])) \"-1.0+2.0i\") true))", pass, fail);
    test_truthy(interp, "conjugate vulkan Complex64 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex64) (= (String (ref (to-device (conjugate (to-device (Tensor Complex64 [1] [(Complex64 1 -2)]) 'vulkan)) 'cpu) [0])) \"1.0+2.0i\") true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 division by zero fails without CPU fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (handle (map / (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (Complex128 0 0)) (raise msg (ref msg 'code))) 'tensor/domain-error) true))", pass, fail);
    test_truthy(interp, "map vulkan Complex128 min fails closed",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (handle (map min (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (Complex128 3 4)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy vulkan Complex128 map into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (let (dst (to-device (Tensor Complex128 [1] [(Complex128 0 0)]) 'vulkan)) (block (realize (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (Complex128 3 4)) dst) (and (= (device dst) 'vulkan) (= (String (ref (to-device dst 'cpu) [0])) \"4.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "realize lazy vulkan Complex128 map into cpu destination fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'elementwise-map-complex128) (= (handle (let (dst (Tensor Complex128 [1] 0)) (realize (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (Complex128 3 4)) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize cpu tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (Tensor Float64 [1] [2.0]) dst) (= (ref (to-device dst 'cpu) [0]) 2.0))) true))", pass, fail);
    test_truthy(interp, "realize Float32 cpu tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float32 [1] [0.0]) 'cuda)) (block (realize (Tensor Float32 [1] [2.5]) dst) (= (ref (to-device dst 'cpu) [0]) 2.5))) true))", pass, fail);
    test_truthy(interp, "realize cuda tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (to-device (Tensor Float64 [1] [3.0]) 'cuda) dst) (and (= (device dst) 'cuda) (= (ref (to-device dst 'cpu) [0]) 3.0)))) true))", pass, fail);
    test_truthy(interp, "realize Float32 cuda tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float32 [1] [0.0]) 'cuda)) (block (realize (to-device (Tensor Float32 [1] [3.5]) 'cuda) dst) (and (= (device dst) 'cuda) (= (String (dtype dst)) \"Float32\") (= (ref (to-device dst 'cpu) [0]) 3.5)))) true))", pass, fail);
    test_truthy(interp, "realize lazy cpu tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map + (Tensor Float64 [1] [4.0]) 0.0) dst) (= (ref (to-device dst 'cpu) [0]) 4.0))) true))", pass, fail);
    test_truthy(interp, "realize lazy Float32 cpu tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float32 [1] [0.0]) 'cuda)) (block (realize (map + (Tensor Float32 [1] [4.0]) 0.5) dst) (= (ref (to-device dst 'cpu) [0]) 4.5))) true))", pass, fail);
    test_truthy(interp, "realize scalar fill into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float64 [2] 0.0) 'cuda)) (block (realize 1.0 dst) (= (ref (to-device dst 'cpu) [1]) 1.0))) true))", pass, fail);
    test_truthy(interp, "realize Float32 scalar fill into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (dst (to-device (Tensor Float32 [2] 0.0) 'cuda)) (block (realize 1.25 dst) (= (ref (to-device dst 'cpu) [1]) 1.25))) true))", pass, fail);
    test_truthy(interp, "realize Complex128 cpu tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (let (dst (to-device (Tensor Complex128 [1] [(Complex128 0 0)]) 'cuda)) (block (realize (Tensor Complex128 [1] [(Complex128 5 -6)]) dst) (and (= (device dst) 'cuda) (= (String (ref (to-device dst 'cpu) [0])) \"5.0-6.0i\")))) true))", pass, fail);
    test_truthy(interp, "realize Complex128 cuda tensor into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (let (dst (to-device (Tensor Complex128 [1] [(Complex128 0 0)]) 'cuda)) (block (realize (to-device (Tensor Complex128 [1] [(Complex128 -5 6)]) 'cuda) dst) (and (= (device dst) 'cuda) (= (String (ref (to-device dst 'cpu) [0])) \"-5.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "realize Complex64 scalar fill into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex64) (let (dst (to-device (Tensor Complex64 [2] 0) 'cuda)) (block (realize (Complex64 7 -8) dst) (= (String (ref (to-device dst 'cpu) [1])) \"7.0-8.0i\"))) true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 tensor scalar add roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 3 4))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex128\") (= (String (ref cpu [0])) \"4.0+6.0i\")))) (if (ref cuda-backend 'complex128) (= (handle (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 3 4)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda Complex64 tensor scalar add roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex64) (let (out (map + (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'cuda) (Complex64 3 4))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex64\") (= (String (ref cpu [0])) \"4.0+6.0i\")))) (if (ref cuda-backend 'complex64) (= (handle (map + (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'cuda) (Complex64 3 4)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda Complex128 scalar tensor subtract roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (String (ref (to-device (map - (Complex128 10 1) (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda)) 'cpu) [0])) \"9.0-1.0i\") true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 tensor tensor multiply roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (String (ref (to-device (map * (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (to-device (Tensor Complex128 [1] [(Complex128 3 -1)]) 'cuda)) 'cpu) [0])) \"5.0+5.0i\") true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 broadcast add roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (map + (to-device (Tensor Complex128 [2 1] [(Complex128 1 1) (Complex128 2 2)]) 'cuda) (to-device (Tensor Complex128 [1 2] [(Complex128 10 0) (Complex128 0 10)]) 'cuda))) (= (String (ref (to-device out 'cpu) [1 1])) \"2.0+12.0i\")) true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 conjugate stays complex when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (map conjugate (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [0])) \"1.0-2.0i\"))) true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 real-part keeps map dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (map real-part (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [0])) \"1.0+0.0i\"))) true))", pass, fail);
    test_truthy(interp, "real-part cuda Complex128 returns Float64 tensor when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (real-part (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [0]) 1.0))) true))", pass, fail);
    test_truthy(interp, "real-part cuda Complex128 fails closed when complex map unsupported",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (and (ref cuda-backend 'complex128) (= (ref cuda-backend 'elementwise-map-complex128) false)) (= (handle (real-part (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "imag-part cuda Complex64 returns Float32 tensor when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex64) (let (out (imag-part (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 2.0)))) true))", pass, fail);
    test_truthy(interp, "abs cuda Complex128 returns Float64 tensor when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (ref (to-device (abs (to-device (Tensor Complex128 [1] [(Complex128 3 4)]) 'cuda)) 'cpu) [0]) 5.0) true))", pass, fail);
    test_truthy(interp, "unary minus cuda Complex128 roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (String (ref (to-device (- (to-device (Tensor Complex128 [1] [(Complex128 1 -2)]) 'cuda)) 'cpu) [0])) \"-1.0+2.0i\") true))", pass, fail);
    test_truthy(interp, "conjugate cuda Complex64 roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex64) (= (String (ref (to-device (conjugate (to-device (Tensor Complex64 [1] [(Complex64 1 -2)]) 'cuda)) 'cpu) [0])) \"1.0+2.0i\") true))", pass, fail);
    test_truthy(interp, "unary minus cuda empty Complex128 preserves empty output when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (- (to-device (Tensor Complex128 [0] []) 'cuda))) (and (= (device out) 'cuda) (= (length (to-device out 'cpu)) 0))) true))", pass, fail);
    test_truthy(interp, "real-part cuda empty Complex128 returns empty Float64 tensor when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (out (real-part (to-device (Tensor Complex128 [0] []) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float64\") (= (length (to-device out 'cpu)) 0))) true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 division by zero fails without CPU fallback",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (handle (map / (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 0 0)) (raise msg (ref msg 'code))) 'tensor/domain-error) true))", pass, fail);
    test_truthy(interp, "map cuda Complex128 min fails closed",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (handle (map min (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 3 4)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda Complex128 map into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (let (dst (to-device (Tensor Complex128 [1] [(Complex128 0 0)]) 'cuda)) (block (realize (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 3 4)) dst) (and (= (device dst) 'cuda) (= (String (ref (to-device dst 'cpu) [0])) \"4.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda Complex128 map into cpu destination fails closed when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-complex128) (= (handle (let (dst (Tensor Complex128 [1] 0)) (realize (map + (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (Complex128 3 4)) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize cuda tensor into cpu destination fails closed when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (let (dst (Tensor Float64 [1] 0.0)) (realize (to-device (Tensor Float64 [1] [2.0]) 'cuda) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize cuda Complex128 tensor into cpu destination fails closed when storage is available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (= (handle (let (dst (Tensor Complex128 [1] 0)) (realize (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda contract into cuda destination when cublas available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (let (dst (to-device (Tensor Float64 [2 2] 0.0) 'cuda)) (block (realize (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) dst) (= (ref (to-device dst 'cpu) [1 1]) 154.0))) true))", pass, fail);
    test_truthy(interp, "realize lazy Float32 cuda contract into cuda destination when cublas Float32 available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (dst (to-device (Tensor Float32 [2 2] 0.0) 'cuda)) (block (realize (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) dst) (and (= (String (dtype dst)) \"Float32\") (= (ref (to-device dst 'cpu) [1 1]) 154.0)))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda rank-1 dot into rank-0 cuda destination",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (let (dst (to-device (Tensor Float64 [] 0.0) 'cuda)) (block (realize (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0 0]) dst) (= (ref (to-device dst 'cpu) []) 140.0))) true))", pass, fail);
    test_truthy(interp, "realize lazy Float32 cuda rank-1 dot into rank-0 cuda destination",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (dst (to-device (Tensor Float32 [] 0.0) 'cuda)) (block (realize (contract (to-device (Tensor Float32 [3] [1 2 3]) 'cuda) (to-device (Tensor Float32 [3] [10 20 30]) 'cuda) [0 0]) dst) (let (cpu (to-device dst 'cpu)) (and (= (String (dtype dst)) \"Float32\") (= (String (type-of (ref cpu []))) \"Float32\") (= (ref cpu []) (Float32 140.0)))))) true))", pass, fail);
    test_truthy(interp, "contract cuda Complex128 rank-1 dot roundtrips when complex contract available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'contract-complex128) (let (out (contract (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (to-device (Tensor Complex128 [1] [(Complex128 3 4)]) 'cuda) [0 0])) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [])) \"-5.0+10.0i\"))) true))", pass, fail);
    test_truthy(interp, "contract cuda Complex64 rank-2 single-axis roundtrips when complex contract available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'contract-complex64) (let (out (contract (to-device (Tensor Complex64 [1 2] [(Complex64 1 1) (Complex64 2 0)]) 'cuda) (to-device (Tensor Complex64 [2 1] [(Complex64 3 -1) (Complex64 0 4)]) 'cuda) [1 0])) (and (= (device out) 'cuda) (= (String (dtype out)) \"Complex64\") (= (String (ref (to-device out 'cpu) [0 0])) \"4.0+10.0i\"))) true))", pass, fail);
    test_truthy(interp, "contract cuda Complex128 fails closed when complex contract unavailable",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (and (ref cuda-backend 'complex128) (= (ref cuda-backend 'contract-complex128) false)) (= (handle (contract (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (to-device (Tensor Complex128 [1] [(Complex128 3 4)]) 'cuda) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda contract into cpu destination fails closed when cublas available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (= (handle (let (dst (Tensor Float64 [2 2] 0.0)) (realize (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda map into cuda destination when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map + (to-device (Tensor Float64 [1] [2.0]) 'cuda) 5.0) dst) (= (ref (to-device dst 'cpu) [0]) 7.0))) true))", pass, fail);
    test_truthy(interp, "realize cuda broadcast map into cuda destination when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (dst (to-device (Tensor Float64 [2 3] 0.0) 'cuda)) (block (realize (map + (to-device (Tensor Float64 [2 1] [1 2]) 'cuda) (to-device (Tensor Float64 [1 3] [10 20 30]) 'cuda)) dst) (and (= (device dst) 'cuda) (= (ref (to-device dst 'cpu) [1 2]) 32.0)))) true))", pass, fail);
    test_truthy(interp, "realize lazy Float32 cuda map into cuda destination when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (dst (to-device (Tensor Float32 [1] [0.0]) 'cuda)) (block (realize (map * (to-device (Tensor Float32 [1] [2.5]) 'cuda) 2.0) dst) (and (= (String (dtype dst)) \"Float32\") (= (ref (to-device dst 'cpu) [0]) (Float32 5.0))))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda map into cpu destination still fails closed when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (handle (let (dst (Tensor Float64 [1] 0.0)) (realize (map + (to-device (Tensor Float64 [1] [2.0]) 'cuda) 0.0) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    {
        char[] name = "map cuda mixed cpu operand reports placement diagnostic";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [1] [2.0]) 'cuda) (Tensor Float64 [1] [1.0]))",
                "map: CUDA operands must remain CUDA-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda mixed cpu lazy operand rejects before cpu materialization";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'cuda))",
                "map: CUDA operands must remain CUDA-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda mixed Tensor dtype reports dtype mismatch";
        EvalResult guard = run("(let (cuda-backend (ref (tensor-backends) 1)) (and (ref cuda-backend 'elementwise-map-float64) (ref cuda-backend 'elementwise-map-float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [1] [2.0]) 'cuda) (to-device (Tensor Float32 [1] [1.0]) 'cuda))",
                "map: tensor dtype mismatch", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "map cuda unary plus returns cuda tensor when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (device (map + (to-device (Tensor Float64 [2] [1.5 -2.5]) 'cuda))) 'cuda) true))", pass, fail);
    test_truthy(interp, "map cuda unary abs roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (map abs (to-device (Tensor Float64 [3] [-1.5 0.0 2.5]) 'cuda)) 'cpu) [0]) 1.5) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 abs roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (= (ref (to-device (map abs (to-device (Tensor Float32 [3] [-1.25 0.0 2.5]) 'cuda)) 'cpu) [0]) 1.25) true))", pass, fail);
    test_truthy(interp, "map cuda unary negation roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (map - (to-device (Tensor Float64 [3] [1.5 -2.0 0.0]) 'cuda)) 'cpu) [1]) 2.0) true))", pass, fail);
    test_truthy(interp, "map cuda unary sqrt roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (map sqrt (to-device (Tensor Float64 [3] [1.0 4.0 9.0]) 'cuda)) 'cpu) [2]) 3.0) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 sqrt preserves dtype when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (out (map sqrt (to-device (Tensor Float32 [3] [1.0 4.0 9.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [2]) (Float32 3.0)))) true))", pass, fail);
    test_truthy(interp, "abs cuda tensor roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (abs (to-device (Tensor Float64 [2] [-1.5 2.5]) 'cuda)) 'cpu) [0]) 1.5) true))", pass, fail);
    test_truthy(interp, "unary minus cuda tensor roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (- (to-device (Tensor Float64 [2] [1.5 -2.5]) 'cuda)) 'cpu) [0]) -1.5) true))", pass, fail);
    test_truthy(interp, "sqrt cuda tensor preserves placement when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (device (sqrt (to-device (Tensor Float64 [2] [4.0 9.0]) 'cuda))) 'cuda) true))", pass, fail);
    test_truthy(interp, "real-part cuda tensor roundtrips when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (real-part (to-device (Tensor Float64 [2] [1.5 -2.5]) 'cuda)) 'cpu) [1]) -2.5) true))", pass, fail);
    test_truthy(interp, "imag-part cuda tensor returns zero when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (ref (to-device (imag-part (to-device (Tensor Float64 [2] [1.5 -2.5]) 'cuda)) 'cpu) [0]) 0.0) true))", pass, fail);
    test_truthy(interp, "conjugate cuda tensor returns cuda tensor when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (= (device (conjugate (to-device (Tensor Float64 [2] [1.5 -2.5]) 'cuda))) 'cuda) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda unary map into cuda destination when map available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (dst (to-device (Tensor Float64 [3] 0.0) 'cuda)) (block (realize (map sqrt (to-device (Tensor Float64 [3] [1.0 4.0 9.0]) 'cuda)) dst) (and (= (device dst) 'cuda) (= (ref (to-device dst 'cpu) [2]) 3.0)))) true))", pass, fail);
    test_truthy(interp, "map cuda scientific unary Float64 family follows capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (and (< (abs (ref (to-device (map sin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (- (ref (to-device (map cos (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.000001) (< (abs (ref (to-device (map tan (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (ref (to-device (map asin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (ref (to-device (map acos (to-device (Tensor Float64 [1] [1.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (ref (to-device (map atan (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (ref (to-device (map sinh (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (- (ref (to-device (map cosh (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.000001) (< (abs (ref (to-device (map tanh (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (- (ref (to-device (map exp (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.000001) (< (abs (ref (to-device (map log (to-device (Tensor Float64 [1] [1.0]) 'cuda)) 'cpu) [0])) 0.000001) (< (abs (- (ref (to-device (map log10 (to-device (Tensor Float64 [1] [100.0]) 'cuda)) 'cpu) [0]) 2.0)) 0.000001)) (if (ref cuda-backend 'elementwise-map-float64) (= (handle (map sin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda scientific unary Float32 family follows capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (and (< (abs (ref (to-device (map sin (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cos (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tan (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map asin (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map acos (to-device (Tensor Float32 [1] [1.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map atan (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map sinh (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cosh (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tanh (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map exp (to-device (Tensor Float32 [1] [0.0]) 'cuda)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map log (to-device (Tensor Float32 [1] [1.0]) 'cuda)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map log10 (to-device (Tensor Float32 [1] [100.0]) 'cuda)) 'cpu) [0]) 2.0)) 0.0001)) (if (ref cuda-backend 'elementwise-map-float32) (= (handle (map exp (to-device (Tensor Float32 [1] [0.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda math/erf and math/erfc Float64 follow scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (and (< (abs (- (ref (to-device (map math/erf (to-device (Tensor Float64 [1] [1.0]) 'cuda)) 'cpu) [0]) 0.842700792949715)) 0.000001) (< (abs (- (ref (to-device (map math/erfc (to-device (Tensor Float64 [1] [1.0]) 'cuda)) 'cpu) [0]) 0.157299207050285)) 0.000001)) (if (ref cuda-backend 'elementwise-map-float64) (= (handle (map math/erf (to-device (Tensor Float64 [1] [1.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda math/erf Float32 preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (map math/erf (to-device (Tensor Float32 [1] [1.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.8427008)) 0.0001))) true))", pass, fail);
    test_truthy(interp, "map cuda stats/normal-cdf Float64 follows scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (and (< (abs (- (ref (to-device (map stats/normal-cdf (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0]) 0.5)) 0.000001) (< (abs (- (ref (to-device (map stats/normal-cdf (to-device (Tensor Float64 [1] [1.0]) 'cuda)) 'cpu) [0]) 0.841344746068543)) 0.000001)) (if (ref cuda-backend 'elementwise-map-float64) (= (handle (map stats/normal-cdf (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "map cuda stats/normal-cdf Float32 preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (map stats/normal-cdf (to-device (Tensor Float32 [1] [0.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.5)) 0.0001))) true))", pass, fail);
    {
        char[] name = "map cuda stats/normal-quantile Float64 preserves placement and values when available";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'scientific-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (out (map stats/normal-quantile (to-device (Tensor Float64 [3] [0.025 0.5 0.975]) 'cuda))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) -1.95996398454005)) 0.00001) (< (abs (ref cpu [1])) 0.000001) (< (abs (- (ref cpu [2]) 1.95996398454005)) 0.00001))))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda stats/normal-quantile rejects zero probability without CPU fallback";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'scientific-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map stats/normal-quantile (to-device (Tensor Float64 [1] [0.0]) 'cuda))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda stats/normal-quantile rejects non-finite probability without CPU fallback";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'scientific-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map stats/normal-quantile (to-device (Tensor Float64 [1] [(ref (json-parse \"[NaN]\" '(allow-nan-inf)) 0)]) 'cuda))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "sin cuda tensor follows scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (and (= (device (sin (to-device (Tensor Float64 [1] [0.0]) 'cuda))) 'cuda) (< (abs (ref (to-device (sin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) 'cpu) [0])) 0.000001)) (if (ref cuda-backend 'elementwise-map-float64) (= (handle (sin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "direct Float32 log10 cuda tensor follows scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (log10 (to-device (Tensor Float32 [1] [100.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 2.0)) 0.0001))) (if (ref cuda-backend 'elementwise-map-float32) (= (handle (log10 (to-device (Tensor Float32 [1] [100.0]) 'cuda)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "direct math/erf cuda tensor follows scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (out (math/erf (to-device (Tensor Float64 [1] [1.0]) 'cuda))) (and (= (device out) 'cuda) (< (abs (- (ref (to-device out 'cpu) [0]) 0.842700792949715)) 0.000001))) true))", pass, fail);
    test_truthy(interp, "direct Float32 math/erfc cuda tensor preserves dtype",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (math/erfc (to-device (Tensor Float32 [1] [1.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.15729921)) 0.0001))) true))", pass, fail);
    test_truthy(interp, "direct stats/normal-cdf cuda tensor follows scientific capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (out (stats/normal-cdf (to-device (Tensor Float64 [1] [0.0]) 'cuda))) (and (= (device out) 'cuda) (< (abs (- (ref (to-device out 'cpu) [0]) 0.5)) 0.000001))) true))", pass, fail);
    test_truthy(interp, "direct Float32 stats/normal-cdf cuda tensor preserves dtype",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (stats/normal-cdf (to-device (Tensor Float32 [1] [0.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.5)) 0.0001))) true))", pass, fail);
    test_truthy(interp, "direct stats/normal-quantile cuda Float32 preserves placement and dtype",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float32) (let (out (stats/normal-quantile (to-device (Tensor Float32 [2] [0.5 0.975]) 'cuda))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (< (abs (ref cpu [0])) 0.0001) (< (abs (- (ref cpu [1]) 1.959964)) 0.0002)))) true))", pass, fail);
    {
        char[] name = "direct cuda floor returns BigInteger tensor";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (floor (to-device (Tensor Float64 [2] [3.7 -3.2]) 'cuda)) 'cpu) f32 (to-device (floor (to-device (Tensor Float32 [2] [3.7 -3.2]) 'cuda)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"3\") (= (String (ref f64 [1])) \"-4\") (= (String (ref f32 [0])) \"3\") (= (String (ref f32 [1])) \"-4\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct cuda ceiling returns BigInteger tensor";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (ceiling (to-device (Tensor Float64 [2] [3.2 -3.7]) 'cuda)) 'cpu) f32 (to-device (ceiling (to-device (Tensor Float32 [2] [3.2 -3.7]) 'cuda)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"4\") (= (String (ref f64 [1])) \"-3\") (= (String (ref f32 [0])) \"4\") (= (String (ref f32 [1])) \"-3\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct cuda round returns BigInteger tensor";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (round (to-device (Tensor Float64 [2] [3.5 -3.5]) 'cuda)) 'cpu) f32 (to-device (round (to-device (Tensor Float32 [2] [3.5 -3.5]) 'cuda)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"4\") (= (String (ref f64 [1])) \"-4\") (= (String (ref f32 [0])) \"4\") (= (String (ref f32 [1])) \"-4\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct cuda truncate returns BigInteger tensor";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (truncate (to-device (Tensor Float64 [2] [3.9 -3.9]) 'cuda)) 'cpu) f32 (to-device (truncate (to-device (Tensor Float32 [2] [3.9 -3.9]) 'cuda)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"3\") (= (String (ref f64 [1])) \"-3\") (= (String (ref f32 [0])) \"3\") (= (String (ref f32 [1])) \"-3\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct cuda rounding BigInteger tensor supports Array and List copyback";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (Array (to-device (floor (to-device (Tensor Float64 [2] [2.9 -2.1]) 'cuda)) 'cpu)) f32 (List (to-device (round (to-device (Tensor Float32 [2] [2.5 -2.5]) 'cuda)) 'cpu))) (and (= (String (ref f64 0)) \"2\") (and (= (String (ref f64 1)) \"-3\") (and (= (String (ref f32 0)) \"3\") (= (String (ref f32 1)) \"-3\")))))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats/normal-quantile cuda rejects one probability without CPU fallback";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'scientific-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats/normal-quantile (to-device (Tensor Float64 [1] [1.0]) 'cuda))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats/normal-quantile cuda gives non-finite status priority";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'scientific-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats/normal-quantile (to-device (Tensor Float64 [2] [0.0 (ref (json-parse \"[NaN]\" '(allow-nan-inf)) 0)]) 'cuda))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "realize lazy cuda scientific unary map into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map sin (to-device (Tensor Float64 [1] [0.0]) 'cuda)) dst) (and (= (device dst) 'cuda) (< (abs (ref (to-device dst 'cpu) [0])) 0.000001)))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda math/erfc map into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map math/erfc (to-device (Tensor Float64 [1] [1.0]) 'cuda)) dst) (and (= (device dst) 'cuda) (< (abs (- (ref (to-device dst 'cpu) [0]) 0.157299207050285)) 0.000001)))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda stats/normal-quantile map into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map stats/normal-quantile (to-device (Tensor Float64 [1] [0.975]) 'cuda)) dst) (and (= (device dst) 'cuda) (< (abs (- (ref (to-device dst 'cpu) [0]) 1.95996398454005)) 0.00001)))) true))", pass, fail);
    test_truthy(interp, "realize lazy cuda stats/normal-cdf map into cuda destination when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'scientific-map-float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (block (realize (map stats/normal-cdf (to-device (Tensor Float64 [1] [0.0]) 'cuda)) dst) (and (= (device dst) 'cuda) (< (abs (- (ref (to-device dst 'cpu) [0]) 0.5)) 0.000001)))) true))", pass, fail);
    test_truthy(interp, "to-device vulkan follows backend availability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (device (to-device (Tensor Float64 [1] [2]) 'vulkan)) 'vulkan) (= (handle (to-device (Tensor Float64 [1] [2]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device vulkan Float32 follows backend capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (to-device (Tensor Float32 [2] [1.25 2.5]) 'vulkan)) 'vulkan) (= (handle (to-device (Tensor Float32 [1] [2]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)) true))", pass, fail);
    test_truthy(interp, "to-device vulkan Float32 roundtrip when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'vulkan) 'cpu) [1]) 2.5) true) true))", pass, fail);
    test_truthy(interp, "to-device vulkan Float32 copyback preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'vulkan) 'cpu))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "to-device vulkan roundtrip when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (ref (to-device (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 'cpu) [1]) 2.0) true))", pass, fail);
    test_truthy(interp, "realize preserves vulkan tensor placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (realize (to-device (Tensor Float64 [1] [2]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "realize vulkan tensor into cpu destination fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (let (dst (Tensor Float64 [1] 0.0)) (realize (to-device (Tensor Float64 [1] [2.0]) 'vulkan) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "realize vulkan Complex64 tensor into cpu destination fails closed when storage is available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (= (handle (let (dst (Tensor Complex64 [1] 0)) (realize (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy vulkan tensor into cpu destination fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (let (dst (Tensor Float64 [1] 0.0)) (realize (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "realize cpu tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'vulkan)) (block (realize (Tensor Float64 [1] [2.0]) dst) (= (ref (to-device dst 'cpu) [0]) 2.0))) true) true))", pass, fail);
    test_truthy(interp, "realize cpu Float32 tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (dst (to-device (Tensor Float32 [2] [0.0 0.0]) 'vulkan)) (block (realize (Tensor Float32 [2] [1.25 2.5]) dst) (= (ref (to-device dst 'cpu) [1]) 2.5))) true) true))", pass, fail);
    test_truthy(interp, "realize scalar fill into vulkan Float32 destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (dst (to-device (Tensor Float32 [2] 0.0) 'vulkan)) (block (realize 1.25 dst) (= (ref (to-device dst 'cpu) [1]) 1.25))) true) true))", pass, fail);
    test_truthy(interp, "realize Complex64 cpu tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (let (dst (to-device (Tensor Complex64 [1] [(Complex64 0 0)]) 'vulkan)) (block (realize (Tensor Complex64 [1] [(Complex64 5 -6)]) dst) (and (= (device dst) 'vulkan) (= (String (ref (to-device dst 'cpu) [0])) \"5.0-6.0i\")))) true))", pass, fail);
    test_truthy(interp, "realize Complex64 scalar fill into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (let (dst (to-device (Tensor Complex64 [2] 0) 'vulkan)) (block (realize (Complex64 7 -8) dst) (= (String (ref (to-device dst 'cpu) [1])) \"7.0-8.0i\"))) true))", pass, fail);
    test_truthy(interp, "realize vulkan tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'vulkan)) (block (realize (to-device (Tensor Float64 [1] [2.0]) 'vulkan) dst) (and (= (device dst) 'vulkan) (= (ref (to-device dst 'cpu) [0]) 2.0)))) true) true))", pass, fail);
    test_truthy(interp, "realize Complex64 vulkan tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (let (dst (to-device (Tensor Complex64 [1] [(Complex64 0 0)]) 'vulkan)) (block (realize (to-device (Tensor Complex64 [1] [(Complex64 -5 6)]) 'vulkan) dst) (and (= (device dst) 'vulkan) (= (String (ref (to-device dst 'cpu) [0])) \"-5.0+6.0i\")))) true))", pass, fail);
    test_truthy(interp, "contract vulkan Complex128 rank-1 dot roundtrips when complex contract available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'contract-complex128) (let (out (contract (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'vulkan) (to-device (Tensor Complex128 [1] [(Complex128 3 4)]) 'vulkan) [0] [0])) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex128\") (= (String (ref (to-device out 'cpu) [])) \"-5.0+10.0i\"))) true))", pass, fail);
    test_truthy(interp, "contract vulkan Complex64 rank-2 single-axis roundtrips when complex contract available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'contract-complex64) (let (out (contract (to-device (Tensor Complex64 [1 2] [(Complex64 1 1) (Complex64 2 0)]) 'vulkan) (to-device (Tensor Complex64 [2 1] [(Complex64 3 -1) (Complex64 0 4)]) 'vulkan) [1 0])) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Complex64\") (= (String (ref (to-device out 'cpu) [0 0])) \"4.0+10.0i\"))) true))", pass, fail);
    test_truthy(interp, "contract vulkan Complex64 fails closed when complex contract unavailable",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'complex64) (= (ref vulkan-backend 'contract-complex64) false)) (= (handle (contract (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan) (to-device (Tensor Complex64 [1] [(Complex64 3 4)]) 'vulkan) [0] [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "realize lazy vulkan tensor into vulkan destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (dst (to-device (Tensor Float64 [1] [0.0]) 'vulkan)) (block (realize (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) dst) (= (ref (to-device dst 'cpu) [0]) 2.0))) true) true))", pass, fail);
    test_truthy(interp, "realize explicit cpu copy from vulkan into destination when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (dst (Tensor Float64 [1] 0.0)) (block (realize (to-device (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) 'cpu) dst) (= (ref dst [0]) 2.0))) true) true))", pass, fail);
    test_truthy(interp, "to-device vulkan same target is idempotent when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (to-device (to-device (Tensor Float64 [1] [2]) 'vulkan) 'vulkan)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "to-device cuda same target is idempotent when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (device (to-device (to-device (Tensor Float64 [1] [2]) 'cuda) 'cuda)) 'cuda) true))", pass, fail);
    test_truthy(interp, "to-device vulkan to cuda fails closed when both available",
        "(let (cuda-backend (ref (tensor-backends) 1) vulkan-backend (ref (tensor-backends) 3)) (if (and (ref cuda-backend 'available) (ref vulkan-backend 'available)) (if (ref vulkan-backend 'float64) (= (handle (to-device (to-device (Tensor Float64 [1] [2]) 'vulkan) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "to-device cuda to vulkan fails closed when both available",
        "(let (cuda-backend (ref (tensor-backends) 1) vulkan-backend (ref (tensor-backends) 3)) (if (and (ref cuda-backend 'available) (ref vulkan-backend 'available)) (if (ref vulkan-backend 'float64) (= (handle (to-device (to-device (Tensor Float64 [1] [2]) 'cuda) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "realize cuda tensor into vulkan destination fails closed when both available",
        "(let (cuda-backend (ref (tensor-backends) 1) vulkan-backend (ref (tensor-backends) 3)) (if (and (ref cuda-backend 'available) (ref vulkan-backend 'available)) (if (ref vulkan-backend 'float64) (= (handle (let (dst (to-device (Tensor Float64 [1] [0.0]) 'vulkan)) (realize (to-device (Tensor Float64 [1] [2.0]) 'cuda) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "realize vulkan tensor into cuda destination fails closed when both available",
        "(let (cuda-backend (ref (tensor-backends) 1) vulkan-backend (ref (tensor-backends) 3)) (if (and (ref cuda-backend 'available) (ref vulkan-backend 'available)) (if (ref vulkan-backend 'float64) (= (handle (let (dst (to-device (Tensor Float64 [1] [0.0]) 'cuda)) (realize (to-device (Tensor Float64 [1] [2.0]) 'vulkan) dst)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "map vulkan add scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 5.0) 'cpu) [2]) 8.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 add scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map + (to-device (Tensor Float32 [3] [1.25 2.5 3.75]) 'vulkan) 0.5) 'cpu) [2]) 4.25) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 add scalar returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (map + (to-device (Tensor Float32 [2] [1.25 2.5]) 'vulkan) 0.5)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (map + (to-device (Tensor Float32 [2] [1.25 2.5]) 'vulkan) 0.5))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float64 still preserves Float64 after Float32 support",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (String (dtype (map + (to-device (Tensor Float64 [2] [1.25 2.5]) 'vulkan) 0.5))) \"Float64\") true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary plus roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + (to-device (Tensor Float64 [3] [1.5 -2.0 0.0]) 'vulkan)) 'cpu) [1]) -2.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 unary plus returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (map + (to-device (Tensor Float32 [2] [1.25 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary plus returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map + (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "unary plus vulkan tensor preserves placement when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (+ (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary abs roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map abs (to-device (Tensor Float64 [3] [-1.5 0.0 2.5]) 'vulkan)) 'cpu) [0]) 1.5) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 abs roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map abs (to-device (Tensor Float32 [3] [-1.25 0.0 2.5]) 'vulkan)) 'cpu) [0]) 1.25) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary abs returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map abs (to-device (Tensor Float64 [2] [-1.5 2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "abs vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (abs (to-device (Tensor Float64 [2] [-1.5 2.5]) 'vulkan)) 'cpu) [0]) 1.5) true) true))", pass, fail);
    test_truthy(interp, "abs vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (abs (map + (to-device (Tensor Float64 [2] [-1.0 2.0]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary negation roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map - (to-device (Tensor Float64 [3] [1.5 -2.0 0.0]) 'vulkan)) 'cpu) [1]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 negation roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map - (to-device (Tensor Float32 [2] [1.25 -2.5]) 'vulkan)) 'cpu) [1]) 2.5) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary negation returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map - (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "unary minus vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (- (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan)) 'cpu) [0]) -1.5) true) true))", pass, fail);
    test_truthy(interp, "unary minus vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (- (map + (to-device (Tensor Float64 [2] [1.0 -2.0]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary sqrt roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map sqrt (to-device (Tensor Float64 [3] [1.0 4.0 9.0]) 'vulkan)) 'cpu) [2]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 sqrt roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map sqrt (to-device (Tensor Float32 [3] [1.0 4.0 9.0]) 'vulkan)) 'cpu) [2]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan unary sqrt returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map sqrt (to-device (Tensor Float64 [2] [4.0 9.0]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "sqrt vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (sqrt (to-device (Tensor Float64 [2] [4.0 9.0]) 'vulkan)) 'cpu) [0]) 2.0) true) true))", pass, fail);
    test_truthy(interp, "direct Float32 sqrt vulkan tensor returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (sqrt (to-device (Tensor Float32 [2] [4.0 9.0]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "sqrt vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (sqrt (map + (to-device (Tensor Float64 [2] [4.0 9.0]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan real-part roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map real-part (to-device (Tensor Float64 [3] [1.5 -2.5 0.0]) 'vulkan)) 'cpu) [1]) -2.5) true) true))", pass, fail);
    test_truthy(interp, "map vulkan imag-part returns zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map imag-part (to-device (Tensor Float64 [3] [1.5 -2.5 0.0]) 'vulkan)) 'cpu) [2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan conjugate returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map conjugate (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "real-part vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (real-part (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan)) 'cpu) [1]) -2.5) true) true))", pass, fail);
    test_truthy(interp, "imag-part vulkan tensor returns zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (imag-part (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan)) 'cpu) [0]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "conjugate vulkan tensor returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (conjugate (to-device (Tensor Float64 [2] [1.5 -2.5]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "real-part vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (real-part (map + (to-device (Tensor Float64 [2] [1.0 -2.0]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "imag-part vulkan lazy map returns zero when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (imag-part (map + (to-device (Tensor Float64 [2] [1.0 -2.0]) 'vulkan) 0.0)) 'cpu) [0]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "conjugate vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (conjugate (map + (to-device (Tensor Float64 [2] [1.0 -2.0]) 'vulkan) 0.0))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan scalar-left add roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + 10.0 (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) 'cpu) [1]) 12.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-scalar subtract roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map - (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) 4.0) 'cpu) [2]) 26.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan scalar-tensor subtract roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map - 100.0 (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)) 'cpu) [1]) 80.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-tensor multiply roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map * (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)) 'cpu) [2]) 90.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 tensor-tensor broadcast roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map + (to-device (Tensor Float32 [2 1] [1.25 2.5]) 'vulkan) (to-device (Tensor Float32 [1 2] [10.0 20.0]) 'vulkan)) 'cpu) [1 1]) 22.5) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-scalar divide roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map / (to-device (Tensor Float64 [3] [8 9 10]) 'vulkan) 2.0) 'cpu) [0]) 4.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-scalar min roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map min (to-device (Tensor Float64 [3] [1.0 5.0 -2.0]) 'vulkan) 0.0) 'cpu) [2]) -2.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 tensor-scalar min roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map min (to-device (Tensor Float32 [3] [1.25 5.5 -2.5]) 'vulkan) 0.0) 'cpu) [2]) -2.5) true) true))", pass, fail);
    test_truthy(interp, "map vulkan scalar-tensor max roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map max 0.0 (to-device (Tensor Float64 [3] [1.0 -5.0 2.0]) 'vulkan)) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan Float32 scalar-tensor max roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (map max 0.0 (to-device (Tensor Float32 [3] [1.25 -5.5 2.5]) 'vulkan)) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-tensor returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [2] [3 4]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-tensor broadcast trailing singleton axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [1 3] [10 20 30]) 'vulkan)) 'cpu) [1 2]) 36.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-tensor broadcast inner singleton axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + (to-device (Tensor Float64 [2 1] [1 2]) 'vulkan) (to-device (Tensor Float64 [1 3] [10 20 30]) 'vulkan)) 'cpu) [1 2]) 32.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan rank-0 tensor broadcasts with rank-2 tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (map + (to-device (Tensor Float64 [] 7.0) 'vulkan) (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan)) 'cpu) [1 1]) 11.0) true) true))", pass, fail);
    test_truthy(interp, "map vulkan tensor-tensor broadcast returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (map + (to-device (Tensor Float64 [2 1] [1 2]) 'vulkan) (to-device (Tensor Float64 [1 3] [10 20 30]) 'vulkan))) 'vulkan) true) true))", pass, fail);
    {
        char[] name = "map vulkan Float64 scientific unary remains fail-closed when available";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map sin (to-device (Tensor Float64 [1] [0.0]) 'vulkan))",
                "map: Vulkan currently supports dense row-major Float64 and Float32 unary operations", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map vulkan unsupported callable rejects lazy source before cpu fallback";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
}
