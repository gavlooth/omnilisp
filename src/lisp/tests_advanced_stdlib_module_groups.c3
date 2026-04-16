module lisp;


import std::io;
import main;

fn void run_advanced_collections_dict_literal_tests(Interp* interp, int* pass, int* fail) {
    // Dicts
    setup(interp, "(define hm1 (Dictionary 1 10 2 20 3 30))");
    test_eq(interp, "ref dict", "(ref hm1 2)", 20, pass, fail);
    test_nil(interp, "ref dict missing", "(ref hm1 99)", pass, fail);
    test_tag_interp(interp, "set! dict returns void", "(set! hm1 2 200)", VOID, pass, fail);
    test_eq(interp, "set! dict overwrite", "(ref hm1 2)", 200, pass, fail);
    test_tag_interp(interp, "set! generic dict returns void", "(set! hm1 2 250)", VOID, pass, fail);
    test_eq(interp, "set! generic dict overwrite", "(ref hm1 2)", 250, pass, fail);
    test_truthy(interp, "has? dict yes", "(has? hm1 1)", pass, fail);
    test_nil(interp, "has? dict no", "(has? hm1 99)", pass, fail);
    test_eq(interp, "length dict", "(length hm1)", 3, pass, fail);
    setup(interp, "(define hm2 (Dictionary 1 10 2 20 3 30))");
    test_tag_interp(interp, "remove! returns void", "(remove! hm2 2)", VOID, pass, fail);
    test_nil(interp, "remove! dict", "(has? hm2 2)", pass, fail);
    test_eq(interp, "remove! count", "(length hm2)", 2, pass, fail);
    setup(interp, "(define hm3 (Dictionary 1 10 2 20))");
    test_eq(interp, "keys len", "(length (keys hm3))", 2, pass, fail);
    test_eq(interp, "values len", "(length (values hm3))", 2, pass, fail);
    test_truthy(interp, "keys canonical order symbol",
        "(= (keys (Dictionary 'b 2 'a 1 'c 3)) '(a b c))", pass, fail);
    test_truthy(interp, "values canonical follow keys",
        "(= (values (Dictionary 'b 2 'a 1 'c 3)) '(1 2 3))", pass, fail);
    test_truthy(interp, "keys canonical after remove+insert",
        "(let (d (Dictionary 'c 3 'a 1 'b 2)) (block (remove! d 'b) (set! d 'b 20) (= (keys d) '(a b c))))", pass, fail);
    test_truthy(interp, "values canonical after remove+insert",
        "(let (d (Dictionary 'c 3 'a 1 'b 2)) (block (remove! d 'b) (set! d 'b 20) (= (values d) '(1 20 3))))", pass, fail);
    setup(interp, "(define hm4 (Dictionary \"name\" \"Alice\" \"age\" 30))");
    test_str(interp, "dict string key", "(ref hm4 \"name\")", pass, fail);
    setup(interp, "(define hm5 (Dictionary))");
    test_eq(interp, "dict empty count", "(length hm5)", 0, pass, fail);
    test_truthy(interp, "dict? Dictionary", "(dict? (Dictionary 1 2))", pass, fail);
    test_truthy(interp, "dict? Dict shorthand", "(dict? (Dict 1 2))", pass, fail);
    test_eq(interp, "Dictionary canonical constructor", "(ref (Dictionary 1 10 2 20) 1)", 10, pass, fail);
    test_eq(interp, "Dict shorthand constructor", "(ref (Dict 1 10 2 20) 1)", 10, pass, fail);
    test_eq(interp, "Dict shorthand type annotation dispatch",
        "(block (define (dict-ann (^Dict d)) (ref d 'x)) (dict-ann (Dictionary 'x 5)))", 5, pass, fail);
    test_nil(interp, "dict? no", "(dict? 42)", pass, fail);
    setup(interp, "(define hm-big (Dictionary))");
    setup(interp, "(let loop (i 0) (if (= i 50) hm-big (block (set! hm-big i (* i i)) (loop (+ i 1)))))");
    test_eq(interp, "dict auto-grow count", "(length hm-big)", 50, pass, fail);
    test_eq(interp, "dict auto-grow ref 7", "(ref hm-big 7)", 49, pass, fail);

    // === LITERAL SYNTAX TESTS ===
    // Dictionary literal {}
    test_eq(interp, "{} empty", "(length {})", 0, pass, fail);
    test_eq(interp, "{} literal", "(ref {'a 1 'b 2} 'a)", 1, pass, fail);
    test_truthy(interp, "{} string keys", "(has? {\"name\" \"Alice\"} \"name\")", pass, fail);
    test_truthy(interp, "{} has key", "(has? {'x 10 'y 20} 'y)", pass, fail);
    test_eq(interp, "{} count", "(length {'a 1 'b 2 'c 3})", 3, pass, fail);
    // Array literal []
    test_eq(interp, "[] empty", "(length [])", 0, pass, fail);
    test_eq(interp, "[] literal", "(ref [10 20 30] 1)", 20, pass, fail);
    test_eq(interp, "[] length", "(length [1 2 3])", 3, pass, fail);
    test_eq(interp, "[] nested", "(ref (ref [1 [2 3]] 1) 0)", 2, pass, fail);
    test_eq(interp, "[] index", "(let (a [10 20 30]) a.[1])", 20, pass, fail);
    // Cons cell dot-path access and mutation
    test_eq(interp, "cons .car", "(let (p (cons 1 2)) p.car)", 1, pass, fail);
    test_eq(interp, "cons .cdr", "(let (p (cons 1 2)) p.cdr)", 2, pass, fail);
    test_tag(interp, "set! cons returns void", "(let (p (cons 1 2)) (set! p.car 99))", VOID, pass, fail);
    test_eq_interp(interp, "set! cons .car", "(let (p (cons 1 2)) (block (set! p.car 99) p.car))", 99, pass, fail);
    test_eq_interp(interp, "set! cons .cdr", "(let (p (cons 1 2)) (block (set! p.cdr 50) p.cdr))", 50, pass, fail);
    test_eq_interp(interp, "set! cons preserves", "(let (p (cons 1 2)) (block (set! p.car 99) p.cdr))", 2, pass, fail);
}

fn void run_advanced_collections_generic_ops_tests(Interp* interp, int* pass, int* fail) {
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
    {
        char[] name = "map vulkan mixed cpu lazy operand rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "map: Vulkan operands must remain Vulkan-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map vulkan unsupported binary callable rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "map vulkan scientific unary Float32 family roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (ref (to-device (map sin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cos (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tan (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map asin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map acos (to-device (Tensor Float32 [1] [1.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map atan (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map sinh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cosh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tanh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map exp (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map log (to-device (Tensor Float32 [1] [1.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map log10 (to-device (Tensor Float32 [1] [100.0]) 'vulkan)) 'cpu) [0]) 2.0)) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats/normal-cdf Float32 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map stats/normal-cdf (to-device (Tensor Float32 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.8413447)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats/normal-cdf Float64 preserves placement and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (out (map stats/normal-cdf (to-device (Tensor Float64 [3] [-1.0 0.0 1.0]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) 0.158655253931457)) 0.000001) (< (abs (- (ref cpu [1]) 0.5)) 0.000001) (< (abs (- (ref cpu [2]) 0.841344746068543)) 0.000001)))) true) true))", pass, fail);
    test_truthy(interp, "sin vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (ref (to-device (sin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "direct Float32 log10 vulkan tensor preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (log10 (to-device (Tensor Float32 [1] [100.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 2.0)) 0.0001))) true) true))", pass, fail);
    test_truthy(interp, "direct stats/normal-cdf vulkan Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (stats/normal-cdf (to-device (Tensor Float32 [1] [0.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.5)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "direct stats/normal-cdf vulkan Float64 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (out (stats/normal-cdf (to-device (Tensor Float64 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.841344746068543)) 0.000001))) true) true))", pass, fail);
    test_truthy(interp, "direct stats/normal-quantile vulkan Float32 preserves placement dtype and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (stats/normal-quantile (to-device (Tensor Float32 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0]) -1.959964)) 0.0005) (< (abs (ref cpu [1])) 0.0002) (< (abs (- (ref cpu [2]) 1.959964)) 0.0005)))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats/normal-quantile Float32 preserves placement dtype and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map stats/normal-quantile (to-device (Tensor Float32 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0]) -1.959964)) 0.0005) (< (abs (ref cpu [1])) 0.0002) (< (abs (- (ref cpu [2]) 1.959964)) 0.0005)))) true) true))", pass, fail);
    {
        char[] name = "direct stats/normal-quantile vulkan Float32 rejects zero probability without CPU fallback";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats/normal-quantile (to-device (Tensor Float32 [1] [0.0]) 'vulkan))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map vulkan stats/normal-quantile Float32 rejects one probability without CPU fallback";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map stats/normal-quantile (to-device (Tensor Float32 [1] [1.0]) 'vulkan))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats/normal-quantile vulkan Float32 rejects vulkan-produced non-finite probability";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats/normal-quantile (sqrt (to-device (Tensor Float32 [1] [-1.0]) 'vulkan)))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats/normal-quantile vulkan Float32 gives non-finite status priority";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats/normal-quantile (sqrt (to-device (Tensor Float32 [2] [0.0 -1.0]) 'vulkan)))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        EvalResult support = run("(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)) (handle (let (out (stats/normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (ref (to-device out 'cpu) [0])) 0.000001))) (raise msg false)) false))", interp);
        bool enabled = !support.error.has_error && !is_nil(support.value) && !(is_symbol(support.value) && support.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, "direct stats/normal-quantile vulkan Float64 preserves placement dtype and values when available",
                "(let (out (stats/normal-quantile (to-device (Tensor Float64 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) -1.95996398454005)) 0.00001) (< (abs (ref cpu [1])) 0.000001) (< (abs (- (ref cpu [2]) 1.95996398454005)) 0.00001))))", pass, fail);
            test_truthy(interp, "map vulkan stats/normal-quantile Float64 preserves placement dtype and values when available",
                "(let (out (map stats/normal-quantile (to-device (Tensor Float64 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) -1.95996398454005)) 0.00001) (< (abs (ref cpu [1])) 0.000001) (< (abs (- (ref cpu [2]) 1.95996398454005)) 0.00001))))", pass, fail);
            test_error_contains(interp, "direct stats/normal-quantile vulkan Float64 rejects zero probability without CPU fallback",
                "(stats/normal-quantile (to-device (Tensor Float64 [1] [0.0]) 'vulkan))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
            test_error_contains(interp, "map vulkan stats/normal-quantile Float64 rejects one probability without CPU fallback",
                "(map stats/normal-quantile (to-device (Tensor Float64 [1] [1.0]) 'vulkan))",
                "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
            test_error_contains(interp, "direct stats/normal-quantile vulkan Float64 rejects vulkan-produced non-finite probability",
                "(stats/normal-quantile (sqrt (to-device (Tensor Float64 [1] [-1.0]) 'vulkan)))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
            test_error_contains(interp, "direct stats/normal-quantile vulkan Float64 gives non-finite status priority",
                "(stats/normal-quantile (sqrt (to-device (Tensor Float64 [2] [0.0 -1.0]) 'vulkan)))",
                "stats/normal-quantile: expected finite numeric input", pass, fail);
        } else {
            EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
            bool can_check_failclosed = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
            if (can_check_failclosed) {
                test_error_contains(interp, "direct stats/normal-quantile vulkan Float64 remains fail-closed when unavailable",
                    "(stats/normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))",
                    "stats/normal-quantile: Vulkan currently supports dense row-major Float32 tensors", pass, fail);
                test_error_contains(interp, "map vulkan stats/normal-quantile Float64 remains fail-closed when unavailable",
                    "(map stats/normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))",
                    "stats/normal-quantile: Vulkan currently supports dense row-major Float32 tensors", pass, fail);
            } else {
                emit_pass("direct stats/normal-quantile vulkan Float64 remains fail-closed when unavailable");
                (*pass)++;
                emit_pass("map vulkan stats/normal-quantile Float64 remains fail-closed when unavailable");
                (*pass)++;
            }
        }
    }
    {
        test_truthy(interp, "realize lazy cpu map succeeds inside handle stack",
            "(= (ref (handle (realize (map + (Tensor Float64 [2] [1.25 2.75]) 0.0)) (raise msg (ref msg 'message))) [1]) 2.75)", pass, fail);
        test_error_contains(interp, "realize propagates single-arg source errors",
            "(realize (/ 1 0))", "/: division by zero", pass, fail);
        test_error_contains(interp, "realize destination propagates source errors",
            "(realize (/ 1 0) (Tensor Float64 [1] 0.0))", "/: division by zero", pass, fail);
    }
    {
        char[] name = "map vulkan unsupported callable fails closed";
        char[] handler_code_name = "map vulkan unsupported callable raise handler preserves code";
        char[] handler_message_name = "map vulkan unsupported callable raise handler preserves message";
        char[] lazy_handler_name = "map vulkan unsupported callable lazy source handler preserves code";
        char[] realize_name = "realize vulkan unsupported callable fails closed";
        char[] realize_handler_code_name = "realize vulkan unsupported callable handler preserves code";
        char[] realize_lazy_handler_message_name = "realize vulkan unsupported callable lazy source handler preserves message";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
            test_truthy(interp, handler_code_name,
                "(= (handle (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_truthy(interp, handler_message_name,
                "(= (handle (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'message))) \"map: Vulkan currently supports Float64 and Float32 arithmetic kernels\")", pass, fail);
            test_truthy(interp, lazy_handler_name,
                "(= (handle (map floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_error_contains(interp, realize_name,
                "(realize (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
            test_truthy(interp, realize_handler_code_name,
                "(= (handle (realize (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_truthy(interp, realize_lazy_handler_message_name,
                "(= (handle (realize (map floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0))) (raise msg (ref msg 'message))) \"map: Vulkan currently supports Float64 and Float32 arithmetic kernels\")", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
            emit_pass(handler_code_name);
            (*pass)++;
            emit_pass(handler_message_name);
            (*pass)++;
            emit_pass(lazy_handler_name);
            (*pass)++;
            emit_pass(realize_name);
            (*pass)++;
            emit_pass(realize_handler_code_name);
            (*pass)++;
            emit_pass(realize_lazy_handler_message_name);
            (*pass)++;
        }
    }
    {
        char[] name = "minmax vulkan mixed cpu lazy operand rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(min (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "minmax: Vulkan operands must remain Vulkan-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "min vulkan tensor scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (min (to-device (Tensor Float64 [3] [1.0 5.0 -2.0]) 'vulkan) 0.0) 'cpu) [0]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "max vulkan scalar tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (max 0.0 (to-device (Tensor Float64 [3] [1.0 -5.0 2.0]) 'vulkan)) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "min vulkan tensor-tensor broadcast roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (min (to-device (Tensor Float64 [2 1] [2.0 3.0]) 'vulkan) (to-device (Tensor Float64 [1 2] [1.0 4.0]) 'vulkan)) 'cpu) [1 1]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "min vulkan Float32 tensor-tensor broadcast roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (min (to-device (Tensor Float32 [2 1] [2.5 3.5]) 'vulkan) (to-device (Tensor Float32 [1 2] [1.5 4.5]) 'vulkan)) 'cpu) [1 1]) 3.5) true) true))", pass, fail);
    test_truthy(interp, "max vulkan tensor scalar returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (max (to-device (Tensor Float64 [2] [-1.0 2.0]) 'vulkan) 0.0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "max vulkan Float32 tensor scalar returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (max (to-device (Tensor Float32 [2] [-1.25 2.5]) 'vulkan) 0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "max vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (max (map + (to-device (Tensor Float64 [2] [-1.0 2.0]) 'vulkan) 0.0) 0.0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "min vulkan mixed cpu operand fails closed",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (min (to-device (Tensor Float64 [1] [1.0]) 'vulkan) (Tensor Float64 [1] [0.0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    {
        char[] name = "direct vulkan rounding returns BigInteger tensor when capability is enabled";
        EvalResult guard = run("(ref (ref (tensor-backends) 3) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (floor (to-device (Tensor Float64 [2] [3.7 -3.2]) 'vulkan)) 'cpu) f32 (to-device (round (to-device (Tensor Float32 [2] [3.5 -3.5]) 'vulkan)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"3\") (= (String (ref f64 [1])) \"-4\") (= (String (ref f32 [0])) \"4\") (= (String (ref f32 [1])) \"-4\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct vulkan ceiling and truncate return BigInteger tensor when capability is enabled";
        EvalResult guard = run("(ref (ref (tensor-backends) 3) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (ceil (to-device (ceiling (to-device (Tensor Float64 [2] [3.2 -3.7]) 'vulkan)) 'cpu) trunc (to-device (truncate (to-device (Tensor Float32 [2] [3.9 -3.9]) 'vulkan)) 'cpu)) (and (= (String (dtype ceil)) \"BigInteger\") (= (String (dtype trunc)) \"BigInteger\") (= (String (ref ceil [0])) \"4\") (= (String (ref ceil [1])) \"-3\") (= (String (ref trunc [0])) \"3\") (= (String (ref trunc [1])) \"-3\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "floor vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "ceiling vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (ceiling (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "round vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (round (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "truncate vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (truncate (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "floor lazy vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "floor vulkan Float32 tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (floor (to-device (Tensor Float32 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "pow vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (pow (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "pow lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (pow (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) 2.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "atan2 vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (atan2 (to-device (Tensor Float64 [1] [1.0]) 'vulkan) 1.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "atan2 lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (atan2 (map + (to-device (Tensor Float64 [1] [1.0]) 'vulkan) 0.0) 1.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "gcd vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (gcd (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "lcm vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (lcm (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "lcm lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (lcm (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot explicit axes roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0] [0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan map operand realizes on vulkan when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (realize (contract (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan map operand copies to cpu when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan Float32 map operand copies to cpu when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (map + (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float32 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
            EvalResult left_eval = run("(to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)", interp);
            EvalResult right_eval = run("(to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)", interp);
            SymbolId plus_sym = interp.symbols.intern("+");
            Value* plus_func = interp.global_env.lookup(plus_sym);
            Value* zero = make_double(interp, 0.0);
            usz[1] vector_shape = { 3 };
            usz[1] left_axes = { 0 };
            usz[1] right_axes = { 0 };
            usz[1] scalar_shape = { 0 };
            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
                plus_func != null && zero != null && zero.tag != ERROR;
            Value* lazy_map = setup_ok ? make_tensor_map_expr(
                plus_func,
                left_eval.value,
                left_eval.value.tensor_val,
                zero,
                null,
                true,
                TENSOR_DTYPE_DOUBLE,
                vector_shape[0:1],
                interp
            ) : null;
            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
                lazy_map,
                right_eval.value,
                &left_axes[0],
                &right_axes[0],
                1,
                scalar_shape[0:0],
                interp
            ) : null;
            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
            ok = setup_ok &&
                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
                cpu_value.tensor_val.data != null &&
                ((double*)cpu_value.tensor_val.data)[0] == 140.0;
        }
        if (ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor lazy Vulkan contract helper realizes mapped operand"
                : "[PASS] tensor lazy Vulkan contract helper unavailable; mapped operand not exercised");
            *pass += 1;
        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
	            EvalResult left_eval = run("(to-device (Tensor Float64 [2] [1 2]) 'vulkan)", interp);
	            EvalResult right_eval = run("(to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)", interp);
	            SymbolId plus_sym = interp.symbols.intern("+");
	            Value* plus_func = interp.global_env.lookup(plus_sym);
	            Value* zero = make_double(interp, 0.0);
	            usz[1] vector_shape = { 2 };
	            usz[2] result_shape = { 2, 3 };
	            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
	                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
	                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
	                plus_func != null && zero != null && zero.tag != ERROR;
	            Value* lazy_map = setup_ok ? make_tensor_map_expr(
	                plus_func,
	                left_eval.value,
	                left_eval.value.tensor_val,
	                zero,
	                null,
	                true,
	                TENSOR_DTYPE_DOUBLE,
	                vector_shape[0:1],
	                interp
	            ) : null;
	            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
	                lazy_map,
	                right_eval.value,
	                null,
	                null,
	                0,
	                result_shape[0:2],
	                interp
	            ) : null;
	            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
	            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
	            ok = setup_ok &&
	                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
	                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
	                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
	                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
	                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
	                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
	                cpu_value.tensor_val.data != null &&
	                ((double*)cpu_value.tensor_val.data)[5] == 60.0;
	        }
	        if (ok) {
	            emit_pass_literal(vulkan_available
	                ? "[PASS] tensor lazy Vulkan contract helper realizes zero-axis mapped operand"
	                : "[PASS] tensor lazy Vulkan contract helper unavailable; zero-axis mapped operand not exercised");
	            *pass += 1;
	        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes zero-axis mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
	            EvalResult left_eval = run("(to-device (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) 'vulkan)", interp);
	            EvalResult right_eval = run("(to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan)", interp);
	            SymbolId plus_sym = interp.symbols.intern("+");
	            Value* plus_func = interp.global_env.lookup(plus_sym);
	            Value* zero = make_double(interp, 0.0);
	            usz[3] left_shape = { 2, 2, 2 };
	            usz[2] left_axes = { 1, 2 };
	            usz[2] right_axes = { 0, 1 };
	            usz[2] result_shape = { 2, 3 };
	            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
	                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
	                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
	                plus_func != null && zero != null && zero.tag != ERROR;
	            Value* lazy_map = setup_ok ? make_tensor_map_expr(
	                plus_func,
	                left_eval.value,
	                left_eval.value.tensor_val,
	                zero,
	                null,
	                true,
	                TENSOR_DTYPE_DOUBLE,
	                left_shape[0:3],
	                interp
	            ) : null;
	            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
	                lazy_map,
	                right_eval.value,
	                &left_axes[0],
	                &right_axes[0],
	                2,
	                result_shape[0:2],
	                interp
	            ) : null;
	            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
	            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
	            ok = setup_ok &&
	                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
	                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
	                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
	                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
	                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
	                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
	                cpu_value.tensor_val.data != null &&
	                ((double*)cpu_value.tensor_val.data)[5] == 210.0;
	        }
	        if (ok) {
	            emit_pass_literal(vulkan_available
	                ? "[PASS] tensor lazy Vulkan contract helper realizes multi-axis mapped operand"
	                : "[PASS] tensor lazy Vulkan contract helper unavailable; multi-axis mapped operand not exercised");
	            *pass += 1;
	        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes multi-axis mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
        if (vulkan_available) {
            EvalResult left_eval = run("(to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)", interp);
            EvalResult right_eval = run("(Tensor Float64 [3] [10 20 30])", interp);
            SymbolId plus_sym = interp.symbols.intern("+");
            Value* plus_func = interp.global_env.lookup(plus_sym);
            Value* zero = make_double(interp, 0.0);
            usz[1] vector_shape = { 3 };
            usz[1] left_axes = { 0 };
            usz[1] right_axes = { 0 };
            usz[1] scalar_shape = { 0 };
            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
                plus_func != null && zero != null && zero.tag != ERROR;
            Value* lazy_map = setup_ok ? make_tensor_map_expr(
                plus_func,
                left_eval.value,
                left_eval.value.tensor_val,
                zero,
                null,
                true,
                TENSOR_DTYPE_DOUBLE,
                vector_shape[0:1],
                interp
            ) : null;
            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
                lazy_map,
                right_eval.value,
                &left_axes[0],
                &right_axes[0],
                1,
                scalar_shape[0:0],
                interp
            ) : null;
            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
            ok = setup_ok &&
                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
                realized != null && realized.tag == ERROR &&
                str_contains(realized.str_chars[:realized.str_len], "contract: Vulkan operands must both be Vulkan-placed");
        }
        if (ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor lazy Vulkan contract helper rejects mixed CPU operand"
                : "[PASS] tensor lazy Vulkan contract helper unavailable; mixed CPU operand not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor lazy Vulkan contract helper rejects mixed CPU operand");
            *fail += 1;
        }
    }
    test_truthy(interp, "contract vulkan rank-2 axis [1 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0]) 'cpu) [1 1]) 154.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 4] [1 2 3 4 5 6 7 8]) 'vulkan) [0 0]) 'cpu) [2 3]) 60.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [1 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [1 1]) 'cpu) [1 3]) 167.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [0 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [0 1]) 'cpu) [2 3]) 69.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-1 axis [1 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3] [7 8 9]) 'vulkan) [1 0]) 'cpu) [1]) 122.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-1 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2] [7 8]) 'vulkan) [0 0]) 'cpu) [2]) 69.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 rank-2 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'vulkan) [0 0]) 'cpu) [1]) 46.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 rank-2 axis [0 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2] [2 3]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'vulkan) [0 1]) 'cpu) [2]) 43.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 rank-2 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [2 0]) 'cpu) [1 2 1]) 460.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-3 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 3 2] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [1 1]) 'cpu) [1 1 0]) 139.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 rank-3 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [5 4 2] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40]) 'vulkan) [2 1]) 'cpu) [1 2 4 1]) 3340.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [2 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-1 outer product roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu) [1 2]) 60.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-2 rank-1 outer product roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu) [1 1 2]) 120.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-0 scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [] 3.0) 'vulkan) (to-device (Tensor Float64 [] 4.0) 'vulkan) [] []) 'cpu) []) 12.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis result remains vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] [])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis zero-free output preserves zero length",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-size rank-1 dot returns additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan) [0 0]) 'cpu) []) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-2 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) [1 0]) 'cpu) [1 2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-free rank-2 preserves zero-length output",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (contract (to-device (Tensor Float64 [0 2] 0.0) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) [1 0]) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-free result remains vulkan tensor",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [0 2] 0.0) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) [1 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-2 rank-1 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan) [1 0]) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-1 rank-2 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) [0 0]) 'cpu) [2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-3 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0 3] 0.0) 'vulkan) (to-device (Tensor Float64 [0 4] 0.0) 'vulkan) [1 0]) 'cpu) [1 2 3]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rejects mixed devices",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (Tensor Float64 [3] [10 20 30]) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "to-device vulkan zero-size follows backend availability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (device (to-device (Tensor Float64 [0] []) 'vulkan)) 'vulkan) (= (handle (to-device (Tensor Float64 [0] []) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device vulkan rejects unsupported dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (handle (to-device (Tensor BigInteger [1] [(BigInteger \"1\")]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_eq_double(interp, "to-device cuda roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (ref (to-device (to-device (Tensor Float64 [2] [1 2]) 'cuda) 'cpu) [1]) 2.0))", 2.0, pass, fail);
    test_truthy(interp, "to-device cuda Float32 roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (ref (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 'cpu) [1]) (Float32 2.5)) true))", pass, fail);
    test_str_val(interp, "to-device cuda Float32 copyback preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (String (dtype (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 'cpu))) \"Float32\"))", "Float32", pass, fail);
    test_truthy(interp, "map cuda add scalar roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (and (= (device (map + (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) 5.0)) 'cuda) (= (ref (to-device (map + (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) 5.0) 'cpu) [2]) 8.0)) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 add scalar preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (out (map + (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 0.5)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1]) (Float32 3.0)))) true))", pass, fail);
    test_truthy(interp, "map cuda binary arithmetic ops roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (a (to-device (Tensor Float64 [3] [6 8 10]) 'cuda) b (to-device (Tensor Float64 [3] [1 2 5]) 'cuda)) (and (= (ref (to-device (map - a b) 'cpu) [1]) 6.0) (= (ref (to-device (map * a b) 'cpu) [2]) 50.0) (= (ref (to-device (map / a b) 'cpu) [0]) 6.0) (= (ref (to-device (map min a b) 'cpu) [0]) 1.0) (= (ref (to-device (map max a b) 'cpu) [2]) 10.0) (= (device (map + a b)) 'cuda))) true))", pass, fail);
    {
        char[] name = "map cuda unsupported callable fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (to-device (Tensor Float64 [1] [1.2]) 'cuda))",
                "map: CUDA currently supports Float64 and Float32 arithmetic and scientific unary kernels", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda mixed cpu operand fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [1] [1.0]) 'cuda) (Tensor Float64 [1] [1.0]))",
                "map: CUDA operands must remain CUDA-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "map cuda tensor broadcast roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (out (map + (to-device (Tensor Float64 [2 1] [1 2]) 'cuda) (to-device (Tensor Float64 [1 3] [10 20 30]) 'cuda))) (and (= (device out) 'cuda) (= (ref (to-device out 'cpu) [1 2]) 32.0))) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 tensor broadcast preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (out (map + (to-device (Tensor Float32 [2 1] [1.25 2.5]) 'cuda) (to-device (Tensor Float32 [1 2] [10.0 20.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 22.5)))) true))", pass, fail);
    {
        char[] name = "map cuda incompatible tensor broadcast fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda))",
                "map: tensor shape mismatch", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_eq_double(interp, "contract cuda cublas rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) 'cpu) [1 1]) 154.0))", 154.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) 'cpu) [1 1]) (Float32 154.0)) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 preserves device and dtype when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (r (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\"))) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [2 4] [1 2 3 4 5 6 7 8]) 'cuda) [0 0]) 'cpu) [2 3]) 60.0))", 60.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [2 4] [1 2 3 4 5 6 7 8]) 'cuda) [0 0]) 'cpu) [2 3]) (Float32 60.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [1 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'cuda) [1 1]) 'cpu) [1 3]) 167.0))", 167.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [1 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'cuda) [1 1]) 'cpu) [1 3]) (Float32 167.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'cuda) [0 1]) 'cpu) [2 3]) 69.0))", 69.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [4 2] [1 2 3 4 5 6 7 8]) 'cuda) [0 1]) 'cpu) [2 3]) (Float32 69.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 rank-1 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3] [7 8 9]) 'cuda) [1 0]) 'cpu) [1]) 122.0))", 122.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 rank-1 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3] [7 8 9]) 'cuda) [1 0]) 'cpu) [1]) (Float32 122.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 rank-1 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [2] [7 8]) 'cuda) [0 0]) 'cpu) [2]) 69.0))", 69.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 rank-1 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [2] [7 8]) 'cuda) [0 0]) 'cpu) [2]) (Float32 69.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-1 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'cuda) [0 0]) 'cpu) [1]) 46.0))", 46.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [3] [1 2 3]) 'cuda) (to-device (Tensor Float32 [3 2] [4 5 6 7 8 9]) 'cuda) [0 0]) 'cpu) [1]) (Float32 46.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-1 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2] [2 3]) 'cuda) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'cuda) [0 1]) 'cpu) [2]) 43.0))", 43.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2] [2 3]) 'cuda) (to-device (Tensor Float32 [3 2] [4 5 6 7 8 9]) 'cuda) [0 1]) 'cpu) [2]) (Float32 43.0)) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas rank-1 dot when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (let (r (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) []) 140.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas rank-1 dot explicit axes when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0] [0]) 'cpu) []) 140.0) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 dot when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (r (contract (to-device (Tensor Float32 [3] [1 2 3]) 'cuda) (to-device (Tensor Float32 [3] [10 20 30]) 'cuda) [0 0])) (let (cpu (to-device r 'cpu)) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\") (= (String (type-of (ref cpu []))) \"Float32\") (= (ref cpu []) (Float32 140.0))))) true))", pass, fail);
    omni_tensor_backend_cublas_disable_for_tests(1);
    test_truthy(interp, "tensor-backends cublas disabled reports unavailable",
        "(= (ref (ref (tensor-backends) 2) 'available) false)", pass, fail);
    test_truthy(interp, "contract cuda cublas disabled fails unavailable",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) (raise msg (ref msg 'code))) 'tensor/backend-unavailable) true))", pass, fail);
    test_truthy(interp, "contract cuda rank-1 dot cublas disabled fails unavailable",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unavailable) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted dimension succeeds with cublas disabled",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0 3] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) [1 2]) 0.0))) true))", pass, fail);
    omni_tensor_backend_cublas_disable_for_tests(0);
    test_truthy(interp, "contract cuda rejects mixed devices",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [1 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "contract cuda rejects mixed Float32 Float64 dtypes",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) true))", pass, fail);
    test_truthy(interp, "contract cuda rejects unsupported multi-axis contraction",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'cuda) (to-device (Tensor Float64 [2 2] [5 6 7 8]) 'cuda) [[0 0] [1 1]]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 fills Float64 additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0 3] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 3) (= (ref (to-device r 'cpu) [1 2]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 fills Float32 additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float32 [2 0] []) 'cuda) (to-device (Tensor Float32 [0 3] []) 'cuda) [1 0])) (let (cpu (to-device r 'cpu)) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\") (= (String (type-of (ref cpu [1 2]))) \"Float32\") (= (ref cpu [1 2]) (Float32 0.0))))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero-free rank-2 preserves empty Float64 output",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0 2] []) 'cuda) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 0) (= (ref (shape r) 1) 3) (= (length (to-device r 'cpu)) 0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 rank-1 fills additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (to-device r 'cpu) [1]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-1 rank-2 fills additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0] []) 'cuda) (to-device (Tensor Float64 [0 2] []) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (to-device r 'cpu) [1]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero-size rank-1 dot returns additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0] []) 'cuda) (to-device (Tensor Float64 [0] []) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) []) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract vulkan multi-axis scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) (to-device (Tensor Float64 [2 2] [5 6 7 8]) 'vulkan) [[0 0] [1 1]]) 'cpu) []) 70.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 multi-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) 'vulkan) (to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [[1 0] [2 1]]) 'cpu) [1 2]) 210.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 rank-1 dot roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 rank-2 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0]) 'cpu) [1 1]) 154.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 preserves dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (String (dtype (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0]))) \"Float32\") true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 rank-3 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float32 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [2 0]) 'cpu) [1 2 1]) 460.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 multi-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 2 2] [1 2 3 4 5 6 7 8]) 'vulkan) (to-device (Tensor Float32 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [[1 0] [2 1]]) 'cpu) [1 2]) 210.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 zero-axis outer product roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2] [1 2]) 'vulkan) (to-device (Tensor Float32 [3] [10 20 30]) 'vulkan) [] []) 'cpu) [1 2]) 60.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 zero-axis scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [] 3.0) 'vulkan) (to-device (Tensor Float32 [] 4.0) 'vulkan) [] []) 'cpu) []) 12.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 zero-axis zero-free output preserves zero length",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (length (to-device (contract (to-device (Tensor Float32 [0] 0.0) 'vulkan) (to-device (Tensor Float32 [3] [10 20 30]) 'vulkan) [] []) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 zero-size dot returns additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [0] 0.0) 'vulkan) (to-device (Tensor Float32 [0] 0.0) 'vulkan) [0 0]) 'cpu) []) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 zero contracted rank-2 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 0] 0.0) 'vulkan) (to-device (Tensor Float32 [0 3] 0.0) 'vulkan) [1 0]) 'cpu) [1 2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 rejects mixed devices",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (contract (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (Tensor Float32 [3] [10 20 30]) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan Float32 rejects mixed dtype without promotion",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (and (ref vulkan-backend 'float32) (ref vulkan-backend 'float64)) (= (handle (contract (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) true) true))", pass, fail);
    test_error_contains(interp, "to-device rejects non-symbol device",
        "(to-device (Tensor Float64 [1] [2]) \"cpu\")", "device target must be a symbol", pass, fail);
    test_error_contains(interp, "device rejects non-tensor",
        "(device 1)", "device: expected Tensor", pass, fail);
    test_eq_double(interp, "Tensor inferred BigInteger narrows to Float64",
        "(ref (Tensor [(BigInteger \"9223372036854775808\")]) [0])",
        9223372036854775808.0, pass, fail);
    test_eq_double(interp, "Tensor inferred BigFloat narrows to Float64",
        "(ref (Tensor [(BigFloat \"1.25\")]) [0])", 1.25, pass, fail);
    test_eq_double(interp, "Tensor explicit BigFloat scalar fill narrows",
        "(ref (Tensor Float64 [2] (BigFloat \"2.5\")) [1])", 2.5, pass, fail);
    test_eq_double(interp, "Tensor explicit BigInteger data narrows",
        "(ref (Tensor Float64 [1] [(BigInteger \"42\")]) [0])", 42.0, pass, fail);
    test_eq(interp, "Tensor inferred empty vector length",
        "(length (Tensor []))", 0, pass, fail);
    test_error_contains(interp, "Tensor inferred BigFloat outside Float64 range rejected",
        "(Tensor [(BigFloat \"1e309\")])", "representable as Float64", pass, fail);
    test_error_contains(interp, "Tensor inferred complex data rejected",
        "(Tensor [(BigComplex 1 2)])", "representable as Float64", pass, fail);
    test_error_contains(interp, "Tensor inferred ragged data rejected",
        "(Tensor [[1] [2 3]])", "rectangular", pass, fail);
    test_error_contains(interp, "Tensor inferred non-numeric data rejected",
        "(Tensor [1 'bad])", "nested arrays/lists of numbers", pass, fail);
    test_str_val(interp, "Tensor BigInteger dtype",
        "(format \"%s\" (dtype (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])))", "BigInteger", pass, fail);
    test_str_val(interp, "Tensor BigInteger explicit ref preserves exact range",
        "(String (ref (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) [0]))", "9223372036854775808", pass, fail);
    test_str_val(interp, "Tensor BigInteger inferred suffix ref",
        "(String (ref (Tensor [(BigInteger \"9223372036854775808\")] BigInteger) [0]))", "9223372036854775808", pass, fail);
    test_str_val(interp, "Tensor BigInteger inferred prefix ref",
        "(String (ref (Tensor BigInteger [(BigInteger \"9223372036854775809\")]) [0]))", "9223372036854775809", pass, fail);
    test_str_val(interp, "Array BigInteger tensor conversion preserves element",
        "(String (ref (Array (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) 0))", "9223372036854775808", pass, fail);
    test_str_val(interp, "List BigInteger tensor conversion preserves element",
        "(String (ref (List (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) 1))", "2", pass, fail);
    test_str_val(interp, "realize BigInteger scalar fill into tensor",
        "(let (dst (Tensor BigInteger [2] 0)) (block (realize (BigInteger \"3\") dst) (String (ref dst [1]))))",
        "3", pass, fail);
    test_str_val(interp, "realize BigInteger tensor copy",
        "(let (src (Tensor BigInteger [1] [(BigInteger \"7\")]) dst (Tensor BigInteger [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7", pass, fail);
    test_error_contains(interp, "Tensor BigInteger rejects inexact data",
        "(Tensor BigInteger [1] [1.5])", "exact integers", pass, fail);
    test_str_val(interp, "Tensor BigFloat dtype",
        "(format \"%s\" (dtype (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])))", "BigFloat", pass, fail);
    test_str_val(interp, "Tensor BigFloat explicit ref preserves precision range",
        "(String (ref (Tensor BigFloat [1] [(BigFloat \"1e309\")]) [0]))", "1e+309", pass, fail);
    test_str_val(interp, "Tensor BigFloat inferred suffix ref",
        "(String (ref (Tensor [(BigFloat \"1.25\")] BigFloat) [0]))", "1.25", pass, fail);
    test_str_val(interp, "Tensor BigFloat inferred prefix ref",
        "(String (ref (Tensor BigFloat [(BigFloat \"2.5\")]) [0]))", "2.5", pass, fail);
    test_str_val(interp, "Array BigFloat tensor conversion preserves element",
        "(String (ref (Array (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) 1))", "2", pass, fail);
    test_str_val(interp, "List BigFloat tensor conversion preserves element",
        "(String (ref (List (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) 0))", "1.25", pass, fail);
    test_str_val(interp, "realize BigFloat scalar fill into tensor",
        "(let (dst (Tensor BigFloat [2] 0)) (block (realize (BigFloat \"3.5\") dst) (String (ref dst [1]))))",
        "3.5", pass, fail);
    test_str_val(interp, "realize BigFloat tensor copy",
        "(let (src (Tensor BigFloat [1] [(BigFloat \"7.25\")]) dst (Tensor BigFloat [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7.25", pass, fail);
    test_str_val(interp, "map BigFloat tensor scalar preserves dtype",
        "(String (ref (map + (Tensor BigFloat [2] [(BigFloat \"1.25\") 2]) (BigFloat \"2\")) [0]))",
        "3.25", pass, fail);
    test_str_val(interp, "Tensor BigComplex dtype",
        "(format \"%s\" (dtype (Tensor BigComplex [2] [(BigComplex 1 2) 3])))", "BigComplex", pass, fail);
    test_str_val(interp, "Tensor BigComplex explicit ref preserves complex value",
        "(String (ref (Tensor BigComplex [1] [(BigComplex 1 2)]) [0]))", "1+2i", pass, fail);
    test_str_val(interp, "Tensor BigComplex inferred suffix ref",
        "(String (ref (Tensor [(BigComplex 1 2)] BigComplex) [0]))", "1+2i", pass, fail);
    test_str_val(interp, "Tensor BigComplex inferred prefix ref",
        "(String (ref (Tensor BigComplex [(BigComplex 3 -4)]) [0]))", "3-4i", pass, fail);
    test_str_val(interp, "Array BigComplex tensor conversion preserves element",
        "(String (ref (Array (Tensor BigComplex [2] [(BigComplex 1 2) 3])) 0))", "1+2i", pass, fail);
    test_str_val(interp, "List BigComplex tensor conversion preserves element",
        "(String (ref (List (Tensor BigComplex [2] [(BigComplex 1 2) 3])) 1))", "3+0i", pass, fail);
    test_str_val(interp, "realize BigComplex scalar fill into tensor",
        "(let (dst (Tensor BigComplex [2] 0)) (block (realize (BigComplex 3 4) dst) (String (ref dst [1]))))",
        "3+4i", pass, fail);
    test_str_val(interp, "realize BigComplex tensor copy",
        "(let (src (Tensor BigComplex [1] [(BigComplex 7 8)]) dst (Tensor BigComplex [1] 0)) (block (realize src dst) (String (ref dst [0]))))",
        "7+8i", pass, fail);
    test_str_val(interp, "real-part BigComplex tensor returns BigFloat tensor",
        "(String (ref (real-part (Tensor BigComplex [2] [(BigComplex 1 2) (BigComplex 3 4)])) [1]))",
        "3", pass, fail);
    test_str_val(interp, "imag-part BigComplex tensor returns BigFloat tensor",
        "(String (ref (imag-part (Tensor BigComplex [2] [(BigComplex 1 2) (BigComplex 3 4)])) [0]))",
        "2", pass, fail);
    test_str_val(interp, "conjugate BigComplex tensor flips imaginary part",
        "(String (ref (conjugate (Tensor BigComplex [1] [(BigComplex 1 2)])) [0]))",
        "1-2i", pass, fail);
    test_str_val(interp, "real-part lazy BigComplex tensor realizes source",
        "(String (ref (real-part (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 3 4))) [0]))",
        "4", pass, fail);
    test_str_val(interp, "conjugate lazy BigComplex tensor realizes source",
        "(String (ref (conjugate (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 3 4))) [0]))",
        "4-6i", pass, fail);
    test_str_val(interp, "Tensor Complex128 dtype",
        "(String (dtype (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)])))",
        "Complex128", pass, fail);
    test_str_val(interp, "Tensor Complex128 explicit ref preserves value",
        "(String (ref (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)]) [1]))",
        "3.0+4.0i", pass, fail);
    test_str_val(interp, "Tensor infers Complex128 dtype from leaves",
        "(String (dtype (Tensor [(Complex128 1 2) (Complex64 3 4)])))",
        "Complex128", pass, fail);
    test_str_val(interp, "Tensor infers Complex64 dtype from leaves",
        "(String (dtype (Tensor [(Complex64 1 2) (Complex64 3 4)])))",
        "Complex64", pass, fail);
    test_str_val(interp, "Tensor Complex64 dtype",
        "(String (dtype (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)])))",
        "Complex64", pass, fail);
    test_str_val(interp, "Tensor Complex64 explicit ref preserves value",
        "(String (ref (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)]) [0]))",
        "1.0+2.0i", pass, fail);
    test_str_val(interp, "Array Complex128 tensor conversion preserves element",
        "(String (ref (Array (Tensor Complex128 [2] [(Complex128 1 2) (Complex128 3 4)])) 1))",
        "3.0+4.0i", pass, fail);
    test_str_val(interp, "List Complex64 tensor conversion preserves element",
        "(String (ref (List (Tensor Complex64 [2] [(Complex64 1 2) (Complex64 3 4)])) 1))",
        "3.0+4.0i", pass, fail);
    test_str_val(interp, "realize Complex128 scalar fill into tensor",
        "(let (dst (Tensor Complex128 [2] 0)) (block (realize (Complex128 3 4) dst) (String (ref dst [1]))))",
        "3.0+4.0i", pass, fail);
    test_str_val(interp, "real-part Complex128 tensor returns Float64 tensor",
        "(String (dtype (real-part (Tensor Complex128 [1] [(Complex128 3 4)]))))",
        "Float64", pass, fail);
    test_eq_double(interp, "imag-part Complex128 tensor extracts imaginary component",
        "(ref (imag-part (Tensor Complex128 [1] [(Complex128 3 4)])) [0])", 4.0, pass, fail);
    test_str_val(interp, "real-part Complex64 tensor returns Float32 tensor",
        "(String (dtype (real-part (Tensor Complex64 [1] [(Complex64 3 4)]))))",
        "Float32", pass, fail);
    test_str_val(interp, "conjugate Complex64 tensor flips imaginary part",
        "(String (ref (conjugate (Tensor Complex64 [1] [(Complex64 3 -4)])) [0]))",
        "3.0+4.0i", pass, fail);
    test_eq_double(interp, "abs Complex128 tensor returns Float64 magnitude",
        "(ref (abs (Tensor Complex128 [1] [(Complex128 3 4)])) [0])", 5.0, pass, fail);
    test_str_val(interp, "unary minus Complex64 tensor preserves dtype",
        "(String (ref (- (Tensor Complex64 [1] [(Complex64 3 -4)])) [0]))",
        "-3.0+4.0i", pass, fail);
    test_truthy(interp, "to-device CUDA Complex128 placement follows storage capability",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'complex128) (= (device (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda)) 'cuda) (= (handle (to-device (Tensor Complex128 [1] [(Complex128 1 2)]) 'cuda) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device Vulkan Complex64 placement follows storage capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'complex64) (= (device (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan)) 'vulkan) (= (handle (to-device (Tensor Complex64 [1] [(Complex64 1 2)]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_eq_double(interp, "real-part Float64 tensor preserves dtype and value",
        "(ref (real-part (Tensor Float64 [1] [1.5])) [0])", 1.5, pass, fail);
    test_eq_double(interp, "imag-part Float64 tensor returns zero tensor",
        "(ref (imag-part (Tensor Float64 [1] [1.5])) [0])", 0.0, pass, fail);
    test_str_val(interp, "conjugate BigInteger tensor preserves exact value",
        "(String (ref (conjugate (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "imag-part BigFloat tensor returns zero tensor",
        "(String (ref (imag-part (Tensor BigFloat [1] [(BigFloat \"1.25\")])) [0]))",
        "0", pass, fail);
    test_eq_double(interp, "abs Float64 tensor preserves dtype and magnitude",
        "(ref (abs (Tensor Float64 [2] [-1.5 2.5])) [0])", 1.5, pass, fail);
    test_eq_double(interp, "unary minus Float64 tensor preserves dtype",
        "(ref (- (Tensor Float64 [2] [1.5 -2.5])) [1])", 2.5, pass, fail);
    test_str_val(interp, "abs BigInteger tensor preserves exact dtype",
        "(String (ref (abs (Tensor BigInteger [1] [(BigInteger \"-9223372036854775808\")])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "unary minus BigInteger tensor preserves exact dtype",
        "(String (ref (- (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")])) [0]))",
        "-9223372036854775808", pass, fail);
    test_str_val(interp, "abs BigFloat tensor preserves high precision dtype",
        "(String (ref (abs (Tensor BigFloat [1] [(BigFloat \"-1e309\")])) [0]))",
        "1e+309", pass, fail);
    test_str_val(interp, "unary minus BigFloat tensor preserves high precision dtype",
        "(String (ref (- (Tensor BigFloat [1] [(BigFloat \"1e309\")])) [0]))",
        "-1e+309", pass, fail);
    test_str_val(interp, "abs BigComplex tensor returns BigFloat magnitude tensor",
        "(String (ref (abs (Tensor BigComplex [1] [(BigComplex 3 4)])) [0]))",
        "5", pass, fail);
    test_str_val(interp, "unary minus BigComplex tensor preserves complex dtype",
        "(String (ref (- (Tensor BigComplex [1] [(BigComplex 3 -4)])) [0]))",
        "-3+4i", pass, fail);
    test_str_val(interp, "abs lazy BigComplex tensor realizes source",
        "(String (ref (abs (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 2))) [0]))",
        "5", pass, fail);
    test_eq_double(interp, "sqrt Float64 tensor preserves Float64 dtype",
        "(ref (sqrt (Tensor Float64 [1] [9.0])) [0])", 3.0, pass, fail);
    test_eq_double(interp, "sqrt BigInteger tensor returns Float64 tensor",
        "(ref (sqrt (Tensor BigInteger [1] [9])) [0])", 3.0, pass, fail);
    test_str_val(interp, "sqrt BigFloat tensor preserves high precision dtype",
        "(String (ref (sqrt (Tensor BigFloat [1] [(BigFloat \"4\")])) [0]))",
        "2", pass, fail);
    test_str_val(interp, "sqrt BigComplex tensor preserves complex dtype",
        "(String (ref (sqrt (Tensor BigComplex [1] [(BigComplex -1 0)])) [0]))",
        "0+1i", pass, fail);
    test_str_val(interp, "sqrt lazy BigComplex tensor realizes source",
        "(String (ref (sqrt (map + (Tensor BigComplex [1] [(BigComplex -2 0)]) (BigComplex 1 0))) [0]))",
        "0+1i", pass, fail);
    test_error_contains(interp, "sqrt BigInteger tensor rejects out of Float64 range",
        "(sqrt (Tensor BigInteger [1] [(BigInteger \"100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\")]))",
        "math: BigInteger tensor element out of Float64 range", pass, fail);
    test_eq_double(interp, "sin Float64 tensor",
        "(ref (sin (Tensor Float64 [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "cos BigInteger tensor returns Float64 tensor",
        "(ref (cos (Tensor BigInteger [1] [0])) [0])", 1.0, pass, fail);
    test_eq_double(interp, "tan Float64 tensor",
        "(ref (tan (Tensor Float64 [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "asin Float64 tensor",
        "(ref (asin (Tensor Float64 [1] [0.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "acos Float64 tensor",
        "(ref (acos (Tensor Float64 [1] [1.0])) [0])", 0.0, pass, fail);
    test_eq_double(interp, "atan Float64 tensor",
        "(ref (atan (Tensor Float64 [1] [0.0])) [0])", 0.0, pass, fail);
    test_str_val(interp, "sinh BigFloat tensor preserves dtype",
        "(String (ref (sinh (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "0", pass, fail);
    test_str_val(interp, "cosh BigFloat tensor preserves dtype",
        "(String (ref (cosh (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "1", pass, fail);
    test_str_val(interp, "tanh BigComplex tensor preserves dtype",
        "(String (ref (tanh (Tensor BigComplex [1] [(BigComplex 0 0)])) [0]))",
        "0+0i", pass, fail);
    test_eq_double(interp, "exp BigInteger tensor returns Float64 tensor",
        "(ref (exp (Tensor BigInteger [1] [0])) [0])", 1.0, pass, fail);
    test_str_val(interp, "log BigFloat tensor preserves dtype",
        "(String (ref (log (Tensor BigFloat [1] [(BigFloat \"1\")])) [0]))",
        "0", pass, fail);
    test_str_val(interp, "log10 lazy BigComplex tensor realizes source",
        "(String (ref (log10 (map + (Tensor BigComplex [1] [(BigComplex 0 0)]) (BigComplex 1 0))) [0]))",
        "0+0i", pass, fail);
    test_truthy(interp, "math/erf Float64 tensor",
        "(< (abs (- (ref (math/erf (Tensor Float64 [1] [1.0])) [0]) 0.842700792949715)) 0.000001)", pass, fail);
    test_truthy(interp, "math/erfc Float32 tensor preserves dtype",
        "(let (out (math/erfc (Tensor Float32 [1] [1.0]))) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out [0]) 0.15729921)) 0.0001)))", pass, fail);
    test_eq_double(interp, "math/erf BigInteger tensor returns Float64 tensor",
        "(ref (math/erf (Tensor BigInteger [1] [0])) [0])", 0.0, pass, fail);
    test_str_val(interp, "math/erfc BigFloat tensor preserves dtype",
        "(String (ref (math/erfc (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "1", pass, fail);
    test_error_contains(interp, "math/erf BigComplex tensor rejected",
        "(math/erf (Tensor BigComplex [1] [(BigComplex 1 0)]))",
        "math: tensor dtype is not supported for this operation", pass, fail);
    test_truthy(interp, "stats/normal-cdf Float64 tensor",
        "(< (abs (- (ref (stats/normal-cdf (Tensor Float64 [1] [1.0])) [0]) 0.841344746068543)) 0.000001)", pass, fail);
    test_truthy(interp, "stats/normal-cdf Float32 tensor preserves dtype",
        "(let (out (stats/normal-cdf (Tensor Float32 [1] [0.0]))) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out [0]) 0.5)) 0.0001)))", pass, fail);
    test_eq_double(interp, "stats/normal-cdf BigInteger tensor returns Float64 tensor",
        "(ref (stats/normal-cdf (Tensor BigInteger [1] [0])) [0])", 0.5, pass, fail);
    test_str_val(interp, "stats/normal-cdf BigFloat tensor preserves dtype",
        "(String (ref (stats/normal-cdf (Tensor BigFloat [1] [(BigFloat \"0\")])) [0]))",
        "0.5", pass, fail);
    test_error_contains(interp, "stats/normal-cdf BigComplex tensor rejected",
        "(stats/normal-cdf (Tensor BigComplex [1] [(BigComplex 1 0)]))",
        "math: tensor dtype is not supported for this operation", pass, fail);
    test_eq_double(interp, "stats/normal-cdf lazy Float64 tensor realizes source",
        "(ref (stats/normal-cdf (map + (Tensor Float64 [1] [0.0]) 0.0)) [0])", 0.5, pass, fail);
    test_truthy(interp, "stats/normal-quantile Float64 tensor",
        "(< (abs (- (ref (stats/normal-quantile (Tensor Float64 [1] [0.975])) [0]) 1.95996398454005)) 0.000001)", pass, fail);
    test_truthy(interp, "stats/normal-quantile Float32 tensor preserves dtype",
        "(let (out (stats/normal-quantile (Tensor Float32 [1] [0.5]))) (and (= (String (dtype out)) \"Float32\") (< (abs (ref out [0])) 0.0001)))", pass, fail);
    test_error_contains(interp, "stats/normal-quantile Float64 tensor rejects low probability",
        "(stats/normal-quantile (Tensor Float64 [1] [0.0]))",
        "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
    test_error_contains(interp, "stats/normal-quantile Float64 tensor rejects high probability",
        "(stats/normal-quantile (Tensor Float64 [1] [1.0]))",
        "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
    test_error_contains(interp, "stats/normal-quantile BigInteger tensor rejects integer probability",
        "(stats/normal-quantile (Tensor BigInteger [1] [0]))",
        "stats/normal-quantile: probability must be between 0 and 1", pass, fail);
    test_str_val(interp, "stats/normal-quantile BigFloat tensor preserves dtype",
        "(String (ref (stats/normal-quantile (Tensor BigFloat [1] [(BigFloat \"0.5\")])) [0]))",
        "0", pass, fail);
    test_error_contains(interp, "stats/normal-quantile BigComplex tensor rejected",
        "(stats/normal-quantile (Tensor BigComplex [1] [(BigComplex 1 0)]))",
        "math: tensor dtype is not supported for this operation", pass, fail);
    test_truthy(interp, "stats/normal-quantile lazy Float64 tensor realizes source",
        "(< (abs (ref (stats/normal-quantile (map + (Tensor Float64 [1] [0.5]) 0.0)) [0])) 0.000001)", pass, fail);
    test_eq_double(interp, "pow Float64 tensor scalar",
        "(ref (pow (Tensor Float64 [1] [2.0]) 3.0) [0])", 8.0, pass, fail);
    test_eq_double(interp, "pow scalar Float64 tensor",
        "(ref (pow 2.0 (Tensor Float64 [1] [3.0])) [0])", 8.0, pass, fail);
    test_eq_double(interp, "pow BigInteger tensor returns Float64 tensor",
        "(ref (pow (Tensor BigInteger [1] [2]) 3) [0])", 8.0, pass, fail);
    test_eq_double(interp, "pow Tensor broadcast",
        "(ref (pow (Tensor Float64 [2 1] [2 3]) (Tensor Float64 [1 2] [2 3])) [1 1])", 27.0, pass, fail);
    test_str_val(interp, "pow BigFloat tensor preserves dtype",
        "(String (ref (pow (Tensor BigFloat [1] [(BigFloat \"2\")]) (BigFloat \"3\")) [0]))",
        "8", pass, fail);
    test_str_val(interp, "pow BigComplex tensor preserves dtype",
        "(String (< (abs (- (ref (pow (Tensor BigComplex [1] [(BigComplex 0 1)]) 2) [0]) (BigComplex -1 0))) (BigFloat \"1e-40\")))",
        "true", pass, fail);
    test_str_val(interp, "pow lazy BigComplex tensor realizes source",
        "(String (< (abs (- (ref (pow (map + (Tensor BigComplex [1] [(BigComplex 0 1)]) (BigComplex 0 0)) 2) [0]) (BigComplex -1 0))) (BigFloat \"1e-40\")))",
        "true", pass, fail);
    test_error_contains(interp, "pow BigInteger tensor rejects out of Float64 range",
        "(pow (Tensor BigInteger [1] [(BigInteger \"100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\")]) 2)",
        "pow: tensor element out of Float64 range", pass, fail);
    test_str_val(interp, "atan2 Float64 tensor scalar",
        "(String (ref (atan2 (Tensor Float64 [1] [1.0]) 1.0) [0]))", "0.785398163397448", pass, fail);
    test_str_val(interp, "atan2 scalar Float64 tensor",
        "(String (ref (atan2 1.0 (Tensor Float64 [1] [1.0])) [0]))", "0.785398163397448", pass, fail);
    test_eq_double(interp, "atan2 BigInteger tensor returns Float64 tensor",
        "(ref (atan2 (Tensor BigInteger [1] [0]) 1) [0])", 0.0, pass, fail);
    test_str_val(interp, "atan2 Tensor broadcast",
        "(String (ref (atan2 (Tensor Float64 [2 1] [0 1]) (Tensor Float64 [1 2] [1 1])) [1 1]))", "0.785398163397448", pass, fail);
    test_str_val(interp, "atan2 BigFloat tensor preserves dtype",
        "(String (ref (atan2 (Tensor BigFloat [1] [(BigFloat \"0\")]) 1) [0]))",
        "0", pass, fail);
    test_str_val(interp, "atan2 lazy BigFloat tensor realizes source",
        "(String (ref (atan2 (map + (Tensor BigFloat [1] [(BigFloat \"0\")]) (BigFloat \"0\")) 1) [0]))",
        "0", pass, fail);
    test_error_contains(interp, "atan2 BigComplex tensor rejected",
        "(atan2 (Tensor BigComplex [1] [(BigComplex 1 0)]) 1)",
        "atan2: complex numbers are not supported", pass, fail);
    test_error_contains(interp, "atan2 BigInteger tensor rejects out of Float64 range",
        "(atan2 (Tensor BigInteger [1] [(BigInteger \"100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\")]) 1)",
        "atan2: tensor element out of Float64 range", pass, fail);
    test_str_val(interp, "floor Float64 tensor returns BigInteger tensor",
        "(String (ref (floor (Tensor Float64 [1] [3.7])) [0]))", "3", pass, fail);
    test_str_val(interp, "ceiling Float64 tensor returns BigInteger tensor",
        "(String (ref (ceiling (Tensor Float64 [1] [3.2])) [0]))", "4", pass, fail);
    test_str_val(interp, "round Float64 tensor returns BigInteger tensor",
        "(String (ref (round (Tensor Float64 [1] [3.5])) [0]))", "4", pass, fail);
    test_str_val(interp, "truncate Float64 tensor returns BigInteger tensor",
        "(String (ref (truncate (Tensor Float64 [1] [3.9])) [0]))", "3", pass, fail);
    test_str_val(interp, "rounding dtype is BigInteger",
        "(String (dtype (floor (Tensor Float64 [1] [3.7]))))", "BigInteger", pass, fail);
    test_str_val(interp, "floor BigInteger tensor clones exact values",
        "(String (ref (floor (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")])) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "ceiling BigFloat tensor promotes large exact integer",
        "(String (ref (ceiling (Tensor BigFloat [1] [(BigFloat \"9223372036854775808.1\")])) [0]))",
        "9223372036854775809", pass, fail);
    test_str_val(interp, "round lazy BigFloat tensor realizes source",
        "(String (ref (round (map + (Tensor BigFloat [1] [(BigFloat \"3.5\")]) (BigFloat \"0\"))) [0]))",
        "4", pass, fail);
    test_error_contains(interp, "floor BigComplex tensor rejected",
        "(floor (Tensor BigComplex [1] [(BigComplex 1 0)]))",
        "rounding: complex numbers are not supported", pass, fail);
    test_error_contains(interp, "floor Float64 tensor rejects out of Integer range",
        "(floor (Tensor Float64 [1] [1.0e20]))",
        "floor: tensor integer result out of supported range", pass, fail);
    test_str_val(interp, "min Float64 tensor scalar preserves Float64 dtype",
        "(String (ref (min (Tensor Float64 [1] [3.7]) 4.2) [0]))", "3.7", pass, fail);
    test_str_val(interp, "min BigInteger tensor scalar preserves exact result",
        "(String (ref (min (Tensor BigInteger [1] [(BigInteger \"5\")]) 4) [0]))", "4", pass, fail);
    test_str_val(interp, "min BigInteger tensor scalar returns BigInteger dtype",
        "(String (dtype (min (Tensor BigInteger [1] [(BigInteger \"5\")]) 4)))", "BigInteger", pass, fail);
    test_str_val(interp, "max BigFloat tensor scalar preserves dtype",
        "(String (dtype (max (Tensor BigFloat [1] [(BigFloat \"2.5\")]) 4)))", "BigFloat", pass, fail);
    test_str_val(interp, "max BigFloat tensor scalar returns exact promoted result",
        "(String (ref (max (Tensor BigFloat [1] [(BigFloat \"2.5\")]) 4) [0]))", "4", pass, fail);
    test_str_val(interp, "min Tensor broadcast",
        "(String (ref (min (Tensor Float64 [2 1] [2 3]) (Tensor Float64 [1 2] [1 4])) [1 1]))", "3.0", pass, fail);
    test_str_val(interp, "max lazy BigFloat tensor realizes source",
        "(String (ref (max (map + (Tensor BigFloat [1] [(BigFloat \"2.5\")]) (BigFloat \"0\")) 4) [0]))", "4", pass, fail);
    test_error_contains(interp, "min BigComplex tensor rejected",
        "(min (Tensor BigComplex [1] [(BigComplex 1 0)]) 1)",
        "minmax: complex numbers are not ordered", pass, fail);
    test_str_val(interp, "gcd BigInteger tensor scalar preserves exact result",
        "(String (ref (gcd (Tensor BigInteger [2] [(BigInteger \"12\") (BigInteger \"18\")]) 6) [1]))", "6", pass, fail);
    test_str_val(interp, "gcd BigInteger tensor scalar returns BigInteger dtype",
        "(String (dtype (gcd (Tensor BigInteger [2] [(BigInteger \"12\") (BigInteger \"18\")]) 6)))", "BigInteger", pass, fail);
    test_str_val(interp, "lcm BigInteger Tensor broadcast",
        "(String (ref (lcm (Tensor BigInteger [2 1] [(BigInteger \"3\") (BigInteger \"4\")]) (Tensor BigInteger [1 2] [(BigInteger \"5\") (BigInteger \"6\")])) [1 1]))",
        "12", pass, fail);
    test_str_val(interp, "lcm lazy BigInteger tensor realizes source",
        "(String (ref (lcm (map + (Tensor BigInteger [1] [(BigInteger \"3\")]) (BigInteger \"0\")) 6) [0]))", "6", pass, fail);
    test_error_contains(interp, "gcd Float64 tensor rejected",
        "(gcd (Tensor Float64 [1] [6.0]) 3)",
        "gcd: tensor inputs must be exact integers", pass, fail);
    test_error_contains(interp, "Tensor BigComplex rejects non-numeric data",
        "(Tensor BigComplex [1] ['bad])", "numbers representable as BigComplex", pass, fail);
    test_eq_double(interp, "Tensor constructor consumes iterator as rank-1 tensor",
        "(ref (Tensor (Iterator [1 2 3])) [2])", 3.0, pass, fail);
    test_eq_double(interp, "Tensor constructor consumes lazy iterator map",
        "(ref (Tensor (map (lambda (x) (+ x 1)) (Iterator [1 2 3]))) [1])", 3.0, pass, fail);
    test_eq_double(interp, "Tensor explicit shape consumes iterator data",
        "(ref (Tensor Float64 [3] (map (lambda (x) (* x 2)) (Iterator [1 2 3]))) [2])", 6.0, pass, fail);
    test_str_val(interp, "Tensor iterator constructor preserves BigInteger dtype",
        "(String (ref (Tensor (Iterator [(BigInteger \"9223372036854775808\") 2]) BigInteger) [0]))",
        "9223372036854775808", pass, fail);
    test_error_contains(interp, "Tensor iterator constructor propagates non-numeric data errors",
        "(Tensor (Iterator [1 'bad 3]))", "inferred data must be numeric", pass, fail);
    test_eq_double(interp, "Iterator Tensor yields flat row-major elements",
        "(ref (Array (Iterator (Tensor Float64 [2 2] [1 2 3 4]))) 2)", 3.0, pass, fail);
    test_eq(interp, "Iterator Tensor supports empty tensors",
        "(length (List (Iterator (Tensor Float64 [0] 0.0))))", 0, pass, fail);
    test_eq_double(interp, "Iterator Tensor realizes lazy CPU tensor once for iteration",
        "(let (it (Iterator (map + (Tensor Float64 [3] [1 2 3]) 1.0))) (ref (Array it) 1))", 3.0, pass, fail);
    test_str_val(interp, "Iterator Tensor preserves BigInteger elements",
        "(String (ref (Array (Iterator (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]))) 0))",
        "9223372036854775808", pass, fail);
    test_truthy(interp, "Iterator Tensor rejects device tensor without explicit CPU copy when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (Iterator (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_eq(interp, "Array tensor conversion length",
        "(length (Array (Tensor Float64 [2 2] [1 2 3 4])))", 4, pass, fail);
    test_eq_double(interp, "Array tensor conversion row-major value",
        "(ref (Array (Tensor Float64 [2 2] [1 2 3 4])) 2)", 3.0, pass, fail);
    test_eq(interp, "List tensor conversion length",
        "(length (List (Tensor Float64 [2 2] [1 2 3 4])))", 4, pass, fail);
    test_eq_double(interp, "List tensor conversion row-major value",
        "(ref (List (Tensor Float64 [2 2] [1 2 3 4])) 2)", 3.0, pass, fail);
    test_eq_double(interp, "Array lazy tensor map conversion realizes",
        "(ref (Array (map + (Tensor Float64 [2] [1 2]) 1)) 1)", 3.0, pass, fail);
    test_eq_double(interp, "List lazy tensor contract conversion realizes",
        "(ref (List (contract (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [10 20]) [0] [0])) 0)", 50.0, pass, fail);
    test_error_contains(interp, "ref tensor wrong rank",
        "(ref (Tensor Float64 [2 3] [1 2 3 4 5 6]) [1])", "ref: tensor index rank mismatch", pass, fail);
    test_error_contains(interp, "ref tensor oob",
        "(ref (Tensor Float64 [2 3] [1 2 3 4 5 6]) [2 0])", "ref: tensor index out of bounds", pass, fail);
    // realize - tensor expression boundary
    test_eq_double(interp, "realize concrete tensor",
        "(ref (realize (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1 2])", 6.0, pass, fail);
    test_eq_double(interp, "realize rank-0 concrete tensor",
        "(let (dst (Tensor Float64 [] 0.0)) (block (realize (Tensor Float64 [] 3.0) dst) (ref dst [])))",
        3.0, pass, fail);
    test_eq(interp, "realize scalar into zero-size destination",
        "(let (dst (Tensor Float64 [0] 0.0)) (block (realize 9 dst) (length dst)))",
        0, pass, fail);
    test_eq(interp, "realize lazy zero-size source into zero-size destination",
        "(let (dst (Tensor Float64 [0] 0.0)) (block (realize (map + (Tensor Float64 [0] 0.0) 1.0) dst) (length dst)))",
        0, pass, fail);
    test_eq_double(interp, "realize tensor into destination",
        "(let (src (Tensor Float64 [2] [7 8]) dst (Tensor Float64 [2] 0.0)) (block (realize src dst) (ref dst [1])))",
        8.0, pass, fail);
    test_eq_double(interp, "realize scalar fill into destination",
        "(let (dst (Tensor Float64 [2 2] 0.0)) (block (realize 9 dst) (ref dst [1 1])))",
        9.0, pass, fail);
    test_eq_double(interp, "realize aliased destination no-op",
        "(let (dst (Tensor Float64 [2] [3 4])) (block (realize dst dst) (ref dst [1])))",
        4.0, pass, fail);
    test_error_contains(interp, "realize rejects scalar without destination",
        "(realize 1)", "realize: expected Tensor source", pass, fail);
    test_error_contains(interp, "realize rejects non-tensor destination",
        "(realize 1 [0])", "realize: destination must be a Tensor", pass, fail);
    test_error_contains(interp, "realize tensor destination shape mismatch",
        "(realize (Tensor Float64 [2] [1 2]) (Tensor Float64 [3] 0.0))", "realize: tensor shape mismatch", pass, fail);
    {
        usz[1] tensor_shape = { 2 };
        Value* src = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        bool setup_ok = src != null && src.tag == TENSOR && src.tensor_val != null && src.tensor_val.data != null;
        void* saved_data = setup_ok ? src.tensor_val.data : null;
        if (setup_ok) src.tensor_val.data = null;
        Value*[1] args = { src };
        Value* result = setup_ok ? prim_realize(args[0:1], null, interp) : null;
        if (setup_ok) src.tensor_val.data = saved_data;
        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            str_contains(result.str_chars[:result.str_len], "realize: source tensor backing storage is missing");
        if (ok) {
            emit_pass_literal("[PASS] realize malformed concrete tensor rejects missing source storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] realize malformed concrete tensor rejects missing source storage (setup_ok=%s tag=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1
            );
            (*fail)++;
        }
    }
    {
        usz[1] tensor_shape = { 2 };
        Value* src = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        Value* dst = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
        bool setup_ok = src != null && src.tag == TENSOR && src.tensor_val != null && src.tensor_val.data != null &&
            dst != null && dst.tag == TENSOR && dst.tensor_val != null && dst.tensor_val.data != null;
        void* saved_data = setup_ok ? src.tensor_val.data : null;
        if (setup_ok) src.tensor_val.data = null;
        Value*[2] args = { src, dst };
        Value* result = setup_ok ? prim_realize(args[0:2], null, interp) : null;
        if (setup_ok) src.tensor_val.data = saved_data;
        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            str_contains(result.str_chars[:result.str_len], "realize: source tensor backing storage is missing");
        if (ok) {
            emit_pass_literal("[PASS] realize malformed tensor into destination rejects missing source storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] realize malformed tensor into destination rejects missing source storage (setup_ok=%s tag=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1
            );
            (*fail)++;
        }
    }
    // map - tensor elementwise lazy expression surface
    test_eq_double(interp, "map tensor unary",
        "(ref (map (lambda (x) (* x 2)) (Tensor Float64 [2 2] [1 2 3 4])) [1 0])",
        6.0, pass, fail);
    test_eq_double(interp, "map tensor primitive unary minus",
        "(ref (map - (Tensor Float64 [2] [1.5 -2.5])) [1])",
        2.5, pass, fail);
    test_eq_double(interp, "map tensor primitive sqrt",
        "(ref (map sqrt (Tensor Float64 [2] [4.0 9.0])) [1])",
        3.0, pass, fail);
    test_eq_double(interp, "map tensor primitive real-part",
        "(ref (map real-part (Tensor Float64 [2] [1.5 -2.5])) [1])",
        -2.5, pass, fail);
    test_eq_double(interp, "map tensor primitive imag-part",
        "(ref (map imag-part (Tensor Float64 [2] [1.5 -2.5])) [0])",
        0.0, pass, fail);
    test_eq_double(interp, "map tensor primitive conjugate",
        "(ref (map conjugate (Tensor Float64 [2] [1.5 -2.5])) [1])",
        -2.5, pass, fail);
    test_eq_double(interp, "map tensor scalar right",
        "(ref (map + (Tensor Float64 [2] [1 2]) 10) [1])",
        12.0, pass, fail);
    test_eq_double(interp, "map tensor scalar left",
        "(ref (map + 10 (Tensor Float64 [2] [1 2])) [1])",
        12.0, pass, fail);
    test_eq_double(interp, "map tensor primitive min scalar",
        "(ref (map min (Tensor Float64 [2] [1.5 -2.5]) 0.0) [0])",
        0.0, pass, fail);
    test_eq_double(interp, "map tensor primitive max scalar left",
        "(ref (map max 0.0 (Tensor Float64 [2] [1.5 -2.5])) [1])",
        0.0, pass, fail);
    test_eq_double(interp, "map tensor tensor exact shape",
        "(ref (map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [10 20])) [1])",
        22.0, pass, fail);
    test_eq_double(interp, "map tensor tensor broadcast trailing singleton axis",
        "(ref (map + (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [1 3] [10 20 30])) [1 2])",
        36.0, pass, fail);
    test_eq_double(interp, "map tensor tensor broadcast inner singleton axis",
        "(ref (map + (Tensor Float64 [2 1] [1 2]) (Tensor Float64 [1 3] [10 20 30])) [1 2])",
        32.0, pass, fail);
    test_eq_double(interp, "map rank-0 tensor with rank-1 tensor",
        "(ref (map + (Tensor Float64 [] 7.0) (Tensor Float64 [2] [1 2])) [1])",
        9.0, pass, fail);
    test_eq_double(interp, "map rank-0 tensor with rank-2 tensor",
        "(ref (map + (Tensor Float64 [] 7.0) (Tensor Float64 [2 2] [1 2 3 4])) [1 1])",
        11.0, pass, fail);
    test_eq_double(interp, "realize mapped tensor into destination",
        "(let (src (map + (Tensor Float64 [2] [1 2]) 3) dst (Tensor Float64 [2] 0.0)) (block (realize src dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "realize mapped expression directly into destination",
        "(let (dst (Tensor Float64 [2] 0.0)) (block (realize (map + (Tensor Float64 [2] [1 2]) 3) dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "failed realize mapped expression leaves destination unchanged",
        "(let (dst (Tensor Float64 [2] [7 8])) (block (handle (realize (map (lambda (x) (if (= x 2) 'bad (+ x 1))) (Tensor Float64 [2] [1 2])) dst) (raise msg nil)) (+ (* (ref dst [0]) 10) (ref dst [1]))))",
        78.0, pass, fail);
    test_eq_double(interp, "realize aliased map destination",
        "(let (dst (Tensor Float64 [2 3] [1 2 3 4 5 6])) (block (realize (map + dst dst) dst) (ref dst [1 1])))",
        10.0, pass, fail);
    test_eq_double(interp, "map rank-0 scalar",
        "(ref (map + (Tensor Float64 [] 7.0) 1.0) [])",
        8.0, pass, fail);
    test_eq_double(interp, "realize mapped rank-0 scalar",
        "(ref (realize (map + (Tensor Float64 [] 7.0) 1.0)) [])",
        8.0, pass, fail);
    test_eq_double(interp, "realize mapped expression aliased destination",
        "(let (dst (Tensor Float64 [2] [3 4])) (block (realize (map + dst 1) dst) (ref dst [1])))",
        5.0, pass, fail);
    test_eq_double(interp, "lazy map tensor survives function return boundary",
        "(block (define (mk-map) (map + (Tensor Float64 [2] [1 2]) 3)) (ref (mk-map) [1]))",
        5.0, pass, fail);
    test_eq_double(interp, "lazy map tensor survives closure capture",
        "(let (src (map + (Tensor Float64 [2] [1 2]) 3)) (let (f (lambda () (ref src [1]))) (f)))",
        5.0, pass, fail);
    test_str_val(interp, "map BigInteger unary preserves exact range",
        "(String (ref (map (lambda (x) x) (Tensor [(BigInteger \"9223372036854775808\")] BigInteger)) [0]))",
        "9223372036854775808", pass, fail);
    test_str_val(interp, "map BigInteger scalar left",
        "(String (ref (map + (BigInteger \"2\") (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2])) [1]))",
        "4", pass, fail);
    test_str_val(interp, "map BigInteger tensor broadcast",
        "(String (ref (map + (Tensor BigInteger [2 1] [(BigInteger \"1\") (BigInteger \"2\")]) (Tensor BigInteger [1 2] [(BigInteger \"10\") (BigInteger \"20\")])) [1 1]))",
        "22", pass, fail);
    test_str_val(interp, "realize mapped BigInteger expression into destination",
        "(let (dst (Tensor BigInteger [2] 0)) (block (realize (map + (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (BigInteger \"2\")) dst) (String (ref dst [0]))))",
        "9223372036854775810", pass, fail);
    test_str_val(interp, "lazy BigInteger map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) (BigInteger \"2\"))) (String (ref (mk) [0])))",
        "9223372036854775810", pass, fail);
    test_str_val(interp, "lazy BigInteger map survives closure capture",
        "(let (src (map + (Tensor BigInteger [1] [(BigInteger \"9223372036854775808\")]) (BigInteger \"2\"))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "9223372036854775810", pass, fail);
    test_error_contains(interp, "map mixed BigInteger Float64 tensors rejected",
        "(map + (Tensor BigInteger [1] [(BigInteger \"1\")]) (Tensor Float64 [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_error_contains(interp, "map BigInteger rejects inexact scalar",
        "(map + (Tensor BigInteger [1] [(BigInteger \"1\")]) 1.5)",
        "map: tensor inputs must be exact integers", pass, fail);
    test_str_val(interp, "contract BigInteger vector dot",
        "(String (ref (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0]) []))",
        "18446744073709551622", pass, fail);
    test_str_val(interp, "contract BigInteger matrix product",
        "(String (ref (contract (Tensor BigInteger [2 3] [1 2 3 4 5 6]) (Tensor BigInteger [3 2] [7 8 9 10 11 12]) [1] [0]) [1 1]))",
        "154", pass, fail);
    test_str_val(interp, "contract BigInteger zero-size identity",
        "(String (ref (contract (Tensor BigInteger [0] []) (Tensor BigInteger [0] []) [0] [0]) []))",
        "0", pass, fail);
    test_str_val(interp, "realize BigInteger contract into destination",
        "(let (dst (Tensor BigInteger [2 2] 0)) (block (realize (contract (Tensor BigInteger [2 3] [1 2 3 4 5 6]) (Tensor BigInteger [3 2] [7 8 9 10 11 12]) [1] [0]) dst) (String (ref dst [1 1]))))",
        "154", pass, fail);
    test_str_val(interp, "lazy BigInteger contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0])) (String (ref (mk) [])))",
        "18446744073709551622", pass, fail);
    test_str_val(interp, "lazy BigInteger contract survives closure capture",
        "(let (src (contract (Tensor BigInteger [2] [(BigInteger \"9223372036854775808\") 2]) (Tensor BigInteger [2] [2 3]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "18446744073709551622", pass, fail);
    test_error_contains(interp, "contract mixed BigInteger Float64 tensors rejected",
        "(contract (Tensor BigInteger [1] [(BigInteger \"1\")]) (Tensor Float64 [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "map BigFloat unary preserves outside Float64 range",
        "(String (ref (map (lambda (x) x) (Tensor [(BigFloat \"1e309\")] BigFloat)) [0]))",
        "1e+309", pass, fail);
    test_str_val(interp, "map BigFloat scalar left",
        "(String (ref (map + (BigFloat \"2\") (Tensor BigFloat [2] [(BigFloat \"1.25\") 2])) [1]))",
        "4", pass, fail);
    test_str_val(interp, "map BigFloat tensor broadcast",
        "(String (ref (map + (Tensor BigFloat [2 1] [(BigFloat \"1\") (BigFloat \"2\")]) (Tensor BigFloat [1 2] [(BigFloat \"10\") (BigFloat \"20\")])) [1 1]))",
        "22", pass, fail);
    test_str_val(interp, "realize mapped BigFloat expression into destination",
        "(let (dst (Tensor BigFloat [2] 0)) (block (realize (map + (Tensor BigFloat [2] [(BigFloat \"1.25\") 2]) (BigFloat \"2\")) dst) (String (ref dst [0]))))",
        "3.25", pass, fail);
    test_str_val(interp, "lazy BigFloat map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigFloat [1] [(BigFloat \"1.25\")]) (BigFloat \"2\"))) (String (ref (mk) [0])))",
        "3.25", pass, fail);
    test_str_val(interp, "lazy BigFloat map survives closure capture",
        "(let (src (map + (Tensor BigFloat [1] [(BigFloat \"1.25\")]) (BigFloat \"2\"))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "3.25", pass, fail);
    test_error_contains(interp, "map mixed BigFloat Float64 tensors rejected",
        "(map + (Tensor BigFloat [1] [(BigFloat \"1\")]) (Tensor Float64 [1] [1]))",
        "map: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "contract BigFloat vector dot",
        "(String (ref (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0]) []))",
        "55", pass, fail);
    test_str_val(interp, "contract BigFloat matrix product",
        "(String (ref (contract (Tensor BigFloat [2 3] [(BigFloat \"1\") (BigFloat \"2\") (BigFloat \"3\") (BigFloat \"4\") (BigFloat \"5\") (BigFloat \"6\")]) (Tensor BigFloat [3 2] [(BigFloat \"7\") (BigFloat \"8\") (BigFloat \"9\") (BigFloat \"10\") (BigFloat \"11\") (BigFloat \"12\")]) [1 0]) [1 1]))",
        "154", pass, fail);
    test_str_val(interp, "contract BigFloat zero-size identity",
        "(String (ref (contract (Tensor BigFloat [0] []) (Tensor BigFloat [0] []) [0] [0]) []))",
        "0", pass, fail);
    test_str_val(interp, "realize BigFloat contract into destination",
        "(let (dst (Tensor BigFloat [2 2] 0)) (block (realize (contract (Tensor BigFloat [2 3] [(BigFloat \"1\") (BigFloat \"2\") (BigFloat \"3\") (BigFloat \"4\") (BigFloat \"5\") (BigFloat \"6\")]) (Tensor BigFloat [3 2] [(BigFloat \"7\") (BigFloat \"8\") (BigFloat \"9\") (BigFloat \"10\") (BigFloat \"11\") (BigFloat \"12\")]) [1 0]) dst) (String (ref dst [1 1]))))",
        "154", pass, fail);
    test_str_val(interp, "lazy BigFloat contract survives function return boundary",
        "(block (define (mk) (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0])) (String (ref (mk) [])))",
        "55", pass, fail);
    test_str_val(interp, "lazy BigFloat contract survives closure capture",
        "(let (src (contract (Tensor BigFloat [2] [(BigFloat \"1.5\") (BigFloat \"2\")]) (Tensor BigFloat [2] [(BigFloat \"10\") (BigFloat \"20\")]) [0] [0])) (let (f (lambda () (String (ref src [])))) (f)))",
        "55", pass, fail);
    test_error_contains(interp, "contract mixed BigFloat Float64 tensors rejected",
        "(contract (Tensor BigFloat [1] [(BigFloat \"1\")]) (Tensor Float64 [1] [1]) [0] [0])",
        "contract: tensor dtype mismatch", pass, fail);
    test_str_val(interp, "map BigComplex unary preserves complex value",
        "(String (ref (map (lambda (x) x) (Tensor [(BigComplex 1 2)] BigComplex)) [0]))",
        "1+2i", pass, fail);
    test_str_val(interp, "map BigComplex scalar left",
        "(String (ref (map + (BigComplex 2 1) (Tensor BigComplex [2] [(BigComplex 1 2) 3])) [1]))",
        "5+1i", pass, fail);
    test_str_val(interp, "map BigComplex tensor broadcast",
        "(String (ref (map + (Tensor BigComplex [2 1] [(BigComplex 1 1) (BigComplex 2 2)]) (Tensor BigComplex [1 2] [(BigComplex 10 10) (BigComplex 20 20)])) [1 1]))",
        "22+22i", pass, fail);
    test_str_val(interp, "realize mapped BigComplex expression into destination",
        "(let (dst (Tensor BigComplex [2] 0)) (block (realize (map + (Tensor BigComplex [2] [(BigComplex 1 2) 3]) (BigComplex 2 3)) dst) (String (ref dst [0]))))",
        "3+5i", pass, fail);
    test_str_val(interp, "lazy BigComplex map survives function return boundary",
        "(block (define (mk) (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 3))) (String (ref (mk) [0])))",
        "3+5i", pass, fail);
    test_str_val(interp, "lazy BigComplex map survives closure capture",
        "(let (src (map + (Tensor BigComplex [1] [(BigComplex 1 2)]) (BigComplex 2 3))) (let (f (lambda () (String (ref src [0])))) (f)))",
        "3+5i", pass, fail);
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
    test_truthy(interp, "matrix solve vulkan empty square keeps singular contract when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/solve (to-device (Tensor Float64 [0 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/singular-matrix) true) true))", pass, fail);
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
    test_truthy(interp, "matrix eigenvalues vulkan n>64 result stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (matrix/eigenvalues (to-device (matrix/identity 65) 'vulkan))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors vulkan n>64 computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (ev (matrix/eigenvectors (to-device (matrix/identity 65) 'vulkan))) (let (vals (to-device (ref ev 'values) 'cpu) vecs (to-device (ref ev 'vectors) 'cpu)) (if (= (length vals) 65) (if (< (abs (- (ref vals [64]) 1.0)) 0.000000001) (if (< (abs (- (ref vecs [0 0]) 1.0)) 0.000000001) (< (abs (- (ref vecs [64 64]) 1.0)) 0.000000001) nil) nil) nil))) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors vulkan n>64 outputs stay on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (ev (matrix/eigenvectors (to-device (matrix/identity 65) 'vulkan))) (if (= (device (ref ev 'values)) 'vulkan) (= (device (ref ev 'vectors)) 'vulkan) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues lazy vulkan n>64 computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (vals (to-device (matrix/eigenvalues (map + (to-device (matrix/identity 65) 'vulkan) 0.0)) 'cpu)) (if (= (length vals) 65) (< (abs (- (ref vals [0]) 1.0)) 0.000000001) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors lazy vulkan n>64 computes when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (vals (to-device (ref (matrix/eigenvectors (map + (to-device (matrix/identity 65) 'vulkan) 0.0)) 'values) 'cpu)) (if (= (length vals) 65) (< (abs (- (ref vals [64]) 1.0)) 0.000000001) nil)) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvalues nonsymmetric vulkan n>64 reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (a (Array (Tensor Float64 [65 65] 0.0))) (block (set! a 1 2.0) (= (handle (matrix/eigenvalues (to-device (Tensor Float64 [65 65] a) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-symmetric))) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenvectors nonsymmetric vulkan n>64 reports not symmetric when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (a (Array (Tensor Float64 [65 65] 0.0))) (block (set! a 1 2.0) (= (handle (matrix/eigenvectors (to-device (Tensor Float64 [65 65] a) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/not-symmetric))) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dsyev_call_count();
        test_truthy(interp, "matrix symmetric eigen vulkan path does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (< (abs (- (ref (to-device (matrix/eigenvalues (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) 'cpu) [0]) 3.0)) 0.000000001) (if (< (abs (- (ref (to-device (ref (matrix/eigenvectors (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) 'values) 'cpu) [1]) 2.0)) 0.000000001) (let (vals (to-device (matrix/eigenvalues (to-device (matrix/identity 65) 'vulkan)) 'cpu)) (< (abs (- (ref vals [64]) 1.0)) 0.000000001)) nil) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dsyev_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan symmetric eigen path does not call LAPACK"
                : "[PASS] tensor Vulkan symmetric eigen unavailable; no-LAPACK path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan symmetric eigen path does not call LAPACK");
            *fail += 1;
        }
    }
    test_eq(interp, "matrix eigenpairs empty values shape",
        "(ref (shape (ref (matrix/eigenpairs (Tensor Float64 [0 0] 0.0)) 'values)) 0)",
        0, pass, fail);
    test_eq(interp, "matrix eigenpairs empty vectors shape",
        "(ref (shape (ref (matrix/eigenpairs (Tensor Float64 [0 0] 0.0)) 'vectors)) 1)",
        0, pass, fail);
    setup(interp,
        "(define (matrix-eigenpairs-residual3-row a i v0 v1 v2) (+ (+ (* (BigComplex (ref a [i 0]) 0) v0) (* (BigComplex (ref a [i 1]) 0) v1)) (* (BigComplex (ref a [i 2]) 0) v2)))");
    setup(interp,
        "(define (matrix-eigenpairs-residual3-ok a k tol) (let (ep (matrix/eigenpairs a) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [k]) v0 (ref vecs [0 k]) v1 (ref vecs [1 k]) v2 (ref vecs [2 k])) (< (+ (+ (abs (- (matrix-eigenpairs-residual3-row a 0 v0 v1 v2) (* lambda v0))) (abs (- (matrix-eigenpairs-residual3-row a 1 v0 v1 v2) (* lambda v1)))) (abs (- (matrix-eigenpairs-residual3-row a 2 v0 v1 v2) (* lambda v2)))) tol)))");
    setup(interp,
        "(define (matrix-eigenpairs-residual3-all-ok a tol) (if (matrix-eigenpairs-residual3-ok a 0 tol) (if (matrix-eigenpairs-residual3-ok a 1 tol) (matrix-eigenpairs-residual3-ok a 2 tol) nil) nil))");
    {
        bool lapack_available = omni_tensor_backend_lapack_dgeev_available() != 0;
        if (lapack_available) {
            test_truthy(interp, "matrix eigenpairs complex value positive imaginary",
                "(< (abs (- (imag-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'values) [0])) (BigFloat \"1\"))) (BigFloat \"1e-10\"))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs complex value negative imaginary",
                "(< (abs (- (imag-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'values) [1])) (BigFloat \"-1\"))) (BigFloat \"1e-10\"))",
                pass, fail);
            test_str_val(interp, "matrix eigenpairs values dtype",
                "(String (dtype (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'values)))",
                "BigComplex", pass, fail);
            test_str_val(interp, "matrix eigenpairs vectors dtype",
                "(String (dtype (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'vectors)))",
                "BigComplex", pass, fail);
            test_eq(interp, "matrix eigenpairs vector shape columns",
                "(ref (shape (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'vectors)) 1)",
                2, pass, fail);
            test_truthy(interp, "matrix eigenpairs real diagonal first",
                "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])) (BigFloat \"3\"))) (BigFloat \"1e-10\"))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs real diagonal vector aligned",
                "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [3 0 0 2])) 'vectors) [0 0])) (BigFloat \"1\"))) (BigFloat \"1e-10\"))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs LAPACK real residual",
                "(let (ep (matrix/eigenpairs (Tensor Float64 [2 2] [3 0 0 2])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [0]) v0 (ref vecs [0 0]) v1 (ref vecs [1 0])) (< (+ (abs (- (* (BigComplex 3 0) v0) (* lambda v0))) (abs (- (* (BigComplex 2 0) v1) (* lambda v1)))) (BigFloat \"1e-8\")))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs LAPACK complex residual",
                "(let (ep (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [0]) v0 (ref vecs [0 0]) v1 (ref vecs [1 0])) (< (+ (abs (- (* (BigComplex -1 0) v1) (* lambda v0))) (abs (- v0 (* lambda v1)))) (BigFloat \"1e-8\")))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs LAPACK nonnormal residual",
                "(let (ep (matrix/eigenpairs (Tensor Float64 [3 3] [5 7 0 0 3 2 0 0 1])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [1]) v0 (ref vecs [0 1]) v1 (ref vecs [1 1]) v2 (ref vecs [2 1])) (< (+ (+ (abs (- (+ (* (BigComplex 5 0) v0) (* (BigComplex 7 0) v1)) (* lambda v0))) (abs (- (+ (* (BigComplex 3 0) v1) (* (BigComplex 2 0) v2)) (* lambda v1)))) (abs (- (* (BigComplex 1 0) v2) (* lambda v2)))) (BigFloat \"1e-6\")))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs LAPACK residual harness 3x3 columns",
                "(let (tol (BigFloat \"1e-6\") a (Tensor Float64 [3 3] [5 7 0 0 3 2 0 0 1]) b (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])) (if (matrix-eigenpairs-residual3-all-ok a tol) (matrix-eigenpairs-residual3-all-ok b tol) nil))",
                pass, fail);
            test_truthy(interp, "matrix eigenpairs realizes lazy input",
                "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (map + (Tensor Float64 [2 2] [3 0 0 2]) 0.0)) 'values) [1])) (BigFloat \"2\"))) (BigFloat \"1e-10\"))",
                pass, fail);
            {
                long lapack_calls_before = omni_tensor_backend_lapack_dgeev_call_count();
                test_truthy(interp, "matrix eigenpairs LAPACK candidate",
                    "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])) (BigFloat \"3\"))) (BigFloat \"1e-10\"))",
                    pass, fail);
                bool lapack_path_ok = omni_tensor_backend_lapack_dgeev_call_count() > lapack_calls_before;
                if (lapack_path_ok) {
                    emit_pass_literal("[PASS] tensor LAPACK backend dgeev matrix eigenpairs path");
                    (*pass)++;
                } else {
                    io::printn("[FAIL] tensor LAPACK backend dgeev matrix eigenpairs path");
                    (*fail)++;
                }
            }
        } else {
            emit_pass_literal("[PASS] tensor LAPACK backend dgeev unavailable; matrix eigenpairs fallback retained");
            (*pass)++;
        }
    }
    {
        int fail_before = *fail;
        omni_tensor_backend_lapack_dgeev_disable_for_tests(1);
        test_truthy(interp, "matrix eigenpairs pure fallback complex positive imaginary",
            "(< (abs (- (imag-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'values) [0])) (BigFloat \"1\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback real diagonal first",
            "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [2 2] [3 0 0 2])) 'values) [0])) (BigFloat \"3\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_str_val(interp, "matrix eigenpairs pure fallback vectors dtype",
            "(String (dtype (ref (matrix/eigenpairs (Tensor Float64 [2 2] [0 -1 1 0])) 'vectors)))",
            "BigComplex", pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback 3x3 diagonal first",
            "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [3 3] [5 0 0 0 3 0 0 0 1])) 'values) [0])) (BigFloat \"5\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback 3x3 upper triangular middle",
            "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [3 3] [5 7 0 0 3 2 0 0 1])) 'values) [1])) (BigFloat \"3\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback 3x3 complex block keeps real first",
            "(< (abs (- (real-part (ref (ref (matrix/eigenpairs (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])) 'values) [0])) (BigFloat \"2\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback 3x3 complex block positive imag second",
            "(< (abs (- (imag-part (ref (ref (matrix/eigenpairs (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])) 'values) [1])) (BigFloat \"1\"))) (BigFloat \"1e-8\"))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback diagonal residual",
            "(let (ep (matrix/eigenpairs (Tensor Float64 [3 3] [5 0 0 0 3 0 0 0 1])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [0]) v0 (ref vecs [0 0]) v1 (ref vecs [1 0]) v2 (ref vecs [2 0])) (< (+ (+ (abs (- (* (BigComplex 5 0) v0) (* lambda v0))) (abs (- (* (BigComplex 3 0) v1) (* lambda v1)))) (abs (- (* (BigComplex 1 0) v2) (* lambda v2)))) (BigFloat \"1e-6\")))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback complex residual",
            "(let (ep (matrix/eigenpairs (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [1]) v0 (ref vecs [0 1]) v1 (ref vecs [1 1]) v2 (ref vecs [2 1])) (< (+ (+ (abs (- (* (BigComplex -1 0) v1) (* lambda v0))) (abs (- v0 (* lambda v1)))) (abs (- (* (BigComplex 2 0) v2) (* lambda v2)))) (BigFloat \"1e-6\")))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback nonnormal residual",
            "(let (ep (matrix/eigenpairs (Tensor Float64 [3 3] [5 7 0 0 3 2 0 0 1])) vals (ref ep 'values) vecs (ref ep 'vectors) lambda (ref vals [1]) v0 (ref vecs [0 1]) v1 (ref vecs [1 1]) v2 (ref vecs [2 1])) (< (+ (+ (abs (- (+ (* (BigComplex 5 0) v0) (* (BigComplex 7 0) v1)) (* lambda v0))) (abs (- (+ (* (BigComplex 3 0) v1) (* (BigComplex 2 0) v2)) (* lambda v1)))) (abs (- (* (BigComplex 1 0) v2) (* lambda v2)))) (BigFloat \"1e-6\")))",
            pass, fail);
        test_truthy(interp, "matrix eigenpairs pure fallback residual harness 3x3 columns",
            "(let (tol (BigFloat \"1e-6\") a (Tensor Float64 [3 3] [5 7 0 0 3 2 0 0 1]) b (Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])) (if (matrix-eigenpairs-residual3-all-ok a tol) (matrix-eigenpairs-residual3-all-ok b tol) nil))",
            pass, fail);
        bool fallback_path_ok = *fail == fail_before && omni_tensor_backend_lapack_dgeev_available() == 0;
        omni_tensor_backend_lapack_dgeev_disable_for_tests(0);
        if (fallback_path_ok) {
            emit_pass_literal("[PASS] tensor LAPACK backend dgeev disabled; matrix eigenpairs pure fallback path");
            (*pass)++;
        } else {
            io::printn("[FAIL] tensor LAPACK backend dgeev disabled; matrix eigenpairs pure fallback path");
            (*fail)++;
        }
    }
    test_error_contains(interp, "matrix eigenpairs rejects vector input",
        "(matrix/eigenpairs (Tensor Float64 [2] [1 2]))",
        "matrix/eigenpairs: expected a square rank-2 Float64 Tensor", pass, fail);
    test_error_contains(interp, "matrix eigenpairs rejects complex dtype",
        "(matrix/eigenpairs (Tensor BigComplex [2 2] [1 0 0 1]))",
        "matrix/eigenpairs: expected a square rank-2 Float64 Tensor", pass, fail);
    test_truthy(interp, "matrix eigenpairs vulkan fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs lazy vulkan fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs vulkan wrong shape fails backend before shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (to-device (Tensor Float64 [2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "matrix eigenpairs lazy vulkan wrong shape fails backend before shape when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (matrix/eigenpairs (map + (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    {
        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
        long lapack_calls_before = omni_tensor_backend_lapack_dgeev_call_count();
        test_truthy(interp, "matrix eigenpairs vulkan fail-closed path does not call LAPACK when available",
            "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (if (= (handle (matrix/eigenpairs (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (matrix/eigenpairs (map + (to-device (Tensor Float64 [2 2] [3 0 0 2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) nil) true) true))", pass, fail);
        bool no_lapack_path_ok = !vulkan_available || omni_tensor_backend_lapack_dgeev_call_count() == lapack_calls_before;
        if (no_lapack_path_ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor Vulkan matrix eigenpairs fail-closed path does not call LAPACK"
                : "[PASS] tensor Vulkan matrix eigenpairs unavailable; no-LAPACK fail-closed path not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor Vulkan matrix eigenpairs fail-closed path does not call LAPACK");
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

fn void run_advanced_module_qualified_cached_tests(Interp* interp, int* pass, int* fail) {
    test_tag_interp(
        interp,
        "module returns void",
        "(module cmd-void-mod (export value) (define value 1))",
        VOID,
        pass,
        fail
    );
    test_tag_interp(interp, "import returns void", "(import cmd-void-mod)", VOID, pass, fail);

    // Qualified access (default import = qualified-only)
    setup(interp, "(module math-utils (export dbl triple) (define dbl (lambda (x) (* x 2))) (define triple (lambda (x) (* x 3))) (define internal (lambda (x) (+ x 100))))");
    setup(interp, "(import math-utils)");
    test_eq(interp, "module qualified dbl", "(math-utils.dbl 5)", 10, pass, fail);
    test_eq(interp, "module qualified triple", "(math-utils.triple 4)", 12, pass, fail);
    test_error(interp, "module unexported hidden", "(math-utils.internal 5)", pass, fail);

    // Module-owned effects should resolve like any other qualified member.
    setup(interp, "(module effect-mod (export open) (define [effect] (open (^Any payload))))");
    setup(interp, "(import effect-mod)");
    test_truthy(interp, "module effect member value", "(= effect-mod.open 'open)", pass, fail);
    test_eq_interp(interp, "module qualified effect signal",
        "(handle (signal effect-mod.open 42) (effect-mod.open x x))",
        42, pass, fail);

    // Unqualified access should fail without selective import
    test_error(interp, "module unqualified fails", "(triple 4)", pass, fail);
    // Re-import (cached)
    setup(interp, "(import math-utils)");
    test_eq(interp, "module re-import cached", "(math-utils.dbl 7)", 14, pass, fail);
    test_error(interp, "module duplicate def", "(module math-utils (export) (define x 1))", pass, fail);
}

fn void run_advanced_module_selective_rename_tests(Interp* interp, int* pass, int* fail) {
    // Selective import: (import mod (sym1 sym2))
    setup(interp, "(module sel-mod (export add-one sub-one) (define add-one (lambda (x) (+ x 1))) (define sub-one (lambda (x) (- x 1))))");
    setup(interp, "(import sel-mod (add-one))");
    test_eq(interp, "selective import add-one", "(add-one 5)", 6, pass, fail);
    test_error(interp, "selective import sub-one hidden", "(sub-one 5)", pass, fail);
    // Qualified still works alongside selective
    test_eq(interp, "selective + qualified", "(sel-mod.sub-one 5)", 4, pass, fail);

    // Rename import: (import mod (sym 'as alias))
    setup(interp, "(module rename-mod (export compute) (define compute (lambda (x) (* x 10))))");
    setup(interp, "(import rename-mod (compute 'as calc))");
    test_eq(interp, "renamed import", "(calc 3)", 30, pass, fail);
    test_error(interp, "original name hidden after rename", "(compute 3)", pass, fail);
}

fn void run_advanced_module_all_dependency_tests(Interp* interp, int* pass, int* fail) {
    // Import 'all (explicit opt-in)
    setup(interp, "(module all-mod (export foo bar) (define foo (lambda () 42)) (define bar (lambda () 99)))");
    setup(interp, "(import all-mod 'all)");
    test_eq(interp, "import all foo", "(foo)", 42, pass, fail);
    test_eq(interp, "import all bar", "(bar)", 99, pass, fail);

    // Original tests updated to use selective import
    setup(interp, "(module list-utils (export sum-list) (define sum-list (lambda (lst) (foldl + 0 lst))))");
    setup(interp, "(import list-utils (sum-list))");
    test_eq(interp, "module stdlib access", "(sum-list (quote (1 2 3 4 5)))", 15, pass, fail);
    setup(interp, "(module base-mod (export base-fn) (define base-fn (lambda (x) (+ x 100))))");
    setup(interp, "(import base-mod 'all)");
    setup(interp, "(module derived-mod (export derived-fn) (define derived-fn (lambda (x) (base-fn (* x 2)))))");
    setup(interp, "(import derived-mod (derived-fn))");
    test_eq(interp, "module depends on another", "(derived-fn 5)", 110, pass, fail);
    setup(interp, "(module private-mod (export) (define secret 42))");
    setup(interp, "(import private-mod)");
    test_error(interp, "module empty export hides", "secret", pass, fail);
    setup(interp, "(module closure-mod (export make-counter) (define make-counter (lambda () (let (n 0) (lambda () (block (set! n (+ n 1)) n))))))");
    setup(interp, "(import closure-mod (make-counter))");
    setup(interp, "(define my-counter (make-counter))");
    // Counter tests are stateful: JIT-only to avoid double mutation
    test_eq_jit(interp, "module counter 1", "(my-counter)", 1, pass, fail);
    test_eq_jit(interp, "module counter 2", "(my-counter)", 2, pass, fail);
    setup(interp, "(module tail-mod (export tail-sum) (define tail-sum (lambda (n acc) (if (= n 0) acc (tail-sum (- n 1) (+ acc n))))))");
    setup(interp, "(import tail-mod (tail-sum))");
    test_eq(interp, "module tail-call sum 1000", "(tail-sum 1000 0)", 500500, pass, fail);
}

fn void run_advanced_module_reexport_shorthand_tests(Interp* interp, int* pass, int* fail) {
    // Export-from (re-export)
    setup(interp, "(module inner-mod (export inner-val) (define inner-val 42))");
    setup(interp, "(module outer-mod (export inner-val) (import inner-mod) (export-from inner-mod (inner-val)))");
    setup(interp, "(import outer-mod (inner-val))");
    test_eq(interp, "export-from re-export", "inner-val", 42, pass, fail);
    setup(interp, "(module inner-all-mod (export all-a all-b) (define all-a 7) (define all-b 9))");
    setup(interp, "(module outer-all-mod (export all-a all-b) (export-from inner-all-mod 'all))");
    setup(interp, "(import outer-all-mod (all-a all-b))");
    test_eq(interp, "export-from 'all all-a", "all-a", 7, pass, fail);
    test_eq(interp, "export-from 'all all-b", "all-b", 9, pass, fail);

    // Module as first-class value
    setup(interp, "(module val-mod (export x) (define x 77))");
    setup(interp, "(import val-mod)");
    test_eq(interp, "module as value", "(val-mod.x)", 77, pass, fail);

    // === SHORTHAND DEFINE TESTS ===
    setup(interp, "(define (sh-double x) (* x 2))");
    test_eq(interp, "shorthand define", "(sh-double 21)", 42, pass, fail);
    setup(interp, "(define (sh-add3 a b c) (+ a (+ b c)))");
    test_eq(interp, "shorthand define multi", "(sh-add3 10 20 30)", 60, pass, fail);
    setup(interp, "(define (sh-answer) 42)");
    test_eq(interp, "shorthand define zero-arg", "(sh-answer)", 42, pass, fail);

    // export-from is command-style and should evaluate to Void.
    setup(interp, "(module export-ret-source (export val) (define val 5))");
    setup(interp, "(module export-ret-target (export ret-type val) (define ret-type (type-of (export-from export-ret-source (val)))) (export-from export-ret-source (val)))");
    setup(interp, "(import export-ret-target (ret-type val))");
    test_truthy(interp, "export-from return type is Void", "(= ret-type 'Void)", pass, fail);
    test_eq(interp, "export-from still re-exports symbol", "val", 5, pass, fail);
}

fn void run_advanced_parser_export_from_edge_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "parser export-from missing module",
        "(module bad-exp-missing (export) (export-from))",
        "expected module name after export-from",
        pass, fail);

    test_error_contains(interp,
        "parser export-from missing specifier",
        "(module bad-exp-nospec (export) (export-from inner-mod))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from bad specifier",
        "(module bad-exp-spec (export) (export-from inner-mod bogus))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from old :all rejected",
        "(module bad-exp-old (export) (export-from inner-mod :all))",
        "expected 'all or (names...) after export-from module",
        pass, fail);

    test_error_contains(interp,
        "parser export-from list non-symbol",
        "(module bad-exp-list (export) (export-from inner-mod (123)))",
        "expected symbol in export-from list",
        pass, fail);

    test_error_contains(interp,
        "parser export-from missing close paren",
        "(module bad-exp-close (export) (export-from inner-mod (a b))",
        ")",
        pass, fail);

    test_error_contains(interp,
        "parser export-from nested list element",
        "(module bad-exp-nested (export) (export-from inner-mod ((a))))",
        "expected symbol in export-from list",
        pass, fail);
}

fn void run_advanced_parser_import_edge_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(interp,
        "parser import old :all rejected",
        "(import bad-mod :all)",
        "expected 'all or (specifiers...) after import module",
        pass, fail);

    test_error_contains(interp,
        "parser import old :as rejected",
        "(import bad-mod (x :as y))",
        "expected 'as in import specifier",
        pass, fail);

    test_error_contains(interp,
        "parser import paren missing 'as",
        "(import bad-mod ((x y)))",
        "expected 'as in import specifier",
        pass, fail);

    test_error_contains(interp,
        "parser import paren missing alias",
        "(import bad-mod ((x 'as)))",
        "expected alias after 'as",
        pass, fail);

    test_error_contains(interp,
        "parser import symbol missing alias",
        "(import bad-mod (x 'as))",
        "expected alias after 'as",
        pass, fail);

    test_error_contains(interp,
        "parser import list non-symbol",
        "(import bad-mod (123))",
        "expected symbol in import list",
        pass, fail);

    test_error_contains(interp,
        "parser import missing close paren",
        "(import bad-mod (x y)",
        ")",
        pass, fail);

    {
        // Regression: import path parsing must not truncate at 255 bytes.
        char[384] long_path;
        usz long_path_len = 320;
        for (usz i = 0; i < long_path_len - 1; i++) long_path[i] = 'a';
        long_path[long_path_len - 1] = 'z';
        long_path[long_path_len] = 0;

        char[512] expr_buf;
        Expr* ast = null;
        if (try char[] expr_src = io::bprintf(&expr_buf, "(import \"%s\")", (ZString)&long_path)) {
            ast = parse_for_jit(expr_src, interp);
        }

        bool ok = ast != null &&
                  ast.tag == E_IMPORT &&
                  ast.import_expr.has_path &&
                  ast.import_expr.path != null &&
                  ast.import_expr.path_len == long_path_len &&
                  ast.import_expr.path[long_path_len - 1] == 'z';
        if (ok) {
            emit_pass_literal("[PASS] parser import long path >255");
            (*pass)++;
        } else {
            io::printfn("[FAIL] parser import long path >255 (parsed_len=%d expected=%d)",
                ast != null && ast.tag == E_IMPORT ? (long)ast.import_expr.path_len : -1,
                (long)long_path_len);
            (*fail)++;
        }
    }
}

fn void run_advanced_parser_module_edge_tests(Interp* interp, int* pass, int* fail) {
    Expr* ast = parse_for_jit(
        "(module bad-mod-fail-closed (export x) (define x 1) (if 1 2))",
        interp
    );
    if (ast == null) {
        emit_pass_literal("[PASS] parser module malformed body fails closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] parser module malformed body fails closed");
        (*fail)++;
    }
}

fn void run_advanced_runtime_import_path_edge_tests(Interp* interp, int* pass, int* fail) {
    {
        // Regression: module export growth must fail closed and preserve
        // existing exports when the replacement table cannot be allocated.
        Module mod = {};
        mod.export_capacity = 1;
        mod.export_count = 1;
        mod.exports = jit_alloc_module_exports(1);
        bool ok = mod.exports != null;
        if (ok) {
            mod.exports[0] = interp.symbols.intern("module-export-growth-existing");
            SymbolId* original_exports = mod.exports;
            bool saved_force = g_jit_force_module_export_alloc_fail;
            g_jit_force_module_export_alloc_fail = true;
            bool appended = module_append_export(&mod, interp.symbols.intern("module-export-growth-next"));
            g_jit_force_module_export_alloc_fail = saved_force;
            ok = !appended &&
                 mod.exports == original_exports &&
                 mod.export_capacity == 1 &&
                 mod.export_count == 1 &&
                 mod.exports[0] == interp.symbols.intern("module-export-growth-existing");
        }
        if (mod.exports != null) mem::free(mod.exports);

        if (ok) {
            emit_pass_literal("[PASS] runtime module export growth allocation failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime module export growth allocation failure fails closed");
            (*fail)++;
        }
    }

    {
        // Regression: checked import path resolution must fail closed on truncation.
        char[8] short_buf;
        usz short_len = 0;
        bool ok = !resolve_import_path_checked("long/path", interp, &short_buf, short_buf.len, &short_len);
        if (ok) {
            emit_pass_literal("[PASS] runtime import path checked resolver fails closed on overflow");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import path checked resolver fails closed on overflow");
            (*fail)++;
        }
    }

    {
        // Regression: string-path import cache must key by resolved file path, not raw import symbol.
        SymbolId cached_name = interp.symbols.intern("lib/util.omni");
        Module* cached = jit_create_file_module(cached_name, "/tmp/omni-cached-module-path.omni", interp);
        if (cached != null) cached.loaded = true;

        Expr* ast = parse_for_jit("(import \"lib/util.omni\")", interp);
        bool pushed = push_source_dir("/__omni_missing_source__/main.omni", interp);
        EvalResult load_result = eval_error("test bootstrap failed");
        if (pushed && ast != null) {
            Module* mod_out = null;
            load_result = load_import_target_module(ast, ast.import_expr.name, interp, &mod_out);
        }
        if (pushed) pop_source_dir(interp);

        bool ok = cached != null && ast != null && load_result.error.has_error;
        if (ok) {
            emit_pass_literal("[PASS] runtime import path cache ignores mismatched symbol-only cache entries");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import path cache ignores mismatched symbol-only cache entries");
            (*fail)++;
        }
    }

    {
        // Regression: post-load module resolution falls back to resolved file path.
        SymbolId requested_name = interp.symbols.intern("runtime-import-requested");
        SymbolId loaded_name = interp.symbols.intern("runtime-import-loaded");
        Module* loaded = jit_create_file_module(loaded_name, "/tmp/omni-runtime-path-resolution.omni", interp);
        if (loaded != null) loaded.loaded = true;
        Module* resolved = resolve_loaded_module(requested_name, "/tmp/omni-runtime-path-resolution.omni", interp);

        bool ok = loaded != null && resolved == loaded;
        if (ok) {
            emit_pass_literal("[PASS] runtime import resolves module by path when names differ");
            (*pass)++;
        } else {
            io::printn("[FAIL] runtime import resolves module by path when names differ");
            (*fail)++;
        }
    }
}

fn void run_advanced_module_system_tests(Interp* interp, int* pass, int* fail) {
    // === MODULE SYSTEM TESTS ===
    run_advanced_module_qualified_cached_tests(interp, pass, fail);
    run_advanced_module_selective_rename_tests(interp, pass, fail);
    run_advanced_module_all_dependency_tests(interp, pass, fail);
    run_advanced_module_reexport_shorthand_tests(interp, pass, fail);
    run_advanced_parser_import_edge_tests(interp, pass, fail);
    run_advanced_parser_export_from_edge_tests(interp, pass, fail);
    run_advanced_parser_module_edge_tests(interp, pass, fail);
    run_advanced_runtime_import_path_edge_tests(interp, pass, fail);
}

fn void run_advanced_collections_module_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_collections_dict_literal_tests(interp, pass, fail);
    run_advanced_collections_generic_ops_tests(interp, pass, fail);
    run_advanced_module_system_tests(interp, pass, fail);
}
