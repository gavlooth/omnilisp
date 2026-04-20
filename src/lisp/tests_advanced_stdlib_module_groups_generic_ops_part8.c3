module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part8(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "tensor-backends reports ML capability contract",
        "(let (rest-off (lambda (b) (and (= (ref b 'ml-convolution) false) (= (ref b 'ml-neural-map) false) (= (ref b 'ml-normalization) false) (= (ref b 'ml-attention) false) (= (ref b 'ml-autograd) false) (= (ref b 'ml-optimizer) false) (= (ref b 'ml-graph-execution) false))) neural-f64-off (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float64) false) (= (ref b 'ml-neural-tanh-float64) false) (= (ref b 'ml-neural-gelu-float64) false))) neural-f32-on (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float32) true) (= (ref b 'ml-neural-tanh-float32) true) (= (ref b 'ml-neural-gelu-float32) true))) neural-f32-off (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float32) false) (= (ref b 'ml-neural-tanh-float32) false) (= (ref b 'ml-neural-gelu-float32) false))) reduction-on (lambda (b) (and (= (ref b 'ml-reduction-float64) true) (= (ref b 'ml-reduction-float32) true))) reduction-off (lambda (b) (and (= (ref b 'ml-reduction-float64) false) (= (ref b 'ml-reduction-float32) false))) backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'ml-linear) true) (= (ref cpu 'ml-linear-direct-float64) true) (= (ref cpu 'ml-linear-direct-float32) true) (= (ref cpu 'ml-neural-relu-float64) true) (= (ref cpu 'ml-neural-relu-float32) true) (neural-f64-off cpu) (neural-f32-on cpu) (reduction-on cpu) (= (ref cuda 'ml-linear) false) (= (ref cuda 'ml-linear-direct-float64) false) (= (ref cuda 'ml-linear-direct-float32) false) (or (= (ref cuda 'ml-neural-relu-float64) true) (= (ref cuda 'ml-neural-relu-float64) false)) (or (= (ref cuda 'ml-neural-relu-float32) true) (= (ref cuda 'ml-neural-relu-float32) false)) (neural-f64-off cuda) (or (neural-f32-on cuda) (neural-f32-off cuda)) (reduction-off cuda) (= (ref cublas 'ml-linear) false) (= (ref cublas 'ml-linear-direct-float64) false) (= (ref cublas 'ml-linear-direct-float32) false) (= (ref cublas 'ml-neural-relu-float64) false) (= (ref cublas 'ml-neural-relu-float32) false) (neural-f64-off cublas) (neural-f32-off cublas) (reduction-off cublas) (= (ref vulkan 'ml-linear) false) (= (ref vulkan 'ml-linear-direct-float64) (ref vulkan 'float64)) (= (ref vulkan 'ml-linear-direct-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-relu-float64) (ref vulkan 'float64)) (= (ref vulkan 'ml-neural-relu-float32) (ref vulkan 'float32)) (neural-f64-off vulkan) (= (ref vulkan 'ml-neural-sigmoid-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-tanh-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-gelu-float32) (ref vulkan 'float32)) (reduction-off vulkan) (rest-off cpu) (rest-off cuda) (rest-off cublas) (rest-off vulkan)))", pass, fail);
    test_eq_double(interp, "ml/relu Float64 CPU clamps negatives",
        "(ref (ml/relu (Tensor Float64 [4] [-2 -0.5 0 3])) [3])",
        3.0, pass, fail);
    test_eq_double(interp, "ml/relu Float64 CPU zeroes negative",
        "(ref (ml/relu (Tensor Float64 [4] [-2 -0.5 0 3])) [0])",
        0.0, pass, fail);
    test_truthy(interp, "ml/relu Float32 CPU preserves dtype",
        "(let (out (ml/relu (Tensor Float32 [3] [-1 0 2]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 0.0)) (= (ref out [2]) (Float32 2.0))))", pass, fail);
    test_truthy(interp, "ml/relu rejects BigInteger tensor",
        "(= (handle (ml/relu (Tensor BigInteger [2] [-1 2])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
    test_truthy(interp, "ml/relu Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/relu (to-device (Tensor Float32 [3] [-1 0 2]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 0.0)) (= (ref (to-device out 'cpu) [2]) (Float32 2.0)))) true))", pass, fail);
    test_truthy(interp, "ml/relu Vulkan Float64 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/relu (to-device (Tensor Float64 [3] [-1 0 2]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [0]) (Float64 0.0)) (= (ref (to-device out 'cpu) [2]) (Float64 2.0)))) true))", pass, fail);
    test_truthy(interp, "ml/sigmoid Float32 CPU midpoint",
        "(let (out (ml/sigmoid (Tensor Float32 [1] [0]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 0.5))))", pass, fail);
    test_truthy(interp, "ml/tanh Float32 CPU zero",
        "(let (out (ml/tanh (Tensor Float32 [1] [0]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 0.0))))", pass, fail);
    test_truthy(interp, "ml/gelu Float32 CPU zero",
        "(let (out (ml/gelu (Tensor Float32 [1] [0]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 0.0))))", pass, fail);
    test_truthy(interp, "ml activations reject unsupported dtypes",
        "(and (= (handle (ml/sigmoid (Tensor BigInteger [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/tanh (Tensor BigInteger [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/gelu (Tensor BigInteger [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/sigmoid (Tensor Float64 [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/tanh (Tensor Float64 [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/gelu (Tensor Float64 [1] [0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported))", pass, fail);
    test_truthy(interp, "ml activations reject Vulkan Float64 explicitly",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (and (= (handle (ml/sigmoid (to-device (Tensor Float64 [1] [0]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/tanh (to-device (Tensor Float64 [1] [0]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/gelu (to-device (Tensor Float64 [1] [0]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)) true))", pass, fail);
    test_truthy(interp, "ml/sigmoid Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-neural-sigmoid-float32) (let (out (ml/sigmoid (to-device (Tensor Float32 [1] [0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 0.5)))) true))", pass, fail);
    test_truthy(interp, "ml/tanh Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-neural-tanh-float32) (let (out (ml/tanh (to-device (Tensor Float32 [1] [0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 0.0)))) true))", pass, fail);
    test_truthy(interp, "ml/gelu Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-neural-gelu-float32) (let (out (ml/gelu (to-device (Tensor Float32 [1] [0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [0]) (Float32 0.0)))) true))", pass, fail);
    test_truthy(interp, "ml/sum Float64 axis 0",
        "(let (out (ml/sum (Tensor Float64 [2 3] [1 2 3 4 5 6]) 0)) (and (= (rank out) 1) (= (ref (shape out) 0) 3) (= (ref out [0]) 5.0) (= (ref out [2]) 9.0)))", pass, fail);
    test_truthy(interp, "ml/mean Float32 negative axis preserves dtype",
        "(let (out (ml/mean (Tensor Float32 [2 2] [1 2 3 5]) -1)) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 1.5)) (= (ref out [1]) (Float32 4.0))))", pass, fail);
    test_truthy(interp, "ml/variance Float64 axis 1 population variance",
        "(let (out (ml/variance (Tensor Float64 [2 2] [1 3 2 6]) [1])) (and (= (ref out [0]) 1.0) (= (ref out [1]) 4.0)))", pass, fail);
    test_truthy(interp, "ml/max Float64 axis 1 prerequisite",
        "(let (out (ml/max (Tensor Float64 [2 3] [1 9 3 4 5 6]) 1)) (and (= (rank out) 1) (= (ref out [0]) 9.0) (= (ref out [1]) 6.0)))", pass, fail);
    test_eq_double(interp, "ml/sum Float64 multi-axis scalar",
        "(ref (ml/sum (Tensor Float64 [2 2] [1 2 3 4]) [0 1]) [])",
        10.0, pass, fail);
    test_eq_double(interp, "ml/sum Float64 proper-list axes scalar",
        "(ref (ml/sum (Tensor Float64 [2 2] [1 2 3 4]) (List 0 1)) [])",
        10.0, pass, fail);
    test_truthy(interp, "ml/mean realizes transpose-view input",
        "(let (out (ml/mean (matrix/transpose-view (Tensor Float64 [2 3] [1 2 3 4 5 6])) 1)) (and (= (rank out) 1) (= (ref out [0]) 2.5) (= (ref out [2]) 4.5)))", pass, fail);
    test_truthy(interp, "ml/sum Float32 overflow reports numeric overflow",
        "(= (handle (ml/sum (Tensor Float32 [2] [2.0e38 2.0e38]) [0]) (raise msg (ref msg 'code))) 'tensor/numeric-overflow)", pass, fail);
    test_truthy(interp, "ml/logsumexp Float64 multi-axis stable",
        "(let (out (ml/logsumexp (Tensor Float64 [2 2] [1000 1000 1000 1000]) [0 1])) (< (abs (- (ref out []) (+ 1000.0 (log 4.0)))) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/logsumexp Float32 negative axis preserves dtype",
        "(let (out (ml/logsumexp (Tensor Float32 [1 2] [0 0]) -1)) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out [0]) (log 2.0))) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/softmax Float64 large logits stable",
        "(let (out (ml/softmax (Tensor Float64 [2] [1000 1000]) 0)) (and (= (rank out) 1) (< (abs (- (ref out [0]) 0.5)) 0.000000001) (< (abs (- (ref out [1]) 0.5)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/softmax Float32 row axis preserves shape and sums",
        "(let (out (ml/softmax (Tensor Float32 [2 2] [0 0 1 1]) 1)) (and (= (String (dtype out)) \"Float32\") (= (ref (shape out) 0) 2) (= (ref (shape out) 1) 2) (< (abs (- (+ (ref out [0 0]) (ref out [0 1])) 1.0)) 0.0001) (< (abs (- (+ (ref out [1 0]) (ref out [1 1])) 1.0)) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/softmax rejects axis lists",
        "(= (handle (ml/softmax (Tensor Float64 [2] [1 2]) [0]) (raise msg (ref msg 'code))) 'type/arg-mismatch)", pass, fail);
    test_truthy(interp, "ml/mean-squared-error Float64 scalar loss",
        "(let (out (ml/mean-squared-error (Tensor Float64 [3] [1 2 3]) (Tensor Float64 [3] [1 1 5]))) (and (= (rank out) 0) (< (abs (- (ref out []) (/ 5.0 3.0))) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error Float32 zero preserves dtype",
        "(let (out (ml/mean-squared-error (Tensor Float32 [2] [1 2]) (Tensor Float32 [2] [1 2]))) (and (= (String (dtype out)) \"Float32\") (= (ref out []) (Float32 0.0))))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error realizes lazy CPU input",
        "(let (out (ml/mean-squared-error (map + (Tensor Float64 [2] [1 3]) 1.0) (Tensor Float64 [2] [1 1]))) (< (abs (- (ref out []) 5.0)) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error rejects shape and dtype mismatch",
        "(and (= (handle (ml/mean-squared-error (Tensor Float64 [2] [1 2]) (Tensor Float64 [1 2] [1 2])) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/mean-squared-error (Tensor Float64 [1] [1]) (Tensor Float32 [1] [1])) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error rejects unsupported dtype and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/mean-squared-error (Tensor BigInteger [1] [1]) (Tensor BigInteger [1] [1])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (if (ref vulkan-backend 'float32) (= (handle (ml/mean-squared-error (to-device (Tensor Float32 [1] [1]) 'vulkan) (to-device (Tensor Float32 [1] [1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Float64 rank-1 one-hot stable",
        "(let (out (ml/cross-entropy (Tensor Float64 [2] [1000 1000]) (Tensor Float64 [2] [1 0]) 0)) (< (abs (- (ref out []) (log 2.0))) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Float32 batch averages over non-class positions",
        "(let (out (ml/cross-entropy (Tensor Float32 [2 2] [0 0 0 0]) (Tensor Float32 [2 2] [1 0 0 1]) 1)) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out []) (log 2.0))) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/cross-entropy supports non-last class axis",
        "(let (out (ml/cross-entropy (Tensor Float64 [2 2] [0 0 0 0]) (Tensor Float64 [2 2] [1 0 0 1]) 0)) (< (abs (- (ref out []) (log 2.0))) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/cross-entropy accepts soft targets",
        "(let (out (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float64 [2] [0.25 0.75]) 0)) (< (abs (- (ref out []) (log 2.0))) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/cross-entropy rejects invalid target probabilities",
        "(= (handle (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float64 [2] [0.25 0.25]) 0) (raise msg (ref msg 'code))) 'tensor/invalid-argument)", pass, fail);
    test_truthy(interp, "ml/cross-entropy rejects shape dtype and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float64 [1 2] [1 0]) 0) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float32 [2] [1 0]) 0) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) (if (ref vulkan-backend 'float32) (= (handle (ml/cross-entropy (to-device (Tensor Float32 [2] [0 0]) 'vulkan) (to-device (Tensor Float32 [2] [1 0]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml reductions reject duplicate and empty axes",
        "(and (= (handle (ml/sum (Tensor Float64 [2 2] [1 2 3 4]) [0 0]) (raise msg (ref msg 'code))) 'tensor/axis-duplicate) (= (handle (ml/mean (Tensor Float64 [2] [1 2]) []) (raise msg (ref msg 'code))) 'tensor/shape-mismatch))", pass, fail);
    test_truthy(interp, "ml reductions reject unsupported dtype and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/sum (Tensor BigInteger [2] [1 2]) [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (if (ref vulkan-backend 'float32) (and (= (handle (ml/sum (to-device (Tensor Float32 [2] [1 2]) 'vulkan) [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/logsumexp (to-device (Tensor Float32 [2] [1 2]) 'vulkan) [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/softmax (to-device (Tensor Float32 [2] [1 2]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)) true)))", pass, fail);
    test_eq_double(interp, "ml/linear Float64 vector projection",
        "(ref (ml/linear (Tensor Float64 [3] [1 2 3]) (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1])",
        32.0, pass, fail);
    test_eq_double(interp, "ml/linear Float64 batched bias projection",
        "(ref (ml/linear (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [2] [10 100])) [1 1])",
        177.0, pass, fail);
    test_eq_double(interp, "ml/linear Float64 rank-3 batch projection",
        "(ref (ml/linear (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1 1 1])",
        167.0, pass, fail);
    test_eq_double(interp, "ml/linear realizes lazy CPU input",
        "(ref (ml/linear (map + (Tensor Float64 [3] [1 2 3]) 0.0) (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1])",
        32.0, pass, fail);
    test_truthy(interp, "ml/linear Float32 preserves dtype and values",
        "(let (out (ml/linear (Tensor Float32 [2] [1.5 2.0]) (Tensor Float32 [1 2] [2.0 3.0]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 9.0))))", pass, fail);
    test_eq_double(interp, "ml/linear/batched-reduce Float64 rank-3 projection",
        "(ref (ml/linear/batched-reduce (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1 1 1])",
        167.0, pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Float32 bias preserves dtype",
        "(let (out (ml/linear/batched-reduce (Tensor Float32 [2 3] [1 2 3 4 5 6]) (Tensor Float32 [2 3] [1 2 3 4 5 6]) (Tensor Float32 [2] [10 100]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [1 1]) (Float32 177.0))))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce rejects vector input",
        "(= (handle (ml/linear/batched-reduce (Tensor Float32 [3] [1 2 3]) (Tensor Float32 [2 3] [1 2 3 4 5 6])) (raise msg (ref msg 'code))) 'tensor/shape-mismatch)", pass, fail);
    test_eq_double(interp, "ml/linear/batched-reduce realizes lazy CPU input",
        "(ref (ml/linear/batched-reduce (map + (Tensor Float64 [2 3] [1 2 3 4 5 6]) 0.0) (Tensor Float64 [1 3] [1 1 1])) [1 0])",
        15.0, pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce dtype mismatch returns tensor/dtype-mismatch",
        "(= (handle (ml/linear/batched-reduce (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float32 [1 3] [1 2 3])) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch)", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce bias shape mismatch returns tensor/shape-mismatch",
        "(= (handle (ml/linear/batched-reduce (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [1 3] [1 1 1]) (Tensor Float64 [2] [10 20])) (raise msg (ref msg 'code))) 'tensor/shape-mismatch)", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 no-bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 77.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 rank-3 batch projection",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [1 1 1]) (Float32 167.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2] [10 100]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1]) (Float32 132.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 bias shape mismatch returns tensor/shape-mismatch",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [3] [10 100 1000]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 mapped bias expression stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (map + (to-device (Tensor Float32 [2] [10 100]) 'vulkan) (Float32 0.0)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1]) (Float32 132.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 rank-3 bias projection",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2] [10 100]) 'vulkan))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [1 1 1]) (Float32 267.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear rejects mixed CPU Vulkan Float32 operands",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (Tensor Float32 [2 3] [1 2 3 4 5 6])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear rejects mixed CPU Vulkan Float32 bias",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (Tensor Float32 [2] [10 100])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 mapped source stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (map + (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (Float32 0.0)) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [1]) (Float32 32.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 view source realizes on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (matrix/transpose-view (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan)) (to-device (Tensor Float32 [1 2] [1 2]) 'vulkan))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [2 0]) (Float32 15.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 view weights realize on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (matrix/transpose-view (to-device (Tensor Float32 [3 2] [1 4 2 5 3 6]) 'vulkan)))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [1]) (Float32 32.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 no-bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1]) (Float64 77.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 rank-3 batch projection",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1 1]) (Float64 167.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2] [10 100]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1]) (Float64 132.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 mapped bias expression stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (map + (to-device (Tensor Float64 [2] [10 100]) 'vulkan) (Float64 0.0)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1]) (Float64 132.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 mapped source stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (Float64 0.0)) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1]) (Float64 32.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 view source realizes on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (matrix/transpose-view (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (to-device (Tensor Float64 [1 2] [1 2]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [2 0]) (Float64 15.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 view weights realize on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (matrix/transpose-view (to-device (Tensor Float64 [3 2] [1 4 2 5 3 6]) 'vulkan)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1]) (Float64 32.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float64 shape mismatch preserves oracle diagnostic",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [3 3] [1 2 3 4 5 6 7 8 9]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true))", pass, fail);
    test_truthy(interp, "ml/linear Vulkan Float32 shape mismatch preserves oracle diagnostic",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear (to-device (Tensor Float32 [2] [1 2]) 'vulkan) (to-device (Tensor Float32 [3 3] [1 2 3 4 5 6 7 8 9]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float32 rank-3 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear/batched-reduce (to-device (Tensor Float32 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1 1]) (Float32 167.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float32 bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear/batched-reduce (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2] [10 100]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 177.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float32 mapped bias expression stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear/batched-reduce (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (map + (to-device (Tensor Float32 [2] [10 100]) 'vulkan) (Float32 0.0)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 177.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce rejects mixed CPU Vulkan Float32 operands",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear/batched-reduce (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (Tensor Float32 [2 3] [1 2 3 4 5 6])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float32 view source realizes on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear/batched-reduce (matrix/transpose-view (to-device (Tensor Float32 [3 2] [1 2 3 4 5 6]) 'vulkan)) (to-device (Tensor Float32 [1 3] [1 1 1]) 'vulkan))) (and (= (device out) 'vulkan) (= (ref (to-device out 'cpu) [1 0]) (Float32 12.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float32 view weights realize on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/linear/batched-reduce (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (matrix/transpose-view (to-device (Tensor Float32 [3 2] [1 0 0 1 0 0]) 'vulkan)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 5.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 rank-3 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1 1]) (Float64 167.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 bias stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2] [10 100]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1]) (Float64 177.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 mapped bias expression stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (map + (to-device (Tensor Float64 [2] [10 100]) 'vulkan) (Float64 0.0)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1]) (Float64 177.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 mapped source stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (map + (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (Float64 0.0)) (to-device (Tensor Float64 [1 3] [1 1 1]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 0]) (Float64 15.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 view source realizes on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (matrix/transpose-view (to-device (Tensor Float64 [3 2] [1 2 3 4 5 6]) 'vulkan)) (to-device (Tensor Float64 [1 3] [1 1 1]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 0]) (Float64 12.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 view weights realize on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (let (out (ml/linear/batched-reduce (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (matrix/transpose-view (to-device (Tensor Float64 [3 2] [1 0 0 1 0 0]) 'vulkan)))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (= (ref (to-device out 'cpu) [1 1]) (Float64 5.0)))) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce Vulkan Float64 input rank diagnostic",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear/batched-reduce (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce rejects CPU view with Vulkan weights before fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear/batched-reduce (matrix/transpose-view (Tensor Float32 [3 2] [1 2 3 4 5 6])) (to-device (Tensor Float32 [1 3] [1 1 1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear/batched-reduce rejects CPU view weights before fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/linear/batched-reduce (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) (matrix/transpose-view (Tensor Float32 [3 2] [1 0 0 0 1 0]))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_error_contains(interp, "ml/linear rejects dtype mismatch",
        "(ml/linear (Tensor Float64 [2] [1 2]) (Tensor Float32 [1 2] [1 2]))",
        "ml/linear: tensor dtype mismatch", pass, fail);
    test_error_contains(interp, "ml/linear rejects incompatible shapes",
        "(ml/linear (Tensor Float64 [2] [1 2]) (Tensor Float64 [3 3] [1 2 3 4 5 6 7 8 9]))",
        "ml/linear: input, weights, and bias shapes are incompatible", pass, fail);
    test_truthy(interp, "ml/linear rejects Vulkan input without CPU fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (Tensor Float64 [2 3] [1 2 3 4 5 6])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear rejects Vulkan weights without CPU fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear (Tensor Float64 [3] [1 2 3]) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear rejects Vulkan bias without CPU fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear (Tensor Float64 [3] [1 2 3]) (Tensor Float64 [2 3] [1 2 3 4 5 6]) (to-device (Tensor Float64 [2] [10 100]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml/linear rejects lazy Vulkan source before CPU realization",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float64) (= (handle (ml/linear (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 0.0) (Tensor Float64 [2 3] [1 2 3 4 5 6])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "tensor-backends reports stats normal capability separately",
        "(let (backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'stats-normal-float64) true) (= (ref cpu 'stats-normal-float32) true) (or (= (ref cuda 'stats-normal-float64) true) (= (ref cuda 'stats-normal-float64) false)) (or (= (ref cuda 'stats-normal-float32) true) (= (ref cuda 'stats-normal-float32) false)) (= (ref cublas 'stats-normal-float64) false) (= (ref cublas 'stats-normal-float32) false) (= (ref vulkan 'stats-normal-float64) (ref vulkan 'float64)) (= (ref vulkan 'stats-normal-float32) (ref vulkan 'float32))))", pass, fail);
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
