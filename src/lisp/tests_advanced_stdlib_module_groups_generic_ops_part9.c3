module lisp;

fn void run_advanced_collections_generic_ops_part9(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "ml/clip-gradients scales Float64 gradient trees by max norm",
        "(let (g (Dictionary 'w (Tensor Float64 [2] [3 4]) 'b (Tensor Float64 [1] [0])) out (ml/clip-gradients g 2.5)) (and (< (abs (- (ref (ref out 'w) [0]) 1.5)) 0.000000001) (< (abs (- (ref (ref out 'w) [1]) 2.0)) 0.000000001) (= (ref (ref out 'b) [0]) 0.0)))", pass, fail);
    test_truthy(interp, "ml/clip-gradients preserves under-norm trees",
        "(let (g (Array (Dictionary 'w (Tensor Float32 [1] [0.25]))) out (ml/clip-gradients g 1.0)) (= (ref (ref (ref out 0) 'w) [0]) (Float32 0.25)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step clip-norm applies before SGD update",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 1.0 'clip-norm 1.0) (Dictionary 'w (Tensor Float64 [2] [10 10])) (Dictionary 'w (Tensor Float64 [2] [3 4])) (Dictionary)) p (ref (ref r 'parameters) 'w)) (and (< (abs (- (ref p [0]) 9.4)) 0.000000001) (< (abs (- (ref p [1]) 9.2)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/clip-gradients validates max norm and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/clip-gradients (Dictionary 'w (Tensor Float64 [1] [1])) -1.0) (raise msg (ref msg 'code))) 'type/arg-mismatch) (= (handle (ml/clip-gradients (Dictionary 'w 1) 1.0) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (if (ref vulkan-backend 'float32) (= (handle (ml/clip-gradients (Dictionary 'w (to-device (Tensor Float32 [1] [1]) 'vulkan)) 1.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
}
