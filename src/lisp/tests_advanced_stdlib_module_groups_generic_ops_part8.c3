module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part8(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "tensor-backends reports ML capability contract",
        "(let (rest-off (lambda (b) (and (= (ref b 'ml-neural-map) false) (= (ref b 'ml-autograd) false) (= (ref b 'ml-optimizer) false) (= (ref b 'ml-graph-execution) false))) neural-f64-off (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float64) false) (= (ref b 'ml-neural-tanh-float64) false) (= (ref b 'ml-neural-gelu-float64) false))) neural-f32-on (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float32) true) (= (ref b 'ml-neural-tanh-float32) true) (= (ref b 'ml-neural-gelu-float32) true))) neural-f32-off (lambda (b) (and (= (ref b 'ml-neural-sigmoid-float32) false) (= (ref b 'ml-neural-tanh-float32) false) (= (ref b 'ml-neural-gelu-float32) false))) reduction-on (lambda (b) (and (= (ref b 'ml-reduction-float64) true) (= (ref b 'ml-reduction-float32) true))) reduction-off (lambda (b) (and (= (ref b 'ml-reduction-float64) false) (= (ref b 'ml-reduction-float32) false))) backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'ml-linear) true) (= (ref cpu 'ml-linear-direct-float64) true) (= (ref cpu 'ml-linear-direct-float32) true) (= (ref cpu 'ml-neural-relu-float64) true) (= (ref cpu 'ml-neural-relu-float32) true) (neural-f64-off cpu) (neural-f32-on cpu) (reduction-on cpu) (= (ref cpu 'ml-normalization) true) (= (ref cpu 'ml-layer-normalization-float64) true) (= (ref cpu 'ml-layer-normalization-float32) true) (= (ref cuda 'ml-linear) false) (= (ref cuda 'ml-linear-direct-float64) false) (= (ref cuda 'ml-linear-direct-float32) false) (or (= (ref cuda 'ml-neural-relu-float64) true) (= (ref cuda 'ml-neural-relu-float64) false)) (or (= (ref cuda 'ml-neural-relu-float32) true) (= (ref cuda 'ml-neural-relu-float32) false)) (neural-f64-off cuda) (or (neural-f32-on cuda) (neural-f32-off cuda)) (reduction-off cuda) (= (ref cuda 'ml-normalization) false) (= (ref cuda 'ml-layer-normalization-float64) false) (= (ref cuda 'ml-layer-normalization-float32) false) (= (ref cublas 'ml-linear) false) (= (ref cublas 'ml-linear-direct-float64) false) (= (ref cublas 'ml-linear-direct-float32) false) (= (ref cublas 'ml-neural-relu-float64) false) (= (ref cublas 'ml-neural-relu-float32) false) (neural-f64-off cublas) (neural-f32-off cublas) (reduction-off cublas) (= (ref cublas 'ml-normalization) false) (= (ref cublas 'ml-layer-normalization-float64) false) (= (ref cublas 'ml-layer-normalization-float32) false) (= (ref vulkan 'ml-linear) false) (= (ref vulkan 'ml-linear-direct-float64) (ref vulkan 'float64)) (= (ref vulkan 'ml-linear-direct-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-relu-float64) (ref vulkan 'float64)) (= (ref vulkan 'ml-neural-relu-float32) (ref vulkan 'float32)) (neural-f64-off vulkan) (= (ref vulkan 'ml-neural-sigmoid-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-tanh-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-neural-gelu-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-reduction-float64) (ref vulkan 'float64)) (= (ref vulkan 'ml-reduction-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-normalization) (ref vulkan 'float32)) (= (ref vulkan 'ml-layer-normalization-float64) false) (= (ref vulkan 'ml-layer-normalization-float32) (ref vulkan 'float32)) (rest-off cpu) (rest-off cuda) (rest-off cublas) (rest-off vulkan)))", pass, fail);
    test_truthy(interp, "tensor-backends reports ML batch normalization narrow capability contract",
        "(let (backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'ml-batch-normalization-float64) true) (= (ref cpu 'ml-batch-normalization-float32) true) (= (ref cuda 'ml-batch-normalization-float64) false) (= (ref cuda 'ml-batch-normalization-float32) false) (= (ref cublas 'ml-batch-normalization-float64) false) (= (ref cublas 'ml-batch-normalization-float32) false) (= (ref vulkan 'ml-batch-normalization-float64) false) (= (ref vulkan 'ml-batch-normalization-float32) (ref vulkan 'float32))))", pass, fail);
    test_truthy(interp, "tensor-backends reports ML attention narrow capability contract",
        "(let (backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'ml-attention) true) (= (ref cpu 'ml-scaled-dot-product-attention-float64) true) (= (ref cpu 'ml-scaled-dot-product-attention-float32) true) (= (ref cuda 'ml-attention) false) (= (ref cuda 'ml-scaled-dot-product-attention-float64) false) (= (ref cuda 'ml-scaled-dot-product-attention-float32) false) (= (ref cublas 'ml-attention) false) (= (ref cublas 'ml-scaled-dot-product-attention-float64) false) (= (ref cublas 'ml-scaled-dot-product-attention-float32) false) (= (ref vulkan 'ml-attention) (ref vulkan 'float32)) (= (ref vulkan 'ml-scaled-dot-product-attention-float64) false) (= (ref vulkan 'ml-scaled-dot-product-attention-float32) (ref vulkan 'float32))))", pass, fail);
    test_truthy(interp, "tensor-backends reports ML convolution narrow capability contract",
        "(let (backends (tensor-backends) cpu (ref backends 0) cuda (ref backends 1) cublas (ref backends 2) vulkan (ref backends 3)) (and (= (ref cpu 'ml-convolution) true) (= (ref cpu 'ml-conv1d-direct-float64) true) (= (ref cpu 'ml-conv1d-direct-float32) true) (= (ref cpu 'ml-conv2d-direct-float64) true) (= (ref cpu 'ml-conv2d-direct-float32) true) (= (ref cpu 'ml-pool2d-direct-float64) true) (= (ref cpu 'ml-pool2d-direct-float32) true) (= (ref cuda 'ml-convolution) false) (= (ref cuda 'ml-conv1d-direct-float64) false) (= (ref cuda 'ml-conv1d-direct-float32) false) (= (ref cuda 'ml-conv2d-direct-float64) false) (= (ref cuda 'ml-conv2d-direct-float32) false) (= (ref cuda 'ml-pool2d-direct-float64) false) (= (ref cuda 'ml-pool2d-direct-float32) false) (= (ref cublas 'ml-convolution) false) (= (ref cublas 'ml-conv1d-direct-float64) false) (= (ref cublas 'ml-conv1d-direct-float32) false) (= (ref cublas 'ml-conv2d-direct-float64) false) (= (ref cublas 'ml-conv2d-direct-float32) false) (= (ref cublas 'ml-pool2d-direct-float64) false) (= (ref cublas 'ml-pool2d-direct-float32) false) (= (ref vulkan 'ml-convolution) (ref vulkan 'float32)) (= (ref vulkan 'ml-conv1d-direct-float64) false) (= (ref vulkan 'ml-conv1d-direct-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-conv2d-direct-float64) false) (= (ref vulkan 'ml-conv2d-direct-float32) (ref vulkan 'float32)) (= (ref vulkan 'ml-pool2d-direct-float64) false) (= (ref vulkan 'ml-pool2d-direct-float32) (ref vulkan 'float32))))", pass, fail);
    test_truthy(interp, "nn/dense constructor normalizes validated DataSpec",
        "(let (spec (nn/dense 4 3 (Dictionary 'activation 'relu))) (and (= (ref (nn/validate spec) 'kind) 'dense) (= (ref spec 'input-features) 4) (= (ref spec 'output-features) 3) (= (ref spec 'use-bias) true) (= (ref spec 'activation) 'relu) (= (ref (ref spec 'kernel-init) 'kind) 'kaiming-uniform) (= (ref (ref spec 'bias-init) 'kind) 'zeros)))", pass, fail);
    test_truthy(interp, "nn/sequential validates convolution pooling flatten dense specs",
        "(let (spec (nn/sequential (nn/conv2d 3 8 [3 3] [1 1] [1 1] [1 1] 1) (nn/relu) (nn/max-pool2d [2 2] [2 2] [0 0]) (nn/flatten) (nn/dense 128 10))) (and (= (ref (nn/validate spec) 'kind) 'sequential) (= (length (ref spec 'layers)) 5)))", pass, fail);
    test_truthy(interp, "nn/validate accepts transparent model bundle",
        "(let (model (Dictionary 'kind 'model 'spec (nn/dense 2 1) 'params (Dictionary) 'state (Dictionary) 'mode 'eval 'dtype 'Float32 'device 'vulkan 'metadata (Dictionary))) (= (ref (nn/validate model) 'kind) 'model))", pass, fail);
    test_truthy(interp, "nn/validate accepts parameter and state tree wrappers",
        "(and (= (ref (nn/validate (Dictionary 'kind 'parameters 'tree (Dictionary))) 'kind) 'parameters) (= (ref (nn/validate (Dictionary 'kind 'state 'tree (Dictionary))) 'kind) 'state))", pass, fail);
    test_truthy(interp, "nn/validate rejects dense feature diagnostics",
        "(= (handle (nn/validate (Dictionary 'kind 'dense 'input-features 0 'output-features 2)) (raise msg (ref msg 'code))) 'nn/invalid-spec)", pass, fail);
    test_truthy(interp, "nn/validate rejects all-padding pool diagnostics",
        "(= (handle (nn/max-pool2d [2 2] [1 1] [2 0]) (raise msg (ref msg 'code))) 'nn/invalid-spec)", pass, fail);
    test_truthy(interp, "nn/validate rejects scalar parameter tree diagnostics",
        "(= (handle (nn/validate (Dictionary 'kind 'model 'spec (nn/dense 2 1) 'params 42 'state (Dictionary) 'mode 'eval 'dtype 'Float32 'device 'cpu)) (raise msg (ref msg 'code))) 'nn/invalid-spec)", pass, fail);
    test_truthy(interp, "nn/init dense params/state deterministic Float32 CPU ones zeros",
        "(let (model (nn/init (nn/dense 2 3 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) params (ref model 'params) weights (ref params 'weights) bias (ref params 'bias) state (ref model 'state)) (and (= (ref model 'kind) 'model) (= (String (ref model 'dtype)) \"Float32\") (= (ref model 'device) 'cpu) (= (length state) 0) (= (length (shape weights)) 2) (= (ref (shape weights) 0) 3) (= (ref (shape weights) 1) 2) (= (ref (shape bias) 0) 3) (= (ref weights [0 0]) (Float32 1.0)) (= (ref weights [0 1]) (Float32 1.0)) (= (ref weights [1 0]) (Float32 1.0)) (= (ref weights [1 1]) (Float32 1.0)) (= (ref weights [2 0]) (Float32 1.0)) (= (ref weights [2 1]) (Float32 1.0)) (= (ref bias [0]) (Float32 0.0)) (= (ref bias [1]) (Float32 0.0)) (= (ref bias [2]) (Float32 0.0)) (= (length params) 2)))",
        pass, fail);
    test_truthy(interp, "nn/init same-seed deterministic uniform Float32 CPU",
        "(let (spec (nn/dense 3 2 (Dictionary 'kernel-init (Dictionary 'kind 'uniform))) opts (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1337) model-a (nn/init spec opts) model-b (nn/init spec opts) weights-a (ref (ref model-a 'params) 'weights) weights-b (ref (ref model-b 'params) 'weights)) (and (= (ref (shape weights-a) 0) 2) (= (ref (shape weights-a) 1) 3) (= (ref (shape weights-b) 0) 2) (= (ref (shape weights-b) 1) 3) (= (length (ref model-a 'state)) 0) (= (ref weights-a [0 0]) (ref weights-b [0 0])) (= (ref weights-a [0 1]) (ref weights-b [0 1])) (= (ref weights-a [0 2]) (ref weights-b [0 2])) (= (ref weights-a [1 0]) (ref weights-b [1 0])) (= (ref weights-a [1 1]) (ref weights-b [1 1])) (= (ref weights-a [1 2]) (ref weights-b [1 2]))))",
        pass, fail);
    test_truthy(interp, "nn/init sequential params/state arrays preserve non-param activation empties",
        "(let (model (nn/init (nn/sequential (nn/relu) (nn/dense 2 1 (Dictionary 'kernel-init (Dictionary 'kind 'uniform))) (nn/relu)) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) params (ref model 'params) state (ref model 'state) dense-weights (ref (ref params 1) 'weights)) (and (= (length params) 3) (= (length state) 3) (= (length (ref params 0)) 0) (= (length (ref state 0)) 0) (= (length (ref params 2)) 0) (= (length (ref state 2)) 0) (= (length (ref params 1)) 2) (= (length (ref state 1)) 0) (= (ref (shape dense-weights) 0) 1) (= (ref (shape dense-weights) 1) 2) (= (ref (shape (ref (ref params 1) 'bias)) 0) 1)))",
        pass, fail);
    test_truthy(interp, "nn/init conv1d honors initializer options and bias",
        "(let (model (nn/init (nn/conv1d 2 4 3 1 0 1 2 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) params (ref model 'params) weights (ref params 'weights) bias (ref params 'bias)) (and (= (ref (shape weights) 0) 4) (= (ref (shape weights) 1) 1) (= (ref (shape weights) 2) 3) (= (ref (shape bias) 0) 4) (= (ref weights [0 0 0]) (Float32 1.0)) (= (ref weights [3 0 2]) (Float32 1.0)) (= (ref bias [0]) (Float32 0.0)) (= (ref bias [3]) (Float32 0.0))))",
        pass, fail);
    test_truthy(interp, "nn/init diagnostics rejects unsupported dtype",
        "(= (handle (nn/init (nn/dense 2 1) (Dictionary 'dtype 'BigInteger)) (raise msg (ref msg 'code))) 'nn/invalid-spec)", pass, fail);
    test_truthy(interp, "nn/init diagnostics rejects negative seed",
        "(= (handle (nn/init (nn/dense 2 1) (Dictionary 'seed -1)) (raise msg (ref msg 'code))) 'nn/invalid-spec)", pass, fail);
    test_truthy(interp, "nn/accessors report model components",
        "(let (model (nn/init (nn/dense 2 3 (Dictionary 'kernel-init (Dictionary 'kind 'ones))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) spec (nn/spec model) params (nn/parameters model) state (nn/state model) mode (nn/mode model)) (and (= (ref spec 'kind) 'dense) (= (ref spec 'input-features) 2) (= (length params) 2) (= (length state) 0) (= mode 'eval)))",
        pass, fail);
    test_truthy(interp, "nn/apply explicit spec params state dense",
        "(let (spec (nn/dense 4 2) params (Dictionary 'weights (Tensor Float32 [2 4] [1 2 3 4 5 6 7 8]) 'bias (Tensor Float32 [2] [1 1])) state (Dictionary) input (Tensor Float32 [4] [1 1 1 1]) out (nn/apply spec params state input)) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 11.0)) (= (ref out [1]) (Float32 27.0))))",
        pass, fail);
    test_truthy(interp, "nn/apply explicit CPU flatten+dense sequential",
        "(let (spec (nn/sequential (nn/flatten 1 2) (nn/dense 4 2 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros)))) params (Array (Dictionary) (Dictionary 'weights (Tensor Float32 [2 4] [1 1 1 1 1 2 3 4]) 'bias (Tensor Float32 [2] [0 0])) ) state (Array (Dictionary) (Dictionary)) input (Tensor Float32 [1 2 2] [1 2 3 4]) out (nn/apply spec params state input)) (and (= (ref (shape out) 0) 1) (= (ref (shape out) 1) 2) (= (ref out [0 0]) (Float32 10.0)) (= (ref out [0 1]) (Float32 30.0))))",
        pass, fail);
    test_truthy(interp, "nn/apply accepts empty reserved options",
        "(let (spec (nn/dense 1 1) params (Dictionary 'weights (Tensor Float32 [1 1] [2]) 'bias (Tensor Float32 [1] [1])) out (nn/apply spec params (Dictionary) (Tensor Float32 [1] [3]) (Dictionary))) (= (ref out [0]) (Float32 7.0)))",
        pass, fail);
    test_truthy(interp, "nn/apply rejects non-empty reserved options",
        "(= (handle (nn/apply (nn/dense 1 1) (Dictionary 'weights (Tensor Float32 [1 1] [2]) 'bias (Tensor Float32 [1] [1])) (Dictionary) (Tensor Float32 [1] [3]) (Dictionary 'training true)) (raise msg (ref msg 'code))) 'nn/invalid-spec)",
        pass, fail);
    test_truthy(interp, "nn/summary reports stable model counts",
        "(let (model (nn/init (nn/dense 3 2) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) summary (nn/summary model)) (and (= (ref summary 'kind) 'summary) (= (ref summary 'layers) 1) (= (ref summary 'parameter-tensors) 2) (= (ref summary 'parameter-elements) 8) (= (ref summary 'mode) 'eval) (= (String (ref summary 'dtype)) \"Float32\")))",
        pass, fail);
    test_truthy(interp, "nn/summary rejects malformed params tree",
        "(= (handle (nn/summary (Dictionary 'kind 'model 'spec (nn/dense 2 1) 'params 42 'state (Dictionary) 'mode 'eval 'dtype 'Float32 'device 'cpu 'metadata (Dictionary))) (raise msg (ref msg 'code))) 'nn/invalid-spec)",
        pass, fail);
    test_truthy(interp, "nn/predict executes dense model in eval mode",
        "(let (model (nn/init (nn/dense 2 1 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) out (nn/predict model (Tensor Float32 [2] [1 2]))) (and (= (String (dtype out)) \"Float32\") (= (ref out [0]) (Float32 3.0))))",
        pass, fail);
    test_truthy(interp, "nn/predict rejects non-eval mode",
        "(= (handle (nn/predict (Dictionary 'kind 'model 'spec (nn/dense 2 1) 'params (Dictionary 'weights (Tensor Float32 [1 2] [1 1]) 'bias (Tensor Float32 [1] [0])) 'state (Dictionary) 'mode 'train 'dtype 'Float32 'device 'cpu 'metadata (Dictionary)) (Tensor Float32 [2] [1 1])) (raise msg (ref msg 'code))) 'nn/invalid-spec)",
        pass, fail);
    test_truthy(interp, "nn/save-spec and nn/load-spec round trip dense spec",
        "(let (spec (nn/load-spec (nn/save-spec (nn/dense 2 1 (Dictionary 'activation 'relu))))) (and (= (ref spec 'kind) 'dense) (= (ref spec 'input-features) 2) (= (ref spec 'output-features) 1) (= (ref spec 'activation) 'relu)))",
        pass, fail);
    test_truthy(interp, "nn/save and nn/load round trip model parameters",
        "(let (model (nn/load (nn/save (nn/init (nn/dense 2 1 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)))) out (nn/predict model (Tensor Float32 [2] [1 2]))) (and (= (ref model 'kind) 'model) (= (ref model 'device) 'cpu) (= (ref out [0]) (Float32 3.0))))",
        pass, fail);
    test_truthy(interp, "nn/save and nn/load round trip checkpoint path",
        "(let (path \"/tmp/omni-nn-checkpoint-test.json\" model (nn/init (nn/dense 2 1 (Dictionary 'kernel-init (Dictionary 'kind 'ones) 'bias-init (Dictionary 'kind 'zeros))) (Dictionary 'dtype 'Float32 'device 'cpu 'seed 1)) saved (nn/save model path) restored (nn/load path) cleanup (fs-unlink path) out (nn/predict restored (Tensor Float32 [2] [1 2]))) (and (= (ref restored 'kind) 'model) (= (ref out [0]) (Float32 3.0))))",
        pass, fail);
    test_truthy(interp, "nn/load-spec rejects model checkpoint payload",
        "(= (handle (nn/load-spec (nn/save (nn/init (nn/dense 1 1) (Dictionary 'dtype 'Float32 'device 'cpu)))) (raise msg (ref msg 'code))) 'nn/invalid-spec)",
        pass, fail);
    test_truthy(interp, "nn/load rejects spec checkpoint payload",
        "(= (handle (nn/load (nn/save-spec (nn/dense 1 1))) (raise msg (ref msg 'code))) 'nn/invalid-spec)",
        pass, fail);
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
    test_truthy(interp, "ml reductions rank-3 middle axis preserve output indexing",
        "(let (sum-out (ml/sum (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 1) log-out (ml/logsumexp (Tensor Float64 [2 3 4] [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]) 1) soft-out (ml/softmax (Tensor Float64 [1 3 2] [0 0 0 0 0 0]) 1)) (and (= (rank sum-out) 2) (= (ref (shape sum-out) 0) 2) (= (ref (shape sum-out) 1) 4) (= (ref sum-out [0 0]) 15.0) (= (ref sum-out [1 3]) 60.0) (< (abs (- (ref log-out [1 2]) (log 3.0))) 0.000000001) (< (abs (- (ref soft-out [0 0 1]) (/ 1.0 3.0))) 0.000000001) (< (abs (- (ref soft-out [0 2 1]) (/ 1.0 3.0))) 0.000000001)))", pass, fail);
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
    test_truthy(interp, "ml reductions Vulkan Float64 and Float32 stay on device and match CPU",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'float64) (let (sum-out (ml/sum (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) 0) mean-out (ml/mean (to-device (Tensor Float64 [2 2] [1 2 3 5]) 'vulkan) -1) variance-out (ml/variance (to-device (Tensor Float64 [2 2] [1 3 2 6]) 'vulkan) [1])) (and (= (device sum-out) 'vulkan) (= (device mean-out) 'vulkan) (= (device variance-out) 'vulkan) (= (String (dtype sum-out)) \"Float64\") (= (String (dtype mean-out)) \"Float64\") (= (String (dtype variance-out)) \"Float64\") (= (ref (to-device sum-out 'cpu) [0]) 5.0) (= (ref (to-device sum-out 'cpu) [2]) 9.0) (= (ref (to-device mean-out 'cpu) [0]) 1.5) (= (ref (to-device mean-out 'cpu) [1]) 4.0) (= (ref (to-device variance-out 'cpu) [0]) 1.0) (= (ref (to-device variance-out 'cpu) [1]) 4.0))) true) (if (ref vulkan-backend 'float32) (let (sum-out (ml/sum (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'vulkan) 0) mean-out (ml/mean (to-device (Tensor Float32 [2 2] [1 2 3 5]) 'vulkan) -1) variance-out (ml/variance (to-device (Tensor Float32 [2 2] [1 3 2 6]) 'vulkan) [1])) (and (= (device sum-out) 'vulkan) (= (device mean-out) 'vulkan) (= (device variance-out) 'vulkan) (= (String (dtype sum-out)) \"Float32\") (= (String (dtype mean-out)) \"Float32\") (= (String (dtype variance-out)) \"Float32\") (= (ref (to-device sum-out 'cpu) [0]) (Float32 5.0)) (= (ref (to-device sum-out 'cpu) [2]) (Float32 9.0)) (= (ref (to-device mean-out 'cpu) [0]) (Float32 1.5)) (= (ref (to-device mean-out 'cpu) [1]) (Float32 4.0)) (= (ref (to-device variance-out 'cpu) [0]) (Float32 1.0)) (= (ref (to-device variance-out 'cpu) [1]) (Float32 4.0)))) true)))", pass, fail);

    test_truthy(interp, "ml/logsumexp Float64 multi-axis stable",
        "(let (out (ml/logsumexp (Tensor Float64 [2 2] [1000 1000 1000 1000]) [0 1])) (< (abs (- (ref out []) (+ 1000.0 (log 4.0)))) 0.000000001))", pass, fail);
    test_truthy(interp, "ml/logsumexp Float32 negative axis preserves dtype",
        "(let (out (ml/logsumexp (Tensor Float32 [1 2] [0 0]) -1)) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out [0]) (log 2.0))) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/logsumexp Vulkan Float32 stays on device and matches CPU",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (row-out (ml/logsumexp (to-device (Tensor Float32 [2 2] [1000 1000 0 0]) 'vulkan) 1) row-cpu (to-device row-out 'cpu) all-out (ml/logsumexp (to-device (Tensor Float32 [2 2] [0 0 0 0]) 'vulkan) [0 1]) all-cpu (to-device all-out 'cpu)) (and (= (device row-out) 'vulkan) (= (String (dtype row-out)) \"Float32\") (< (abs (- (ref row-cpu [0]) (+ 1000.0 (log 2.0)))) 0.0001) (< (abs (- (ref row-cpu [1]) (log 2.0))) 0.0001) (= (device all-out) 'vulkan) (= (rank all-cpu) 0) (< (abs (- (ref all-cpu []) (log 4.0))) 0.0001))) true))", pass, fail);
    test_truthy(interp, "ml/softmax Float64 large logits stable",
        "(let (out (ml/softmax (Tensor Float64 [2] [1000 1000]) 0)) (and (= (rank out) 1) (< (abs (- (ref out [0]) 0.5)) 0.000000001) (< (abs (- (ref out [1]) 0.5)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/softmax Float32 row axis preserves shape and sums",
        "(let (out (ml/softmax (Tensor Float32 [2 2] [0 0 1 1]) 1)) (and (= (String (dtype out)) \"Float32\") (= (ref (shape out) 0) 2) (= (ref (shape out) 1) 2) (< (abs (- (+ (ref out [0 0]) (ref out [0 1])) 1.0)) 0.0001) (< (abs (- (+ (ref out [1 0]) (ref out [1 1])) 1.0)) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/softmax Vulkan Float32 stays on device and normalizes rows",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/softmax (to-device (Tensor Float32 [2 3] [1000 1000 1000 1 2 3]) 'vulkan) 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref (shape cpu) 0) 2) (= (ref (shape cpu) 1) 3) (< (abs (- (+ (ref cpu [0 0]) (ref cpu [0 1]) (ref cpu [0 2])) 1.0)) 0.0001) (< (abs (- (+ (ref cpu [1 0]) (ref cpu [1 1]) (ref cpu [1 2])) 1.0)) 0.0001) (< (abs (- (ref cpu [0 0]) (/ 1.0 3.0))) 0.0001) (> (ref cpu [1 2]) (ref cpu [1 1])) (> (ref cpu [1 1]) (ref cpu [1 0])))) true))", pass, fail);
    test_truthy(interp, "ml/softmax Vulkan Float32 supports column and negative axes",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (col (to-device (ml/softmax (to-device (Tensor Float32 [2 2] [0 1 0 1]) 'vulkan) 0) 'cpu) neg (to-device (ml/softmax (to-device (Tensor Float32 [1 2] [0 0]) 'vulkan) -1) 'cpu)) (and (< (abs (- (+ (ref col [0 0]) (ref col [1 0])) 1.0)) 0.0001) (< (abs (- (+ (ref col [0 1]) (ref col [1 1])) 1.0)) 0.0001) (< (abs (- (ref col [0 0]) 0.5)) 0.0001) (< (abs (- (ref col [1 1]) 0.5)) 0.0001) (< (abs (- (ref neg [0 0]) 0.5)) 0.0001) (< (abs (- (ref neg [0 1]) 0.5)) 0.0001))) true))", pass, fail);
    test_truthy(interp, "ml/softmax Vulkan Float32 rank-3 middle axis",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (cpu (to-device (ml/softmax (to-device (Tensor Float32 [2 2 2] [0 10 1 11 2 20 3 21]) 'vulkan) 1) 'cpu)) (and (< (abs (- (+ (ref cpu [0 0 0]) (ref cpu [0 1 0])) 1.0)) 0.0001) (< (abs (- (+ (ref cpu [0 0 1]) (ref cpu [0 1 1])) 1.0)) 0.0001) (< (abs (- (+ (ref cpu [1 0 0]) (ref cpu [1 1 0])) 1.0)) 0.0001) (< (abs (- (+ (ref cpu [1 0 1]) (ref cpu [1 1 1])) 1.0)) 0.0001) (> (ref cpu [0 1 0]) (ref cpu [0 0 0])) (> (ref cpu [1 1 1]) (ref cpu [1 0 1])))) true))", pass, fail);
    test_truthy(interp, "ml/layer-normalization Float64 row axis preserves shape and centers rows",
        "(let (out (ml/layer-normalization (Tensor Float64 [2 3] [1 2 3 2 4 6]) 1) row0 (+ (+ (ref out [0 0]) (ref out [0 1])) (ref out [0 2])) row1 (+ (+ (ref out [1 0]) (ref out [1 1])) (ref out [1 2]))) (and (= (rank out) 2) (= (ref (shape out) 0) 2) (= (ref (shape out) 1) 3) (< (abs row0) 0.000000001) (< (abs row1) 0.000000001) (< (abs (ref out [0 1])) 0.000000001) (< (abs (ref out [1 1])) 0.000000001) (> (ref out [0 2]) 1.2247) (< (ref out [0 0]) -1.2247)))", pass, fail);
    test_truthy(interp, "ml/layer-normalization Float32 negative axis preserves dtype",
        "(let (out (ml/layer-normalization (Tensor Float32 [1 3] [1 2 3]) -1) row (+ (+ (ref out [0 0]) (ref out [0 1])) (ref out [0 2]))) (and (= (String (dtype out)) \"Float32\") (< (abs row) 0.0001) (< (abs (ref out [0 1])) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/layer-normalization custom epsilon changes scale",
        "(let (out (ml/layer-normalization (Tensor Float64 [2] [1 3]) 0 1.0)) (and (< (abs (+ (ref out [0]) 0.7071067811865475)) 0.000000001) (< (abs (- (ref out [1]) 0.7071067811865475)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/layer-normalization Vulkan Float32 stays on device and centers rows",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-layer-normalization-float32) (let (out (ml/layer-normalization (to-device (Tensor Float32 [2 3] [1 2 3 2 4 6]) 'vulkan) 1) cpu (to-device out 'cpu) row0 (+ (+ (ref cpu [0 0]) (ref cpu [0 1])) (ref cpu [0 2])) row1 (+ (+ (ref cpu [1 0]) (ref cpu [1 1])) (ref cpu [1 2]))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs row0) 0.0001) (< (abs row1) 0.0001) (> (ref cpu [0 2]) 1.224) (< (ref cpu [0 0]) -1.224))) true))", pass, fail);
    test_truthy(interp, "ml/layer-normalization rejects invalid inputs",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/layer-normalization (Tensor BigInteger [2] [1 2]) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/layer-normalization (Tensor Float64 [2] [1 2]) 0 0.0) (raise msg (ref msg 'code))) 'tensor/domain-error) (= (handle (ml/layer-normalization (Tensor Float64 [2] [1 2]) [0]) (raise msg (ref msg 'code))) 'type/arg-mismatch) (if (ref vulkan-backend 'float64) (= (handle (ml/layer-normalization (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/batch-normalization Float64 explicit stats channel axis",
        "(let (out (ml/batch-normalization (Tensor Float64 [1 2 1 2] [2 4 10 14]) (Tensor Float64 [2] [1 1]) (Tensor Float64 [2] [0 0]) (Tensor Float64 [2] [3 12]) (Tensor Float64 [2] [1 4]) 1 1.0)) (and (= (ref (shape out) 1) 2) (< (abs (+ (ref out [0 0 0 0]) 0.7071067811865475)) 0.000000001) (< (abs (- (ref out [0 1 0 1]) 0.8944271909999159)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/batch-normalization Float32 negative axis preserves dtype",
        "(let (out (ml/batch-normalization (Tensor Float32 [1 3] [1 2 3]) (Tensor Float32 [3] [1 2 3]) (Tensor Float32 [3] [0.5 0.5 0.5]) (Tensor Float32 [3] [1 2 3]) (Tensor Float32 [3] [1 1 1]) -1 1.0)) (and (= (String (dtype out)) \"Float32\") (= (ref out [0 0]) (Float32 0.5)) (= (ref out [0 2]) (Float32 0.5))))", pass, fail);
    test_truthy(interp, "ml/batch-normalization Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-batch-normalization-float32) (let (out (ml/batch-normalization (to-device (Tensor Float32 [1 2 1 2] [2 4 10 14]) 'vulkan) (to-device (Tensor Float32 [2] [1 1]) 'vulkan) (to-device (Tensor Float32 [2] [0 0]) 'vulkan) (to-device (Tensor Float32 [2] [3 12]) 'vulkan) (to-device (Tensor Float32 [2] [1 4]) 'vulkan) 1 1.0) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (+ (ref cpu [0 0 0 0]) 0.7071067)) 0.0001) (< (abs (- (ref cpu [0 1 0 1]) 0.8944272)) 0.0001))) true))", pass, fail);
    test_truthy(interp, "ml/batch-normalization rejects invalid CPU contract",
        "(and (= (handle (ml/batch-normalization (Tensor BigInteger [1] [1]) (Tensor BigInteger [1] [1]) (Tensor BigInteger [1] [0]) (Tensor BigInteger [1] [0]) (Tensor BigInteger [1] [1]) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/batch-normalization (Tensor Float64 [1 2] [1 2]) (Tensor Float64 [1] [1]) (Tensor Float64 [1] [0]) (Tensor Float64 [1] [0]) (Tensor Float64 [1] [1]) 1) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/batch-normalization (Tensor Float64 [1] [1]) (Tensor Float64 [1] [1]) (Tensor Float64 [1] [0]) (Tensor Float64 [1] [0]) (Tensor Float64 [1] [1]) 0 0.0) (raise msg (ref msg 'code))) 'tensor/domain-error))", pass, fail);
    test_truthy(interp, "ml/batch-normalization rejects unsupported Vulkan contracts",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'float32) (= (handle (ml/batch-normalization (to-device (Tensor Float32 [1] [1]) 'vulkan) (Tensor Float32 [1] [1]) (to-device (Tensor Float32 [1] [0]) 'vulkan) (to-device (Tensor Float32 [1] [0]) 'vulkan) (to-device (Tensor Float32 [1] [1]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) (if (ref vulkan-backend 'float64) (= (handle (ml/batch-normalization (to-device (Tensor Float64 [1] [1]) 'vulkan) (to-device (Tensor Float64 [1] [1]) 'vulkan) (to-device (Tensor Float64 [1] [0]) 'vulkan) (to-device (Tensor Float64 [1] [0]) 'vulkan) (to-device (Tensor Float64 [1] [1]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/scaled-dot-product-attention Float64 CPU stable",
        "(let (out (ml/scaled-dot-product-attention (Tensor Float64 [1 2] [1 2]) (Tensor Float64 [2 2] [1 0 0 1]) (Tensor Float64 [2 2] [10 20 30 40]) 1.0)) (and (= (rank out) 2) (= (ref (shape out) 0) 1) (= (ref (shape out) 1) 2) (< (abs (- (ref out [0 0]) 24.6211715726)) 0.000001) (< (abs (- (ref out [0 1]) 34.6211715726)) 0.000001)))", pass, fail);
    test_truthy(interp, "ml/scaled-dot-product-attention Float32 CPU additive mask",
        "(let (out (ml/scaled-dot-product-attention (Tensor Float32 [1 1] [1]) (Tensor Float32 [2 1] [1 1]) (Tensor Float32 [2 1] [10 30]) (Tensor Float32 [1 2] [0 -1000]) 1.0)) (and (= (String (dtype out)) \"Float32\") (< (abs (- (ref out [0 0]) 10.0)) 0.0001)))", pass, fail);
    test_truthy(interp, "ml/scaled-dot-product-attention Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-scaled-dot-product-attention-float32) (let (out (ml/scaled-dot-product-attention (to-device (Tensor Float32 [1 2] [1 2]) 'vulkan) (to-device (Tensor Float32 [2 2] [1 0 0 1]) 'vulkan) (to-device (Tensor Float32 [2 2] [10 20 30 40]) 'vulkan) 1.0) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0 0]) 24.6212)) 0.0002) (< (abs (- (ref cpu [0 1]) 34.6212)) 0.0002))) true))", pass, fail);
    test_truthy(interp, "ml/scaled-dot-product-attention rejects invalid contracts",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/scaled-dot-product-attention (Tensor Float64 [1 1] [1]) (Tensor Float32 [1 1] [1]) (Tensor Float64 [1 1] [1])) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) (= (handle (ml/scaled-dot-product-attention (Tensor Float64 [1 1] [1]) (Tensor Float64 [1 1] [1]) (Tensor Float64 [1 1] [1]) (Tensor Float64 [2 2] [0 0 0 0])) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (if (ref vulkan-backend 'float32) (= (handle (ml/scaled-dot-product-attention (to-device (Tensor Float32 [1 1] [1]) 'vulkan) (Tensor Float32 [1 1] [1]) (to-device (Tensor Float32 [1 1] [1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) (if (ref vulkan-backend 'float64) (= (handle (ml/scaled-dot-product-attention (to-device (Tensor Float64 [1 1] [1]) 'vulkan) (to-device (Tensor Float64 [1 1] [1]) 'vulkan) (to-device (Tensor Float64 [1 1] [1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/grad linear MSE Float64 returns data gradients",
        "(let (g (ml/grad (Dictionary 'kind 'linear-mean-squared-error 'input (Tensor Float64 [2] [1 2]) 'weights (Tensor Float64 [1 2] [1 1]) 'bias (Tensor Float64 [1] [0]) 'targets (Tensor Float64 [1] [4]))) pg (ref g 'parameter-gradients)) (and (= (ref (ref g 'loss) []) 1.0) (= (ref (ref g 'output) [0]) 3.0) (= (ref (ref g 'input-gradient) [0]) -2.0) (= (ref (ref g 'input-gradient) [1]) -2.0) (= (ref (ref pg 'weights) [0 0]) -2.0) (= (ref (ref pg 'weights) [0 1]) -4.0) (= (ref (ref pg 'bias) [0]) -2.0)))", pass, fail);
    test_truthy(interp, "ml/grad linear MSE Float32 preserves dtype",
        "(let (g (ml/grad (Dictionary 'kind 'linear-mean-squared-error 'input (Tensor Float32 [1 2] [1 2]) 'weights (Tensor Float32 [1 2] [1 1]) 'targets (Tensor Float32 [1 1] [4]))) pg (ref g 'parameter-gradients)) (and (= (String (dtype (ref g 'loss))) \"Float32\") (= (ref (ref g 'loss) []) (Float32 1.0)) (= (ref (ref pg 'bias) [0]) (Float32 -2.0))))", pass, fail);
    test_truthy(interp, "ml/grad rejects unsupported specs and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/grad (Dictionary 'kind 'unknown)) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (= (handle (ml/grad (Dictionary 'kind 'linear-mean-squared-error 'input (Tensor Float64 [1] [1]) 'weights (Tensor Float64 [1 1] [1]) 'targets (Tensor Float64 [2] [1 2]))) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (if (ref vulkan-backend 'float32) (= (handle (ml/grad (Dictionary 'kind 'linear-mean-squared-error 'input (to-device (Tensor Float32 [1] [1]) 'vulkan) 'weights (Tensor Float32 [1 1] [1]) 'targets (Tensor Float32 [1] [1]))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/grad linear activation MSE relu Float64 gradients",
        "(let (g (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'relu 'input (Tensor Float64 [2] [1 2]) 'weights (Tensor Float64 [2 2] [-1 0 1 1]) 'bias (Tensor Float64 [2] [0 0]) 'targets (Tensor Float64 [2] [0 4]))) pg (ref g 'parameter-gradients)) (and (= (ref g 'source) 'linear-activation-mean-squared-error) (= (ref g 'activation) 'relu) (= (ref (ref g 'loss) []) 0.5) (= (ref (ref g 'output) [0]) 0.0) (= (ref (ref g 'output) [1]) 3.0) (= (ref (ref g 'input-gradient) [0]) -1.0) (= (ref (ref g 'input-gradient) [1]) -1.0) (= (ref (ref pg 'weights) [1 0]) -1.0) (= (ref (ref pg 'weights) [1 1]) -2.0) (= (ref (ref pg 'bias) [0]) 0.0) (= (ref (ref pg 'bias) [1]) -1.0)))", pass, fail);
    test_truthy(interp, "ml/grad linear activation MSE Float32 sigmoid tanh gelu",
        "(let (s (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'sigmoid 'input (Tensor Float32 [1] [1]) 'weights (Tensor Float32 [1 1] [0]) 'targets (Tensor Float32 [1] [1]))) t (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'tanh 'input (Tensor Float32 [1] [1]) 'weights (Tensor Float32 [1 1] [0]) 'targets (Tensor Float32 [1] [1]))) g (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'gelu 'input (Tensor Float32 [1] [1]) 'weights (Tensor Float32 [1 1] [0]) 'targets (Tensor Float32 [1] [1])))) (and (= (ref (ref s 'output) [0]) (Float32 0.5)) (= (ref (ref (ref s 'parameter-gradients) 'bias) [0]) (Float32 -0.25)) (= (ref (ref (ref t 'parameter-gradients) 'bias) [0]) (Float32 -2.0)) (= (ref (ref (ref g 'parameter-gradients) 'bias) [0]) (Float32 -1.0))))", pass, fail);
    test_truthy(interp, "ml/grad linear activation MSE rejects unsupported activation contracts",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'sigmoid 'input (Tensor Float64 [1] [1]) 'weights (Tensor Float64 [1 1] [0]) 'targets (Tensor Float64 [1] [1]))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'bogus 'input (Tensor Float32 [1] [1]) 'weights (Tensor Float32 [1 1] [0]) 'targets (Tensor Float32 [1] [1]))) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (if (ref vulkan-backend 'float32) (= (handle (ml/grad (Dictionary 'kind 'linear-activation-mean-squared-error 'activation 'relu 'input (to-device (Tensor Float32 [1] [1]) 'vulkan) 'weights (Tensor Float32 [1 1] [1]) 'targets (Tensor Float32 [1] [1]))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/grad linear softmax cross entropy Float64 gradients",
        "(let (g (ml/grad (Dictionary 'kind 'linear-softmax-cross-entropy 'input (Tensor Float64 [2] [1 2]) 'weights (Tensor Float64 [2 2] [1 0 0 1]) 'bias (Tensor Float64 [2] [0 0]) 'targets (Tensor Float64 [2] [0 1]))) pg (ref g 'parameter-gradients)) (and (= (ref g 'source) 'linear-softmax-cross-entropy) (< (abs (- (ref (ref g 'loss) []) 0.3132616875)) 0.000000001) (< (abs (- (ref (ref g 'output) [1]) 0.7310585786)) 0.000000001) (< (abs (- (ref (ref g 'input-gradient) [0]) 0.2689414214)) 0.000000001) (< (abs (+ (ref (ref g 'input-gradient) [1]) 0.2689414214)) 0.000000001) (< (abs (+ (ref (ref pg 'weights) [1 1]) 0.5378828428)) 0.000000001) (< (abs (+ (ref (ref pg 'bias) [1]) 0.2689414214)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/grad linear softmax cross entropy Float32 preserves dtype",
        "(let (g (ml/grad (Dictionary 'kind 'linear-softmax-cross-entropy 'input (Tensor Float32 [1] [1]) 'weights (Tensor Float32 [2 1] [0 0]) 'targets (Tensor Float32 [2] [1 0]))) pg (ref g 'parameter-gradients)) (and (= (String (dtype (ref g 'loss))) \"Float32\") (< (abs (- (ref (ref g 'loss) []) (Float32 0.69314718))) (Float32 0.0001)) (= (ref (ref (ref g 'parameter-gradients) 'bias) [0]) (Float32 -0.5)) (= (ref (ref pg 'weights) [1 0]) (Float32 0.5))))", pass, fail);
    test_truthy(interp, "ml/grad linear softmax cross entropy rejects invalid targets and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/grad (Dictionary 'kind 'linear-softmax-cross-entropy 'input (Tensor Float64 [1] [1]) 'weights (Tensor Float64 [2 1] [0 0]) 'targets (Tensor Float64 [2] [1 1]))) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (if (ref vulkan-backend 'float32) (= (handle (ml/grad (Dictionary 'kind 'linear-softmax-cross-entropy 'input (to-device (Tensor Float32 [1] [1]) 'vulkan) 'weights (Tensor Float32 [2 1] [0 0]) 'targets (Tensor Float32 [2] [1 0]))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/sgd-step Float64 dictionary update is functional",
        "(let (p (Dictionary 'weights (Tensor Float64 [2] [1 2]) 'bias (Tensor Float64 [1] [3])) g (Dictionary 'weights (Tensor Float64 [2] [0.5 -1]) 'bias (Tensor Float64 [1] [2])) out (ml/sgd-step p g 0.1)) (and (< (abs (- (ref (ref out 'weights) [0]) 0.95)) 0.000000001) (< (abs (- (ref (ref out 'weights) [1]) 2.1)) 0.000000001) (< (abs (- (ref (ref out 'bias) [0]) 2.8)) 0.000000001) (= (ref (ref p 'weights) [0]) 1.0)))", pass, fail);
    test_truthy(interp, "ml/sgd-step recurses through Float32 sequential parameter trees",
        "(let (out (ml/sgd-step (Array (Dictionary 'weights (Tensor Float32 [1] [2]))) (Array (Dictionary 'weights (Tensor Float32 [1] [0.5]))) 0.5)) (and (= (String (dtype (ref (ref out 0) 'weights))) \"Float32\") (= (ref (ref (ref out 0) 'weights) [0]) (Float32 1.75))))", pass, fail);
    test_truthy(interp, "ml/sgd-step rejects tree mismatch and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/sgd-step (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary 'b (Tensor Float32 [1] [1])) 0.1) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (if (ref vulkan-backend 'float32) (= (handle (ml/sgd-step (Dictionary 'w (to-device (Tensor Float32 [1] [1]) 'vulkan)) (Dictionary 'w (Tensor Float32 [1] [1])) 0.1) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step SGD momentum initializes state",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 0.1 'momentum 0.9) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.5])) (Dictionary)) p (ref r 'parameters) v (ref (ref (ref r 'state) 'velocity) 'w)) (and (= (ref r 'kind) 'optimizer-step) (< (abs (- (ref (ref p 'w) [0]) 0.95)) 0.000000001) (= (ref v [0]) 0.5)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step SGD momentum consumes prior state",
        "(let (state (Dictionary 'velocity (Dictionary 'w (Tensor Float64 [1] [0.5]))) r (ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 0.1 'momentum 0.9) (Dictionary 'w (Tensor Float64 [1] [0.95])) (Dictionary 'w (Tensor Float64 [1] [0.5])) state) p (ref r 'parameters) v (ref (ref (ref r 'state) 'velocity) 'w)) (and (< (abs (- (ref (ref p 'w) [0]) 0.855)) 0.000000001) (< (abs (- (ref v [0]) 0.95)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step SGD weight decay without momentum is stateless",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 0.1 'weight-decay 0.1) (Dictionary 'w (Tensor Float32 [1] [2])) (Dictionary 'w (Tensor Float32 [1] [0.5])) (Dictionary)) p (ref r 'parameters)) (and (= (length (ref r 'state)) 0) (< (abs (- (ref (ref p 'w) [0]) (Float32 1.93))) (Float32 0.0001))))", pass, fail);
    test_truthy(interp, "ml/optimizer-step Adam initializes moments and step",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.1])) (Dictionary)) st (ref r 'state) p (ref (ref r 'parameters) 'w) m (ref (ref st 'first-moment) 'w) v (ref (ref st 'second-moment) 'w)) (and (= (ref r 'optimizer) 'adam) (= (ref st 'step) 1) (< (abs (- (ref p [0]) 0.90000001)) 0.000000001) (< (abs (- (ref m [0]) 0.01)) 0.000000001) (< (abs (- (ref v [0]) 0.00001)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step AdamW decouples weight decay",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'adamw 'learning-rate 0.1 'weight-decay 0.1) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0])) (Dictionary)) st (ref r 'state) p (ref (ref r 'parameters) 'w)) (and (= (ref r 'optimizer) 'adamw) (= (ref st 'step) 1) (< (abs (- (ref p [0]) 0.99)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step Adam consumes prior state and validates schema",
        "(let (r1 (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.1])) (Dictionary)) st1 (ref r1 'state) r2 (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1) (ref r1 'parameters) (Dictionary 'w (Tensor Float64 [1] [0.1])) st1) st2 (ref r2 'state) m2 (ref (ref st2 'first-moment) 'w)) (and (= (ref st2 'step) 2) (< (abs (- (ref m2 [0]) 0.019)) 0.000000001) (= (handle (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1 'beta1 1.0) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.1])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (= (handle (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.1])) (Dictionary 'first-moment (Dictionary 'w (Tensor Float64 [1] [0])))) (raise msg (ref msg 'code))) 'tensor/invalid-argument)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step RMSProp initializes square average",
        "(let (r (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1 'alpha 0.0) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.5])) (Dictionary)) st (ref r 'state) p (ref (ref r 'parameters) 'w) s (ref (ref st 'square-average) 'w)) (and (= (ref r 'optimizer) 'rmsprop) (= (ref st 'step) 1) (< (abs (- (ref p [0]) 0.900000002)) 0.000000001) (< (abs (- (ref s [0]) 0.25)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step RMSProp momentum consumes state",
        "(let (r1 (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1 'alpha 0.0 'momentum 0.9) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.5])) (Dictionary)) r2 (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1 'alpha 0.0 'momentum 0.9) (ref r1 'parameters) (Dictionary 'w (Tensor Float64 [1] [0.5])) (ref r1 'state)) st2 (ref r2 'state) p2 (ref (ref r2 'parameters) 'w) v2 (ref (ref st2 'velocity) 'w)) (and (= (ref st2 'step) 2) (< (abs (- (ref p2 [0]) 0.7100000058)) 0.000000001) (< (abs (- (ref v2 [0]) 0.1899999962)) 0.000000001)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step RMSProp validates spec state and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1 'alpha 1.0) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.5])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (= (handle (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1) (Dictionary 'w (Tensor Float64 [1] [1])) (Dictionary 'w (Tensor Float64 [1] [0.5])) (Dictionary 'square-average (Dictionary 'w (Tensor Float64 [2] [0 0])))) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (if (ref vulkan-backend 'float32) (= (handle (ml/optimizer-step (Dictionary 'kind 'rmsprop 'learning-rate 0.1) (Dictionary 'w (to-device (Tensor Float32 [1] [1]) 'vulkan)) (Dictionary 'w (Tensor Float32 [1] [0.5])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/optimizer-step rejects invalid spec and Adam Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/optimizer-step (Dictionary 'kind 'bogus 'learning-rate 0.1) (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (= (handle (ml/optimizer-step (Dictionary 'kind 'sgd 'learning-rate 0.1 'momentum 1.0) (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (if (ref vulkan-backend 'float32) (= (handle (ml/optimizer-step (Dictionary 'kind 'adam 'learning-rate 0.1) (Dictionary 'w (to-device (Tensor Float32 [1] [1]) 'vulkan)) (Dictionary 'w (Tensor Float32 [1] [1])) (Dictionary)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
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
    test_truthy(interp, "ml/mean-squared-error Vulkan Float64 and Float32 stay on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'float64) (let (out (ml/mean-squared-error (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [1 1 5]) 'vulkan)) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (rank cpu) 0) (< (abs (- (ref cpu []) (/ 5.0 3.0))) 0.000000001))) true) (if (ref vulkan-backend 'float32) (let (out (ml/mean-squared-error (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float32 [3] [1 1 5]) 'vulkan)) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (rank cpu) 0) (< (abs (- (ref cpu []) (/ 5.0 3.0))) 0.0001))) true)))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error Vulkan Float32 overflow matches CPU",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/mean-squared-error (to-device (Tensor Float32 [1] [1.0e20]) 'vulkan) (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/numeric-overflow) true))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error Vulkan staged reductions stay on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'float64) (let (out (ml/mean-squared-error (to-device (Tensor Float64 [300] 1.0) 'vulkan) (to-device (Tensor Float64 [300] 0.0) 'vulkan)) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (rank cpu) 0) (< (abs (- (ref cpu []) 1.0)) 0.000000001))) true) (if (ref vulkan-backend 'float32) (let (out (ml/mean-squared-error (to-device (Tensor Float32 [70000] 1.0) 'vulkan) (to-device (Tensor Float32 [70000] 0.0) 'vulkan)) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (rank cpu) 0) (< (abs (- (ref cpu []) 1.0)) 0.0001))) true)))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error Vulkan staged Float32 overflow matches CPU",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/mean-squared-error (to-device (Tensor Float32 [300] 1.0e20) 'vulkan) (to-device (Tensor Float32 [300] 0.0) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/numeric-overflow) true))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error rejects unsupported dtype and mixed Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/mean-squared-error (Tensor BigInteger [1] [1]) (Tensor BigInteger [1] [1])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (if (ref vulkan-backend 'float64) (= (handle (ml/mean-squared-error (to-device (Tensor Float64 [1] [1]) 'vulkan) (Tensor Float64 [1] [1])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) (if (ref vulkan-backend 'float32) (= (handle (ml/mean-squared-error (to-device (Tensor Float32 [1] [1]) 'vulkan) (Tensor Float32 [1] [1])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/mean-squared-error rejects Vulkan shape and dtype mismatch",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (and (ref vulkan-backend 'float64) (ref vulkan-backend 'float32)) (= (handle (ml/mean-squared-error (to-device (Tensor Float64 [1] [1]) 'vulkan) (to-device (Tensor Float32 [1] [1]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) true) (if (ref vulkan-backend 'float64) (= (handle (ml/mean-squared-error (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [1 2] [1 2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) true)))", pass, fail);
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
    test_truthy(interp, "ml/cross-entropy Vulkan Float32 stays on device and stays stable",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/cross-entropy (to-device (Tensor Float32 [2] [1000 1000]) 'vulkan) (to-device (Tensor Float32 [2] [1 0]) 'vulkan) 0) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (rank cpu) 0) (< (abs (- (ref cpu []) (log 2.0))) 0.0001))) true))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Vulkan Float32 supports non-last class axis",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (cpu (to-device (ml/cross-entropy (to-device (Tensor Float32 [2 2] [0 0 0 0]) 'vulkan) (to-device (Tensor Float32 [2 2] [1 0 0 1]) 'vulkan) 0) 'cpu)) (< (abs (- (ref cpu []) (log 2.0))) 0.0001)) true))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Vulkan Float32 staged reductions stay on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (let (out (ml/cross-entropy (to-device (Tensor Float32 [300 2] 0.0) 'vulkan) (to-device (Tensor Float32 [300 2] 0.5) 'vulkan) 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (rank cpu) 0) (< (abs (- (ref cpu []) (log 2.0))) 0.0001))) true))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Vulkan Float32 rejects invalid targets",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/cross-entropy (to-device (Tensor Float32 [2] [0 0]) 'vulkan) (to-device (Tensor Float32 [2] [0.25 0.25]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/invalid-argument) true))", pass, fail);
    test_truthy(interp, "ml/cross-entropy Vulkan Float32 staged reductions preserve invalid targets",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/cross-entropy (to-device (Tensor Float32 [300 2] 0.0) 'vulkan) (to-device (Tensor Float32 [300 2] 0.25) 'vulkan) 1) (raise msg (ref msg 'code))) 'tensor/invalid-argument) true))", pass, fail);
    test_truthy(interp, "ml/cross-entropy rejects shape dtype and Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float64 [1 2] [1 0]) 0) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/cross-entropy (Tensor Float64 [2] [0 0]) (Tensor Float32 [2] [1 0]) 0) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) (if (ref vulkan-backend 'float64) (= (handle (ml/cross-entropy (to-device (Tensor Float64 [2] [0 0]) 'vulkan) (to-device (Tensor Float64 [2] [1 0]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) (if (ref vulkan-backend 'float32) (= (handle (ml/cross-entropy (to-device (Tensor Float32 [2] [0 0]) 'vulkan) (Tensor Float32 [2] [1 0]) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/cross-entropy rejects lazy CPU and Vulkan mixing before CPU realization",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'float32) (= (handle (ml/cross-entropy (map + (Tensor Float32 [2] [0 0]) 0.0) (to-device (Tensor Float32 [2] [1 0]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "ml reductions reject duplicate and empty axes",
        "(and (= (handle (ml/sum (Tensor Float64 [2 2] [1 2 3 4]) [0 0]) (raise msg (ref msg 'code))) 'tensor/axis-duplicate) (= (handle (ml/mean (Tensor Float64 [2] [1 2]) []) (raise msg (ref msg 'code))) 'tensor/shape-mismatch))", pass, fail);
    test_truthy(interp, "ml/max Vulkan axis reduction preserves device and values",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (if (ref vulkan-backend 'float64) (let (out (ml/max (to-device (Tensor Float64 [2 3] [1 9 3 4 5 6]) 'vulkan) 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (rank cpu) 1) (= (ref cpu [0]) 9.0) (= (ref cpu [1]) 6.0))) true) (if (ref vulkan-backend 'float32) (let (out (ml/max (to-device (Tensor Float32 [2 3] [1 9 3 -4 -5 -6]) 'vulkan) 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (rank cpu) 1) (= (ref cpu [0]) (Float32 9.0)) (= (ref cpu [1]) (Float32 -4.0)))) true)))", pass, fail);
    test_truthy(interp, "ml reductions reject unsupported dtype and unsupported Vulkan stable reductions",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (and (= (handle (ml/sum (Tensor BigInteger [2] [1 2]) [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (if (ref vulkan-backend 'float64) (= (handle (ml/logsumexp (to-device (Tensor Float64 [2] [1 2]) 'vulkan) [0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) (if (ref vulkan-backend 'float64) (= (handle (ml/softmax (to-device (Tensor Float64 [2] [1 2]) 'vulkan) 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true)))", pass, fail);
    test_truthy(interp, "ml/conv1d Float64 grouped NCW dense row-major",
        "(let (out (ml/conv1d (Tensor Float64 [1 2 4] [1 2 3 4 10 20 30 40]) (Tensor Float64 [2 1 2] [1 1 2 0]) 1 0 1 2)) (and (= (ref out [0 0 2]) 7.0) (= (ref out [0 1 2]) 60.0)))", pass, fail);
    test_truthy(interp, "ml/conv1d Float64 stride padding output shape",
        "(let (out (ml/conv1d (Tensor Float64 [1 1 4] [1 2 3 4]) (Tensor Float64 [1 1 3] [1 2 3]) 2 1 1 1)) (and (= (ref out [0 0 0]) 8.0) (= (ref out [0 0 1]) 20.0)))", pass, fail);
    test_truthy(interp, "ml/conv1d rejects dtype shape and parameter mismatches",
        "(and (= (handle (ml/conv1d (Tensor Float64 [1 1 4] [1 2 3 4]) (Tensor Float32 [1 1 2] [1 1]) 1 0 1 1) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) (= (handle (ml/conv1d (Tensor Float64 [1 2 4] [1 2 3 4 5 6 7 8]) (Tensor Float64 [1 2 2] [1 1 1 1]) 1 0 1 2) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/conv1d (Tensor Float64 [1 1 4] [1 2 3 4]) (Tensor Float64 [1 1 2] [1 1]) 0 0 1 1) (raise msg (ref msg 'code))) 'tensor/invalid-argument))", pass, fail);
    test_truthy(interp, "ml/conv1d Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-conv1d-direct-float32) (let (out (ml/conv1d (to-device (Tensor Float32 [1 1 4] [1 2 3 4]) 'vulkan) (to-device (Tensor Float32 [1 1 2] [10 1]) 'vulkan) 1 0 1 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref cpu [0 0 0]) (Float32 12.0)) (= (ref cpu [0 0 2]) (Float32 34.0)))) true))", pass, fail);
    test_truthy(interp, "ml/conv1d rejects mixed CPU Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-conv1d-direct-float32) (and (= (handle (ml/conv1d (to-device (Tensor Float32 [1 1 4] [1 2 3 4]) 'vulkan) (Tensor Float32 [1 1 2] [10 1]) 1 0 1 1) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/conv1d (map + (Tensor Float32 [1 1 4] [1 2 3 4]) (Float32 0.0)) (to-device (Tensor Float32 [1 1 2] [10 1]) 'vulkan) 1 0 1 1) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)) true))", pass, fail);
    test_truthy(interp, "ml/conv2d Float64 grouped NCHW dense row-major",
        "(let (out (ml/conv2d (Tensor Float64 [1 2 2 3] [1 2 3 4 5 6 10 20 30 40 50 60]) (Tensor Float64 [2 1 2 2] [1 1 1 1 2 0 0 0]) 1 1 0 0 1 1 2)) (and (= (ref out [0 0 0 0]) 12.0) (= (ref out [0 0 0 1]) 16.0) (= (ref out [0 1 0 1]) 40.0)))", pass, fail);
    test_truthy(interp, "ml/conv2d Float64 asymmetric stride padding output shape",
        "(let (out (ml/conv2d (Tensor Float64 [1 1 3 3] [1 2 3 4 5 6 7 8 9]) (Tensor Float64 [1 1 2 2] [1 0 0 1]) 2 1 1 0 1 1 1)) (and (= (ref out [0 0 0 0]) 2.0) (= (ref out [0 0 0 1]) 3.0) (= (ref out [0 0 1 0]) 12.0) (= (ref out [0 0 1 1]) 14.0)))", pass, fail);
    test_truthy(interp, "ml/conv2d rejects dtype shape and parameter mismatches",
        "(and (= (handle (ml/conv2d (Tensor Float64 [1 1 2 2] [1 2 3 4]) (Tensor Float32 [1 1 1 1] [1]) 1 1 0 0 1 1 1) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) (= (handle (ml/conv2d (Tensor Float64 [1 2 2 2] [1 2 3 4 5 6 7 8]) (Tensor Float64 [1 2 1 1] [1 1]) 1 1 0 0 1 1 2) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/conv2d (Tensor Float64 [1 1 2 2] [1 2 3 4]) (Tensor Float64 [1 1 1 1] [1]) 0 1 0 0 1 1 1) (raise msg (ref msg 'code))) 'tensor/invalid-argument))", pass, fail);
    test_truthy(interp, "ml/conv2d Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-conv2d-direct-float32) (let (out (ml/conv2d (to-device (Tensor Float32 [1 1 2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float32 [1 1 2 2] [1 1 1 1]) 'vulkan) 1 1 0 0 1 1 1) cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (= (ref cpu [0 0 0 0]) (Float32 12.0)) (= (ref cpu [0 0 0 1]) (Float32 16.0)))) true))", pass, fail);
    test_truthy(interp, "ml/conv2d rejects mixed CPU Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-conv2d-direct-float32) (and (= (handle (ml/conv2d (to-device (Tensor Float32 [1 1 2 3] [1 2 3 4 5 6]) 'vulkan) (Tensor Float32 [1 1 2 2] [1 1 1 1]) 1 1 0 0 1 1 1) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/conv2d (map + (Tensor Float32 [1 1 2 3] [1 2 3 4 5 6]) (Float32 0.0)) (to-device (Tensor Float32 [1 1 2 2] [1 1 1 1]) 'vulkan) 1 1 0 0 1 1 1) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)) true))", pass, fail);
    test_truthy(interp, "ml/max-pool2d Float64 dense NCHW row-major",
        "(let (out (ml/max-pool2d (Tensor Float64 [1 1 3 4] [1 2 3 4 5 6 7 8 9 10 11 12]) 2 2 1 2 0 0)) (and (= (ref out [0 0 0 0]) 6.0) (= (ref out [0 0 0 1]) 8.0) (= (ref out [0 0 1 0]) 10.0) (= (ref out [0 0 1 1]) 12.0)))", pass, fail);
    test_truthy(interp, "ml/avg-pool2d Float64 excludes padding from divisor",
        "(let (out (ml/avg-pool2d (Tensor Float64 [1 1 2 2] [1 2 3 4]) 2 2 1 1 1 1)) (and (= (ref out [0 0 0 0]) 1.0) (= (ref out [0 0 0 1]) 1.5) (= (ref out [0 0 1 1]) 2.5) (= (ref out [0 0 2 2]) 4.0)))", pass, fail);
    test_truthy(interp, "ml pool2d rejects shape dtype and parameter mismatches",
        "(and (= (handle (ml/max-pool2d (Tensor BigInteger [1 1 2 2] [1 2 3 4]) 2 2 1 1 0 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) (= (handle (ml/avg-pool2d (Tensor Float64 [1 1 2] [1 2]) 2 2 1 1 0 0) (raise msg (ref msg 'code))) 'tensor/shape-mismatch) (= (handle (ml/max-pool2d (Tensor Float64 [1 1 2 2] [1 2 3 4]) 2 2 0 1 0 0) (raise msg (ref msg 'code))) 'tensor/invalid-argument) (= (handle (ml/avg-pool2d (Tensor Float64 [1 1 2 2] [1 2 3 4]) 2 2 1 1 2 0) (raise msg (ref msg 'code))) 'tensor/shape-mismatch))", pass, fail);
    test_truthy(interp, "ml pool2d Vulkan Float32 stays on device",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-pool2d-direct-float32) (let (max-out (ml/max-pool2d (to-device (Tensor Float32 [1 1 3 3] [1 2 3 4 5 6 7 8 9]) 'vulkan) 2 2 1 1 0 0) avg-out (ml/avg-pool2d (to-device (Tensor Float32 [1 1 3 3] [1 2 3 4 5 6 7 8 9]) 'vulkan) 2 2 1 1 0 0) max-cpu (to-device max-out 'cpu) avg-cpu (to-device avg-out 'cpu)) (and (= (device max-out) 'vulkan) (= (device avg-out) 'vulkan) (= (ref max-cpu [0 0 0 0]) (Float32 5.0)) (= (ref max-cpu [0 0 1 1]) (Float32 9.0)) (= (ref avg-cpu [0 0 0 0]) (Float32 3.0)) (= (ref avg-cpu [0 0 1 1]) (Float32 7.0)))) true))", pass, fail);
    test_truthy(interp, "ml pool2d rejects mixed lazy CPU Vulkan fallback",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'ml-pool2d-direct-float32) (= (handle (ml/max-pool2d (map + (to-device (Tensor Float32 [1 1 2 2] [1 2 3 4]) 'vulkan) (Tensor Float32 [1 1 2 2] [0 0 0 0])) 2 2 1 1 0 0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
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
