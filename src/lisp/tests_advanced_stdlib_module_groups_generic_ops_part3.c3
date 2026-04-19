module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part3(Interp* interp, int* pass, int* fail) {
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
}
