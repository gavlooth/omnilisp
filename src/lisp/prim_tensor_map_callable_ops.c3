module lisp;

import std::core::mem;
import main;

fn Value* tensor_map_dtype_raise(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: function result dtype mismatch", null);
}

fn bool tensor_map_callable(Value* func) @inline {
    if (func == null) return false;
    switch (func.tag) {
        case CLOSURE:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
        case CONTINUATION:
            return true;
        default:
            return false;
    }
}

fn bool tensor_primitive_payload_name_equals(Primitive* prim, char[] expected) @inline {
    if (prim == null || expected.len >= 64) return false;
    for (usz i = 0; i < expected.len; i++) {
        char actual = prim.name[i];
        if (actual == 0 || actual != expected[i]) return false;
    }
    return prim.name[expected.len] == 0;
}

fn bool tensor_map_callable_is_add(Value* func) @inline {
    if (primitive_name_equals(func, "+")) return true;
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        return tensor_primitive_payload_name_equals(fallback, "+");
    }
    return false;
}

fn bool tensor_map_callable_vulkan_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "*")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "/")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "min")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "max")) {
        *out_op = 5;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "*")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "/")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "min")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "max")) {
            *out_op = 5;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_cuda_op(Value* func, uint* out_op) @inline {
    return tensor_map_callable_vulkan_op(func, out_op);
}

fn bool tensor_map_callable_complex_binary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "*")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "/")) {
        *out_op = 3;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "*")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "/")) {
            *out_op = 3;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_complex_unary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "abs")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "+")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "real-part")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "imag-part")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "conjugate")) {
        *out_op = 5;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "abs")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "real-part")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "imag-part")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "conjugate")) {
            *out_op = 5;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_cuda_unary_op(Value* func, uint* out_op) @inline {
    if (tensor_map_callable_vulkan_unary_op(func, out_op)) return tensor_cuda_unary_op_available(*out_op);
    if (out_op == null) return false;
    if (primitive_name_equals(func, "math/erf")) {
        *out_op = 17;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "math/erfc")) {
        *out_op = 18;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "stats/normal-cdf")) {
        *out_op = 19;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "stats/normal-quantile")) {
        *out_op = 20;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "math/erf")) {
            *out_op = 17;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "math/erfc")) {
            *out_op = 18;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-cdf")) {
            *out_op = 19;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-quantile")) {
            *out_op = 20;
            return tensor_cuda_unary_op_available(*out_op);
        }
    }
    return false;
}

fn bool tensor_cuda_unary_op_available(uint op) @inline {
    if (op <= 4) return true;
    if (op <= 20) return omni_tensor_backend_cuda_scientific_map_available() != 0;
    return false;
}

fn bool tensor_cuda_complex_map_available_for_dtype(TensorDType dtype) @inline {
    if (!tensor_dtype_is_fixed_complex(dtype)) return false;
    return omni_tensor_backend_cuda_complex_map_available() != 0;
}

fn Value* tensor_map_cuda_callable_op_or_raise(Value* func, bool is_binary, TensorDType dtype, uint* out_op, Interp* interp) {
    if (out_op == null) return tensor_runtime_raise(interp, "map: CUDA callable state is missing");
    uint op = 0;
    if (tensor_dtype_is_fixed_complex(dtype)) {
        if (!tensor_cuda_complex_map_available_for_dtype(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA fixed-width complex elementwise kernels are unavailable", null);
        }
        bool supported = is_binary
            ? tensor_map_callable_complex_binary_op(func, &op)
            : tensor_map_callable_complex_unary_op(func, &op);
        if (!supported) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Complex128 and Complex64 arithmetic elementwise kernels", null);
        }
    } else if (!is_binary) {
        if (!tensor_map_callable_cuda_unary_op(func, &op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64 and Float32 arithmetic and scientific unary kernels", null);
        }
    } else if (!tensor_map_callable_cuda_op(func, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64 and Float32 arithmetic kernels", null);
    }
    *out_op = op;
    return null;
}

fn bool tensor_map_callable_vulkan_unary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "abs")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "sqrt")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "real-part")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "conjugate")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "imag-part")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "sin")) {
        *out_op = 5;
        return true;
    }
    if (primitive_name_equals(func, "cos")) {
        *out_op = 6;
        return true;
    }
    if (primitive_name_equals(func, "tan")) {
        *out_op = 7;
        return true;
    }
    if (primitive_name_equals(func, "asin")) {
        *out_op = 8;
        return true;
    }
    if (primitive_name_equals(func, "acos")) {
        *out_op = 9;
        return true;
    }
    if (primitive_name_equals(func, "atan")) {
        *out_op = 10;
        return true;
    }
    if (primitive_name_equals(func, "sinh")) {
        *out_op = 11;
        return true;
    }
    if (primitive_name_equals(func, "cosh")) {
        *out_op = 12;
        return true;
    }
    if (primitive_name_equals(func, "tanh")) {
        *out_op = 13;
        return true;
    }
    if (primitive_name_equals(func, "exp")) {
        *out_op = 14;
        return true;
    }
    if (primitive_name_equals(func, "log")) {
        *out_op = 15;
        return true;
    }
    if (primitive_name_equals(func, "log10")) {
        *out_op = 16;
        return true;
    }
    if (primitive_name_equals(func, "stats/normal-cdf")) {
        *out_op = 19;
        return true;
    }
    if (primitive_name_equals(func, "stats/normal-quantile")) {
        *out_op = 20;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "abs")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sqrt")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "real-part")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "conjugate")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "imag-part")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sin")) {
            *out_op = 5;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "cos")) {
            *out_op = 6;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "tan")) {
            *out_op = 7;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "asin")) {
            *out_op = 8;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "acos")) {
            *out_op = 9;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "atan")) {
            *out_op = 10;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sinh")) {
            *out_op = 11;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "cosh")) {
            *out_op = 12;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "tanh")) {
            *out_op = 13;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "exp")) {
            *out_op = 14;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "log")) {
            *out_op = 15;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "log10")) {
            *out_op = 16;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-cdf")) {
            *out_op = 19;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-quantile")) {
            *out_op = 20;
            return true;
        }
    }
    return false;
}

fn bool tensor_unary_math_vulkan_op(int big_float_op, uint* out_op) @inline {
    if (out_op == null) return false;
    switch (big_float_op) {
        case BIG_FLOAT_MATH_SIN:
            *out_op = 5;
            return true;
        case BIG_FLOAT_MATH_COS:
            *out_op = 6;
            return true;
        case BIG_FLOAT_MATH_TAN:
            *out_op = 7;
            return true;
        case BIG_FLOAT_MATH_ASIN:
            *out_op = 8;
            return true;
        case BIG_FLOAT_MATH_ACOS:
            *out_op = 9;
            return true;
        case BIG_FLOAT_MATH_ATAN:
            *out_op = 10;
            return true;
        case BIG_FLOAT_MATH_SINH:
            *out_op = 11;
            return true;
        case BIG_FLOAT_MATH_COSH:
            *out_op = 12;
            return true;
        case BIG_FLOAT_MATH_TANH:
            *out_op = 13;
            return true;
        case BIG_FLOAT_MATH_EXP:
            *out_op = 14;
            return true;
        case BIG_FLOAT_MATH_LOG:
            *out_op = 15;
            return true;
        case BIG_FLOAT_MATH_LOG10:
            *out_op = 16;
            return true;
        case BIG_FLOAT_MATH_NORMAL_CDF:
            *out_op = 19;
            return true;
        case BIG_FLOAT_MATH_NORMAL_QUANTILE:
            *out_op = 20;
            return true;
        case BIG_FLOAT_MATH_SQRT:
            *out_op = 2;
            return true;
        default:
            return false;
    }
}

fn bool tensor_unary_math_cuda_op(int big_float_op, uint* out_op) @inline {
    if (!tensor_unary_math_vulkan_op(big_float_op, out_op)) return false;
    return tensor_cuda_unary_op_available(*out_op);
}

fn bool tensor_unary_math_cuda_extra_op(int big_float_op, uint* out_op) @inline {
    if (out_op == null) return false;
    switch (big_float_op) {
        case BIG_FLOAT_MATH_ERF:
            *out_op = 17;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_ERFC:
            *out_op = 18;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_NORMAL_CDF:
            *out_op = 19;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_NORMAL_QUANTILE:
            *out_op = 20;
            return tensor_cuda_unary_op_available(*out_op);
        default:
            return false;
    }
}

fn bool tensor_unary_math_cuda_dispatch_op(int big_float_op, uint* out_op) @inline {
    if (tensor_unary_math_cuda_op(big_float_op, out_op)) return true;
    if (tensor_unary_math_cuda_extra_op(big_float_op, out_op)) return true;
    return false;
}

fn bool tensor_unary_math_has_complex_op(int big_complex_op) @inline {
    return big_complex_op > 0;
}
