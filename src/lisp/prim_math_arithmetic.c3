module lisp;

import std::io;
import std::math;

const double MATH_DOUBLE_FINITE_LIMIT = 1.7976931348623157e308;
const double MATH_FLOAT32_FINITE_LIMIT = 3.4028234663852886e38;
const double MATH_INT_MAX_DOUBLE = (double)long.max;
const double MATH_INT_MIN_DOUBLE = (double)long.min;

fn bool math_try_double_to_int(double v, long* out) @inline {
    if (v != v) return false; // NaN
    if (v > MATH_DOUBLE_FINITE_LIMIT || v < -MATH_DOUBLE_FINITE_LIMIT) return false;
    if (v < MATH_INT_MIN_DOUBLE || v > MATH_INT_MAX_DOUBLE) return false;
    *out = (long)v; // truncation toward zero
    return true;
}

fn Value* math_int_from_double_or_raise(Interp* interp, double value, char[] op_name) {
    long out = 0;
    if (math_try_double_to_int(value, &out)) return make_int(interp, out);

    char[128] ebuf;
    char[] msg;
    if (try char[] formatted = io::bprintf(&ebuf, "%s: result out of Integer range", (ZString)op_name)) {
        msg = formatted;
    } else {
        msg = "math: result out of Integer range";
    }
    return math_raise(interp, "type/range-error", msg);
}

fn bool math_try_double_to_float32(double v, float* out) @inline {
    if (out == null) return false;
    if (v != v) return false;
    if (v > MATH_FLOAT32_FINITE_LIMIT || v < -MATH_FLOAT32_FINITE_LIMIT) return false;
    *out = (float)v;
    return true;
}

fn Value* numeric_binary_float32_result(Interp* interp, Value* lhs, Value* rhs, int op, char[] op_name) {
    double left = 0.0;
    double right = 0.0;
    if (!try_numeric_to_double(lhs, &left) || !try_numeric_to_double(rhs, &right)) {
        char[128] ebuf;
        char[] msg = "numeric operation: value is out of Float64 range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: value is out of Float64 range", (ZString)op_name)) {
            msg = formatted;
        }
        return math_raise(interp, "type/range-error", msg);
    }

    double result = 0.0;
    switch (op) {
        case BIG_INTEGER_OP_ADD:
            result = left + right;
        case BIG_INTEGER_OP_SUB:
            result = left - right;
        case BIG_INTEGER_OP_MUL:
            result = left * right;
        case BIG_INTEGER_OP_DIV:
            if (right == 0.0) return math_raise(interp, "type/div-by-zero", "/: division by zero");
            result = left / right;
        default:
            return math_raise(interp, "type/unsupported", "numeric operation: unknown binary op");
    }

    float narrowed = 0.0f;
    if (!math_try_double_to_float32(result, &narrowed)) {
        char[128] ebuf;
        char[] msg = "numeric operation: result out of Float32 range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: result out of Float32 range", (ZString)op_name)) {
            msg = formatted;
        }
        return math_raise(interp, "type/range-error", msg);
    }
    return make_float32(interp, narrowed);
}

fn Value* numeric_binary_double_result(Interp* interp, Value* lhs, Value* rhs, int op, char[] op_name) {
    double left = 0.0;
    double right = 0.0;
    if (!try_numeric_to_double(lhs, &left) || !try_numeric_to_double(rhs, &right)) {
        char[128] ebuf;
        char[] msg = "numeric operation: BigInteger value is out of Float64 range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: BigInteger value is out of Float64 range", (ZString)op_name)) {
            msg = formatted;
        }
        return math_raise(interp, "type/range-error", msg);
    }
    switch (op) {
        case BIG_INTEGER_OP_ADD:
            return make_double(interp, left + right);
        case BIG_INTEGER_OP_SUB:
            return make_double(interp, left - right);
        case BIG_INTEGER_OP_MUL:
            return make_double(interp, left * right);
        case BIG_INTEGER_OP_DIV:
            if (right == 0.0) return math_raise(interp, "type/div-by-zero", "/: division by zero");
            return make_double(interp, left / right);
        default:
            return math_raise(interp, "type/unsupported", "numeric operation: unknown binary op");
    }
}

fn bool numeric_fixed_complex_result_is_complex64(Value* lhs, Value* rhs) @inline {
    if (is_complex128(lhs) || is_complex128(rhs)) return false;
    if (is_double(lhs) || is_double(rhs) || is_big_integer(lhs) || is_big_integer(rhs) || is_big_float(lhs) || is_big_float(rhs)) return false;
    return is_complex64(lhs) || is_complex64(rhs);
}

fn Value* numeric_binary_fixed_complex_result(Interp* interp, Value* lhs, Value* rhs, int op, char[] op_name) {
    if ((is_big_complex(lhs) || is_big_complex(rhs)) && (is_fixed_complex(lhs) || is_fixed_complex(rhs))) {
        return math_raise(interp, "type/arg-mismatch", "numeric operation: mixed BigComplex and fixed-width complex requires explicit conversion");
    }

    if (numeric_fixed_complex_result_is_complex64(lhs, rhs)) {
        Complex64Val left;
        Complex64Val right;
        Complex64Val result;
        if (!fixed_complex_value_to_complex64(lhs, &left) || !fixed_complex_value_to_complex64(rhs, &right)) {
            return math_raise(interp, "type/range-error", "numeric operation: value is not representable as Complex64");
        }
        if (op == BIG_INTEGER_OP_DIV && complex64_is_zero(right)) return math_raise(interp, "type/div-by-zero", "/: division by zero");
        if (!fixed_complex_binary64(left, right, op, &result)) {
            char[128] ebuf;
            char[] msg = "numeric operation: Complex64 result out of range";
            if (try char[] formatted = io::bprintf(&ebuf, "%s: Complex64 result out of range", (ZString)op_name)) {
                msg = formatted;
            }
            return math_raise(interp, "type/range-error", msg);
        }
        return make_complex64(interp, result.real, result.imag);
    }

    Complex128Val left128;
    Complex128Val right128;
    Complex128Val result128;
    if (!fixed_complex_value_to_complex128(lhs, &left128) || !fixed_complex_value_to_complex128(rhs, &right128)) {
        return math_raise(interp, "type/range-error", "numeric operation: value is not representable as Complex128");
    }
    if (op == BIG_INTEGER_OP_DIV && complex128_is_zero(right128)) return math_raise(interp, "type/div-by-zero", "/: division by zero");
    if (!fixed_complex_binary128(left128, right128, op, &result128)) {
        char[128] ebuf;
        char[] msg = "numeric operation: Complex128 result out of range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: Complex128 result out of range", (ZString)op_name)) {
            msg = formatted;
        }
        return math_raise(interp, "type/range-error", msg);
    }
    return make_complex128(interp, result128.real, result128.imag);
}

fn Value* numeric_add2(Interp* interp, Value* lhs, Value* rhs) {
    if (!is_numeric_value(lhs)) return primitive_arg_type_raise(interp, "+", 0, "number", lhs);
    if (!is_numeric_value(rhs)) return primitive_arg_type_raise(interp, "+", 1, "number", rhs);
    if (is_fixed_complex(lhs) || is_fixed_complex(rhs)) {
        return numeric_binary_fixed_complex_result(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+");
    }
    if (is_big_complex(lhs) || is_big_complex(rhs)) {
        return big_complex_binary_values(interp, lhs, rhs, BIG_COMPLEX_OP_ADD, "+: failed to allocate BigComplex result");
    }
    if (is_big_float(lhs) || is_big_float(rhs)) {
        return big_float_binary_values(interp, lhs, rhs, BIG_FLOAT_OP_ADD, "+: failed to allocate BigFloat result");
    }
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+");
    if (is_float32(lhs) || is_float32(rhs)) return numeric_binary_float32_result(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+");
    if (is_big_integer(lhs) || is_big_integer(rhs)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+: failed to allocate BigInteger result");
    }
    long result = 0;
    if (math::overflow_add(lhs.int_val, rhs.int_val, &result)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, result);
}

fn Value* numeric_sub2(Interp* interp, Value* lhs, Value* rhs) {
    if (!is_numeric_value(lhs)) return primitive_arg_type_raise(interp, "-", 0, "number", lhs);
    if (!is_numeric_value(rhs)) return primitive_arg_type_raise(interp, "-", 1, "number", rhs);
    if (is_fixed_complex(lhs) || is_fixed_complex(rhs)) {
        return numeric_binary_fixed_complex_result(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-");
    }
    if (is_big_complex(lhs) || is_big_complex(rhs)) {
        return big_complex_binary_values(interp, lhs, rhs, BIG_COMPLEX_OP_SUB, "-: failed to allocate BigComplex result");
    }
    if (is_big_float(lhs) || is_big_float(rhs)) {
        return big_float_binary_values(interp, lhs, rhs, BIG_FLOAT_OP_SUB, "-: failed to allocate BigFloat result");
    }
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-");
    if (is_float32(lhs) || is_float32(rhs)) return numeric_binary_float32_result(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-");
    if (is_big_integer(lhs) || is_big_integer(rhs)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-: failed to allocate BigInteger result");
    }
    long result = 0;
    if (math::overflow_sub(lhs.int_val, rhs.int_val, &result)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, result);
}

fn Value* numeric_mul2(Interp* interp, Value* lhs, Value* rhs) {
    if (!is_numeric_value(lhs)) return primitive_arg_type_raise(interp, "*", 0, "number", lhs);
    if (!is_numeric_value(rhs)) return primitive_arg_type_raise(interp, "*", 1, "number", rhs);
    if (is_fixed_complex(lhs) || is_fixed_complex(rhs)) {
        return numeric_binary_fixed_complex_result(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*");
    }
    if (is_big_complex(lhs) || is_big_complex(rhs)) {
        return big_complex_binary_values(interp, lhs, rhs, BIG_COMPLEX_OP_MUL, "*: failed to allocate BigComplex result");
    }
    if (is_big_float(lhs) || is_big_float(rhs)) {
        return big_float_binary_values(interp, lhs, rhs, BIG_FLOAT_OP_MUL, "*: failed to allocate BigFloat result");
    }
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*");
    if (is_float32(lhs) || is_float32(rhs)) return numeric_binary_float32_result(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*");
    if (is_big_integer(lhs) || is_big_integer(rhs)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*: failed to allocate BigInteger result");
    }
    long result = 0;
    if (math::overflow_mul(lhs.int_val, rhs.int_val, &result)) {
        return big_integer_binary_values(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, result);
}

fn Value* numeric_neg1(Interp* interp, Value* value) {
    if (is_tensor(value)) return tensor_neg_value(value, interp);
    if (!is_numeric_value(value)) return primitive_arg_type_raise(interp, "-", 0, "number", value);
    if (is_complex128(value)) return make_complex128(interp, -value.complex128_val.real, -value.complex128_val.imag);
    if (is_complex64(value)) return make_complex64(interp, -value.complex64_val.real, -value.complex64_val.imag);
    if (is_big_complex(value)) return big_complex_neg_value(interp, value, "-: failed to allocate BigComplex result");
    if (is_big_float(value)) return big_float_neg_value(interp, value, "-: failed to allocate BigFloat result");
    if (is_double(value)) return make_double(interp, -value.double_val);
    if (is_float32(value)) return make_float32(interp, -value.float32_val);
    if (is_big_integer(value) || value.int_val == long.min) {
        return big_integer_neg_value(interp, value, "-: failed to allocate BigInteger result");
    }
    return make_int(interp, -value.int_val);
}

<*
@require interp != null
*>
fn Value* prim_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return primitive_arity_raise(interp, "+", "1 or 2 arguments", args.len);

    if (args.len == 1) {
        if (is_tensor(args[0])) return args[0];
        if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "+", 0, "number", args[0]);
        return args[0];
    }

    return numeric_add2(interp, args[0], args[1]);
}

<*
@require interp != null
*>
fn Value* prim_sub(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return primitive_arity_raise(interp, "-", "1 or 2 arguments", args.len);
    if (args.len == 1) return numeric_neg1(interp, args[0]);
    return numeric_sub2(interp, args[0], args[1]);
}

<*
@require interp != null
*>
fn Value* prim_mul(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "*", "2 arguments", args.len);
    return numeric_mul2(interp, args[0], args[1]);
}

<*
@require interp != null
*>
fn Value* prim_div(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "/", "2 arguments", args.len);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "/", 0, "number", args[0]);
    if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "/", 1, "number", args[1]);
    if (is_fixed_complex(args[0]) || is_fixed_complex(args[1])) {
        return numeric_binary_fixed_complex_result(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/");
    }
    if (is_big_complex(args[0]) || is_big_complex(args[1])) {
        if (big_complex_value_is_zero(args[1])) return math_raise(interp, "type/div-by-zero", "/: division by zero");
        return big_complex_binary_values(interp, args[0], args[1], BIG_COMPLEX_OP_DIV, "/: failed to allocate BigComplex result");
    }
    if (is_big_float(args[0]) || is_big_float(args[1])) {
        Value* zero = make_int(interp, 0);
        int cmp = 0;
        if (numeric_values_compare(args[1], zero, &cmp) && cmp == 0) return math_raise(interp, "type/div-by-zero", "/: division by zero");
        return big_float_binary_values(interp, args[0], args[1], BIG_FLOAT_OP_DIV, "/: failed to allocate BigFloat result");
    }
    if (is_double(args[0]) || is_double(args[1])) {
        return numeric_binary_double_result(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/");
    }
    if (is_float32(args[0]) || is_float32(args[1])) {
        return numeric_binary_float32_result(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/");
    }
    if (exact_integer_value_is_zero(args[1])) return math_raise(interp, "type/div-by-zero", "/: division by zero");
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/: failed to allocate BigInteger result");
    }
    long b = args[1].int_val;
    if (args[0].int_val == long.min && b == -1) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, args[0].int_val / b);
}

<*
@require interp != null
*>
fn Value* prim_mod(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "%", "2 arguments", args.len);
    if (!(is_int(args[0]) || is_big_integer(args[0]))) return primitive_arg_type_raise(interp, "%", 0, "integer", args[0]);
    if (!(is_int(args[1]) || is_big_integer(args[1]))) return primitive_arg_type_raise(interp, "%", 1, "integer", args[1]);
    if (exact_integer_value_is_zero(args[1])) return math_raise(interp, "type/div-by-zero", "%: division by zero");
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_MOD, "%: failed to allocate BigInteger result");
    }
    long a = args[0].int_val;
    long b = args[1].int_val;
    if (a == long.min && b == -1) return make_int(interp, 0);
    return make_int(interp, a % b);
}
