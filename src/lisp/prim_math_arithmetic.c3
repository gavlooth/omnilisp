module lisp;

import std::io;
import std::math;

const double MATH_DOUBLE_FINITE_LIMIT = 1.7976931348623157e308;
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
    return raise_error(interp, msg);
}

fn Value* numeric_binary_double_result(Interp* interp, Value* lhs, Value* rhs, int op, char[] op_name) {
    double left = 0.0;
    double right = 0.0;
    if (!try_numeric_to_double(lhs, &left) || !try_numeric_to_double(rhs, &right)) {
        char[128] ebuf;
        char[] msg = "numeric operation: BigInteger value is out of Double range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: BigInteger value is out of Double range", (ZString)op_name)) {
            msg = formatted;
        }
        return raise_error(interp, msg);
    }
    switch (op) {
        case BIG_INTEGER_OP_ADD:
            return make_double(interp, left + right);
        case BIG_INTEGER_OP_SUB:
            return make_double(interp, left - right);
        case BIG_INTEGER_OP_MUL:
            return make_double(interp, left * right);
        case BIG_INTEGER_OP_DIV:
            if (right == 0.0) return raise_error(interp, "/: division by zero");
            return make_double(interp, left / right);
        default:
            return raise_error(interp, "numeric operation: unknown binary op");
    }
}

fn Value* numeric_add2(Interp* interp, Value* lhs, Value* rhs) {
    if (!is_numeric_value(lhs)) return primitive_arg_type_raise(interp, "+", 0, "number", lhs);
    if (!is_numeric_value(rhs)) return primitive_arg_type_raise(interp, "+", 1, "number", rhs);
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_ADD, "+");
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
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_SUB, "-");
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
    if (is_double(lhs) || is_double(rhs)) return numeric_binary_double_result(interp, lhs, rhs, BIG_INTEGER_OP_MUL, "*");
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
    if (!is_numeric_value(value)) return primitive_arg_type_raise(interp, "-", 0, "number", value);
    if (is_double(value)) return make_double(interp, -value.double_val);
    if (is_big_integer(value) || value.int_val == long.min) {
        return big_integer_neg_value(interp, value, "-: failed to allocate BigInteger result");
    }
    return make_int(interp, -value.int_val);
}

fn Value* prim_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "+", "at least 1 argument", args.len);

    if (args.len == 1) {
        if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "+", 0, "number", args[0]);
        return args[0];
    }

    return numeric_add2(interp, args[0], args[1]);
}

fn Value* prim_sub(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "-", "at least 1 argument", args.len);
    if (args.len == 1) return numeric_neg1(interp, args[0]);
    return numeric_sub2(interp, args[0], args[1]);
}

fn Value* prim_mul(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "*", "2 arguments", args.len);
    return numeric_mul2(interp, args[0], args[1]);
}

fn Value* prim_div(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "/", "2 arguments", args.len);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "/", 0, "number", args[0]);
    if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "/", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        return numeric_binary_double_result(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/");
    }
    if (exact_integer_value_is_zero(args[1])) return raise_error(interp, "/: division by zero");
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/: failed to allocate BigInteger result");
    }
    long b = args[1].int_val;
    if (args[0].int_val == long.min && b == -1) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_DIV, "/: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, args[0].int_val / b);
}

fn Value* prim_mod(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "%", "2 arguments", args.len);
    if (!(is_int(args[0]) || is_big_integer(args[0]))) return primitive_arg_type_raise(interp, "%", 0, "integer", args[0]);
    if (!(is_int(args[1]) || is_big_integer(args[1]))) return primitive_arg_type_raise(interp, "%", 1, "integer", args[1]);
    if (exact_integer_value_is_zero(args[1])) return raise_error(interp, "%: division by zero");
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_MOD, "%: failed to allocate BigInteger result");
    }
    long a = args[0].int_val;
    long b = args[1].int_val;
    if (a == long.min && b == -1) return make_int(interp, 0);
    return make_int(interp, a % b);
}
