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

fn Value* prim_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "+", "at least 1 argument", args.len);

    if (args.len == 1) {
        if (!is_number(args[0])) return primitive_arg_type_raise(interp, "+", 0, "number", args[0]);
        return args[0];
    }

    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "+", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "+", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) + to_double(args[1]));
    }
    long result = 0;
    if (math::overflow_add(args[0].int_val, args[1].int_val, &result)) return raise_error(interp, "+: integer overflow");
    return make_int(interp, result);
}

fn Value* prim_sub(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "-", "at least 1 argument", args.len);
    if (args.len == 1) {
        if (!is_number(args[0])) return primitive_arg_type_raise(interp, "-", 0, "number", args[0]);
        if (is_double(args[0])) return make_double(interp, -args[0].double_val);
        if (args[0].int_val == long.min) return raise_error(interp, "-: integer overflow");
        return make_int(interp, -args[0].int_val);
    }
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "-", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "-", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) - to_double(args[1]));
    }
    long result = 0;
    if (math::overflow_sub(args[0].int_val, args[1].int_val, &result)) return raise_error(interp, "-: integer overflow");
    return make_int(interp, result);
}

fn Value* prim_mul(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "*", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "*", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "*", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) * to_double(args[1]));
    }
    long result = 0;
    if (math::overflow_mul(args[0].int_val, args[1].int_val, &result)) return raise_error(interp, "*: integer overflow");
    return make_int(interp, result);
}

fn Value* prim_div(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "/", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "/", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "/", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        double b = to_double(args[1]);
        if (b == 0.0) return raise_error(interp, "/: division by zero");
        return make_double(interp, to_double(args[0]) / b);
    }
    long b = args[1].int_val;
    if (b == 0) return raise_error(interp, "/: division by zero");
    if (args[0].int_val == long.min && b == -1) return raise_error(interp, "/: integer overflow");
    return make_int(interp, args[0].int_val / b);
}

fn Value* prim_mod(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "%", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "%", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "%", 1, "integer", args[1]);
    long a = args[0].int_val;
    long b = args[1].int_val;
    if (b == 0) return raise_error(interp, "%: division by zero");
    if (a == long.min && b == -1) return raise_error(interp, "%: integer overflow");
    return make_int(interp, a % b);
}
