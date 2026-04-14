module lisp;

import std::math;

extern fn double c_sin(double x) @extern("sin");
extern fn double c_cos(double x) @extern("cos");
extern fn double c_tan(double x) @extern("tan");
extern fn double c_asin(double x) @extern("asin");
extern fn double c_acos(double x) @extern("acos");
extern fn double c_atan(double x) @extern("atan");
extern fn double c_atan2(double y, double x) @extern("atan2");
extern fn double c_exp(double x) @extern("exp");
extern fn double c_log(double x) @extern("log");
extern fn double c_log10(double x) @extern("log10");
extern fn double c_pow(double x, double y) @extern("pow");
extern fn double c_sqrt(double x) @extern("sqrt");
extern fn double c_floor(double x) @extern("floor");
extern fn double c_ceil(double x) @extern("ceil");
extern fn double c_round(double x) @extern("round");
extern fn double c_fabs(double x) @extern("fabs");

fn Value* prim_math_lgamma(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "math/lgamma", "1 argument", args.len);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "math/lgamma", 0, "number", args[0]);

    double input = 0.0;
    if (!try_numeric_to_double(args[0], &input)) {
        return raise_error(interp, "math/lgamma: value out of Double range");
    }

    double output = 0.0;
    int status = omni_boost_math_lgamma(input, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return raise_error(interp, "math/lgamma: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return raise_error(interp, "math/lgamma: domain error");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return raise_error(interp, "math/lgamma: range error");
        default:
            return raise_error(interp, "math/lgamma: evaluation error");
    }
}

fn Value* prim_sin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "sin", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "sin", 0, "number", args[0]);
    return make_double(interp, c_sin(to_double(args[0])));
}

fn Value* prim_cos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "cos", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "cos", 0, "number", args[0]);
    return make_double(interp, c_cos(to_double(args[0])));
}

fn Value* prim_tan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "tan", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "tan", 0, "number", args[0]);
    return make_double(interp, c_tan(to_double(args[0])));
}

fn Value* prim_asin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "asin", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "asin", 0, "number", args[0]);
    return make_double(interp, c_asin(to_double(args[0])));
}

fn Value* prim_acos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "acos", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "acos", 0, "number", args[0]);
    return make_double(interp, c_acos(to_double(args[0])));
}

fn Value* prim_atan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "atan", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "atan", 0, "number", args[0]);
    return make_double(interp, c_atan(to_double(args[0])));
}

fn Value* prim_atan2(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "atan2", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "atan2", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "atan2", 1, "number", args[1]);
    return make_double(interp, c_atan2(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_exp(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "exp", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "exp", 0, "number", args[0]);
    return make_double(interp, c_exp(to_double(args[0])));
}

fn Value* prim_log(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "log", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "log", 0, "number", args[0]);
    return make_double(interp, c_log(to_double(args[0])));
}

fn Value* prim_log10(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "log10", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "log10", 0, "number", args[0]);
    return make_double(interp, c_log10(to_double(args[0])));
}

fn Value* prim_pow(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "pow", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "pow", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "pow", 1, "number", args[1]);
    return make_double(interp, c_pow(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_sqrt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "sqrt", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "sqrt", 0, "number", args[0]);
    return make_double(interp, c_sqrt(to_double(args[0])));
}

fn Value* prim_floor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "floor", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "floor", 0, "number", args[0]);
    return math_int_from_double_or_raise(interp, c_floor(to_double(args[0])), "floor");
}

fn Value* prim_ceiling(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "ceiling", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "ceiling", 0, "number", args[0]);
    return math_int_from_double_or_raise(interp, c_ceil(to_double(args[0])), "ceiling");
}

fn Value* prim_round(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "round", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "round", 0, "number", args[0]);
    return math_int_from_double_or_raise(interp, c_round(to_double(args[0])), "round");
}

fn Value* prim_truncate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "truncate", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "truncate", 0, "number", args[0]);
    return math_int_from_double_or_raise(interp, to_double(args[0]), "truncate");
}

fn Value* prim_abs(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "abs", "1 argument", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "abs", 0, "number", args[0]);
    if (is_double(args[0])) return make_double(interp, c_fabs(args[0].double_val));
    long n = args[0].int_val;
    if (n == long.min) return raise_error(interp, "abs: integer overflow");
    return make_int(interp, n < 0 ? -n : n);
}

fn Value* prim_min(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "min", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "min", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "min", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a < b ? a : b);
    }
    return make_int(interp, args[0].int_val < args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_max(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "max", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "max", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "max", 1, "number", args[1]);
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a > b ? a : b);
    }
    return make_int(interp, args[0].int_val > args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_gcd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "gcd", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "gcd", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "gcd", 1, "integer", args[1]);
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == long.min || b == long.min) return raise_error(interp, "gcd: integer overflow");
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    while (b != 0) { long t = b; b = a % b; a = t; }
    return make_int(interp, a);
}

fn Value* prim_lcm(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "lcm", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "lcm", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "lcm", 1, "integer", args[1]);
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == 0 || b == 0) return make_int(interp, 0);
    if (a == long.min || b == long.min) return raise_error(interp, "lcm: integer overflow");
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    long ga = a; long gb = b;
    while (gb != 0) { long t = gb; gb = ga % gb; ga = t; }
    long factor = a / ga;
    long result = 0;
    if (math::overflow_mul(factor, b, &result)) return raise_error(interp, "lcm: integer overflow");
    return make_int(interp, result);
}

fn Value* prim_bitwise_and(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-and", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "bitwise-and", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "bitwise-and", 1, "integer", args[1]);
    return make_int(interp, args[0].int_val & args[1].int_val);
}

fn Value* prim_bitwise_or(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-or", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "bitwise-or", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "bitwise-or", 1, "integer", args[1]);
    return make_int(interp, args[0].int_val | args[1].int_val);
}

fn Value* prim_bitwise_xor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-xor", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "bitwise-xor", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "bitwise-xor", 1, "integer", args[1]);
    return make_int(interp, args[0].int_val ^ args[1].int_val);
}

fn Value* prim_bitwise_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "bitwise-not", "1 argument", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "bitwise-not", 0, "integer", args[0]);
    return make_int(interp, ~args[0].int_val);
}

fn Value* prim_lshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "lshift", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "lshift", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "lshift", 1, "integer", args[1]);
    long shift = args[1].int_val;
    if (shift < 0 || shift >= 64) return make_int(interp, 0);
    return make_int(interp, args[0].int_val << shift);
}

fn Value* prim_rshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "rshift", "2 arguments", args.len);
    if (!is_int(args[0])) return primitive_arg_type_raise(interp, "rshift", 0, "integer", args[0]);
    if (!is_int(args[1])) return primitive_arg_type_raise(interp, "rshift", 1, "integer", args[1]);
    long shift = args[1].int_val;
    if (shift < 0 || shift >= 64) return make_int(interp, 0);
    return make_int(interp, args[0].int_val >> shift);
}
