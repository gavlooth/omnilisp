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
extern fn double c_sinh(double x) @extern("sinh");
extern fn double c_cosh(double x) @extern("cosh");
extern fn double c_tanh(double x) @extern("tanh");
extern fn double c_pow(double x, double y) @extern("pow");
extern fn double c_sqrt(double x) @extern("sqrt");
extern fn double c_floor(double x) @extern("floor");
extern fn double c_ceil(double x) @extern("ceil");
extern fn double c_round(double x) @extern("round");
extern fn double c_fabs(double x) @extern("fabs");

fn Value* math_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, null);
}

fn Value* math_runtime_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", message, null);
}

fn bool math_arg_to_double(Value* value, double* out) @inline {
    return is_numeric_value(value) && try_numeric_to_double(value, out);
}

<*
@require interp != null
*>
fn Value* prim_math_lgamma(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "math.lgamma", "1 argument", args.len);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "math.lgamma", 0, "number", args[0]);
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(
            interp, args[0], BIG_FLOAT_MATH_LGAMMA, "math.lgamma: BigFloat evaluation error");
    }

    double input = 0.0;
    if (!try_numeric_to_double(args[0], &input)) {
        return math_raise(interp, "type/range-error", "math.lgamma: value out of Float64 range");
    }

    double output = 0.0;
    int status = omni_boost_math_lgamma(input, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return math_raise(interp, "type/arg-mismatch", "math.lgamma: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return math_raise(interp, "type/domain-error", "math.lgamma: domain error");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return math_raise(interp, "type/range-error", "math.lgamma: range error");
        default:
            return math_runtime_raise(interp, "math.lgamma: evaluation error");
    }
}

<*
@require interp != null
*>
fn Value* prim_math_erf(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "math.erf", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_ERF, 0, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "math.erf", 0, "number", args[0]);
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(
            interp, args[0], BIG_FLOAT_MATH_ERF, "math.erf: BigFloat evaluation error");
    }

    double input = 0.0;
    if (!try_numeric_to_double(args[0], &input)) {
        return math_raise(interp, "type/range-error", "math.erf: value out of Float64 range");
    }

    double output = 0.0;
    int status = omni_boost_math_erf(input, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return math_raise(interp, "type/arg-mismatch", "math.erf: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return math_raise(interp, "type/domain-error", "math.erf: domain error");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return math_raise(interp, "type/range-error", "math.erf: range error");
        default:
            return math_runtime_raise(interp, "math.erf: evaluation error");
    }
}

<*
@require interp != null
*>
fn Value* prim_math_erfc(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "math.erfc", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_ERFC, 0, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "math.erfc", 0, "number", args[0]);
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(
            interp, args[0], BIG_FLOAT_MATH_ERFC, "math.erfc: BigFloat evaluation error");
    }

    double input = 0.0;
    if (!try_numeric_to_double(args[0], &input)) {
        return math_raise(interp, "type/range-error", "math.erfc: value out of Float64 range");
    }

    double output = 0.0;
    int status = omni_boost_math_erfc(input, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return math_raise(interp, "type/arg-mismatch", "math.erfc: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return math_raise(interp, "type/domain-error", "math.erfc: domain error");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return math_raise(interp, "type/range-error", "math.erfc: range error");
        default:
            return math_runtime_raise(interp, "math.erfc: evaluation error");
    }
}

<*
@require interp != null
*>
fn Value* prim_stats_normal_cdf(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "stats.normal-cdf", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_NORMAL_CDF, 0, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "stats.normal-cdf", 0, "number", args[0]);
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(
            interp, args[0], BIG_FLOAT_MATH_NORMAL_CDF, "stats.normal-cdf: BigFloat evaluation error");
    }

    double input = 0.0;
    if (!try_numeric_to_double(args[0], &input)) {
        return math_raise(interp, "type/range-error", "stats.normal-cdf: value out of Float64 range");
    }

    double output = 0.0;
    int status = omni_boost_math_standard_normal_cdf(input, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return math_raise(interp, "type/arg-mismatch", "stats.normal-cdf: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return math_raise(interp, "type/domain-error", "stats.normal-cdf: domain error");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return math_raise(interp, "type/range-error", "stats.normal-cdf: range error");
        default:
            return math_runtime_raise(interp, "stats.normal-cdf: evaluation error");
    }
}

<*
@require interp != null
*>
fn Value* prim_stats_normal_quantile(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "stats.normal-quantile", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_NORMAL_QUANTILE, 0, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "stats.normal-quantile", 0, "number", args[0]);
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(
            interp, args[0], BIG_FLOAT_MATH_NORMAL_QUANTILE, "stats.normal-quantile: probability must be between 0 and 1");
    }

    double probability = 0.0;
    if (!try_numeric_to_double(args[0], &probability)) {
        return math_raise(interp, "type/range-error", "stats.normal-quantile: probability out of Float64 range");
    }

    double output = 0.0;
    int status = omni_boost_math_standard_normal_quantile(probability, &output);
    switch (status) {
        case BOOST_MATH_STATUS_OK:
            return make_double(interp, output);
        case BOOST_MATH_STATUS_INVALID_ARGUMENT:
            return math_raise(interp, "type/arg-mismatch", "stats.normal-quantile: expected finite numeric input");
        case BOOST_MATH_STATUS_DOMAIN_ERROR:
            return math_raise(interp, "type/domain-error", "stats.normal-quantile: probability must be between 0 and 1");
        case BOOST_MATH_STATUS_RANGE_ERROR:
            return math_raise(interp, "type/range-error", "stats.normal-quantile: range error");
        default:
            return math_runtime_raise(interp, "stats.normal-quantile: evaluation error");
    }
}

<*
@require interp != null
*>
fn Value* prim_sin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "sin", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_SIN, BIG_COMPLEX_MATH_SIN, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_SIN, "sin: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_SIN, "sin: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "sin", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_sin(x));
}

<*
@require interp != null
*>
fn Value* prim_cos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "cos", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_COS, BIG_COMPLEX_MATH_COS, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_COS, "cos: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_COS, "cos: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "cos", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_cos(x));
}

<*
@require interp != null
*>
fn Value* prim_tan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "tan", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_TAN, BIG_COMPLEX_MATH_TAN, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_TAN, "tan: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_TAN, "tan: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "tan", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_tan(x));
}

<*
@require interp != null
*>
fn Value* prim_sinh(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "sinh", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_SINH, BIG_COMPLEX_MATH_SINH, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_SINH, "sinh: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_SINH, "sinh: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "sinh", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_sinh(x));
}

<*
@require interp != null
*>
fn Value* prim_cosh(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "cosh", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_COSH, BIG_COMPLEX_MATH_COSH, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_COSH, "cosh: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_COSH, "cosh: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "cosh", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_cosh(x));
}

<*
@require interp != null
*>
fn Value* prim_tanh(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "tanh", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_TANH, BIG_COMPLEX_MATH_TANH, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_TANH, "tanh: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_TANH, "tanh: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "tanh", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_tanh(x));
}

<*
@require interp != null
*>
fn Value* prim_asin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "asin", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_ASIN, BIG_COMPLEX_MATH_ASIN, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_ASIN, "asin: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_ASIN, "asin: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "asin", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_asin(x));
}

<*
@require interp != null
*>
fn Value* prim_acos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "acos", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_ACOS, BIG_COMPLEX_MATH_ACOS, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_ACOS, "acos: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_ACOS, "acos: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "acos", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_acos(x));
}

<*
@require interp != null
*>
fn Value* prim_atan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "atan", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_ATAN, BIG_COMPLEX_MATH_ATAN, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_ATAN, "atan: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_ATAN, "atan: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "atan", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_atan(x));
}

<*
@require interp != null
*>
fn Value* prim_atan2(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "atan2", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) return tensor_atan2_value(args[0], args[1], interp);
    if (is_big_complex(args[0]) || is_big_complex(args[1])) {
        return math_raise(interp, "type/unsupported", "atan2: complex numbers are not supported");
    }
    if (is_big_float(args[0]) || is_big_float(args[1])) {
        if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "atan2", 0, "number", args[0]);
        if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "atan2", 1, "number", args[1]);
        return big_float_binary_math_values(
            interp, args[0], args[1], BIG_FLOAT_MATH_ATAN2, "atan2: BigFloat evaluation error");
    }
    double y = 0.0;
    double x = 0.0;
    if (!math_arg_to_double(args[0], &y)) return primitive_arg_type_raise(interp, "atan2", 0, "number representable as Float64", args[0]);
    if (!math_arg_to_double(args[1], &x)) return primitive_arg_type_raise(interp, "atan2", 1, "number representable as Float64", args[1]);
    return make_double(interp, c_atan2(y, x));
}

<*
@require interp != null
*>
fn Value* prim_exp(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "exp", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_EXP, BIG_COMPLEX_MATH_EXP, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_EXP, "exp: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_EXP, "exp: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "exp", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_exp(x));
}

<*
@require interp != null
*>
fn Value* prim_log(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "log", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_LOG, BIG_COMPLEX_MATH_LOG, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_LOG, "log: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_LOG, "log: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "log", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_log(x));
}

<*
@require interp != null
*>
fn Value* prim_log10(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "log10", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_unary_math_value(args[0], BIG_FLOAT_MATH_LOG10, BIG_COMPLEX_MATH_LOG10, interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_LOG10, "log10: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_LOG10, "log10: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "log10", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_log10(x));
}

<*
@require interp != null
*>
fn Value* prim_pow(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "pow", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) return tensor_pow_value(args[0], args[1], interp);
    if (is_big_complex(args[0]) || is_big_complex(args[1])) {
        if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "pow", 0, "number", args[0]);
        if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "pow", 1, "number", args[1]);
        return big_complex_binary_math_values(
            interp, args[0], args[1], BIG_COMPLEX_MATH_POW, "pow: BigComplex evaluation error");
    }
    if (is_big_float(args[0]) || is_big_float(args[1])) {
        if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "pow", 0, "number", args[0]);
        if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "pow", 1, "number", args[1]);
        return big_float_binary_math_values(
            interp, args[0], args[1], BIG_FLOAT_MATH_POW, "pow: BigFloat evaluation error");
    }
    double base = 0.0;
    double exp = 0.0;
    if (!math_arg_to_double(args[0], &base)) return primitive_arg_type_raise(interp, "pow", 0, "number representable as Float64", args[0]);
    if (!math_arg_to_double(args[1], &exp)) return primitive_arg_type_raise(interp, "pow", 1, "number representable as Float64", args[1]);
    return make_double(interp, c_pow(base, exp));
}

<*
@require interp != null
*>
fn Value* prim_sqrt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "sqrt", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_sqrt_value(args[0], interp);
    if (is_big_complex(args[0])) {
        return big_complex_unary_math_value(interp, args[0], BIG_COMPLEX_MATH_SQRT, "sqrt: BigComplex evaluation error");
    }
    if (is_big_float(args[0])) {
        return big_float_unary_math_value(interp, args[0], BIG_FLOAT_MATH_SQRT, "sqrt: BigFloat evaluation error");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "sqrt", 0, "number representable as Float64", args[0]);
    return make_double(interp, c_sqrt(x));
}

<*
@require interp != null
*>
fn Value* prim_real_part(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "real-part", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_big_complex_component_value(args[0], false, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "real-part", 0, "number", args[0]);
    if (is_complex128(args[0])) return make_double(interp, args[0].complex128_val.real);
    if (is_complex64(args[0])) return make_float32(interp, args[0].complex64_val.real);
    return big_complex_real_part_value(interp, args[0], "real-part: failed to extract numeric real part");
}

<*
@require interp != null
*>
fn Value* prim_imag_part(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "imag-part", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_big_complex_component_value(args[0], true, interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "imag-part", 0, "number", args[0]);
    if (is_complex128(args[0])) return make_double(interp, args[0].complex128_val.imag);
    if (is_complex64(args[0])) return make_float32(interp, args[0].complex64_val.imag);
    return big_complex_imag_part_value(interp, args[0], "imag-part: failed to extract numeric imaginary part");
}

<*
@require interp != null
*>
fn Value* prim_conjugate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "conjugate", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_big_complex_conjugate_value(args[0], interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "conjugate", 0, "number", args[0]);
    if (is_complex128(args[0])) return make_complex128(interp, args[0].complex128_val.real, -args[0].complex128_val.imag);
    if (is_complex64(args[0])) return make_complex64(interp, args[0].complex64_val.real, -args[0].complex64_val.imag);
    return big_complex_conjugate_value(interp, args[0], "conjugate: failed to allocate BigComplex result");
}

<*
@require interp != null
*>
fn Value* prim_floor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "floor", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_floor_value(args[0], interp);
    if (is_big_float(args[0])) {
        return big_float_round_to_integer_value(interp, args[0], BIG_FLOAT_ROUND_FLOOR, "floor: BigFloat integer result out of supported range");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "floor", 0, "number representable as Float64", args[0]);
    return math_int_from_double_or_raise(interp, c_floor(x), "floor");
}

<*
@require interp != null
*>
fn Value* prim_ceiling(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "ceiling", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_ceiling_value(args[0], interp);
    if (is_big_float(args[0])) {
        return big_float_round_to_integer_value(interp, args[0], BIG_FLOAT_ROUND_CEILING, "ceiling: BigFloat integer result out of supported range");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "ceiling", 0, "number representable as Float64", args[0]);
    return math_int_from_double_or_raise(interp, c_ceil(x), "ceiling");
}

<*
@require interp != null
*>
fn Value* prim_round(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "round", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_round_value(args[0], interp);
    if (is_big_float(args[0])) {
        return big_float_round_to_integer_value(interp, args[0], BIG_FLOAT_ROUND_ROUND, "round: BigFloat integer result out of supported range");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "round", 0, "number representable as Float64", args[0]);
    return math_int_from_double_or_raise(interp, c_round(x), "round");
}

<*
@require interp != null
*>
fn Value* prim_truncate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "truncate", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_truncate_value(args[0], interp);
    if (is_big_float(args[0])) {
        return big_float_round_to_integer_value(interp, args[0], BIG_FLOAT_ROUND_TRUNCATE, "truncate: BigFloat integer result out of supported range");
    }
    double x = 0.0;
    if (!math_arg_to_double(args[0], &x)) return primitive_arg_type_raise(interp, "truncate", 0, "number representable as Float64", args[0]);
    return math_int_from_double_or_raise(interp, x, "truncate");
}

<*
@require interp != null
*>
fn Value* prim_abs(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "abs", "1 argument", args.len);
    if (is_tensor(args[0])) return tensor_abs_value(args[0], interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "abs", 0, "number", args[0]);
    if (is_complex128(args[0])) return make_double(interp, complex128_abs_value(args[0].complex128_val));
    if (is_complex64(args[0])) return make_float32(interp, complex64_abs_value(args[0].complex64_val));
    if (is_big_complex(args[0])) return big_complex_abs_value(interp, args[0], "abs: failed to allocate BigFloat result");
    if (is_big_float(args[0])) return big_float_abs_value(interp, args[0], "abs: failed to allocate BigFloat result");
    if (is_double(args[0])) return make_double(interp, c_fabs(args[0].double_val));
    if (is_float32(args[0])) return make_float32(interp, (float)c_fabs((double)args[0].float32_val));
    if (args[0].tag == BIG_INTEGER) {
        int cmp = omni_big_integer_compare_i64(args[0].big_integer_val, 0);
        if (cmp < 0) return big_integer_neg_value(interp, args[0], "abs: failed to allocate BigInteger result");
        return args[0];
    }
    long n = args[0].int_val;
    if (n == long.min) return big_integer_neg_value(interp, args[0], "abs: failed to allocate BigInteger overflow result");
    return make_int(interp, n < 0 ? -n : n);
}

<*
@require interp != null
*>
fn Value* prim_min(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "min", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) return tensor_minmax_value(args[0], args[1], true, "min", interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "min", 0, "number", args[0]);
    if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "min", 1, "number", args[1]);
    if (is_big_complex(args[0]) || is_big_complex(args[1]) || is_fixed_complex(args[0]) || is_fixed_complex(args[1])) return math_raise(interp, "type/unsupported", "min: complex numbers are not ordered");
    if (is_big_float(args[0]) || is_big_float(args[1])) {
        int cmp = 0;
        if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "min: numeric comparison failed");
        return cmp <= 0 ? args[0] : args[1];
    }
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        int cmp = 0;
        if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "min: numeric comparison failed");
        return cmp <= 0 ? args[0] : args[1];
    }
    if (is_double(args[0]) || is_double(args[1])) {
        double a = 0.0;
        double b = 0.0;
        if (!try_numeric_to_double(args[0], &a) || !try_numeric_to_double(args[1], &b)) {
            return math_raise(interp, "type/range-error", "min: BigInteger value is out of Float64 range");
        }
        return make_double(interp, a < b ? a : b);
    }
    if (is_float32(args[0]) || is_float32(args[1])) {
        double a = 0.0;
        double b = 0.0;
        if (!try_numeric_to_double(args[0], &a) || !try_numeric_to_double(args[1], &b)) {
            return math_raise(interp, "type/range-error", "min: BigInteger value is out of Float64 range");
        }
        return make_float32(interp, (float)(a < b ? a : b));
    }
    int cmp = 0;
    if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "min: numeric comparison failed");
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) return cmp <= 0 ? args[0] : args[1];
    return make_int(interp, args[0].int_val < args[1].int_val ? args[0].int_val : args[1].int_val);
}

<*
@require interp != null
*>
fn Value* prim_max(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "max", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) return tensor_minmax_value(args[0], args[1], false, "max", interp);
    if (!is_numeric_value(args[0])) return primitive_arg_type_raise(interp, "max", 0, "number", args[0]);
    if (!is_numeric_value(args[1])) return primitive_arg_type_raise(interp, "max", 1, "number", args[1]);
    if (is_big_complex(args[0]) || is_big_complex(args[1]) || is_fixed_complex(args[0]) || is_fixed_complex(args[1])) return math_raise(interp, "type/unsupported", "max: complex numbers are not ordered");
    if (is_big_float(args[0]) || is_big_float(args[1])) {
        int cmp = 0;
        if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "max: numeric comparison failed");
        return cmp >= 0 ? args[0] : args[1];
    }
    if (is_big_integer(args[0]) || is_big_integer(args[1])) {
        int cmp = 0;
        if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "max: numeric comparison failed");
        return cmp >= 0 ? args[0] : args[1];
    }
    if (is_double(args[0]) || is_double(args[1])) {
        double a = 0.0;
        double b = 0.0;
        if (!try_numeric_to_double(args[0], &a) || !try_numeric_to_double(args[1], &b)) {
            return math_raise(interp, "type/range-error", "max: BigInteger value is out of Float64 range");
        }
        return make_double(interp, a > b ? a : b);
    }
    if (is_float32(args[0]) || is_float32(args[1])) {
        double a = 0.0;
        double b = 0.0;
        if (!try_numeric_to_double(args[0], &a) || !try_numeric_to_double(args[1], &b)) {
            return math_raise(interp, "type/range-error", "max: BigInteger value is out of Float64 range");
        }
        return make_float32(interp, (float)(a > b ? a : b));
    }
    int cmp = 0;
    if (!numeric_values_compare(args[0], args[1], &cmp)) return math_runtime_raise(interp, "max: numeric comparison failed");
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) return cmp >= 0 ? args[0] : args[1];
    return make_int(interp, args[0].int_val > args[1].int_val ? args[0].int_val : args[1].int_val);
}

<*
@require interp != null
*>
fn Value* prim_gcd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "gcd", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) {
        return tensor_exact_integer_binary_value(
            args[0], args[1], BIG_INTEGER_OP_GCD,
            "gcd: tensor inputs must be exact integers",
            "gcd: tensor BigInteger element is missing",
            "gcd: tensor shape mismatch",
            "gcd: failed to allocate BigInteger tensor result",
            interp);
    }
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "gcd", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "gcd", 1, "integer", args[1]);
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_GCD, "gcd: failed to allocate BigInteger result");
    }
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == long.min || b == long.min) {
        return big_integer_binary_values_or_integer(
            interp, args[0], args[1], BIG_INTEGER_OP_GCD, "gcd: failed to allocate BigInteger overflow result");
    }
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    while (b != 0) { long t = b; b = a % b; a = t; }
    return make_int(interp, a);
}

<*
@require interp != null
*>
fn Value* prim_lcm(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "lcm", "2 arguments", args.len);
    if (is_tensor(args[0]) || is_tensor(args[1])) {
        return tensor_exact_integer_binary_value(
            args[0], args[1], BIG_INTEGER_OP_LCM,
            "lcm: tensor inputs must be exact integers",
            "lcm: tensor BigInteger element is missing",
            "lcm: tensor shape mismatch",
            "lcm: failed to allocate BigInteger tensor result",
            interp);
    }
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "lcm", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "lcm", 1, "integer", args[1]);
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) {
        return big_integer_binary_values(interp, args[0], args[1], BIG_INTEGER_OP_LCM, "lcm: failed to allocate BigInteger result");
    }
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == 0 || b == 0) return make_int(interp, 0);
    if (a == long.min || b == long.min) {
        return big_integer_binary_values_or_integer(
            interp, args[0], args[1], BIG_INTEGER_OP_LCM, "lcm: failed to allocate BigInteger overflow result");
    }
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    long ga = a; long gb = b;
    while (gb != 0) { long t = gb; gb = ga % gb; ga = t; }
    long factor = a / ga;
    long result = 0;
    if (math::overflow_mul(factor, b, &result)) {
        return big_integer_binary_values_or_integer(
            interp, args[0], args[1], BIG_INTEGER_OP_LCM, "lcm: failed to allocate BigInteger overflow result");
    }
    return make_int(interp, result);
}

<*
@require interp != null
*>
fn Value* prim_bitwise_and(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-and", "2 arguments", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "bitwise-and", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "bitwise-and", 1, "integer", args[1]);
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) {
        return big_integer_binary_values(
            interp, args[0], args[1], BIG_INTEGER_OP_BIT_AND, "bitwise-and: failed to allocate BigInteger result");
    }
    return make_int(interp, args[0].int_val & args[1].int_val);
}

<*
@require interp != null
*>
fn Value* prim_bitwise_or(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-or", "2 arguments", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "bitwise-or", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "bitwise-or", 1, "integer", args[1]);
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) {
        return big_integer_binary_values(
            interp, args[0], args[1], BIG_INTEGER_OP_BIT_OR, "bitwise-or: failed to allocate BigInteger result");
    }
    return make_int(interp, args[0].int_val | args[1].int_val);
}

<*
@require interp != null
*>
fn Value* prim_bitwise_xor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "bitwise-xor", "2 arguments", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "bitwise-xor", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "bitwise-xor", 1, "integer", args[1]);
    if (args[0].tag == BIG_INTEGER || args[1].tag == BIG_INTEGER) {
        return big_integer_binary_values(
            interp, args[0], args[1], BIG_INTEGER_OP_BIT_XOR, "bitwise-xor: failed to allocate BigInteger result");
    }
    return make_int(interp, args[0].int_val ^ args[1].int_val);
}

<*
@require interp != null
*>
fn Value* prim_bitwise_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "bitwise-not", "1 argument", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "bitwise-not", 0, "integer", args[0]);
    if (args[0].tag == BIG_INTEGER) {
        return big_integer_bitwise_not_value(
            interp, args[0], false, "bitwise-not: failed to allocate BigInteger result");
    }
    return make_int(interp, ~args[0].int_val);
}

<*
@require interp != null
*>
fn Value* prim_lshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "lshift", "2 arguments", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "lshift", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "lshift", 1, "integer", args[1]);
    long shift = 0;
    if (!exact_integer_value_to_i64(args[1], &shift)) return math_raise(interp, "type/range-error", "lshift: shift count out of range");
    if (shift < 0) return make_int(interp, 0);
    if (shift > BIG_INTEGER_MAX_SHIFT_BITS) return math_raise(interp, "type/range-error", "lshift: shift count out of range");
    return big_integer_shift_value(
        interp, args[0], (ulong)shift, true, args[0].tag == INT, "lshift: failed to allocate BigInteger result");
}

<*
@require interp != null
*>
fn Value* prim_rshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "rshift", "2 arguments", args.len);
    if (!is_exact_integer_value(args[0])) return primitive_arg_type_raise(interp, "rshift", 0, "integer", args[0]);
    if (!is_exact_integer_value(args[1])) return primitive_arg_type_raise(interp, "rshift", 1, "integer", args[1]);
    long shift = 0;
    if (!exact_integer_value_to_i64(args[1], &shift)) return math_raise(interp, "type/range-error", "rshift: shift count out of range");
    if (shift < 0) return make_int(interp, 0);
    if (shift > BIG_INTEGER_MAX_SHIFT_BITS) return math_raise(interp, "type/range-error", "rshift: shift count out of range");
    return big_integer_shift_value(
        interp, args[0], (ulong)shift, false, args[0].tag == INT, "rshift: failed to allocate BigInteger result");
}
