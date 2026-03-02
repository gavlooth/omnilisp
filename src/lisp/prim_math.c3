module lisp;

import std::io;
import std::core::mem;
import main;

// =============================================================================
// MATH PRIMITIVES
// =============================================================================

fn Value* prim_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "+: expected at least 1 argument");

    if (args.len == 1) {
        if (!is_number(args[0])) return raise_error(interp, "+: expected number argument");
        return args[0];
    }

    if (!is_number(args[0])) return raise_error(interp, "+: expected number argument");
    if (!is_number(args[1])) return raise_error(interp, "+: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) + to_double(args[1]));
    }
    return make_int(interp, args[0].int_val + args[1].int_val);
}

fn Value* prim_sub(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "-: expected at least 1 argument");
    if (args.len == 1) {
        if (!is_number(args[0])) return raise_error(interp, "-: expected number argument");
        if (is_double(args[0])) return make_double(interp, -args[0].double_val);
        return make_int(interp, -args[0].int_val);
    }
    if (!is_number(args[0])) return raise_error(interp, "-: expected number argument");
    if (!is_number(args[1])) return raise_error(interp, "-: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) - to_double(args[1]));
    }
    return make_int(interp, args[0].int_val - args[1].int_val);
}

fn Value* prim_mul(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "*: expected 2 arguments");
    if (!is_number(args[0])) return raise_error(interp, "*: expected number argument");
    if (!is_number(args[1])) return raise_error(interp, "*: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return make_double(interp, to_double(args[0]) * to_double(args[1]));
    }
    return make_int(interp, args[0].int_val * args[1].int_val);
}

fn Value* prim_div(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "/: expected 2 arguments");
    if (!is_number(args[0])) return raise_error(interp, "/: expected number argument");
    if (!is_number(args[1])) return raise_error(interp, "/: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        double b = to_double(args[1]);
        if (b == 0.0) return raise_error(interp, "/: division by zero");
        return make_double(interp, to_double(args[0]) / b);
    }
    long b = args[1].int_val;
    if (b == 0) return raise_error(interp, "/: division by zero");
    return make_int(interp, args[0].int_val / b);
}

fn Value* prim_mod(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "%: expected 2 arguments");
    if (!is_number(args[0])) return raise_error(interp, "%: expected number argument");
    if (!is_number(args[1])) return raise_error(interp, "%: expected number argument");
    if (is_double(args[0]) || is_double(args[1])) {
        return raise_error(interp, "%: expected integer arguments");
    }
    long a = args[0].int_val;
    long b = args[1].int_val;
    if (b == 0) return raise_error(interp, "%: division by zero");
    return make_int(interp, a % b);
}

// --- Trig and math library ---

import std::math;

// C math functions (linked via -lm)
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

fn Value* prim_sin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "sin: expected number");
    return make_double(interp, c_sin(to_double(args[0])));
}

fn Value* prim_cos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "cos: expected number");
    return make_double(interp, c_cos(to_double(args[0])));
}

fn Value* prim_tan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "tan: expected number");
    return make_double(interp, c_tan(to_double(args[0])));
}

fn Value* prim_asin(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "asin: expected number");
    return make_double(interp, c_asin(to_double(args[0])));
}

fn Value* prim_acos(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "acos: expected number");
    return make_double(interp, c_acos(to_double(args[0])));
}

fn Value* prim_atan(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "atan: expected number");
    return make_double(interp, c_atan(to_double(args[0])));
}

fn Value* prim_atan2(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return raise_error(interp, "atan2: expected 2 numbers");
    return make_double(interp, c_atan2(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_exp(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "exp: expected number");
    return make_double(interp, c_exp(to_double(args[0])));
}

fn Value* prim_log(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "log: expected number");
    return make_double(interp, c_log(to_double(args[0])));
}

fn Value* prim_log10(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "log10: expected number");
    return make_double(interp, c_log10(to_double(args[0])));
}

fn Value* prim_pow(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return raise_error(interp, "pow: expected 2 numbers");
    return make_double(interp, c_pow(to_double(args[0]), to_double(args[1])));
}

fn Value* prim_sqrt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "sqrt: expected number");
    return make_double(interp, c_sqrt(to_double(args[0])));
}

fn Value* prim_floor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "floor: expected number");
    return make_int(interp, (long)c_floor(to_double(args[0])));
}

fn Value* prim_ceiling(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "ceiling: expected number");
    return make_int(interp, (long)c_ceil(to_double(args[0])));
}

fn Value* prim_round(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "round: expected number");
    return make_int(interp, (long)c_round(to_double(args[0])));
}

fn Value* prim_truncate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "truncate: expected number");
    return make_int(interp, (long)to_double(args[0]));
}

fn Value* prim_abs(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "abs: expected number");
    if (is_double(args[0])) return make_double(interp, c_fabs(args[0].double_val));
    long n = args[0].int_val;
    return make_int(interp, n < 0 ? -n : n);
}

fn Value* prim_min(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return raise_error(interp, "min: expected 2 numbers");
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a < b ? a : b);
    }
    return make_int(interp, args[0].int_val < args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_max(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_number(args[0]) || !is_number(args[1])) return raise_error(interp, "max: expected 2 numbers");
    if (is_double(args[0]) || is_double(args[1])) {
        double a = to_double(args[0]); double b = to_double(args[1]);
        return make_double(interp, a > b ? a : b);
    }
    return make_int(interp, args[0].int_val > args[1].int_val ? args[0].int_val : args[1].int_val);
}

fn Value* prim_gcd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "gcd: expected 2 integers");
    long a = args[0].int_val; long b = args[1].int_val;
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    while (b != 0) { long t = b; b = a % b; a = t; }
    return make_int(interp, a);
}

fn Value* prim_lcm(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "lcm: expected 2 integers");
    long a = args[0].int_val; long b = args[1].int_val;
    if (a == 0 || b == 0) return make_int(interp, 0);
    if (a < 0) { a = -a; }
    if (b < 0) { b = -b; }
    // gcd
    long ga = a; long gb = b;
    while (gb != 0) { long t = gb; gb = ga % gb; ga = t; }
    return make_int(interp, (a / ga) * b);
}

// --- Bitwise operations ---

fn Value* prim_bitwise_and(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "bitwise-and: expected 2 integers");
    return make_int(interp, args[0].int_val & args[1].int_val);
}

fn Value* prim_bitwise_or(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "bitwise-or: expected 2 integers");
    return make_int(interp, args[0].int_val | args[1].int_val);
}

fn Value* prim_bitwise_xor(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "bitwise-xor: expected 2 integers");
    return make_int(interp, args[0].int_val ^ args[1].int_val);
}

fn Value* prim_bitwise_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return raise_error(interp, "bitwise-not: expected integer");
    return make_int(interp, ~args[0].int_val);
}

fn Value* prim_lshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "lshift: expected 2 integers");
    long shift = args[1].int_val;
    if (shift < 0 || shift >= 64) return make_int(interp, 0);
    return make_int(interp, args[0].int_val << shift);
}

fn Value* prim_rshift(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_int(args[0]) || !is_int(args[1])) return raise_error(interp, "rshift: expected 2 integers");
    long shift = args[1].int_val;
    if (shift < 0 || shift >= 64) return make_int(interp, 0);
    return make_int(interp, args[0].int_val >> shift);
}

// --- Random ---

extern fn int c_getrandom(void* buf, usz buflen, uint flags) @extern("getrandom");

fn Value* prim_random(Value*[] args, Env* env, Interp* interp) {
    ulong buf;
    c_getrandom(&buf, 8, 0);
    // Convert to double in [0, 1): mask to 52 bits, divide by 2^52
    ulong mantissa = buf & 0x000F_FFFF_FFFF_FFFF;
    double val = (double)mantissa / 4503599627370496.0;  // 2^52
    return make_double(interp, val);
}

fn Value* prim_random_int(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "random-int: expected max value");
    long n;
    if (args[0].tag == INT) { n = args[0].int_val; }
    else if (args[0].tag == DOUBLE) { n = (long)args[0].double_val; }
    else { return raise_error(interp, "random-int: expected number"); }
    if (n <= 0) return make_int(interp, 0);

    ulong buf;
    c_getrandom(&buf, 8, 0);
    long result = (long)(buf % (ulong)n);
    return make_int(interp, result);
}
