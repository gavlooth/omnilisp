module lisp;

import std::io;
import main;

// =============================================================================
// SECTION: STRING/NUMBER/SYMBOL CONVERSIONS
// =============================================================================
fn Value* conversion_type_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", message, null);
}

fn Value* conversion_runtime_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, message, null);
}

const double CONVERSION_DOUBLE_FINITE_LIMIT = 1.7976931348623157e308;
const double CONVERSION_INT_MAX_DOUBLE = (double)long.max;
const double CONVERSION_INT_MIN_DOUBLE = (double)long.min;

fn bool conversion_double_is_finite(double v) @inline {
    if (v != v) return false; // NaN
    return v <= CONVERSION_DOUBLE_FINITE_LIMIT && v >= -CONVERSION_DOUBLE_FINITE_LIMIT;
}

fn bool conversion_try_double_to_int(double v, long* out) @inline {
    if (!conversion_double_is_finite(v)) return false;
    if (v < CONVERSION_INT_MIN_DOUBLE || v > CONVERSION_INT_MAX_DOUBLE) return false;
    *out = (long)v; // truncation toward zero
    return true;
}

fn Value* conversion_int_from_double_or_raise(Interp* interp, double v, char[] message) @inline {
    long out = 0;
    if (!conversion_try_double_to_int(v, &out)) return conversion_type_raise(interp, message);
    return make_int(interp, out);
}

fn Value* prim_ctor_int(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Integer: expected int, finite double, or in-range numeric string");

    Value* arg = args[0];
    if (arg == null || arg.tag == NIL) {
        return conversion_type_raise(interp, "Integer: expected int, finite double, or in-range numeric string");
    }
    if (arg.tag == INT) return arg;
    if (arg.tag == DOUBLE) {
        return conversion_int_from_double_or_raise(
            interp,
            arg.double_val,
            "Integer: expected int, finite double, or in-range numeric string");
    }
    if (arg.tag == STRING) {
        Value* parsed = prim_string_to_number(args, env, interp);
        if (parsed != null && parsed.tag != NIL) {
            if (parsed.tag == INT) return parsed;
            if (parsed.tag == DOUBLE) {
                return conversion_int_from_double_or_raise(
                    interp,
                    parsed.double_val,
                    "Integer: expected int, finite double, or in-range numeric string");
            }
        }
    }
    return conversion_type_raise(interp, "Integer: expected int, finite double, or in-range numeric string");
}

fn Value* prim_ctor_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "String: expected string, number, or symbol");

    Value* arg = args[0];
    if (arg == null || arg.tag == NIL) {
        return conversion_type_raise(interp, "String: expected string, number, or symbol");
    }
    if (arg.tag == STRING) return arg;
    if (is_number(arg)) return prim_number_to_string(args, env, interp);
    if (arg.tag == SYMBOL) return prim_symbol_to_string(args, env, interp);
    return conversion_type_raise(interp, "String: expected string, number, or symbol");
}

fn Value* prim_ctor_double(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Double: expected int, double, or numeric string");

    Value* arg = args[0];
    if (arg == null || arg.tag == NIL) {
        return conversion_type_raise(interp, "Double: expected int, double, or numeric string");
    }
    if (arg.tag == DOUBLE) return arg;
    if (arg.tag == INT) return make_double(interp, (double)arg.int_val);
    if (arg.tag == STRING) {
        Value* parsed = prim_string_to_number(args, env, interp);
        if (parsed != null && parsed.tag != NIL) {
            if (parsed.tag == DOUBLE) return parsed;
            if (parsed.tag == INT) return make_double(interp, (double)parsed.int_val);
        }
    }
    return conversion_type_raise(interp, "Double: expected int, double, or numeric string");
}

fn Value* prim_ctor_symbol(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Symbol: expected symbol or string");

    Value* arg = args[0];
    if (arg == null || arg.tag == NIL) {
        return conversion_type_raise(interp, "Symbol: expected symbol or string");
    }
    if (arg.tag == SYMBOL) return arg;
    if (arg.tag == STRING) return prim_string_to_symbol(args, env, interp);
    return conversion_type_raise(interp, "Symbol: expected symbol or string");
}

fn Value* prim_ctor_bool(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Boolean: expected 1 argument");
    return is_falsy(args[0], interp) ? make_symbol(interp, interp.sym_false) : make_symbol(interp, interp.sym_true);
}

fn Value* prim_ctor_nil(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Nil: expected nil");
    Value* arg = args[0];
    return (arg == null || arg.tag == NIL) ? make_nil(interp) : conversion_type_raise(interp, "Nil: expected nil");
}

fn Value* prim_ctor_void(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return conversion_type_raise(interp, "Void: expected 0 arguments");
    return make_void(interp);
}

fn Value* prim_ctor_closure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return conversion_type_raise(interp, "Closure: expected closure");
    Value* arg = args[0];
    if (is_closure(arg)) return arg;
    return conversion_type_raise(interp, "Closure: expected closure");
}

fn Value* prim_string_to_number(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) return make_nil(interp);
    char[] s = args[0].str_chars[:args[0].str_len];
    if (s.len == 0) return make_nil(interp);
    // Check if it contains a dot or exponent → parse as float
    bool is_float = false;
    for (usz j = 0; j < s.len; j++) {
        if (s[j] == '.' || s[j] == 'e' || s[j] == 'E') { is_float = true; break; }
    }
    if (is_float) {
        // Parse as double: [+-]digits[.digits][e[+-]digits]
        usz i = 0;
        bool negative = false;
        if (i < s.len && (s[i] == '-' || s[i] == '+')) { negative = s[i] == '-'; i++; }
        if (i >= s.len) return make_nil(interp);
        double val = 0.0;
        bool has_digits = false;
        while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val * 10.0 + (double)(s[i] - '0'); i++; has_digits = true; }
        if (i < s.len && s[i] == '.') {
            i++;
            double frac = 0.1;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val + (double)(s[i] - '0') * frac; frac = frac * 0.1; i++; has_digits = true; }
        }
        if (!has_digits) return make_nil(interp);
        if (i < s.len && (s[i] == 'e' || s[i] == 'E')) {
            i++;
            bool exp_neg = false;
            if (i < s.len && (s[i] == '-' || s[i] == '+')) { exp_neg = s[i] == '-'; i++; }
            long exp_val = 0;
            bool has_exp_digits = false;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') {
                has_exp_digits = true;
                if (exp_val > 10000) return make_nil(interp); // bounded parse cost / overflow guard
                exp_val = exp_val * 10 + (long)(s[i] - '0');
                i++;
            }
            if (!has_exp_digits) return make_nil(interp);
            double mult = 1.0;
            for (long e = 0; e < exp_val; e++) mult = mult * 10.0;
            if (exp_neg) { val = val / mult; } else { val = val * mult; }
        }
        if (i != s.len) return make_nil(interp);
        if (negative) val = -val;
        if (!conversion_double_is_finite(val)) return make_nil(interp);
        return make_double(interp, val);
    }
    // Integer parse
    usz i = 0;
    bool negative = false;
    if (s[0] == '-' || s[0] == '+') { negative = s[0] == '-'; i = 1; }
    if (i >= s.len) return make_nil(interp);
    long val = 0;
    while (i < s.len) {
        if (s[i] < '0' || s[i] > '9') return make_nil(interp);
        long digit = (long)(s[i] - '0');
        if (negative) {
            if (val < (long.min + digit) / 10) return make_nil(interp);
            val = val * 10 - digit;
        } else {
            if (val > (long.max - digit) / 10) return make_nil(interp);
            val = val * 10 + digit;
        }
        i++;
    }
    return make_int(interp, val);
}

fn Value* prim_number_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return conversion_type_raise(interp, "number->string: expected number");
    char[64] buf;
    if (is_double(args[0])) {
        return make_string(interp, double_to_string(args[0].double_val, &buf));
    }
    return make_string(interp, int_to_string(args[0].int_val, &buf));
}

fn Value* prim_exact_to_inexact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return conversion_type_raise(interp, "exact->inexact: expected number");
    return make_double(interp, to_double(args[0]));
}

fn Value* prim_inexact_to_exact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return conversion_type_raise(interp, "inexact->exact: expected number");
    return conversion_int_from_double_or_raise(interp, to_double(args[0]), "inexact->exact: expected finite number in Integer range");
}

fn Value* prim_read_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return conversion_type_raise(interp, "read-string: expected string");
    char[] src = args[0].str_chars[:args[0].str_len];
    EvalResult r = run(src, interp);
    if (r.error.has_error) return conversion_runtime_raise(interp, "runtime/read-string-failed", "read-string: parse/eval error");
    return r.value;
}

fn Value* prim_string_to_symbol(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return conversion_type_raise(interp, "string->symbol: expected string");
    SymbolId sym = interp.symbols.intern(args[0].str_chars[:args[0].str_len]);
    return make_symbol(interp, sym);
}

fn Value* prim_symbol_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return conversion_type_raise(interp, "symbol->string: expected symbol");
    char[] name = interp.symbols.get_name(args[0].sym_val);
    return make_string(interp, name);
}
