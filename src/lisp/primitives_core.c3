module lisp;

import std::io;
import main;

// Core primitive operations and basic predicates.
// SECTION 7: PRIMITIVES
// =============================================================================

const int NUMERIC_COMPARE_LT = 1;
const int NUMERIC_COMPARE_GT = 2;
const int NUMERIC_COMPARE_LE = 3;
const int NUMERIC_COMPARE_GE = 4;

fn Value* numeric_compare_bool_result(Interp* interp, int cmp, int op) {
    bool result = false;
    switch (op) {
        case NUMERIC_COMPARE_LT:
            result = cmp < 0;
        case NUMERIC_COMPARE_GT:
            result = cmp > 0;
        case NUMERIC_COMPARE_LE:
            result = cmp <= 0;
        case NUMERIC_COMPARE_GE:
            result = cmp >= 0;
        default:
            return math_raise(interp, "type/unsupported", "numeric comparison: unknown comparison op");
    }
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* numeric_compare_primitive_result(Interp* interp, Value* lhs, Value* rhs, char[] op_name, int op) {
    if (!is_numeric_value(lhs)) return primitive_arg_type_raise(interp, op_name, 0, "number", lhs);
    if (!is_numeric_value(rhs)) return primitive_arg_type_raise(interp, op_name, 1, "number", rhs);

    int cmp = 0;
    if (!numeric_values_compare(lhs, rhs, &cmp)) {
        if (is_big_complex(lhs) || is_big_complex(rhs) || is_fixed_complex(lhs) || is_fixed_complex(rhs)) {
            return math_raise(interp, "type/arg-mismatch", "numeric comparison: complex numbers are not ordered");
        }
        char[128] ebuf;
        char[] msg = "numeric comparison: BigInteger value is out of Float64 range";
        if (try char[] formatted = io::bprintf(&ebuf, "%s: BigInteger value is out of Float64 range", (ZString)op_name)) {
            msg = formatted;
        }
        return math_raise(interp, "type/arg-mismatch", msg);
    }
    return numeric_compare_bool_result(interp, cmp, op);
}

<*
@require interp != null
*>
fn Value* prim_eq(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "=", "2 arguments", args.len);
    bool eq = values_equal(args[0], args[1]);
    return eq ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

<*
@require interp != null
*>
fn Value* prim_lt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "<", "2 arguments", args.len);
    return numeric_compare_primitive_result(interp, args[0], args[1], "<", NUMERIC_COMPARE_LT);
}

<*
@require interp != null
*>
fn Value* prim_gt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, ">", "2 arguments", args.len);
    return numeric_compare_primitive_result(interp, args[0], args[1], ">", NUMERIC_COMPARE_GT);
}

<*
@require interp != null
*>
fn Value* prim_le(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "<=", "2 arguments", args.len);
    return numeric_compare_primitive_result(interp, args[0], args[1], "<=", NUMERIC_COMPARE_LE);
}

<*
@require interp != null
*>
fn Value* prim_ge(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, ">=", "2 arguments", args.len);
    return numeric_compare_primitive_result(interp, args[0], args[1], ">=", NUMERIC_COMPARE_GE);
}

<*
@require interp != null
*>
fn Value* prim_cons(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "cons", "2 arguments", args.len);
    return make_cons(interp, args[0], args[1]);
}

<*
@require interp != null
*>
fn Value* prim_car(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "car", "1 argument", args.len);
    if (!is_cons(args[0])) return primitive_arg_type_raise(interp, "car", 0, "pair/list", args[0]);
    return args[0].cons_val.car;
}

<*
@require interp != null
*>
fn Value* prim_cdr(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "cdr", "1 argument", args.len);
    if (!is_cons(args[0])) return primitive_arg_type_raise(interp, "cdr", 0, "pair/list", args[0]);
    return args[0].cons_val.cdr;
}

<*
@require interp != null
*>
fn Value* prim_null(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "null?", "1 argument", args.len);
    bool is_null = is_nil(args[0]);
    return is_null ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

<*
@require interp != null
*>
fn Value* prim_pair(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "pair?", "1 argument", args.len);
    bool is_pair = is_cons(args[0]);
    return is_pair ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

<*
@require interp != null
*>
fn Value* prim_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "not", "1 argument", args.len);
    bool falsy = is_falsy(args[0], interp);
    return falsy ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

<*
@require interp != null
*>
fn Value* prim_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_nil(interp);

    // Single iterator arg -> consume to list
    if (args.len == 1 && args[0].tag == ITERATOR) {
        Value*[1] one = { args[0] };
        return prim_collect(one[0:1], env, interp);
    }

    // Single string arg -> split into codepoint strings.
    if (args.len == 1 && is_string(args[0])) {
        return string_to_list_value(args[0], interp,
            "List: expected string argument",
            "List: failed to allocate result list");
    }

    // Single array arg → convert to list
    if (args.len == 1 && is_array(args[0])) {
        Array* arr = args[0].array_val;
        Value* result = make_nil(interp);
        for (isz i = (isz)arr.length - 1; i >= 0; i--) {
            result = make_cons_or_error(interp, arr.items[(usz)i], result, "list: failed to allocate result list");
            if (result == null || result.tag == ERROR) return result;
        }
        return result;
    }

    // Single tensor arg -> realize and expose flat row-major values.
    if (args.len == 1 && is_tensor(args[0])) {
        return tensor_to_flat_list_value(args[0], interp);
    }

    // Single set arg -> deterministic canonical list of set elements.
    if (args.len == 1 && args[0].tag == SET) {
        return prim_set_to_list(args, env, interp);
    }

    Value* result = make_nil(interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) {
        result = make_cons_or_error(interp, args[(usz)i], result, "list: failed to allocate result list");
        if (result == null || result.tag == ERROR) return result;
    }
    return result;
}

<*
@require interp != null
*>
fn Value* prim_reverse_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "__reverse-list", "1 argument", args.len);
    if (!is_cons(args[0]) && !is_nil(args[0])) {
        return primitive_arg_type_raise(interp, "__reverse-list", 0, "list", args[0]);
    }
    return dispatch_reverse_list(args[0], interp);
}

// =============================================================================

/**
 * (continuation? v) -> true if v is a continuation
 */
<*
@require interp != null
*>
fn Value* prim_is_continuation(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "continuation?", "1 argument", args.len);
    bool result = args[0].tag == CONTINUATION;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}


<*
@require interp != null
*>
fn Value* prim_gensym(Value*[] args, Env* env, Interp* interp) {
    char[64] buf;
    buf[0] = 'g'; buf[1] = '#';
    usz len = 2;
    interp.gensym_counter++;
    long n = (long)interp.gensym_counter;
    // Convert number to string
    char[20] digits;
    usz dlen = 0;
    if (n == 0) { digits[0] = '0'; dlen = 1; }
    else {
        while (n > 0) { digits[dlen] = (char)('0' + (char)(n % 10)); dlen++; n = n / 10; }
    }
    for (usz j = 0; j < dlen; j++) {
        buf[len + j] = digits[dlen - 1 - j];
    }
    len += dlen;
    SymbolId id = interp.symbols.intern(buf[:len]);
    return make_symbol(interp, id);
}
