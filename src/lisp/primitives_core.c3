module lisp;

import std::io;
import main;

// Core primitive operations and basic predicates.
// SECTION 7: PRIMITIVES
// =============================================================================

fn Value* prim_eq(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "=", "2 arguments", args.len);
    bool eq = values_equal(args[0], args[1]);
    return eq ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_lt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "<", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "<", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "<", 1, "number", args[1]);
    bool lt = to_double(args[0]) < to_double(args[1]);
    return lt ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_gt(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, ">", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, ">", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, ">", 1, "number", args[1]);
    bool gt = to_double(args[0]) > to_double(args[1]);
    return gt ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_le(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "<=", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, "<=", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, "<=", 1, "number", args[1]);
    bool le = to_double(args[0]) <= to_double(args[1]);
    return le ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_ge(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, ">=", "2 arguments", args.len);
    if (!is_number(args[0])) return primitive_arg_type_raise(interp, ">=", 0, "number", args[0]);
    if (!is_number(args[1])) return primitive_arg_type_raise(interp, ">=", 1, "number", args[1]);
    bool ge = to_double(args[0]) >= to_double(args[1]);
    return ge ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_cons(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return primitive_arity_raise(interp, "cons", "2 arguments", args.len);
    return make_cons(interp, args[0], args[1]);
}

fn Value* prim_car(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "car", "1 argument", args.len);
    if (!is_cons(args[0])) return primitive_arg_type_raise(interp, "car", 0, "pair/list", args[0]);
    return args[0].cons_val.car;
}

fn Value* prim_cdr(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "cdr", "1 argument", args.len);
    if (!is_cons(args[0])) return primitive_arg_type_raise(interp, "cdr", 0, "pair/list", args[0]);
    return args[0].cons_val.cdr;
}

fn Value* prim_null(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "null?", "1 argument", args.len);
    bool is_null = is_nil(args[0]);
    return is_null ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_pair(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "pair?", "1 argument", args.len);
    bool is_pair = is_cons(args[0]);
    return is_pair ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_not(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "not", "1 argument", args.len);
    bool falsy = is_falsy(args[0], interp);
    return falsy ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_nil(interp);

    // Single iterator arg -> consume to list
    if (args.len == 1 && args[0].tag == ITERATOR) {
        Value*[1] one = { args[0] };
        return prim_collect(one[0:1], env, interp);
    }

    // Single array arg → convert to list
    if (args.len == 1 && is_array(args[0])) {
        Array* arr = args[0].array_val;
        Value* result = make_nil(interp);
        for (isz i = (isz)arr.length - 1; i >= 0; i--) {
            result = make_cons(interp, arr.items[(usz)i], result);
        }
        return result;
    }

    Value* result = make_nil(interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) {
        result = make_cons(interp, args[(usz)i], result);
    }
    return result;
}

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
fn Value* prim_is_continuation(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return primitive_arity_raise(interp, "continuation?", "1 argument", args.len);
    bool result = args[0].tag == CONTINUATION;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}


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
