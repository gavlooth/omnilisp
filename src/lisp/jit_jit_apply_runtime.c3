module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (apply/eval core)
// =============================================================================
fn Value* jit_make_int(Interp* interp, long n) {
    return make_int(interp, n);
}

fn Value* jit_make_nil(Interp* interp) {
    return make_nil(interp);
}

fn Value* jit_lookup_global(Interp* interp, uint sym_id) {
    SymbolId name = (SymbolId)sym_id;
    Value* v = interp.global_env.lookup(name);
    if (v != null) return v;
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "unbound variable '%s'",
        (ZString)interp.symbols.get_name(name))!!;
    return raise_error(interp, msg);
}

fn usz format_arity_error(char[] buf, usz expected, usz got) {
    char[] prefix = "arity mismatch: expected ";
    usz pos = 0;
    for (usz i = 0; i < prefix.len && pos < buf.len - 1; i++) buf[pos++] = prefix[i];
    // Write expected count
    char[20] num_buf;
    char[] num_str = int_to_string((long)expected, &num_buf);
    for (usz i = 0; i < num_str.len && pos < buf.len - 1; i++) buf[pos++] = num_str[i];
    char[] mid = " argument(s), got ";
    for (usz i = 0; i < mid.len && pos < buf.len - 1; i++) buf[pos++] = mid[i];
    num_str = int_to_string((long)got, &num_buf);
    for (usz i = 0; i < num_str.len && pos < buf.len - 1; i++) buf[pos++] = num_str[i];
    buf[pos] = 0;
    return pos;
}

fn Value* jit_apply_eval_result_or_error(EvalResult r, Interp* interp) @inline {
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_apply_non_callable_error(Value* func, Interp* interp) {
    char[256] ebuf;
    char[] type_name = interp.symbols.get_name(value_type_name(func, interp));
    if ((uint)interp.last_call_name != 0) {
        char[] msg = io::bprintf(&ebuf, "'%s' is %s, not a function",
            (ZString)interp.symbols.get_name(interp.last_call_name),
            (ZString)type_name)!!;
        return raise_error(interp, msg);
    }
    char[] msg = io::bprintf(&ebuf, "called value is %s, not a function",
        (ZString)type_name)!!;
    return raise_error(interp, msg);
}

fn Value* jit_apply_value_closure(Value* func, Value* arg, Interp* interp) {
    assert(func.closure_val != null, "jit_apply_value_closure: func.closure_val is null");
    Closure* closure = func.closure_val;
    if (!closure.has_param && !closure.has_rest) {
        return jit_eval_in_single_scope(closure.body, closure.env, interp);
    }
    if (closure.has_rest) {
        usz pc = closure.param_count;
        Env* new_env = closure.env;
        if (pc == 0) {
            Value* rest_list = make_cons(interp, arg, make_nil(interp));
            new_env = new_env.extend(interp, closure.rest_param, rest_list);
            return jit_eval_in_single_scope(closure.body, new_env, interp);
        }
        if (pc == 1) {
            Value*[1] typed_args = { arg };
            Value* type_err = check_lambda_call_boundary(closure, typed_args[:1], 1, interp);
            if (type_err != null) return type_err;
            new_env = new_env.extend(interp, closure.params[0], arg);
            new_env = new_env.extend(interp, closure.rest_param, make_nil(interp));
            return jit_eval_in_single_scope(closure.body, new_env, interp);
        }
        return raise_error(interp, "too few arguments for variadic lambda");
    }
    if (closure.param_count > 1) {
        char[128] buf;
        usz len = format_arity_error(&buf, closure.param_count, 1);
        return raise_error(interp, buf[:len]);
    }
    Value*[1] typed_args = { arg };
    Value* type_err = check_lambda_call_boundary(closure, typed_args[:1], 1, interp);
    if (type_err != null) return type_err;
    assert(closure.env != null, "jit_apply_value_closure: env is null");
    Env* new_env = closure.env.extend(interp, closure.param, arg);
    return jit_eval_in_single_scope(closure.body, new_env, interp);
}

fn Value* jit_apply_value_method_table(Value* func, Value* arg, Interp* interp) {
    Value*[1] args = { arg };
    MethodTable* mt = func.method_table_val;
    Value* resolved = find_best_method(mt, args[:1], interp);
    if (resolved != null && resolved.tag == ERROR) return resolved;
    if (resolved == null && mt.fallback != null) {
        resolved = mt.fallback;
    }
    if (resolved == null) {
        return format_dispatch_error(mt, args[:1], interp);
    }
    return jit_apply_value(resolved, arg, interp);
}

fn Value* jit_apply_value_impl(Value* func, Value* arg, Interp* interp) {
    if (func == null) {
        if ((uint)interp.last_call_name != 0) {
            char[256] ebuf;
            char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
                (ZString)interp.symbols.get_name(interp.last_call_name))!!;
            return raise_error(interp, msg);
        }
        return raise_error(interp, "called value is nil — not a function");
    }

    // Propagate errors from function evaluation (e.g. unbound variable)
    if (func.tag == ERROR) return func;

    switch (func.tag) {
        case CLOSURE:
            return jit_apply_value_closure(func, arg, interp);

        case PRIMITIVE: {
            // Set constructor type_id (needed for type constructor primitives)
            interp.constructor_type_id = func.prim_val.tag;
            EvalResult r = apply_primitive(func, arg, interp);
            return jit_apply_eval_result_or_error(r, interp);
        }

        case PARTIAL_PRIM: {
            EvalResult r = apply_partial(func, arg, interp);
            return jit_apply_eval_result_or_error(r, interp);
        }

        case CONTINUATION: {
            EvalResult r = jit_apply_continuation_impl(func, arg, interp);
            return jit_apply_eval_result_or_error(r, interp);
        }

        case METHOD_TABLE:
            return jit_apply_value_method_table(func, arg, interp);

        case SYMBOL: {
            // Symbol-as-function: ('name dict) => (ref dict 'name)
            // Call prim_ref directly — avoids recursion through jit_apply_value
            Value*[2] ref_args;
            ref_args[0] = arg;
            ref_args[1] = func;
            return prim_ref(ref_args[..], null, interp);
        }

        default:
            return jit_apply_non_callable_error(func, interp);
    }
}

<* @require func != null : "jit_apply_value called with null func" *>
fn Value* jit_apply_value(Value* func, Value* arg, Interp* interp) {
    Value* frame_error = interp_apply_frame_enter(interp);
    if (frame_error != null) return frame_error;
    Value* result = jit_apply_value_impl(func, arg, interp);
    interp_apply_frame_leave(interp);
    return result;
}

// Tail-call variant: sets TCO bounce fields for CLOSURE instead of recursing.
// ONLY called when the result goes directly to R0 as the function return value.
fn Value* jit_apply_value_tail(Value* func, Value* arg, Interp* interp) {
    if (func == null) {
        if ((uint)interp.last_call_name != 0) {
            char[256] ebuf;
            char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
                (ZString)interp.symbols.get_name(interp.last_call_name))!!;
            return raise_error(interp, msg);
        }
        return raise_error(interp, "called value is nil — not a function");
    }

    // Propagate errors from function evaluation (e.g. unbound variable)
    if (func.tag == ERROR) return func;

    if (func.tag == CLOSURE) {
        Env* new_env;
        if (!func.closure_val.has_param && !func.closure_val.has_rest) {
            new_env = func.closure_val.env;
        } else if (func.closure_val.has_rest) {
            usz pc = func.closure_val.param_count;
            if (pc == 0) {
                new_env = func.closure_val.env;
                Value* rest_list = make_cons(interp, arg, make_nil(interp));
                new_env = new_env.extend(interp, func.closure_val.rest_param, rest_list);
            } else if (pc == 1) {
                Value*[1] typed_args = { arg };
                Value* type_err = check_lambda_call_boundary(func.closure_val, typed_args[:1], 1, interp);
                if (type_err != null) return type_err;
                new_env = func.closure_val.env;
                new_env = new_env.extend(interp, func.closure_val.params[0], arg);
                new_env = new_env.extend(interp, func.closure_val.rest_param, make_nil(interp));
            } else {
                return raise_error(interp, "too few arguments for variadic lambda");
            }
        } else if (func.closure_val.param_count > 1) {
            // Arity check: non-variadic multi-param closure called with 1 arg
            char[128] buf;
            usz len = format_arity_error(&buf, func.closure_val.param_count, 1);
            return raise_error(interp, buf[:len]);
        } else {
            Value*[1] typed_args = { arg };
            Value* type_err = check_lambda_call_boundary(func.closure_val, typed_args[:1], 1, interp);
            if (type_err != null) return type_err;
            new_env = func.closure_val.env.extend(interp, func.closure_val.param, arg);
        }
        interp.jit_tco_expr = func.closure_val.body;
        interp.jit_tco_env = new_env;
        interp.flags.jit_tco_bounce = true;
        return jit_tco_sentinel();
    }

    // Non-closure: normal apply (no TCO needed for primitives/continuations)
    return jit_apply_value(func, arg, interp);
}

fn Value* jit_eval_fallback(Expr* expr, Env* env, Interp* interp) {
    return jit_eval(expr, env, interp);
}

// Adapter: converts jit_eval's Value* return to EvalResult.
// Used to mechanically replace eval() calls in EvalResult-returning functions.
fn EvalResult jit_eval_to_result(Expr* expr, Env* env, Interp* interp) {
    Value* v = jit_eval(expr, env, interp);
    if (v != null && v.tag == ERROR) {
        return eval_error_expr(v.str_chars[:v.str_len], expr);
    }
    return eval_ok(v);
}

// Variable lookup that checks jit_env first (for sub-expression evaluation with local bindings)
fn Value* jit_lookup_var(Interp* interp, uint sym_id) {
    SymbolId name = (SymbolId)sym_id;
    // Check JIT env first (set during jit_eval sub-expression evaluation)
    if (interp.jit_env != null) {
        Value* v = interp.jit_env.lookup(name);
        if (v != null) return v;
    }
    // Fall back to global
    Value* v = interp.global_env.lookup(name);
    if (v != null) return v;
    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "unbound variable '%s'",
        (ZString)interp.symbols.get_name(name))!!;
    return raise_error(interp, msg);
}

// TCO sentinel — returned by helpers to signal "bounce, don't recurse"
Value g_jit_tco_sentinel;
