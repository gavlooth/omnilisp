module lisp;


import std::io;
import std::collections::list;
import main;

fn bool jit_pointer_looks_canonical(void* ptr) @inline {
    ulong raw = (ulong)ptr;
    if (raw < 0x10000) return false;
    if ((raw & 0x7) != 0) return false;
    ulong top = raw >> 48;
    return top == 0 || top == 0xFFFF;
}

fn Value* jit_stabilize_staged_value(Value* value, Interp* interp) @inline {
    if (value == null || interp == null || interp.current_scope == null) return value;
    if (value.tag == ERROR) return value;
    if (value.scope_gen == interp.current_scope.escape_generation) return value;
    if (value.scope_gen == interp.current_scope.generation) {
        return boundary_promote_to_escape(value, interp);
    }
    return value;
}

fn Value* jit_eval_call_cont_safe_common(Interp* interp, Expr* expr, Env* env, bool is_tail) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    if (expr == null || expr.tag != E_CALL) {
        return jit_raise(interp, "runtime/evaluation-error", "jit: continuation-safe call helper expects call expression");
    }
    if (boundary_c_getenv("OMNI_TRACE_CONT_SAFE_CALL") != null) {
        io::printfn(
            "cont-safe-call: is_tail=%d argc=%d func_tag=%d",
            is_tail ? 1 : 0,
            (int)expr.call.arg_count,
            expr.call.func != null ? (int)expr.call.func.tag : -1
        );
    }

    Value* func = jit_eval(expr.call.func, env, interp);
    if (func != null && func.tag == ERROR) return func;
    func = jit_stabilize_staged_value(func, interp);
    if (func != null && func.tag == ERROR) return func;

    usz argc = expr.call.arg_count;
    if (argc == 0) {
        Value* nil_args = make_nil(interp);
        if (is_tail) {
            return jit_apply_multi_args_tail(interp, func, nil_args, 0);
        }
        return jit_apply_multi_args(interp, func, nil_args, 0);
    }

    if (argc == 1) {
        Value* arg = jit_eval(expr.call.args[0], env, interp);
        if (arg != null && arg.tag == ERROR) return arg;
        if (is_tail) {
            return jit_apply_value_tail(func, arg, interp);
        }
        return jit_apply_value(func, arg, interp);
    }

    if (argc > usz.max / Value*.sizeof) {
        return jit_raise(interp, "runtime/out-of-memory", "jit: out of memory while staging continuation-sensitive call args");
    }

    Value** argv = interp.current_scope != null
        ? (Value**)interp.current_scope.alloc_escape(Value*.sizeof * argc)
        : null;
    if (argv == null) return jit_raise(interp, "runtime/out-of-memory", "jit: out of memory while staging continuation-sensitive call args");
    for (usz i = 0; i < argc; i++) {
        Value* arg = jit_eval(expr.call.args[i], env, interp);
        if (arg != null && arg.tag == ERROR) return arg;
        arg = jit_stabilize_staged_value(arg, interp);
        if (arg != null && arg.tag == ERROR) return arg;
        argv[i] = arg;
    }

    Value* arg_list = is_tail
        ? jit_make_nil_escape(interp)
        : boundary_promote_to_escape(make_nil(interp), interp);
    if (arg_list != null && arg_list.tag == ERROR) return arg_list;
    for (usz i = argc; i > 0; i--) {
        Value* next = is_tail
            ? jit_cons_escape(interp, argv[i - 1], arg_list)
            : make_cons(interp, argv[i - 1], arg_list);
        if (next == null) {
            return jit_raise(interp, "runtime/evaluation-error", "jit: failed to construct call arg list");
        }
        if (next.tag == ERROR) return next;
        arg_list = next;
    }
    if (is_tail) {
        return jit_apply_multi_args_tail(interp, func, arg_list, argc);
    }
    return jit_apply_multi_args(interp, func, arg_list, argc);
}

fn Value* jit_eval_call_cont_safe(Interp* interp, Expr* expr, Env* env) {
    return jit_eval_call_cont_safe_common(interp, expr, env, false);
}

fn Value* jit_eval_call_cont_safe_tail(Interp* interp, Expr* expr, Env* env) {
    return jit_eval_call_cont_safe_common(interp, expr, env, true);
}

// General staged-call helper for 2+ arg call sites. This shares the same
// runtime argument staging path as the continuation-safe helper and avoids
// JIT-side cons-list construction across helper calls.
fn Value* jit_eval_call_staged(Interp* interp, Expr* expr, Env* env) {
    return jit_eval_call_cont_safe_common(interp, expr, env, false);
}

fn Value* jit_eval_call_staged_tail(Interp* interp, Expr* expr, Env* env) {
    return jit_eval_call_cont_safe_common(interp, expr, env, true);
}

fn Value* jit_apply_value_impl(Value* func, Value* arg, Interp* interp) {
    if (func != null && !jit_pointer_looks_canonical((void*)func)) {
        return jit_raise(interp, "type/arg-mismatch", "jit: invalid callable pointer");
    }
    if (func == null) {
        if ((uint)interp.last_call_name != 0) {
            char[256] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
                (ZString)interp.symbols.get_name(interp.last_call_name))) {
                return jit_raise(interp, "runtime/evaluation-error", msg);
            }
            return jit_raise(interp, "runtime/evaluation-error", "called value is not defined");
        }
        return jit_raise(interp, "type/arg-mismatch", "called value is nil — not a function");
    }

    // Propagate errors from function evaluation (e.g. unbound variable)
    if (func.tag == ERROR) return func;

    switch (func.tag) {
        case CLOSURE:
            return jit_apply_value_closure(func, arg, interp);

        case PRIMITIVE: {
            return jit_apply_value_primitive(func, arg, interp);
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

        default:
            return jit_apply_non_callable_error(func, interp);
    }
}

fn Value* jit_apply_value(Value* func, Value* arg, Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* frame_error = interp_apply_frame_enter(interp);
    if (frame_error != null) return frame_error;
    Value* result = jit_apply_value_impl(func, arg, interp);
    interp_apply_frame_leave(interp);
    return result;
}

// Tail-call variant: sets TCO bounce fields for CLOSURE instead of recursing.
// ONLY called when the result goes directly to R0 as the function return value.
fn Value* jit_apply_value_tail(Value* func, Value* arg, Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    if (func == null) {
        if ((uint)interp.last_call_name != 0) {
            char[256] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
                (ZString)interp.symbols.get_name(interp.last_call_name))) {
                return jit_raise(interp, "runtime/evaluation-error", msg);
            }
            return jit_raise(interp, "runtime/evaluation-error", "called value is not defined");
        }
        return jit_raise(interp, "type/arg-mismatch", "called value is nil — not a function");
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
                Value* rest_list = make_list1_or_error(interp, arg, "out of memory while binding variadic rest arguments");
                if (rest_list == null || rest_list.tag == ERROR) return rest_list;
                new_env = jit_env_extend(interp, new_env, func.closure_val.rest_param, rest_list);
                if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
            } else if (pc == 1) {
                Value*[1] typed_args = { arg };
                Value* type_err = check_lambda_call_boundary(func.closure_val, typed_args[:1], 1, interp);
                if (type_err != null) return type_err;
                new_env = func.closure_val.env;
                new_env = jit_env_extend(interp, new_env, func.closure_val.params[0], arg);
                if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
                new_env = jit_env_extend(interp, new_env, func.closure_val.rest_param, make_nil(interp));
                if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
            } else {
                return jit_raise(interp, "runtime/evaluation-error", "too few arguments for variadic lambda");
            }
        } else if (func.closure_val.param_count > 1) {
            // Arity check: non-variadic multi-param closure called with 1 arg
            char[128] buf;
            usz len = format_arity_error(&buf, func.closure_val.param_count, 1);
            return raise_error_with_payload_names(interp, "type", "type/arity", (String)buf[:len], null);
        } else {
            Value*[1] typed_args = { arg };
            Value* type_err = check_lambda_call_boundary(func.closure_val, typed_args[:1], 1, interp);
            if (type_err != null) return type_err;
            new_env = jit_env_extend(interp, func.closure_val.env, func.closure_val.param, arg);
            if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
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
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return jit_eval(expr, env, interp);
}

// Adapter: converts jit_eval's Value* return to EvalResult.
// Used to mechanically replace eval() calls in EvalResult-returning functions.
fn EvalResult jit_eval_to_result(Expr* expr, Env* env, Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return eval_ok(null);
    Value* v = jit_eval(expr, env, interp);
    if (v != null && v.tag == ERROR) {
        return eval_error_expr(v.str_chars[:v.str_len], expr);
    }
    return eval_ok(v);
}

// Variable lookup that checks jit_env first (for sub-expression evaluation with local bindings)
fn Value* jit_lookup_var(Interp* interp, uint sym_id) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    SymbolId name = (SymbolId)sym_id;
    // Check JIT env first (set during jit_eval sub-expression evaluation)
    if (interp.jit_env != null) {
        Value* v = interp.jit_env.lookup(name);
        if (v != null && jit_pointer_looks_canonical((void*)v)) return v;
    }
    // Fall back to global
    Value* v = interp.global_env.lookup(name);
    if (v != null) return v;
    char[256] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "unbound variable '%s'",
        (ZString)interp.symbols.get_name(name))) {
        return jit_raise(interp, "runtime/evaluation-error", msg);
    }
    return jit_raise(interp, "runtime/evaluation-error", "unbound variable");
}

// TCO sentinel — returned by helpers to signal "bounce, don't recurse"
Value g_jit_tco_sentinel;
