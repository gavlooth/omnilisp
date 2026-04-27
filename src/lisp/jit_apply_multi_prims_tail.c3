module lisp;


import std::io;

tlocal long g_jit_tail_multi_trace_counter = 0;

fn bool jit_tail_multi_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_TAIL_MULTI") != null;
}

fn int jit_tail_multi_env_depth(Env* env, Env* stop_at) {
    int depth = 0;
    Env* cur = env;
    while (cur != null && cur != stop_at && depth < 32) {
        depth++;
        cur = cur.parent;
    }
    return depth;
}

fn Value* jit_set_tco_bounce(Interp* interp, Expr* expr, Env* env) @inline {
    interp.jit_tco_expr = expr;
    interp.jit_tco_env = env;
    interp.flags.jit_tco_bounce = true;
    return jit_tco_sentinel();
}

fn Value* jit_apply_multi_args_tail_variadic(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    if (!jit_value_has_closure_payload_and_body(func)) {
        return jit_invalid_closure_state_error(interp);
    }
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        char[256] ebuf;
        char[] msg;
        if (try char[] formatted = io::bprintf(&ebuf, "variadic lambda requires at least %d argument(s), got %d", (int)pc, (int)arg_count)) {
            msg = formatted;
        } else {
            msg = "variadic lambda arity mismatch";
        }
        return jit_raise(interp, "runtime/evaluation-error", msg);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* new_env = func.closure_val.env;
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return jit_raise(interp, "runtime/evaluation-error", "arg list too short");
        new_env = jit_env_extend(interp, new_env, func.closure_val.params[i], curr.cons_val.car);
        if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
        curr = curr.cons_val.cdr;
    }
    new_env = jit_env_extend(interp, new_env, func.closure_val.rest_param, curr);
    if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
    return jit_set_tco_bounce(interp, func.closure_val.body, new_env);
}

fn Value* jit_apply_multi_args_tail_closure_multi(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    if (!jit_value_has_closure_payload_and_body(func)) {
        return jit_invalid_closure_state_error(interp);
    }
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        char[128] buf;
        usz len = format_arity_error(&buf, pc, arg_count);
        return raise_error_with_payload_names(interp, "type", "type/arity", (String)buf[:len], null);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* call_env = make_env(interp, func.closure_val.env);
    if (call_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return jit_raise(interp, "runtime/evaluation-error", "arg list too short");
        Value* bind_err = env_define_with_barrier_result(call_env, func.closure_val.params[i], curr.cons_val.car, interp);
        if (bind_err != null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
        curr = curr.cons_val.cdr;
    }
    if (jit_tail_multi_trace_enabled() && arg_count == pc && pc == 2) {
        g_jit_tail_multi_trace_counter++;
        if (g_jit_tail_multi_trace_counter <= 8 || (g_jit_tail_multi_trace_counter % 100) == 0) {
            Value* arg0 = call_env.binding_count > 0 ? call_env.bindings[0].value : null;
            Value* arg1 = call_env.binding_count > 1 ? call_env.bindings[1].value : null;
            io::printfn(
                "OMNI_TAIL_MULTI_TRACE count=%d func_env_depth=%d call_env_depth=%d call_scope_gen=%d func_env_scope_gen=%d arg0_tag=%d arg0_in_current=%d arg0_in_root=%d arg1_tag=%d arg1_in_current=%d arg1_in_root=%d",
                (int)g_jit_tail_multi_trace_counter,
                jit_tail_multi_env_depth(func.closure_val.env, interp.global_env),
                jit_tail_multi_env_depth(call_env, interp.global_env),
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                func.closure_val.env_scope != null ? (int)func.closure_val.env_scope.generation : -1,
                arg0 != null ? (int)arg0.tag : -1,
                arg0 != null && boundary_ptr_in_scope((void*)arg0, interp.current_scope) ? 1 : 0,
                arg0 != null && boundary_ptr_in_scope((void*)arg0, interp.root_scope) ? 1 : 0,
                arg1 != null ? (int)arg1.tag : -1,
                arg1 != null && boundary_ptr_in_scope((void*)arg1, interp.current_scope) ? 1 : 0,
                arg1 != null && boundary_ptr_in_scope((void*)arg1, interp.root_scope) ? 1 : 0
            );
        }
    }
    if (arg_count > pc) {
        Value* result = jit_eval_in_single_scope(func.closure_val.body, call_env, interp);
        result = jit_apply_normalize_result(result, interp);
        if (result != null && result.tag != ERROR) {
            usz remaining = arg_count - pc;
            return jit_apply_multi_args_tail(interp, result, curr, remaining);
        }
        return result;
    }
    return jit_set_tco_bounce(interp, func.closure_val.body, call_env);
}

fn Value* jit_apply_multi_args_tail_method_table(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    if (!jit_value_has_method_table_payload(func)) {
        return jit_raise(interp, "runtime/evaluation-error", "jit: invalid method table state");
    }
    Value** args = jit_alloc_multi_arg_buffer(interp, arg_count);
    if (arg_count > 0 && args == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while preparing call arguments");
    Value* curr = arg_list;
    usz actual_count = 0;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != CONS) break;
        Value* arg = curr.cons_val.car;
        args[actual_count++] = arg;
        curr = curr.cons_val.cdr;
    }
    if (actual_count < arg_count) return jit_raise(interp, "runtime/evaluation-error", "arg list too short");
    MethodTable* mt = func.method_table_val;
    Value* resolved = find_best_method(mt, args[:actual_count], interp);
    if (resolved != null && resolved.tag == ERROR) return resolved;
    if (resolved == null && mt.fallback != null) resolved = mt.fallback;
    if (resolved == null) return format_dispatch_error(mt, args[:actual_count], interp);
    return jit_apply_multi_args_tail(interp, resolved, arg_list, arg_count);
}

fn Value* jit_apply_multi_args_tail(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value* frame_error = interp_apply_frame_enter(interp);
    if (frame_error != null) return frame_error;
    defer interp_apply_frame_leave(interp);

    if (func == null) return jit_null_callable_error(interp);
    if (func.tag == ERROR) return func;
    if (func.tag == CLOSURE && !jit_value_has_closure_payload_and_body(func)) {
        return jit_invalid_closure_state_error(interp);
    }
    if (arg_count == 0 && func.tag == CLOSURE && !func.closure_val.has_param && !func.closure_val.has_rest) {
        return jit_set_tco_bounce(interp, func.closure_val.body, func.closure_val.env);
    }
    if (func.tag == CLOSURE && func.closure_val.has_rest) {
        return jit_apply_multi_args_tail_variadic(interp, func, arg_list, arg_count);
    }
    if (func.tag == CLOSURE) {
        usz pc = func.closure_val.param_count;
        if (pc > 1) {
            return jit_apply_multi_args_tail_closure_multi(interp, func, arg_list, arg_count);
        }
    }
    if (func.tag == METHOD_TABLE) {
        return jit_apply_multi_args_tail_method_table(interp, func, arg_list, arg_count);
    }
    return jit_apply_multi_args(interp, func, arg_list, arg_count);
}

fn Value* jit_env_lookup_local(Interp* interp, uint sym_id, Env* env) {
    if (env == null) return jit_raise(interp, "type/arg-mismatch", "jit: missing mutable local binding");
    SymbolId name = (SymbolId)sym_id;
    Value* v = env.lookup(name);
    if (v != null) return v;
    return jit_raise(interp, "type/arg-mismatch", "jit: missing mutable local binding");
}

fn Env* jit_env_reparent(Env* env, Env* new_parent) {
    if (env == null) return new_parent;
    env.parent = new_parent;
    return env;
}

fn Value* jit_prim_add(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) return numeric_add2(interp, a, b);
    return jit_raise(interp, "type/arg-mismatch", "+: expected numbers");
}

fn Value* jit_prim_sub(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) return numeric_sub2(interp, a, b);
    return jit_raise(interp, "type/arg-mismatch", "-: expected numbers");
}

fn Value* jit_prim_mul(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) return numeric_mul2(interp, a, b);
    return jit_raise(interp, "type/arg-mismatch", "*: expected numbers");
}

fn Value* jit_prim_lt(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null && is_numeric_value(a) && is_numeric_value(b)) {
        int cmp = 0;
        if (is_big_complex(a) || is_big_complex(b)) return jit_raise(interp, "runtime/evaluation-error", "<: complex numbers are not ordered");
        if (!numeric_values_compare(a, b, &cmp)) return jit_raise(interp, "runtime/evaluation-error", "<: BigInteger value is out of Float64 range");
        return numeric_compare_bool_result(interp, cmp, NUMERIC_COMPARE_LT);
    }
    return jit_raise(interp, "type/arg-mismatch", "<: expected numbers");
}

fn Value* jit_prim_gt(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null && is_numeric_value(a) && is_numeric_value(b)) {
        int cmp = 0;
        if (is_big_complex(a) || is_big_complex(b)) return jit_raise(interp, "runtime/evaluation-error", ">: complex numbers are not ordered");
        if (!numeric_values_compare(a, b, &cmp)) return jit_raise(interp, "runtime/evaluation-error", ">: BigInteger value is out of Float64 range");
        return numeric_compare_bool_result(interp, cmp, NUMERIC_COMPARE_GT);
    }
    return jit_raise(interp, "type/arg-mismatch", ">: expected numbers");
}

fn Value* jit_prim_eq(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) {
        bool eq = values_equal(a, b);
        return eq ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    }
    return make_nil(interp);
}
