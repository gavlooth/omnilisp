module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (apply/eval core)
// =============================================================================
fn bool zeroarg_closure_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_ZEROARG_CLOSURE") != null;
}

fn Value* jit_make_int(Interp* interp, long n) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return make_int(interp, n);
}

fn Value* jit_make_nil(Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return make_nil(interp);
}

fn Value* jit_make_void(Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return make_void(interp);
}

fn Value* jit_lookup_global(Interp* interp, uint sym_id) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    SymbolId name = (SymbolId)sym_id;
    Value* v = interp.global_env.lookup(name);
    if (v != null) return v;
    char[256] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "unbound variable '%s'",
        (ZString)interp.symbols.get_name(name))) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "unbound variable");
}

fn usz format_arity_error(char[] buf, usz expected, usz got) {
    char[] prefix = "arity mismatch: expected ";
    usz pos = 0;
    for (usz i = 0; i < prefix.len && pos < buf.len - 1; i++) buf[pos++] = prefix[i];
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
        if (try char[] msg = io::bprintf(&ebuf, "'%s' is %s, not a function",
            (ZString)interp.symbols.get_name(interp.last_call_name),
            (ZString)type_name)) {
            return raise_error(interp, msg);
        }
        return raise_error(interp, "called value is not a function");
    }
    if (try char[] msg = io::bprintf(&ebuf, "called value is %s, not a function",
        (ZString)type_name)) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "called value is not a function");
}

fn Value* jit_apply_value_closure(Value* func, Value* arg, Interp* interp) {
    assert(func.closure_val != null, "jit_apply_value_closure: func.closure_val is null");
    Closure* closure = func.closure_val;
    if (zeroarg_closure_trace_enabled()) {
        io::printfn(
            "OMNI_CLOSURE_TRACE enter param_count=%d has_rest=%d body_tag=%d env_scope=%d env_is_global=%d",
            (int)closure.param_count,
            closure.has_rest ? 1 : 0,
            (int)closure.body.tag,
            closure.env_scope != null ? 1 : 0,
            closure.env == interp.global_env ? 1 : 0
        );
    }
    if (!closure.has_param && !closure.has_rest) {
        if (zeroarg_closure_trace_enabled()) {
            io::printfn(
                "OMNI_ZEROARG_TRACE enter body_tag=%d env_scope=%d env_is_global=%d",
                (int)closure.body.tag,
                closure.env_scope != null ? 1 : 0,
                closure.env == interp.global_env ? 1 : 0
            );
        }
        Value* result = jit_eval_in_single_scope(closure.body, closure.env, interp);
        if (zeroarg_closure_trace_enabled()) {
            io::printfn("OMNI_ZEROARG_TRACE exit body_tag=%d", (int)closure.body.tag);
            io::printfn("OMNI_CLOSURE_TRACE exit param_count=%d body_tag=%d", (int)closure.param_count, (int)closure.body.tag);
        }
        return result;
    }
    if (closure.has_rest) {
        usz pc = closure.param_count;
        Env* new_env = closure.env;
        if (pc == 0) {
            Value* rest_list = make_list1_or_error(interp, arg, "out of memory while binding variadic rest arguments");
            if (rest_list == null || rest_list.tag == ERROR) return rest_list;
            new_env = jit_env_extend(interp, new_env, closure.rest_param, rest_list);
            if (new_env == null) return raise_error(interp, "out of memory while binding argument");
            Value* result = jit_eval_in_single_scope(closure.body, new_env, interp);
            if (zeroarg_closure_trace_enabled()) {
                io::printfn("OMNI_CLOSURE_TRACE exit param_count=%d body_tag=%d", (int)closure.param_count, (int)closure.body.tag);
            }
            return result;
        }
        if (pc == 1) {
            Value*[1] typed_args = { arg };
            Value* type_err = check_lambda_call_boundary(closure, typed_args[:1], 1, interp);
            if (type_err != null) return type_err;
            new_env = jit_env_extend(interp, new_env, closure.params[0], arg);
            if (new_env == null) return raise_error(interp, "out of memory while binding argument");
            new_env = jit_env_extend(interp, new_env, closure.rest_param, make_nil(interp));
            if (new_env == null) return raise_error(interp, "out of memory while binding argument");
            Value* result = jit_eval_in_single_scope(closure.body, new_env, interp);
            if (zeroarg_closure_trace_enabled()) {
                io::printfn("OMNI_CLOSURE_TRACE exit param_count=%d body_tag=%d", (int)closure.param_count, (int)closure.body.tag);
            }
            return result;
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
    Env* new_env = jit_env_extend(interp, closure.env, closure.param, arg);
    if (new_env == null) return raise_error(interp, "out of memory while binding argument");
    Value* result = jit_eval_in_single_scope(closure.body, new_env, interp);
    if (zeroarg_closure_trace_enabled()) {
        io::printfn("OMNI_CLOSURE_TRACE exit param_count=%d body_tag=%d", (int)closure.param_count, (int)closure.body.tag);
    }
    return result;
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

fn Value* jit_apply_value_primitive(Value* func, Value* arg, Interp* interp) {
    if (func == null || func.tag != PRIMITIVE || func.prim_val == null || func.prim_val.func == null) {
        return raise_error(interp, "invalid primitive application state");
    }
    int arity = func.prim_val.arity;
    // IMPORTANT: keep primitive apply on Value* return path.
    // Avoid EvalResult struct-return here because this call site can cross
    // suspend/resume cloned stack boundaries (AOT closure primitive path).
    if (arity == 1 || arity == -1) {
        Value*[8] args;
        args[0] = arg;
        interp.constructor_type_id = func.prim_val.tag;
        interp.prim_user_data = func.prim_val.user_data;
        Value* result = func.prim_val.func(args[:1], null, interp);
        if (result != null && result.tag == ERROR) {
            return make_error(interp, result.str_chars[:result.str_len]);
        }
        return result;
    }

    char[128] buf;
    usz len = format_arity_error(&buf, (usz)arity, 1);
    return make_error(interp, buf[:len]);
}
