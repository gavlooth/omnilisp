module lisp;

fn Value* jit_raise(Interp* interp, String code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", code, (String)message, null);
}


import std::core::mem;
import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (apply/eval core)
// =============================================================================
struct JitEscapeAllocationStats {
    usz nil_calls;
    usz cons_calls;
    usz array_calls;
    usz string_calls;
}

JitEscapeAllocationStats g_jit_escape_allocation_stats;

fn void jit_escape_allocation_stats_reset() {
    g_jit_escape_allocation_stats = {};
}

fn JitEscapeAllocationStats jit_escape_allocation_stats_snapshot() @inline {
    return g_jit_escape_allocation_stats;
}

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

fn Value* jit_make_nil_escape(Interp* interp) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* v = boundary_alloc_value_in_current_escape(interp);
    if (v == null) return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape nil");
    v.tag = NIL;
    g_jit_escape_allocation_stats.nil_calls++;
    return v;
}

fn Value* jit_cons_escape(Interp* interp, Value* car, Value* cdr) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* promoted_car =
        car != null && car.scope_gen == interp.current_scope.escape_generation
        ? car
        : boundary_promote_to_escape(car, interp);
    if (car != null && promoted_car == null) return promoted_car;
    if (car != null && promoted_car.tag == ERROR && !make_cons_promoted_error_value_ok(interp, car, promoted_car)) return promoted_car;

    Value* promoted_cdr =
        cdr != null && cdr.scope_gen == interp.current_scope.escape_generation
        ? cdr
        : boundary_promote_to_escape(cdr, interp);
    if (cdr != null && (promoted_cdr == null || promoted_cdr.tag == ERROR)) {
        boundary_cleanup_materialized_value(car, promoted_car, interp.current_scope, true);
        return promoted_cdr;
    }

    Value* v = boundary_alloc_value_in_current_escape(interp);
    if (v == null) {
        boundary_cleanup_materialized_value(car, promoted_car, interp.current_scope, true);
        boundary_cleanup_materialized_value(cdr, promoted_cdr, interp.current_scope, true);
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape cons");
    }
    v.tag = CONS;
    v.cons_val.car = promoted_car;
    v.cons_val.cdr = promoted_cdr;
    g_jit_escape_allocation_stats.cons_calls++;
    return v;
}

fn Value* jit_make_array_escape(Interp* interp, usz capacity) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Array* arr = (Array*)mem::malloc(Array.sizeof);
    if (arr == null) return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape array");
    arr.capacity = capacity < 4 ? 4 : capacity;
    arr.length = 0;
    if (arr.capacity > usz.max / Value*.sizeof) {
        mem::free(arr);
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape array");
    }
    arr.items = (Value**)mem::malloc(Value*.sizeof * arr.capacity);
    if (arr.items == null) {
        mem::free(arr);
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape array");
    }

    Value* v = boundary_alloc_value_in_current_escape(interp, true);
    if (v == null) {
        mem::free(arr.items);
        mem::free(arr);
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape array");
    }
    v.tag = ARRAY;
    v.array_val = arr;
    g_jit_escape_allocation_stats.array_calls++;
    return v;
}

fn Value* jit_make_string_escape(Interp* interp, char* chars, usz len) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    if (chars == null) return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape string");
    if (len == usz.max) return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape string");
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    char* owned = (char*)mem::malloc(cap);
    if (owned == null) return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape string");
    for (usz i = 0; i < len; i++) {
        owned[i] = chars[i];
    }
    owned[len] = 0;

    Value* v = boundary_alloc_value_in_current_escape(interp, true);
    if (v == null) {
        mem::free(owned);
        return jit_raise(interp, "runtime/out-of-memory", "jit: failed to allocate escape string");
    }
    v.tag = STRING;
    v.str_chars = owned;
    v.str_len = len;
    v.string_heap_owned = true;
    g_jit_escape_allocation_stats.string_calls++;
    return v;
}

fn Value* jit_make_string_literal_escape(Interp* interp, Value* literal) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    if (literal == null || literal.tag != STRING || literal.str_chars == null) {
        return jit_raise(interp, "type/arg-mismatch", "jit: expected string literal for escape allocation");
    }
    return jit_make_string_escape(interp, literal.str_chars, literal.str_len);
}

fn Value* jit_array_escape_set(Interp* interp, Value* array, long index, Value* value) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    if (array == null) return jit_raise(interp, "type/arg-mismatch", "jit: missing escape array");
    if (array.tag == ERROR) return array;
    if (array.tag != ARRAY || array.array_val == null) {
        return jit_raise(interp, "type/arg-mismatch", "jit: invalid escape array");
    }
    if (index < 0 || (usz)index >= array.array_val.capacity) {
        return jit_raise(interp, "runtime/evaluation-error", "jit: escape array index out of bounds");
    }

    Value* promoted =
        value != null && value.scope_gen == interp.current_scope.escape_generation
        ? value
        : boundary_promote_to_escape(value, interp);
    if (value != null && (promoted == null || promoted.tag == ERROR)) return promoted;
    array.array_val.items[(usz)index] = promoted;
    if ((usz)index >= array.array_val.length) array.array_val.length = (usz)index + 1;
    stable_graph_note_mutation_ptr((void*)array.array_val);
    return array;
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
        return jit_raise(interp, "runtime/evaluation-error", msg);
    }
    return jit_raise(interp, "runtime/evaluation-error", "unbound variable");
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
            return jit_raise(interp, "runtime/evaluation-error", msg);
        }
        return jit_raise(interp, "type/arg-mismatch", "called value is not a function");
    }
    if (try char[] msg = io::bprintf(&ebuf, "called value is %s, not a function",
        (ZString)type_name)) {
        return jit_raise(interp, "runtime/evaluation-error", msg);
    }
    return jit_raise(interp, "type/arg-mismatch", "called value is not a function");
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
            if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
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
            if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
            new_env = jit_env_extend(interp, new_env, closure.rest_param, make_nil(interp));
            if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
            Value* result = jit_eval_in_single_scope(closure.body, new_env, interp);
            if (zeroarg_closure_trace_enabled()) {
                io::printfn("OMNI_CLOSURE_TRACE exit param_count=%d body_tag=%d", (int)closure.param_count, (int)closure.body.tag);
            }
            return result;
        }
        return jit_raise(interp, "runtime/evaluation-error", "too few arguments for variadic lambda");
    }
    if (closure.param_count > 1) {
        char[128] buf;
        usz len = format_arity_error(&buf, closure.param_count, 1);
        return raise_error_with_payload_names(interp, "type", "type/arity", (String)buf[:len], null);
    }
    Value*[1] typed_args = { arg };
    Value* type_err = check_lambda_call_boundary(closure, typed_args[:1], 1, interp);
    if (type_err != null) return type_err;
    assert(closure.env != null, "jit_apply_value_closure: env is null");
    Env* new_env = jit_env_extend(interp, closure.env, closure.param, arg);
    if (new_env == null) return jit_raise(interp, "runtime/out-of-memory", "out of memory while binding argument");
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
        return jit_raise(interp, "type/arg-mismatch", "invalid primitive application state");
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
