module lisp;

import std::core::mem;
import std::io;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (apply-multi/tail + primitive shims)
// =============================================================================
const usz JIT_TYPED_PREFIX_INLINE_ARG_CAPACITY = 8;
bool g_jit_force_multi_arg_buffer_alloc_fail = false;

fn Value** jit_alloc_multi_arg_buffer(Interp* interp, usz arg_count) @inline {
    if (arg_count == 0) return null;
    if (g_jit_force_multi_arg_buffer_alloc_fail) return null;
    return (Value**)interp.current_scope.alloc(Value*.sizeof * arg_count);
}

fn Value* jit_cons(Interp* interp, Value* car, Value* cdr) {
    return make_cons(interp, car, cdr);
}

fn Value* jit_null_callable_error(Interp* interp) {
    if ((uint)interp.last_call_name != 0) {
        char[256] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
            (ZString)interp.symbols.get_name(interp.last_call_name))) {
            return raise_error(interp, msg);
        }
        return raise_error(interp, "called value is not defined");
    }
    return raise_error(interp, "called value is nil — not a function");
}

fn Value* jit_apply_multi_args_zeroarg_closure(Interp* interp, Value* func) {
    if (zeroarg_closure_trace_enabled()) {
        io::printfn(
            "OMNI_MULTI_TRACE zeroarg-enter body_tag=%d env_scope=%d env_is_global=%d",
            (int)func.closure_val.body.tag,
            func.closure_val.env_scope != null ? 1 : 0,
            func.closure_val.env == interp.global_env ? 1 : 0
        );
    }
    Value* result = jit_eval_in_single_scope(func.closure_val.body, func.closure_val.env, interp);
    if (zeroarg_closure_trace_enabled()) {
        io::printfn("OMNI_MULTI_TRACE zeroarg-exit body_tag=%d", (int)func.closure_val.body.tag);
    }
    return result;
}

fn Value* jit_check_closure_typed_prefix_args(Interp* interp, Closure* closure, Value* arg_list, usz arg_count) {
    if (closure == null || !closure.has_typed_params || closure.type_sig == null) return null;
    usz checked_count = closure.type_sig.param_count;
    if (checked_count == 0 || arg_count < checked_count) return null;

    Value*[] checked_args = {};
    Value*[JIT_TYPED_PREFIX_INLINE_ARG_CAPACITY] checked_inline;
    Value** checked_storage = null;
    if (checked_count <= JIT_TYPED_PREFIX_INLINE_ARG_CAPACITY) {
        checked_args = checked_inline[:checked_count];
    } else {
        checked_storage = (Value**)mem::malloc(Value*.sizeof * checked_count);
        if (checked_storage == null) {
            return raise_error(interp, "lambda type check: out of memory");
        }
        checked_args = checked_storage[:checked_count];
    }
    defer if (checked_storage != null) mem::free(checked_storage);

    Value* curr = arg_list;
    for (usz i = 0; i < checked_count; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        checked_args[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }

    return check_lambda_call_boundary(closure, checked_args, arg_count, interp);
}

fn Value* jit_apply_multi_args_variadic_closure(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    if (zeroarg_closure_trace_enabled()) {
        io::printfn(
            "OMNI_MULTI_TRACE variadic-enter param_count=%d arg_count=%d env_scope=%d env_is_global=%d",
            (int)func.closure_val.param_count,
            (int)arg_count,
            func.closure_val.env_scope != null ? 1 : 0,
            func.closure_val.env == interp.global_env ? 1 : 0
        );
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
        return raise_error(interp, msg);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* new_env = func.closure_val.env;
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        new_env = jit_env_extend(interp, new_env, func.closure_val.params[i], curr.cons_val.car);
        if (new_env == null) return raise_error(interp, "out of memory while binding argument");
        curr = curr.cons_val.cdr;
    }
    new_env = jit_env_extend(interp, new_env, func.closure_val.rest_param, curr);
    if (new_env == null) return raise_error(interp, "out of memory while binding argument");
    Value* result = jit_eval_in_single_scope(func.closure_val.body, new_env, interp);
    if (zeroarg_closure_trace_enabled()) {
        io::printfn("OMNI_MULTI_TRACE variadic-exit body_tag=%d", (int)func.closure_val.body.tag);
    }
    return result;
}

fn Value* jit_apply_multi_args_primitive(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    interp.constructor_type_id = func.prim_val.tag;
    interp.prim_user_data = func.prim_val.user_data;
    Value** args = jit_alloc_multi_arg_buffer(interp, arg_count);
    if (arg_count > 0 && args == null) return raise_error(interp, "out of memory while preparing call arguments");
    Value* curr = arg_list;
    usz actual_count = 0;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != CONS) break;
        args[actual_count++] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }
    if (actual_count < arg_count) {
        return raise_error(interp, "arg list too short");
    }
    Value* result = func.prim_val.func(args[:actual_count], null, interp);
    if (is_error(result)) return raise_error(interp, result.str_chars[:result.str_len]);
    return result;
}

fn Value* jit_apply_multi_args_closure_multi(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    if (zeroarg_closure_trace_enabled()) {
        io::printfn(
            "OMNI_MULTI_TRACE closure-multi-enter param_count=%d arg_count=%d env_scope=%d env_is_global=%d",
            (int)func.closure_val.param_count,
            (int)arg_count,
            func.closure_val.env_scope != null ? 1 : 0,
            func.closure_val.env == interp.global_env ? 1 : 0
        );
    }
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        char[128] buf;
        usz len = format_arity_error(&buf, pc, arg_count);
        return raise_error(interp, buf[:len]);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* call_env = make_env(interp, func.closure_val.env);
    if (call_env == null) return raise_error(interp, "out of memory while binding argument");
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        Value* bind_err = env_define_with_barrier_result(call_env, func.closure_val.params[i], curr.cons_val.car, interp);
        if (bind_err != null) return raise_error(interp, "out of memory while binding argument");
        curr = curr.cons_val.cdr;
    }
    Value* result = jit_eval_in_single_scope(func.closure_val.body, call_env, interp);
    if (arg_count > pc && result != null && result.tag != ERROR) {
        usz remaining = arg_count - pc;
        if (zeroarg_closure_trace_enabled()) {
            io::printfn("OMNI_MULTI_TRACE closure-multi-chain remaining=%d", (int)remaining);
        }
        return jit_apply_multi_args(interp, result, curr, remaining);
    }
    if (zeroarg_closure_trace_enabled()) {
        io::printfn("OMNI_MULTI_TRACE closure-multi-exit body_tag=%d", (int)func.closure_val.body.tag);
    }
    return result;
}

fn Value* jit_apply_multi_args_method_table(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value** args = jit_alloc_multi_arg_buffer(interp, arg_count);
    if (arg_count > 0 && args == null) return raise_error(interp, "out of memory while preparing call arguments");
    Value* curr = arg_list;
    usz actual_count = 0;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != CONS) break;
        Value* arg = curr.cons_val.car;
        args[actual_count++] = arg;
        curr = curr.cons_val.cdr;
    }
    if (actual_count < arg_count) return raise_error(interp, "arg list too short");
    MethodTable* mt = func.method_table_val;
    Value* resolved = find_best_method(mt, args[:actual_count], interp);
    if (resolved != null && resolved.tag == ERROR) return resolved;
    if (resolved == null && mt.fallback != null) resolved = mt.fallback;
    if (resolved == null) return format_dispatch_error(mt, args[:actual_count], interp);
    return jit_apply_multi_args(interp, resolved, arg_list, arg_count);
}

fn Value* jit_apply_multi_args_iterative(Value* func, Value* arg_list, usz arg_count, Interp* interp) {
    Value* result = func;
    Value* curr = arg_list;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != CONS) break;
        Value* arg = curr.cons_val.car;
        curr = curr.cons_val.cdr;
        result = jit_apply_value(result, arg, interp);
        if (result != null && result.tag == ERROR) return result;
    }
    return result;
}

// Apply a function to a list of arguments, handling variadic closures natively.
// For variadic closures: binds fixed params from list head, rest from tail, evals body.
// For non-variadic closures: curries through list one arg at a time.
// For primitives: dispatches directly with all args.
// Detects variadic closures appearing mid-curry chain.
fn Value* jit_apply_multi_args(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value* frame_error = interp_apply_frame_enter(interp);
    if (frame_error != null) return frame_error;
    defer interp_apply_frame_leave(interp);

    if (func == null) return jit_null_callable_error(interp);
    if (func.tag == ERROR) return func;

    if (arg_count == 0 && func.tag == CLOSURE && !func.closure_val.has_param && !func.closure_val.has_rest) {
        if (zeroarg_closure_trace_enabled()) {
            io::printfn("OMNI_MULTI_TRACE dispatch zeroarg");
        }
        return jit_apply_multi_args_zeroarg_closure(interp, func);
    }
    if (func.tag == CLOSURE && func.closure_val.has_rest) {
        if (zeroarg_closure_trace_enabled()) {
            io::printfn("OMNI_MULTI_TRACE dispatch variadic arg_count=%d", (int)arg_count);
        }
        return jit_apply_multi_args_variadic_closure(interp, func, arg_list, arg_count);
    }
    if (func.tag == PRIMITIVE) {
        return jit_apply_multi_args_primitive(interp, func, arg_list, arg_count);
    }
    if (func.tag == CLOSURE) {
        usz pc = func.closure_val.param_count;
        if (pc > 1) {
            if (zeroarg_closure_trace_enabled()) {
                io::printfn("OMNI_MULTI_TRACE dispatch closure-multi param_count=%d arg_count=%d", (int)pc, (int)arg_count);
            }
            return jit_apply_multi_args_closure_multi(interp, func, arg_list, arg_count);
        }
    }
    if (func.tag == METHOD_TABLE) {
        return jit_apply_multi_args_method_table(interp, func, arg_list, arg_count);
    }
    return jit_apply_multi_args_iterative(func, arg_list, arg_count, interp);
}
