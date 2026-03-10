module lisp;

import std::core::mem;
import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (apply-multi/tail + primitive shims)
// =============================================================================
fn Value* jit_cons(Interp* interp, Value* car, Value* cdr) {
    return make_cons(interp, car, cdr);
}

fn Value* jit_null_callable_error(Interp* interp) {
    if ((uint)interp.last_call_name != 0) {
        char[256] ebuf;
        char[] msg = io::bprintf(&ebuf, "'%s' is not defined\n  hint: did you (load ...) or (import ...) it?",
            (ZString)interp.symbols.get_name(interp.last_call_name))!!;
        return raise_error(interp, msg);
    }
    return raise_error(interp, "called value is nil — not a function");
}

fn Value* jit_apply_multi_args_zeroarg_closure(Interp* interp, Value* func) {
    return jit_eval_in_single_scope(func.closure_val.body, func.closure_val.env, interp);
}

fn Value* jit_check_closure_typed_prefix_args(Interp* interp, Closure* closure, Value* arg_list, usz arg_count) {
    if (closure == null || !closure.has_typed_params || closure.type_sig == null) return null;
    usz checked_count = closure.type_sig.param_count;
    if (checked_count == 0 || arg_count < checked_count) return null;

    Value** checked_storage = (Value**)mem::malloc(Value*.sizeof * checked_count);
    if (checked_storage == null) {
        return raise_error(interp, "lambda type check: out of memory");
    }
    defer mem::free(checked_storage);

    Value* curr = arg_list;
    for (usz i = 0; i < checked_count; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        checked_storage[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }

    return check_lambda_call_boundary(closure, checked_storage[:checked_count], arg_count, interp);
}

fn Value* jit_apply_multi_args_variadic_closure(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        return raise_error(interp, "variadic lambda arity mismatch");
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* new_env = func.closure_val.env;
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        new_env = new_env.extend(interp, func.closure_val.params[i], curr.cons_val.car);
        curr = curr.cons_val.cdr;
    }
    new_env = new_env.extend(interp, func.closure_val.rest_param, curr);
    return jit_eval_in_single_scope(func.closure_val.body, new_env, interp);
}

fn Value* jit_apply_multi_args_primitive(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    // Set constructor type_id and user_data (needed for type constructor / FFI primitives)
    interp.constructor_type_id = func.prim_val.tag;
    interp.prim_user_data = func.prim_val.user_data;
    Value** args = arg_count > 0
        ? (Value**)interp.current_scope.alloc(Value*.sizeof * arg_count)
        : null;
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
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        return raise_error(interp, "arity mismatch");
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* call_env = make_env(interp, func.closure_val.env);
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        env_define_with_barrier(call_env, func.closure_val.params[i], curr.cons_val.car, interp);
        curr = curr.cons_val.cdr;
    }
    Value* result = jit_eval_in_single_scope(func.closure_val.body, call_env, interp);
    // If extra args remain and result is callable, chain-apply them
    if (arg_count > pc && result != null && result.tag != ERROR) {
        usz remaining = arg_count - pc;
        return jit_apply_multi_args(interp, result, curr, remaining);
    }
    return result;
}

fn Value* jit_apply_multi_args_method_table(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value** args = arg_count > 0
        ? (Value**)interp.current_scope.alloc(Value*.sizeof * arg_count)
        : null;
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
    if (resolved == null && mt.fallback != null) {
        resolved = mt.fallback;
    }
    if (resolved == null) {
        return format_dispatch_error(mt, args[:actual_count], interp);
    }
    // Apply resolved closure/primitive with all args
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

    // Propagate errors from function evaluation (e.g. unbound variable)
    if (func.tag == ERROR) return func;

    // Zero-arg non-variadic closure: (lambda () body)
    if (arg_count == 0 && func.tag == CLOSURE && !func.closure_val.has_param && !func.closure_val.has_rest) {
        return jit_apply_multi_args_zeroarg_closure(interp, func);
    }

    // Direct variadic closure path
    if (func.tag == CLOSURE && func.closure_val.has_rest) {
        return jit_apply_multi_args_variadic_closure(interp, func, arg_list, arg_count);
    }

    // Direct primitive path for multi-arg primitives
    if (func.tag == PRIMITIVE) {
        return jit_apply_multi_args_primitive(interp, func, arg_list, arg_count);
    }

    // Non-variadic closure
    if (func.tag == CLOSURE) {
        usz pc = func.closure_val.param_count;
        if (pc > 1) {
            return jit_apply_multi_args_closure_multi(interp, func, arg_list, arg_count);
        }
        // Single-param: apply one arg, chain-apply rest if any
        // (falls through to one-at-a-time loop below)
    }

    // METHOD_TABLE: dispatch with all args at once
    if (func.tag == METHOD_TABLE) {
        return jit_apply_multi_args_method_table(interp, func, arg_list, arg_count);
    }

    // Partial primitives, continuations, etc. — apply one arg at a time
    return jit_apply_multi_args_iterative(func, arg_list, arg_count, interp);
}

fn Value* jit_set_tco_bounce(Interp* interp, Expr* expr, Env* env) @inline {
    interp.jit_tco_expr = expr;
    interp.jit_tco_env = env;
    interp.flags.jit_tco_bounce = true;
    return jit_tco_sentinel();
}

fn Value* jit_apply_multi_args_tail_variadic(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        char[256] ebuf;
        char[] msg = io::bprintf(&ebuf, "variadic lambda requires at least %d argument(s), got %d",
            (int)pc, (int)arg_count)!!;
        return raise_error(interp, msg);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* new_env = func.closure_val.env;
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        new_env = new_env.extend(interp, func.closure_val.params[i], curr.cons_val.car);
        curr = curr.cons_val.cdr;
    }
    new_env = new_env.extend(interp, func.closure_val.rest_param, curr);
    return jit_set_tco_bounce(interp, func.closure_val.body, new_env);
}

fn Value* jit_apply_multi_args_tail_closure_multi(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    usz pc = func.closure_val.param_count;
    if (arg_count < pc) {
        char[128] buf;
        usz len = format_arity_error(&buf, pc, arg_count);
        return raise_error(interp, buf[:len]);
    }
    Value* type_err = jit_check_closure_typed_prefix_args(interp, func.closure_val, arg_list, arg_count);
    if (type_err != null) return type_err;
    Env* call_env = make_env(interp, func.closure_val.env);
    Value* curr = arg_list;
    for (usz i = 0; i < pc; i++) {
        if (curr == null || curr.tag != CONS) return raise_error(interp, "arg list too short");
        env_define_with_barrier(call_env, func.closure_val.params[i], curr.cons_val.car, interp);
        curr = curr.cons_val.cdr;
    }
    if (arg_count > pc) {
        Value* result = jit_eval_in_single_scope(func.closure_val.body, call_env, interp);
        if (result != null && result.tag != ERROR) {
            usz remaining = arg_count - pc;
            return jit_apply_multi_args_tail(interp, result, curr, remaining);
        }
        return result;
    }
    return jit_set_tco_bounce(interp, func.closure_val.body, call_env);
}

fn Value* jit_apply_multi_args_tail_method_table(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value** args = arg_count > 0
        ? (Value**)interp.current_scope.alloc(Value*.sizeof * arg_count)
        : null;
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
    return jit_apply_multi_args_tail(interp, resolved, arg_list, arg_count);
}

// Tail-call variant: sets TCO bounce fields for CLOSURE instead of recursing.
// ONLY called when the result goes directly to R0 as the function return value.
fn Value* jit_apply_multi_args_tail(Interp* interp, Value* func, Value* arg_list, usz arg_count) {
    Value* frame_error = interp_apply_frame_enter(interp);
    if (frame_error != null) return frame_error;
    defer interp_apply_frame_leave(interp);

    if (func == null) return jit_null_callable_error(interp);

    // Propagate errors from function evaluation (e.g. unbound variable)
    if (func.tag == ERROR) return func;

    // Zero-arg non-variadic closure
    if (arg_count == 0 && func.tag == CLOSURE && !func.closure_val.has_param && !func.closure_val.has_rest) {
        return jit_set_tco_bounce(interp, func.closure_val.body, func.closure_val.env);
    }

    // Variadic closure
    if (func.tag == CLOSURE && func.closure_val.has_rest) {
        return jit_apply_multi_args_tail_variadic(interp, func, arg_list, arg_count);
    }

    // Non-variadic closure
    if (func.tag == CLOSURE) {
        usz pc = func.closure_val.param_count;
        if (pc > 1) {
            return jit_apply_multi_args_tail_closure_multi(interp, func, arg_list, arg_count);
        }
        // Single-param: fall through to one-at-a-time loop
    }

    // METHOD_TABLE: resolve, then bounce if closure
    if (func.tag == METHOD_TABLE) {
        return jit_apply_multi_args_tail_method_table(interp, func, arg_list, arg_count);
    }

    // Everything else: normal apply
    return jit_apply_multi_args(interp, func, arg_list, arg_count);
}

// Environment lookup helper for mutable (boxed) let-locals.
// The env node IS the box — lookup returns the current value.
fn Value* jit_env_lookup_local(Env* env, uint sym_id) {
    SymbolId name = (SymbolId)sym_id;
    Value* v = env.lookup(name);
    if (v != null) return v;
    return null;
}

// Reparent an env node — sets env.parent to new_parent.
// Used by emit_build_locals_env for mutable locals.
fn void jit_env_reparent(Env* env, Env* new_parent) {
    env.parent = new_parent;
}

// Direct primitive helpers — bypass apply() for arithmetic hot path
fn Value* jit_prim_add(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) {
        if (is_double(a) || is_double(b)) {
            if (is_number(a) && is_number(b)) return make_double(interp, to_double(a) + to_double(b));
        }
        if (a.tag == INT && b.tag == INT) return make_int(interp, a.int_val + b.int_val);
    }
    return raise_error(interp, "+: expected numbers");
}

fn Value* jit_prim_sub(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) {
        if (is_double(a) || is_double(b)) {
            if (is_number(a) && is_number(b)) return make_double(interp, to_double(a) - to_double(b));
        }
        if (a.tag == INT && b.tag == INT) return make_int(interp, a.int_val - b.int_val);
    }
    return raise_error(interp, "-: expected numbers");
}

fn Value* jit_prim_mul(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) {
        if (is_double(a) || is_double(b)) {
            if (is_number(a) && is_number(b)) return make_double(interp, to_double(a) * to_double(b));
        }
        if (a.tag == INT && b.tag == INT) return make_int(interp, a.int_val * b.int_val);
    }
    return raise_error(interp, "*: expected numbers");
}

fn Value* jit_prim_lt(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null && is_number(a) && is_number(b)) {
        return to_double(a) < to_double(b) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    }
    return raise_error(interp, "<: expected numbers");
}

fn Value* jit_prim_gt(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null && is_number(a) && is_number(b)) {
        return to_double(a) > to_double(b) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    }
    return raise_error(interp, ">: expected numbers");
}

fn Value* jit_prim_eq(Interp* interp, Value* a, Value* b) {
    if (a != null && b != null) {
        bool eq = values_equal(a, b);
        return eq ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    }
    return make_nil(interp);
}
