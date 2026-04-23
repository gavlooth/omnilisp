module lisp;

fn Value* meta_raise(Interp* interp, String code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, (String)message, null);
}


// Macro/eval/type/meta primitives.
fn Value* prim_is_boolean(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "boolean?: expected 1 argument");
    if (is_symbol(args[0]) && (args[0].sym_val == interp.sym_true || args[0].sym_val == interp.sym_false)) {
        return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

fn Value* prim_is_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "list?: expected 1 argument");
    Value* v = args[0];
    if (is_nil(v)) return make_symbol(interp, interp.sym_true); // empty list is a list
    // Check if proper list (nil-terminated cons chain)
    while (is_cons(v)) { v = v.cons_val.cdr; }
    return is_nil(v) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_procedure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "procedure?: expected 1 argument");
    ValueTag t = args[0].tag;
    return (t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE || t == CONTINUATION) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_zero(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_numeric_value(args[0])) return meta_raise(interp, "type/arg-mismatch", "zero?: expected number");
    if (is_big_complex(args[0])) return big_complex_value_is_zero(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    if (is_complex128(args[0])) return complex128_is_zero(args[0].complex128_val) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    if (is_complex64(args[0])) return complex64_is_zero(args[0].complex64_val) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    int cmp = 0;
    Value* zero = make_int(interp, 0);
    if (!numeric_values_compare(args[0], zero, &cmp)) return meta_raise(interp, "runtime/evaluation-error", "zero?: numeric comparison failed");
    return cmp == 0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_positive(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_numeric_value(args[0])) return meta_raise(interp, "type/arg-mismatch", "positive?: expected number");
    if (is_big_complex(args[0]) || is_fixed_complex(args[0])) return meta_raise(interp, "type/arg-mismatch", "positive?: complex numbers are not ordered");
    int cmp = 0;
    Value* zero = make_int(interp, 0);
    if (!numeric_values_compare(args[0], zero, &cmp)) return meta_raise(interp, "runtime/evaluation-error", "positive?: numeric comparison failed");
    return cmp > 0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_negative(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_numeric_value(args[0])) return meta_raise(interp, "type/arg-mismatch", "negative?: expected number");
    if (is_big_complex(args[0]) || is_fixed_complex(args[0])) return meta_raise(interp, "type/arg-mismatch", "negative?: complex numbers are not ordered");
    int cmp = 0;
    Value* zero = make_int(interp, 0);
    if (!numeric_values_compare(args[0], zero, &cmp)) return meta_raise(interp, "runtime/evaluation-error", "negative?: numeric comparison failed");
    return cmp < 0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_even(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return meta_raise(interp, "type/arg-mismatch", "even?: expected integer");
    return (args[0].int_val % 2 == 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_odd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return meta_raise(interp, "type/arg-mismatch", "odd?: expected integer");
    return (args[0].int_val % 2 != 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_time_point_p(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "time-point?: expected 1 argument");
    return is_time_point(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

// --- Phase 7: Macroexpand, eval, apply, bound? ---

fn Value* prim_macroexpand(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arg-mismatch", "macroexpand: expected quoted expression");
    Value* form = args[0];
    // Form should be a list (macro-name arg...)
    if (!is_cons(form)) return form; // not a macro call, return as-is
    Expr* call_expr = value_to_expr(form, interp);
    if (call_expr == null) return form;
    if (call_expr.tag == E_LIT && call_expr.lit.value != null && call_expr.lit.value.tag == ERROR) {
        return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", (String)call_expr.lit.value.str_chars[:call_expr.lit.value.str_len], null);
    }

    EvalResult r;
    if (!try_expand_macro_call_expr(call_expr, interp, &r)) return form;
    if (r.error.has_error) return raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", (String)r.error.message[:256], null);
    if (r.value != null) return r.value;
    return make_nil(interp);
}

fn Value* prim_eval(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "eval: expected expression");
    Expr* expr = value_to_expr(args[0], interp);
    if (expr == null) return meta_raise(interp, "runtime/evaluation-error", "eval: could not convert to expression");
    Value* result = runtime_eval_expr(expr, interp.global_env, interp);
    if (result != null && result.tag == ERROR) return raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", (String)result.str_chars[:result.str_len], null);
    return result;
}

fn Value* prim_apply(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return meta_raise(interp, "type/arity", "apply: expected function and argument list");
    Value* func = args[0];
    Value* arg_list = args[1];
    if (!is_cons(arg_list) && !is_nil(arg_list)) {
        return meta_raise(interp, "type/arity", "apply: expected argument list");
    }
    // Count args
    usz arg_count = 0;
    Value* cur = arg_list;
    while (is_cons(cur)) {
        arg_count++;
        cur = cur.cons_val.cdr;
    }
    if (!is_nil(cur)) {
        return meta_raise(interp, "type/arity", "apply: expected a proper argument list");
    }
    // Use multi-arg apply (handles multi-param closures, variadics, etc.)
    return jit_apply_multi_args(interp, func, arg_list, arg_count);
}

fn Value* prim_bound(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return meta_raise(interp, "type/arg-mismatch", "bound?: expected symbol");
    SymbolId sym = args[0].sym_val;
    Env* lookup_env = env != null ? env : interp.global_env;
    Value* v = lookup_env.lookup(sym);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_error(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return meta_raise(interp, "type/arg-mismatch", "error: expected string message");
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", (String)args[0].str_chars[:args[0].str_len], null);
}

fn Value* prim_is_error(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "error?: expected 1 argument");
    return (args[0] != null && args[0].tag == ERROR)
        ? make_symbol(interp, interp.sym_true)
        : make_nil(interp);
}

fn Value* prim_error_message(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != ERROR) return meta_raise(interp, "type/arg-mismatch", "error-message: expected error value");
    return make_string(interp, args[0].str_chars[:args[0].str_len]);
}
