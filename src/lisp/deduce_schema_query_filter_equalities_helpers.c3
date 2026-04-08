module lisp;

fn bool deduce_query_filter_expr_try_collect_equalities_from_captured_scalar_call(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    bool[] term_bound,
    Value*[] term_values,
    Interp* interp,
    bool* has_any_bound_out
) {
    if (expr == null || rel == null || interp == null || has_any_bound_out == null) return false;
    if (expr.tag != E_CALL || expr.call == null || expr.call.func == null || expr.call.args == null) return false;
    if (expr.call.func.tag != E_VAR) return false;

    Env* lookup_env = capture_env != null ? capture_env : interp.global_env;
    if (lookup_env == null) return false;
    Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
    if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) return false;

    Closure* closure = callee.closure_val;
    if (closure.has_rest ||
        closure.param_count != expr.call.arg_count ||
        closure.params == null ||
        closure.body == null) return false;

    Env* parent_env = closure.env != null ? closure.env : interp.global_env;
    if (parent_env == null) return false;
    Env* call_env = make_env(interp, parent_env);
    if (call_env == null) return false;

    usz scalar_arg_index = usz.max;
    usz scalar_column_index = 0;
    for (usz i = 0; i < expr.call.arg_count; i++) {
        Expr* arg_expr = expr.call.args[i];
        if (arg_expr == null) return false;

        usz column_index = 0;
        if (deduce_query_expr_try_extract_row_column_ref(
                arg_expr,
                row_param,
                capture_env,
                rel,
                interp,
                &column_index
            )) {
            if (scalar_arg_index != usz.max) return false;
            scalar_arg_index = i;
            scalar_column_index = column_index;
            continue;
        }

        if (deduce_query_expr_contains_row_param(arg_expr, row_param)) return false;

        Value* literal_value = null;
        if (!deduce_query_expr_try_extract_literal_value(
                arg_expr,
                row_param,
                capture_env,
                interp,
                &literal_value
            )) return false;
        if (literal_value == null) return false;
        if (!call_env.define(closure.params[i], literal_value)) return false;
    }

    if (scalar_arg_index == usz.max) return false;
    return deduce_query_filter_expr_try_collect_scalar_param_equalities(
        closure.body,
        closure.params[scalar_arg_index],
        scalar_column_index,
        call_env,
        rel,
        term_bound,
        term_values,
        interp,
        has_any_bound_out
    );
}
