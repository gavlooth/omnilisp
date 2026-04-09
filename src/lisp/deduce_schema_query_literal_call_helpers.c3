module lisp;

fn bool deduce_query_expr_try_extract_control_flow_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (expr == null || out_value == null) return false;

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_literal_value(tail, row_param, capture_env, interp, out_value);
        }
        case E_LET: {
            if (interp == null || expr.let_expr.init == null || expr.let_expr.body == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;
            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            if (!scoped_env.define(expr.let_expr.name, init_value)) return false;

            return deduce_query_expr_try_extract_literal_value(
                expr.let_expr.body,
                row_param,
                scoped_env,
                interp,
                out_value
            );
        }
        case E_IF: {
            if (interp == null || expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.if_expr.test, row_param, capture_env, interp, &test_value)) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_expr_try_extract_literal_value(
                selected_branch,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_AND: {
            if (interp == null || expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.and_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.and_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        case E_OR: {
            if (interp == null || expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(expr.or_expr.left, row_param, capture_env, interp, &left_value)) {
                return false;
            }
            if (!is_falsy(left_value, interp)) {
                *out_value = left_value;
                return *out_value != null;
            }
            return deduce_query_expr_try_extract_literal_value(
                expr.or_expr.right,
                row_param,
                capture_env,
                interp,
                out_value
            );
        }
        default:
            return false;
    }
}

fn bool deduce_query_expr_try_extract_symbolic_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (expr == null || out_value == null) return false;

    switch (expr.tag) {
        case E_LAMBDA: {
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null || interp == null) return false;
            Value* closure = runtime_eval_expr(expr, lookup_env, interp);
            if (closure != null && !is_error(closure)) {
                *out_value = closure;
                return true;
            }
            return false;
        }
        case E_VAR: {
            if (expr.var_expr.name == row_param) return false;
            Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
            if (lookup_env == null) return false;
            *out_value = lookup_env.lookup(expr.var_expr.name);
            return *out_value != null;
        }
        default:
            return false;
    }
}

fn bool deduce_query_expr_try_extract_closure_call_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (expr == null || interp == null || out_value == null) return false;
    if (expr.call == null || expr.call.func == null || expr.call.func.tag != E_VAR) return false;

    Env* lookup_env = capture_env != null ? capture_env : interp.global_env;
    if (lookup_env == null) return false;

    Value* callee = lookup_env.lookup(expr.call.func.var_expr.name);
    if (callee == null || callee.tag != CLOSURE || callee.closure_val == null) return false;

    Closure* closure = callee.closure_val;
    if (closure.has_rest ||
        closure.param_count != 1 ||
        closure.params == null ||
        expr.call.arg_count != 1 ||
        expr.call.args == null ||
        expr.call.args[0] == null) {
        return false;
    }

    Value* call_arg = null;
    if (!deduce_query_expr_try_extract_literal_value(
            expr.call.args[0],
            row_param,
            capture_env,
            interp,
            &call_arg
        )) return false;
    if (call_arg == null) return false;

    Env* parent_env = closure.env != null ? closure.env : interp.global_env;
    if (parent_env == null) return false;
    Env* call_env = make_env(interp, parent_env);
    if (call_env == null) return false;
    if (!call_env.define(closure.params[0], call_arg)) return false;

    if (deduce_query_expr_try_extract_literal_value(
        closure.body,
        row_param,
        call_env,
        interp,
        out_value
    )) {
        return true;
    }

    // Last-resort fallback for closed one-arg wrapper closures.
    // Evaluate the wrapper body directly in the wrapper call env
    // so small pure wrappers such as `id` still collapse even if
    // the recursive extractor cannot simplify the body shape.
    Value* call_value = runtime_eval_expr(closure.body, call_env, interp);
    if (call_value == null) {
        call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
    }
    if (call_value != null) {
        *out_value = call_value;
        return true;
    }

    return false;
}

fn bool deduce_query_expr_try_extract_call_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (expr == null || interp == null || out_value == null) return false;
    if (expr.call == null || expr.call.func == null || expr.call.args == null) return false;
    if (expr.call.func.tag != E_VAR) return false;

    char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
    if (deduce_query_expr_try_extract_numeric_call_value(
            expr,
            func_name,
            row_param,
            capture_env,
            interp,
            out_value
        )) {
        return true;
    }

    if (deduce_query_expr_try_extract_closure_call_literal_value(
            expr,
            row_param,
            capture_env,
            interp,
            out_value
        )) {
        return true;
    }

    // Do not fold row-dependent calls into literals. If we evaluate these with no row binding,
    // they can collapse to falsy values and incorrectly prune disjunctive demand branches.
    if (deduce_query_expr_contains_row_param((Expr*)expr.call, row_param)) {
        return false;
    }

    Env* lookup_env = capture_env != null ? capture_env : (interp != null ? interp.global_env : null);
    if (lookup_env != null) {
        Value* call_value = runtime_eval_expr((Expr*)expr.call, lookup_env, interp);
        if (call_value != null) {
            *out_value = call_value;
            return true;
        }
    }
    return false;
}
