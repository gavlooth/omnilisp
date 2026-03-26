module lisp;

fn bool deduce_query_filter_expr_try_collect_scalar_param_equalities(
    Expr* expr,
    SymbolId scalar_param,
    usz scalar_column_index,
    Env* capture_env,
    Relation* rel,
    bool[] term_bound,
    Value*[] term_values,
    Interp* interp,
    bool* has_any_bound_out
) {
    if (expr == null || rel == null || interp == null || has_any_bound_out == null) return false;

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                tail,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    scalar_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                selected_branch,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return false;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return false;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    scalar_param,
                    capture_env,
                    interp,
                    &init_value
                )) return false;
            if (init_value == null) return false;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return false;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) return false;
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.let_expr.body,
                scalar_param,
                scalar_column_index,
                scoped_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    scalar_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                    expr.or_expr.right,
                    scalar_param,
                    scalar_column_index,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    scalar_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_scalar_param_equalities(
                    expr.or_expr.left,
                    scalar_param,
                    scalar_column_index,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            return false;
        }
        case E_AND: {
            bool left_supported = deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.and_expr.left,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            bool right_supported = deduce_query_filter_expr_try_collect_scalar_param_equalities(
                expr.and_expr.right,
                scalar_param,
                scalar_column_index,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            return left_supported || right_supported;
        }
        case E_CALL: {
            if (expr.call == null || expr.call.func == null || expr.call.args == null) return false;
            if (expr.call.func.tag != E_VAR) return false;
            char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
            if (deduce_name_equals(func_name, "=")) {
                if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;

                bool left_is_scalar = expr.call.args[0].tag == E_VAR && expr.call.args[0].var_expr.name == scalar_param;
                bool right_is_scalar = expr.call.args[1].tag == E_VAR && expr.call.args[1].var_expr.name == scalar_param;
                if (left_is_scalar == right_is_scalar) return false;

                Expr* literal_expr = left_is_scalar ? expr.call.args[1] : expr.call.args[0];
                Value* literal_value = null;
                if (!deduce_query_expr_try_extract_literal_value(
                        literal_expr,
                        scalar_param,
                        capture_env,
                        interp,
                        &literal_value
                    )) return false;
                if (literal_value == null) return false;

                if (term_bound[scalar_column_index]) {
                    return values_equal(term_values[scalar_column_index], literal_value);
                }
                term_bound[scalar_column_index] = true;
                term_values[scalar_column_index] = literal_value;
                *has_any_bound_out = true;
                return true;
            }

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
            for (usz i = 0; i < expr.call.arg_count; i++) {
                Expr* arg_expr = expr.call.args[i];
                if (arg_expr == null) return false;

                if (arg_expr.tag == E_VAR && arg_expr.var_expr.name == scalar_param) {
                    if (scalar_arg_index != usz.max) return false;
                    scalar_arg_index = i;
                    continue;
                }

                if (deduce_query_expr_contains_row_param(arg_expr, scalar_param)) return false;

                Value* literal_value = null;
                if (!deduce_query_expr_try_extract_literal_value(
                        arg_expr,
                        scalar_param,
                        capture_env,
                        interp,
                        &literal_value
                    )) return false;
                if (literal_value == null) return false;
                call_env.define(closure.params[i], literal_value);
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
        default:
            return false;
    }
}

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
        call_env.define(closure.params[i], literal_value);
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

fn bool deduce_query_filter_expr_try_collect_equalities(
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

    Expr* unwrapped_expr = null;
    SymbolId unwrapped_row_param = 0;
    Env* unwrapped_env = null;
    if (deduce_query_expr_try_unwrap_preserved_row_call_chain(
            expr,
            row_param,
            capture_env,
            interp,
            &unwrapped_expr,
            &unwrapped_row_param,
            &unwrapped_env
        )) {
        return deduce_query_filter_expr_try_collect_equalities(
            unwrapped_expr,
            unwrapped_row_param,
            unwrapped_env,
            rel,
            term_bound,
            term_values,
            interp,
            has_any_bound_out
        );
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_filter_expr_try_collect_equalities(
                tail,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) return false;
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return false;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_filter_expr_try_collect_equalities(
                selected_branch,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return false;
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
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_filter_expr_try_collect_equalities(
                expr.let_expr.body,
                row_param,
                scoped_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_equalities(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return false;
                return deduce_query_filter_expr_try_collect_equalities(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }
            return false;
        }
        default:
            return false;
    }
}
