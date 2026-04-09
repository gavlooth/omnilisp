module lisp;

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
            if (!scoped_env.define(expr.let_expr.name, init_value)) return false;

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
            if (!deduce_query_expr_contains_row_param(expr.or_expr.left, row_param) &&
                deduce_query_expr_try_extract_literal_value(
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
            if (!deduce_query_expr_contains_row_param(expr.or_expr.right, row_param) &&
                deduce_query_expr_try_extract_literal_value(
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
        case E_AND: {
            bool left_supported = deduce_query_filter_expr_try_collect_equalities(
                expr.and_expr.left,
                row_param,
                capture_env,
                rel,
                term_bound,
                term_values,
                interp,
                has_any_bound_out
            );
            bool right_supported = deduce_query_filter_expr_try_collect_equalities(
                expr.and_expr.right,
                row_param,
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
            if (!deduce_name_equals(func_name, "=")) {
                return deduce_query_filter_expr_try_collect_equalities_from_captured_scalar_call(
                    expr,
                    row_param,
                    capture_env,
                    rel,
                    term_bound,
                    term_values,
                    interp,
                    has_any_bound_out
                );
            }
            if (expr.call.arg_count != 2 || expr.call.args[0] == null || expr.call.args[1] == null) return false;

            usz left_column_index = 0;
            bool left_is_row_column = deduce_query_expr_try_extract_row_column_ref(
                expr.call.args[0],
                row_param,
                capture_env,
                rel,
                interp,
                &left_column_index
            );
            usz right_column_index = 0;
            bool right_is_row_column = deduce_query_expr_try_extract_row_column_ref(
                expr.call.args[1],
                row_param,
                capture_env,
                rel,
                interp,
                &right_column_index
            );
            if (left_is_row_column == right_is_row_column) return false;

            usz column_index = left_is_row_column ? left_column_index : right_column_index;
            Expr* literal_expr = left_is_row_column ? expr.call.args[1] : expr.call.args[0];
            Value* literal_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    literal_expr,
                    row_param,
                    capture_env,
                    interp,
                    &literal_value
                )) return false;
            if (literal_value == null) return false;

            if (term_bound[column_index]) {
                return values_equal(term_values[column_index], literal_value);
            }
            term_bound[column_index] = true;
            term_values[column_index] = literal_value;
            *has_any_bound_out = true;
            return true;
        }
        default:
            return false;
    }
}
