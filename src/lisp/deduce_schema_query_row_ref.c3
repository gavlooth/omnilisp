module lisp;

fn bool deduce_query_expr_try_extract_row_column_ref(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Relation* rel,
    Interp* interp,
    usz* out_column_index
) {
    if (out_column_index == null || expr == null || rel == null || interp == null) return false;
    *out_column_index = 0;

    Expr* unwrapped_expr = null;
    SymbolId unwrapped_row_param = (SymbolId)0;
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
        return deduce_query_expr_try_extract_row_column_ref(
            unwrapped_expr,
            unwrapped_row_param,
            unwrapped_env,
            rel,
            interp,
            out_column_index
        );
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return false;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return false;
            return deduce_query_expr_try_extract_row_column_ref(
                tail,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_LET: {
            if (expr.let_expr.init == null || expr.let_expr.body == null) return false;
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

            return deduce_query_expr_try_extract_row_column_ref(
                expr.let_expr.body,
                row_param,
                scoped_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return false;
            }
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
            return deduce_query_expr_try_extract_row_column_ref(
                selected_branch,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_AND: {
            if (expr.and_expr.left == null || expr.and_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.and_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.and_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return false;
            Value* left_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) return false;
            if (!is_falsy(left_value, interp)) {
                return deduce_query_expr_try_extract_row_column_ref(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    rel,
                    interp,
                    out_column_index
                );
            }
            return deduce_query_expr_try_extract_row_column_ref(
                expr.or_expr.right,
                row_param,
                capture_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_WITH_MODULE: {
            Expr* tail = null;
            Env* scoped_env = null;
            if (!deduce_query_expr_try_open_with_module_tail_env(
                    expr,
                    capture_env,
                    interp,
                    &tail,
                    &scoped_env
                )) return false;
            return deduce_query_expr_try_extract_row_column_ref(
                tail,
                row_param,
                scoped_env,
                rel,
                interp,
                out_column_index
            );
        }
        case E_CALL:
            break;
        default:
            return false;
    }
    if (expr.call == null || expr.call.arg_count != 2) return false;
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return false;
    char[] func_name = interp.symbols.get_name(expr.call.func.var_expr.name);
    if (!deduce_name_equals(func_name, "ref")) return false;
    if (expr.call.args == null || expr.call.args[0] == null || expr.call.args[1] == null) return false;
    if (expr.call.args[0].tag != E_VAR || expr.call.args[0].var_expr.name != row_param) return false;

    Value* column_value = null;
    if (!deduce_query_expr_try_extract_literal_value(expr.call.args[1], row_param, capture_env, interp, &column_value)) return false;
    if (column_value == null || column_value.tag != SYMBOL) return false;
    return deduce_find_column_position(rel.columns, rel.col_count, column_value.sym_val, out_column_index);
}
