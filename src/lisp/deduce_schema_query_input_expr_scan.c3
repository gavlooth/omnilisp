module lisp;

fn bool deduce_query_expr_try_open_with_module_tail_env(
    Expr* expr,
    Env* capture_env,
    Interp* interp,
    Expr** out_tail,
    Env** out_env
) {
    if (expr == null || expr.tag != E_WITH_MODULE || expr.with_module == null ||
        interp == null || out_tail == null || out_env == null) return false;
    *out_tail = null;
    *out_env = null;
    if (expr.with_module.body == null || expr.with_module.body_count == 0) return false;

    Expr* tail = expr.with_module.body[expr.with_module.body_count - 1];
    if (tail == null) return false;

    ExprImport spec = expr.with_module.import_expr;
    Module* mod = null;
    EvalResult load_result = load_import_target_module_spec(&spec, spec.name, interp, &mod);
    if (load_result.error.has_error || mod == null) return false;

    Env* parent_env = capture_env != null ? capture_env : interp.global_env;
    if (parent_env == null) return false;
    Env* scoped_env = make_env(interp, parent_env);
    if (scoped_env == null) return false;

    spec.import_all = true;
    spec.import_count = 0;
    EvalResult bind_result = bind_import_symbols_spec(&spec, scoped_env, mod, interp);
    if (bind_result.error.has_error) return false;

    *out_tail = tail;
    *out_env = scoped_env;
    return true;
}

fn bool deduce_query_expr_contains_row_param(Expr* expr, SymbolId row_param) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_VAR:
            return expr.var_expr.name == row_param;
        case E_BLOCK:
            if (expr.block == null || expr.block.exprs == null) return false;
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (deduce_query_expr_contains_row_param(expr.block.exprs[i], row_param)) return true;
            }
            return false;
        case E_LET:
            return (expr.let_expr.init != null && deduce_query_expr_contains_row_param(expr.let_expr.init, row_param)) ||
                (expr.let_expr.body != null && deduce_query_expr_contains_row_param(expr.let_expr.body, row_param));
        case E_IF:
            return (expr.if_expr.test != null && deduce_query_expr_contains_row_param(expr.if_expr.test, row_param)) ||
                (expr.if_expr.then_branch != null && deduce_query_expr_contains_row_param(expr.if_expr.then_branch, row_param)) ||
                (expr.if_expr.else_branch != null && deduce_query_expr_contains_row_param(expr.if_expr.else_branch, row_param));
        case E_AND:
            return (expr.and_expr.left != null && deduce_query_expr_contains_row_param(expr.and_expr.left, row_param)) ||
                (expr.and_expr.right != null && deduce_query_expr_contains_row_param(expr.and_expr.right, row_param));
        case E_OR:
            return (expr.or_expr.left != null && deduce_query_expr_contains_row_param(expr.or_expr.left, row_param)) ||
                (expr.or_expr.right != null && deduce_query_expr_contains_row_param(expr.or_expr.right, row_param));
        case E_CALL:
            if (expr.call == null) return false;
            if (deduce_query_expr_contains_row_param(expr.call.func, row_param)) return true;
            if (expr.call.args != null) {
                for (usz i = 0; i < expr.call.arg_count; i++) {
                    if (deduce_query_expr_contains_row_param(expr.call.args[i], row_param)) return true;
                }
            }
            return false;
        case E_APP:
            return (expr.app.func != null && deduce_query_expr_contains_row_param(expr.app.func, row_param)) ||
                (expr.app.arg != null && deduce_query_expr_contains_row_param(expr.app.arg, row_param));
        case E_PATH:
            return false;
        case E_INDEX:
            return (expr.index.collection != null && deduce_query_expr_contains_row_param(expr.index.collection, row_param)) ||
                (expr.index.index != null && deduce_query_expr_contains_row_param(expr.index.index, row_param));
        case E_SET:
            return expr.set_expr.value != null && deduce_query_expr_contains_row_param(expr.set_expr.value, row_param);
        case E_DEFINE:
            return expr.define.value != null && deduce_query_expr_contains_row_param(expr.define.value, row_param);
        case E_RESET:
            return expr.reset.body != null && deduce_query_expr_contains_row_param(expr.reset.body, row_param);
        case E_SHIFT:
            return expr.shift.body != null && deduce_query_expr_contains_row_param(expr.shift.body, row_param);
        case E_PERFORM:
            return expr.perform.arg != null && deduce_query_expr_contains_row_param(expr.perform.arg, row_param);
        case E_RESOLVE:
            return expr.resolve.value != null && deduce_query_expr_contains_row_param(expr.resolve.value, row_param);
        case E_HANDLE:
            if (expr.handle.body != null && deduce_query_expr_contains_row_param(expr.handle.body, row_param)) return true;
            if (expr.handle.clauses != null) {
                for (usz i = 0; i < expr.handle.clause_count; i++) {
                    if (expr.handle.clauses[i].handler_body != null &&
                        deduce_query_expr_contains_row_param(expr.handle.clauses[i].handler_body, row_param)) return true;
                }
            }
            return false;
        case E_QUASIQUOTE:
            return expr.quasiquote.body != null && deduce_query_expr_contains_row_param(expr.quasiquote.body, row_param);
        case E_UNQUOTE:
            return expr.unquote.body != null && deduce_query_expr_contains_row_param(expr.unquote.body, row_param);
        case E_UNQUOTE_SPLICING:
            return expr.unquote_splicing.body != null && deduce_query_expr_contains_row_param(expr.unquote_splicing.body, row_param);
        case E_LAMBDA:
            if (expr.lambda.body == null) return false;
            return deduce_query_expr_contains_row_param(expr.lambda.body, row_param);
        case E_WITH_MODULE:
            if (expr.with_module == null || expr.with_module.body == null) return false;
            for (usz i = 0; i < expr.with_module.body_count; i++) {
                if (deduce_query_expr_contains_row_param(expr.with_module.body[i], row_param)) return true;
            }
            return false;
        default:
            return false;
    }
}
