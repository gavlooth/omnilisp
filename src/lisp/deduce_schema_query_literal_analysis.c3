module lisp;

fn bool deduce_query_expr_try_extract_literal_value(
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    Interp* interp,
    Value** out_value
) {
    if (out_value == null || expr == null) return false;
    *out_value = null;

    switch (expr.tag) {
        case E_LIT:
            *out_value = expr.lit.value;
            return *out_value != null;
        case E_QUOTE:
            *out_value = expr.quote.datum;
            return *out_value != null;
        case E_BLOCK:
        case E_LET:
        case E_IF:
        case E_AND:
        case E_OR:
            return deduce_query_expr_try_extract_control_flow_literal_value(
                expr,
                row_param,
                capture_env,
                interp,
                out_value
            );
        case E_LAMBDA:
        case E_VAR:
            return deduce_query_expr_try_extract_symbolic_literal_value(
                expr,
                row_param,
                capture_env,
                interp,
                out_value
            );
        case E_CALL:
            return deduce_query_expr_try_extract_call_literal_value(
                expr,
                row_param,
                capture_env,
                interp,
                out_value
            );
        default:
            return false;
    }
}
