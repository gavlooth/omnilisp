module lisp;

fn Value* deduce_query_collect_disjunctive_head_demands_from_expr(
    Relation* rel,
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (supported_out != null) *supported_out = false;
    if (rel == null || expr == null || out_demands == null || supported_out == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    switch (expr.tag) {
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return null;

            Value* left_value = null;
            if (!deduce_query_expr_contains_row_param(expr.or_expr.left, row_param) &&
                deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
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
                if (!is_falsy(right_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
                );
            }

            DeduceQueryHeadDemandCases left_demands = {};
            defer deduce_query_head_demand_cases_release(&left_demands);
            DeduceQueryHeadDemandCases right_demands = {};
            defer deduce_query_head_demand_cases_release(&right_demands);
            bool left_supported = false;
            bool right_supported = false;
            Value* left_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.left,
                row_param,
                capture_env,
                &left_demands,
                &left_supported,
                interp
            );
            if (left_err != null) return left_err;
            Value* right_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.right,
                row_param,
                capture_env,
                &right_demands,
                &right_supported,
                interp
            );
            if (right_err != null) return right_err;
            if (!left_supported || !right_supported || left_demands.count == 0 || right_demands.count == 0) {
                return null;
            }

            for (usz i = 0; i < left_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &left_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }
            for (usz i = 0; i < right_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &right_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }

            *supported_out = true;
            return null;
        }
        default:
            return deduce_query_collect_disjunctive_head_demands_from_non_or_expr(
                rel,
                expr,
                row_param,
                capture_env,
                out_demands,
                supported_out,
                interp
            );
    }
}

fn Value* deduce_query_try_build_head_demand_disjunction_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (out_demands == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    *out_demands = {};
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_collect_disjunctive_head_demands_from_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_demands,
        supported_out,
        interp
    );
    if (demand_err != null) {
        deduce_query_head_demand_cases_release(out_demands);
        return demand_err;
    }
    if (!*supported_out || out_demands.count < 2) {
        deduce_query_head_demand_cases_release(out_demands);
        *supported_out = false;
    }
    return null;
}
