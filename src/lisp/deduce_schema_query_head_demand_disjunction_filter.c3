module lisp;

fn Value* deduce_query_collect_disjunctive_head_demands_from_non_or_expr(
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

    DeduceRuleHeadDemand head_demand = {};
    defer deduce_rule_head_demand_release(&head_demand);
    bool has_any_bound = false;
    bool demand_supported = false;
    Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
        rel,
        expr,
        row_param,
        capture_env,
        &head_demand,
        &has_any_bound,
        &demand_supported,
        interp
    );
    if (demand_err != null) return demand_err;
    if (!demand_supported || !has_any_bound) return null;

    Value* append_err = deduce_query_head_demand_cases_append_take(out_demands, &head_demand, interp);
    if (append_err != null) return append_err;
    *supported_out = true;
    return null;
}
