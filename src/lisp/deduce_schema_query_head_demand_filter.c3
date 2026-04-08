module lisp;

import std::core::mem;

fn Value* deduce_query_build_head_demand_from_filter_expr(
    Relation* rel,
    Expr* body,
    SymbolId row_param,
    Env* capture_env,
    DeduceRuleHeadDemand* out_head_demand,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || body == null || interp == null || row_param == (SymbolId)0) return null;
    if (rel.col_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }

    bool has_any_bound = false;
    if (!deduce_query_filter_expr_try_collect_equalities(
            body,
            row_param,
            capture_env,
            rel,
            term_bound,
            term_values,
            interp,
            &has_any_bound
        ) || !has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *has_any_bound_out = has_any_bound;
    *supported_out = true;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}
