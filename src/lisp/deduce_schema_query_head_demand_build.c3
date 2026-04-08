module lisp;

import std::core::mem;

fn Value* deduce_query_build_union_head_demand_positions(
    Relation* rel,
    DeduceRuleHeadDemand* demands,
    usz demand_count,
    DeduceRuleHeadDemand* out_union,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_union == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid union head demand state");
    }

    *out_union = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0 || demands == null || demand_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }
    for (usz i = 0; i < demand_count; i++) {
        DeduceRuleHeadDemand* demand = &demands[i];
        if (demand.predicate != rel.name || demand.term_bound == null) continue;
        usz limit = demand.term_count < rel.col_count ? demand.term_count : rel.col_count;
        for (usz j = 0; j < limit; j++) {
            if (!demand.term_bound[j]) continue;
            term_bound[j] = true;
            has_any_bound = true;
        }
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_union = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    *has_any_bound_out = true;
    return null;
}
