module lisp;

import std::core::mem;

fn Value* deduce_scan_range_build_head_demand(
    Relation* rel,
    Value*[] lower_values,
    Value*[] upper_values,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/scan-range-invalid-state", "deduce-scan-range: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0) {
        *out_head_demand = { .predicate = rel != null ? rel.name : (SymbolId)0, .term_count = 0 };
        return null;
    }

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
    }

    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
        if (i < lower_values.len && i < upper_values.len && values_equal(lower_values[i], upper_values[i])) {
            term_bound[i] = true;
            term_values[i] = lower_values[i];
            *has_any_bound_out = true;
        }
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}
