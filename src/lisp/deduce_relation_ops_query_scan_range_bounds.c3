module lisp;

import std::core::mem;

fn Value* deduce_scan_range_prepare_bounds(
    Value* lower_arg,
    Value* upper_arg,
    Relation* rel,
    char* lower_key_buf,
    usz* lower_key_len_out,
    DeduceRuleHeadDemand* head_demand,
    bool[]* term_bound_out,
    Value*[]* term_values_out,
    bool* has_any_bound_out,
    Value*[]* lower_values_out,
    Value*[]* upper_values_out,
    Interp* interp
) {
    Value*[] lower_values = {};
    Value*[] upper_values = {};
    if (rel.col_count > 0) {
        lower_values = mem::new_array(Value*, rel.col_count);
        if (lower_values.ptr == null) {
            return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
        }
        upper_values = mem::new_array(Value*, rel.col_count);
        if (upper_values.ptr == null) {
            mem::free(lower_values.ptr);
            return deduce_raise(interp, "deduce/scan-range-out-of-memory", "deduce-scan-range: out of memory");
        }
    }

    Value* lower_err = deduce_parse_bound_tuple(
        lower_arg,
        rel.col_count,
        lower_values,
        interp,
        "deduce/scan-range-lower-not-list",
        "deduce-scan-range: lower-bound must be a tuple list",
        "deduce/scan-range-lower-arity-mismatch",
        "deduce-scan-range: lower-bound arity must match relation columns"
    );
    if (lower_err != null) {
        if (upper_values.ptr != null) mem::free(upper_values.ptr);
        if (lower_values.ptr != null) mem::free(lower_values.ptr);
        return lower_err;
    }

    Value* upper_err = deduce_parse_bound_tuple(
        upper_arg,
        rel.col_count,
        upper_values,
        interp,
        "deduce/scan-range-upper-not-list",
        "deduce-scan-range: upper-bound must be a tuple list",
        "deduce/scan-range-upper-arity-mismatch",
        "deduce-scan-range: upper-bound arity must match relation columns"
    );
    if (upper_err != null) {
        if (upper_values.ptr != null) mem::free(upper_values.ptr);
        if (lower_values.ptr != null) mem::free(lower_values.ptr);
        return upper_err;
    }

    usz? lower_key_len = encode_tuple(lower_values, lower_key_buf[0..4096], 4096);
    if (catch err = lower_key_len) {
        if (upper_values.ptr != null) mem::free(upper_values.ptr);
        if (lower_values.ptr != null) mem::free(lower_values.ptr);
        return deduce_raise(interp, "deduce/scan-range-lower-key-too-large", "deduce-scan-range: lower-bound tuple too large for buffer");
    }

    Value* demand_err = deduce_scan_range_build_head_demand(
        rel,
        lower_values,
        upper_values,
        head_demand,
        term_bound_out,
        term_values_out,
        has_any_bound_out,
        interp
    );
    if (demand_err != null) {
        if (upper_values.ptr != null) mem::free(upper_values.ptr);
        if (lower_values.ptr != null) mem::free(lower_values.ptr);
        return demand_err;
    }

    if (lower_values_out != null) *lower_values_out = lower_values;
    if (upper_values_out != null) *upper_values_out = upper_values;
    if (lower_key_len_out != null) *lower_key_len_out = lower_key_len;
    return null;
}
