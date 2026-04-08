module lisp;

import std::core::mem;

fn bool deduce_why_result_bind_head_tuple_variables(
    DeduceRuleSignature* sig,
    Value*[] head_tuple,
    bool[]* bound_out,
    Value*[]* bound_values_out
) {
    if (bound_out != null) *bound_out = {};
    if (bound_values_out != null) *bound_values_out = {};
    if (sig == null || bound_out == null || bound_values_out == null) return false;
    if (sig.head_term_count != head_tuple.len) return false;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null) return false;

    usz var_count = (usz)sig.max_var_id + 1;
    bool[] bound = mem::new_array(bool, var_count);
    if (var_count > 0 && bound.ptr == null) return false;
    Value*[] bound_values = mem::new_array(Value*, var_count);
    if (var_count > 0 && bound_values.ptr == null) {
        if (bound.ptr != null) mem::free(bound.ptr);
        return false;
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        switch (sig.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_LITERAL:
                if (sig.head_term_literals == null ||
                    deduce_compare_value(head_tuple[i], sig.head_term_literals[i]) != 0) {
                    if (bound.ptr != null) mem::free(bound.ptr);
                    if (bound_values.ptr != null) mem::free(bound_values.ptr);
                    return false;
                }
                break;
            case DEDUCE_RULE_TERM_VARIABLE:
                uint var_id = sig.head_term_var_ids[i];
                if ((usz)var_id >= var_count) {
                    if (bound.ptr != null) mem::free(bound.ptr);
                    if (bound_values.ptr != null) mem::free(bound_values.ptr);
                    return false;
                }
                if (bound[var_id]) {
                    if (deduce_compare_value(bound_values[var_id], head_tuple[i]) != 0) {
                        if (bound.ptr != null) mem::free(bound.ptr);
                        if (bound_values.ptr != null) mem::free(bound_values.ptr);
                        return false;
                    }
                } else {
                    bound[var_id] = true;
                    bound_values[var_id] = head_tuple[i];
                }
                break;
            case DEDUCE_RULE_TERM_AGGREGATE:
                if (bound.ptr != null) mem::free(bound.ptr);
                if (bound_values.ptr != null) mem::free(bound_values.ptr);
                return false;
        }
    }

    *bound_out = bound;
    *bound_values_out = bound_values;
    return true;
}
