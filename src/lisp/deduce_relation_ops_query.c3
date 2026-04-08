module lisp;

import std::core::mem;

fn Value* prim_deduce_scan_range(Value*[] args, Env* env, Interp* interp) {
    Value* rel_val = null;
    Relation* rel = null;
    DeduceGoalDirectedSelectorSurface surface = {};
    bool use_seminaive_recursive = true;
    long selector_rule_index = -1;
    Value*[] lower_values = {};
    Value*[] upper_values = {};
    DeduceRuleHeadDemand head_demand = {};
    bool[] term_bound = {};
    Value*[] term_values = {};
    bool has_any_bound = false;
    char[4096] lower_key_buf;
    usz lower_key_len = 0;
    Value* prepare_err = deduce_scan_range_prepare_request(
        args,
        &rel_val,
        &rel,
        &surface,
        &selector_rule_index,
        &use_seminaive_recursive,
        &lower_values,
        &upper_values,
        &lower_key_buf[0],
        &lower_key_len,
        &head_demand,
        &term_bound,
        &term_values,
        &has_any_bound,
        interp
    );
    if (prepare_err != null) return prepare_err;
    defer if (lower_values.ptr != null) mem::free(lower_values.ptr);
    defer if (upper_values.ptr != null) mem::free(upper_values.ptr);
    defer if (term_bound.ptr != null) mem::free(term_bound.ptr);
    defer if (term_values.ptr != null) mem::free(term_values.ptr);

    if (has_any_bound) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_scan_range_execute_goal_directed_selector_demand(
                rel,
                lower_values,
                upper_values,
                &lower_key_buf[0],
                lower_key_len,
                use_seminaive_recursive,
                selector_rule_index,
                &head_demand,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_scan_range_execute_goal_directed_plain_demand(
                rel,
                lower_values,
                upper_values,
                &lower_key_buf[0],
                lower_key_len,
                use_seminaive_recursive,
                &head_demand,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    return deduce_scan_range_execute_relation_read(
        rel_val,
        rel,
        selector_rule_index,
        use_seminaive_recursive,
        &surface,
        &lower_key_buf[0],
        lower_key_len,
        lower_values,
        upper_values,
        env,
        interp
    );
}
