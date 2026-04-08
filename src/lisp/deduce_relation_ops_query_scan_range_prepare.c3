module lisp;

fn Value* deduce_scan_range_prepare_request(
    Value*[] args,
    Value** rel_val_out,
    Relation** rel_out,
    DeduceGoalDirectedSelectorSurface* surface,
    long* selector_rule_index_out,
    bool* use_seminaive_recursive_out,
    Value*[]* lower_values_out,
    Value*[]* upper_values_out,
    char* lower_key_buf,
    usz* lower_key_len_out,
    DeduceRuleHeadDemand* head_demand,
    bool[]* term_bound_out,
    Value*[]* term_values_out,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (selector_rule_index_out != null) *selector_rule_index_out = -1;
    if (use_seminaive_recursive_out != null) *use_seminaive_recursive_out = true;
    if (lower_values_out != null) *lower_values_out = {};
    if (upper_values_out != null) *upper_values_out = {};
    if (term_bound_out != null) *term_bound_out = {};
    if (term_values_out != null) *term_values_out = {};
    if (has_any_bound_out != null) *has_any_bound_out = false;
    if (lower_key_len_out != null) *lower_key_len_out = 0;

    if (args.len < 3 || args.len > 5) {
        return deduce_raise(interp, "deduce/scan-range-expected-relation-lower-upper",
            "deduce-scan-range: expected (deduce-scan-range relation lower-bound upper-bound [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/scan-range-relation-not-handle", "deduce-scan-range: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/scan-range-relation-handle-closed", "deduce-scan-range: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/scan-range-relation-not-deduce-relation", "deduce-scan-range: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/scan-range-database-not-open", "deduce-scan-range: database not open");
    }
    if (rel.dropped) {
        return deduce_raise(interp, "deduce/scan-range-relation-dropped", "deduce-scan-range: relation is dropped");
    }

    *surface = {
        .out_of_memory_code = "deduce/scan-range-out-of-memory",
        .out_of_memory_message = "deduce/scan-range: out of memory",
        .invalid_selector_code = "deduce/scan-range-invalid-selector",
        .invalid_selector_message = "deduce-scan-range: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/scan-range-invalid-engine",
        .invalid_engine_message = "deduce-scan-range: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/scan-range-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-scan-range: rule-index is out of range",
        .invalid_state_code = "deduce/scan-range-invalid-state",
        .invalid_state_message = "deduce-scan-range: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/scan-range-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/scan-range: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/scan-range-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/scan-range: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/scan-range-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/scan-range: selected rule closure does not produce the requested relation",
    };

    Value* parse_err = deduce_parse_goal_directed_selector_and_engine(
        args,
        3,
        surface,
        selector_rule_index_out,
        use_seminaive_recursive_out,
        interp
    );
    if (parse_err != null) return parse_err;

    Value*[] lower_values = {};
    Value*[] upper_values = {};
    Value* bounds_err = deduce_scan_range_prepare_bounds(
        args[1],
        args[2],
        rel,
        lower_key_buf,
        lower_key_len_out,
        head_demand,
        term_bound_out,
        term_values_out,
        has_any_bound_out,
        &lower_values,
        &upper_values,
        interp
    );
    if (bounds_err != null) return bounds_err;

    if (rel_val_out != null) *rel_val_out = rel_val;
    if (rel_out != null) *rel_out = rel;
    if (lower_values_out != null) *lower_values_out = lower_values;
    if (upper_values_out != null) *upper_values_out = upper_values;
    return null;
}
