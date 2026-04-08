module lisp;

fn Value* deduce_aggregate_group_note_binding_with_polarity(
    DeduceAggregateGroup* group,
    DeduceRuleSignature* sig,
    bool[] var_bound,
    Value*[] var_values,
    bool removal,
    Interp* interp
) {
    if (group == null || sig == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate state");
    }
    if (sig.head_term_kinds == null || sig.head_term_aggregate_ops == null ||
        sig.head_term_var_ids == null || group.term_states == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate metadata");
    }

    deduce_aggregate_group_snapshot_previous(group, sig);

    long group_delta = removal ? -1 : 1;
    if (group_delta > 0) {
        if (group.support_count > long.max - group_delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support overflow");
        }
        group.support_count += group_delta;
    } else {
        if (group.support_count < -group_delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support underflow");
        }
        group.support_count += group_delta;
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] != DEDUCE_RULE_TERM_AGGREGATE) continue;

        uint var_id = sig.head_term_var_ids[i];
        if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound aggregate input at runtime");
        }

        Value* input = var_values[var_id];
        DeduceAggregateTermState* state = &group.term_states[i];
        long delta = removal ? -1 : 1;
        switch (sig.head_term_aggregate_ops[i]) {
            case DEDUCE_AGGREGATE_COUNT:
                if (delta > 0) {
                    state.initialized = true;
                    if (state.count_value > long.max - delta) {
                        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate count overflow");
                    }
                    state.count_value += delta;
                } else {
                    if (state.count_value < -delta) {
                        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate count underflow");
                    }
                    state.count_value += delta;
                    if (state.count_value == 0) state.initialized = false;
                }
                break;
            case DEDUCE_AGGREGATE_SUM:
                if (!is_number(input)) {
                    return deduce_raise(interp, "deduce/aggregate-type-mismatch",
                        "deduce/analyze: sum aggregate expects numeric input");
                }
                if (delta > 0 && !state.initialized) {
                    state.initialized = true;
                    state.sum_is_double = is_double(input);
                    state.sum_int_value = is_int(input) ? input.int_val : 0;
                    state.sum_double_value = to_double(input);
                    break;
                }
                if (!state.initialized) {
                    return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate sum underflow");
                }
                if (!state.sum_is_double && is_int(input)) {
                    state.sum_int_value += delta * input.int_val;
                    state.sum_double_value = (double)state.sum_int_value;
                } else {
                    if (!state.sum_is_double) {
                        state.sum_double_value = (double)state.sum_int_value;
                        state.sum_is_double = true;
                    }
                    state.sum_double_value += (double)delta * to_double(input);
                }
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.sum_is_double = false;
                    state.sum_int_value = 0;
                    state.sum_double_value = 0.0;
                }
                break;
            case DEDUCE_AGGREGATE_MIN: {
                long next_support = 0;
                Value* err = deduce_aggregate_group_adjust_term_support_count(
                    group,
                    i,
                    input,
                    delta,
                    &next_support,
                    interp
                );
                if (err != null) return err;
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.extremum_value = null;
                    break;
                }
                if (delta > 0) {
                    if (!state.initialized || deduce_compare_value(input, state.extremum_value) < 0) {
                        state.initialized = true;
                        state.extremum_value = input;
                    }
                } else if (!state.initialized ||
                           (state.extremum_value != null && values_equal(input, state.extremum_value) && next_support == 0)) {
                    deduce_aggregate_group_recompute_extremum(group, i, false, state);
                }
                break;
            }
            case DEDUCE_AGGREGATE_MAX: {
                long next_support = 0;
                Value* err = deduce_aggregate_group_adjust_term_support_count(
                    group,
                    i,
                    input,
                    delta,
                    &next_support,
                    interp
                );
                if (err != null) return err;
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.extremum_value = null;
                    break;
                }
                if (delta > 0) {
                    if (!state.initialized || deduce_compare_value(input, state.extremum_value) > 0) {
                        state.initialized = true;
                        state.extremum_value = input;
                    }
                } else if (!state.initialized ||
                           (state.extremum_value != null && values_equal(input, state.extremum_value) && next_support == 0)) {
                    deduce_aggregate_group_recompute_extremum(group, i, true, state);
                }
                break;
            }
            case DEDUCE_AGGREGATE_NONE:
                return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate operator");
        }
    }
    return null;
}

fn Value* deduce_aggregate_group_note_binding(
    DeduceAggregateGroup* group,
    DeduceRuleSignature* sig,
    bool[] var_bound,
    Value*[] var_values,
    Interp* interp
) {
    return deduce_aggregate_group_note_binding_with_polarity(
        group,
        sig,
        var_bound,
        var_values,
        false,
        interp
    );
}

fn Value* deduce_naive_accumulate_aggregate_binding(
    DeduceNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.aggregate_groups == null || exec.sig == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }

    DeduceAggregateGroup* group = null;
    Value* err = deduce_aggregate_group_find_or_add(
        exec.aggregate_groups,
        exec.sig,
        var_bound,
        var_values,
        &group,
        exec.interp
    );
    if (err != null) return err;
    return deduce_aggregate_group_note_binding(group, exec.sig, var_bound, var_values, exec.interp);
}
