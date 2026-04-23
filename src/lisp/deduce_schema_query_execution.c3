module lisp;

import std::core::mem;

<*
@require interp != null
*>
fn Value* prim_deduce_query(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2 || args.len > 4) {
        return deduce_raise(interp, "deduce/query-expected-relation-filter",
            "deduce-query: expected (deduce-query relation filter-fn [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/query-relation-not-handle", "deduce-query: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/query-relation-handle-closed", "deduce-query: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/query-relation-not-deduce-relation", "deduce-query: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/query-database-not-open", "deduce-query: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/query-relation-dropped", "deduce-query: relation is dropped");

    Value* filter_fn = args[1];
    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/query-out-of-memory",
        .out_of_memory_message = "deduce/query: out of memory",
        .invalid_selector_code = "deduce/query-invalid-selector",
        .invalid_selector_message = "deduce-query: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/query-invalid-engine",
        .invalid_engine_message = "deduce-query: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/query-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-query: rule-index is out of range",
        .invalid_state_code = "deduce/query-invalid-state",
        .invalid_state_message = "deduce-query: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/query-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/query: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/query-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/query: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/query-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce-query: selected rule closure does not produce the requested relation",
    };
    bool use_seminaive_recursive = true;
    long selector_rule_index = -1;
    Value* parse_err = deduce_parse_goal_directed_selector_and_engine(
        args,
        2,
        &surface,
        &selector_rule_index,
        &use_seminaive_recursive,
        interp
    );
    if (parse_err != null) return parse_err;
    Value* exec_err = null;

    Value* key_values_err = deduce_relation_ensure_column_key_values(rel, interp, null);
    if (key_values_err != null) return key_values_err;

    DeduceRuleHeadDemand head_demand = {};
    bool[] term_bound = {};
    Value*[] term_values = {};
    bool has_any_bound = false;
    bool supports_head_demand = false;
    Value* demand_err = deduce_query_try_build_head_demand_from_filter(
        rel,
        filter_fn,
        &head_demand,
        &term_bound,
        &term_values,
        &has_any_bound,
        &supports_head_demand,
        interp
    );
    if (demand_err != null) return demand_err;
    defer if (term_bound.ptr != null) mem::free(term_bound.ptr);
    defer if (term_values.ptr != null) mem::free(term_values.ptr);

    DeduceQueryHeadDemandCases disjunctive_head_demands = {};
    defer deduce_query_head_demand_cases_release(&disjunctive_head_demands);
    bool supports_disjunctive_head_demand = false;
    Value* disjunctive_err = deduce_query_try_build_head_demand_disjunction_from_filter(
        rel,
        filter_fn,
        &disjunctive_head_demands,
        &supports_disjunctive_head_demand,
        interp
    );
    if (disjunctive_err != null) return disjunctive_err;

    if (supports_disjunctive_head_demand && disjunctive_head_demands.count > 0) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral_disjunction(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral_disjunction(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &disjunctive_head_demands,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    } else if (supports_head_demand && has_any_bound) {
        if (selector_rule_index >= 0) {
            Value* demand_result = deduce_query_execute_goal_directed_selector_ephemeral(
                rel,
                filter_fn,
                selector_rule_index,
                use_seminaive_recursive,
                &head_demand,
                &surface,
                interp
            );
            if (demand_result != null) return demand_result;
        } else {
            bool used_plain_demand = false;
            Value* plain_demand_result = deduce_query_execute_goal_directed_plain_ephemeral(
                rel,
                filter_fn,
                use_seminaive_recursive,
                &head_demand,
                &used_plain_demand,
                interp
            );
            if (plain_demand_result != null || used_plain_demand) return plain_demand_result;
        }
    }

    bool selector_goal_directed_executed = false;
    if (selector_rule_index >= 0) {
        exec_err = deduce_execute_goal_directed_relation_selector(
            rel.db,
            rel.name,
            selector_rule_index,
            use_seminaive_recursive,
            interp.symbols.intern("query"),
            &surface,
            interp
        );
        if (exec_err != null) return exec_err;
        selector_goal_directed_executed = true;
        bool handled = false;
        exec_err = deduce_relation_maybe_refresh_on_read(
            rel_val,
            rel,
            env,
            interp.symbols.intern("query"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    } else {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("query"),
            env,
            interp
        );
        if (exec_err != null) return exec_err;
    }

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            deduce_goal_directed_read_subject_key_capture_record(
                &subject_keys,
                it.key.mv_data,
                it.key.mv_size
            );
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    if (selector_rule_index >= 0 && !selector_goal_directed_executed) {
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("no-op"),
            interp.symbols.intern("query"),
            selector_rule_index
        );
    }
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return deduce_reverse_list(filtered_rev, interp);
}
