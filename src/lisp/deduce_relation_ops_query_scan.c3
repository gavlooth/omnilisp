module lisp;

<*
@require interp != null
*>
fn Value* prim_deduce_scan(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/scan-expected-relation",
            "deduce-scan: expected (deduce-scan relation [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/scan-relation-not-handle", "deduce-scan: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/scan-relation-handle-closed", "deduce-scan: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/scan-relation-not-deduce-relation", "deduce-scan: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/scan-database-not-open", "deduce-scan: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/scan-relation-dropped", "deduce-scan: relation is dropped");

    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/scan-out-of-memory",
        .out_of_memory_message = "deduce/scan: out of memory",
        .invalid_selector_code = "deduce/scan-invalid-selector",
        .invalid_selector_message = "deduce-scan: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/scan-invalid-engine",
        .invalid_engine_message = "deduce-scan: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/scan-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-scan: rule-index is out of range",
        .invalid_state_code = "deduce/scan-invalid-state",
        .invalid_state_message = "deduce-scan: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/scan-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/scan: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/scan-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/scan: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/scan-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/scan: selected rule closure does not produce the requested relation",
    };
    bool use_seminaive_recursive = true;
    long selector_rule_index = -1;
    Value* parse_err = deduce_parse_goal_directed_selector_and_engine(
        args,
        1,
        &surface,
        &selector_rule_index,
        &use_seminaive_recursive,
        interp
    );
    if (parse_err != null) return parse_err;
    bool selector_goal_directed_executed = false;
    Value* exec_err = deduce_execute_goal_directed_relation_selector(
        rel.db,
        rel.name,
        selector_rule_index,
        use_seminaive_recursive,
        interp.symbols.intern("scan"),
        &surface,
        interp
    );
    if (exec_err != null) return exec_err;
    if (selector_rule_index >= 0) selector_goal_directed_executed = true;
    if (selector_rule_index < 0) {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("scan"),
            env,
            interp
        );
        if (exec_err != null) return exec_err;
    } else {
        bool handled = false;
        exec_err = deduce_relation_maybe_refresh_on_read(
            rel_val,
            rel,
            env,
            interp.symbols.intern("scan"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    }

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = relation_scan_all(
        rel,
        interp,
        &subject_keys
    );
    if (rows != null && rows.tag == ERROR) return rows;
    if (selector_rule_index >= 0 && !selector_goal_directed_executed) {
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("no-op"),
            interp.symbols.intern("scan"),
            selector_rule_index
        );
    }
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}
