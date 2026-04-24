module lisp;

fn Value* deduce_scan_range_execute_relation_read(
    Value* rel_val,
    Relation* rel,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceGoalDirectedSelectorSurface* surface,
    char* lower_key_buf,
    usz lower_key_len,
    Value*[] lower_values,
    Value*[] upper_values,
    Env* env,
    Interp* interp
) {
    bool selector_goal_directed_executed = false;
    Value* exec_err = deduce_execute_goal_directed_relation_selector(
        rel.db,
        rel.name,
        selector_rule_index,
        use_seminaive_recursive,
        interp.symbols.intern("scan-range"),
        surface,
        interp
    );
    if (exec_err != null) return exec_err;
    if (selector_rule_index >= 0) selector_goal_directed_executed = true;
    if (selector_rule_index < 0) {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("scan-range"),
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
            interp.symbols.intern("scan-range"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    }

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = relation_scan_range(
        rel,
        interp,
        lower_key_buf,
        lower_key_len,
        true,
        lower_values,
        upper_values,
        true,
        &subject_keys
    );
    if (rows != null && rows.tag == ERROR) return rows;
    if (selector_rule_index >= 0 && !selector_goal_directed_executed) {
        deduce_db_note_goal_directed_read(
            rel.db,
            rel.name,
            interp.symbols.intern("no-op"),
            interp.symbols.intern("scan-range"),
            selector_rule_index
        );
    }
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}
