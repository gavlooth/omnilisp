module lisp;

// ============================================================
// Count + scan operations
// ============================================================
<*
@require interp != null
*>
fn Value* prim_deduce_count(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/count-expected-relation",
            "deduce-count: expected (deduce-count relation [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/count-relation-not-handle", "deduce-count: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/count-relation-handle-closed", "deduce-count: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/count-relation-not-deduce-relation", "deduce-count: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/count-database-not-open", "deduce-count: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/count-relation-dropped", "deduce-count: relation is dropped");

    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/count-out-of-memory",
        .out_of_memory_message = "deduce/count: out of memory",
        .invalid_selector_code = "deduce/count-invalid-selector",
        .invalid_selector_message = "deduce-count: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/count-invalid-engine",
        .invalid_engine_message = "deduce-count: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/count-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-count: rule-index is out of range",
        .invalid_state_code = "deduce/count-invalid-state",
        .invalid_state_message = "deduce-count: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/count-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/count: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/count-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/count: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/count-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/count: selected rule closure does not produce the requested relation",
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
    Value* exec_err = deduce_execute_goal_directed_relation_selector(
        rel.db,
        rel.name,
        selector_rule_index,
        use_seminaive_recursive,
        interp.symbols.intern("count"),
        &surface,
        interp
    );
    if (exec_err != null) return exec_err;
    if (selector_rule_index < 0) {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("count"),
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
            interp.symbols.intern("count"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    }

    MdbTxn* txn = null;
    int rc = g_deduce_force_read_txn_begin_fail ? -1 : mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/count-transaction-failed", "deduce-count: transaction open failed");
    }
    defer mdb_txn_abort(txn);

    MdbCursor* cursor = null;
    rc = g_deduce_force_read_cursor_open_fail ? -1 : mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/count-cursor-open-failed", "deduce-count: cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    long count = 0;
    MdbVal key;
    MdbVal data;
    rc = deduce_read_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        count++;
        rc = deduce_read_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    if (rc != MDB_NOTFOUND) {
        return deduce_raise(interp, "deduce/count-cursor-read-failed", "deduce-count: cursor read failed");
    }

    return make_int(interp, count);
}
