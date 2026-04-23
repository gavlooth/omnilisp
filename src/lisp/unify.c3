module lisp;

// ============================================================
// (deduce-match relation pattern [rule-index] [engine]) → list of dicts
//
// Pattern is a quoted list: '(person ?name ?age _)
// Returns dicts with ?var bindings (? stripped from keys).
// ============================================================

<*
@require interp != null
*>
fn Value* prim_deduce_match(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 2 || args.len > 4) {
        return deduce_raise(interp, "deduce/match-expected-relation-pattern",
            "deduce-match: expected (deduce-match relation '(pattern) [rule-index] [engine])");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/match-relation-not-handle", "deduce-match: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/match-relation-handle-closed", "deduce-match: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/match-relation-not-deduce-relation", "deduce-match: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/match-database-not-open", "deduce-match: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/match-relation-dropped", "deduce-match: relation is dropped");

    Value* pattern = args[1];
    if (!is_cons(pattern)) return deduce_raise(interp, "deduce/match-pattern-not-list", "deduce-match: pattern must be a list");

    DeduceGoalDirectedSelectorSurface surface = {
        .out_of_memory_code = "deduce/match-out-of-memory",
        .out_of_memory_message = "deduce/match: out of memory",
        .invalid_selector_code = "deduce/match-invalid-selector",
        .invalid_selector_message = "deduce-match: rule-index selector must be an integer",
        .invalid_engine_code = "deduce/match-invalid-engine",
        .invalid_engine_message = "deduce-match: engine must be 'naive or 'semi-naive",
        .rule_index_out_of_range_code = "deduce/match-rule-index-out-of-range",
        .rule_index_out_of_range_message = "deduce-match: rule-index is out of range",
        .invalid_state_code = "deduce/match-invalid-state",
        .invalid_state_message = "deduce-match: selected rule is missing from the SCC plan",
        .full_recompute_code = "deduce/match-goal-directed-selector-full-recompute-required",
        .full_recompute_message = "deduce/match: selected goal-directed execution is unavailable while full recompute is required",
        .selector_not_eligible_code = "deduce/match-goal-directed-selector-not-eligible",
        .selector_not_eligible_message = "deduce/match: selected rule is not goal-directed eligible",
        .relation_mismatch_code = "deduce/match-goal-directed-selector-does-not-produce-relation",
        .relation_mismatch_message = "deduce/match: selected rule closure does not produce the requested relation",
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

    DeduceRuleHeadDemand head_demand = {};
    bool[] term_bound = {};
    Value*[] term_values = {};
    bool has_any_bound = false;
    Value* demand_err = deduce_match_build_head_demand(
        pattern,
        rel,
        &head_demand,
        &term_bound,
        &term_values,
        &has_any_bound,
        interp
    );
    if (demand_err != null) return demand_err;
    defer if (term_bound.ptr != null) mem::free(term_bound.ptr);
    defer if (term_values.ptr != null) mem::free(term_values.ptr);

    if (selector_rule_index >= 0) {
        if (has_any_bound) {
            return deduce_match_execute_goal_directed_selector_ephemeral(
                rel,
                pattern,
                selector_rule_index,
                use_seminaive_recursive,
                &head_demand,
                &surface,
                interp
            );
        }
    } else if (has_any_bound) {
        bool used = false;
        Value* plain_demand_result = deduce_match_execute_goal_directed_plain_ephemeral(
            rel,
            pattern,
            use_seminaive_recursive,
            &head_demand,
            &used,
            interp
        );
        if (plain_demand_result != null || used) return plain_demand_result;
    }

    Value* exec_err = deduce_execute_goal_directed_relation_selector(
        rel.db,
        rel.name,
        selector_rule_index,
        use_seminaive_recursive,
        interp.symbols.intern("match"),
        &surface,
        interp
    );
    if (exec_err != null) return exec_err;
    if (selector_rule_index < 0) {
        exec_err = deduce_prepare_plain_relation_read(
            rel_val,
            rel,
            true,
            interp.symbols.intern("match"),
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
            interp.symbols.intern("match"),
            interp,
            &handled
        );
        if (exec_err != null) return exec_err;
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    DeduceGoalDirectedReadSubjectKeyCapture subject_keys = { .complete = true };
    defer deduce_goal_directed_read_subject_key_capture_release(&subject_keys);
    Value* rows = deduce_match_scan_relation_in_txn(
        rel,
        pattern,
        txn,
        interp,
        &subject_keys
    );
    deduce_relation_schema_note_goal_directed_read_subject_keys(
        deduce_relation_lookup_schema(rel),
        &subject_keys
    );
    return rows;
}
