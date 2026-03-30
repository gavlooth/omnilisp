module lisp;

import std::core::mem;
import std::io;
import std::collections::list;

// ============================================================
// Deduce Unification Engine
//
// Matches patterns with ?variables against dicts from relation scans.
// A binding environment maps ?var SymbolIds to Values.
// ============================================================

struct BindingEntry {
    SymbolId name;
    Value* value;
}

struct Bindings {
    List{BindingEntry} entries;
}

fn void bindings_init(Bindings* b) {
    *b = {};
}

fn void bindings_deinit(Bindings* b) {
    b.entries.free();
}

fn Value* bindings_lookup(Bindings* b, SymbolId vname) {
    for (usz i = 0; i < b.entries.len(); i++) {
        if ((uint)b.entries[i].name == (uint)vname) return b.entries[i].value;
    }
    return null;
}

fn void? bindings_bind(Bindings* b, SymbolId vname, Value* val) {
    // Check if already bound
    Value* existing = bindings_lookup(b, vname);
    if (existing != null) {
        // Already bound — check consistency
        if (!deduce_val_eq(existing, val)) return TYPE_MISMATCH~;
        return;
    }
    b.entries.push({ .name = vname, .value = val });
}

fn bool deduce_val_eq(Value* a, Value* b) {
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;
    if (a.tag != b.tag) return false;
    switch (a.tag) {
        case INT: return a.int_val == b.int_val;
        case DOUBLE: return a.double_val == b.double_val;
        case STRING: {
            if (a.str_len != b.str_len) return false;
            for (usz i = 0; i < a.str_len; i++) {
                if (a.str_chars[i] != b.str_chars[i]) return false;
            }
            return true;
        }
        case SYMBOL: return a.sym_val == b.sym_val;
        case NIL: return true;
        case VOID: return true;
        default: return a == b;  // pointer equality for other types
    }
}

fn bool is_logic_var(Value* v, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] name = interp.symbols.get_name(v.sym_val);
    return name.len > 0 && name[0] == '?';
}

fn bool is_wildcard(Value* v, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] name = interp.symbols.get_name(v.sym_val);
    return name.len == 1 && name[0] == '_';
}

fn uint bindings_result_dict_capacity(usz binding_count) {
    uint cap = 8;
    usz target = binding_count * 2;
    if (target < binding_count) target = binding_count;
    while ((usz)cap < target && cap <= uint.max / 2) {
        cap *= 2;
    }
    return cap;
}

// ============================================================
// Match a pattern clause against a dict row
//
// Pattern: (relation-name ?var1 ?var2 literal _ ...)
// Row: dict with column keys
// Relation: provides column name ordering
//
// Returns void on match success, TYPE_MISMATCH~ on failure.
// ============================================================

fn void? match_clause_against_row(
    Value* clause,       // list: (rel-name arg1 arg2 ...)
    Value* row,          // dict from deduce-scan
    Relation* rel,       // relation for column names
    Bindings* bindings,
    Interp* interp
) {
    // Skip relation name (first element)
    Value* args = cdr(clause);

    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        // Get column value from row dict
        Value* key = interp.alloc_value();
        key.tag = SYMBOL;
        key.sym_val = rel.columns[col];
        Value* col_val = hashmap_get(row.hashmap_val, key);

        if (is_wildcard(pattern_elem, interp)) {
            // _ matches anything
        } else if (is_logic_var(pattern_elem, interp)) {
            // ?var — bind or check consistency
            if (catch err = bindings_bind(bindings, pattern_elem.sym_val, col_val)) {
                return TYPE_MISMATCH~;  // inconsistent binding
            }
        } else {
            // Literal — must match exactly
            if (!deduce_val_eq(pattern_elem, col_val)) {
                return TYPE_MISMATCH~;
            }
        }
        args = cdr(args);
    }
}

fn void? match_clause_against_tuple(
    Value* clause,         // list: (rel-name arg1 arg2 ...)
    Value*[] tuple_vals,   // decoded tuple values in relation column order
    Relation* rel,
    Bindings* bindings,
    Interp* interp
) {
    Value* args = cdr(clause);

    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        Value* col_val = col < tuple_vals.len ? tuple_vals[col] : make_nil(interp);

        if (is_wildcard(pattern_elem, interp)) {
            // _ matches anything
        } else if (is_logic_var(pattern_elem, interp)) {
            if (catch err = bindings_bind(bindings, pattern_elem.sym_val, col_val)) {
                return TYPE_MISMATCH~;
            }
        } else {
            if (!deduce_val_eq(pattern_elem, col_val)) {
                return TYPE_MISMATCH~;
            }
        }
        args = cdr(args);
    }
}

// ============================================================
// Build result dict from bindings + projection
//
// If projection is nil, return all bindings.
// If projection is a list of ?vars, return only those.
// Strip ? from variable names in result keys.
// ============================================================

fn Value* build_result_dict(Bindings* bindings, Value* projection, Interp* interp) {
    usz binding_count = bindings.entries.len();
    uint dict_capacity = bindings_result_dict_capacity(binding_count);
    Value* dict = make_hashmap(interp, dict_capacity);

    if (projection == null || projection.tag == NIL) {
        // Return all bindings
        for (usz i = 0; i < binding_count; i++) {
            // Strip ? from variable name
            char[] name = interp.symbols.get_name(bindings.entries[i].name);
            SymbolId stripped;
            if (name.len > 1 && name[0] == '?') {
                stripped = interp.symbols.intern(name[1..]);
            } else {
                stripped = bindings.entries[i].name;
            }
            Value* key = interp.alloc_value();
            key.tag = SYMBOL;
            key.sym_val = stripped;
            hashmap_set(dict.hashmap_val, key, bindings.entries[i].value, interp);
        }
    } else {
        // Return only projected variables
        Value* proj = projection;
        while (is_cons(proj)) {
            Value* pvar = car(proj);
            if (pvar.tag == SYMBOL) {
                Value* val = bindings_lookup(bindings, pvar.sym_val);
                if (val != null) {
                    char[] name = interp.symbols.get_name(pvar.sym_val);
                    SymbolId stripped;
                    if (name.len > 1 && name[0] == '?') {
                        stripped = interp.symbols.intern(name[1..]);
                    } else {
                        stripped = pvar.sym_val;
                    }
                    Value* key = interp.alloc_value();
                    key.tag = SYMBOL;
                    key.sym_val = stripped;
                    hashmap_set(dict.hashmap_val, key, val, interp);
                }
            }
            proj = cdr(proj);
        }
    }

    return dict;
}

fn Value* deduce_match_build_head_demand(
    Value* pattern,
    Relation* rel,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0) {
        *out_head_demand = { .predicate = rel != null ? rel.name : (SymbolId)0, .term_count = 0 };
        return null;
    }

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
    }

    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }

    Value* args = cdr(pattern);
    for (usz col = 0; col < rel.col_count && is_cons(args); col++) {
        Value* pattern_elem = car(args);
        if (!is_wildcard(pattern_elem, interp) && !is_logic_var(pattern_elem, interp)) {
            term_bound[col] = true;
            term_values[col] = pattern_elem;
            *has_any_bound_out = true;
        }
        args = cdr(args);
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}

fn Value* deduce_match_scan_relation_in_txn(
    Relation* rel,
    Value* pattern,
    MdbTxn* txn,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid transaction scan state");
    }
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    Value*[] tuple_vals = {};
    if (rel.col_count > 0) {
        tuple_vals = mem::new_array(Value*, rel.col_count);
        if (tuple_vals.ptr == null) {
            return deduce_raise(interp, "deduce/match-out-of-memory", "deduce-match: out of memory");
        }
    }
    defer if (tuple_vals.ptr != null) mem::free(tuple_vals.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, rel.dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    Value* result_rev = make_nil(interp);
    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (decoded_count != rel.col_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        Bindings bindings = {};
        bindings_init(&bindings);
        if (catch err = match_clause_against_tuple(pattern, tuple_vals, rel, &bindings, interp)) {
            // Pattern did not match this tuple.
        } else {
            if (subject_keys_out != null) {
                deduce_goal_directed_read_subject_key_capture_record(
                    subject_keys_out,
                    key.mv_data,
                    key.mv_size
                );
            }
            Value* result_dict = build_result_dict(&bindings, null, interp);
            result_rev = make_cons(interp, result_dict, result_rev);
        }
        bindings_deinit(&bindings);
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return deduce_reverse_list(result_rev, interp);
}

fn Value* deduce_match_scan_relation(
    Relation* rel,
    Value* pattern,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);
    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    Value* result_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Bindings bindings = {};
        bindings_init(&bindings);

        if (catch err = match_clause_against_tuple(pattern, it.vals, rel, &bindings, interp)) {
            // Pattern didn't match this row — skip.
        } else {
            if (subject_keys_out != null) {
                deduce_goal_directed_read_subject_key_capture_record(
                    subject_keys_out,
                    it.key.mv_data,
                    it.key.mv_size
                );
            }
            Value* result_dict = build_result_dict(&bindings, null, interp);
            result_rev = make_cons(interp, result_dict, result_rev);
        }
        bindings_deinit(&bindings);
    }
    return deduce_reverse_list(result_rev, interp);
}

fn Value* deduce_match_execute_goal_directed_selector_ephemeral(
    Relation* rel,
    Value* pattern,
    long selector_rule_index,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    if (rel == null || rel.db == null || head_demand == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid goal-directed match state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        rel.db,
        rel.name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        long iterations = 0;
        long derived = 0;
        long max_component_iterations = 0;
        Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            rel.db,
            &scc_plan,
            selected_components,
            256,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            {},
            interp
        );
        if (exec_err != null) return exec_err;
        deduce_fixpoint_clear_selected_dirty_predicates(rel.db, &scc_plan, selected_components);
        return deduce_match_scan_relation(rel, pattern, interp);
    }

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    DeduceObservedRuleCounters[] observed_rules = {};
    Value* observed_err = deduce_allocate_selected_rule_observed_counters(
        rel.db,
        selector_rule_index,
        &observed_rules,
        interp
    );
    if (observed_err != null) return observed_err;
    defer deduce_free_selected_rule_observed_counters(observed_rules);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        rel.db,
        &scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        &projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        observed_rules,
        interp
    );
    if (exec_err != null) return exec_err;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-match"),
        interp.symbols.intern("match"),
        selector_rule_index,
        head_demand,
        &projected_head_demand,
        &observed_rules[(usz)selector_rule_index]
    );
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

fn Value* deduce_match_execute_goal_directed_plain_ephemeral(
    Relation* rel,
    Value* pattern,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    if (rel == null || rel.db == null || head_demand == null || interp == null) {
        return deduce_raise(interp, "deduce/match-invalid-state", "deduce-match: invalid plain goal-directed match state");
    }

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        rel.db,
        rel.name,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    DeduceRuleHeadDemand projected_head_demand = {};
    bool[] projected_term_bound = {};
    Value*[] projected_term_values = {};
    bool has_projected_head_demand = false;
    Value* project_err = deduce_component_project_relation_head_demand(
        rel.db,
        &scc_plan,
        selected_component_id,
        head_demand,
        &projected_head_demand,
        &projected_term_bound,
        &projected_term_values,
        &has_projected_head_demand,
        interp
    );
    if (project_err != null) return project_err;
    defer if (projected_term_bound.ptr != null) mem::free(projected_term_bound.ptr);
    defer if (projected_term_values.ptr != null) mem::free(projected_term_values.ptr);

    if (!has_projected_head_demand) {
        return null;
    }

    DeduceSelectedSchemaEstimateSnapshot[] schema_snapshots = {};
    Value* snapshot_err = deduce_fixpoint_snapshot_selected_schema_estimates(
        rel.db,
        &scc_plan,
        selected_components,
        &schema_snapshots,
        interp
    );
    if (snapshot_err != null) return snapshot_err;
    defer {
        deduce_fixpoint_restore_selected_schema_estimates(schema_snapshots);
        if (schema_snapshots.ptr != null) mem::free(schema_snapshots.ptr);
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/match-transaction-failed", "deduce-match: transaction open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
        rel.db,
        &scc_plan,
        selected_components,
        txn,
        256,
        use_seminaive_recursive,
        &projected_head_demand,
        &iterations,
        &derived,
        &max_component_iterations,
        {},
        interp
    );
    if (exec_err != null) return exec_err;

    if (used_out != null) *used_out = true;
    deduce_db_note_goal_directed_read(
        rel.db,
        rel.name,
        interp.symbols.intern("ephemeral-head-demand-match"),
        interp.symbols.intern("match"),
        -1,
        head_demand,
        &projected_head_demand
    );
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

// ============================================================
// (deduce-match relation pattern [rule-index] [engine]) → list of dicts
//
// Pattern is a quoted list: '(person ?name ?age _)
// Returns dicts with ?var bindings (? stripped from keys).
// ============================================================

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
