module lisp;

import std::core::mem;

fn Value* deduce_explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    return out;
}

fn SymbolId deduce_explain_operator_symbol(Interp* interp, DeducePhysicalOpKind op_kind) {
    switch (op_kind) {
        case DEDUCE_PHYSICAL_FULL_SCAN:
            return interp.symbols.intern("full-scan");
        case DEDUCE_PHYSICAL_INDEX_SCAN:
            return interp.symbols.intern("index-scan");
        case DEDUCE_PHYSICAL_NEGATED_SCAN:
            return interp.symbols.intern("negated-scan");
        case DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE:
            return interp.symbols.intern("negated-index-probe");
    }
}

fn SymbolId deduce_explain_aggregate_op_symbol(Interp* interp, DeduceAggregateOpKind op_kind) {
    switch (op_kind) {
        case DEDUCE_AGGREGATE_NONE:
            return interp.symbols.intern("none");
        case DEDUCE_AGGREGATE_COUNT:
            return interp.symbols.intern("count");
        case DEDUCE_AGGREGATE_SUM:
            return interp.symbols.intern("sum");
        case DEDUCE_AGGREGATE_MIN:
            return interp.symbols.intern("min");
        case DEDUCE_AGGREGATE_MAX:
            return interp.symbols.intern("max");
    }
}

fn SymbolId deduce_explain_head_projection_symbol(Interp* interp, DeduceRuleTermKind kind) {
    switch (kind) {
        case DEDUCE_RULE_TERM_LITERAL:
            return interp.symbols.intern("literal");
        case DEDUCE_RULE_TERM_VARIABLE:
            return interp.symbols.intern("group-key");
        case DEDUCE_RULE_TERM_AGGREGATE:
            return interp.symbols.intern("aggregate");
    }
}

fn Value* deduce_explain_build_head_projections(
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (sig == null || sig.head_term_count == 0 || sig.head_term_kinds == null) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.head_term_count; i++) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
        DeduceRuleTermKind kind = sig.head_term_kinds[i];
        explain_dict_set(entry, interp, "position", make_int(interp, (long)i));
        explain_dict_set_symbol(entry, interp, "projection-kind",
            deduce_explain_head_projection_symbol(interp, kind));
        if (kind == DEDUCE_RULE_TERM_LITERAL) {
            Value* literal = sig.head_term_literals != null ? sig.head_term_literals[i] : null;
            explain_dict_set(entry, interp, "literal", literal == null ? make_nil(interp) : literal);
            explain_dict_set(entry, interp, "source-var-id", make_nil(interp));
            explain_dict_set(entry, interp, "aggregate-function", make_nil(interp));
        } else {
            uint source_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[i] : 0;
            explain_dict_set(entry, interp, "literal", make_nil(interp));
            explain_dict_set(entry, interp, "source-var-id", make_int(interp, (long)source_var_id));
            if (kind == DEDUCE_RULE_TERM_AGGREGATE && sig.head_term_aggregate_ops != null) {
                explain_dict_set_symbol(entry, interp, "aggregate-function",
                    deduce_explain_aggregate_op_symbol(interp, sig.head_term_aggregate_ops[i]));
            } else {
                explain_dict_set(entry, interp, "aggregate-function", make_nil(interp));
            }
        }
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn long deduce_explain_rows_read_estimate(DeduceRelationSchema* schema) {
    if (schema == null) return 0;
    if (schema.cardinality_estimate > (ulong)long.max) return long.max;
    return (long)schema.cardinality_estimate;
}

fn long deduce_explain_rows_emitted_estimate(
    DeduceRelationSchema* schema,
    ulong bound_mask,
    int selected_index
) {
    if (schema == null) return 0;
    long rows_read = deduce_explain_rows_read_estimate(schema);
    if (rows_read <= 0) return 0;
    if (selected_index < 0) return rows_read;
    if (bound_mask == 0) return rows_read;
    if (schema.distinct_estimate == 0) return 1;

    ulong distinct = schema.distinct_estimate;
    if (distinct > schema.cardinality_estimate) distinct = schema.cardinality_estimate;
    if (distinct == 0) return 1;
    if (distinct >= schema.cardinality_estimate) return 1;

    long emitted = rows_read / (long)distinct;
    if (emitted <= 0) emitted = 1;
    if (emitted > rows_read) emitted = rows_read;
    return emitted;
}

fn Value* deduce_explain_build_step_counters(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    usz step_index,
    ulong bound_mask,
    int selected_index,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    Value* counters = make_hashmap(interp, 8);
    if (counters == null || counters.tag != HASHMAP || counters.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    if (observed_rule_counters != null && observed_rule_counters.steps != null &&
        step_index < observed_rule_counters.step_count) {
        DeduceObservedStepCounters step = observed_rule_counters.steps[step_index];
        explain_dict_set(counters, interp, "rows-read", make_int(interp, step.rows_read));
        explain_dict_set(counters, interp, "rows-emitted", make_int(interp, step.rows_emitted));
        explain_dict_set(counters, interp, "index-hits", make_int(interp, step.index_hits));
        explain_dict_set(counters, interp, "index-misses", make_int(interp, step.index_misses));
        explain_dict_set(counters, interp, "join-probes", make_int(interp, step.join_probes));
        explain_dict_set_symbol(counters, interp, "counter-kind", interp.symbols.intern("observed"));
        return counters;
    }

    DeduceRelationSchema* schema = null;
    if (db != null && sig != null && sig.body_predicates != null && step_index < sig.body_count) {
        schema = deduce_db_lookup_relation_schema(db, sig.body_predicates[step_index]);
    }

    long rows_read = deduce_explain_rows_read_estimate(schema);
    long rows_emitted = deduce_explain_rows_emitted_estimate(schema, bound_mask, selected_index);
    long index_hits = selected_index >= 0 ? rows_emitted : 0;
    long index_misses = 0;
    if (selected_index >= 0 && rows_read > index_hits) {
        index_misses = rows_read - index_hits;
    }

    explain_dict_set(counters, interp, "rows-read", make_int(interp, rows_read));
    explain_dict_set(counters, interp, "rows-emitted", make_int(interp, rows_emitted));
    explain_dict_set(counters, interp, "index-hits", make_int(interp, index_hits));
    explain_dict_set(counters, interp, "index-misses", make_int(interp, index_misses));
    explain_dict_set(counters, interp, "join-probes", make_int(interp, 0));
    explain_dict_set_symbol(counters, interp, "counter-kind", interp.symbols.intern("estimated"));
    return counters;
}

fn Value* deduce_explain_build_join_order(
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (sig == null || sig.body_count == 0 || sig.body_predicates == null) {
        return make_nil(interp);
    }
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.body_count; i++) {
        rev = make_cons(interp, make_symbol(interp, sig.body_predicates[i]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_build_plan_steps(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (sig == null || sig.body_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.body_count; i++) {
        Value* step = make_hashmap(interp, 16);
        if (step == null || step.tag != HASHMAP || step.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
        DeduceRuleAtom op_probe = {};
        op_probe.negated = sig.body_negated != null ? sig.body_negated[i] : false;
        int selected_index = sig.body_selected_indexes != null ? sig.body_selected_indexes[i] : -1;
        ulong bound_mask = sig.body_bound_masks != null ? sig.body_bound_masks[i] : 0;
        DeducePhysicalOpKind op_kind = deduce_rule_choose_op_kind(&op_probe, selected_index);

        explain_dict_set(step, interp, "position", make_int(interp, (long)i));
        if (sig.body_predicates != null) {
            explain_dict_set(step, interp, "predicate", make_symbol(interp, sig.body_predicates[i]));
        } else {
            explain_dict_set(step, interp, "predicate", make_nil(interp));
        }
        explain_dict_set(step, interp, "negated",
            op_probe.negated ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(step, interp, "bound-mask", make_int(interp, (long)bound_mask));
        if (selected_index >= 0) {
            explain_dict_set(step, interp, "selected-index", make_int(interp, (long)selected_index));
        } else {
            explain_dict_set(step, interp, "selected-index", make_nil(interp));
        }
        bool filter_pushdown = sig.body_filter_pushdown != null ? sig.body_filter_pushdown[i] : (bound_mask != 0);
        ulong projection_mask = sig.body_projection_masks != null ? sig.body_projection_masks[i] : 0;
        Value* counters = deduce_explain_build_step_counters(
            db,
            sig,
            i,
            bound_mask,
            selected_index,
            observed_rule_counters,
            interp
        );
        if (counters != null && counters.tag == ERROR) return counters;
        explain_dict_set(step, interp, "filter-pushdown",
            filter_pushdown ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(step, interp, "projection-mask", make_int(interp, (long)projection_mask));
        explain_dict_set(step, interp, "operator",
            make_symbol(interp, deduce_explain_operator_symbol(interp, op_kind)));
        explain_dict_set(step, interp, "counters", counters);
        rev = make_cons(interp, step, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_build_rule_execution_entry(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    usz rule_index,
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (sig == null) return make_nil(interp);
    Value* entry = make_hashmap(interp, 12);
    if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    Value* steps = deduce_explain_build_plan_steps(db, sig, observed_rule_counters, interp);
    if (steps != null && steps.tag == ERROR) return steps;
    explain_dict_set(entry, interp, "rule-index", make_int(interp, (long)rule_index));
    explain_dict_set(entry, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(entry, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(entry, interp, "aggregate-projection-count", make_int(interp, (long)sig.aggregate_projection_count));
    Value* head_projections = deduce_explain_build_head_projections(sig, interp);
    if (head_projections != null && head_projections.tag == ERROR) return head_projections;
    explain_dict_set(entry, interp, "head-projections", head_projections);
    explain_dict_set(entry, interp, "steps", steps);
    return entry;
}

fn Value* deduce_explain_build_rule_execution_list(
    DeduceDb* db,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || db.rule_signature_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        Value* entry = deduce_explain_build_rule_execution_entry(
            db,
            &db.rule_signatures[i],
            i,
            observed_rule,
            interp
        );
        if (entry != null && entry.tag == ERROR) return entry;
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* prim_deduce_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/explain-expected-db-selector",
            "deduce/explain: expected (deduce/explain db [head-symbol|rule-index] [engine])");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/explain-db-not-handle",
            "deduce/explain: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/explain-db-handle-closed",
            "deduce/explain: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/explain-db-not-open", "deduce/explain: database is not open");
    }
    if (db.rule_signature_count == 0) {
        return deduce_raise(interp, "deduce/explain-no-rules", "deduce/explain: no rules are registered");
    }

    isz selected_rule_index = (isz)db.rule_signature_count - 1;
    if (args.len >= 2) {
        Value* selector = args[1];
        if (selector != null && selector.tag == INT) {
            if (selector.int_val < 0 || (usz)selector.int_val >= db.rule_signature_count) {
                return deduce_raise(interp, "deduce/explain-rule-index-out-of-range",
                    "deduce/explain: rule index is out of range");
            }
            selected_rule_index = (isz)selector.int_val;
        } else if (selector != null && selector.tag == SYMBOL) {
            bool found = false;
            for (usz i = 0; i < db.rule_signature_count; i++) {
                if (db.rule_signatures[i].head_predicate == selector.sym_val) {
                    selected_rule_index = (isz)i;
                    found = true;
                    break;
                }
            }
            if (!found) {
                return deduce_raise(interp, "deduce/explain-rule-not-found",
                    "deduce/explain: no rule found for requested head predicate");
            }
        } else {
            return deduce_raise(interp, "deduce/explain-selector-not-symbol-or-int",
                "deduce/explain: selector must be a head symbol or rule index");
        }
    }

    bool use_seminaive_recursive = true;
    SymbolId requested_execution_engine_sym = interp.symbols.intern("semi-naive-scc");
    if (args.len == 3) {
        Value* engine_arg = args[2];
        if (engine_arg == null || engine_arg.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/explain-invalid-engine",
                "deduce/explain: engine must be symbol 'naive or 'semi-naive");
        }
        char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
        if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
            use_seminaive_recursive = false;
            requested_execution_engine_sym = interp.symbols.intern("naive-scc");
        } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
            use_seminaive_recursive = true;
            requested_execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        } else {
            return deduce_raise(interp, "deduce/explain-invalid-engine",
                "deduce/explain: engine must be 'naive or 'semi-naive");
        }
    }

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    SymbolId execution_engine_sym = requested_execution_engine_sym;
    bool naive_recursive_mixed_aggregate_routed = false;
    if (!use_seminaive_recursive &&
        selected_rule_index >= 0 &&
        (usz)selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)selected_rule_index];
        bool recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        if (recursive && deduce_component_has_mixed_aggregate_rules(db, &scc_plan, component_id)) {
            use_seminaive_recursive = true;
            execution_engine_sym = interp.symbols.intern("semi-naive-scc");
            naive_recursive_mixed_aggregate_routed = true;
        }
    }

    bool aggregate_recursive_fallback = false;
    if (use_seminaive_recursive &&
        selected_rule_index >= 0 &&
        (usz)selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)selected_rule_index];
        bool recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        if (recursive &&
            deduce_component_has_aggregate_rules(db, &scc_plan, component_id) &&
            (!deduce_component_can_use_seminaive_aggregates(db, &scc_plan, component_id) ||
             !deduce_component_has_full_recursive_aggregate_seminaive_support(db, &scc_plan, component_id))) {
            aggregate_recursive_fallback = true;
            execution_engine_sym = interp.symbols.intern("naive-scc");
        }
    }

    DeduceRuleSignature* sig = &db.rule_signatures[(usz)selected_rule_index];
    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    DeduceObservedStepCounters[] observed_steps = {};
    if (sig.body_count > 0) {
        observed_steps = mem::new_array(DeduceObservedStepCounters, sig.body_count);
        if (observed_steps.ptr == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
    }
    defer if (observed_steps.ptr != null) mem::free(observed_steps.ptr);

    DeduceObservedRuleCounters observed_rule = {
        .steps = observed_steps.ptr,
        .step_count = sig.body_count,
    };
    Value* observed_err = deduce_collect_rule_observed_counters(db, sig, &observed_rule, interp);
    if (observed_err != null) return observed_err;

    Value* steps = deduce_explain_build_plan_steps(db, sig, &observed_rule, interp);
    if (steps != null && steps.tag == ERROR) return steps;
    Value* head_projections = deduce_explain_build_head_projections(sig, interp);
    if (head_projections != null && head_projections.tag == ERROR) return head_projections;

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-plan"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "surface-kind", interp.symbols.intern("planner-snapshot"));
    explain_dict_set_symbol(out, interp, "execution-engine", execution_engine_sym);
    explain_dict_set_symbol(out, interp, "requested-execution-engine", requested_execution_engine_sym);
    explain_dict_set(out, interp, "recursive-aggregate-naive-fallback",
        aggregate_recursive_fallback ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "naive-recursive-aggregate-mixed-routed-to-seminaive",
        naive_recursive_mixed_aggregate_routed ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "rule-index", make_int(interp, (long)selected_rule_index));
    explain_dict_set(out, interp, "rule-count", make_int(interp, (long)db.rule_signature_count));
    explain_dict_set(out, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(out, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(out, interp, "aggregate-projection-count", make_int(interp, (long)sig.aggregate_projection_count));
    explain_dict_set(out, interp, "head-projections", head_projections);
    explain_dict_set(out, interp, "steps", steps);
    return out;
}
