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

fn bool deduce_component_has_negated_body_atoms(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.body_negated == null) continue;
        for (usz step = 0; step < sig.body_count; step++) {
            if (sig.body_negated[step]) return true;
        }
    }
    return false;
}

fn Value* deduce_explain_goal_directed_blockers(
    bool recursive,
    bool has_aggregates,
    bool has_negated_body_atoms,
    Interp* interp
) {
    Value* rev = make_nil(interp);
    if (!recursive) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("non-recursive")), rev);
    }
    if (has_aggregates) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("aggregates-present")), rev);
    }
    if (has_negated_body_atoms) {
        rev = make_cons(interp, make_symbol(interp, interp.symbols.intern("negated-body-atom")), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_component_summaries(
    DeduceDb* db,
    DeduceSccPlan* plan,
    Interp* interp
) {
    if (db == null || plan == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        bool recursive =
            plan.component_recursive.ptr != null &&
            component_id < plan.component_recursive.len &&
            plan.component_recursive[component_id];
        if (!recursive) continue;

        bool has_aggregates = deduce_component_has_aggregate_rules(db, plan, component_id);
        bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, plan, component_id);
        bool eligible = !has_aggregates && !has_negated_body_atoms;
        long rule_count = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            rule_count++;
        }

        Value* entry = make_hashmap(interp, 16);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }

        explain_dict_set(entry, interp, "component-id", make_int(interp, (long)component_id));
        explain_dict_set(entry, interp, "stratum",
            make_int(interp, component_id < plan.component_strata.len ? (long)plan.component_strata[component_id] : 0));
        explain_dict_set(entry, interp, "rule-count", make_int(interp, rule_count));
        explain_dict_set(entry, interp, "goal-directed-eligible",
            eligible ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(entry, interp, "goal-directed-shape",
            eligible
                ? make_symbol(interp, interp.symbols.intern("positive-recursive-closure"))
                : make_nil(interp));
        explain_dict_set(entry, interp, "goal-directed-blockers",
            deduce_explain_goal_directed_blockers(
                recursive,
                has_aggregates,
                has_negated_body_atoms,
                interp
            ));
        explain_dict_set(entry, interp, "aggregate-rules-present",
            has_aggregates ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(entry, interp, "negated-body-atoms-present",
            has_negated_body_atoms ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_selected_components(
    DeduceSccPlan* plan,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || plan == null || interp == null) return make_nil(interp);

    bool[] selected_components = mem::new_array(bool, plan.component_count);
    if (selected_components.ptr == null && plan.component_count > 0) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_fixpoint_select_dependency_components(plan, component_id, selected_components)) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < selected_components.len; i++) {
        if (!selected_components[i]) continue;
        rev = make_cons(interp, make_int(interp, (long)i), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_selected_predicates(
    DeduceSccPlan* plan,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || plan == null || interp == null) return make_nil(interp);

    bool[] selected_components = mem::new_array(bool, plan.component_count);
    if (selected_components.ptr == null && plan.component_count > 0) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);

    if (!deduce_fixpoint_select_dependency_components(plan, component_id, selected_components)) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz predicate_index = 0; predicate_index < plan.predicates.len; predicate_index++) {
        if (predicate_index >= plan.predicate_component.len) continue;
        int predicate_component = plan.predicate_component[predicate_index];
        if (predicate_component < 0 || (usz)predicate_component >= selected_components.len) continue;
        if (!selected_components[(usz)predicate_component]) continue;
        rev = make_cons(interp, make_symbol(interp, plan.predicates[predicate_index]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_goal_directed_demand_bound_head_positions(
    DeduceDb* db,
    DeduceSccPlan* plan,
    DeduceRuleSignature* sig,
    usz component_id,
    bool eligible,
    Interp* interp
) {
    if (!eligible || db == null || plan == null || sig == null || interp == null) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (!deduce_component_position_can_use_relation_head_demand(
                db,
                plan,
                component_id,
                sig.head_predicate,
                i
            )) {
            continue;
        }
        rev = make_cons(interp, make_int(interp, (long)i), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_parallel_batch_components(
    DeduceSccPlan* plan,
    usz[] parallel_wave_ids,
    usz component_id,
    Interp* interp
) {
    if (plan == null || interp == null || component_id >= plan.component_count ||
        component_id >= plan.component_strata.len || component_id >= parallel_wave_ids.len) {
        return make_nil(interp);
    }

    usz stratum = plan.component_strata[component_id];
    usz wave = parallel_wave_ids[component_id];
    Value* rev = make_nil(interp);
    for (usz other = 0; other < plan.component_count; other++) {
        if (other >= plan.component_strata.len || other >= parallel_wave_ids.len) continue;
        if (plan.component_strata[other] != stratum) continue;
        if (parallel_wave_ids[other] != wave) continue;
        rev = make_cons(interp, make_int(interp, (long)other), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
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

fn void deduce_explain_set_last_goal_directed_read_fields(
    Value* out,
    DeduceDb* db,
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null || interp == null) return;

    DeduceRelationSchema* schema = null;
    if (db != null && sig != null) {
        schema = deduce_db_lookup_relation_schema(db, sig.head_predicate);
    }

    SymbolId execution_path = schema != null ? schema.last_goal_directed_read_execution_path : (SymbolId)0;
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        execution_path != (SymbolId)0
            ? make_symbol(interp, execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        schema != null && schema.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        schema != null && schema.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, schema.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        schema != null && schema.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, schema.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_requested_bound_positions,
                schema.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        execution_path != (SymbolId)0
            ? make_int(interp, (long)schema.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                schema.last_goal_directed_read_applied_bound_positions,
                schema.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-step-counters",
        execution_path != (SymbolId)0 && schema != null && schema.last_goal_directed_read_step_count > 0
            ? deduce_explain_observed_step_counter_payload(
                schema.last_goal_directed_read_step_counters,
                schema.last_goal_directed_read_step_count,
                interp
            )
            : make_nil(interp));
}

fn Value* deduce_explain_observed_step_counter_payload(
    DeduceObservedStepCounters* steps,
    usz step_count,
    Interp* interp
) {
    if (steps == null || step_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < step_count; i++) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
        explain_dict_set(entry, interp, "step-index", make_int(interp, (long)i));
        explain_dict_set(entry, interp, "rows-read", make_int(interp, steps[i].rows_read));
        explain_dict_set(entry, interp, "rows-emitted", make_int(interp, steps[i].rows_emitted));
        explain_dict_set(entry, interp, "index-hits", make_int(interp, steps[i].index_hits));
        explain_dict_set(entry, interp, "index-misses", make_int(interp, steps[i].index_misses));
        explain_dict_set(entry, interp, "join-probes", make_int(interp, steps[i].join_probes));
        explain_dict_set_symbol(entry, interp, "counter-kind", interp.symbols.intern("observed"));
        rev = make_cons(interp, entry, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
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
    Value* entry = make_hashmap(interp, 16);
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
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_err != null) return parallel_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

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
    bool goal_directed_recursive = false;
    bool goal_directed_has_aggregates = false;
    bool goal_directed_has_negated_body_atoms = false;
    isz goal_directed_component_id = -1;
    if (use_seminaive_recursive &&
        selected_rule_index >= 0 &&
        (usz)selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)selected_rule_index];
        bool recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        goal_directed_component_id = (isz)component_id;
        goal_directed_recursive = recursive;
        goal_directed_has_aggregates = deduce_component_has_aggregate_rules(db, &scc_plan, component_id);
        goal_directed_has_negated_body_atoms =
            deduce_component_has_negated_body_atoms(db, &scc_plan, component_id);
        if (recursive &&
            goal_directed_has_aggregates &&
            (!deduce_component_can_use_seminaive_aggregates(db, &scc_plan, component_id) ||
             !deduce_component_has_full_recursive_aggregate_seminaive_support(db, &scc_plan, component_id))) {
            aggregate_recursive_fallback = true;
            execution_engine_sym = interp.symbols.intern("naive-scc");
        }
    } else if (selected_rule_index >= 0 &&
               (usz)selected_rule_index < scc_plan.rule_component_ids.len) {
        usz component_id = (usz)scc_plan.rule_component_ids[(usz)selected_rule_index];
        goal_directed_component_id = (isz)component_id;
        goal_directed_recursive =
            component_id < scc_plan.component_recursive.len && scc_plan.component_recursive[component_id];
        goal_directed_has_aggregates = deduce_component_has_aggregate_rules(db, &scc_plan, component_id);
        goal_directed_has_negated_body_atoms =
            deduce_component_has_negated_body_atoms(db, &scc_plan, component_id);
    }
    bool goal_directed_eligible =
        goal_directed_recursive && !goal_directed_has_aggregates && !goal_directed_has_negated_body_atoms;
    Value* goal_directed_blockers = deduce_explain_goal_directed_blockers(
        goal_directed_recursive,
        goal_directed_has_aggregates,
        goal_directed_has_negated_body_atoms,
        interp
    );
    Value* goal_directed_selected_components = deduce_explain_goal_directed_selected_components(
        &scc_plan,
        goal_directed_component_id,
        goal_directed_eligible,
        interp
    );
    if (goal_directed_selected_components != null && goal_directed_selected_components.tag == ERROR) {
        return goal_directed_selected_components;
    }
    Value* goal_directed_selected_predicates = deduce_explain_goal_directed_selected_predicates(
        &scc_plan,
        goal_directed_component_id,
        goal_directed_eligible,
        interp
    );
    if (goal_directed_selected_predicates != null && goal_directed_selected_predicates.tag == ERROR) {
        return goal_directed_selected_predicates;
    }
    Value* parallel_batch_components =
        goal_directed_component_id >= 0
            ? deduce_explain_parallel_batch_components(
                &scc_plan,
                parallel_wave_ids,
                (usz)goal_directed_component_id,
                interp
            )
            : make_nil(interp);
    if (parallel_batch_components != null && parallel_batch_components.tag == ERROR) {
        return parallel_batch_components;
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
    Value* demand_bound_head_positions = deduce_explain_goal_directed_demand_bound_head_positions(
        db,
        &scc_plan,
        sig,
        goal_directed_component_id,
        goal_directed_eligible,
        interp
    );
    if (demand_bound_head_positions != null && demand_bound_head_positions.tag == ERROR) {
        return demand_bound_head_positions;
    }
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-plan"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "surface-kind", interp.symbols.intern("planner-snapshot"));
    explain_dict_set_symbol(out, interp, "execution-engine", execution_engine_sym);
    explain_dict_set_symbol(out, interp, "requested-execution-engine", requested_execution_engine_sym);
    explain_dict_set(out, interp, "recursive-aggregate-naive-fallback",
        aggregate_recursive_fallback ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "naive-recursive-aggregate-mixed-routed-to-seminaive",
        naive_recursive_mixed_aggregate_routed ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-component-id",
        goal_directed_component_id >= 0
            ? make_int(interp, (long)goal_directed_component_id)
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-eligible",
        goal_directed_eligible ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-shape",
        goal_directed_eligible
            ? make_symbol(interp, interp.symbols.intern("positive-recursive-closure"))
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-blockers", goal_directed_blockers);
    explain_dict_set(out, interp, "goal-directed-execution-path",
        goal_directed_eligible
            ? make_symbol(interp, interp.symbols.intern("selected-component-closure"))
            : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-selected-components", goal_directed_selected_components);
    explain_dict_set(out, interp, "goal-directed-selected-predicates", goal_directed_selected_predicates);
    explain_dict_set(out, interp, "goal-directed-demand-bound-head-positions", demand_bound_head_positions);
    deduce_explain_set_last_goal_directed_read_fields(out, db, sig, interp);
    explain_dict_set(out, interp, "parallel-wave",
        goal_directed_component_id >= 0 && (usz)goal_directed_component_id < parallel_wave_ids.len
            ? make_int(interp, (long)parallel_wave_ids[(usz)goal_directed_component_id])
            : make_nil(interp));
    explain_dict_set(out, interp, "parallel-batch-size",
        goal_directed_component_id >= 0 && (usz)goal_directed_component_id < parallel_batch_sizes.len
            ? make_int(interp, (long)parallel_batch_sizes[(usz)goal_directed_component_id])
            : make_nil(interp));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-components", parallel_batch_components);
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            &scc_plan,
            parallel_wave_ids[..],
            parallel_batch_sizes[..],
            interp
        ));
    explain_dict_set(out, interp, "rule-index", make_int(interp, (long)selected_rule_index));
    explain_dict_set(out, interp, "rule-count", make_int(interp, (long)db.rule_signature_count));
    explain_dict_set(out, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(out, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(out, interp, "aggregate-projection-count", make_int(interp, (long)sig.aggregate_projection_count));
    explain_dict_set(out, interp, "head-projections", head_projections);
    explain_dict_set(out, interp, "steps", steps);
    return out;
}
