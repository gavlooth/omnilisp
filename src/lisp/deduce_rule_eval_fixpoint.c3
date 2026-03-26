module lisp;

import std::core::mem;

struct DeduceGoalDirectedSelectorSurface {
    char[] out_of_memory_code;
    char[] out_of_memory_message;
    char[] invalid_selector_code;
    char[] invalid_selector_message;
    char[] invalid_engine_code;
    char[] invalid_engine_message;
    char[] rule_index_out_of_range_code;
    char[] rule_index_out_of_range_message;
    char[] invalid_state_code;
    char[] invalid_state_message;
    char[] full_recompute_code;
    char[] full_recompute_message;
    char[] selector_not_eligible_code;
    char[] selector_not_eligible_message;
    char[] relation_mismatch_code;
    char[] relation_mismatch_message;
}

struct DeduceSelectedSchemaEstimateSnapshot {
    DeduceRelationSchema* schema;
    ulong                 cardinality_estimate;
    ulong                 distinct_estimate;
}

fn Value* deduce_goal_directed_selector_surface_error(
    char[] code,
    char[] message,
    long selector_rule_index,
    usz component_id,
    Value* blockers,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index));
    explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id));
    explain_dict_set(data, interp, "goal-directed-blockers",
        blockers == null ? make_nil(interp) : blockers);
    return deduce_raise(interp, code, message, data);
}

fn Value* deduce_goal_directed_relation_surface_error(
    char[] code,
    char[] message,
    long selector_rule_index,
    usz component_id,
    SymbolId relation_name,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index));
    explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id));
    explain_dict_set(data, interp, "relation",
        relation_name != (SymbolId)0 ? make_symbol(interp, relation_name) : make_nil(interp));
    return deduce_raise(interp, code, message, data);
}

fn Value* deduce_parse_goal_directed_selector_and_engine(
    Value*[] args,
    usz required_arg_count,
    DeduceGoalDirectedSelectorSurface* surface,
    long* selector_rule_index_out,
    bool* use_seminaive_recursive_out,
    Interp* interp
) {
    if (selector_rule_index_out == null || use_seminaive_recursive_out == null || surface == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid selector parse state");
    }

    *selector_rule_index_out = -1;
    *use_seminaive_recursive_out = true;
    if (args.len <= required_arg_count) return null;

    Value* selector_arg = args[required_arg_count];
    if (selector_arg == null || selector_arg.tag != INT) {
        return deduce_raise(interp, surface.invalid_selector_code, surface.invalid_selector_message);
    }
    *selector_rule_index_out = selector_arg.int_val;

    if (args.len == required_arg_count + 1) return null;

    Value* engine_arg = args[required_arg_count + 1];
    if (engine_arg == null || engine_arg.tag != SYMBOL) {
        return deduce_raise(interp, surface.invalid_engine_code, surface.invalid_engine_message);
    }
    char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
    if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
        *use_seminaive_recursive_out = false;
        return null;
    }
    if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
        *use_seminaive_recursive_out = true;
        return null;
    }
    return deduce_raise(interp, surface.invalid_engine_code, surface.invalid_engine_message);
}

fn Value* deduce_allocate_selected_rule_observed_counters(
    DeduceDb* db,
    long selector_rule_index,
    DeduceObservedRuleCounters[]* out_observed_rules,
    Interp* interp
) {
    if (out_observed_rules == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid observed counter state");
    }
    *out_observed_rules = {};
    if (db == null || selector_rule_index < 0 || (ulong)selector_rule_index >= db.rule_signature_count) {
        return null;
    }

    DeduceObservedRuleCounters[] observed_rules =
        mem::new_array(DeduceObservedRuleCounters, db.rule_signature_count);
    if (observed_rules.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }
    for (usz i = 0; i < observed_rules.len; i++) observed_rules[i] = {};

    usz step_count = db.rule_signatures[(usz)selector_rule_index].body_count;
    if (step_count > 0) {
        DeduceObservedStepCounters[] steps = mem::new_array(DeduceObservedStepCounters, step_count);
        if (steps.ptr == null) {
            mem::free(observed_rules.ptr);
            return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
        }
        observed_rules[(usz)selector_rule_index].steps = steps.ptr;
        observed_rules[(usz)selector_rule_index].step_count = step_count;
        deduce_observed_rule_counters_reset(&observed_rules[(usz)selector_rule_index]);
    }

    *out_observed_rules = observed_rules;
    return null;
}

fn void deduce_free_selected_rule_observed_counters(
    DeduceObservedRuleCounters[] observed_rules
) {
    if (observed_rules.ptr == null) return;
    for (usz i = 0; i < observed_rules.len; i++) {
        if (observed_rules[i].steps != null) {
            mem::free(observed_rules[i].steps);
            observed_rules[i].steps = null;
        }
    }
    mem::free(observed_rules.ptr);
}

fn Value* deduce_prepare_goal_directed_relation_selector_plan(
    DeduceDb* db,
    SymbolId relation_name,
    long selector_rule_index,
    DeduceGoalDirectedSelectorSurface* surface,
    DeduceSccPlan* out_plan,
    bool[]* out_selected_components,
    usz* out_selected_component_id,
    Interp* interp
) {
    if (out_plan == null || out_selected_components == null || out_selected_component_id == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed selector state");
    }
    *out_plan = {};
    *out_selected_components = {};
    *out_selected_component_id = 0;

    if (selector_rule_index < 0) return null;
    if (db == null || surface == null || interp == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed execution state");
    }
    if (db.incremental_full_recompute_required) {
        return deduce_goal_directed_relation_surface_error(
            surface.full_recompute_code,
            surface.full_recompute_message,
            selector_rule_index,
            0,
            relation_name,
            interp
        );
    }

    Value* plan_err = deduce_build_scc_plan(db, out_plan, interp);
    if (plan_err != null) return plan_err;

    if ((ulong)selector_rule_index >= db.rule_signature_count) {
        return deduce_raise(interp, surface.rule_index_out_of_range_code, surface.rule_index_out_of_range_message);
    }
    if (out_plan.rule_component_ids.ptr == null || (ulong)selector_rule_index >= out_plan.rule_component_ids.len) {
        return deduce_raise(interp, surface.invalid_state_code, surface.invalid_state_message);
    }

    usz selected_component_id = out_plan.rule_component_ids[(usz)selector_rule_index];
    bool recursive =
        out_plan.component_recursive.ptr != null &&
        selected_component_id < out_plan.component_recursive.len &&
        out_plan.component_recursive[selected_component_id];
    bool has_aggregates = deduce_component_has_aggregate_rules(db, out_plan, selected_component_id);
    bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, out_plan, selected_component_id);
    if (!recursive || has_aggregates || has_negated_body_atoms) {
        Value* blockers = deduce_explain_goal_directed_blockers(
            recursive,
            has_aggregates,
            has_negated_body_atoms,
            interp
        );
        return deduce_goal_directed_selector_surface_error(
            surface.selector_not_eligible_code,
            surface.selector_not_eligible_message,
            selector_rule_index,
            selected_component_id,
            blockers,
            interp
        );
    }

    bool[] selected_components = mem::new_array(bool, out_plan.component_count);
    if (selected_components.ptr == null && out_plan.component_count > 0) {
        return deduce_raise(interp, surface.out_of_memory_code, surface.out_of_memory_message);
    }

    if (!deduce_fixpoint_select_dependency_components(out_plan, selected_component_id, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_raise(interp, surface.invalid_state_code, surface.invalid_state_message);
    }
    if (!deduce_fixpoint_predicate_in_selected_components(out_plan, selected_components, relation_name)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_goal_directed_relation_surface_error(
            surface.relation_mismatch_code,
            surface.relation_mismatch_message,
            selector_rule_index,
            selected_component_id,
            relation_name,
            interp
        );
    }

    *out_selected_components = selected_components;
    *out_selected_component_id = selected_component_id;
    return null;
}

fn Value* deduce_execute_goal_directed_relation_selector(
    DeduceDb* db,
    SymbolId relation_name,
    long selector_rule_index,
    bool use_seminaive_recursive,
    SymbolId read_surface,
    DeduceGoalDirectedSelectorSurface* surface,
    Interp* interp
) {
    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz selected_component_id = 0;
    Value* prepare_err = deduce_prepare_goal_directed_relation_selector_plan(
        db,
        relation_name,
        selector_rule_index,
        surface,
        &scc_plan,
        &selected_components,
        &selected_component_id,
        interp
    );
    if (prepare_err != null) return prepare_err;

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
        db,
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
    deduce_fixpoint_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    deduce_db_note_goal_directed_read(
        db,
        relation_name,
        interp.symbols.intern("selected-component-closure"),
        read_surface,
        selector_rule_index,
        null,
        null,
        null,
        interp.symbols.intern("dirty-closure")
    );
    return null;
}

fn Value* deduce_fixpoint_snapshot_selected_schema_estimates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    DeduceSelectedSchemaEstimateSnapshot[]* out_snapshots,
    Interp* interp
) {
    if (out_snapshots == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid schema snapshot state");
    }
    *out_snapshots = {};
    if (db == null || plan == null) return null;

    usz snapshot_count = 0;
    for (usz i = 0; i < plan.predicates.len; i++) {
        SymbolId predicate = plan.predicates[i];
        if (!deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        if (deduce_db_lookup_relation_schema(db, predicate) != null) snapshot_count++;
    }
    if (snapshot_count == 0) return null;

    DeduceSelectedSchemaEstimateSnapshot[] snapshots =
        mem::new_array(DeduceSelectedSchemaEstimateSnapshot, snapshot_count);
    if (snapshots.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    usz write_index = 0;
    for (usz i = 0; i < plan.predicates.len; i++) {
        SymbolId predicate = plan.predicates[i];
        if (!deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) continue;

        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
        if (schema == null) continue;

        snapshots[write_index++] = {
            .schema = schema,
            .cardinality_estimate = schema.cardinality_estimate,
            .distinct_estimate = schema.distinct_estimate,
        };
    }
    *out_snapshots = snapshots;
    return null;
}

fn void deduce_fixpoint_restore_selected_schema_estimates(
    DeduceSelectedSchemaEstimateSnapshot[] snapshots
) {
    for (usz i = 0; i < snapshots.len; i++) {
        DeduceSelectedSchemaEstimateSnapshot snapshot = snapshots[i];
        if (snapshot.schema == null) continue;
        snapshot.schema.cardinality_estimate = snapshot.cardinality_estimate;
        snapshot.schema.distinct_estimate = snapshot.distinct_estimate;
    }
}

fn bool deduce_fixpoint_selected_components_have_dirty_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) {
        return false;
    }

    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        if (deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) {
            return true;
        }
    }
    return false;
}

fn Value* deduce_prepare_goal_directed_target_relation_plan_if_dirty(
    DeduceDb* db,
    SymbolId relation_name,
    DeduceSccPlan* out_plan,
    bool[]* out_selected_components,
    usz* out_selected_component_id,
    bool* out_ready,
    Interp* interp
) {
    if (out_plan == null || out_selected_components == null || out_selected_component_id == null || out_ready == null) {
        return deduce_raise(interp, "deduce/invalid-state", "deduce: invalid goal-directed target relation state");
    }
    *out_plan = {};
    *out_selected_components = {};
    *out_selected_component_id = 0;
    *out_ready = false;

    if (db == null || relation_name == (SymbolId)0 || interp == null) return null;
    if (db.incremental_full_recompute_required || db.incremental_dirty_count == 0) return null;

    Value* plan_err = deduce_build_scc_plan(db, out_plan, interp);
    if (plan_err != null) return plan_err;

    int predicate_index = deduce_scc_find_predicate_index(out_plan.predicates, relation_name);
    if (predicate_index < 0 || (usz)predicate_index >= out_plan.predicate_component.len) return null;

    int component_id = out_plan.predicate_component[(usz)predicate_index];
    if (component_id < 0 || (usz)component_id >= out_plan.component_count) return null;

    bool recursive =
        out_plan.component_recursive.ptr != null &&
        (usz)component_id < out_plan.component_recursive.len &&
        out_plan.component_recursive[(usz)component_id];
    if (!recursive) return null;

    bool has_aggregates = deduce_component_has_aggregate_rules(db, out_plan, (usz)component_id);
    bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, out_plan, (usz)component_id);
    if (has_aggregates || has_negated_body_atoms) return null;

    bool[] selected_components = mem::new_array(bool, out_plan.component_count);
    if (selected_components.ptr == null && out_plan.component_count > 0) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory");
    }

    if (!deduce_fixpoint_select_dependency_components(out_plan, (usz)component_id, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return deduce_raise(interp, "deduce/invalid-state",
            "deduce: failed to select automatic goal-directed component closure");
    }
    if (!deduce_fixpoint_selected_components_have_dirty_predicates(db, out_plan, selected_components)) {
        if (selected_components.ptr != null) mem::free(selected_components.ptr);
        return null;
    }

    *out_selected_components = selected_components;
    *out_selected_component_id = (usz)component_id;
    *out_ready = true;
    return null;
}

fn Value* deduce_auto_execute_goal_directed_relation_if_dirty(
    DeduceDb* db,
    SymbolId relation_name,
    bool use_seminaive_recursive,
    bool* used_out,
    Interp* interp
) {
    if (used_out != null) *used_out = false;
    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    usz component_id = 0;
    bool ready = false;
    Value* prepare_err = deduce_prepare_goal_directed_target_relation_plan_if_dirty(
        db,
        relation_name,
        &scc_plan,
        &selected_components,
        &component_id,
        &ready,
        interp
    );
    if (prepare_err != null) return prepare_err;
    if (!ready) return null;

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_selected_components_with_plan(
        db,
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
    deduce_fixpoint_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
    if (used_out != null) *used_out = true;
    return null;
}

fn Value* deduce_prepare_plain_relation_read(
    Value* rel_handle,
    Relation* rel,
    bool use_seminaive_recursive,
    SymbolId read_surface,
    Env* env,
    Interp* interp
) {
    if (rel == null || rel.db == null || interp == null) return null;

    bool on_read_handled = false;
    Value* on_read_err = deduce_relation_maybe_refresh_on_read(
        rel_handle,
        rel,
        env,
        read_surface,
        interp,
        &on_read_handled
    );
    if (on_read_err != null) return on_read_err;
    if (on_read_handled) return null;

    DeduceDb* db = rel.db;
    SymbolId relation_name = rel.name;

    if (!db.incremental_full_recompute_required) {
        bool used_selected_closure = false;
        Value* exec_err = deduce_auto_execute_goal_directed_relation_if_dirty(
            db,
            relation_name,
            use_seminaive_recursive,
            &used_selected_closure,
            interp
        );
        if (exec_err != null) return exec_err;
        deduce_db_note_goal_directed_read(
            db,
            relation_name,
            interp.symbols.intern(used_selected_closure ? "selected-component-closure" : "no-op"),
            read_surface,
            -1,
            null,
            null,
            null,
            used_selected_closure ? interp.symbols.intern("dirty-closure") : (SymbolId)0
        );
        return null;
    }

    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, relation_name);
    if (schema == null || !deduce_relation_schema_has_derived_rules(db, schema)) return null;

    DeduceSccPlan scc_plan = {};
    defer deduce_scc_plan_free(&scc_plan);
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;

    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* exec_err = deduce_naive_fixpoint_evaluate_with_plan(
        db,
        &scc_plan,
        256,
        use_seminaive_recursive,
        &iterations,
        &derived,
        &max_component_iterations,
        {},
        interp
    );
    if (exec_err != null) return exec_err;
    deduce_db_reset_incremental_tracking(db);
    deduce_db_note_goal_directed_read(
        db,
        relation_name,
        interp.symbols.intern("full-db-fixpoint"),
        read_surface,
        -1,
        null,
        null,
        null,
        interp.symbols.intern("full-recompute-required")
    );
    return null;
}

fn bool deduce_fixpoint_select_dependency_components(
    DeduceSccPlan* plan,
    usz target_component_id,
    bool[] selected_components
) {
    if (plan == null || target_component_id >= plan.component_count) return false;
    if (selected_components.len < plan.component_count) return false;

    for (usz i = 0; i < selected_components.len; i++) selected_components[i] = false;
    selected_components[target_component_id] = true;

    bool changed = true;
    while (changed) {
        changed = false;
        foreach (edge : plan.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= plan.predicate_component.len || (usz)edge.to >= plan.predicate_component.len) continue;

            int from_component = plan.predicate_component[(usz)edge.from];
            int to_component = plan.predicate_component[(usz)edge.to];
            if (from_component < 0 || to_component < 0 || from_component == to_component) continue;
            if ((usz)from_component >= selected_components.len || (usz)to_component >= selected_components.len) continue;
            if (!selected_components[(usz)from_component] || selected_components[(usz)to_component]) continue;

            selected_components[(usz)to_component] = true;
            changed = true;
        }
    }
    return true;
}

fn bool deduce_fixpoint_predicate_in_selected_components(
    DeduceSccPlan* plan,
    bool[] selected_components,
    SymbolId predicate
) {
    if (plan == null || predicate == (SymbolId)0) return false;
    int predicate_index = deduce_scc_find_predicate_index(plan.predicates, predicate);
    if (predicate_index < 0 || (usz)predicate_index >= plan.predicate_component.len) return false;

    int component_id = plan.predicate_component[(usz)predicate_index];
    if (component_id < 0 || (usz)component_id >= selected_components.len) return false;
    return selected_components[(usz)component_id];
}

fn void deduce_fixpoint_clear_selected_dirty_predicates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components
) {
    if (db == null || plan == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) return;

    usz write_index = 0;
    for (usz read_index = 0; read_index < db.incremental_dirty_count; read_index++) {
        SymbolId predicate = db.incremental_dirty_predicates[read_index];
        if (deduce_fixpoint_predicate_in_selected_components(plan, selected_components, predicate)) continue;
        db.incremental_dirty_predicates[write_index++] = predicate;
    }
    db.incremental_dirty_count = write_index;
}

fn bool deduce_component_has_aggregate_rules(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        if (deduce_rule_signature_has_aggregates(&db.rule_signatures[i])) return true;
    }
    return false;
}

fn bool deduce_rule_body_atom_preserves_head_demand(
    DeduceRuleSignature* sig,
    usz body_index,
    DeduceRuleHeadDemand* head_demand
) {
    if (sig == null || head_demand == null || body_index >= sig.body_count) return false;
    if (head_demand.term_bound == null) return true;

    for (usz i = 0; i < head_demand.term_count; i++) {
        if (!head_demand.term_bound[i]) continue;
        if (!deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, head_demand.predicate, i)) {
            return false;
        }
    }
    return true;
}

fn bool deduce_rule_body_atom_preserves_head_demand_position(
    DeduceRuleSignature* sig,
    usz body_index,
    SymbolId predicate,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (sig.body_predicates == null || sig.body_predicates[body_index] != predicate) return false;
    if (head_term_index >= sig.head_term_count) return false;

    usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
    usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
    if (body_count < sig.head_term_count) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds != null
        ? sig.head_term_kinds[head_term_index]
        : DEDUCE_RULE_TERM_LITERAL;
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE: {
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            uint head_var_id = sig.head_term_var_ids[head_term_index];
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] == head_var_id) return true;
            }
            return false;
        }
        case DEDUCE_RULE_TERM_LITERAL:
            // Head literals already constrain the produced relation rows. When a demand
            // binds a literal head position, no additional body-position preservation is
            // required to keep the recursive demand sound.
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}

fn bool deduce_rule_body_atom_preserves_head_demand_position_at_same_index(
    DeduceRuleSignature* sig,
    usz body_index,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (head_term_index >= sig.head_term_count) return false;

    usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
    usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
    if (head_term_index >= body_count) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds != null
        ? sig.head_term_kinds[head_term_index]
        : DEDUCE_RULE_TERM_LITERAL;
    usz body_term_index = body_offset + head_term_index;
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE:
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            return sig.body_term_is_variable[body_term_index] &&
                sig.body_term_var_ids[body_term_index] == sig.head_term_var_ids[head_term_index];
        case DEDUCE_RULE_TERM_LITERAL:
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}

fn bool deduce_rule_body_atom_preserves_head_demand_at_same_indices(
    DeduceRuleSignature* sig,
    usz body_index,
    DeduceRuleHeadDemand* head_demand
) {
    if (sig == null || head_demand == null || head_demand.term_bound == null) return false;

    bool has_bound_positions = false;
    for (usz head_index = 0; head_index < head_demand.term_count; head_index++) {
        if (!head_demand.term_bound[head_index]) continue;
        has_bound_positions = true;
        if (!deduce_rule_body_atom_preserves_head_demand_position_at_same_index(sig, body_index, head_index)) {
            return false;
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_contains_head_predicate(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        if (db.rule_signatures[i].head_predicate == predicate) return true;
    }
    return false;
}

fn bool deduce_component_position_has_same_index_recursive_carrier(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;
            if (!deduce_component_contains_head_predicate(
                    db,
                    plan,
                    component_id,
                    sig.body_predicates[body_index]
                )) {
                continue;
            }
            if (deduce_rule_body_atom_preserves_head_demand_position_at_same_index(sig, body_index, head_term_index)) {
                return true;
            }
        }
    }
    return false;
}

fn bool deduce_rule_body_atom_supports_query_head_demand_position(
    DeduceRuleSignature* sig,
    usz body_index,
    SymbolId predicate,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (head_term_index >= sig.head_term_count) return false;
    if (sig.body_predicates == null) return false;

    SymbolId body_predicate = sig.body_predicates[body_index];
    if (sig.head_term_kinds == null) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds[head_term_index];
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE: {
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            uint head_var_id = sig.head_term_var_ids[head_term_index];
            bool saw_head_var = false;
            usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
            usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] != head_var_id) continue;
                saw_head_var = true;
                if (body_predicate == predicate) {
                    return deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, predicate, head_term_index);
                }
                break;
            }
            return saw_head_var;
        }
        case DEDUCE_RULE_TERM_LITERAL:
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}

fn bool deduce_component_position_can_use_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    bool matched_head_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;
        matched_head_rule = true;
        bool saw_positive_self_recursive = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_predicates == null || sig.body_predicates[body_index] != predicate) continue;
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            saw_positive_self_recursive = true;
            if (!deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, predicate, head_term_index)) {
                return false;
            }
        }
    }
    return matched_head_rule;
}

fn bool deduce_component_position_can_use_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    bool matched_head_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;
        matched_head_rule = true;
        bool saw_any_positive_mention = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;
            if (sig.body_predicates[body_index] == predicate) {
                if (!deduce_rule_body_atom_supports_query_head_demand_position(sig, body_index, predicate, head_term_index)) {
                    return false;
                }
                if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) continue;
                usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
                usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
                uint head_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[head_term_index] : 0;
                for (usz j = 0; j < body_count; j++) {
                    usz body_term_index = body_offset + j;
                    if (!sig.body_term_is_variable[body_term_index]) continue;
                    if (sig.body_term_var_ids[body_term_index] == head_var_id) {
                        saw_any_positive_mention = true;
                        break;
                    }
                }
                continue;
            }
            if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) continue;
            usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
            usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
            uint head_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[head_term_index] : 0;
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] == head_var_id) {
                    saw_any_positive_mention = true;
                    break;
                }
            }
        }
        if (saw_any_positive_mention) return true;
    }
    return false;
}

fn bool deduce_component_can_use_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            if (!deduce_component_position_can_use_relation_head_demand(
                    db,
                    plan,
                    component_id,
                    head_demand.predicate,
                    i
                )) {
                return false;
            }
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_can_use_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            if (!deduce_component_position_can_use_relation_head_demand_for_query(
                    db,
                    plan,
                    component_id,
                    head_demand.predicate,
                    i
                )) {
                return false;
            }
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_can_use_relation_head_demand_at_same_indices(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            break;
        }
    }
    if (!has_bound_positions) return false;

    bool saw_positive_recursive_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];

        bool rule_has_positive_recursive_body = false;
        bool rule_preserves_same_indices = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;

            SymbolId body_predicate = sig.body_predicates[body_index];
            bool body_is_recursive_in_component = false;
            for (usz j = 0; j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id) continue;
                if (db.rule_signatures[j].head_predicate != body_predicate) continue;
                body_is_recursive_in_component = true;
                break;
            }
            if (!body_is_recursive_in_component) continue;

            rule_has_positive_recursive_body = true;
            if (deduce_rule_body_atom_preserves_head_demand_at_same_indices(sig, body_index, head_demand)) {
                rule_preserves_same_indices = true;
                break;
            }
        }
        if (!rule_has_positive_recursive_body) continue;
        saw_positive_recursive_rule = true;
        if (!rule_preserves_same_indices) return false;
    }
    return saw_positive_recursive_rule;
}

fn bool deduce_component_head_rule_has_multiple_recursive_body_atoms(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate
) {
    if (db == null || plan == null) return false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

        usz recursive_body_count = 0;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;
            if (!deduce_component_contains_head_predicate(
                    db,
                    plan,
                    component_id,
                    sig.body_predicates[body_index]
                )) {
                continue;
            }
            recursive_body_count++;
            if (recursive_body_count > 1) return true;
        }
    }
    return false;
}

fn Value* deduce_component_project_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* requested_head_demand,
    DeduceRuleHeadDemand* out_projected_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* out_has_any_bound,
    Interp* interp
) {
    if (out_projected_head_demand == null || out_term_bound == null || out_term_values == null ||
        out_has_any_bound == null) {
        return deduce_raise(interp, "deduce/goal-directed-invalid-state", "deduce: invalid projected head demand state");
    }

    *out_projected_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *out_has_any_bound = false;

    if (db == null || plan == null || requested_head_demand == null) return null;
    if (requested_head_demand.term_count == 0 || requested_head_demand.term_bound == null) return null;

    bool[] term_bound = mem::new_array(bool, requested_head_demand.term_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }
    Value*[] term_values = mem::new_array(Value*, requested_head_demand.term_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < requested_head_demand.term_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;

        if (!requested_head_demand.term_bound[i]) continue;
        if (!deduce_component_position_can_use_relation_head_demand(
                db,
                plan,
                component_id,
                requested_head_demand.predicate,
                i
            )) {
            continue;
        }
        term_bound[i] = true;
        term_values[i] = requested_head_demand.term_values != null ? requested_head_demand.term_values[i] : null;
        has_any_bound = true;
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_has_any_bound = true;
    *out_projected_head_demand = {
        .predicate = requested_head_demand.predicate,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = requested_head_demand.term_count,
    };
    return null;
}

fn Value* deduce_component_project_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* requested_head_demand,
    DeduceRuleHeadDemand* out_projected_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* out_has_any_bound,
    Interp* interp
) {
    if (requested_head_demand == null || requested_head_demand.term_bound == null) {
        return deduce_component_project_relation_head_demand(
            db,
            plan,
            component_id,
            requested_head_demand,
            out_projected_head_demand,
            out_term_bound,
            out_term_values,
            out_has_any_bound,
            interp
        );
    }

    if (out_projected_head_demand == null || out_term_bound == null || out_term_values == null ||
        out_has_any_bound == null) {
        return deduce_raise(interp, "deduce/goal-directed-invalid-state", "deduce: invalid projected head demand state");
    }

    *out_projected_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *out_has_any_bound = false;

    if (db == null || plan == null || requested_head_demand == null) return null;
    if (requested_head_demand.term_count == 0 || requested_head_demand.term_bound == null) return null;

    bool[] term_bound = mem::new_array(bool, requested_head_demand.term_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }
    Value*[] term_values = mem::new_array(Value*, requested_head_demand.term_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < requested_head_demand.term_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;

        if (!requested_head_demand.term_bound[i]) continue;
        if (!deduce_component_position_can_use_relation_head_demand_for_query(
                db,
                plan,
                component_id,
                requested_head_demand.predicate,
                i
            )) {
            continue;
        }
        term_bound[i] = true;
        term_values[i] = requested_head_demand.term_values != null ? requested_head_demand.term_values[i] : null;
        has_any_bound = true;
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_has_any_bound = true;
    *out_projected_head_demand = {
        .predicate = requested_head_demand.predicate,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = requested_head_demand.term_count,
    };
    return null;
}

fn bool deduce_component_has_mixed_aggregate_rules(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null) return false;
    bool has_aggregate_rule = false;
    bool has_nonaggregate_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        if (deduce_rule_signature_has_aggregates(&db.rule_signatures[i])) {
            has_aggregate_rule = true;
        } else {
            has_nonaggregate_rule = true;
        }
        if (has_aggregate_rule && has_nonaggregate_rule) return true;
    }
    return false;
}

fn bool deduce_plan_has_recursive_aggregate_components(DeduceDb* db, DeduceSccPlan* plan) {
    if (db == null || plan == null || plan.component_recursive.ptr == null) return false;
    for (usz component_id = 0; component_id < plan.component_recursive.len; component_id++) {
        if (!plan.component_recursive[component_id]) continue;
        if (deduce_component_has_aggregate_rules(db, plan, component_id)) return true;
    }
    return false;
}

fn bool deduce_plan_has_recursive_mixed_aggregate_components(DeduceDb* db, DeduceSccPlan* plan) {
    if (db == null || plan == null || plan.component_recursive.ptr == null) return false;
    for (usz component_id = 0; component_id < plan.component_recursive.len; component_id++) {
        if (!plan.component_recursive[component_id]) continue;
        if (deduce_component_has_mixed_aggregate_rules(db, plan, component_id)) return true;
    }
    return false;
}

fn bool deduce_component_can_use_seminaive_aggregates(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null || component_id >= plan.component_count) return false;
    bool has_rules = false;
    bool has_aggregate_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        has_rules = true;
        if (deduce_rule_signature_has_aggregates(sig)) has_aggregate_rule = true;
    }
    return has_rules && has_aggregate_rule;
}

fn bool deduce_component_has_full_recursive_aggregate_seminaive_support(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null || component_id >= plan.component_count) return false;
    // Stage B5.7 now enables all currently valid recursive aggregate SCCs on
    // the seminaive signed-delta/support-accounting substrate. Stratification
    // already rejects negated recursion cycles, so negated body atoms that
    // survive into one of these components only target lower-stratum
    // relations and can keep using the normal existence-check path.
    return deduce_component_can_use_seminaive_aggregates(db, plan, component_id);
}

fn bool deduce_plan_has_recursive_aggregate_fallback_components(DeduceDb* db, DeduceSccPlan* plan) {
    if (db == null || plan == null || plan.component_recursive.ptr == null) return false;
    for (usz component_id = 0; component_id < plan.component_recursive.len; component_id++) {
        if (!plan.component_recursive[component_id]) continue;
        if (!deduce_component_has_aggregate_rules(db, plan, component_id)) continue;
        if (!deduce_component_can_use_seminaive_aggregates(db, plan, component_id) ||
            !deduce_component_has_full_recursive_aggregate_seminaive_support(db, plan, component_id)) {
            return true;
        }
    }
    return false;
}

fn bool deduce_aggregate_signatures_share_head_batch(
    DeduceRuleSignature* left,
    DeduceRuleSignature* right
) {
    if (left == null || right == null) return false;
    if (!deduce_rule_signature_has_aggregates(left) || !deduce_rule_signature_has_aggregates(right)) return false;
    if (left.head_predicate != right.head_predicate || left.head_term_count != right.head_term_count) return false;
    if (left.head_term_kinds == null || right.head_term_kinds == null ||
        left.head_term_aggregate_ops == null || right.head_term_aggregate_ops == null ||
        left.head_term_literals == null || right.head_term_literals == null) {
        return false;
    }

    for (usz i = 0; i < left.head_term_count; i++) {
        if (left.head_term_kinds[i] != right.head_term_kinds[i]) return false;
        switch (left.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_VARIABLE:
                break;
            case DEDUCE_RULE_TERM_LITERAL:
                if (!values_equal(left.head_term_literals[i], right.head_term_literals[i])) return false;
                break;
            case DEDUCE_RULE_TERM_AGGREGATE:
                if (left.head_term_aggregate_ops[i] != right.head_term_aggregate_ops[i]) return false;
                break;
        }
    }
    return true;
}

fn Value* deduce_naive_evaluate_component_pass_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    bool* changed_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool changed = false;
    long derived = 0;
    bool[] processed = mem::new_array(bool, db.rule_signature_count);
    if (processed.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (processed.ptr != null) mem::free(processed.ptr);
    for (usz i = 0; i < processed.len; i++) processed[i] = false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id || processed[i]) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        DeduceObservedRuleCounters* observed_rule =
            observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
        Value* err = null;
        if (!deduce_rule_signature_has_aggregates(sig)) {
            err = deduce_naive_evaluate_rule(
                db,
                sig,
                txn,
                head_demand,
                &derived,
                &changed,
                observed_rule,
                false,
                interp
            );
            processed[i] = true;
        } else {
            DeduceAggregateGroups aggregate_groups = {};
            defer deduce_aggregate_groups_release(&aggregate_groups);

            err = deduce_naive_evaluate_rule_with_groups(
                db,
                sig,
                txn,
                head_demand,
                &aggregate_groups,
                false,
                &derived,
                &changed,
                observed_rule,
                false,
                interp
            );
            processed[i] = true;

            for (usz j = i + 1; err == null && j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id || processed[j]) continue;
                DeduceRuleSignature* other = &db.rule_signatures[j];
                if (!deduce_aggregate_signatures_share_head_batch(sig, other)) continue;
                DeduceObservedRuleCounters* other_observed =
                    observed_rules.ptr != null && j < observed_rules.len ? &observed_rules[j] : null;
                err = deduce_naive_evaluate_rule_with_groups(
                    db,
                    other,
                    txn,
                    head_demand,
                    &aggregate_groups,
                    false,
                    &derived,
                    &changed,
                    other_observed,
                    false,
                    interp
                );
                processed[j] = true;
            }

            if (err == null) {
                err = deduce_naive_finalize_aggregate_groups_for_signature(
                    db,
                    sig,
                    txn,
                    &aggregate_groups,
                    &derived,
                    &changed,
                    interp
                );
            }
        }
        if (err != null) return err;
    }

    if (changed_out != null) *changed_out = changed;
    if (derived_out != null) *derived_out = derived;
    return null;
}

fn Value* deduce_naive_evaluate_component_pass(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    bool* changed_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    bool changed = false;
    long derived = 0;
    Value* err = deduce_naive_evaluate_component_pass_in_txn(
        db,
        plan,
        component_id,
        txn,
        null,
        &changed,
        &derived,
        observed_rules,
        interp
    );
    if (err == null) {
        err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
    }
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }

    if (changed_out != null) *changed_out = changed;
    if (derived_out != null) *derived_out = derived;
    return null;
}

fn Value* deduce_seminaive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    Value* err = deduce_seminaive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        null,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }
    return null;
}

fn Value* deduce_seminaive_evaluate_recursive_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    if (plan.predicates.len == 0) return null;

    DeduceSignedDeltaSet[] current_delta_sets = mem::new_array(DeduceSignedDeltaSet, plan.predicates.len);
    if (current_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (current_delta_sets.ptr != null) {
            for (usz i = 0; i < current_delta_sets.len; i++) deduce_signed_delta_set_release(&current_delta_sets[i]);
            mem::free(current_delta_sets.ptr);
        }
    }

    DeduceSignedDeltaSet[] next_delta_sets = mem::new_array(DeduceSignedDeltaSet, plan.predicates.len);
    if (next_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (next_delta_sets.ptr != null) {
            for (usz i = 0; i < next_delta_sets.len; i++) deduce_signed_delta_set_release(&next_delta_sets[i]);
            mem::free(next_delta_sets.ptr);
        }
    }

    for (usz i = 0; i < plan.predicates.len; i++) {
        current_delta_sets[i] = {};
        next_delta_sets[i] = {};
    }

    Value* seed_err = null;
    if (db.incremental_full_recompute_required) {
        seed_err = deduce_seminaive_clear_component_predicates(db, plan, component_id, txn, interp);
        if (seed_err == null) {
            seed_err = deduce_seminaive_seed_component_extensional_state_in_txn(
                db,
                plan,
                component_id,
                txn,
                current_delta_sets,
                {},
                true,
                interp
            );
        }
    } else {
        seed_err = deduce_seminaive_seed_component_delta_sets(db, plan, component_id, current_delta_sets, interp);
    }
    if (seed_err != null) {
        mdb_txn_abort(txn);
        return seed_err;
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            DeduceObservedRuleCounters* observed_rule =
                observed_rules.ptr != null && i < observed_rules.len ? &observed_rules[i] : null;
            Value* err = deduce_seminaive_evaluate_rule(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                head_demand,
                &derived,
                &changed,
                observed_rule,
                interp
            );
            if (err != null) {
                return err;
            }
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
        if (!has_delta) {
            return deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}

fn Value* deduce_naive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    Value* err = deduce_naive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        null,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }
    return null;
}

fn Value* deduce_naive_evaluate_recursive_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    MdbTxn* txn,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass_in_txn(
            db,
            plan,
            component_id,
            txn,
            head_demand,
            &changed,
            &derived,
            observed_rules,
            interp
        );
        if (err != null) {
            return err;
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        if (!changed) {
            return deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}

fn Value* deduce_naive_evaluate_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool has_rules = false;
    for (usz i = 0; i < plan.rule_component_ids.len; i++) {
        if (plan.rule_component_ids[i] == component_id) {
            has_rules = true;
            break;
        }
    }
    if (!has_rules) return null;

    bool recursive = plan.component_recursive.ptr != null && plan.component_recursive[component_id];
    bool has_recursive_aggregates = recursive && deduce_component_has_aggregate_rules(db, plan, component_id);
    if (!recursive) {
        if (db.incremental_full_recompute_required) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(db.env, null, 0, &txn);
            if (rc != MDB_SUCCESS || txn == null) {
                return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
            }

            bool changed = false;
            long derived = 0;
            Value* err = deduce_seminaive_clear_component_predicates(db, plan, component_id, txn, interp);
            if (err == null) {
                err = deduce_seminaive_seed_component_extensional_state_in_txn(
                    db,
                    plan,
                    component_id,
                    txn,
                    {},
                    {},
                    true,
                    interp
                );
            }
            if (err == null) {
                err = deduce_naive_evaluate_component_pass_in_txn(
                    db,
                    plan,
                    component_id,
                    txn,
                    null,
                    &changed,
                    &derived,
                    observed_rules,
                    interp
                );
            }
            if (err == null) {
                err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
            }
            if (err != null) {
                mdb_txn_abort(txn);
                return err;
            }
            rc = mdb_txn_commit(txn);
            if (rc != MDB_SUCCESS) {
                return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
            }
            if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
            if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
            return null;
        }

        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass(
            db,
            plan,
            component_id,
            &changed,
            &derived,
            observed_rules,
            interp
        );
        if (err != null) return err;
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        return null;
    }

    if (use_seminaive_recursive && !has_recursive_aggregates) {
        return deduce_seminaive_evaluate_recursive_component(
            db,
            plan,
            component_id,
            iteration_limit,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    if (use_seminaive_recursive && has_recursive_aggregates &&
        deduce_component_can_use_seminaive_aggregates(db, plan, component_id) &&
        deduce_component_has_full_recursive_aggregate_seminaive_support(db, plan, component_id)) {
        return deduce_seminaive_evaluate_recursive_aggregate_component(
            db,
            plan,
            component_id,
            iteration_limit,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    return deduce_naive_evaluate_recursive_component(
        db,
        plan,
        component_id,
        iteration_limit,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
}

fn Value* deduce_naive_evaluate_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    usz iteration_limit,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool has_rules = false;
    for (usz i = 0; i < plan.rule_component_ids.len; i++) {
        if (plan.rule_component_ids[i] == component_id) {
            has_rules = true;
            break;
        }
    }
    if (!has_rules) return null;

    bool recursive = plan.component_recursive.ptr != null && plan.component_recursive[component_id];
    bool has_recursive_aggregates = recursive && deduce_component_has_aggregate_rules(db, plan, component_id);
    if (!recursive) {
        if (db.incremental_full_recompute_required) {
            bool changed = false;
            long derived = 0;
            Value* err = deduce_seminaive_clear_component_predicates(db, plan, component_id, txn, interp);
            if (err == null) {
                err = deduce_seminaive_seed_component_extensional_state_in_txn(
                    db,
                    plan,
                    component_id,
                    txn,
                    {},
                    {},
                    true,
                    interp
                );
            }
            if (err == null) {
                err = deduce_naive_evaluate_component_pass_in_txn(
                    db,
                    plan,
                    component_id,
                    txn,
                    head_demand,
                    &changed,
                    &derived,
                    observed_rules,
                    interp
                );
            }
            if (err == null) {
                err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
            }
            if (err != null) return err;
            if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
            if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
            return null;
        }

        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass_in_txn(
            db,
            plan,
            component_id,
            txn,
            head_demand,
            &changed,
            &derived,
            observed_rules,
            interp
        );
        if (err != null) return err;
        err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        if (err != null) return err;
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        return null;
    }

    if (use_seminaive_recursive && !has_recursive_aggregates) {
        return deduce_seminaive_evaluate_recursive_component_in_txn(
            db,
            plan,
            component_id,
            iteration_limit,
            txn,
            head_demand,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    if (use_seminaive_recursive && has_recursive_aggregates &&
        deduce_component_can_use_seminaive_aggregates(db, plan, component_id) &&
        deduce_component_has_full_recursive_aggregate_seminaive_support(db, plan, component_id)) {
        return deduce_seminaive_evaluate_recursive_aggregate_component(
            db,
            plan,
            component_id,
            iteration_limit,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
    }

    return deduce_naive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        head_demand,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
}

fn Value* deduce_naive_fixpoint_evaluate_with_plan(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz max_iterations,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    long* max_component_iterations_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (iterations_out != null) *iterations_out = 0;
    if (derived_out != null) *derived_out = 0;
    if (max_component_iterations_out != null) *max_component_iterations_out = 0;
    if (db == null || !db.open || db.env == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: database is not open");
    }
    if (db.rule_signature_count == 0 || plan.component_count == 0) return null;

    usz hard_limit = max_iterations > 0 ? max_iterations : 256;
    for (usz i = 0; i < plan.component_order.len; i++) {
        usz component_id = plan.component_order[i];
        long before_iterations = iterations_out != null ? *iterations_out : 0;
        Value* err = deduce_naive_evaluate_component(
            db,
            plan,
            component_id,
            hard_limit,
            use_seminaive_recursive,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
        if (err != null) return err;

        if (max_component_iterations_out != null && iterations_out != null) {
            long component_iterations = *iterations_out - before_iterations;
            if (component_iterations > *max_component_iterations_out) {
                *max_component_iterations_out = component_iterations;
            }
        }
    }
    return null;
}

fn Value* deduce_naive_fixpoint_evaluate_selected_components_with_plan(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    usz max_iterations,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    long* max_component_iterations_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (iterations_out != null) *iterations_out = 0;
    if (derived_out != null) *derived_out = 0;
    if (max_component_iterations_out != null) *max_component_iterations_out = 0;
    if (db == null || !db.open || db.env == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: database is not open");
    }
    if (selected_components.len < plan.component_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid selected component state");
    }
    if (db.rule_signature_count == 0 || plan.component_count == 0) return null;

    usz hard_limit = max_iterations > 0 ? max_iterations : 256;
    for (usz i = 0; i < plan.component_order.len; i++) {
        usz component_id = plan.component_order[i];
        if (component_id >= selected_components.len || !selected_components[component_id]) continue;

        long before_iterations = iterations_out != null ? *iterations_out : 0;
        Value* err = deduce_naive_evaluate_component(
            db,
            plan,
            component_id,
            hard_limit,
            use_seminaive_recursive,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
        if (err != null) return err;

        if (max_component_iterations_out != null && iterations_out != null) {
            long component_iterations = *iterations_out - before_iterations;
            if (component_iterations > *max_component_iterations_out) {
                *max_component_iterations_out = component_iterations;
            }
        }
    }
    return null;
}

fn Value* deduce_naive_fixpoint_evaluate_selected_components_in_txn_with_plan(
    DeduceDb* db,
    DeduceSccPlan* plan,
    bool[] selected_components,
    MdbTxn* txn,
    usz max_iterations,
    bool use_seminaive_recursive,
    DeduceRuleHeadDemand* head_demand,
    long* iterations_out,
    long* derived_out,
    long* max_component_iterations_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (iterations_out != null) *iterations_out = 0;
    if (derived_out != null) *derived_out = 0;
    if (max_component_iterations_out != null) *max_component_iterations_out = 0;
    if (db == null || !db.open || db.env == null || plan == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: database is not open");
    }
    if (selected_components.len < plan.component_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid selected component state");
    }
    if (db.rule_signature_count == 0 || plan.component_count == 0) return null;

    usz hard_limit = max_iterations > 0 ? max_iterations : 256;
    for (usz i = 0; i < plan.component_order.len; i++) {
        usz component_id = plan.component_order[i];
        if (component_id >= selected_components.len || !selected_components[component_id]) continue;

        long before_iterations = iterations_out != null ? *iterations_out : 0;
        Value* err = deduce_naive_evaluate_component_in_txn(
            db,
            plan,
            component_id,
            txn,
            hard_limit,
            use_seminaive_recursive,
            head_demand,
            iterations_out,
            derived_out,
            observed_rules,
            interp
        );
        if (err != null) return err;

        if (max_component_iterations_out != null && iterations_out != null) {
            long component_iterations = *iterations_out - before_iterations;
            if (component_iterations > *max_component_iterations_out) {
                *max_component_iterations_out = component_iterations;
            }
        }
    }
    return null;
}
