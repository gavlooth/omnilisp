module lisp;

import std::core::mem;

fn Value* deduce_analyze_dirty_predicates_payload(DeduceDb* db, Interp* interp) {
    if (db == null || db.incremental_dirty_predicates == null || db.incremental_dirty_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        SymbolId predicate = db.incremental_dirty_predicates[i];
        if (predicate == (SymbolId)0) continue;
        rev = make_cons(interp, make_symbol(interp, predicate), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_analyze_goal_directed_selector_error(
    long selector_rule_index,
    usz component_id,
    Value* blockers,
    Interp* interp
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP || data.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    explain_dict_set(data, interp, "rule-index", make_int(interp, selector_rule_index));
    explain_dict_set(data, interp, "component-id", make_int(interp, (long)component_id));
    explain_dict_set(data, interp, "goal-directed-blockers",
        blockers == null ? make_nil(interp) : blockers);
    return deduce_raise(
        interp,
        "deduce/analyze-goal-directed-selector-not-eligible",
        "deduce/analyze: selected rule is not goal-directed eligible",
        data
    );
}

fn Value* prim_deduce_analyze(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/analyze-expected-db",
            "deduce/analyze: expected (deduce/analyze db [rule-index] [engine])");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/analyze-db-not-handle",
            "deduce/analyze: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/analyze-db-handle-closed",
            "deduce/analyze: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/analyze-db-not-open", "deduce/analyze: database is not open");
    }
    long dependency_edges = 0;
    deduce_db_catalog_dependency_edge_counts(db, &dependency_edges, null);
    long catalog_rule_count = (long)deduce_db_catalog_rule_count(db);
    long incremental_dirty_predicates = (long)db.incremental_dirty_count;
    long keyed_relation_count = 0;
    long unique_relation_count = 0;
    long reference_constraint_count = 0;
    long resolved_reference_constraint_count = 0;
    long unresolved_reference_constraint_count = 0;
    long check_constraint_count = 0;
    long integrity_constraint_count = 0;
    long materialized_view_count = 0;
    long ready_materialized_view_count = 0;
    long unready_materialized_view_count = 0;
    long stale_materialized_view_count = 0;
    Value* ready_materialized_relations_rev = make_nil(interp);
    Value* unready_materialized_relations_rev = make_nil(interp);
    Value* stale_materialized_relations_rev = make_nil(interp);
    long goal_directed_recursive_component_count = 0;
    long goal_directed_eligible_component_count = 0;
    long goal_directed_blocked_component_count = 0;
    long goal_directed_aggregate_blocked_component_count = 0;
    long goal_directed_negated_blocked_component_count = 0;
    long parallel_recursive_component_count = 0;
    long parallel_batched_component_count = 0;
    long parallel_batch_count = 0;
    long parallel_max_batch_size = 0;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (db.relation_schemas[i].key_column != (SymbolId)0) keyed_relation_count++;
        if (db.relation_schemas[i].unique_column_count > 0 &&
            !deduce_relation_schema_unique_equivalent_to_key(&db.relation_schemas[i])) {
            unique_relation_count++;
        }
        long resolved_reference_count =
            deduce_relation_schema_resolved_reference_constraint_count(db, &db.relation_schemas[i]);
        reference_constraint_count += (long)db.relation_schemas[i].reference_constraint_count;
        check_constraint_count += (long)db.relation_schemas[i].check_constraint_count;
        resolved_reference_constraint_count += resolved_reference_count;
        unresolved_reference_constraint_count +=
            (long)db.relation_schemas[i].reference_constraint_count - resolved_reference_count;
        integrity_constraint_count += deduce_relation_schema_constraint_count(&db.relation_schemas[i]);
        if (db.relation_schemas[i].materialized_view) {
            materialized_view_count++;
            Value* relation_symbol = make_symbol(interp, db.relation_schemas[i].name);
            if (deduce_relation_schema_is_materialized_ready(db, &db.relation_schemas[i])) {
                ready_materialized_view_count++;
                ready_materialized_relations_rev =
                    make_cons(interp, relation_symbol, ready_materialized_relations_rev);
            } else {
                unready_materialized_view_count++;
                unready_materialized_relations_rev =
                    make_cons(interp, relation_symbol, unready_materialized_relations_rev);
            }
            if (deduce_relation_schema_is_materialized_stale(db, &db.relation_schemas[i])) {
                stale_materialized_view_count++;
                stale_materialized_relations_rev =
                    make_cons(interp, relation_symbol, stale_materialized_relations_rev);
            }
        }
    }
    SymbolId incremental_invalidation_mode =
        interp.symbols.intern(db.incremental_full_recompute_required ? "full-recompute" : "tracked");

    bool use_seminaive_recursive = true;
    SymbolId execution_engine_sym = interp.symbols.intern("semi-naive-scc");
    long selector_rule_index = -1;
    if (args.len >= 2) {
        Value* selector_or_engine = args[1];
        if (selector_or_engine != null && selector_or_engine.tag == INT) {
            selector_rule_index = selector_or_engine.int_val;
            if (args.len == 3) {
                Value* engine_arg = args[2];
                if (engine_arg == null || engine_arg.tag != SYMBOL) {
                    return deduce_raise(interp, "deduce/analyze-invalid-engine",
                        "deduce/analyze: engine must be symbol 'naive or 'semi-naive");
                }
                char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
                if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
                    use_seminaive_recursive = false;
                    execution_engine_sym = interp.symbols.intern("naive-scc");
                } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
                    use_seminaive_recursive = true;
                    execution_engine_sym = interp.symbols.intern("semi-naive-scc");
                } else {
                    return deduce_raise(interp, "deduce/analyze-invalid-engine",
                        "deduce/analyze: engine must be 'naive or 'semi-naive");
                }
            }
        } else if (args.len == 2 && selector_or_engine != null && selector_or_engine.tag == SYMBOL) {
            Value* engine_arg = selector_or_engine;
            char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
            if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
                use_seminaive_recursive = false;
                execution_engine_sym = interp.symbols.intern("naive-scc");
            } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
                use_seminaive_recursive = true;
                execution_engine_sym = interp.symbols.intern("semi-naive-scc");
            } else {
                return deduce_raise(interp, "deduce/analyze-invalid-engine",
                    "deduce/analyze: engine must be 'naive or 'semi-naive");
            }
        } else if (args.len == 3) {
            return deduce_raise(interp, "deduce/analyze-invalid-selector",
                "deduce/analyze: rule-index selector must be an integer");
        } else {
            return deduce_raise(interp, "deduce/analyze-invalid-engine",
                "deduce/analyze: engine must be symbol 'naive or 'semi-naive");
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

    for (usz component_id = 0; component_id < scc_plan.component_count; component_id++) {
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[component_id];
        if (!recursive) continue;

        goal_directed_recursive_component_count++;
        parallel_recursive_component_count++;

        bool has_aggregates = deduce_component_has_aggregate_rules(db, &scc_plan, component_id);
        bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, &scc_plan, component_id);
        bool eligible = !has_aggregates && !has_negated_body_atoms;

        if (eligible) {
            goal_directed_eligible_component_count++;
        } else {
            goal_directed_blocked_component_count++;
        }
        if (has_aggregates) goal_directed_aggregate_blocked_component_count++;
        if (has_negated_body_atoms) goal_directed_negated_blocked_component_count++;

        long batch_size =
            component_id < parallel_batch_sizes.len ? (long)parallel_batch_sizes[component_id] : 0;
        if (batch_size > 1) parallel_batched_component_count++;
        if (batch_size > parallel_max_batch_size) parallel_max_batch_size = batch_size;

        bool batch_seen = false;
        if (component_id < parallel_wave_ids.len && component_id < scc_plan.component_strata.len) {
            usz stratum = scc_plan.component_strata[component_id];
            usz wave = parallel_wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    scc_plan.component_recursive.ptr != null &&
                    prev < scc_plan.component_recursive.len &&
                    scc_plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= parallel_wave_ids.len || prev >= scc_plan.component_strata.len) continue;
                if (scc_plan.component_strata[prev] != stratum) continue;
                if (parallel_wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (!batch_seen) parallel_batch_count++;
    }

    SymbolId requested_execution_engine_sym = execution_engine_sym;
    bool naive_recursive_mixed_aggregate_routed = false;
    if (selector_rule_index < 0 && !use_seminaive_recursive && deduce_plan_has_recursive_mixed_aggregate_components(db, &scc_plan)) {
        use_seminaive_recursive = true;
        execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        naive_recursive_mixed_aggregate_routed = true;
    }

    bool aggregate_recursive_fallback =
        selector_rule_index < 0 &&
        use_seminaive_recursive &&
        deduce_plan_has_recursive_aggregate_fallback_components(db, &scc_plan);
    if (aggregate_recursive_fallback) {
        execution_engine_sym = interp.symbols.intern("naive-scc");
    }

    bool selected_component_analysis = false;
    usz selected_component_id = 0;
    bool[] selected_components = {};
    defer if (selected_components.ptr != null) mem::free(selected_components.ptr);
    if (selector_rule_index >= 0) {
        if ((ulong)selector_rule_index >= db.rule_signature_count) {
            return deduce_raise(interp, "deduce/analyze-rule-index-out-of-range",
                "deduce/analyze: rule-index is out of range");
        }
        if (scc_plan.rule_component_ids.ptr == null || (ulong)selector_rule_index >= scc_plan.rule_component_ids.len) {
            return deduce_raise(interp, "deduce/analyze-invalid-state",
                "deduce/analyze: selected rule is missing from the SCC plan");
        }

        selected_component_id = scc_plan.rule_component_ids[(usz)selector_rule_index];
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            selected_component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[selected_component_id];
        bool has_aggregates = deduce_component_has_aggregate_rules(db, &scc_plan, selected_component_id);
        bool has_negated_body_atoms = deduce_component_has_negated_body_atoms(db, &scc_plan, selected_component_id);
        bool eligible = recursive && !has_aggregates && !has_negated_body_atoms;
        if (!eligible) {
            Value* blockers = deduce_explain_goal_directed_blockers(
                recursive,
                has_aggregates,
                has_negated_body_atoms,
                interp
            );
            return deduce_analyze_goal_directed_selector_error(
                selector_rule_index,
                selected_component_id,
                blockers,
                interp
            );
        }

        selected_components = mem::new_array(bool, scc_plan.component_count);
        if (selected_components.ptr == null && scc_plan.component_count > 0) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (!deduce_fixpoint_select_dependency_components(&scc_plan, selected_component_id, selected_components)) {
            return deduce_raise(interp, "deduce/analyze-invalid-state",
                "deduce/analyze: failed to select goal-directed component closure");
        }
        selected_component_analysis = true;
    }

    DeduceObservedRuleCounters[] observed_rules = {};
    if (db.rule_signature_count > 0) {
        observed_rules = mem::new_array(DeduceObservedRuleCounters, db.rule_signature_count);
        if (observed_rules.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < observed_rules.len; i++) {
            observed_rules[i] = {};
            usz step_count = db.rule_signatures[i].body_count;
            if (step_count == 0) continue;
            DeduceObservedStepCounters[] steps = mem::new_array(DeduceObservedStepCounters, step_count);
            if (steps.ptr == null) {
                for (usz j = 0; j < i; j++) {
                    if (observed_rules[j].steps != null) mem::free(observed_rules[j].steps);
                }
                mem::free(observed_rules.ptr);
                return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
            }
            observed_rules[i].steps = steps.ptr;
            observed_rules[i].step_count = step_count;
            deduce_observed_rule_counters_reset(&observed_rules[i]);
        }
    }
    defer {
        if (observed_rules.ptr != null) {
            for (usz i = 0; i < observed_rules.len; i++) {
                if (observed_rules[i].steps != null) mem::free(observed_rules[i].steps);
            }
            mem::free(observed_rules.ptr);
        }
    };

    const usz ANALYZE_ITERATION_LIMIT = 256;
    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* incremental_dirty_predicate_payload = deduce_analyze_dirty_predicates_payload(db, interp);
    Value* err = selected_component_analysis
        ? deduce_naive_fixpoint_evaluate_selected_components_with_plan(
            db,
            &scc_plan,
            selected_components,
            ANALYZE_ITERATION_LIMIT,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        )
        : deduce_naive_fixpoint_evaluate_with_plan(
            db,
            &scc_plan,
            ANALYZE_ITERATION_LIMIT,
            use_seminaive_recursive,
            &iterations,
            &derived,
            &max_component_iterations,
            observed_rules,
            interp
        );
    if (err != null) return err;

    long recursive_strata = 0;
    for (usz i = 0; i < scc_plan.component_recursive.len; i++) {
        if (scc_plan.component_recursive[i]) recursive_strata++;
    }

    Value* out = make_hashmap(interp, 128);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    Value* goal_directed_components =
        deduce_explain_goal_directed_component_summaries(db, &scc_plan, interp);
    if (goal_directed_components != null && goal_directed_components.tag == ERROR) {
        return goal_directed_components;
    }
    Value* goal_directed_selected_components =
        selected_component_analysis
            ? deduce_explain_goal_directed_selected_components(&scc_plan, selected_component_id, true, interp)
            : make_nil(interp);
    if (goal_directed_selected_components != null && goal_directed_selected_components.tag == ERROR) {
        return goal_directed_selected_components;
    }
    Value* goal_directed_selected_predicates =
        selected_component_analysis
            ? deduce_explain_goal_directed_selected_predicates(&scc_plan, selected_component_id, true, interp)
            : make_nil(interp);
    if (goal_directed_selected_predicates != null && goal_directed_selected_predicates.tag == ERROR) {
        return goal_directed_selected_predicates;
    }
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-analyze"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "mode", interp.symbols.intern("naive-bottom-up"));
    explain_dict_set_symbol(out, interp, "goal-directed-execution-path",
        interp.symbols.intern(selected_component_analysis ? "selected-component-closure" : "full-db-fixpoint"));
    explain_dict_set_symbol(out, interp, "execution-engine", execution_engine_sym);
    explain_dict_set_symbol(out, interp, "requested-execution-engine", requested_execution_engine_sym);
    explain_dict_set(out, interp, "goal-directed-selector-rule-index",
        selected_component_analysis ? make_int(interp, selector_rule_index) : make_nil(interp));
    explain_dict_set(out, interp, "goal-directed-selected-component-id",
        selected_component_analysis ? make_int(interp, (long)selected_component_id) : make_nil(interp));
    explain_dict_set(out, interp, "recursive-aggregate-naive-fallback",
        aggregate_recursive_fallback ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "naive-recursive-aggregate-mixed-routed-to-seminaive",
        naive_recursive_mixed_aggregate_routed ? make_symbol(interp, interp.sym_true) : make_nil(interp));
    explain_dict_set(out, interp, "version", make_int(interp, 1));
    explain_dict_set(out, interp, "rule-count", make_int(interp, catalog_rule_count));
    explain_dict_set(out, interp, "strata-count", make_int(interp, (long)scc_plan.component_count));
    explain_dict_set(out, interp, "stratum-count", make_int(interp, (long)scc_plan.stratum_count));
    explain_dict_set(out, interp, "recursive-strata", make_int(interp, recursive_strata));
    explain_dict_set(out, interp, "goal-directed-recursive-component-count",
        make_int(interp, goal_directed_recursive_component_count));
    explain_dict_set(out, interp, "goal-directed-eligible-component-count",
        make_int(interp, goal_directed_eligible_component_count));
    explain_dict_set(out, interp, "goal-directed-blocked-component-count",
        make_int(interp, goal_directed_blocked_component_count));
    explain_dict_set(out, interp, "goal-directed-aggregate-blocked-component-count",
        make_int(interp, goal_directed_aggregate_blocked_component_count));
    explain_dict_set(out, interp, "goal-directed-negated-blocked-component-count",
        make_int(interp, goal_directed_negated_blocked_component_count));
    explain_dict_set(out, interp, "parallel-recursive-component-count",
        make_int(interp, parallel_recursive_component_count));
    explain_dict_set(out, interp, "parallel-batched-component-count",
        make_int(interp, parallel_batched_component_count));
    explain_dict_set(out, interp, "parallel-batch-count",
        make_int(interp, parallel_batch_count));
    explain_dict_set(out, interp, "parallel-max-batch-size",
        make_int(interp, parallel_max_batch_size));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(&scc_plan, parallel_wave_ids[..], parallel_batch_sizes[..], interp));
    explain_dict_set(out, interp, "goal-directed-components", goal_directed_components);
    explain_dict_set(out, interp, "goal-directed-selected-components", goal_directed_selected_components);
    explain_dict_set(out, interp, "goal-directed-selected-predicates", goal_directed_selected_predicates);
    explain_dict_set(out, interp, "iteration-limit", make_int(interp, (long)ANALYZE_ITERATION_LIMIT));
    explain_dict_set(out, interp, "max-component-iterations", make_int(interp, max_component_iterations));
    explain_dict_set(out, interp, "iterations", make_int(interp, iterations));
    explain_dict_set(out, interp, "derived-facts", make_int(interp, derived));
    explain_dict_set(out, interp, "incremental-dependency-edges", make_int(interp, dependency_edges));
    explain_dict_set(out, interp, "incremental-dirty-predicate-count", make_int(interp, incremental_dirty_predicates));
    explain_dict_set(out, interp, "incremental-dirty-predicates", incremental_dirty_predicate_payload);
    explain_dict_set_symbol(out, interp, "incremental-invalidation-mode", incremental_invalidation_mode);
    explain_dict_set(out, interp, "keyed-relation-count", make_int(interp, keyed_relation_count));
    explain_dict_set(out, interp, "unique-relation-count", make_int(interp, unique_relation_count));
    explain_dict_set(out, interp, "reference-constraint-count", make_int(interp, reference_constraint_count));
    explain_dict_set(out, interp, "check-constraint-count", make_int(interp, check_constraint_count));
    explain_dict_set(out, interp, "resolved-reference-constraint-count", make_int(interp, resolved_reference_constraint_count));
    explain_dict_set(out, interp, "unresolved-reference-constraint-count", make_int(interp, unresolved_reference_constraint_count));
    explain_dict_set(out, interp, "integrity-constraint-count", make_int(interp, integrity_constraint_count));
    explain_dict_set(out, interp, "materialized-view-count", make_int(interp, materialized_view_count));
    explain_dict_set(out, interp, "ready-materialized-view-count", make_int(interp, ready_materialized_view_count));
    explain_dict_set(out, interp, "unready-materialized-view-count", make_int(interp, unready_materialized_view_count));
    explain_dict_set(out, interp, "stale-materialized-view-count", make_int(interp, stale_materialized_view_count));
    explain_dict_set(out, interp, "ready-materialized-relations",
        deduce_explain_reverse_list(ready_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "unready-materialized-relations",
        deduce_explain_reverse_list(unready_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "stale-materialized-relations",
        deduce_explain_reverse_list(stale_materialized_relations_rev, interp));
    explain_dict_set(out, interp, "refreshed-materialized-view-count", make_int(interp, 0));
    explain_dict_set(out, interp, "integrity-violation-count", make_int(interp, (long)db.integrity_violation_count));
    explain_dict_set(out, interp, "key-integrity-violation-count", make_int(interp, (long)db.key_integrity_violation_count));
    explain_dict_set(out, interp, "unique-integrity-violation-count", make_int(interp, (long)db.unique_integrity_violation_count));
    explain_dict_set(out, interp, "reference-integrity-violation-count", make_int(interp, (long)db.reference_integrity_violation_count));
    explain_dict_set(out, interp, "check-integrity-violation-count", make_int(interp, (long)db.check_integrity_violation_count));
    explain_dict_set(out, interp, "last-integrity-violation-code",
        db.last_integrity_violation_code != (SymbolId)0
            ? make_symbol(interp, db.last_integrity_violation_code)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-integrity-violation-relation",
        db.last_integrity_violation_relation != (SymbolId)0
            ? make_symbol(interp, db.last_integrity_violation_relation)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        db.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        db.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-relation",
        db.last_goal_directed_read_relation != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_relation)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        db.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, db.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                db.last_goal_directed_read_requested_bound_positions,
                db.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                db.last_goal_directed_read_applied_bound_positions,
                db.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));
    Value* recent_integrity_violations = deduce_integrity_violation_history_payload(
        db.integrity_violation_history[..],
        db.integrity_violation_history_count,
        interp
    );
    if (recent_integrity_violations != null && recent_integrity_violations.tag == ERROR) return recent_integrity_violations;
    explain_dict_set(out, interp, "recent-integrity-violations", recent_integrity_violations);
    Value* rule_execution = deduce_explain_build_rule_execution_list(db, observed_rules, interp);
    if (rule_execution != null && rule_execution.tag == ERROR) return rule_execution;
    explain_dict_set(out, interp, "rule-execution", rule_execution);
    if (selected_component_analysis) {
        if (!db.incremental_full_recompute_required) {
            deduce_fixpoint_clear_selected_dirty_predicates(db, &scc_plan, selected_components);
        }
    } else {
        deduce_db_reset_incremental_tracking(db);
    }
    return out;
}

fn Value* prim_deduce_rule(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) {
        return deduce_raise(interp, "deduce/rule-expected-db-head-body", "deduce-rule!: expected (deduce/rule! db head-atom body-atom...)");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/rule-db-not-handle", "deduce-rule!: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/rule-db-handle-closed", "deduce-rule!: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/rule-db-not-open", "deduce-rule!: database is not open");
    }

    DeduceRuleIr rule = {};
    Value* parse_err = deduce_rule_parse_ir(args[1..], &rule, interp);
    if (parse_err != null) {
        deduce_rule_ir_free(&rule);
        return parse_err;
    }
    defer deduce_rule_ir_free(&rule);

    Value* err = deduce_rule_validate_install_candidate(db, &rule, interp);
    if (err != null) return err;

    SymbolId[] body_preds = mem::new_array(SymbolId, rule.body_count);
    if (body_preds.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_preds.ptr);

    bool[] body_negated = mem::new_array(bool, rule.body_count);
    if (body_negated.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_negated.ptr);

    ulong[] body_bound_masks = mem::new_array(ulong, rule.body_count);
    if (body_bound_masks.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_bound_masks.ptr);

    int[] body_selected_indexes = mem::new_array(int, rule.body_count);
    if (body_selected_indexes.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_selected_indexes.ptr);

    bool[] body_filter_pushdown = mem::new_array(bool, rule.body_count);
    if (body_filter_pushdown.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_filter_pushdown.ptr);

    ulong[] body_projection_masks = mem::new_array(ulong, rule.body_count);
    if (body_projection_masks.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_projection_masks.ptr);

    DeduceRuleTermKind[] head_term_kinds = mem::new_array(DeduceRuleTermKind, rule.head.term_count);
    if (head_term_kinds.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_kinds.ptr != null) mem::free(head_term_kinds.ptr);

    DeduceAggregateOpKind[] head_term_aggregate_ops = mem::new_array(DeduceAggregateOpKind, rule.head.term_count);
    if (head_term_aggregate_ops.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_aggregate_ops.ptr != null) mem::free(head_term_aggregate_ops.ptr);

    bool[] head_term_is_variable = mem::new_array(bool, rule.head.term_count);
    if (head_term_is_variable.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);

    uint[] head_term_var_ids = mem::new_array(uint, rule.head.term_count);
    if (head_term_var_ids.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);

    Value*[] head_term_literals = mem::new_array(Value*, rule.head.term_count);
    if (head_term_literals.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);

    usz[] body_term_offsets = mem::new_array(usz, rule.body_count);
    if (body_term_offsets.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);

    usz[] body_term_counts = mem::new_array(usz, rule.body_count);
    if (body_term_counts.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);

    usz total_body_term_count = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        if (total_body_term_count > usz.max - rule.body[i].term_count) {
            return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
        }
        total_body_term_count += rule.body[i].term_count;
    }

    bool[] body_term_is_variable = mem::new_array(bool, total_body_term_count);
    if (body_term_is_variable.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);

    uint[] body_term_var_ids = mem::new_array(uint, total_body_term_count);
    if (body_term_var_ids.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);

    Value*[] body_term_literals = mem::new_array(Value*, total_body_term_count);
    if (body_term_literals.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);

    DeduceBodyPlanStep[] body_plan = mem::new_array(DeduceBodyPlanStep, rule.body_count);
    if (body_plan.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_plan.ptr);

    for (usz i = 0; i < rule.body_count; i++) {
        body_plan[i].logical_body_index = 0;
        body_plan[i].bound_mask = 0;
        body_plan[i].selected_index = -1;
        body_plan[i].op_kind = DEDUCE_PHYSICAL_FULL_SCAN;
    }

    err = deduce_plan_conjunctive_body(
        db,
        rule.body,
        rule.body_count,
        rule.max_var_id,
        body_plan.ptr,
        body_plan.len,
        interp
    );
    if (err != null) return err;

    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_preds[i] = atom.predicate;
        body_negated[i] = atom.negated;
        body_bound_masks[i] = body_plan[i].bound_mask;
        body_selected_indexes[i] = body_plan[i].selected_index;
        body_filter_pushdown[i] = body_plan[i].bound_mask != 0 ||
            deduce_rule_atom_has_local_equality_constraint(&atom);
        body_projection_masks[i] = deduce_rule_projection_mask_for_step(&rule, body_plan, i);
    }

    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
        head_term_kinds[i] = term.kind;
        head_term_aggregate_ops[i] = term.aggregate_op;
        head_term_is_variable[i] = is_var;
        head_term_var_ids[i] =
            (term.kind == DEDUCE_RULE_TERM_VARIABLE || term.kind == DEDUCE_RULE_TERM_AGGREGATE)
                ? term.normalized_var_id
                : 0;
        if (is_var || term.kind == DEDUCE_RULE_TERM_AGGREGATE) {
            head_term_literals[i] = null;
        } else {
            head_term_literals[i] = boundary_promote_to_escape(term.literal_value, interp);
        }
    }

    usz body_term_cursor = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_term_offsets[i] = body_term_cursor;
        body_term_counts[i] = atom.term_count;
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
            body_term_is_variable[body_term_cursor] = is_var;
            body_term_var_ids[body_term_cursor] = is_var ? term.normalized_var_id : 0;
            if (is_var) {
                body_term_literals[body_term_cursor] = null;
            } else {
                body_term_literals[body_term_cursor] = boundary_promote_to_escape(term.literal_value, interp);
            }
            body_term_cursor++;
        }
    }

    DeduceRuleRegisterStatus register_status =
        deduce_db_add_rule_signature(
            db,
            rule.head.predicate,
            body_preds,
            body_negated,
            body_bound_masks,
            body_selected_indexes,
            body_filter_pushdown,
            body_projection_masks,
            head_term_kinds,
            head_term_aggregate_ops,
            head_term_is_variable,
            head_term_var_ids,
            head_term_literals,
            body_term_offsets,
            body_term_counts,
            body_term_is_variable,
            body_term_var_ids,
            body_term_literals,
            rule.max_var_id
        );
    if (register_status != DEDUCE_RULE_REGISTER_OK) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    Value* materialized_err = deduce_db_note_rule_signature_install(db, interp);
    if (materialized_err != null) return materialized_err;
    Value* persist_err = deduce_db_persist_rule_catalog(db, interp);
    if (persist_err != null) return persist_err;
    Value* persist_signatures_err = deduce_db_persist_rule_signatures(db, interp);
    if (persist_signatures_err != null) return persist_signatures_err;
    return make_void(interp);
}
