module lisp;

import std::core::mem;

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
