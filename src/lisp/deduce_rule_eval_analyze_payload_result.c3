module lisp;

fn Value* deduce_analyze_build_payload(
    DeduceDb* db,
    DeduceSccPlan* scc_plan,
    DeduceAnalyzeCatalogSummary* catalog_summary,
    DeduceAnalyzeSelectedComponent* selected,
    DeduceObservedRuleCounters[] observed_rules,
    usz[] parallel_wave_ids,
    usz[] parallel_batch_sizes,
    DeduceAnalyzeExecutionSummary* summary,
    Interp* interp
) {
    if (db == null || scc_plan == null || catalog_summary == null || selected == null || summary == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid result payload state");
    }

    Value* out = make_hashmap_checked(interp, 128, "deduce/analyze: out of memory");
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    Value* goal_directed_components = null;
    Value* goal_directed_selected_components = null;
    Value* goal_directed_selected_predicates = null;
    Value* goal_directed_err = deduce_analyze_prepare_goal_directed_payloads(
        db,
        scc_plan,
        selected,
        &goal_directed_components,
        &goal_directed_selected_components,
        &goal_directed_selected_predicates,
        interp
    );
    if (goal_directed_err != null) return goal_directed_err;

    Value* fields_err = deduce_analyze_set_result_payload_fields(
        out,
        db,
        scc_plan,
        catalog_summary,
        selected,
        parallel_wave_ids,
        parallel_batch_sizes,
        summary,
        goal_directed_components,
        goal_directed_selected_components,
        goal_directed_selected_predicates,
        interp
    );
    if (fields_err != null) return fields_err;
    Value* tail_err = deduce_analyze_append_runtime_tail_payload(
        out,
        db,
        observed_rules,
        interp
    );
    if (tail_err != null) return tail_err;
    return out;
}
