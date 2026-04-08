module lisp;

fn Value* deduce_seminaive_record_new_delta_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.next_delta_sets.len) return null;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return null;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return null;
    return deduce_delta_set_insert(&exec.next_delta_sets[(usz)pred_idx].additions, key_data, key_len, exec.interp);
}

fn bool deduce_seminaive_scratch_visible_contains(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return false;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.scratch_visible_sets.len) return false;
    return deduce_delta_set_contains(&exec.scratch_visible_sets[(usz)pred_idx], key_data, key_len, exec.interp);
}

fn Value* deduce_seminaive_record_scratch_visible_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.scratch_visible_sets.len) return null;
    return deduce_delta_set_insert(&exec.scratch_visible_sets[(usz)pred_idx], key_data, key_len, exec.interp);
}

fn Value* deduce_seminaive_accumulate_component_visible_additions(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] source,
    DeduceDeltaTupleSet[] target,
    Interp* interp
) {
    if (plan == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid worker-visible accumulation state");
    }
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= source.len || pred_idx >= target.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        for (usz tuple_idx = 0; tuple_idx < source[pred_idx].additions.count; tuple_idx++) {
            DeduceEncodedTuple tuple = source[pred_idx].additions.tuples[tuple_idx];
            Value* err = deduce_delta_set_insert(&target[pred_idx], tuple.key_bytes, tuple.key_len, interp);
            if (err != null) return err;
        }
    }
    return null;
}

fn Value* deduce_seminaive_record_removed_delta_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.next_delta_sets.len) return null;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return null;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return null;
    return deduce_delta_set_insert(&exec.next_delta_sets[(usz)pred_idx].removals, key_data, key_len, exec.interp);
}

fn bool deduce_seminaive_component_has_delta(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] delta_sets
) {
    if (plan == null || component_id >= plan.component_count) return false;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= delta_sets.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;
        if (delta_sets[pred_idx].additions.count > 0 || delta_sets[pred_idx].removals.count > 0) {
            return true;
        }
    }
    return false;
}
