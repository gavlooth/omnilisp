module lisp;

import std::io;

fn bool deduce_worker_scratch_trace_enabled() @inline {
    return boundary_env_flag_enabled("OMNI_TRACE_DEDUCE_WORKER_SCRATCH");
}

fn void deduce_worker_scratch_trace(
    String phase,
    usz component_id,
    long a = 0,
    long b = 0
) @inline {
    if (!deduce_worker_scratch_trace_enabled()) return;
    io::printfn(
        "OMNI_DEDUCE_WORKER_SCRATCH phase=%s component=%d a=%d b=%d",
        (ZString)phase,
        component_id,
        a,
        b
    );
}

fn usz deduce_worker_scratch_component_visible_tuple_count(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceDeltaTupleSet[] sets
) {
    if (plan == null) return 0;
    usz total = 0;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= sets.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        total += sets[pred_idx].count;
    }
    return total;
}

fn usz deduce_worker_scratch_component_next_additions_count(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] sets
) {
    if (plan == null) return 0;
    usz total = 0;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= sets.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        total += sets[pred_idx].additions.count;
    }
    return total;
}

fn bool deduce_component_has_worker_scratch_single_recursive_atom_shape(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        usz recursive_atom_count = 0;
        for (usz body_idx = 0; body_idx < sig.body_count; body_idx++) {
            if (sig.body_negated != null && sig.body_negated[body_idx]) continue;
            if (sig.body_predicates == null) return false;
            if (!deduce_scc_predicate_in_component(plan, sig.body_predicates[body_idx], component_id)) continue;
            recursive_atom_count++;
            if (recursive_atom_count > 1) return false;
        }
    }
    return true;
}

fn Value* deduce_seminaive_accumulate_component_additions(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] source,
    DeduceSignedDeltaSet[] target,
    Interp* interp
) {
    if (plan == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid scratch accumulation state");
    }
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= source.len || pred_idx >= target.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        for (usz tuple_idx = 0; tuple_idx < source[pred_idx].additions.count; tuple_idx++) {
            DeduceEncodedTuple tuple = source[pred_idx].additions.tuples[tuple_idx];
            Value* err = deduce_delta_set_insert(&target[pred_idx].additions, tuple.key_bytes, tuple.key_len, interp);
            if (err != null) return err;
        }
    }
    return null;
}
