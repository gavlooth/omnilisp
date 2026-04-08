module lisp;

import std::core::mem;

fn Value* deduce_seminaive_compute_component_scratch_pass_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    char** bytes_out,
    usz* len_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count ||
        bytes_out == null || len_out == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid worker-scratch component delta state");
    }
    *bytes_out = null;
    *len_out = 0;
    deduce_worker_scratch_trace("scratch_pass_begin", component_id, (long)db.rule_signature_count, 0);

    bool recursive = plan.component_recursive.ptr != null &&
        component_id < plan.component_recursive.len &&
        plan.component_recursive[component_id];
    if (!recursive) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-component-not-recursive",
            "deduce/parallel: worker scratch batches require a recursive component");
    }
    if (deduce_component_has_aggregate_rules(db, plan, component_id)) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-aggregate-unsupported",
            "deduce/parallel: worker scratch batches do not yet support aggregate recursive components");
    }
    if (deduce_component_has_negated_body_atoms(db, plan, component_id)) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-negation-unsupported",
            "deduce/parallel: worker scratch batches do not yet support negated recursive components");
    }

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

    DeduceDeltaTupleSet[] scratch_visible_sets = mem::new_array(DeduceDeltaTupleSet, plan.predicates.len);
    if (scratch_visible_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (scratch_visible_sets.ptr != null) {
            for (usz i = 0; i < scratch_visible_sets.len; i++) deduce_delta_set_release(&scratch_visible_sets[i]);
            mem::free(scratch_visible_sets.ptr);
        }
    }

    for (usz i = 0; i < plan.predicates.len; i++) {
        current_delta_sets[i] = {};
        next_delta_sets[i] = {};
        scratch_visible_sets[i] = {};
    }

    Value* seed_err = deduce_seminaive_seed_component_delta_sets(db, plan, component_id, current_delta_sets, interp);
    if (seed_err != null) return seed_err;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    bool changed = false;
    long derived = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        deduce_worker_scratch_trace("scratch_pass_rule_loop_begin", component_id, (long)i, (long)sig.head_predicate);
        Value* err = deduce_seminaive_evaluate_rule_dry_run(
            db,
            plan,
            component_id,
            sig,
            txn,
            current_delta_sets,
            next_delta_sets,
            scratch_visible_sets,
            null,
            &derived,
            &changed,
            null,
            interp
        );
        if (err != null) return err;
        deduce_worker_scratch_trace(
            "scratch_pass_rule_loop_after_dry_run",
            component_id,
            (long)deduce_worker_scratch_component_next_additions_count(plan, component_id, next_delta_sets),
            (long)deduce_worker_scratch_component_visible_tuple_count(plan, component_id, scratch_visible_sets)
        );

        err = deduce_seminaive_accumulate_component_visible_additions(
            plan,
            component_id,
            next_delta_sets,
            scratch_visible_sets,
            interp
        );
        if (err != null) return err;
        deduce_worker_scratch_trace(
            "scratch_pass_rule_loop_after_accumulate_visible",
            component_id,
            (long)deduce_worker_scratch_component_next_additions_count(plan, component_id, next_delta_sets),
            (long)deduce_worker_scratch_component_visible_tuple_count(plan, component_id, scratch_visible_sets)
        );
    }
    deduce_worker_scratch_trace(
        "scratch_pass_end_before_serialize",
        component_id,
        (long)deduce_worker_scratch_component_next_additions_count(plan, component_id, next_delta_sets),
        (long)deduce_worker_scratch_component_visible_tuple_count(plan, component_id, scratch_visible_sets)
    );

    return deduce_component_delta_payload_serialize(plan, component_id, next_delta_sets, bytes_out, len_out, interp);
}
