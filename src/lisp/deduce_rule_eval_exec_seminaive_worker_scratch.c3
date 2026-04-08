module lisp;

import std::core::mem;

fn Value* deduce_seminaive_compute_component_scratch_closure_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    char** bytes_out,
    usz* len_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count ||
        bytes_out == null || len_out == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid worker-scratch closure state");
    }
    *bytes_out = null;
    *len_out = 0;

    bool recursive = plan.component_recursive.ptr != null &&
        component_id < plan.component_recursive.len &&
        plan.component_recursive[component_id];
    if (!recursive) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-component-not-recursive",
            "deduce/parallel: worker scratch closure requires a recursive component");
    }
    if (deduce_component_has_aggregate_rules(db, plan, component_id)) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-aggregate-unsupported",
            "deduce/parallel: worker scratch closure does not yet support aggregate recursive components");
    }
    if (deduce_component_has_negated_body_atoms(db, plan, component_id)) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-negation-unsupported",
            "deduce/parallel: worker scratch closure does not yet support negated recursive components");
    }
    if (!deduce_component_has_worker_scratch_single_recursive_atom_shape(db, plan, component_id)) {
        return deduce_raise(interp, "deduce/parallel-worker-scratch-multi-atom-unsupported",
            "deduce/parallel: worker scratch closure does not yet support multi-atom recursive rule shapes");
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

    DeduceSignedDeltaSet[] accumulated_delta_sets = mem::new_array(DeduceSignedDeltaSet, plan.predicates.len);
    if (accumulated_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (accumulated_delta_sets.ptr != null) {
            for (usz i = 0; i < accumulated_delta_sets.len; i++) deduce_signed_delta_set_release(&accumulated_delta_sets[i]);
            mem::free(accumulated_delta_sets.ptr);
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
        accumulated_delta_sets[i] = {};
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

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            Value* err = deduce_seminaive_evaluate_rule_with_groups(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                scratch_visible_sets,
                {},
                null,
                null,
                false,
                true,
                true,
                &derived,
                &changed,
                null,
                interp
            );
            if (err != null) return err;
        }

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        if (!has_delta) {
            return deduce_component_delta_payload_serialize(plan, component_id, accumulated_delta_sets, bytes_out, len_out, interp);
        }

        Value* err = deduce_seminaive_accumulate_component_additions(
            plan,
            component_id,
            next_delta_sets,
            accumulated_delta_sets,
            interp
        );
        if (err != null) return err;

        err = deduce_seminaive_accumulate_component_visible_additions(
            plan,
            component_id,
            next_delta_sets,
            scratch_visible_sets,
            interp
        );
        if (err != null) return err;

        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}
