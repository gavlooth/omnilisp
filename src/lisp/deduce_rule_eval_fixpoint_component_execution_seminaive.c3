module lisp;

import std::core::mem;

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

    return deduce_seminaive_run_recursive_component_iterations_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        head_demand,
        iterations_out,
        derived_out,
        observed_rules,
        current_delta_sets,
        next_delta_sets,
        interp
    );
}
