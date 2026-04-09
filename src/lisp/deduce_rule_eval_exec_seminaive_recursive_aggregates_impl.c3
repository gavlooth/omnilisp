module lisp;

import std::core::mem;

fn Value* deduce_seminaive_run_recursive_aggregate_component_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    MdbTxn* txn,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid recursive aggregate state");
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

    DeduceTupleSupportTable[] support_tables = mem::new_array(DeduceTupleSupportTable, plan.predicates.len);
    if (support_tables.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer {
        if (support_tables.ptr != null) {
            for (usz i = 0; i < support_tables.len; i++) deduce_tuple_support_table_release(&support_tables[i]);
            mem::free(support_tables.ptr);
        }
    }

    isz[] aggregate_rule_batch_ids = mem::new_array(isz, db.rule_signature_count);
    if (aggregate_rule_batch_ids.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (aggregate_rule_batch_ids.ptr != null) mem::free(aggregate_rule_batch_ids.ptr);
    for (usz i = 0; i < aggregate_rule_batch_ids.len; i++) aggregate_rule_batch_ids[i] = -1;

    DeduceSemiNaiveAggregateBatch* batches = null;
    usz batch_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (!deduce_rule_signature_has_aggregates(sig)) continue;

        isz batch_id = -1;
        for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
            if (deduce_aggregate_signatures_share_head_batch(batches[batch_idx].sig, sig)) {
                batch_id = (isz)batch_idx;
                break;
            }
        }
        if (batch_id >= 0) {
            aggregate_rule_batch_ids[i] = batch_id;
            continue;
        }

        DeduceSemiNaiveAggregateBatch* next_batches =
            (DeduceSemiNaiveAggregateBatch*)mem::realloc(
                batches,
                DeduceSemiNaiveAggregateBatch.sizeof * (batch_count + 1)
            );
        if (next_batches == null) {
            for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
                deduce_aggregate_groups_release(&batches[batch_idx].groups);
            }
            if (batches != null) mem::free(batches);
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        batches = next_batches;
        batches[batch_count] = {};
        batches[batch_count].sig = sig;
        aggregate_rule_batch_ids[i] = (isz)batch_count;
        batch_count++;
    }
    defer {
        if (batches != null) {
            for (usz batch_idx = 0; batch_idx < batch_count; batch_idx++) {
                deduce_aggregate_groups_release(&batches[batch_idx].groups);
            }
            mem::free(batches);
        }
    }

    bool[] processed = mem::new_array(bool, db.rule_signature_count);
    if (processed.ptr == null && db.rule_signature_count > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (processed.ptr != null) mem::free(processed.ptr);

    Value* err = deduce_seminaive_clear_component_predicates(db, plan, component_id, txn, interp);
    if (err == null) {
        err = deduce_seminaive_seed_component_extensional_state_in_txn(
            db,
            plan,
            component_id,
            txn,
            next_delta_sets,
            support_tables,
            true,
            interp
        );
    }
    if (err == null) {
        for (usz i = 0; i < processed.len; i++) processed[i] = false;
        err = deduce_seminaive_execute_component_rule_pass(
            db,
            plan,
            component_id,
            txn,
            current_delta_sets,
            next_delta_sets,
            support_tables,
            batches,
            batch_count,
            aggregate_rule_batch_ids,
            processed,
            true,
            true,
            derived_out,
            observed_rules,
            interp
        );
    }
    for (usz batch_idx = 0; batch_idx < batch_count && err == null; batch_idx++) {
        err = deduce_seminaive_finalize_aggregate_groups_for_signature(
            db,
            plan,
            component_id,
            batches[batch_idx].sig,
            txn,
            &batches[batch_idx].groups,
            next_delta_sets,
            support_tables,
            derived_out,
            null,
            interp
        );
    }
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }
    if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;

    for (usz i = 0; i < plan.predicates.len; i++) {
        current_delta_sets[i] = next_delta_sets[i];
        next_delta_sets[i] = {};
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 1; iter < limit; iter++) {
        if (!deduce_seminaive_component_has_delta(plan, component_id, current_delta_sets)) {
            Value* validate_err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
            return validate_err;
        }

        for (usz i = 0; i < processed.len; i++) processed[i] = false;
        err = deduce_seminaive_execute_component_rule_pass(
            db,
            plan,
            component_id,
            txn,
            current_delta_sets,
            next_delta_sets,
            support_tables,
            batches,
            batch_count,
            aggregate_rule_batch_ids,
            processed,
            false,
            false,
            derived_out,
            observed_rules,
            interp
        );

        for (usz batch_idx = 0; batch_idx < batch_count && err == null; batch_idx++) {
            err = deduce_seminaive_finalize_aggregate_groups_for_signature(
                db,
                plan,
                component_id,
                batches[batch_idx].sig,
                txn,
                &batches[batch_idx].groups,
                next_delta_sets,
                support_tables,
                derived_out,
                null,
                interp
            );
        }

        if (err != null) {
            mdb_txn_abort(txn);
            return err;
        }
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
        if (!has_delta) {
            Value* validate_err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
            return validate_err;
        }
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit",
        "deduce/analyze: recursive stratum iteration limit reached", data);
}
