module lisp;

import std::core::mem;

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
    bool[] processed = mem::new_array(bool, db.rule_signature_count);
    if (processed.ptr == null && db.rule_signature_count > 0) {
        mdb_txn_abort(txn);
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
        if (err != null) {
            mdb_txn_abort(txn);
            return err;
        }
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

    Value* seed_err = deduce_seminaive_seed_component_delta_sets(db, plan, component_id, current_delta_sets, interp);
    if (seed_err != null) return seed_err;

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        MdbTxn* txn = null;
        int rc = mdb_txn_begin(db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
        }

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
                &derived,
                &changed,
                observed_rule,
                interp
            );
            if (err != null) {
                mdb_txn_abort(txn);
                return err;
            }
        }

        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            deduce_signed_delta_set_release(&current_delta_sets[i]);
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = {};
        }
        if (!has_delta && !changed) return null;
        if (!has_delta) return null;
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
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
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
        if (!changed) return null;
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
