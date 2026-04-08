module lisp;

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
