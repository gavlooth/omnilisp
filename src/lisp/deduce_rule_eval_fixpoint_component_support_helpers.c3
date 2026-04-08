module lisp;

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

fn bool deduce_component_position_can_use_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    bool matched_head_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;
        matched_head_rule = true;
        bool saw_positive_self_recursive = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_predicates == null || sig.body_predicates[body_index] != predicate) continue;
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            saw_positive_self_recursive = true;
            if (!deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, predicate, head_term_index)) {
                return false;
            }
        }
    }
    return matched_head_rule;
}

fn bool deduce_component_position_can_use_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    bool matched_head_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;
        matched_head_rule = true;
        bool saw_any_positive_mention = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;
            if (sig.body_predicates[body_index] == predicate) {
                if (!deduce_rule_body_atom_supports_query_head_demand_position(sig, body_index, predicate, head_term_index)) {
                    return false;
                }
                if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) continue;
                usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
                usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
                uint head_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[head_term_index] : 0;
                for (usz j = 0; j < body_count; j++) {
                    usz body_term_index = body_offset + j;
                    if (!sig.body_term_is_variable[body_term_index]) continue;
                    if (sig.body_term_var_ids[body_term_index] == head_var_id) {
                        saw_any_positive_mention = true;
                        break;
                    }
                }
                continue;
            }
            if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) continue;
            usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
            usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
            uint head_var_id = sig.head_term_var_ids != null ? sig.head_term_var_ids[head_term_index] : 0;
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] == head_var_id) {
                    saw_any_positive_mention = true;
                    break;
                }
            }
        }
        if (saw_any_positive_mention) return true;
    }
    return false;
}

fn bool deduce_component_can_use_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            if (!deduce_component_position_can_use_relation_head_demand(
                    db,
                    plan,
                    component_id,
                    head_demand.predicate,
                    i
                )) {
                return false;
            }
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_can_use_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            if (!deduce_component_position_can_use_relation_head_demand_for_query(
                    db,
                    plan,
                    component_id,
                    head_demand.predicate,
                    i
                )) {
                return false;
            }
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_can_use_relation_head_demand_at_same_indices(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* head_demand
) {
    if (db == null || plan == null || head_demand == null) return false;

    bool has_bound_positions = false;
    for (usz i = 0; i < head_demand.term_count; i++) {
        if (head_demand.term_bound != null && head_demand.term_bound[i]) {
            has_bound_positions = true;
            break;
        }
    }
    if (!has_bound_positions) return false;

    bool saw_positive_recursive_rule = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];

        bool rule_has_positive_recursive_body = false;
        bool rule_preserves_same_indices = false;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;

            SymbolId body_predicate = sig.body_predicates[body_index];
            bool body_is_recursive_in_component = false;
            for (usz j = 0; j < db.rule_signature_count; j++) {
                if (j >= plan.rule_component_ids.len || plan.rule_component_ids[j] != component_id) continue;
                if (db.rule_signatures[j].head_predicate != body_predicate) continue;
                body_is_recursive_in_component = true;
                break;
            }
            if (!body_is_recursive_in_component) continue;

            rule_has_positive_recursive_body = true;
            if (deduce_rule_body_atom_preserves_head_demand_at_same_indices(sig, body_index, head_demand)) {
                rule_preserves_same_indices = true;
                break;
            }
        }
        if (!rule_has_positive_recursive_body) continue;
        saw_positive_recursive_rule = true;
        if (!rule_preserves_same_indices) return false;
    }
    return saw_positive_recursive_rule;
}

fn bool deduce_component_head_rule_has_multiple_recursive_body_atoms(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate
) {
    if (db == null || plan == null) return false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

        usz recursive_body_count = 0;
        for (usz body_index = 0; body_index < sig.body_count; body_index++) {
            if (sig.body_negated != null && sig.body_negated[body_index]) continue;
            if (sig.body_predicates == null) continue;
            if (!deduce_component_contains_head_predicate(
                    db,
                    plan,
                    component_id,
                    sig.body_predicates[body_index]
                )) {
                continue;
            }
            recursive_body_count++;
            if (recursive_body_count > 1) return true;
        }
    }
    return false;
}
