module lisp;

fn bool deduce_rule_body_atom_preserves_head_demand(
    DeduceRuleSignature* sig,
    usz body_index,
    DeduceRuleHeadDemand* head_demand
) {
    if (sig == null || head_demand == null || body_index >= sig.body_count) return false;
    if (head_demand.term_bound == null) return true;

    for (usz i = 0; i < head_demand.term_count; i++) {
        if (!head_demand.term_bound[i]) continue;
        if (!deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, head_demand.predicate, i)) {
            return false;
        }
    }
    return true;
}

fn bool deduce_rule_body_atom_preserves_head_demand_position(
    DeduceRuleSignature* sig,
    usz body_index,
    SymbolId predicate,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (sig.body_predicates == null || sig.body_predicates[body_index] != predicate) return false;
    if (head_term_index >= sig.head_term_count) return false;

    usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
    usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
    if (body_count < sig.head_term_count) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds != null
        ? sig.head_term_kinds[head_term_index]
        : DEDUCE_RULE_TERM_LITERAL;
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE: {
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            uint head_var_id = sig.head_term_var_ids[head_term_index];
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] == head_var_id) return true;
            }
            return false;
        }
        case DEDUCE_RULE_TERM_LITERAL:
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}

fn bool deduce_rule_body_atom_preserves_head_demand_position_at_same_index(
    DeduceRuleSignature* sig,
    usz body_index,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (head_term_index >= sig.head_term_count) return false;

    usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
    usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
    if (head_term_index >= body_count) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds != null
        ? sig.head_term_kinds[head_term_index]
        : DEDUCE_RULE_TERM_LITERAL;
    usz body_term_index = body_offset + head_term_index;
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE:
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            return sig.body_term_is_variable[body_term_index] &&
                sig.body_term_var_ids[body_term_index] == sig.head_term_var_ids[head_term_index];
        case DEDUCE_RULE_TERM_LITERAL:
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}

fn bool deduce_rule_body_atom_preserves_head_demand_at_same_indices(
    DeduceRuleSignature* sig,
    usz body_index,
    DeduceRuleHeadDemand* head_demand
) {
    if (sig == null || head_demand == null || head_demand.term_bound == null) return false;

    bool has_bound_positions = false;
    for (usz head_index = 0; head_index < head_demand.term_count; head_index++) {
        if (!head_demand.term_bound[head_index]) continue;
        has_bound_positions = true;
        if (!deduce_rule_body_atom_preserves_head_demand_position_at_same_index(sig, body_index, head_index)) {
            return false;
        }
    }
    return has_bound_positions;
}

fn bool deduce_component_contains_head_predicate(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate
) {
    if (db == null || plan == null) return false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        if (db.rule_signatures[i].head_predicate == predicate) return true;
    }
    return false;
}

fn bool deduce_component_position_has_same_index_recursive_carrier(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    SymbolId predicate,
    usz head_term_index
) {
    if (db == null || plan == null) return false;

    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

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
            if (deduce_rule_body_atom_preserves_head_demand_position_at_same_index(sig, body_index, head_term_index)) {
                return true;
            }
        }
    }
    return false;
}

fn bool deduce_rule_body_atom_supports_query_head_demand_position(
    DeduceRuleSignature* sig,
    usz body_index,
    SymbolId predicate,
    usz head_term_index
) {
    if (sig == null || body_index >= sig.body_count) return false;
    if (sig.body_negated != null && sig.body_negated[body_index]) return false;
    if (head_term_index >= sig.head_term_count) return false;
    if (sig.body_predicates == null) return false;

    SymbolId body_predicate = sig.body_predicates[body_index];
    if (sig.head_term_kinds == null) return false;

    DeduceRuleTermKind head_kind = sig.head_term_kinds[head_term_index];
    switch (head_kind) {
        case DEDUCE_RULE_TERM_VARIABLE: {
            if (sig.head_term_var_ids == null || sig.body_term_is_variable == null || sig.body_term_var_ids == null) {
                return false;
            }
            uint head_var_id = sig.head_term_var_ids[head_term_index];
            bool saw_head_var = false;
            usz body_offset = sig.body_term_offsets != null ? sig.body_term_offsets[body_index] : 0;
            usz body_count = sig.body_term_counts != null ? sig.body_term_counts[body_index] : 0;
            for (usz j = 0; j < body_count; j++) {
                usz body_term_index = body_offset + j;
                if (!sig.body_term_is_variable[body_term_index]) continue;
                if (sig.body_term_var_ids[body_term_index] != head_var_id) continue;
                saw_head_var = true;
                if (body_predicate == predicate) {
                    return deduce_rule_body_atom_preserves_head_demand_position(sig, body_index, predicate, head_term_index);
                }
                break;
            }
            return saw_head_var;
        }
        case DEDUCE_RULE_TERM_LITERAL:
            return true;
        case DEDUCE_RULE_TERM_AGGREGATE:
            return false;
    }
}
