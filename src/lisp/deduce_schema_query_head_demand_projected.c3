module lisp;

fn bool deduce_query_head_demand_try_get_single_bound_position(
    DeduceRuleHeadDemand* demand,
    usz* out_position,
    Value** out_value
) {
    if (demand == null || out_position == null || out_value == null) return false;
    if (demand.term_bound == null || demand.term_values == null || demand.term_count == 0) return false;

    bool found = false;
    usz position = 0;
    Value* value = null;
    for (usz i = 0; i < demand.term_count; i++) {
        if (!demand.term_bound[i]) continue;
        if (found) return false;
        found = true;
        position = i;
        value = demand.term_values[i];
    }
    if (!found || value == null) return false;
    *out_position = position;
    *out_value = value;
    return true;
}

fn usz deduce_query_head_demand_bound_position_count(DeduceRuleHeadDemand* demand) {
    if (demand == null || demand.term_bound == null || demand.term_count == 0) return 0;

    usz count = 0;
    for (usz i = 0; i < demand.term_count; i++) {
        if (demand.term_bound[i]) count++;
    }
    return count;
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_relaxation(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    if (deduce_component_head_rule_has_multiple_recursive_body_atoms(
            db,
            plan,
            component_id,
            projected_head_demand.predicate
        )) {
        return false;
    }

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return true;

    return !deduce_component_can_use_relation_head_demand_at_same_indices(
        db,
        plan,
        component_id,
        projected_head_demand
    );
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_fallback(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return true;

    if (deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        return false;
    }

    if (deduce_component_head_rule_has_multiple_recursive_body_atoms(
            db,
            plan,
            component_id,
            projected_head_demand.predicate
        )) {
        return true;
    }

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (projected_head_demand.term_bound == null || !projected_head_demand.term_bound[i]) continue;
        if (deduce_component_position_has_same_index_recursive_carrier(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            return false;
        }
    }
    return true;
}

fn bool deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (projected_head_demand == null || projected_head_demand.term_bound == null || projected_head_demand.term_count == 0) {
        return false;
    }

    if (deduce_component_head_rule_has_multiple_recursive_body_atoms(
            db,
            plan,
            component_id,
            projected_head_demand.predicate
        )) {
        return false;
    }

    bool same_index_only = false;
    if (deduce_component_can_use_relation_head_demand_for_query(
            db,
            plan,
            component_id,
            projected_head_demand
        ) && !deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        same_index_only = true;
    }

    usz kept_position = projected_head_demand.term_count;
    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (!projected_head_demand.term_bound[i]) continue;
        if (same_index_only) {
            if (!deduce_component_position_has_same_index_recursive_carrier(
                    db,
                    plan,
                    component_id,
                    projected_head_demand.predicate,
                    i
                )) {
                continue;
            }
        } else if (!deduce_component_position_can_use_relation_head_demand_for_query(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            continue;
        }
        kept_position = i;
        break;
    }
    if (kept_position >= projected_head_demand.term_count) return false;

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (i == kept_position || !projected_head_demand.term_bound[i]) continue;
        projected_head_demand.term_bound[i] = false;
        if (projected_head_demand.term_values != null) projected_head_demand.term_values[i] = null;
    }
    return true;
}
