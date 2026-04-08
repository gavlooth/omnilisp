module lisp;

import std::core::mem;

fn Value* deduce_component_project_relation_head_demand(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* requested_head_demand,
    DeduceRuleHeadDemand* out_projected_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* out_has_any_bound,
    Interp* interp
) {
    if (out_projected_head_demand == null || out_term_bound == null || out_term_values == null ||
        out_has_any_bound == null) {
        return deduce_raise(interp, "deduce/goal-directed-invalid-state", "deduce: invalid projected head demand state");
    }

    *out_projected_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *out_has_any_bound = false;

    if (db == null || plan == null || requested_head_demand == null) return null;
    if (requested_head_demand.term_count == 0 || requested_head_demand.term_bound == null) return null;

    bool[] term_bound = mem::new_array(bool, requested_head_demand.term_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }
    Value*[] term_values = mem::new_array(Value*, requested_head_demand.term_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < requested_head_demand.term_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;

        if (!requested_head_demand.term_bound[i]) continue;
        if (!deduce_component_position_can_use_relation_head_demand(
                db,
                plan,
                component_id,
                requested_head_demand.predicate,
                i
            )) {
            continue;
        }
        term_bound[i] = true;
        term_values[i] = requested_head_demand.term_values != null ? requested_head_demand.term_values[i] : null;
        has_any_bound = true;
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_has_any_bound = true;
    *out_projected_head_demand = {
        .predicate = requested_head_demand.predicate,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = requested_head_demand.term_count,
    };
    return null;
}

fn Value* deduce_component_project_relation_head_demand_for_query(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* requested_head_demand,
    DeduceRuleHeadDemand* out_projected_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* out_has_any_bound,
    Interp* interp
) {
    if (requested_head_demand == null || requested_head_demand.term_bound == null) {
        return deduce_component_project_relation_head_demand(
            db,
            plan,
            component_id,
            requested_head_demand,
            out_projected_head_demand,
            out_term_bound,
            out_term_values,
            out_has_any_bound,
            interp
        );
    }

    if (out_projected_head_demand == null || out_term_bound == null || out_term_values == null ||
        out_has_any_bound == null) {
        return deduce_raise(interp, "deduce/goal-directed-invalid-state", "deduce: invalid projected head demand state");
    }

    *out_projected_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *out_has_any_bound = false;

    if (db == null || plan == null || requested_head_demand == null) return null;
    if (requested_head_demand.term_count == 0 || requested_head_demand.term_bound == null) return null;

    bool[] term_bound = mem::new_array(bool, requested_head_demand.term_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }
    Value*[] term_values = mem::new_array(Value*, requested_head_demand.term_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/out-of-memory", "deduce: out of memory projecting head demand");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < requested_head_demand.term_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;

        if (!requested_head_demand.term_bound[i]) continue;
        if (!deduce_component_position_can_use_relation_head_demand_for_query(
                db,
                plan,
                component_id,
                requested_head_demand.predicate,
                i
            )) {
            continue;
        }
        term_bound[i] = true;
        term_values[i] = requested_head_demand.term_values != null ? requested_head_demand.term_values[i] : null;
        has_any_bound = true;
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_term_bound = term_bound;
    *out_term_values = term_values;
    *out_has_any_bound = true;
    *out_projected_head_demand = {
        .predicate = requested_head_demand.predicate,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = requested_head_demand.term_count,
    };
    return null;
}
