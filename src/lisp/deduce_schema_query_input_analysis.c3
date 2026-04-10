module lisp;

import std::core::mem;

struct DeduceQueryHeadDemandCases {
    DeduceRuleHeadDemand* entries;
    usz                   count;
    usz                   capacity;
}

fn void deduce_query_head_demand_cases_release(DeduceQueryHeadDemandCases* cases) {
    if (cases == null) return;
    if (cases.entries != null) {
        for (usz i = 0; i < cases.count; i++) {
            deduce_rule_head_demand_release(&cases.entries[i]);
        }
        mem::free(cases.entries);
    }
    *cases = {};
}

fn Value* deduce_query_head_demand_cases_append_take(
    DeduceQueryHeadDemandCases* cases,
    DeduceRuleHeadDemand* demand,
    Interp* interp
) {
    if (cases == null || demand == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand case state");
    }

    for (usz i = 0; i < cases.count; i++) {
        DeduceRuleHeadDemand* existing = &cases.entries[i];
        if (existing.predicate != demand.predicate || existing.term_count != demand.term_count ||
            existing.term_bound == null || existing.term_values == null ||
            demand.term_bound == null || demand.term_values == null) {
            continue;
        }

        bool same = true;
        for (usz j = 0; j < demand.term_count; j++) {
            if (existing.term_bound[j] != demand.term_bound[j]) {
                same = false;
                break;
            }
            if (demand.term_bound[j] && !values_equal(existing.term_values[j], demand.term_values[j])) {
                same = false;
                break;
            }
        }
        if (same) {
            deduce_rule_head_demand_release(demand);
            return null;
        }
    }

    if (cases.count == cases.capacity) {
        if (cases.capacity > usz.max / 2) {
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
        }
        usz new_cap = cases.capacity > 0 ? cases.capacity * 2 : 4;
        if (new_cap > usz.max / DeduceRuleHeadDemand.sizeof) {
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
        }
        DeduceRuleHeadDemand* next =
            (DeduceRuleHeadDemand*)mem::realloc(cases.entries, DeduceRuleHeadDemand.sizeof * new_cap);
        if (next == null) {
            return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
        }
        cases.entries = next;
        cases.capacity = new_cap;
    }

    cases.entries[cases.count++] = *demand;
    *demand = {};
    return null;
}
