module lisp;

import std::core::mem;

struct DeduceRuleHeadDemand {
    SymbolId predicate;
    bool*    term_bound;
    Value**  term_values;
    usz      term_count;
}

fn void deduce_rule_head_demand_release(DeduceRuleHeadDemand* demand) {
    if (demand == null) return;
    if (demand.term_bound != null) {
        mem::free(demand.term_bound);
        demand.term_bound = null;
    }
    if (demand.term_values != null) {
        mem::free(demand.term_values);
        demand.term_values = null;
    }
    demand.predicate = (SymbolId)0;
    demand.term_count = 0;
}

struct DeduceNaiveRuleExec {
    DeduceDb*             db;
    DeduceRuleSignature*  sig;
    MdbTxn*               txn;
    DeduceRuleHeadDemand* head_demand;
    MdbDbi                head_dbi;
    DeduceRelationSchema* head_schema;
    Value*[]              head_tuple;
    DeduceAggregateGroups* aggregate_groups;
    void*                 observed_rule_counters;
    bool                  dry_run;
    long*                 derived_out;
    bool*                 changed_out;
    Interp*               interp;
}

struct DeduceSemiNaiveRuleExec {
    DeduceDb*             db;
    DeduceRuleSignature*  sig;
    MdbTxn*               txn;
    DeduceRuleHeadDemand* head_demand;
    MdbDbi                head_dbi;
    DeduceRelationSchema* head_schema;
    Value*[]              head_tuple;
    DeduceAggregateGroups* aggregate_groups;
    DeduceSccPlan*        plan;
    usz                   component_id;
    DeduceSignedDeltaSet[] current_delta_sets;
    DeduceSignedDeltaSet[] next_delta_sets;
    DeduceDeltaTupleSet[] scratch_visible_sets;
    DeduceTupleSupportTable[] support_tables;
    Value*                proof_seen_map;
    bool                  anchor_active;
    usz                   anchor_step;
    bool                  anchor_delta_is_removal;
    bool                  aggregate_component_mode;
    void*                 observed_rule_counters;
    bool                  dry_run;
    long*                 derived_out;
    bool*                 changed_out;
    Interp*               interp;
}

fn bool deduce_rule_step_has_bound_join_input(
    DeduceRuleSignature* sig,
    usz step_idx,
    bool[] var_bound
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null) return false;

    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        if (!sig.body_term_is_variable[term_idx]) continue;
        uint var_id = sig.body_term_var_ids[term_idx];
        if (var_id < var_bound.len && var_bound[var_id]) return true;
    }
    return false;
}

fn bool deduce_rule_apply_head_demand(
    DeduceRuleSignature* sig,
    DeduceRuleHeadDemand* demand,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || demand == null) return true;
    if (demand.predicate == (SymbolId)0 || sig.head_predicate != demand.predicate) return true;
    if (sig.head_term_count == 0) return true;
    if (demand.term_count != sig.head_term_count) return false;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null || sig.head_term_literals == null) {
        return false;
    }
    if ((demand.term_bound == null && demand.term_count > 0) ||
        (demand.term_values == null && demand.term_count > 0)) {
        return false;
    }

    for (usz i = 0; i < demand.term_count; i++) {
        if (!demand.term_bound[i]) continue;
        Value* demand_value = demand.term_values[i];
        switch (sig.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_LITERAL:
                if (!values_equal(sig.head_term_literals[i], demand_value)) return false;
                break;
            case DEDUCE_RULE_TERM_VARIABLE:
                uint var_id = sig.head_term_var_ids[i];
                if (var_id >= var_bound.len || var_id >= var_values.len) return false;
                if (var_bound[var_id]) {
                    if (!values_equal(var_values[var_id], demand_value)) return false;
                } else {
                    var_bound[var_id] = true;
                    var_values[var_id] = demand_value;
                }
                break;
            case DEDUCE_RULE_TERM_AGGREGATE:
                return false;
        }
    }

    return true;
}
