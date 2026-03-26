module lisp;

import std::core::mem;
import std::collections::list;

struct DeduceAggregateTermState {
    bool   initialized;
    bool   sum_is_double;
    long   count_value;
    long   sum_int_value;
    double sum_double_value;
    Value* extremum_value;
}

struct DeduceAggregateGroup {
    char*                    key_bytes;
    usz                      key_len;
    Value**                  head_values;
    Value**                  term_support_counts;
    DeduceAggregateTermState* term_states;
    DeduceAggregateTermState* previous_term_states;
    long                     support_count;
    long                     previous_support_count;
    bool                     materialized;
    bool                     dirty;
}

struct DeduceAggregateGroups {
    DeduceAggregateGroup* entries;
    usz                  count;
    usz                  capacity;
}

struct DeduceEncodedTuple {
    char* key_bytes;
    usz   key_len;
}

struct DeduceDeltaTupleSet {
    Value*            seen_map;
    DeduceEncodedTuple* tuples;
    usz               count;
    usz               capacity;
}

struct DeduceSignedDeltaSet {
    DeduceDeltaTupleSet additions;
    DeduceDeltaTupleSet removals;
}

const uint DEDUCE_COMPONENT_DELTA_PAYLOAD_MAGIC = 0x50444344u; // "DDCP"
const uint DEDUCE_COMPONENT_DELTA_PAYLOAD_VERSION = 1u;

struct DeduceSerializedComponentDeltaEntry {
    usz                  predicate_index;
    SymbolId             predicate;
    DeduceSignedDeltaSet delta_set;
}

struct DeduceSerializedComponentDeltaPayload {
    usz                                component_id;
    DeduceSerializedComponentDeltaEntry* entries;
    usz                                entry_count;
}

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

struct DeduceTupleSupportTable {
    Value* base_counts;
    Value* recursive_counts;
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

struct DeduceObservedStepCounters {
    long rows_read;
    long rows_emitted;
    long index_hits;
    long index_misses;
    long join_probes;
}

struct DeduceObservedRuleCounters {
    DeduceObservedStepCounters* steps;
    usz                         step_count;
}

fn DeduceObservedStepCounters* deduce_observed_step_counters_ref(
    void* observed_rule_counters,
    usz step_idx
) @inline {
    if (observed_rule_counters == null) return null;
    DeduceObservedRuleCounters* rule = (DeduceObservedRuleCounters*)observed_rule_counters;
    if (rule.steps == null || step_idx >= rule.step_count) return null;
    return &rule.steps[step_idx];
}

fn void deduce_observed_step_note_rows_read(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.rows_read < long.max) step.rows_read++;
}

fn void deduce_observed_step_note_rows_emitted(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.rows_emitted < long.max) step.rows_emitted++;
}

fn void deduce_observed_step_note_join_probe(void* observed_rule_counters, usz step_idx) @inline {
    DeduceObservedStepCounters* step = deduce_observed_step_counters_ref(observed_rule_counters, step_idx);
    if (step != null && step.join_probes < long.max) step.join_probes++;
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

fn bool deduce_rule_build_step_prefix_probe(
    DeduceRuleSignature* sig,
    usz step_idx,
    bool[] var_bound,
    Value*[] var_values,
    Value*[] out_prefix_values,
    usz* out_prefix_len
) {
    if (out_prefix_len == null) return false;
    *out_prefix_len = 0;
    if (sig == null || step_idx >= sig.body_count || out_prefix_values.ptr == null) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) return false;

    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > out_prefix_values.len) return false;

    usz prefix_len = 0;
    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) break;
            out_prefix_values[prefix_len++] = var_values[var_id];
            continue;
        }
        out_prefix_values[prefix_len++] = sig.body_term_literals[term_idx];
    }

    *out_prefix_len = prefix_len;
    return prefix_len > 0;
}

fn bool deduce_tuple_matches_prefix(
    Value*[] tuple_vals,
    Value*[] prefix_values,
    usz prefix_len
) @inline {
    if (prefix_len == 0) return true;
    if (tuple_vals.len < prefix_len || prefix_values.len < prefix_len) return false;
    for (usz i = 0; i < prefix_len; i++) {
        if (!values_equal(tuple_vals[i], prefix_values[i])) return false;
    }
    return true;
}

fn void deduce_observed_rule_counters_reset(DeduceObservedRuleCounters* counters) {
    if (counters == null || counters.steps == null) return;
    for (usz i = 0; i < counters.step_count; i++) {
        counters.steps[i] = {};
    }
}

fn bool deduce_bytes_equal(char* left, usz left_len, char* right, usz right_len) @inline {
    if (left_len != right_len) return false;
    for (usz i = 0; i < left_len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn void deduce_encoded_tuples_release(DeduceEncodedTuple* tuples, usz count) {
    if (tuples == null) return;
    for (usz i = 0; i < count; i++) {
        if (tuples[i].key_bytes != null) {
            mem::free(tuples[i].key_bytes);
            tuples[i].key_bytes = null;
        }
        tuples[i].key_len = 0;
    }
    mem::free(tuples);
}

fn void deduce_delta_set_release(DeduceDeltaTupleSet* set) {
    if (set == null) return;
    if (set.tuples != null) {
        deduce_encoded_tuples_release(set.tuples, set.count);
        set.tuples = null;
    }
    set.seen_map = null;
    set.count = 0;
    set.capacity = 0;
}

fn bool deduce_delta_set_append_encoded_tuple(
    DeduceDeltaTupleSet* set,
    void* key_data,
    usz key_len
) {
    if (set == null || (key_data == null && key_len > 0)) return false;

    usz needed = set.count + 1;
    if (needed > set.capacity) {
        usz new_cap = set.capacity > 0 ? set.capacity : 4;
        while (new_cap < needed) {
            if (new_cap > usz.max / 2) {
                new_cap = needed;
                break;
            }
            new_cap *= 2;
        }
        DeduceEncodedTuple* next =
            (DeduceEncodedTuple*)mem::realloc(set.tuples, DeduceEncodedTuple.sizeof * new_cap);
        if (next == null) return false;
        set.tuples = next;
        set.capacity = new_cap;
    }

    char* copied = null;
    if (key_len > 0) {
        copied = (char*)mem::malloc(key_len);
        if (copied == null) return false;
        mem::copy(copied, (char*)key_data, key_len);
    }

    set.tuples[set.count] = { .key_bytes = copied, .key_len = key_len };
    set.count++;
    return true;
}

fn void deduce_signed_delta_set_release(DeduceSignedDeltaSet* set) {
    if (set == null) return;
    deduce_delta_set_release(&set.additions);
    deduce_delta_set_release(&set.removals);
}

fn void deduce_serialized_component_delta_payload_release(DeduceSerializedComponentDeltaPayload* payload) {
    if (payload == null) return;
    if (payload.entries != null) {
        for (usz i = 0; i < payload.entry_count; i++) {
            deduce_signed_delta_set_release(&payload.entries[i].delta_set);
        }
        mem::free(payload.entries);
        payload.entries = null;
    }
    payload.component_id = 0;
    payload.entry_count = 0;
}

fn bool deduce_component_delta_payload_ensure_capacity(char** bytes_ref, usz* capacity_ref, usz needed) {
    if (bytes_ref == null || capacity_ref == null) return false;
    if (needed <= *capacity_ref) return true;

    usz next_capacity = *capacity_ref > 0 ? *capacity_ref : 64;
    while (next_capacity < needed) {
        if (next_capacity > usz.max / 2) {
            next_capacity = needed;
            break;
        }
        next_capacity *= 2;
    }

    char* next = *bytes_ref == null
        ? (char*)mem::malloc(next_capacity)
        : (char*)mem::realloc(*bytes_ref, next_capacity);
    if (next == null) return false;
    *bytes_ref = next;
    *capacity_ref = next_capacity;
    return true;
}

fn bool deduce_component_delta_payload_append_bytes(
    char** bytes_ref,
    usz* len_ref,
    usz* capacity_ref,
    char* payload,
    usz payload_len
) {
    if (bytes_ref == null || len_ref == null || capacity_ref == null) return false;
    if (payload == null && payload_len > 0) return false;
    if (*len_ref > usz.max - payload_len) return false;
    usz next_len = *len_ref + payload_len;
    if (!deduce_component_delta_payload_ensure_capacity(bytes_ref, capacity_ref, next_len)) return false;
    if (payload_len > 0) mem::copy(*bytes_ref + *len_ref, payload, payload_len);
    *len_ref = next_len;
    return true;
}

fn bool deduce_component_delta_payload_append_u32(
    char** bytes_ref,
    usz* len_ref,
    usz* capacity_ref,
    uint value
) {
    char[4] encoded = {
        (char)(value & 0xFF),
        (char)((value >> 8) & 0xFF),
        (char)((value >> 16) & 0xFF),
        (char)((value >> 24) & 0xFF)
    };
    return deduce_component_delta_payload_append_bytes(bytes_ref, len_ref, capacity_ref, &encoded[0], encoded.len);
}

fn bool deduce_component_delta_payload_append_u64(
    char** bytes_ref,
    usz* len_ref,
    usz* capacity_ref,
    ulong value
) {
    char[8] encoded = {
        (char)(value & 0xFF),
        (char)((value >> 8) & 0xFF),
        (char)((value >> 16) & 0xFF),
        (char)((value >> 24) & 0xFF),
        (char)((value >> 32) & 0xFF),
        (char)((value >> 40) & 0xFF),
        (char)((value >> 48) & 0xFF),
        (char)((value >> 56) & 0xFF)
    };
    return deduce_component_delta_payload_append_bytes(bytes_ref, len_ref, capacity_ref, &encoded[0], encoded.len);
}

fn bool deduce_component_delta_payload_read_u32(char[] bytes, usz* pos_ref, uint* out) {
    if (pos_ref == null || out == null) return false;
    if (*pos_ref > bytes.len || bytes.len - *pos_ref < 4) return false;
    char* cursor = bytes.ptr + *pos_ref;
    *out =
        ((uint)(char)cursor[0]) |
        (((uint)(char)cursor[1]) << 8) |
        (((uint)(char)cursor[2]) << 16) |
        (((uint)(char)cursor[3]) << 24);
    *pos_ref += 4;
    return true;
}

fn bool deduce_component_delta_payload_read_u64(char[] bytes, usz* pos_ref, ulong* out) {
    if (pos_ref == null || out == null) return false;
    if (*pos_ref > bytes.len || bytes.len - *pos_ref < 8) return false;
    char* cursor = bytes.ptr + *pos_ref;
    *out =
        ((ulong)(char)cursor[0]) |
        (((ulong)(char)cursor[1]) << 8) |
        (((ulong)(char)cursor[2]) << 16) |
        (((ulong)(char)cursor[3]) << 24) |
        (((ulong)(char)cursor[4]) << 32) |
        (((ulong)(char)cursor[5]) << 40) |
        (((ulong)(char)cursor[6]) << 48) |
        (((ulong)(char)cursor[7]) << 56);
    *pos_ref += 8;
    return true;
}

fn Value* deduce_component_delta_payload_append_tuple_set(
    char** bytes_ref,
    usz* len_ref,
    usz* capacity_ref,
    DeduceDeltaTupleSet* set,
    Interp* interp
) {
    if (set == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta set");
    }
    if (set.count > uint.max) {
        return deduce_raise(interp, "deduce/parallel-component-delta-too-large",
            "deduce/parallel: component delta tuple count exceeds payload format");
    }
    if (!deduce_component_delta_payload_append_u32(bytes_ref, len_ref, capacity_ref, (uint)set.count)) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < set.count; i++) {
        DeduceEncodedTuple tuple = set.tuples[i];
        if (tuple.key_len > uint.max) {
            return deduce_raise(interp, "deduce/parallel-component-delta-too-large",
                "deduce/parallel: component delta tuple key exceeds payload format");
        }
        if (!deduce_component_delta_payload_append_u32(bytes_ref, len_ref, capacity_ref, (uint)tuple.key_len) ||
            !deduce_component_delta_payload_append_bytes(bytes_ref, len_ref, capacity_ref, tuple.key_bytes, tuple.key_len)) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    return null;
}

// Versioned worker-scratch payload for routing one component's signed deltas
// through the existing scheduler byte-result seam.
fn Value* deduce_component_delta_payload_serialize(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] delta_sets,
    char** bytes_out,
    usz* len_out,
    Interp* interp
) {
    if (bytes_out == null || len_out == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta payload state");
    }
    *bytes_out = null;
    *len_out = 0;

    if (component_id >= plan.component_count ||
        plan.predicate_component.ptr == null ||
        delta_sets.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta payload plan");
    }

    usz entry_count = 0;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component >= 0 && (usz)pred_component == component_id) entry_count++;
    }
    if (entry_count > uint.max || component_id > uint.max) {
        return deduce_raise(interp, "deduce/parallel-component-delta-too-large",
            "deduce/parallel: component delta metadata exceeds payload format");
    }

    char* bytes = null;
    usz len = 0;
    usz capacity = 0;
    bool committed = false;
    defer if (!committed && bytes != null) mem::free(bytes);

    if (!deduce_component_delta_payload_append_u32(&bytes, &len, &capacity, DEDUCE_COMPONENT_DELTA_PAYLOAD_MAGIC) ||
        !deduce_component_delta_payload_append_u32(&bytes, &len, &capacity, DEDUCE_COMPONENT_DELTA_PAYLOAD_VERSION) ||
        !deduce_component_delta_payload_append_u32(&bytes, &len, &capacity, (uint)component_id) ||
        !deduce_component_delta_payload_append_u32(&bytes, &len, &capacity, (uint)entry_count)) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        if (pred_idx > uint.max) {
            return deduce_raise(interp, "deduce/parallel-component-delta-too-large",
                "deduce/parallel: predicate index exceeds payload format");
        }
        if (!deduce_component_delta_payload_append_u32(&bytes, &len, &capacity, (uint)pred_idx) ||
            !deduce_component_delta_payload_append_u64(&bytes, &len, &capacity, (ulong)plan.predicates[pred_idx])) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }

        Value* err = deduce_component_delta_payload_append_tuple_set(
            &bytes,
            &len,
            &capacity,
            &delta_sets[pred_idx].additions,
            interp
        );
        if (err != null) return err;
        err = deduce_component_delta_payload_append_tuple_set(
            &bytes,
            &len,
            &capacity,
            &delta_sets[pred_idx].removals,
            interp
        );
        if (err != null) return err;
    }

    *bytes_out = bytes;
    *len_out = len;
    committed = true;
    return null;
}

fn Value* deduce_component_delta_payload_read_tuple_set(
    char[] bytes,
    usz* pos_ref,
    DeduceDeltaTupleSet* set,
    Interp* interp
) {
    if (pos_ref == null || set == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta tuple decode state");
    }

    uint tuple_count = 0;
    if (!deduce_component_delta_payload_read_u32(bytes, pos_ref, &tuple_count)) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: malformed component delta payload");
    }

    for (uint i = 0; i < tuple_count; i++) {
        uint tuple_len = 0;
        if (!deduce_component_delta_payload_read_u32(bytes, pos_ref, &tuple_len)) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: malformed component delta payload");
        }
        if (*pos_ref > bytes.len || bytes.len - *pos_ref < tuple_len) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: malformed component delta payload");
        }
        char* tuple_bytes = tuple_len > 0 ? bytes.ptr + *pos_ref : null;
        if (!deduce_delta_set_append_encoded_tuple(set, tuple_bytes, tuple_len)) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        *pos_ref += tuple_len;
    }

    return null;
}

fn Value* deduce_component_delta_payload_deserialize(
    char[] bytes,
    DeduceSerializedComponentDeltaPayload* out,
    Interp* interp
) {
    if (out == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta decode target");
    }
    *out = {};

    usz pos = 0;
    uint magic = 0;
    uint version = 0;
    uint component_id = 0;
    uint entry_count = 0;
    if (!deduce_component_delta_payload_read_u32(bytes, &pos, &magic) ||
        !deduce_component_delta_payload_read_u32(bytes, &pos, &version) ||
        !deduce_component_delta_payload_read_u32(bytes, &pos, &component_id) ||
        !deduce_component_delta_payload_read_u32(bytes, &pos, &entry_count)) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: malformed component delta payload");
    }
    if (magic != DEDUCE_COMPONENT_DELTA_PAYLOAD_MAGIC || version != DEDUCE_COMPONENT_DELTA_PAYLOAD_VERSION) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: unsupported component delta payload version");
    }

    if (entry_count > 0) {
        out.entries = (DeduceSerializedComponentDeltaEntry*)mem::malloc(
            DeduceSerializedComponentDeltaEntry.sizeof * entry_count
        );
        if (out.entries == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < entry_count; i++) out.entries[i] = {};
    }
    out.component_id = component_id;
    out.entry_count = entry_count;

    bool committed = false;
    defer if (!committed) deduce_serialized_component_delta_payload_release(out);

    for (usz i = 0; i < entry_count; i++) {
        uint predicate_index = 0;
        ulong predicate = 0;
        if (!deduce_component_delta_payload_read_u32(bytes, &pos, &predicate_index) ||
            !deduce_component_delta_payload_read_u64(bytes, &pos, &predicate)) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: malformed component delta payload");
        }

        DeduceSerializedComponentDeltaEntry* entry = &out.entries[i];
        entry.predicate_index = predicate_index;
        entry.predicate = (SymbolId)predicate;

        Value* err = deduce_component_delta_payload_read_tuple_set(bytes, &pos, &entry.delta_set.additions, interp);
        if (err != null) return err;
        err = deduce_component_delta_payload_read_tuple_set(bytes, &pos, &entry.delta_set.removals, interp);
        if (err != null) return err;
    }

    if (pos != bytes.len) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: malformed component delta payload");
    }

    committed = true;
    return null;
}

fn Value* deduce_apply_serialized_component_delta_payload(
    DeduceDb* db,
    DeduceSccPlan* plan,
    MdbTxn* txn,
    DeduceSerializedComponentDeltaPayload* payload,
    long* applied_out,
    bool* changed_out,
    Interp* interp
) {
    if (applied_out != null) *applied_out = 0;
    if (changed_out != null) *changed_out = false;
    if (db == null || plan == null || txn == null || payload == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta apply state");
    }
    if (payload.component_id >= plan.component_count ||
        plan.predicate_component.ptr == null ||
        plan.predicates.ptr == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid component delta apply plan");
    }

    for (usz entry_idx = 0; entry_idx < payload.entry_count; entry_idx++) {
        DeduceSerializedComponentDeltaEntry* entry = &payload.entries[entry_idx];
        usz predicate_index = entry.predicate_index;
        if (predicate_index >= plan.predicates.len || predicate_index >= plan.predicate_component.len) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: invalid component delta predicate index");
        }
        int pred_component = plan.predicate_component[predicate_index];
        if (pred_component < 0 ||
            (usz)pred_component != payload.component_id ||
            plan.predicates[predicate_index] != entry.predicate) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: invalid component delta predicate routing");
        }

        MdbDbi head_dbi = 0;
        DeduceRelationSchema* head_schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, entry.predicate, &head_dbi, &head_schema, interp) ||
            head_schema == null) {
            return deduce_raise(interp, "deduce/relation-not-found",
                "deduce/parallel: component delta target relation is not registered");
        }

        for (usz tuple_idx = 0; tuple_idx < entry.delta_set.additions.count; tuple_idx++) {
            DeduceEncodedTuple* tuple = &entry.delta_set.additions.tuples[tuple_idx];
            Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
                db,
                head_schema,
                txn,
                head_dbi,
                tuple.key_bytes,
                tuple.key_len,
                interp
            );
            if (integrity_err != null) return integrity_err;

            char empty_key = 0;
            MdbVal key = {
                .mv_size = tuple.key_len,
                .mv_data = tuple.key_len > 0 ? tuple.key_bytes : &empty_key
            };
            MdbVal existing = {};
            if (mdb_get(txn, head_dbi, &key, &existing) == MDB_SUCCESS) continue;

            MdbVal data = {};
            char empty = 0;
            data.mv_size = 0;
            data.mv_data = &empty;
            int put_rc = mdb_put(txn, head_dbi, &key, &data, 0);
            if (put_rc != MDB_SUCCESS) {
                return deduce_raise(interp, "deduce/analyze-put-failed",
                    "deduce/parallel: failed to publish worker-computed derived tuple");
            }

            if (applied_out != null && *applied_out < long.max) *applied_out = *applied_out + 1;
            if (changed_out != null) *changed_out = true;
            if (head_schema.cardinality_estimate < ulong.max) {
                head_schema.cardinality_estimate++;
                if (head_schema.distinct_estimate < head_schema.cardinality_estimate) {
                    head_schema.distinct_estimate = head_schema.cardinality_estimate;
                }
            }
        }

        for (usz tuple_idx = 0; tuple_idx < entry.delta_set.removals.count; tuple_idx++) {
            DeduceEncodedTuple* tuple = &entry.delta_set.removals.tuples[tuple_idx];
            char empty_key = 0;
            MdbVal key = {
                .mv_size = tuple.key_len,
                .mv_data = tuple.key_len > 0 ? tuple.key_bytes : &empty_key
            };
            MdbVal existing = {};
            if (mdb_get(txn, head_dbi, &key, &existing) != MDB_SUCCESS) continue;

            int del_rc = mdb_del(txn, head_dbi, &key, null);
            if (del_rc != MDB_SUCCESS && del_rc != MDB_NOTFOUND) {
                return deduce_raise(interp, "deduce/analyze-put-failed",
                    "deduce/parallel: failed to retract worker-computed derived tuple");
            }
            if (del_rc == MDB_NOTFOUND) continue;

            if (applied_out != null && *applied_out < long.max) *applied_out = *applied_out + 1;
            if (changed_out != null) *changed_out = true;
            if (head_schema.cardinality_estimate > 0) {
                head_schema.cardinality_estimate--;
                head_schema.distinct_estimate = head_schema.cardinality_estimate;
            }
        }
    }

    return null;
}

fn void deduce_tuple_support_table_release(DeduceTupleSupportTable* table) {
    if (table == null) return;
    table.base_counts = null;
    table.recursive_counts = null;
}

fn void deduce_aggregate_groups_release(DeduceAggregateGroups* groups) {
    if (groups == null || groups.entries == null) return;
    for (usz i = 0; i < groups.count; i++) {
        DeduceAggregateGroup* group = &groups.entries[i];
        if (group.key_bytes != null) {
            mem::free(group.key_bytes);
            group.key_bytes = null;
        }
        if (group.head_values != null) {
            mem::free(group.head_values);
            group.head_values = null;
        }
        if (group.term_support_counts != null) {
            mem::free(group.term_support_counts);
            group.term_support_counts = null;
        }
        if (group.term_states != null) {
            mem::free(group.term_states);
            group.term_states = null;
        }
        if (group.previous_term_states != null) {
            mem::free(group.previous_term_states);
            group.previous_term_states = null;
        }
        group.key_len = 0;
    }
    mem::free(groups.entries);
    groups.entries = null;
    groups.count = 0;
    groups.capacity = 0;
}

fn Value* deduce_aggregate_groups_ensure_capacity(
    DeduceAggregateGroups* groups,
    usz needed,
    Interp* interp
) {
    if (groups == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate state");
    }
    if (needed <= groups.capacity) return null;

    usz new_cap = groups.capacity > 0 ? groups.capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceAggregateGroup* next =
        (DeduceAggregateGroup*)mem::realloc(groups.entries, DeduceAggregateGroup.sizeof * new_cap);
    if (next == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = groups.capacity; i < new_cap; i++) {
        next[i] = {};
    }
    groups.entries = next;
    groups.capacity = new_cap;
    return null;
}

fn usz deduce_rule_head_group_key_count(DeduceRuleSignature* sig) {
    if (sig == null || sig.head_term_kinds == null) return 0;
    usz count = 0;
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] == DEDUCE_RULE_TERM_VARIABLE) count++;
    }
    return count;
}

fn Value* deduce_aggregate_group_find_or_add(
    DeduceAggregateGroups* groups,
    DeduceRuleSignature* sig,
    bool[] var_bound,
    Value*[] var_values,
    DeduceAggregateGroup** group_out,
    Interp* interp
) {
    if (groups == null || sig == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate state");
    }
    if (group_out == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate group target");
    }
    *group_out = null;
    if (sig.head_term_kinds == null || sig.head_term_var_ids == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate head metadata");
    }

    usz group_key_count = deduce_rule_head_group_key_count(sig);
    Value*[] group_key_vals = {};
    if (group_key_count > 0) {
        group_key_vals = mem::new_array(Value*, group_key_count);
        if (group_key_vals.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (group_key_vals.ptr != null) mem::free(group_key_vals.ptr);

    usz key_idx = 0;
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] != DEDUCE_RULE_TERM_VARIABLE) continue;
        uint var_id = sig.head_term_var_ids[i];
        if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound group key at runtime");
        }
        if (key_idx < group_key_vals.len) {
            group_key_vals[key_idx] = var_values[var_id];
        }
        key_idx++;
    }

    char[4096] key_buf;
    usz? encoded_key_len = encode_tuple(group_key_vals, key_buf[..], key_buf.len);
    if (catch err = encoded_key_len) {
        return deduce_raise(interp, "deduce/analyze-tuple-too-large", "deduce/analyze: aggregate group key too large for buffer");
    }

    for (usz i = 0; i < groups.count; i++) {
        DeduceAggregateGroup* existing = &groups.entries[i];
        if (deduce_bytes_equal(existing.key_bytes, existing.key_len, &key_buf[0], encoded_key_len)) {
            *group_out = existing;
            return null;
        }
    }

    Value* cap_err = deduce_aggregate_groups_ensure_capacity(groups, groups.count + 1, interp);
    if (cap_err != null) return cap_err;

    DeduceAggregateGroup* group = &groups.entries[groups.count];
    *group = {};

    if (encoded_key_len > 0) {
        group.key_bytes = (char*)mem::malloc(encoded_key_len);
        if (group.key_bytes == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < encoded_key_len; i++) group.key_bytes[i] = key_buf[i];
    }
    group.key_len = encoded_key_len;

    if (sig.head_term_count > 0) {
        group.head_values = (Value**)mem::malloc(Value*.sizeof * sig.head_term_count);
        if (group.head_values == null) {
            if (group.key_bytes != null) mem::free(group.key_bytes);
            group.key_bytes = null;
            group.key_len = 0;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        group.term_states = (DeduceAggregateTermState*)mem::malloc(
            DeduceAggregateTermState.sizeof * sig.head_term_count
        );
        if (group.term_states == null) {
            mem::free(group.head_values);
            group.head_values = null;
            if (group.key_bytes != null) mem::free(group.key_bytes);
            group.key_bytes = null;
            group.key_len = 0;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        group.previous_term_states = (DeduceAggregateTermState*)mem::malloc(
            DeduceAggregateTermState.sizeof * sig.head_term_count
        );
        if (group.previous_term_states == null) {
            mem::free(group.term_states);
            group.term_states = null;
            mem::free(group.head_values);
            group.head_values = null;
            if (group.key_bytes != null) mem::free(group.key_bytes);
            group.key_bytes = null;
            group.key_len = 0;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        group.term_support_counts = (Value**)mem::malloc(Value*.sizeof * sig.head_term_count);
        if (group.term_support_counts == null) {
            mem::free(group.previous_term_states);
            group.previous_term_states = null;
            mem::free(group.term_states);
            group.term_states = null;
            mem::free(group.head_values);
            group.head_values = null;
            if (group.key_bytes != null) mem::free(group.key_bytes);
            group.key_bytes = null;
            group.key_len = 0;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < sig.head_term_count; i++) {
            group.head_values[i] = null;
            group.term_support_counts[i] = null;
            group.term_states[i] = {};
            group.previous_term_states[i] = {};
            if (sig.head_term_kinds[i] == DEDUCE_RULE_TERM_VARIABLE) {
                uint var_id = sig.head_term_var_ids[i];
                if (var_id >= var_values.len || var_id >= var_bound.len || !var_bound[var_id]) {
                    return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound group key at runtime");
                }
                group.head_values[i] = var_values[var_id];
            }
        }
    }

    groups.count++;
    *group_out = group;
    return null;
}

fn void deduce_aggregate_group_snapshot_previous(DeduceAggregateGroup* group, DeduceRuleSignature* sig) {
    if (group == null || sig == null || group.previous_term_states == null || group.term_states == null || group.dirty) {
        return;
    }
    for (usz i = 0; i < sig.head_term_count; i++) {
        group.previous_term_states[i] = group.term_states[i];
    }
    group.previous_support_count = group.support_count;
    group.dirty = true;
}

fn Value* deduce_aggregate_group_ensure_term_support_map(
    DeduceAggregateGroup* group,
    usz term_idx,
    Interp* interp
) {
    if (group == null || interp == null || group.term_support_counts == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate support state");
    }
    if (group.term_support_counts[term_idx] == null) {
        group.term_support_counts[term_idx] = make_hashmap(interp, 16);
        if (group.term_support_counts[term_idx] == null ||
            group.term_support_counts[term_idx].tag != HASHMAP ||
            group.term_support_counts[term_idx].hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    return null;
}

fn Value* deduce_aggregate_group_adjust_term_support_count(
    DeduceAggregateGroup* group,
    usz term_idx,
    Value* input,
    long delta,
    long* next_count_out,
    Interp* interp
) {
    if (group == null || interp == null || group.term_support_counts == null || input == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate support update");
    }
    if (next_count_out != null) *next_count_out = 0;
    if (delta == 0) return null;

    if (delta > 0) {
        Value* err = deduce_aggregate_group_ensure_term_support_map(group, term_idx, interp);
        if (err != null) return err;
    }

    Value* map_val = group.term_support_counts[term_idx];
    long current = 0;
    if (map_val != null && map_val.tag == HASHMAP && map_val.hashmap_val != null) {
        Value* current_val = hashmap_get(map_val.hashmap_val, input);
        if (current_val != null && current_val.tag == INT) current = current_val.int_val;
    }

    long next = current;
    if (delta > 0) {
        if (current > long.max - delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support overflow");
        }
        next = current + delta;
    } else {
        if (current < -delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support underflow");
        }
        next = current + delta;
    }

    if (next <= 0) {
        if (map_val != null && map_val.tag == HASHMAP && map_val.hashmap_val != null) {
            hashmap_remove(map_val.hashmap_val, input);
        }
        next = 0;
    } else {
        hashmap_set(group.term_support_counts[term_idx].hashmap_val, input, make_int(interp, next), interp);
    }
    if (next_count_out != null) *next_count_out = next;
    return null;
}

fn void deduce_aggregate_group_recompute_extremum(
    DeduceAggregateGroup* group,
    usz term_idx,
    bool want_max,
    DeduceAggregateTermState* state
) {
    if (group == null || state == null || group.term_support_counts == null) return;
    Value* map_val = group.term_support_counts[term_idx];
    if (map_val == null || map_val.tag != HASHMAP || map_val.hashmap_val == null || map_val.hashmap_val.entries == null) {
        state.initialized = false;
        state.extremum_value = null;
        return;
    }

    Value* extremum = null;
    for (uint i = 0; i < map_val.hashmap_val.capacity; i++) {
        HashEntry* entry = &map_val.hashmap_val.entries[i];
        if (entry.key == null || entry.value == null || entry.value.tag != INT || entry.value.int_val <= 0) continue;
        if (extremum == null) {
            extremum = entry.key;
            continue;
        }
        int cmp = deduce_compare_value(entry.key, extremum);
        if ((want_max && cmp > 0) || (!want_max && cmp < 0)) {
            extremum = entry.key;
        }
    }
    state.initialized = extremum != null;
    state.extremum_value = extremum;
}

fn Value* deduce_aggregate_group_note_binding_with_polarity(
    DeduceAggregateGroup* group,
    DeduceRuleSignature* sig,
    bool[] var_bound,
    Value*[] var_values,
    bool removal,
    Interp* interp
) {
    if (group == null || sig == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate state");
    }
    if (sig.head_term_kinds == null || sig.head_term_aggregate_ops == null ||
        sig.head_term_var_ids == null || group.term_states == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate metadata");
    }

    deduce_aggregate_group_snapshot_previous(group, sig);

    long group_delta = removal ? -1 : 1;
    if (group_delta > 0) {
        if (group.support_count > long.max - group_delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support overflow");
        }
        group.support_count += group_delta;
    } else {
        if (group.support_count < -group_delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate support underflow");
        }
        group.support_count += group_delta;
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_kinds[i] != DEDUCE_RULE_TERM_AGGREGATE) continue;

        uint var_id = sig.head_term_var_ids[i];
        if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound aggregate input at runtime");
        }

        Value* input = var_values[var_id];
        DeduceAggregateTermState* state = &group.term_states[i];
        long delta = removal ? -1 : 1;
        switch (sig.head_term_aggregate_ops[i]) {
            case DEDUCE_AGGREGATE_COUNT:
                if (delta > 0) {
                    state.initialized = true;
                    if (state.count_value > long.max - delta) {
                        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate count overflow");
                    }
                    state.count_value += delta;
                } else {
                    if (state.count_value < -delta) {
                        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate count underflow");
                    }
                    state.count_value += delta;
                    if (state.count_value == 0) state.initialized = false;
                }
                break;
            case DEDUCE_AGGREGATE_SUM:
                if (!is_number(input)) {
                    return deduce_raise(interp, "deduce/aggregate-type-mismatch",
                        "deduce/analyze: sum aggregate expects numeric input");
                }
                if (delta > 0 && !state.initialized) {
                    state.initialized = true;
                    state.sum_is_double = is_double(input);
                    state.sum_int_value = is_int(input) ? input.int_val : 0;
                    state.sum_double_value = to_double(input);
                    break;
                }
                if (!state.initialized) {
                    return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: aggregate sum underflow");
                }
                if (!state.sum_is_double && is_int(input)) {
                    state.sum_int_value += delta * input.int_val;
                    state.sum_double_value = (double)state.sum_int_value;
                } else {
                    if (!state.sum_is_double) {
                        state.sum_double_value = (double)state.sum_int_value;
                        state.sum_is_double = true;
                    }
                    state.sum_double_value += (double)delta * to_double(input);
                }
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.sum_is_double = false;
                    state.sum_int_value = 0;
                    state.sum_double_value = 0.0;
                }
                break;
            case DEDUCE_AGGREGATE_MIN: {
                long next_support = 0;
                Value* err = deduce_aggregate_group_adjust_term_support_count(
                    group,
                    i,
                    input,
                    delta,
                    &next_support,
                    interp
                );
                if (err != null) return err;
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.extremum_value = null;
                    break;
                }
                if (delta > 0) {
                    if (!state.initialized || deduce_compare_value(input, state.extremum_value) < 0) {
                        state.initialized = true;
                        state.extremum_value = input;
                    }
                } else if (!state.initialized ||
                           (state.extremum_value != null && values_equal(input, state.extremum_value) && next_support == 0)) {
                    deduce_aggregate_group_recompute_extremum(group, i, false, state);
                }
                break;
            }
            case DEDUCE_AGGREGATE_MAX: {
                long next_support = 0;
                Value* err = deduce_aggregate_group_adjust_term_support_count(
                    group,
                    i,
                    input,
                    delta,
                    &next_support,
                    interp
                );
                if (err != null) return err;
                if (group.support_count == 0) {
                    state.initialized = false;
                    state.extremum_value = null;
                    break;
                }
                if (delta > 0) {
                    if (!state.initialized || deduce_compare_value(input, state.extremum_value) > 0) {
                        state.initialized = true;
                        state.extremum_value = input;
                    }
                } else if (!state.initialized ||
                           (state.extremum_value != null && values_equal(input, state.extremum_value) && next_support == 0)) {
                    deduce_aggregate_group_recompute_extremum(group, i, true, state);
                }
                break;
            }
            case DEDUCE_AGGREGATE_NONE:
                return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate operator");
        }
    }
    return null;
}

fn Value* deduce_aggregate_group_note_binding(
    DeduceAggregateGroup* group,
    DeduceRuleSignature* sig,
    bool[] var_bound,
    Value*[] var_values,
    Interp* interp
) {
    return deduce_aggregate_group_note_binding_with_polarity(
        group,
        sig,
        var_bound,
        var_values,
        false,
        interp
    );
}

fn Value* deduce_build_aggregate_group_head_tuple(
    DeduceRuleSignature* sig,
    DeduceAggregateGroup* group,
    DeduceAggregateTermState* term_states,
    Value*[] head_tuple,
    Interp* interp
) {
    if (sig == null || group == null || term_states == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate tuple state");
    }
    if (sig.head_term_kinds == null || sig.head_term_aggregate_ops == null ||
        sig.head_term_literals == null || group.head_values == null || head_tuple.len != sig.head_term_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate tuple metadata");
    }

    for (usz i = 0; i < sig.head_term_count; i++) {
        switch (sig.head_term_kinds[i]) {
            case DEDUCE_RULE_TERM_VARIABLE:
                head_tuple[i] = group.head_values[i];
                break;
            case DEDUCE_RULE_TERM_LITERAL:
                head_tuple[i] = sig.head_term_literals[i];
                break;
            case DEDUCE_RULE_TERM_AGGREGATE: {
                DeduceAggregateTermState* state = &term_states[i];
                if (!state.initialized) {
                    return deduce_raise(interp, "deduce/analyze-invalid-state",
                        "deduce/analyze: aggregate finalize state is incomplete");
                }

                switch (sig.head_term_aggregate_ops[i]) {
                    case DEDUCE_AGGREGATE_COUNT:
                        head_tuple[i] = make_int(interp, state.count_value);
                        break;
                    case DEDUCE_AGGREGATE_SUM:
                        head_tuple[i] = state.sum_is_double
                            ? make_double(interp, state.sum_double_value)
                            : make_int(interp, state.sum_int_value);
                        break;
                    case DEDUCE_AGGREGATE_MIN:
                    case DEDUCE_AGGREGATE_MAX:
                        head_tuple[i] = state.extremum_value;
                        break;
                    case DEDUCE_AGGREGATE_NONE:
                        return deduce_raise(interp, "deduce/analyze-invalid-state",
                            "deduce/analyze: invalid aggregate operator");
                }
                break;
            }
        }
    }
    return null;
}

fn Value* deduce_encode_aggregate_group_tuple(
    DeduceRuleSignature* sig,
    DeduceAggregateGroup* group,
    DeduceAggregateTermState* term_states,
    Value*[] head_tuple,
    DeduceEncodedTuple* tuple_out,
    Interp* interp
) {
    if (tuple_out == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate tuple target");
    }
    *tuple_out = {};

    Value* err = deduce_build_aggregate_group_head_tuple(sig, group, term_states, head_tuple, interp);
    if (err != null) return err;

    char[4096] key_buf;
    usz? key_len = encode_tuple(head_tuple, key_buf[..], key_buf.len);
    if (catch err2 = key_len) {
        return deduce_raise(interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }

    if (key_len > 0) {
        tuple_out.key_bytes = (char*)mem::malloc(key_len);
        if (tuple_out.key_bytes == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < key_len; i++) tuple_out.key_bytes[i] = key_buf[i];
    }
    tuple_out.key_len = key_len;
    return null;
}

fn void deduce_aggregate_group_commit_materialized_state(DeduceAggregateGroup* group, DeduceRuleSignature* sig) {
    if (group == null || sig == null || group.term_states == null || group.previous_term_states == null) return;
    for (usz i = 0; i < sig.head_term_count; i++) {
        group.previous_term_states[i] = group.term_states[i];
    }
    group.previous_support_count = group.support_count;
    group.materialized = group.support_count > 0;
    group.dirty = false;
}

fn Value* deduce_collect_rule_observed_counters(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    DeduceObservedRuleCounters* counters,
    Interp* interp
) {
    if (db == null || sig == null || counters == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid observed-counter state");
    }
    deduce_observed_rule_counters_reset(counters);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    return deduce_naive_evaluate_rule(db, sig, txn, null, null, null, counters, true, interp);
}

fn Value* deduce_delta_set_insert(DeduceDeltaTupleSet* set, void* key_data, usz key_len, Interp* interp) {
    if (set == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set state");
    }
    if (set.seen_map == null) {
        set.seen_map = make_hashmap(interp, 64);
        if (set.seen_map == null || set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }

    if (set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set payload");
    }

    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    if (hashmap_get(set.seen_map.hashmap_val, key_val) != null) return null;
    hashmap_set(set.seen_map.hashmap_val, key_val, make_symbol(interp, interp.sym_true), interp);

    if (set.count >= set.capacity) {
        usz new_cap = set.capacity > 0 ? set.capacity * 2 : 4;
        if (new_cap < set.count + 1) new_cap = set.count + 1;
        DeduceEncodedTuple* next = (DeduceEncodedTuple*)mem::realloc(
            set.tuples,
            DeduceEncodedTuple.sizeof * new_cap
        );
        if (next == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = set.capacity; i < new_cap; i++) next[i] = {};
        set.tuples = next;
        set.capacity = new_cap;
    }

    DeduceEncodedTuple* tuple = &set.tuples[set.count];
    *tuple = {};
    if (key_len > 0) {
        tuple.key_bytes = (char*)mem::malloc(key_len);
        if (tuple.key_bytes == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        for (usz i = 0; i < key_len; i++) tuple.key_bytes[i] = ((char*)key_data)[i];
    }
    tuple.key_len = key_len;
    set.count++;
    return null;
}

fn Value* deduce_tuple_support_table_ensure_map(Value** map_out, Interp* interp) {
    if (map_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table state");
    }
    if (*map_out == null) {
        *map_out = make_hashmap(interp, 64);
        if (*map_out == null || (*map_out).tag != HASHMAP || (*map_out).hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    if ((*map_out).tag != HASHMAP || (*map_out).hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table payload");
    }
    return null;
}

fn long deduce_tuple_support_table_lookup_count(
    Value* map,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (interp == null || (key_data == null && key_len > 0)) return 0;
    if (map == null || map.tag != HASHMAP || map.hashmap_val == null) return 0;
    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    Value* count_val = hashmap_get(map.hashmap_val, key_val);
    if (count_val == null || count_val.tag != INT || count_val.int_val <= 0) return 0;
    return count_val.int_val;
}

fn Value* deduce_tuple_support_table_set_count(
    Value** map_out,
    void* key_data,
    usz key_len,
    long count,
    Interp* interp
) {
    if (map_out == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table update");
    }
    if (count < 0) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter underflow");
    }

    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    if (count == 0) {
        if (*map_out != null && (*map_out).tag == HASHMAP && (*map_out).hashmap_val != null) {
            hashmap_remove((*map_out).hashmap_val, key_val);
        }
        return null;
    }

    Value* err = deduce_tuple_support_table_ensure_map(map_out, interp);
    if (err != null) return err;
    hashmap_set((*map_out).hashmap_val, key_val, make_int(interp, count), interp);
    return null;
}

fn long deduce_tuple_support_table_total_count(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (table == null || interp == null) return 0;
    long base_count = deduce_tuple_support_table_lookup_count(table.base_counts, key_data, key_len, interp);
    long recursive_count = deduce_tuple_support_table_lookup_count(table.recursive_counts, key_data, key_len, interp);
    if (base_count > long.max - recursive_count) return long.max;
    return base_count + recursive_count;
}

fn Value* deduce_tuple_support_table_seed_base_tuple(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    long* total_count_out,
    Interp* interp
) {
    if (table == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed");
    }
    long base_count = deduce_tuple_support_table_lookup_count(table.base_counts, key_data, key_len, interp);
    if (base_count >= long.max) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter overflow");
    }
    Value* err = deduce_tuple_support_table_set_count(&table.base_counts, key_data, key_len, base_count + 1, interp);
    if (err != null) return err;
    if (total_count_out != null) {
        *total_count_out = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    }
    return null;
}

fn Value* deduce_tuple_support_table_adjust_recursive_count(
    DeduceTupleSupportTable* table,
    void* key_data,
    usz key_len,
    long delta,
    long* total_count_out,
    bool* became_present_out,
    bool* became_absent_out,
    Interp* interp
) {
    if (table == null || interp == null || (key_data == null && key_len > 0)) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table delta");
    }
    if (became_present_out != null) *became_present_out = false;
    if (became_absent_out != null) *became_absent_out = false;

    long old_total = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    long recursive_count = deduce_tuple_support_table_lookup_count(table.recursive_counts, key_data, key_len, interp);
    long next_recursive = recursive_count;
    if (delta > 0) {
        if (recursive_count > long.max - delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter overflow");
        }
        next_recursive = recursive_count + delta;
    } else if (delta < 0) {
        if (recursive_count < -delta) {
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: support counter underflow");
        }
        next_recursive = recursive_count + delta;
    }

    Value* err = deduce_tuple_support_table_set_count(&table.recursive_counts, key_data, key_len, next_recursive, interp);
    if (err != null) return err;

    long next_total = deduce_tuple_support_table_total_count(table, key_data, key_len, interp);
    if (became_present_out != null) *became_present_out = old_total == 0 && next_total > 0;
    if (became_absent_out != null) *became_absent_out = old_total > 0 && next_total == 0;
    if (total_count_out != null) *total_count_out = next_total;
    return null;
}

fn Value* deduce_encode_value_vector_to_owned_string(
    Value*[] values,
    Value** key_out,
    Interp* interp
) {
    if (key_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid encoded tuple target");
    }
    *key_out = null;

    usz buf_size = values.len > 0 ? values.len * 16 : 16;
    if (buf_size < 64) buf_size = 64;

    while (true) {
        if (buf_size == 0 || buf_size > 1_048_576) {
            return deduce_raise(interp, "deduce/analyze-tuple-too-large",
                "deduce/analyze: encoded tuple key exceeds supported size");
        }

        char* owned = (char*)mem::malloc(buf_size + 1);
        if (owned == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }

        usz? encoded_len = encode_tuple(values, owned, buf_size);
        if (catch err = encoded_len) {
            mem::free(owned);
            if (buf_size > usz.max / 2) {
                return deduce_raise(interp, "deduce/analyze-tuple-too-large",
                    "deduce/analyze: encoded tuple key exceeds supported size");
            }
            buf_size *= 2;
            continue;
        }

        owned[encoded_len] = 0;
        *key_out = make_string_owned(interp, owned, encoded_len);
        return null;
    }
}

fn bool deduce_delta_set_contains(DeduceDeltaTupleSet* set, void* key_data, usz key_len, Interp* interp) {
    if (set == null || interp == null || (key_data == null && key_len > 0)) return false;
    if (set.seen_map == null || set.seen_map.tag != HASHMAP || set.seen_map.hashmap_val == null) return false;
    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    return hashmap_get(set.seen_map.hashmap_val, key_val) != null;
}

fn int deduce_scc_predicate_index(DeduceSccPlan* plan, SymbolId predicate) {
    if (plan == null || plan.predicates.len == 0) return -1;
    return deduce_scc_find_predicate_index(plan.predicates, predicate);
}

fn bool deduce_scc_predicate_in_component(DeduceSccPlan* plan, SymbolId predicate, usz component_id) {
    if (plan == null || component_id >= plan.component_count) return false;
    int pred_idx = deduce_scc_predicate_index(plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= plan.predicate_component.len) return false;
    return (usz)plan.predicate_component[(usz)pred_idx] == component_id;
}

fn Value* deduce_seminaive_record_new_delta_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.next_delta_sets.len) return null;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return null;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return null;
    return deduce_delta_set_insert(&exec.next_delta_sets[(usz)pred_idx].additions, key_data, key_len, exec.interp);
}

fn bool deduce_seminaive_scratch_visible_contains(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return false;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.scratch_visible_sets.len) return false;
    return deduce_delta_set_contains(&exec.scratch_visible_sets[(usz)pred_idx], key_data, key_len, exec.interp);
}

fn Value* deduce_seminaive_record_scratch_visible_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.scratch_visible_sets.len) return null;
    return deduce_delta_set_insert(&exec.scratch_visible_sets[(usz)pred_idx], key_data, key_len, exec.interp);
}

fn Value* deduce_seminaive_accumulate_component_visible_additions(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] source,
    DeduceDeltaTupleSet[] target,
    Interp* interp
) {
    if (plan == null || interp == null) {
        return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
            "deduce/parallel: invalid worker-visible accumulation state");
    }
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= source.len || pred_idx >= target.len) continue;
        int pred_component = plan.predicate_component[pred_idx];
        if (pred_component < 0 || (usz)pred_component != component_id) continue;
        for (usz tuple_idx = 0; tuple_idx < source[pred_idx].additions.count; tuple_idx++) {
            DeduceEncodedTuple tuple = source[pred_idx].additions.tuples[tuple_idx];
            Value* err = deduce_delta_set_insert(&target[pred_idx], tuple.key_bytes, tuple.key_len, interp);
            if (err != null) return err;
        }
    }
    return null;
}

fn Value* deduce_seminaive_record_removed_delta_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.next_delta_sets.len) return null;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return null;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return null;
    return deduce_delta_set_insert(&exec.next_delta_sets[(usz)pred_idx].removals, key_data, key_len, exec.interp);
}

fn bool deduce_seminaive_component_has_delta(
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] delta_sets
) {
    if (plan == null || component_id >= plan.component_count) return false;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= delta_sets.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;
        if (delta_sets[pred_idx].additions.count > 0 || delta_sets[pred_idx].removals.count > 0) {
            return true;
        }
    }
    return false;
}

fn Value* deduce_seminaive_seed_component_delta_sets(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceSignedDeltaSet[] delta_sets,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed state");
    }
    if (delta_sets.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed table");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbCursor* cursor = null;
        rc = mdb_cursor_open(txn, dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
        }

        MdbVal key = {};
        MdbVal data = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            Value* err = deduce_delta_set_insert(&delta_sets[pred_idx].additions, key.mv_data, key.mv_size, interp);
            if (err != null) {
                mdb_cursor_close(cursor);
                return err;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        mdb_cursor_close(cursor);
    }
    return null;
}

fn Value* deduce_seminaive_seed_component_support_tables_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceTupleSupportTable[] support_tables,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed state");
    }
    if (support_tables.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed table");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbCursor* cursor = null;
        int rc = mdb_cursor_open(txn, dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
        }

        MdbVal key = {};
        MdbVal data = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            Value* err =
                deduce_tuple_support_table_seed_base_tuple(&support_tables[pred_idx], key.mv_data, key.mv_size, null, interp);
            if (err != null) {
                mdb_cursor_close(cursor);
                return err;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        mdb_cursor_close(cursor);
    }
    return null;
}

fn Value* deduce_seminaive_seed_component_extensional_state_in_txn(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    DeduceSignedDeltaSet[] delta_sets,
    DeduceTupleSupportTable[] support_tables,
    bool restore_main_relations,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid extensional seed state");
    }

    bool seed_deltas = delta_sets.len >= plan.predicates.len;
    bool seed_support = support_tables.len >= plan.predicates.len;
    char empty = 0;
    MdbVal data = { .mv_size = 0, .mv_data = &empty };

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi main_dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &main_dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbDbi extensional_dbi = 0;
        if (!deduce_open_extensional_dbi_for_predicate(db, predicate, txn, false, &extensional_dbi, interp)) {
            if (schema != null && restore_main_relations) {
                schema.cardinality_estimate = 0;
                schema.distinct_estimate = 0;
            }
            continue;
        }

        MdbCursor* cursor = null;
        int rc = mdb_cursor_open(txn, extensional_dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open extensional scan cursor");
        }
        defer mdb_cursor_close(cursor);

        long restored_count = 0;
        MdbVal key = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            if (seed_support) {
                Value* err =
                    deduce_tuple_support_table_seed_base_tuple(&support_tables[pred_idx], key.mv_data, key.mv_size, null, interp);
                if (err != null) return err;
            }
            if (seed_deltas) {
                Value* err = deduce_delta_set_insert(&delta_sets[pred_idx].additions, key.mv_data, key.mv_size, interp);
                if (err != null) return err;
            }
            if (restore_main_relations) {
                int put_rc = mdb_put(txn, main_dbi, &key, &data, 0);
                if (put_rc != MDB_SUCCESS) {
                    return deduce_raise(interp, "deduce/analyze-put-failed",
                        "deduce/analyze: failed to restore extensional tuple into recursive relation");
                }
            }
            if (restored_count < long.max) restored_count++;
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }

        if (schema != null && restore_main_relations) {
            ulong count = restored_count > 0 ? (ulong)restored_count : 0;
            schema.cardinality_estimate = count;
            schema.distinct_estimate = count;
        }
        mdb_cursor_close(cursor);
        cursor = null;
    }
    return null;
}

fn usz deduce_seminaive_collect_anchor_steps(
    DeduceRuleSignature* sig,
    DeduceSccPlan* plan,
    usz component_id,
    usz[] anchor_steps
) {
    if (sig == null || plan == null) return 0;
    usz count = 0;
    for (usz i = 0; i < sig.body_count; i++) {
        if (sig.body_negated != null && sig.body_negated[i]) continue;
        if (sig.body_predicates == null) continue;
        SymbolId predicate = sig.body_predicates[i];
        if (!deduce_scc_predicate_in_component(plan, predicate, component_id)) continue;
        if (count < anchor_steps.len) anchor_steps[count] = i;
        count++;
    }
    return count;
}

fn Value* deduce_analyze_iteration_limit_payload(
    Interp* interp,
    usz component_id,
    usz iteration_limit
) {
    Value* payload = make_hashmap(interp, 8);
    if (payload == null || payload.tag != HASHMAP || payload.hashmap_val == null) return null;
    hashmap_set(payload.hashmap_val, make_symbol(interp, interp.symbols.intern("component-id")), make_int(interp, (long)component_id), interp);
    hashmap_set(payload.hashmap_val, make_symbol(interp, interp.symbols.intern("iteration-limit")), make_int(interp, (long)iteration_limit), interp);
    return payload;
}

fn bool deduce_rule_open_existing_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    MdbDbi* dbi_out,
    DeduceRelationSchema** schema_out,
    Interp* interp
) {
    if (db == null || txn == null || dbi_out == null || schema_out == null || interp == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
    if (schema == null) return false;

    char[] name = interp.symbols.get_name(predicate);
    char* name_c = deduce_copy_c_string(name);
    if (name_c == null) return false;
    defer mem::free(name_c);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, name_c, 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    *schema_out = schema;
    return true;
}

fn bool deduce_rule_match_positive_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (var_bound[var_id]) {
                if (!values_equal(var_values[var_id], tuple_v)) return false;
            } else {
                var_bound[var_id] = true;
                var_values[var_id] = tuple_v;
            }
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}

fn bool deduce_rule_match_negated_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (!var_bound[var_id]) return false;
            if (!values_equal(var_values[var_id], tuple_v)) return false;
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}

fn Value* deduce_naive_insert_head_tuple(DeduceNaiveRuleExec* exec) {
    if (exec == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }
    if (exec.dry_run) {
        return null;
    }

    MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    Value* integrity_err = deduce_assert_schema_derived_write_integrity(
        exec.db,
        exec.head_schema,
        exec.txn,
        exec.head_dbi,
        exec.head_tuple,
        &key,
        exec.interp
    );
    if (integrity_err != null) return integrity_err;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }
    return null;
}

fn Value* deduce_validate_component_ref_head_snapshots(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    MdbTxn* txn,
    Interp* interp
) {
    if (db == null || plan == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid constrained head validation state");
    }

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
        if (schema == null || schema.reference_constraint_count == 0) continue;

        MdbDbi dbi = 0;
        if (!deduce_open_relation_dbi_by_symbol(db, txn, predicate, &dbi, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found",
                "deduce/analyze: head predicate relation is not registered");
        }

        Value* err = deduce_validate_schema_fact_integrity_snapshot(db, schema, txn, dbi, interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_seminaive_seed_component_support_tables(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceTupleSupportTable[] support_tables,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid support-table seed state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    return deduce_seminaive_seed_component_support_tables_in_txn(
        db,
        plan,
        component_id,
        txn,
        support_tables,
        interp
    );
}

fn Value* deduce_assert_encoded_head_tuple_integrity(
    DeduceDb* db,
    DeduceRelationSchema* head_schema,
    MdbTxn* txn,
    MdbDbi head_dbi,
    void* key_data,
    usz key_len,
    Interp* interp
) {
    if (db == null || head_schema == null || txn == null || interp == null) return null;
    if (deduce_relation_schema_constraint_count(head_schema) == 0) return null;

    Value*[] decoded = {};
    if (head_schema.arity > 0) {
        decoded = mem::new_array(Value*, head_schema.arity);
        if (decoded.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (decoded.ptr != null) mem::free(decoded.ptr);

    char empty_key = 0;
    char* key_bytes = key_len > 0 ? (char*)key_data : &empty_key;
    usz? decoded_count = decode_tuple(key_bytes, key_len, decoded, interp);
    if (catch err = decoded_count) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: failed to decode derived head tuple for integrity validation");
    }
    if (decoded_count != head_schema.arity) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: decoded derived head tuple arity mismatch");
    }

    MdbVal key = { .mv_size = key_len, .mv_data = key_bytes };
    return deduce_assert_schema_derived_write_integrity(db, head_schema, txn, head_dbi, decoded, &key, interp);
}

fn Value* deduce_naive_insert_encoded_head_tuple(
    DeduceNaiveRuleExec* exec,
    char* key_bytes,
    usz key_len
) {
    if (exec == null || exec.txn == null || exec.interp == null || (key_bytes == null && key_len > 0)) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }

    char empty_key = 0;
    MdbVal key = { .mv_size = key_len, .mv_data = key_len > 0 ? key_bytes : &empty_key };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    Value* integrity_err = deduce_assert_encoded_head_tuple_integrity(
        exec.db,
        exec.head_schema,
        exec.txn,
        exec.head_dbi,
        key.mv_data,
        key_len,
        exec.interp
    );
    if (integrity_err != null) return integrity_err;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }
    return null;
}

fn Value* deduce_naive_accumulate_aggregate_binding(
    DeduceNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.aggregate_groups == null || exec.sig == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }

    DeduceAggregateGroup* group = null;
    Value* err = deduce_aggregate_group_find_or_add(
        exec.aggregate_groups,
        exec.sig,
        var_bound,
        var_values,
        &group,
        exec.interp
    );
    if (err != null) return err;
    return deduce_aggregate_group_note_binding(group, exec.sig, var_bound, var_values, exec.interp);
}

fn Value* deduce_naive_materialize_aggregate_groups(
    DeduceNaiveRuleExec* exec,
    DeduceEncodedTuple** tuples_out,
    usz* tuple_count_out
) {
    if (exec == null || exec.sig == null || exec.interp == null || exec.aggregate_groups == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }
    if (tuples_out == null || tuple_count_out == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate materialization target");
    }
    *tuples_out = null;
    *tuple_count_out = 0;

    usz tuple_count = exec.aggregate_groups.count;
    if (tuple_count == 0) return null;

    DeduceEncodedTuple* tuples = (DeduceEncodedTuple*)mem::malloc(DeduceEncodedTuple.sizeof * tuple_count);
    if (tuples == null) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < tuple_count; i++) tuples[i] = {};

    for (usz group_idx = 0; group_idx < tuple_count; group_idx++) {
        DeduceAggregateGroup* group = &exec.aggregate_groups.entries[group_idx];
        for (usz i = 0; i < exec.sig.head_term_count; i++) {
            switch (exec.sig.head_term_kinds[i]) {
                case DEDUCE_RULE_TERM_VARIABLE:
                    exec.head_tuple[i] = group.head_values != null ? group.head_values[i] : null;
                    break;
                case DEDUCE_RULE_TERM_LITERAL:
                    exec.head_tuple[i] = exec.sig.head_term_literals[i];
                    break;
                case DEDUCE_RULE_TERM_AGGREGATE: {
                    DeduceAggregateTermState* state =
                        group.term_states != null ? &group.term_states[i] : null;
                    if (state == null || !state.initialized) {
                        deduce_encoded_tuples_release(tuples, tuple_count);
                        return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
                            "deduce/analyze: aggregate finalize state is incomplete");
                    }

                    switch (exec.sig.head_term_aggregate_ops[i]) {
                        case DEDUCE_AGGREGATE_COUNT:
                            exec.head_tuple[i] = make_int(exec.interp, state.count_value);
                            break;
                        case DEDUCE_AGGREGATE_SUM:
                            exec.head_tuple[i] = state.sum_is_double
                                ? make_double(exec.interp, state.sum_double_value)
                                : make_int(exec.interp, state.sum_int_value);
                            break;
                        case DEDUCE_AGGREGATE_MIN:
                        case DEDUCE_AGGREGATE_MAX:
                            exec.head_tuple[i] = state.extremum_value;
                            break;
                        case DEDUCE_AGGREGATE_NONE:
                            deduce_encoded_tuples_release(tuples, tuple_count);
                            return deduce_raise(exec.interp, "deduce/analyze-invalid-state",
                                "deduce/analyze: invalid aggregate operator");
                    }
                    break;
                }
            }
        }

        char[4096] key_buf;
        usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
        if (catch err = key_len) {
            deduce_encoded_tuples_release(tuples, tuple_count);
            return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
        }

        if (key_len > 0) {
            tuples[group_idx].key_bytes = (char*)mem::malloc(key_len);
            if (tuples[group_idx].key_bytes == null) {
                deduce_encoded_tuples_release(tuples, tuple_count);
                return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
            }
            for (usz i = 0; i < key_len; i++) tuples[group_idx].key_bytes[i] = key_buf[i];
        }
        tuples[group_idx].key_len = key_len;
    }

    *tuples_out = tuples;
    *tuple_count_out = tuple_count;
    return null;
}

fn Value* deduce_relation_count_rows(
    MdbTxn* txn,
    MdbDbi dbi,
    long* count_out,
    Interp* interp
) {
    if (txn == null || count_out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid relation count state");
    }

    *count_out = 0;
    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(txn, dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
    while (rc == MDB_SUCCESS) {
        if (*count_out < long.max) *count_out = *count_out + 1;
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}

fn Value* deduce_naive_finalize_aggregate_groups(DeduceNaiveRuleExec* exec) {
    if (exec == null || exec.sig == null || exec.interp == null || exec.aggregate_groups == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid aggregate evaluator state");
        }
        return null;
    }
    if (exec.dry_run) return null;
    if (exec.sig.head_term_kinds == null || exec.sig.head_term_aggregate_ops == null ||
        exec.sig.head_term_literals == null || exec.aggregate_groups.entries == null && exec.aggregate_groups.count > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing aggregate finalize metadata");
    }

    DeduceEncodedTuple* tuples = null;
    usz tuple_count = 0;
    Value* err = deduce_naive_materialize_aggregate_groups(exec, &tuples, &tuple_count);
    if (err != null) return err;
    defer deduce_encoded_tuples_release(tuples, tuple_count);

    long current_count = 0;
    err = deduce_relation_count_rows(exec.txn, exec.head_dbi, &current_count, exec.interp);
    if (err != null) return err;

    long retained_count = 0;
    for (usz i = 0; i < tuple_count; i++) {
        char empty_key = 0;
        MdbVal key = { .mv_size = tuples[i].key_len, .mv_data = tuples[i].key_len > 0 ? tuples[i].key_bytes : &empty_key };
        MdbVal existing = {};
        if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
            if (retained_count < long.max) retained_count++;
        }
    }

    bool changed = current_count != (long)tuple_count || retained_count != (long)tuple_count;
    if (!changed) {
        if (exec.head_schema != null) {
            exec.head_schema.cardinality_estimate = (ulong)tuple_count;
            exec.head_schema.distinct_estimate = (ulong)tuple_count;
        }
        return null;
    }

    int rc = mdb_drop(exec.txn, exec.head_dbi, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to reset aggregate head relation");
    }
    if (exec.head_schema != null) {
        exec.head_schema.cardinality_estimate = 0;
        exec.head_schema.distinct_estimate = 0;
    }

    for (usz i = 0; i < tuple_count; i++) {
        err = deduce_naive_insert_encoded_head_tuple(exec, tuples[i].key_bytes, tuples[i].key_len);
        if (err != null) return err;
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) {
        long changed_rows = current_count + (long)tuple_count - 2 * retained_count;
        if (changed_rows < 0) changed_rows = 0;
        if (*exec.derived_out <= long.max - changed_rows) {
            *exec.derived_out = *exec.derived_out + changed_rows;
        } else {
            *exec.derived_out = long.max;
        }
    }
    return null;
}

fn Value* deduce_naive_emit_head_fact(
    DeduceNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (exec.sig.head_term_count != exec.head_tuple.len) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid head term metadata");
    }
    if (exec.sig.head_term_is_variable == null || exec.sig.head_term_var_ids == null || exec.sig.head_term_literals == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing head term metadata");
    }
    if (deduce_rule_signature_has_aggregates(exec.sig)) {
        return deduce_naive_accumulate_aggregate_binding(exec, var_bound, var_values);
    }

    for (usz i = 0; i < exec.sig.head_term_count; i++) {
        if (exec.sig.head_term_is_variable[i]) {
            uint var_id = exec.sig.head_term_var_ids[i];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
                return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound head variable at runtime");
            }
            exec.head_tuple[i] = var_values[var_id];
        } else {
            exec.head_tuple[i] = exec.sig.head_term_literals[i];
        }
    }
    return deduce_naive_insert_head_tuple(exec);
}
