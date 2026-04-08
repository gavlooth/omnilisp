module lisp;

import std::core::mem;

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
