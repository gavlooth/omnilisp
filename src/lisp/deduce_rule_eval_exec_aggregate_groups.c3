module lisp;

import std::core::mem;

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
