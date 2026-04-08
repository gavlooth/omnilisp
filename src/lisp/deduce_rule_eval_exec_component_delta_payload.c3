module lisp;

import std::core::mem;

struct DeduceEncodedTuple {
    char* key_bytes;
    usz   key_len;
}

struct DeduceDeltaTupleSet {
    Value*             seen_map;
    DeduceEncodedTuple* tuples;
    usz                count;
    usz                capacity;
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
    usz                                 component_id;
    DeduceSerializedComponentDeltaEntry* entries;
    usz                                 entry_count;
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
