module lisp;

import std::core::mem;

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
        if ((usz)entry_count > usz.max / DeduceSerializedComponentDeltaEntry.sizeof) {
            return deduce_raise(interp, "deduce/parallel-invalid-component-delta-payload",
                "deduce/parallel: malformed component delta payload");
        }
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
