module lisp;

import std::core::mem;

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

    if (set.count == usz.max) return false;
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
        if (new_cap > usz.max / DeduceEncodedTuple.sizeof) return false;
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
