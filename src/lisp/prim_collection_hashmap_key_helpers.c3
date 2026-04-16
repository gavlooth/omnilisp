module lisp;

import std::core::mem;
import main;

fn uint fnv1a(char[] data) {
    uint h = 2166136261;
    for (usz i = 0; i < data.len; i++) { h ^= (uint)data[i]; h *= 16777619; }
    return h;
}

fn uint murmur_finalizer(uint h) {
    h ^= h >> 16; h *= 0x85ebca6b; h ^= h >> 13; h *= 0xc2b2ae35; h ^= h >> 16;
    return h;
}

// ValueKey — wrapper for using Value* as C3 map key
// Provides .hash() and == required by std::collections::map
struct ValueKey {
    Value* val;
}

fn uint ValueKey.hash(self) {
    return hash_value(self.val);
}

fn bool ValueKey.is_eq(self, ValueKey other) {
    return values_equal(self.val, other.val);
}

// Custom HashMap and Array RETAINED — C3's generic collections require
// equatable Value types (map) or add churn for no benefit (list).
// These are Omni language types with scope-region integration, not reimplementations.

fn uint hash_value(Value* key) {
    if (key == null) return 0;
    switch (key.tag) {
        case INT: return murmur_finalizer((uint)key.int_val);
        case FLOAT32: {
            char[64] buf;
            return fnv1a(double_to_string((double)key.float32_val, &buf));
        }
        case COMPLEX128: {
            char[160] buf;
            PrintBuf pb;
            pb.buf = &buf[0];
            pb.pos = 0;
            pb.capacity = buf.len;
            fixed_complex_append_f64(key.complex128_val.real, key.complex128_val.imag, &pb);
            buf[pb.pos] = 0;
            return fnv1a(buf[0:pb.pos]);
        }
        case COMPLEX64: {
            char[160] buf;
            PrintBuf pb;
            pb.buf = &buf[0];
            pb.pos = 0;
            pb.capacity = buf.len;
            fixed_complex_append_f64((double)key.complex64_val.real, (double)key.complex64_val.imag, &pb);
            buf[pb.pos] = 0;
            return fnv1a(buf[0:pb.pos]);
        }
        case BIG_INTEGER: {
            if (key.big_integer_val == null) return 0;
            usz len = 0;
            char* rendered = omni_big_integer_to_string(key.big_integer_val, &len);
            if (rendered == null) return 0;
            uint hashed = fnv1a(rendered[:len]);
            omni_big_integer_string_free(rendered);
            return hashed;
        }
        case BIG_FLOAT: {
            if (key.big_float_val == null) return 0;
            usz len = 0;
            char* rendered = omni_big_float_to_string(key.big_float_val, &len);
            if (rendered == null) return 0;
            uint hashed = fnv1a(rendered[:len]);
            omni_big_float_string_free(rendered);
            return hashed;
        }
        case BIG_COMPLEX: {
            if (key.big_complex_val == null) return 0;
            usz len = 0;
            char* rendered = omni_big_complex_to_string(key.big_complex_val, &len);
            if (rendered == null) return 0;
            uint hashed = fnv1a(rendered[:len]);
            omni_big_complex_string_free(rendered);
            return hashed;
        }
        case STRING: return fnv1a(key.str_chars[:key.str_len]);
        case SYMBOL: return murmur_finalizer((uint)key.sym_val);
        case TIME_POINT: {
            uint h = 2166136261;
            h ^= murmur_finalizer((uint)key.time_point_val.year);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.month);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.day);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.hour);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.minute);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.second);
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.microsecond);
            h *= 16777619;
            h ^= murmur_finalizer((uint)(uint)(key.time_point_val.tz_offset + 0x8000));
            h *= 16777619;
            h ^= murmur_finalizer((uint)key.time_point_val.kind);
            return murmur_finalizer(h);
        }
        default: return 0;
    }
}

fn bool hashmap_key_equals(Value* left, Value* right) {
    if (left == right) return true;
    if (left == null || right == null) return false;
    if (left.tag != right.tag) return false;

    switch (left.tag) {
        case NIL:
        case VOID:
            return true;
        case INT:
            return left.int_val == right.int_val;
        case DOUBLE:
            return left.double_val == right.double_val;
        case FLOAT32:
            return left.float32_val == right.float32_val;
        case COMPLEX128:
            return left.complex128_val.real == right.complex128_val.real &&
                left.complex128_val.imag == right.complex128_val.imag;
        case COMPLEX64:
            return left.complex64_val.real == right.complex64_val.real &&
                left.complex64_val.imag == right.complex64_val.imag;
        case BIG_INTEGER:
            return big_integer_values_equal(left, right);
        case BIG_FLOAT:
            return big_float_values_equal(left, right);
        case BIG_COMPLEX:
            return big_complex_values_equal(left, right);
        case STRING:
            if (left.str_len != right.str_len) return false;
            for (usz i = 0; i < left.str_len; i++) {
                if (left.str_chars[i] != right.str_chars[i]) return false;
            }
            return true;
        case SYMBOL:
            return left.sym_val == right.sym_val;
        case TIME_POINT:
            return left.time_point_val.year == right.time_point_val.year &&
                   left.time_point_val.month == right.time_point_val.month &&
                   left.time_point_val.day == right.time_point_val.day &&
                   left.time_point_val.hour == right.time_point_val.hour &&
                   left.time_point_val.minute == right.time_point_val.minute &&
                   left.time_point_val.second == right.time_point_val.second &&
                   left.time_point_val.microsecond == right.time_point_val.microsecond &&
                   left.time_point_val.tz_offset == right.time_point_val.tz_offset &&
                   left.time_point_val.kind == right.time_point_val.kind;
        default:
            return values_equal(left, right);
    }
}

fn int compare_char_slices(char[] left, char[] right) @inline {
    usz min_len = left.len < right.len ? left.len : right.len;
    for (usz i = 0; i < min_len; i++) {
        if (left[i] < right[i]) return -1;
        if (left[i] > right[i]) return 1;
    }
    if (left.len < right.len) return -1;
    if (left.len > right.len) return 1;
    return 0;
}

fn int compare_time_points(TimePoint left, TimePoint right) @inline {
    if (left.year != right.year) return left.year < right.year ? -1 : 1;
    if (left.month != right.month) return left.month < right.month ? -1 : 1;
    if (left.day != right.day) return left.day < right.day ? -1 : 1;
    if (left.hour != right.hour) return left.hour < right.hour ? -1 : 1;
    if (left.minute != right.minute) return left.minute < right.minute ? -1 : 1;
    if (left.second != right.second) return left.second < right.second ? -1 : 1;
    if (left.microsecond != right.microsecond) return left.microsecond < right.microsecond ? -1 : 1;
    if (left.tz_offset != right.tz_offset) return left.tz_offset < right.tz_offset ? -1 : 1;
    if (left.kind != right.kind) return left.kind < right.kind ? -1 : 1;
    return 0;
}

// Canonical ordering for dictionary/set iteration surfaces.
// This intentionally uses a deterministic total-ish order for common key types
// and a stable hash fallback for unsupported tags.
fn int hashmap_iteration_key_compare(Value* left, Value* right, Interp* interp) {
    if (left == right) return 0;
    if (left == null) return -1;
    if (right == null) return 1;
    if (left.tag != right.tag) return left.tag < right.tag ? -1 : 1;

    switch (left.tag) {
        case NIL:
        case VOID:
            return 0;
        case INT:
            if (left.int_val < right.int_val) return -1;
            if (left.int_val > right.int_val) return 1;
            return 0;
        case DOUBLE: {
            double l = left.double_val;
            double r = right.double_val;
            bool l_nan = l != l;
            bool r_nan = r != r;
            if (l_nan && r_nan) return 0;
            if (l_nan) return 1;
            if (r_nan) return -1;
            if (l < r) return -1;
            if (l > r) return 1;
            return 0;
        }
        case FLOAT32: {
            float l = left.float32_val;
            float r = right.float32_val;
            bool l_nan = l != l;
            bool r_nan = r != r;
            if (l_nan && r_nan) return 0;
            if (l_nan) return 1;
            if (r_nan) return -1;
            if (l < r) return -1;
            if (l > r) return 1;
            return 0;
        }
        case BIG_FLOAT: {
            int cmp = 0;
            if (big_float_compare_values(left, right, &cmp)) return cmp;
            return 0;
        }
        case STRING:
            return compare_char_slices(left.str_chars[:left.str_len], right.str_chars[:right.str_len]);
        case SYMBOL: {
            char[] l_name = interp.symbols.get_name(left.sym_val);
            char[] r_name = interp.symbols.get_name(right.sym_val);
            return compare_char_slices(l_name, r_name);
        }
        case TIME_POINT:
            return compare_time_points(left.time_point_val, right.time_point_val);
        default: {
            uint l_hash = hash_value(left);
            uint r_hash = hash_value(right);
            if (l_hash < r_hash) return -1;
            if (l_hash > r_hash) return 1;
            return 0;
        }
    }
}

fn uint[] hashmap_sorted_slots(HashMap* map, Interp* interp) {
    usz count = map == null ? 0 : (usz)map.count;
    uint[] slots = mem::new_array(uint, count);
    if (slots.ptr == null || count == 0) return slots;

    usz used = 0;
    for (uint i = 0; i < map.capacity && used < count; i++) {
        if (map.entries[i].key != null) {
            slots[used++] = i;
        }
    }

    // Stable insertion sort by canonical key order.
    for (usz i = 1; i < used; i++) {
        uint slot_key = slots[i];
        Value* key_value = map.entries[slot_key].key;
        isz j = (isz)i - 1;
        while (j >= 0) {
            uint slot_j = slots[(usz)j];
            Value* probe_value = map.entries[slot_j].key;
            int cmp = hashmap_iteration_key_compare(probe_value, key_value, interp);
            if (cmp <= 0) break;
            slots[(usz)(j + 1)] = slot_j;
            j--;
        }
        slots[(usz)(j + 1)] = slot_key;
    }
    return slots;
}

fn Value* hashmap_keys_list_canonical(HashMap* map, Interp* interp) {
    if (map == null || map.count == 0) return make_nil(interp);

    uint[] slots = hashmap_sorted_slots(map, interp);
    if (slots.ptr == null) {
        // Fallback under allocation pressure.
        Value* fallback = make_nil(interp);
        for (uint i = 0; i < map.capacity; i++) {
            if (map.entries[i].key == null) continue;
            Value* next = make_cons_or_error(interp, map.entries[i].key, fallback, "keys: failed to allocate result list");
            if (next == null || next.tag == ERROR) return next;
            fallback = next;
        }
        return fallback;
    }
    defer mem::free(slots.ptr);

    Value* result = make_nil(interp);
    for (isz i = (isz)slots.len - 1; i >= 0; i--) {
        uint slot = slots[(usz)i];
        Value* next = make_cons_or_error(interp, map.entries[slot].key, result, "keys: failed to allocate result list");
        if (next == null || next.tag == ERROR) return next;
        result = next;
    }
    return result;
}

fn Value* hashmap_values_list_canonical(HashMap* map, Interp* interp) {
    if (map == null || map.count == 0) return make_nil(interp);

    uint[] slots = hashmap_sorted_slots(map, interp);
    if (slots.ptr == null) {
        // Fallback under allocation pressure.
        Value* fallback = make_nil(interp);
        for (uint i = 0; i < map.capacity; i++) {
            if (map.entries[i].key == null) continue;
            Value* next = make_cons_or_error(interp, map.entries[i].value, fallback, "values: failed to allocate result list");
            if (next == null || next.tag == ERROR) return next;
            fallback = next;
        }
        return fallback;
    }
    defer mem::free(slots.ptr);

    Value* result = make_nil(interp);
    for (isz i = (isz)slots.len - 1; i >= 0; i--) {
        uint slot = slots[(usz)i];
        Value* next = make_cons_or_error(interp, map.entries[slot].value, result, "values: failed to allocate result list");
        if (next == null || next.tag == ERROR) return next;
        result = next;
    }
    return result;
}
