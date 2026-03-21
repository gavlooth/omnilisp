module lisp;

import std::io;
import std::core::mem;
import main;

// Collection primitives: length + hashmap/dict core.
fn Value* dict_ctor_type_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, null);
}

fn Value* prim_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "length: expected 1 argument");

    // nil has length 0
    if (is_nil(args[0])) {
        return make_int(interp, 0);
    }

    // String length
    if (is_string(args[0])) {
        return make_int(interp, (long)args[0].str_len);
    }

    // Array length
    if (is_array(args[0])) {
        return make_int(interp, (long)args[0].array_val.length);
    }

    // Dict / Set count
    if (is_hashmap_backed(args[0])) {
        return make_int(interp, (long)args[0].hashmap_val.count);
    }

    // Count cons cells
    if (!is_cons(args[0])) {
        return raise_error(interp, "length: expected list, array, dict, set, or string");
    }

    long count = 0;
    Value* current = args[0];
    while (is_cons(current)) {
        count++;
        current = current.cons_val.cdr;
    }
    return make_int(interp, count);
}

// =============================================================================
// HASH MAP OPERATIONS
// =============================================================================

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
            if (map.entries[i].key != null) fallback = make_cons(interp, map.entries[i].key, fallback);
        }
        return fallback;
    }
    defer mem::free(slots.ptr);

    Value* result = make_nil(interp);
    for (isz i = (isz)slots.len - 1; i >= 0; i--) {
        uint slot = slots[(usz)i];
        result = make_cons(interp, map.entries[slot].key, result);
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
            if (map.entries[i].key != null) fallback = make_cons(interp, map.entries[i].value, fallback);
        }
        return fallback;
    }
    defer mem::free(slots.ptr);

    Value* result = make_nil(interp);
    for (isz i = (isz)slots.len - 1; i >= 0; i--) {
        uint slot = slots[(usz)i];
        result = make_cons(interp, map.entries[slot].value, result);
    }
    return result;
}

fn HashMap* hashmap_new(uint capacity, Interp* interp) {
    // Allocate HashMap struct via malloc (freed by scope_dtor_value)
    HashMap* map = (HashMap*)mem::malloc(HashMap.sizeof);
    map.capacity = capacity;
    map.count = 0;
    map.mask = capacity - 1;

    // Allocate entries array via malloc (contiguous needed for indexing)
    map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * capacity);
    for (uint i = 0; i < capacity; i++) {
        map.entries[i].key = null;
        map.entries[i].value = null;
    }

    return map;
}

fn Value* hashmap_get(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return null;  // empty slot
        if (values_equal(entry.key, key)) return entry.value;
    }
    return null;
}

fn void hashmap_grow(HashMap* map, Interp* interp) {
    uint old_cap = map.capacity;
    HashEntry* old_entries = map.entries;
    uint new_cap = old_cap * 2;
    uint new_mask = new_cap - 1;

    // Allocate new entries array via malloc
    HashEntry* new_entries = (HashEntry*)mem::malloc(HashEntry.sizeof * new_cap);
    for (uint i = 0; i < new_cap; i++) {
        new_entries[i].key = null;
        new_entries[i].value = null;
    }

    map.entries = new_entries;
    map.capacity = new_cap;
    map.mask = new_mask;
    map.count = 0;

    // Rehash
    for (uint i = 0; i < old_cap; i++) {
        if (old_entries[i].key != null) {
            uint h = hash_value(old_entries[i].key) & new_mask;
            for (uint j = 0; j < new_cap; j++) {
                uint idx = (h + j) & new_mask;
                if (new_entries[idx].key == null) {
                    new_entries[idx].key = old_entries[i].key;
                    new_entries[idx].value = old_entries[i].value;
                    map.count++;
                    break;
                }
            }
        }
    }
    mem::free(old_entries);
}

fn void hashmap_set(HashMap* map, Value* key, Value* value, Interp* interp) {
    // Grow at 70% load
    if (map.count * 10 >= map.capacity * 7) {
        hashmap_grow(map, interp);
    }

    // Promote key/value to root_scope — hashmap lives in root_scope,
    // so its entries must survive child scope release
    key = boundary_promote_to_root(key, interp);
    value = boundary_promote_to_root(value, interp);

    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) {
            entry.key = key;
            entry.value = value;
            map.count++;
            return;
        }
        if (values_equal(entry.key, key)) {
            entry.value = value;
            return;
        }
    }
}

fn bool hashmap_remove(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return false;
        if (values_equal(entry.key, key)) {
            // Backward shift delete
            uint j = idx;
            for (;;) {
                uint next = (j + 1) & map.mask;
                if (map.entries[next].key == null) break;
                uint natural = hash_value(map.entries[next].key) & map.mask;
                // Check if next entry would be displaced by removing j
                // If natural position is between (j+1) and next, it's fine where it is
                bool should_move;
                if (j < next) {
                    should_move = natural <= j || natural > next;
                } else {
                    should_move = natural <= j && natural > next;
                }
                if (!should_move) break;
                map.entries[j] = map.entries[next];
                j = next;
            }
            map.entries[j].key = null;
            map.entries[j].value = null;
            map.count--;
            return true;
        }
    }
    return false;
}

fn Value* make_hashmap(Interp* interp, uint capacity) {
    // Allocate value in root_scope so it persists.
    Value* v = boundary_alloc_value_in_root(interp, true);
    v.tag = HASHMAP;
    v.hashmap_val = hashmap_new(capacity, interp);
    return v;
}

// Dict primitives
fn Value* prim_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len % 2 != 0) {
        return dict_ctor_type_raise(interp, "type/arity", "Dictionary: expected even number of key/value arguments");
    }
    uint cap = 16;
    while (cap < (uint)args.len * 2) cap *= 2;
    Value* v = make_hashmap(interp, cap);
    for (usz i = 0; i < args.len; i += 2) {
        hashmap_set(v.hashmap_val, args[i], args[i + 1], interp);
    }
    return v;
}

fn Value* prim_dict_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "set!: expected collection, key, value");
    if (args[0].tag != HASHMAP) return raise_error(interp, "set!: generic form expects array or dict target");
    hashmap_set(args[0].hashmap_val, args[1], args[2], interp);
    return make_void(interp);
}

// prim_dict_has, prim_dict_remove, prim_dict_keys, prim_dict_values
// inlined into their dispatch wrappers (prim_has, prim_remove, prim_keys, prim_values)


// =============================================================================
// PHASE 1-7 NEW PRIMITIVES
// =============================================================================
