module lisp;

import std::core::mem;
import main;

bool g_hashmap_force_struct_alloc_fail = false;
bool g_hashmap_force_entries_alloc_fail = false;
bool g_hashmap_force_grow_entries_alloc_fail = false;

fn bool hashmap_is_pow2(uint value) @inline {
    return value != 0 && (value & (value - 1)) == 0;
}

fn bool hashmap_normalize_capacity(uint requested, uint* out) @inline {
    if (out == null || requested == 0) return false;
    uint capacity = 1;
    while (capacity < requested) {
        if (capacity > uint.max / (uint)2) return false;
        capacity *= 2;
    }
    *out = capacity;
    return true;
}

// Collection primitives: length + hashmap/dict core.
fn Value* dict_ctor_type_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, null);
}

fn Value* dict_ctor_runtime_raise(Interp* interp, char[] message) @inline {
    return collection_raise(interp, "type/arg-mismatch", message);
}

<*
@require interp != null
*>
fn Value* prim_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return collection_raise(interp, "type/arity", "length: expected 1 argument");

    // nil has length 0
    if (is_nil(args[0])) {
        return make_int(interp, 0);
    }

    // String length is codepoint-based; use string-byte-length for bytes.
    if (is_string(args[0])) {
        return make_int(interp, (long)utf8_strlen(args[0].str_chars[:args[0].str_len]));
    }

    // Array length
    if (is_array(args[0])) {
        if (args[0].array_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "length: array backing storage is missing");
        return make_int(interp, (long)args[0].array_val.length);
    }

    // Tensor element count
    if (is_tensor(args[0])) {
        if (args[0].tensor_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "length: tensor backing storage is missing");
        if (args[0].tensor_val.element_count > (usz)long.max) return collection_raise(interp, "type/range-error", "length: tensor element count is too large");
        return make_int(interp, (long)args[0].tensor_val.element_count);
    }

    // Dictionary / Set count
    if (is_hashmap_backed(args[0])) {
        if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "length: dict/set backing storage is missing");
        return make_int(interp, (long)args[0].hashmap_val.count);
    }

    // Count cons cells
    if (!is_cons(args[0])) {
        return collection_raise(interp, "type/arg-mismatch", "length: expected list, array, tensor, dict, set, or string");
    }

    long count = 0;
    Value* current = args[0];
    while (is_cons(current)) {
        count++;
        current = current.cons_val.cdr;
    }
    if (!is_nil(current)) count++;
    return make_int(interp, count);
}

// =============================================================================
// HASH MAP OPERATIONS
// =============================================================================

fn HashEntry* hashmap_alloc_entries(uint capacity, bool force_fail = false) @inline {
    if (force_fail) return null;
    if (!hashmap_is_pow2(capacity)) return null;
    if ((usz)capacity > usz.max / HashEntry.sizeof) return null;
    HashEntry* entries = (HashEntry*)mem::malloc(HashEntry.sizeof * capacity);
    if (entries == null) return null;
    for (uint i = 0; i < capacity; i++) {
        entries[i].key = null;
        entries[i].value = null;
    }
    return entries;
}

fn HashMap* hashmap_new_checked(uint capacity, Interp* interp) {
    if (!hashmap_normalize_capacity(capacity, &capacity)) return null;
    HashMap* map = g_hashmap_force_struct_alloc_fail
        ? null
        : (HashMap*)mem::malloc(HashMap.sizeof);
    if (map == null) return null;
    map.capacity = capacity;
    map.count = 0;
    map.mask = capacity - 1;
    map.entries = hashmap_alloc_entries(capacity, g_hashmap_force_entries_alloc_fail);
    if (map.entries == null) {
        mem::free(map);
        return null;
    }
    return map;
}

fn HashMap* hashmap_new(uint capacity, Interp* interp) {
    return hashmap_new_checked(capacity, interp);
}

fn void hashmap_free_payload(HashMap* map) {
    if (map == null) return;
    if (map.entries != null) mem::free(map.entries);
    mem::free(map);
}

fn Value* hashmap_get(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return null;  // empty slot
        if (hashmap_key_equals(entry.key, key)) return entry.value;
    }
    return null;
}

fn void hashmap_cleanup_failed_promoted_value(Value* original, Value* promoted, Interp* interp) {
    if (promoted == null || promoted == original || interp == null || interp.root_scope == null) return;
    boundary_cleanup_materialized_value(original, promoted, interp.root_scope);
}

struct HashMapSetRollback {
    Value* original_key;
    Value* inserted_key;
    Value* original_value;
    Value* inserted_value;
    bool inserted;
}

fn bool hashmap_set_symbol_checked(HashMap* map, SymbolId key, Value* value, Interp* interp) {
    if (map == null || map.entries == null || !hashmap_is_pow2(map.capacity)) return false;
    if ((usz)map.count * (usz)10 >= (usz)map.capacity * (usz)7) {
        if (!hashmap_grow_checked(map, interp)) return false;
    }

    Value* key_value = make_symbol(interp, key);
    Value* original_value = value;
    key_value = boundary_promote_to_root(key_value, interp);
    value = boundary_promote_to_root(value, interp);
    if (key_value == null || key_value.tag == ERROR || value == null || value.tag == ERROR) {
        hashmap_cleanup_failed_promoted_value(null, key_value, interp);
        hashmap_cleanup_failed_promoted_value(original_value, value, interp);
        return false;
    }

    uint h = murmur_finalizer((uint)key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) {
            entry.key = key_value;
            entry.value = value;
            map.count++;
            stable_graph_note_mutation_ptr((void*)map);
            return true;
        }
        if (entry.key.tag == SYMBOL && entry.key.sym_val == key) {
            entry.value = value;
            stable_graph_note_mutation_ptr((void*)map);
            return true;
        }
    }
    hashmap_cleanup_failed_promoted_value(null, key_value, interp);
    hashmap_cleanup_failed_promoted_value(original_value, value, interp);
    return false;
}

fn bool hashmap_set_symbol(HashMap* map, SymbolId key, Value* value, Interp* interp) {
    return hashmap_set_symbol_checked(map, key, value, interp);
}

fn bool hashmap_grow_checked(HashMap* map, Interp* interp) {
    if (map == null || map.entries == null || !hashmap_is_pow2(map.capacity)) return false;
    uint old_cap = map.capacity;
    HashEntry* old_entries = map.entries;
    if (old_cap == 0 || old_cap > uint.max / (uint)2) return false;
    uint new_cap = old_cap * 2;
    uint new_mask = new_cap - 1;

    HashEntry* new_entries = hashmap_alloc_entries(new_cap, g_hashmap_force_grow_entries_alloc_fail);
    if (new_entries == null) return false;

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
    stable_graph_note_mutation_ptr((void*)map);
    return true;
}

fn bool hashmap_grow(HashMap* map, Interp* interp) {
    return hashmap_grow_checked(map, interp);
}

fn bool hashmap_set_checked(HashMap* map, Value* key, Value* value, Interp* interp) {
    if (map == null || map.entries == null || !hashmap_is_pow2(map.capacity)) return false;
    // Grow at 70% load
    if ((usz)map.count * (usz)10 >= (usz)map.capacity * (usz)7) {
        if (!hashmap_grow_checked(map, interp)) return false;
    }

    // Promote key/value to root_scope — hashmap lives in root_scope,
    // so its entries must survive child scope release
    Value* original_key = key;
    Value* original_value = value;
    key = boundary_promote_to_root(key, interp);
    value = boundary_promote_to_root(value, interp);
    if (key == null || key.tag == ERROR || value == null || value.tag == ERROR) {
        hashmap_cleanup_failed_promoted_value(original_key, key, interp);
        hashmap_cleanup_failed_promoted_value(original_value, value, interp);
        return false;
    }

    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) {
            entry.key = key;
            entry.value = value;
            map.count++;
            stable_graph_note_mutation_ptr((void*)map);
            return true;
        }
        if (hashmap_key_equals(entry.key, key)) {
            entry.value = value;
            stable_graph_note_mutation_ptr((void*)map);
            return true;
        }
    }
    hashmap_cleanup_failed_promoted_value(original_key, key, interp);
    hashmap_cleanup_failed_promoted_value(original_value, value, interp);
    return false;
}

fn bool hashmap_set_checked_rollback_record(
    HashMap* map,
    Value* key,
    Value* value,
    Interp* interp,
    HashMapSetRollback* rollback
) {
    if (rollback != null) *rollback = {};
    if (map == null || map.entries == null || !hashmap_is_pow2(map.capacity)) return false;
    // Grow at 70% load.
    if ((usz)map.count * (usz)10 >= (usz)map.capacity * (usz)7) {
        if (!hashmap_grow_checked(map, interp)) return false;
    }

    Value* original_key = key;
    Value* original_value = value;
    key = boundary_promote_to_root(key, interp);
    value = boundary_promote_to_root(value, interp);
    if (key == null || key.tag == ERROR || value == null || value.tag == ERROR) {
        hashmap_cleanup_failed_promoted_value(original_key, key, interp);
        hashmap_cleanup_failed_promoted_value(original_value, value, interp);
        return false;
    }

    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) {
            entry.key = key;
            entry.value = value;
            map.count++;
            stable_graph_note_mutation_ptr((void*)map);
            if (rollback != null) {
                rollback.original_key = original_key;
                rollback.inserted_key = key;
                rollback.original_value = original_value;
                rollback.inserted_value = value;
                rollback.inserted = true;
            }
            return true;
        }
        if (hashmap_key_equals(entry.key, key)) {
            entry.value = value;
            stable_graph_note_mutation_ptr((void*)map);
            if (rollback != null) {
                rollback.original_key = original_key;
                rollback.inserted_key = key;
                rollback.original_value = original_value;
                rollback.inserted_value = value;
                rollback.inserted = true;
            }
            return true;
        }
    }
    hashmap_cleanup_failed_promoted_value(original_key, key, interp);
    hashmap_cleanup_failed_promoted_value(original_value, value, interp);
    return false;
}

fn void hashmap_cleanup_rollback_record(HashMapSetRollback* rollback, Interp* interp) {
    if (rollback == null || !rollback.inserted) return;
    hashmap_cleanup_failed_promoted_value(rollback.original_key, rollback.inserted_key, interp);
    if (rollback.inserted_value != rollback.inserted_key) {
        hashmap_cleanup_failed_promoted_value(rollback.original_value, rollback.inserted_value, interp);
    }
    *rollback = {};
}

fn bool hashmap_set(HashMap* map, Value* key, Value* value, Interp* interp) {
    return hashmap_set_checked(map, key, value, interp);
}

fn bool hashmap_remove(HashMap* map, Value* key) {
    uint h = hash_value(key) & map.mask;
    for (uint i = 0; i < map.capacity; i++) {
        uint idx = (h + i) & map.mask;
        HashEntry* entry = &map.entries[idx];
        if (entry.key == null) return false;
        if (hashmap_key_equals(entry.key, key)) {
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
            stable_graph_note_mutation_ptr((void*)map);
            return true;
        }
    }
    return false;
}

fn Value* make_hashmap_no_raise(Interp* interp, uint capacity) {
    HashMap* map = hashmap_new_checked(capacity, interp);
    if (map == null) return null;

    Value* v = boundary_alloc_value_in_root(interp, true);
    if (v == null) {
        hashmap_free_payload(map);
        return null;
    }
    v.tag = HASHMAP;
    v.hashmap_val = map;
    return v;
}

fn Value* make_hashmap_checked(Interp* interp, uint capacity, char[] oom_message) {
    Value* v = make_hashmap_no_raise(interp, capacity);
    if (v == null) return dict_ctor_runtime_raise(interp, oom_message);
    return v;
}

fn Value* make_hashmap(Interp* interp, uint capacity) {
    return make_hashmap_checked(interp, capacity, "Dictionary: out of memory");
}

fn Value* make_set_checked(Interp* interp, uint capacity, char[] oom_message) {
    Value* v = make_hashmap_checked(interp, capacity, oom_message);
    if (v != null && v.tag == HASHMAP) v.tag = SET;
    return v;
}

// Dictionary primitives
fn bool dict_initial_capacity_for_arg_count(usz arg_count, uint* out) {
    if (out == null) return false;
    if (arg_count > usz.max / (usz)2) return false;
    usz required = arg_count * (usz)2;
    if (required < (usz)16) required = (usz)16;
    if (required > (usz)uint.max) return false;

    uint cap = 16;
    while ((usz)cap < required) {
        if (cap > uint.max / (uint)2) return false;
        cap *= 2;
    }
    *out = cap;
    return true;
}

<*
@require interp != null
*>
fn Value* prim_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len % 2 != 0) {
        return dict_ctor_type_raise(interp, "type/arity", "Dictionary: expected even number of key/value arguments");
    }
    uint cap = 16;
    if (!dict_initial_capacity_for_arg_count(args.len, &cap)) {
        return dict_ctor_runtime_raise(interp, "Dictionary: out of memory");
    }
    Value* v = make_hashmap_checked(interp, cap, "Dictionary: out of memory");
    if (v == null || v.tag != HASHMAP) return v;
    for (usz i = 0; i < args.len; i += 2) {
        if (args[i] != null && args[i].tag == ERROR) return args[i];
        if (args[i + 1] != null && args[i + 1].tag == ERROR) return args[i + 1];
        if (!hashmap_set_checked(v.hashmap_val, args[i], args[i + 1], interp)) {
            return dict_ctor_runtime_raise(interp, "Dictionary: out of memory while growing backing storage");
        }
    }
    return v;
}

<*
@require interp != null
*>
fn Value* prim_dict_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return collection_raise(interp, "type/arity", "set!: expected collection, key, value");
    if (args[0].tag != HASHMAP) return runtime_raise(interp, "runtime/evaluation-error", "set!: generic form expects array or dict target");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set!: dict backing storage is missing");
    if (!hashmap_set(args[0].hashmap_val, args[1], args[2], interp)) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/out-of-memory",
            "set!: dict backing storage out of memory");
    }
    return make_void(interp);
}

// prim_dict_has, prim_dict_remove, prim_dict_keys, prim_dict_values
// inlined into their dispatch wrappers (prim_has, prim_remove, prim_keys, prim_values)


// =============================================================================
// PHASE 1-7 NEW PRIMITIVES
// =============================================================================
