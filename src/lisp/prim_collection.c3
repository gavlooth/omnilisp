module lisp;

import std::io;
import std::core::mem;
import main;

// =============================================================================
// COLLECTION PRIMITIVES
// =============================================================================

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

    // Dict count
    if (args[0].tag == HASHMAP) {
        return make_int(interp, (long)args[0].hashmap_val.count);
    }

    // Count cons cells
    if (!is_cons(args[0])) {
        return raise_error(interp, "length: expected list, array, dict, or string");
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
        default: return 0;
    }
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
    key = promote_to_root(key, interp);
    value = promote_to_root(value, interp);

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
    // Allocate value in root_scope so it persists
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Value* v = interp.alloc_value();
    v.tag = HASHMAP;
    main::scope_register_dtor(interp.root_scope, (void*)v, &scope_dtor_value);
    v.hashmap_val = hashmap_new(capacity, interp);
    interp.current_scope = saved_scope;
    return v;
}

// Dict primitives
fn Value* prim_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len % 2 != 0) return raise_error(interp, "dict: expected even number of args");
    uint cap = 16;
    while (cap < (uint)args.len * 2) cap *= 2;
    Value* v = make_hashmap(interp, cap);
    for (usz i = 0; i < args.len; i += 2) {
        hashmap_set(v.hashmap_val, args[i], args[i + 1], interp);
    }
    return v;
}

fn Value* prim_dict_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "dict-set!: expected 3 arguments");
    if (args[0].tag != HASHMAP) return raise_error(interp, "dict-set!: expected dict");
    hashmap_set(args[0].hashmap_val, args[1], args[2], interp);
    return args[0];
}

// prim_dict_has, prim_dict_remove, prim_dict_keys, prim_dict_values
// inlined into their dispatch wrappers (prim_has, prim_remove, prim_keys, prim_values)


// =============================================================================
// PHASE 1-7 NEW PRIMITIVES
// =============================================================================

// --- Phase 3: Sorting ---

fn Value* prim_sort(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "sort: expected list argument");
    if (is_nil(args[0])) return make_nil(interp);
    // Convert list to array
    Value*[256] arr;
    usz len = 0;
    Value* cur = args[0];
    while (is_cons(cur) && len < 256) {
        arr[len++] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    // Insertion sort (stable)
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        // Compare: numbers by value, strings lexicographic
        while (j >= 0) {
            bool gt = false;
            if (is_number(arr[(usz)j]) && is_number(key)) {
                gt = to_double(arr[(usz)j]) > to_double(key);
            } else if (arr[(usz)j].tag == STRING && key.tag == STRING) {
                // Lexicographic string comparison
                usz al = arr[(usz)j].str_len; usz bl = key.str_len;
                usz ml = al < bl ? al : bl;
                int cmp = 0;
                for (usz k = 0; k < ml; k++) {
                    if (arr[(usz)j].str_chars[k] != key.str_chars[k]) {
                        cmp = arr[(usz)j].str_chars[k] > key.str_chars[k] ? 1 : -1;
                        break;
                    }
                }
                if (cmp == 0) { cmp = al > bl ? 1 : (al < bl ? -1 : 0); }
                gt = cmp > 0;
            }
            if (!gt) break;
            arr[(usz)(j + 1)] = arr[(usz)j];
            j--;
        }
        arr[(usz)(j + 1)] = key;
    }
    // Rebuild list
    Value* result = make_nil(interp);
    for (isz i = (isz)len - 1; i >= 0; i--) {
        result = make_cons(interp, arr[(usz)i], result);
    }
    return result;
}

fn Value* prim_sort_by(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "sort-by: expected comparator and list");
    Value* cmp_fn = args[0];
    if (is_nil(args[1])) return make_nil(interp);
    // Convert list to array
    Value*[256] arr;
    usz len = 0;
    Value* cur = args[1];
    while (is_cons(cur) && len < 256) {
        arr[len++] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    // Insertion sort with custom comparator
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        while (j >= 0) {
            // Call comparator: (cmp a b) should return negative/0/positive or truthy for a>b
            Value* r1 = jit_apply_value(cmp_fn, arr[(usz)j], interp);
            if (r1 == null || r1.tag == ERROR) break;
            Value* r2 = jit_apply_value(r1, key, interp);
            if (r2 == null || r2.tag == ERROR) break;
            // If result > 0 (or truthy for >), swap
            bool gt = false;
            if (is_int(r2)) { gt = r2.int_val > 0; }
            else if (is_double(r2)) { gt = r2.double_val > 0.0; }
            else { gt = !is_nil(r2); }
            if (!gt) break;
            arr[(usz)(j + 1)] = arr[(usz)j];
            j--;
        }
        arr[(usz)(j + 1)] = key;
    }
    // Rebuild list
    Value* result = make_nil(interp);
    for (isz i = (isz)len - 1; i >= 0; i--) {
        result = make_cons(interp, arr[(usz)i], result);
    }
    return result;
}


// --- Phase 8: Array primitives ---

fn Value* prim_array(Value*[] args, Env* env, Interp* interp) {
    // Single list/nil arg → convert to array
    if (args.len == 1 && (is_cons(args[0]) || is_nil(args[0]))) {
        usz count = 0;
        Value* cur = args[0];
        while (is_cons(cur) && count < 65536) { count++; cur = cur.cons_val.cdr; }
        Value* v = make_array(interp, count);
        cur = args[0];
        for (usz i = 0; i < count; i++) {
            v.array_val.items[i] = promote_to_root(cur.cons_val.car, interp);
            cur = cur.cons_val.cdr;
        }
        v.array_val.length = count;
        return v;
    }

    Value* v = make_array(interp, args.len < 4 ? 4 : args.len);
    for (usz i = 0; i < args.len; i++) {
        v.array_val.items[i] = promote_to_root(args[i], interp);
    }
    v.array_val.length = args.len;
    return v;
}

fn Value* prim_array_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || !is_array(args[0]) || !is_int(args[1])) return raise_error(interp, "array-set!: expected array, int, value");
    long idx = args[1].int_val;
    long alen = (long)args[0].array_val.length;
    if (idx < 0) idx += alen;
    if (idx < 0 || idx >= alen) return raise_error(interp, "array-set!: index out of range");
    args[0].array_val.items[(usz)idx] = promote_to_root(args[2], interp);
    return make_nil(interp);
}

fn Value* prim_array_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return raise_error(interp, "array-length: expected array");
    return make_int(interp, (long)args[0].array_val.length);
}

fn Value* prim_array_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return raise_error(interp, "array->list: expected array");
    Array* vec = args[0].array_val;
    Value* result = make_nil(interp);
    for (isz i = (isz)vec.length - 1; i >= 0; i--) {
        result = make_cons(interp, vec.items[(usz)i], result);
    }
    return result;
}

fn Value* prim_list_to_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "list->array: expected list");
    // Count elements
    usz count = 0;
    Value* cur = args[0];
    while (is_cons(cur) && count < 65536) { count++; cur = cur.cons_val.cdr; }
    Value* v = make_array(interp, count);
    cur = args[0];
    for (usz i = 0; i < count; i++) {
        v.array_val.items[i] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    v.array_val.length = count;
    return v;
}

fn Value* prim_is_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "array?: expected 1 argument");
    return is_array(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_array_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_array(args[0])) return raise_error(interp, "push!: expected array and value");
    Array* vec = args[0].array_val;
    if (vec.length >= vec.capacity) {
        usz new_cap = vec.capacity * 2;
        Value** new_items = (Value**)mem::malloc(Value*.sizeof * new_cap);
        for (usz i = 0; i < vec.length; i++) { new_items[i] = vec.items[i]; }
        mem::free(vec.items);
        vec.items = new_items;
        vec.capacity = new_cap;
    }
    vec.items[vec.length++] = promote_to_root(args[1], interp);
    return make_nil(interp);
}

// --- Generic collection primitives ---

fn Value* prim_ref(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "ref: expected collection and key");
    Value* coll = args[0];

    // Array: (ref arr idx) — supports negative indexing
    if (coll.tag == ARRAY) {
        if (!is_int(args[1])) return raise_error(interp, "ref: array requires int index");
        long idx = args[1].int_val;
        long alen = (long)coll.array_val.length;
        if (idx < 0) idx += alen;
        if (idx < 0 || idx >= alen) { return raise_error(interp, "ref: array index out of bounds"); }
        return coll.array_val.items[(usz)idx];
    }

    // Dict: (ref dict key)
    if (coll.tag == HASHMAP) {
        Value* result = hashmap_get(coll.hashmap_val, args[1]);
        if (result == null) return make_nil(interp);
        return result;
    }

    // Cons/list: walk cons chain — supports negative indexing
    if (coll.tag == CONS) {
        if (!is_int(args[1])) return raise_error(interp, "ref: list requires int index");
        long idx = args[1].int_val;
        if (idx < 0) {
            long len = 0;
            Value* c = coll;
            while (is_cons(c)) { len++; c = c.cons_val.cdr; }
            idx += len;
            if (idx < 0) return raise_error(interp, "ref: list index out of bounds");
        }
        Value* current = coll;
        for (long i = 0; i < idx; i++) {
            if (!is_cons(current)) return raise_error(interp, "ref: list index out of bounds");
            current = cdr(current);
        }
        if (is_cons(current)) return car(current);
        // Dotted pair terminal: (ref (cons 10 20) 1) => 20
        if (idx > 0) return current;
        return raise_error(interp, "ref: list index out of bounds");
    }

    // String: (ref str idx) = char as int — supports negative indexing
    if (coll.tag == STRING) {
        if (!is_int(args[1])) return raise_error(interp, "ref: string requires int index");
        long idx = args[1].int_val;
        long slen = (long)coll.str_len;
        if (idx < 0) idx += slen;
        if (idx < 0 || idx >= slen) { return raise_error(interp, "ref: string index out of bounds"); }
        return make_int(interp, (long)coll.str_chars[(usz)idx]);
    }

    return raise_error(interp, "ref: expected array, dict, cons, or string");
}

fn Value* prim_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "push!: expected 2 arguments");
    if (!is_array(args[0])) return raise_error(interp, "push!: expected array");
    return prim_array_push(args, env, interp);
}

fn Value* prim_keys(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "keys: expected 1 argument");
    if (args[0].tag != HASHMAP) return raise_error(interp, "keys: expected dict");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].key, result);
        }
    }
    return result;
}

fn Value* prim_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "values: expected 1 argument");
    if (args[0].tag != HASHMAP) return raise_error(interp, "values: expected dict");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].value, result);
        }
    }
    return result;
}

fn Value* prim_has(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "has?: expected 2 arguments");
    if (args[0].tag != HASHMAP) return raise_error(interp, "has?: expected dict");
    Value* found = hashmap_get(args[0].hashmap_val, args[1]);
    return found != null ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "remove!: expected 2 arguments");
    if (args[0].tag != HASHMAP) return raise_error(interp, "remove!: expected dict");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return args[0];
}

// --- Phase 9: Set primitives (built on dict) ---

fn Value* prim_set(Value*[] args, Env* env, Interp* interp) {
    // Create a new hash map in root_scope, using args as keys, value = true
    uint cap = 16;
    Value* map_val = make_hashmap(interp, cap);
    HashMap* map = map_val.hashmap_val;
    Value* true_val = make_symbol(interp, interp.sym_true);
    for (usz i = 0; i < args.len; i++) {
        hashmap_set(map, args[i], true_val, interp);
    }
    return map_val;
}

fn Value* prim_set_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return raise_error(interp, "set-add: expected set and value");
    Value* true_val = make_symbol(interp, interp.sym_true);
    hashmap_set(args[0].hashmap_val, args[1], true_val, interp);
    return args[0];
}

fn Value* prim_set_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return raise_error(interp, "set-remove: expected set and value");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return args[0];
}

fn Value* prim_set_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != HASHMAP) return raise_error(interp, "set-contains?: expected set and value");
    Value* v = hashmap_get(args[0].hashmap_val, args[1]);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_set_size(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != HASHMAP) return raise_error(interp, "set-size: expected set");
    return make_int(interp, (long)args[0].hashmap_val.count);
}

fn Value* prim_set_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != HASHMAP) return raise_error(interp, "set->list: expected set");
    HashMap* map = args[0].hashmap_val;
    Value* result = make_nil(interp);
    for (uint i = 0; i < map.capacity; i++) {
        if (map.entries[i].key != null) {
            result = make_cons(interp, map.entries[i].key, result);
        }
    }
    return result;
}
