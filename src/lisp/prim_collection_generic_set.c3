module lisp;

import std::io;
import std::core::mem;
import main;

// Collection primitives: generic ref/push/keys/values + set operations.
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
