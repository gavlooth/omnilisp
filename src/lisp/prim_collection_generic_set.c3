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
    return hashmap_keys_list_canonical(args[0].hashmap_val, interp);
}

fn Value* prim_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "values: expected 1 argument");
    if (args[0].tag != HASHMAP) return raise_error(interp, "values: expected dict");
    return hashmap_values_list_canonical(args[0].hashmap_val, interp);
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
    return make_void(interp);
}

// Generic mutating update surface:
// (set! collection key value) dispatches by collection type.
fn Value* prim_set_bang(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "set!: expected collection, key, value");
    if (args[0].tag == ARRAY) return prim_array_set(args, env, interp);
    if (args[0].tag == HASHMAP) return prim_dict_set(args, env, interp);
    return raise_error(interp, "set!: generic form expects array or dict target");
}

// --- Phase 9: Set primitives ---

fn uint set_constructor_capacity(usz elem_count) @inline {
    uint cap = 16;
    while (cap < (uint)elem_count * 2) cap *= 2;
    return cap;
}

fn Value* prim_set(Value*[] args, Env* env, Interp* interp) {
    uint cap = set_constructor_capacity(args.len);
    Value* set_val = make_set(interp, cap);
    HashMap* map = set_val.hashmap_val;
    Value* true_val = make_symbol(interp, interp.sym_true);
    for (usz i = 0; i < args.len; i++) {
        hashmap_set(map, args[i], true_val, interp);
    }
    return set_val;
}

fn Value* prim_set_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-add: expected set and value");
    Value* true_val = make_symbol(interp, interp.sym_true);
    hashmap_set(args[0].hashmap_val, args[1], true_val, interp);
    return make_void(interp);
}

fn Value* prim_set_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-remove: expected set and value");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return make_void(interp);
}

fn Value* prim_set_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-contains?: expected set and value");
    Value* v = hashmap_get(args[0].hashmap_val, args[1]);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_set_size(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != SET) return raise_error(interp, "set-size: expected set");
    return make_int(interp, (long)args[0].hashmap_val.count);
}

fn Value* prim_set_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != SET) return raise_error(interp, "set->list: expected set");
    return hashmap_keys_list_canonical(args[0].hashmap_val, interp);
}
