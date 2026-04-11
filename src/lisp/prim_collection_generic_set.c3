module lisp;

import std::io;
import std::core::mem;
import main;

// Collection primitives: generic ref/push/keys/values + set operations.
fn Value* ref_type_error(Interp* interp) {
    return raise_error(interp, "ref: expected array, dict, cons, or string");
}

fn bool ref_try_lookup_collection(Interp* interp, Value* coll, Value* key, Value** out) {
    if (coll == null) return false;

    // Array: (ref arr idx) — supports negative indexing
    if (coll.tag == ARRAY) {
        if (coll.array_val == null) {
            *out = raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "ref: array backing storage is missing");
            return true;
        }
        if (!is_int(key)) {
            *out = raise_error(interp, "ref: array requires int index");
            return true;
        }
        if (coll.array_val.length > (usz)long.max) {
            *out = raise_error(interp, "ref: array too large to index");
            return true;
        }
        long idx = key.int_val;
        long alen = (long)coll.array_val.length;
        if (idx < 0) idx += alen;
        if (idx < 0 || idx >= alen) {
            *out = raise_error(interp, "ref: array index out of bounds");
            return true;
        }
        *out = coll.array_val.items[(usz)idx];
        return true;
    }

    // Dictionary: (ref dict key)
    if (coll.tag == HASHMAP) {
        if (coll.hashmap_val == null) {
            *out = raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "ref: dict backing storage is missing");
            return true;
        }
        Value* result = hashmap_get(coll.hashmap_val, key);
        *out = result == null ? make_nil(interp) : result;
        return true;
    }

    // Cons/list: walk cons chain — supports negative indexing
    if (coll.tag == CONS) {
        if (!is_int(key)) {
            *out = raise_error(interp, "ref: list requires int index");
            return true;
        }
        long idx = key.int_val;
        if (idx < 0) {
            long len = 0;
            Value* c = coll;
            while (is_cons(c)) { len++; c = c.cons_val.cdr; }
            if (!is_nil(c)) len++;
            idx += len;
            if (idx < 0) {
                *out = raise_error(interp, "ref: list index out of bounds");
                return true;
            }
        }
        Value* current = coll;
        for (long i = 0; i < idx; i++) {
            if (!is_cons(current)) {
                *out = raise_error(interp, "ref: list index out of bounds");
                return true;
            }
            current = cdr(current);
        }
        if (is_cons(current)) {
            *out = car(current);
            return true;
        }
        // Dotted pair terminal: (ref (cons 10 20) 1) => 20
        if (idx > 0) {
            *out = current;
            return true;
        }
        *out = raise_error(interp, "ref: list index out of bounds");
        return true;
    }

    // String: codepoint-indexed character access. Use string-byte-length for bytes.
    if (coll.tag == STRING) {
        if (!is_int(key)) {
            *out = raise_error(interp, "ref: string requires int index");
            return true;
        }
        char[] bytes = coll.str_chars[:coll.str_len];
        usz cp_count = utf8_strlen(bytes);
        if (cp_count > (usz)long.max) {
            *out = raise_error(interp, "ref: string too large to index");
            return true;
        }
        long idx = key.int_val;
        long slen = (long)cp_count;
        if (idx < 0) idx += slen;
        if (idx < 0 || idx >= slen) {
            *out = raise_error(interp, "ref: string index out of bounds");
            return true;
        }
        usz byte_off = utf8_byte_offset(bytes, (usz)idx);
        usz cplen = utf8_codepoint_len(bytes[byte_off]);
        if (byte_off + cplen > bytes.len) cplen = bytes.len - byte_off;
        *out = make_string(interp, bytes[byte_off:cplen]);
        return true;
    }

    return false;
}

fn Value* prim_ref(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "ref: expected collection and key");
    Value* result = null;
    if (ref_try_lookup_collection(interp, args[0], args[1], &result)) return result;
    return ref_type_error(interp);
}

fn Value* prim_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "push!: expected 2 arguments");
    if (!is_array(args[0])) return raise_error(interp, "push!: expected array");
    return prim_array_push(args, env, interp);
}

fn Value* prim_keys(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "keys: expected 1 argument");
    if (args[0].tag != HASHMAP) return raise_error(interp, "keys: expected dict");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "keys: dict backing storage is missing");
    return hashmap_keys_list_canonical(args[0].hashmap_val, interp);
}

fn Value* prim_values(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "values: expected 1 argument");
    if (args[0].tag != HASHMAP) return raise_error(interp, "values: expected dict");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "values: dict backing storage is missing");
    return hashmap_values_list_canonical(args[0].hashmap_val, interp);
}

fn Value* prim_has(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "has?: expected 2 arguments");
    if (args[0].tag != HASHMAP) return raise_error(interp, "has?: expected dict");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "has?: dict backing storage is missing");
    Value* found = hashmap_get(args[0].hashmap_val, args[1]);
    return found != null ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "remove!: expected 2 arguments");
    if (args[0].tag != HASHMAP) return raise_error(interp, "remove!: expected dict");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "remove!: dict backing storage is missing");
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

fn bool set_constructor_capacity(usz elem_count, uint* out) {
    if (out == null) return false;
    if (elem_count > usz.max / (usz)2) return false;
    usz required = elem_count * (usz)2;
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

fn Value* prim_set(Value*[] args, Env* env, Interp* interp) {
    uint cap = 0;
    if (!set_constructor_capacity(args.len, &cap)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "Set: out of memory");
    }
    Value* set_val = make_set(interp, cap);
    if (set_val == null || set_val.tag != SET) return set_val;
    HashMap* map = set_val.hashmap_val;
    if (map == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "Set: set backing storage is missing");
    Value* true_val = make_symbol(interp, interp.sym_true);
    for (usz i = 0; i < args.len; i++) {
        if (!hashmap_set_checked(map, args[i], true_val, interp)) {
            return raise_error(interp, "Set: out of memory while growing backing storage");
        }
    }
    return set_val;
}

fn Value* prim_set_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-add: expected set and value");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set-add: set backing storage is missing");
    Value* true_val = make_symbol(interp, interp.sym_true);
    if (!hashmap_set(args[0].hashmap_val, args[1], true_val, interp)) {
        return raise_error_with_payload_names(
            interp,
            "runtime",
            "runtime/out-of-memory",
            "set-add: set backing storage out of memory");
    }
    return make_void(interp);
}

fn Value* prim_set_remove(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-remove: expected set and value");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set-remove: set backing storage is missing");
    hashmap_remove(args[0].hashmap_val, args[1]);
    return make_void(interp);
}

fn Value* prim_set_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != SET) return raise_error(interp, "set-contains?: expected set and value");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set-contains?: set backing storage is missing");
    Value* v = hashmap_get(args[0].hashmap_val, args[1]);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_set_size(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != SET) return raise_error(interp, "length: expected set");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "length: set backing storage is missing");
    return make_int(interp, (long)args[0].hashmap_val.count);
}

fn Value* prim_set_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != SET) return raise_error(interp, "List: expected set");
    if (args[0].hashmap_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "List: set backing storage is missing");
    return hashmap_keys_list_canonical(args[0].hashmap_val, interp);
}
