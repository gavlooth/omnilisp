module lisp;

import std::io;
import std::core::mem;
import main;

// Collection primitives: sorting + array operations.
bool g_array_push_force_grow_alloc_fail = false;

fn Value* array_promote_or_error(Value* value, Interp* interp) @inline {
    Value* promoted = boundary_promote_to_root(value, interp);
    if (promoted != null && promoted.tag == ERROR) return promoted;
    return promoted;
}

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
        Value* next = make_cons(interp, arr[(usz)i], result);
        if (next == null || next.tag == ERROR) return next;
        result = next;
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
            if (r1 == null) return raise_error(interp, "sort-by: comparator application failed");
            if (r1.tag == ERROR) return r1;
            Value* r2 = jit_apply_value(r1, key, interp);
            if (r2 == null) return raise_error(interp, "sort-by: comparator application failed");
            if (r2.tag == ERROR) return r2;
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
        Value* next = make_cons(interp, arr[(usz)i], result);
        if (next == null || next.tag == ERROR) return next;
        result = next;
    }
    return result;
}


// --- Phase 8: Array primitives ---

fn Value* prim_array(Value*[] args, Env* env, Interp* interp) {
    // Single iterator arg -> consume to array
    if (args.len == 1 && args[0].tag == ITERATOR) {
        Value*[1] one = { args[0] };
        return prim_to_array(one[0:1], env, interp);
    }

    // Single list/nil arg → convert to array
    if (args.len == 1 && (is_cons(args[0]) || is_nil(args[0]))) {
        usz count = 0;
        Value* cur = args[0];
        while (is_cons(cur) && count < 65536) { count++; cur = cur.cons_val.cdr; }
        Value* v = make_array(interp, count);
        if (v == null || v.tag != ARRAY || v.array_val == null) return v;
        cur = args[0];
        for (usz i = 0; i < count; i++) {
            Value* promoted = array_promote_or_error(cur.cons_val.car, interp);
            if (promoted == null || promoted.tag == ERROR) return promoted;
            v.array_val.items[i] = promoted;
            cur = cur.cons_val.cdr;
        }
        v.array_val.length = count;
        return v;
    }

    Value* v = make_array(interp, args.len < 4 ? 4 : args.len);
    if (v == null || v.tag != ARRAY || v.array_val == null) return v;
    for (usz i = 0; i < args.len; i++) {
        Value* promoted = array_promote_or_error(args[i], interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        v.array_val.items[i] = promoted;
    }
    v.array_val.length = args.len;
    return v;
}

fn Value* prim_array_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || !is_array(args[0]) || !is_int(args[1])) return raise_error(interp, "set!: expected array target, integer index, value");
    if (args[0].array_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set!: array backing storage is missing");
    long idx = args[1].int_val;
    long alen = (long)args[0].array_val.length;
    if (idx < 0) idx += alen;
    if (idx < 0 || idx >= alen) return raise_error(interp, "set!: array index out of range");
    Value* promoted = array_promote_or_error(args[2], interp);
    if (promoted == null || promoted.tag == ERROR) return promoted;
    args[0].array_val.items[(usz)idx] = promoted;
    return make_void(interp);
}

fn Value* prim_array_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return raise_error(interp, "array-length: expected array");
    if (args[0].array_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "array-length: array backing storage is missing");
    return make_int(interp, (long)args[0].array_val.length);
}

fn Value* prim_array_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return raise_error(interp, "array->list: expected array");
    Array* vec = args[0].array_val;
    if (vec == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "array->list: array backing storage is missing");
    Value* result = make_nil(interp);
    for (isz i = (isz)vec.length - 1; i >= 0; i--) {
        Value* next = make_cons(interp, vec.items[(usz)i], result);
        if (next == null || next.tag == ERROR) return next;
        result = next;
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
    if (v == null || v.tag != ARRAY || v.array_val == null) return v;
    cur = args[0];
    for (usz i = 0; i < count; i++) {
        Value* promoted = array_promote_or_error(cur.cons_val.car, interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        v.array_val.items[i] = promoted;
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
    if (vec == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "push!: array backing storage is missing");
    if (vec.length >= vec.capacity) {
        if (vec.capacity == 0 || vec.capacity > usz.max / (usz)2) {
            return raise_error(interp, "push!: array backing storage out of memory");
        }
        usz new_cap = vec.capacity * 2;
        if (new_cap > usz.max / Value*.sizeof) {
            return raise_error(interp, "push!: array backing storage out of memory");
        }
        Value** new_items = g_array_push_force_grow_alloc_fail
            ? null
            : (Value**)mem::malloc(Value*.sizeof * new_cap);
        if (new_items == null) {
            return raise_error(interp, "push!: array backing storage out of memory");
        }
        for (usz i = 0; i < vec.length; i++) { new_items[i] = vec.items[i]; }
        mem::free(vec.items);
        vec.items = new_items;
        vec.capacity = new_cap;
    }
    Value* promoted = array_promote_or_error(args[1], interp);
    if (promoted == null || promoted.tag == ERROR) return promoted;
    vec.items[vec.length++] = promoted;
    return make_void(interp);
}
