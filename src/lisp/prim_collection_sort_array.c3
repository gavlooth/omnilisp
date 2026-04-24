module lisp;

import std::io;
import std::core::mem;
import main;

// Collection primitives: sorting + array operations.
bool g_array_push_force_grow_alloc_fail = false;

struct CollectionListItems {
    Value** items;
    usz len;
}

fn Value* array_promote_or_error(Value* value, Interp* interp) @inline {
    Value* promoted = boundary_promote_to_root(value, interp);
    if (promoted != null && promoted.tag == ERROR) return promoted;
    return promoted;
}

<*
@require interp != null
@require out != null
*>
fn Value* collect_proper_list_items(
    Value* list,
    Interp* interp,
    char[] improper_message,
    char[] cyclic_message,
    char[] overflow_message,
    char[] alloc_message,
    CollectionListItems* out
) {
    out.items = null;
    out.len = 0;

    Value* slow = list;
    Value* fast = list;
    while (is_cons(fast)) {
        fast = fast.cons_val.cdr;
        if (!is_cons(fast)) break;
        fast = fast.cons_val.cdr;
        slow = slow.cons_val.cdr;
        if (fast == slow) return collection_raise(interp, "type/range-error", cyclic_message);
    }

    usz count = 0;
    Value* cur = list;
    while (is_cons(cur)) {
        if (count == usz.max) return collection_raise(interp, "type/range-error", overflow_message);
        count++;
        cur = cur.cons_val.cdr;
    }
    if (!is_nil(cur)) return collection_raise(interp, "type/arg-mismatch", improper_message);
    if (count == 0) return null;
    if (count > usz.max / Value*.sizeof) return collection_raise(interp, "type/range-error", overflow_message);

    Value** items = (Value**)mem::malloc(Value*.sizeof * count);
    if (items == null) return collection_raise(interp, "runtime/out-of-memory", alloc_message);

    cur = list;
    for (usz i = 0; i < count; i++) {
        items[i] = cur.cons_val.car;
        cur = cur.cons_val.cdr;
    }
    out.items = items;
    out.len = count;
    return null;
}

<*
@require interp != null
*>
fn Value* sort_by_apply_comparator(Value* cmp_fn, Value* left, Value* right, Interp* interp) {
    Value* tail = make_cons(interp, right, make_nil(interp));
    if (tail == null || tail.tag == ERROR) return collection_raise(interp, "runtime/out-of-memory", "sort-by: failed to allocate comparator call args");
    Value* call_args = make_cons(interp, left, tail);
    if (call_args == null || call_args.tag == ERROR) return collection_raise(interp, "runtime/out-of-memory", "sort-by: failed to allocate comparator call args");
    Value* result = jit_apply_multi_args(interp, cmp_fn, call_args, 2);
    if (result == null) return collection_raise(interp, "runtime/evaluation-error", "sort-by: comparator application failed");
    return result;
}

<*
@require interp != null
*>
fn Value* prim_sort(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return collection_raise(interp, "type/arg-mismatch", "sort: expected list argument");
    if (is_nil(args[0])) return make_nil(interp);
    if (!is_cons(args[0])) return collection_raise(interp, "type/arg-mismatch", "sort: expected list argument");
    CollectionListItems list_items;
    Value* collect_error = collect_proper_list_items(
        args[0],
        interp,
        "sort: expected a proper list",
        "sort: list input is cyclic",
        "sort: list input too large",
        "sort: out of memory while staging list",
        &list_items
    );
    if (collect_error != null) return collect_error;
    defer if (list_items.items != null) mem::free(list_items.items);
    Value** arr = list_items.items;
    usz len = list_items.len;
    // Insertion sort (stable)
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        // Compare: numbers by value, strings lexicographic
        while (j >= 0) {
            bool gt = false;
            if (is_numeric_value(arr[(usz)j]) && is_numeric_value(key)) {
                int cmp = 0;
                gt = numeric_values_compare(arr[(usz)j], key, &cmp) && cmp > 0;
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

<*
@require interp != null
*>
fn Value* prim_sort_by(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return collection_raise(interp, "type/arg-mismatch", "sort-by: expected comparator and list");
    Value* cmp_fn = args[0];
    if (is_nil(args[1])) return make_nil(interp);
    if (!is_cons(args[1])) return collection_raise(interp, "type/arg-mismatch", "sort-by: expected list argument");
    CollectionListItems list_items;
    Value* collect_error = collect_proper_list_items(
        args[1],
        interp,
        "sort-by: expected a proper list",
        "sort-by: list input is cyclic",
        "sort-by: list input too large",
        "sort-by: out of memory while staging list",
        &list_items
    );
    if (collect_error != null) return collect_error;
    defer if (list_items.items != null) mem::free(list_items.items);
    Value** arr = list_items.items;
    usz len = list_items.len;
    // Insertion sort with custom comparator
    for (usz i = 1; i < len; i++) {
        Value* key = arr[i];
        isz j = (isz)i - 1;
        while (j >= 0) {
            // Comparator returns positive numeric or truthy when left should move after right.
            Value* r2 = sort_by_apply_comparator(cmp_fn, arr[(usz)j], key, interp);
            if (r2 == null) return collection_raise(interp, "runtime/evaluation-error", "sort-by: comparator application failed");
            if (r2.tag == ERROR) return r2;
            bool gt = false;
            double cmp_value = 0.0;
            if (try_numeric_to_double(r2, &cmp_value)) {
                gt = cmp_value > 0.0;
            } else {
                gt = !is_falsy(r2, interp);
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


// --- Phase 8: Array primitives ---

<*
@require interp != null
*>
fn Value* prim_array(Value*[] args, Env* env, Interp* interp) {
    // Single iterator arg -> consume to array
    if (args.len == 1 && args[0].tag == ITERATOR) {
        Value*[1] one = { args[0] };
        return prim_to_array(one[0:1], env, interp);
    }

    // Single tensor arg -> realize and expose flat row-major values.
    if (args.len == 1 && is_tensor(args[0])) {
        return tensor_to_flat_array_value(args[0], interp);
    }

    // Single list/nil arg → convert to array
    if (args.len == 1 && (is_cons(args[0]) || is_nil(args[0]))) {
        CollectionListItems list_items;
        Value* collect_error = collect_proper_list_items(
            args[0],
            interp,
            "Array: expected a proper list",
            "Array: list input is cyclic",
            "Array: list input too large",
            "Array: out of memory while staging list",
            &list_items
        );
        if (collect_error != null) return collect_error;
        defer if (list_items.items != null) mem::free(list_items.items);

        Value* v = make_array(interp, list_items.len);
        if (v == null || v.tag != ARRAY || v.array_val == null) return v;
        for (usz i = 0; i < list_items.len; i++) {
            Value* promoted = array_promote_or_error(list_items.items[i], interp);
            if (promoted == null || promoted.tag == ERROR) return promoted;
            v.array_val.items[i] = promoted;
        }
        v.array_val.length = list_items.len;
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

<*
@require interp != null
*>
fn Value* prim_array_set(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || !is_array(args[0]) || !is_int(args[1])) return collection_raise(interp, "type/arg-mismatch", "set!: expected array target, integer index, value");
    if (args[0].array_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "set!: array backing storage is missing");
    long idx = args[1].int_val;
    long alen = (long)args[0].array_val.length;
    if (idx < 0) idx += alen;
    if (idx < 0 || idx >= alen) return collection_raise(interp, "type/range-error", "set!: array index out of range");
    Value* promoted = array_promote_or_error(args[2], interp);
    if (promoted == null || promoted.tag == ERROR) return promoted;
    args[0].array_val.items[(usz)idx] = promoted;
    stable_graph_note_mutation_ptr((void*)args[0].array_val);
    return make_void(interp);
}

<*
@require interp != null
*>
fn Value* prim_array_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return collection_raise(interp, "type/arg-mismatch", "array-length: expected array");
    if (args[0].array_val == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "array-length: array backing storage is missing");
    return make_int(interp, (long)args[0].array_val.length);
}

<*
@require interp != null
*>
fn Value* prim_array_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_array(args[0])) return collection_raise(interp, "type/arg-mismatch", "array->list: expected array");
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

<*
@require interp != null
*>
fn Value* prim_list_to_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return collection_raise(interp, "type/arg-mismatch", "list->array: expected list");
    if (!is_cons(args[0]) && !is_nil(args[0])) return collection_raise(interp, "type/arg-mismatch", "list->array: expected list");

    CollectionListItems list_items;
    Value* collect_error = collect_proper_list_items(
        args[0],
        interp,
        "list->array: expected a proper list",
        "list->array: list input is cyclic",
        "list->array: list input too large",
        "list->array: out of memory while staging list",
        &list_items
    );
    if (collect_error != null) return collect_error;
    defer if (list_items.items != null) mem::free(list_items.items);

    Value* v = make_array(interp, list_items.len);
    if (v == null || v.tag != ARRAY || v.array_val == null) return v;
    for (usz i = 0; i < list_items.len; i++) {
        Value* promoted = array_promote_or_error(list_items.items[i], interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        v.array_val.items[i] = promoted;
    }
    v.array_val.length = list_items.len;
    return v;
}

<*
@require interp != null
*>
fn Value* prim_is_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return collection_raise(interp, "type/arity", "array?: expected 1 argument");
    return is_array(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

<*
@require interp != null
*>
fn Value* prim_array_push(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || !is_array(args[0])) return collection_raise(interp, "type/arg-mismatch", "push!: expected array and value");
    Array* vec = args[0].array_val;
    if (vec == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "push!: array backing storage is missing");
    if (vec.length >= vec.capacity) {
        if (vec.capacity == 0 || vec.capacity > usz.max / (usz)2) {
            return collection_raise(interp, "type/range-error", "push!: array backing storage out of memory");
        }
        usz new_cap = vec.capacity * 2;
        if (new_cap > usz.max / Value*.sizeof) {
            return collection_raise(interp, "type/range-error", "push!: array backing storage out of memory");
        }
        Value** new_items = g_array_push_force_grow_alloc_fail
            ? null
            : (Value**)mem::malloc(Value*.sizeof * new_cap);
        if (new_items == null) {
            return collection_raise(interp, "type/range-error", "push!: array backing storage out of memory");
        }
        for (usz i = 0; i < vec.length; i++) { new_items[i] = vec.items[i]; }
        mem::free(vec.items);
        usz old_cap = vec.capacity;
        vec.items = new_items;
        vec.capacity = new_cap;
        boundary_value_shape_note_array_growth(old_cap, new_cap);
    }
    Value* promoted = array_promote_or_error(args[1], interp);
    if (promoted == null || promoted.tag == ERROR) return promoted;
    vec.items[vec.length++] = promoted;
    stable_graph_note_mutation_ptr((void*)vec);
    return make_void(interp);
}
