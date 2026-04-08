module lisp;

/**
 * Get the length of a list.
 */
fn usz get_list_length(Value* v) {
    usz len = 0;
    while (is_cons(v)) {
        len++;
        v = cdr(v);
    }
    return len;
}

/**
 * Get the nth element of a list.
 */
fn Value* get_list_nth(Value* v, usz n) {
    for (usz i = 0; i < n; i++) {
        if (!is_cons(v)) return null;
        v = cdr(v);
    }
    if (!is_cons(v)) return null;
    return car(v);
}

/**
 * Get the rest of a sequence starting at index n.
 * Works for both CONS lists and ARRAY values.
 */
fn Value* get_list_rest(Value* v, usz n, Interp* interp) {
    if (v != null && v.tag == ARRAY && v.array_val != null) {
        // ARRAY: build cons list from remaining elements
        Value* result = make_nil(interp);
        usz alen = v.array_val.length;
        for (usz i = alen; i > n; i--) {
            result = make_cons(interp, v.array_val.items[i - 1], result);
        }
        return result;
    }
    // CONS list: walk to position n
    for (usz i = 0; i < n; i++) {
        if (!is_cons(v)) return make_nil(interp);
        v = cdr(v);
    }
    return v;
}
