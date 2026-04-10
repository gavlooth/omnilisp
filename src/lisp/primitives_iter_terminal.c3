module lisp;

import main;

// Iterator terminal and collection-conversion primitives.
fn Value* iterator_promote_item_or_error(Value* value, Interp* interp) @inline {
    Value* promoted = boundary_promote_to_root(value, interp);
    if (promoted != null && promoted.tag == ERROR) return promoted;
    return promoted;
}

fn Value* prim_iterator_p(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    if (args[0].tag == ITERATOR) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

fn Value* prim_make_iterator(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__make-iterator: expected 1 argument");
    if (!is_closure(args[0])) return iterator_ctor_raise(interp, "type/arg-mismatch", "__make-iterator: expected closure thunk");
    return make_iterator(interp, args[0]);
}

fn Value* prim_ctor_iterator(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "Iterator: expected 1 argument");
    if (args[0].tag == ITERATOR) return args[0];
    if (is_closure(args[0])) return make_iterator(interp, args[0]);
    if (is_cons(args[0]) || is_nil(args[0])) return iterator_from_list_value(args[0], interp);
    if (is_array(args[0])) return iterator_from_array_value(args[0], 0, interp);
    if (args[0].tag == HASHMAP) return iterator_from_dict_value(args[0], interp);
    return iterator_ctor_raise(interp, "type/arg-mismatch", "Iterator: expected closure thunk, iterator, or collection");
}

fn Value* prim_next(Value*[] args, Env* env, Interp* interp) {
    Value* iterator = null;
    Value* parse_err = iterator_require_arg(args, "next: expected iterator", &iterator, interp);
    if (parse_err != null) return parse_err;
    return jit_apply_value(iterator.iterator_val, make_nil(interp), interp);
}

fn Value* iterator_require_arg(Value*[] args, char[] expected_msg, Value** iterator_out, Interp* interp) {
    if (args.len < 1 || args[0].tag != ITERATOR) {
        return raise_error(interp, expected_msg);
    }
    *iterator_out = args[0];
    return null;
}

fn Value* iterator_consume_items(Value* iterator, Value** items, usz* count_out, Interp* interp) {
    usz count = 0;
    Value* it = iterator;

    while (count < ITER_BUFFER_MAX) {
        Value* pair = jit_apply_value(it.iterator_val, make_nil(interp), interp);
        if (pair == null || pair.tag == NIL) break;
        if (pair.tag == ERROR) return pair;
        if (!is_cons(pair)) {
            return iterator_ctor_raise(interp, "type/arg-mismatch", "iterator: expected iterator pair");
        }

        items[count++] = car(pair);
        Value* rest = null;
        Value* tail_err = iterator_tail_or_error(pair, "iterator: expected iterator tail", &rest, interp);
        if (tail_err != null) return tail_err;
        it = rest;
    }

    *count_out = count;
    return null;
}

fn Value* prim_collect(Value*[] args, Env* env, Interp* interp) {
    Value* iterator = null;
    Value* parse_err = iterator_require_arg(args, "collect: expected iterator", &iterator, interp);
    if (parse_err != null) return parse_err;

    Value*[ITER_BUFFER_MAX] items;
    usz count = 0;
    Value* consume_err = iterator_consume_items(iterator, &items[0], &count, interp);
    if (consume_err != null) return consume_err;

    Value* result = make_nil(interp);
    for (isz i = (isz)count - 1; i >= 0; i--) {
        Value* next = make_cons(interp, items[(usz)i], result);
        if (next == null || next.tag == ERROR) return next;
        result = next;
    }
    return result;
}

fn Value* prim_to_array(Value*[] args, Env* env, Interp* interp) {
    Value* iterator = null;
    Value* parse_err = iterator_require_arg(args, "to-array: expected iterator", &iterator, interp);
    if (parse_err != null) return parse_err;

    Value*[ITER_BUFFER_MAX] items;
    usz count = 0;
    Value* consume_err = iterator_consume_items(iterator, &items[0], &count, interp);
    if (consume_err != null) return consume_err;

    Value* arr = make_array_checked(interp, count, "to-array: failed to allocate result array");
    if (arr == null || arr.tag != ARRAY) return arr;
    for (usz i = 0; i < count; i++) {
        Value* promoted = iterator_promote_item_or_error(items[i], interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        arr.array_val.items[i] = promoted;
    }
    arr.array_val.length = count;
    return arr;
}
