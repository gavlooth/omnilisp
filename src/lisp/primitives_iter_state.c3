module lisp;

import main;

bool g_iterator_force_partial_alloc_fail = false;

fn Value* iterator_make_pair_or_propagate(Value* item, Value* next, Interp* interp) @inline {
    if (next == null) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "iterator: expected iterator tail");
    }
    if (next.tag == ERROR) return next;
    return make_cons(interp, item, next);
}

fn Value* iterator_tail_or_error(Value* pair, char[] message, Value** tail_out, Interp* interp) @inline {
    Value* tail = cdr(pair);
    if (tail == null || tail.tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", message);
    }
    *tail_out = tail;
    return null;
}

fn Value* iterator_make_thunk1(PrimitiveFn func, Value* first_arg, Interp* interp) {
    Value* partial = g_iterator_force_partial_alloc_fail ? null : interp.alloc_value();
    if (partial == null) {
        return iterator_ctor_raise(interp, "runtime/out-of-memory", "iterator: failed to allocate thunk");
    }
    partial.tag = PARTIAL_PRIM;
    partial.partial_val.func = func;
    partial.partial_val.first_arg = first_arg;
    partial.partial_val.second_arg = null;
    partial.partial_val.remaining = 1;
    return partial;
}

fn Value* iterator_make_thunk2(PrimitiveFn func, Value* first_arg, Value* second_arg, Interp* interp) {
    Value* partial = g_iterator_force_partial_alloc_fail ? null : interp.alloc_value();
    if (partial == null) {
        return iterator_ctor_raise(interp, "runtime/out-of-memory", "iterator: failed to allocate thunk");
    }
    partial.tag = PARTIAL_PRIM;
    partial.partial_val.func = func;
    partial.partial_val.first_arg = first_arg;
    partial.partial_val.second_arg = second_arg;
    partial.partial_val.remaining = 1;
    return partial;
}

fn Value* prim_iterator_from_list_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-list: expected list state");
    }
    Value* current = args[0];
    if (current == null) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-list: expected list state");
    }
    if (current.tag == NIL) return make_nil(interp);
    if (!is_cons(current)) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-list: expected list");
    }
    return iterator_make_pair_or_propagate(car(current), iterator_from_list_value(cdr(current), interp), interp);
}

fn Value* prim_iterator_from_array_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-array: expected array state");
    }
    Value* array_value = args[0];
    Value* index_value = args[1];
    if (array_value == null || !is_array(array_value) || index_value == null || !is_int(index_value)) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-array: expected array state");
    }

    long index = index_value.int_val;
    if (index < 0 || (usz)index >= array_value.array_val.length) return make_nil(interp);

    Value* item = array_value.array_val.items[(usz)index];
    return iterator_make_pair_or_propagate(item, iterator_from_array_value(array_value, index + 1, interp), interp);
}

fn Value* iterator_from_list_value(Value* list_value, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk1(&prim_iterator_from_list_thunk, list_value, interp));
}

fn Value* iterator_from_array_value(Value* array_value, long index, Interp* interp) {
    return make_iterator(
        interp,
        iterator_make_thunk2(
            &prim_iterator_from_array_thunk,
            array_value,
            make_int(interp, index),
            interp
        )
    );
}

fn Value* iterator_from_dict_value(Value* dict_value, Interp* interp) {
    Value*[1] one = { dict_value };
    Value* keys_list = prim_keys(one[0:1], null, interp);
    if (keys_list != null && keys_list.tag == ERROR) return keys_list;
    return iterator_from_list_value(keys_list, interp);
}

fn Value* iterator_map_value(Value* func, Value* iterator, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk2(&prim_iterator_map_thunk, func, iterator, interp));
}

fn Value* iterator_filter_value(Value* pred, Value* iterator, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk2(&prim_iterator_filter_thunk, pred, iterator, interp));
}

fn Value* iterator_take_value(Value* n_value, Value* iterator, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk2(&prim_iterator_take_thunk, n_value, iterator, interp));
}

fn Value* iterator_zip_value(Value* left, Value* right, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk2(&prim_iterator_zip_thunk, left, right, interp));
}

fn Value* iterator_range_from_value(Value* n_value, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk1(&prim_iterator_range_from_thunk, n_value, interp));
}

fn Value* iterator_repeat_value(Value* item, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk1(&prim_iterator_repeat_thunk, item, interp));
}

fn Value* iterator_cycle_state_value(Value* coll, Value* iterator, Interp* interp) {
    return make_iterator(interp, iterator_make_thunk2(&prim_iterator_cycle_thunk, coll, iterator, interp));
}

fn Value* iterator_call_next(Value* iterator, Interp* interp) @inline {
    Value*[1] one = { iterator };
    return prim_next(one[0:1], null, interp);
}

fn bool iterator_predicate_truthy(Value* result) @inline {
    return result != null && result.tag != NIL;
}
