module lisp;

import main;

const usz ITER_BUFFER_MAX = 4096;

fn Value* iterator_ctor_raise(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, null);
}

fn Value* prim_iterator_map(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return iterator_ctor_raise(interp, "type/arity", "__iterator-map: expected 2 arguments");
    if (args[1] == null || args[1].tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-map: expected iterator state");
    }
    return iterator_map_value(args[0], args[1], interp);
}

fn Value* prim_iterator_map_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_nil(interp);
    Value* func = args[0];
    Value* iterator = args[1];
    Value* pair = iterator_call_next(iterator, interp);
    if (pair == null || pair.tag == NIL || pair.tag == ERROR) return pair;
    if (!is_cons(pair)) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-map: expected iterator pair");
    }

    Value* mapped = jit_apply_value(func, car(pair), interp);
    if (mapped != null && mapped.tag == ERROR) return mapped;
    return iterator_make_pair_or_propagate(mapped, iterator_map_value(func, cdr(pair), interp), interp);
}

fn Value* prim_iterator_filter(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return iterator_ctor_raise(interp, "type/arity", "__iterator-filter: expected 2 arguments");
    if (args[1] == null || args[1].tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-filter: expected iterator state");
    }
    return iterator_filter_value(args[0], args[1], interp);
}

fn Value* prim_iterator_filter_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_nil(interp);
    Value* pred = args[0];
    Value* iterator = args[1];

    while (iterator != null && iterator.tag == ITERATOR) {
        Value* pair = iterator_call_next(iterator, interp);
        if (pair == null || pair.tag == NIL || pair.tag == ERROR) return pair;
        if (!is_cons(pair)) {
            return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-filter: expected iterator pair");
        }

        Value* keep = jit_apply_value(pred, car(pair), interp);
        if (keep != null && keep.tag == ERROR) return keep;
        if (iterator_predicate_truthy(keep)) {
            return iterator_make_pair_or_propagate(car(pair), iterator_filter_value(pred, cdr(pair), interp), interp);
        }
        iterator = cdr(pair);
    }
    return make_nil(interp);
}

fn Value* prim_iterator_take(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return iterator_ctor_raise(interp, "type/arity", "__iterator-take: expected 2 arguments");
    if (args[0] == null || !is_int(args[0])) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-take: expected integer count");
    }
    if (args[1] == null || args[1].tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-take: expected iterator state");
    }
    if (args[0].int_val <= 0) return iterator_from_list_value(make_nil(interp), interp);
    return iterator_take_value(args[0], args[1], interp);
}

fn Value* prim_iterator_take_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_nil(interp);
    Value* n_value = args[0];
    Value* iterator = args[1];
    if (n_value == null || !is_int(n_value) || n_value.int_val <= 0) return make_nil(interp);

    Value* pair = iterator_call_next(iterator, interp);
    if (pair == null || pair.tag == NIL || pair.tag == ERROR) return pair;
    if (!is_cons(pair)) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-take: expected iterator pair");
    }

    return iterator_make_pair_or_propagate(
        car(pair),
        iterator_take_value(make_int(interp, n_value.int_val - 1), cdr(pair), interp),
        interp
    );
}

fn Value* prim_iterator_zip(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return iterator_ctor_raise(interp, "type/arity", "__iterator-zip: expected 2 arguments");
    if (args[0] == null || args[0].tag != ITERATOR || args[1] == null || args[1].tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-zip: expected iterators");
    }
    return iterator_zip_value(args[0], args[1], interp);
}

fn Value* prim_iterator_zip_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_nil(interp);
    Value* left = args[0];
    Value* right = args[1];
    Value* left_pair = iterator_call_next(left, interp);
    if (left_pair == null || left_pair.tag == NIL || left_pair.tag == ERROR) return left_pair;
    Value* right_pair = iterator_call_next(right, interp);
    if (right_pair == null || right_pair.tag == NIL || right_pair.tag == ERROR) return right_pair;
    if (!is_cons(left_pair) || !is_cons(right_pair)) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-zip: expected iterator pairs");
    }

    Value* item_pair = make_cons(interp, car(left_pair), car(right_pair));
    return iterator_make_pair_or_propagate(item_pair, iterator_zip_value(cdr(left_pair), cdr(right_pair), interp), interp);
}

fn Value* prim_iterator_foldl(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return iterator_ctor_raise(interp, "type/arity", "__iterator-foldl: expected 3 arguments");
    if (args[2] == null || args[2].tag != ITERATOR) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-foldl: expected iterator");
    }

    Value* func = args[0];
    Value* acc = args[1];
    Value* current = args[2];

    while (current != null && current.tag == ITERATOR) {
        Value* pair = iterator_call_next(current, interp);
        if (pair == null || pair.tag == NIL) return acc;
        if (pair.tag == ERROR) return pair;
        if (!is_cons(pair)) {
            return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-foldl: expected iterator pair");
        }

        Value* call_args = make_cons(interp, acc, make_cons(interp, car(pair), make_nil(interp)));
        acc = jit_apply_multi_args(interp, func, call_args, 2);
        if (acc != null && acc.tag == ERROR) return acc;
        current = cdr(pair);
    }

    return acc;
}
