module lisp;

import main;

fn Value* prim_iterator_from_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-from-list: expected 1 argument");
    if (!is_cons(args[0]) && !is_nil(args[0])) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-list: expected list");
    }
    return iterator_from_list_value(args[0], interp);
}

fn Value* prim_iterator_from_array(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-from-array: expected 1 argument");
    if (!is_array(args[0])) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-array: expected array");
    }
    return iterator_from_array_value(args[0], 0, interp);
}

fn Value* prim_iterator_from_dict(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-from-dict: expected 1 argument");
    if (args[0] == null || args[0].tag != HASHMAP) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-from-dict: expected dict");
    }
    return iterator_from_dict_value(args[0], interp);
}

fn Value* prim_iterator_range_from(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-range-from: expected 1 argument");
    if (args[0] == null || !is_int(args[0])) {
        return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-range-from: expected integer");
    }
    return iterator_range_from_value(args[0], interp);
}

fn Value* prim_iterator_range_from_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0] == null || !is_int(args[0])) return make_nil(interp);
    Value* current = args[0];
    return iterator_make_pair_or_propagate(current, iterator_range_from_value(make_int(interp, current.int_val + 1), interp), interp);
}

fn Value* prim_iterator_repeat(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-repeat: expected 1 argument");
    return iterator_repeat_value(args[0], interp);
}

fn Value* prim_iterator_repeat_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return make_nil(interp);
    return iterator_make_pair_or_propagate(args[0], iterator_repeat_value(args[0], interp), interp);
}

fn Value* prim_iterator_cycle(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return iterator_ctor_raise(interp, "type/arity", "__iterator-cycle: expected 1 argument");
    Value*[1] one = { args[0] };
    Value* start = prim_ctor_iterator(one[0:1], null, interp);
    if (start != null && start.tag == ERROR) return start;
    return iterator_cycle_state_value(args[0], start, interp);
}

fn Value* prim_iterator_cycle_thunk(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return make_nil(interp);
    Value* coll = args[0];
    Value* current = args[1];

    while (true) {
        Value* pair = iterator_call_next(current, interp);
        if (pair == null || pair.tag == ERROR) return pair;
        if (pair.tag == NIL) {
            Value*[1] one = { coll };
            Value* reset = prim_ctor_iterator(one[0:1], null, interp);
            if (reset != null && reset.tag == ERROR) return reset;
            if (reset == null || reset.tag != ITERATOR) return make_nil(interp);
            Value* reset_pair = iterator_call_next(reset, interp);
            if (reset_pair == null || reset_pair.tag == ERROR || reset_pair.tag == NIL) return reset_pair;
            if (!is_cons(reset_pair)) {
                return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-cycle: expected iterator pair");
            }
            return iterator_make_pair_or_propagate(car(reset_pair), iterator_cycle_state_value(coll, cdr(reset_pair), interp), interp);
        }
        if (!is_cons(pair)) {
            return iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-cycle: expected iterator pair");
        }
        return iterator_make_pair_or_propagate(car(pair), iterator_cycle_state_value(coll, cdr(pair), interp), interp);
    }
}
