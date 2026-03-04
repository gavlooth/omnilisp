module lisp;

fn EvalResult apply_primitive(Value* prim, Value* arg, Interp* interp) {
    int arity = prim.prim_val.arity;

    // For unary or variadic primitives, apply immediately
    if (arity <= 1 || arity == -1) {
        Value*[8] args;
        args[0] = arg;
        interp.constructor_type_id = prim.prim_val.tag;
        interp.prim_user_data = prim.prim_val.user_data;
        Value* result = prim.prim_val.func(args[:1], null, interp);
        if (is_error(result)) {
            return eval_error(result.str_chars[:result.str_len]);
        }
        return eval_ok(result);
    }

    // For multi-arity primitives, create a partial application
    Value* partial = interp.alloc_value();
    partial.tag = PARTIAL_PRIM;
    partial.partial_val.func = prim.prim_val.func;
    partial.partial_val.first_arg = arg;
    partial.partial_val.second_arg = null;
    partial.partial_val.remaining = arity - 1;

    return eval_ok(partial);
}

fn EvalResult apply_partial(Value* partial, Value* arg, Interp* interp) {
    if (partial.partial_val.remaining > 1) {
        // Still need more args — chain another partial
        Value* next = interp.alloc_value();
        next.tag = PARTIAL_PRIM;
        next.partial_val.func = partial.partial_val.func;
        next.partial_val.first_arg = partial.partial_val.first_arg;
        next.partial_val.second_arg = arg;
        next.partial_val.remaining = partial.partial_val.remaining - 1;
        return eval_ok(next);
    }

    // All args collected — call the function
    Value*[8] args;
    args[0] = partial.partial_val.first_arg;
    if (partial.partial_val.second_arg != null) {
        args[1] = partial.partial_val.second_arg;
        args[2] = arg;
        Value* result = partial.partial_val.func(args[:3], null, interp);
        if (is_error(result)) { return eval_error(result.str_chars[:result.str_len]); }
        return eval_ok(result);
    }
    args[1] = arg;
    Value* result = partial.partial_val.func(args[:2], null, interp);
    if (is_error(result)) {
        return eval_error(result.str_chars[:result.str_len]);
    }
    return eval_ok(result);
}
