module lisp;

const usz DISPATCH_INLINE_ARG_CAPACITY = 8;
const usz DISPATCH_INLINE_BINDING_CAPACITY = 8;

fn Value* check_lambda_call_boundary(Closure* closure, Value*[] args, usz provided_arg_count, Interp* interp) {
    if (closure == null || !closure.has_typed_params || closure.type_sig == null) {
        return null;
    }

    MethodSignature* sig = closure.type_sig;
    if (sig.param_count == 0) return null;

    DispatchMatchBreakdown breakdown = {};
    breakdown.failure_reason = DISPATCH_MATCH_FAIL_NONE;
    breakdown.failure_arg_index = usz.max;

    if (args.len != sig.param_count) {
        breakdown.failure_reason = DISPATCH_MATCH_FAIL_ARITY;
        return lambda_call_type_error(closure, sig, args, provided_arg_count, {}, &breakdown, interp);
    }

    TypeId[] arg_types = {};
    TypeId[DISPATCH_INLINE_ARG_CAPACITY] arg_types_inline;
    TypeId* arg_types_storage = null;
    if (args.len > 0) {
        if (args.len <= DISPATCH_INLINE_ARG_CAPACITY) {
            arg_types = arg_types_inline[:args.len];
        } else {
            if (args.len > usz.max / TypeId.sizeof) {
                return raise_error(interp, "lambda type check: out of memory");
            }
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return raise_error(interp, "lambda type check: out of memory");
            }
            arg_types = arg_types_storage[:args.len];
        }
        for (usz i = 0; i < args.len; i++) {
            arg_types[i] = infer_value_type(args[i], interp);
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    MethodEntry entry = {};
    entry.sig = *sig;
    breakdown = dispatch_match_breakdown(&entry, args, arg_types, interp);
    if (breakdown.matched) return null;

    return lambda_call_type_error(closure, sig, args, provided_arg_count, arg_types, &breakdown, interp);
}


fn Value* find_best_method(MethodTable* mt, Value*[] args, Interp* interp) {
    long best_score = -1;
    Value* best_method = null;
    usz best_count = 0;
    Value* best_match_indices_rev = make_nil(interp);

    TypeId[] arg_types = {};
    TypeId[DISPATCH_INLINE_ARG_CAPACITY] arg_types_inline;
    TypeId* arg_types_storage = null;
    if (args.len > 0) {
        if (args.len <= DISPATCH_INLINE_ARG_CAPACITY) {
            arg_types = arg_types_inline[:args.len];
        } else {
            if (args.len > usz.max / TypeId.sizeof) {
                return raise_error(interp, "method dispatch: out of memory");
            }
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return raise_error(interp, "method dispatch: out of memory");
            }
            arg_types = arg_types_storage[:args.len];
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    for (usz a = 0; a < args.len; a++) {
        arg_types[a] = infer_value_type(args[a], interp);
    }

    for (usz i = 0; i < mt.entry_count; i++) {
        MethodEntry* entry = &mt.entries[i];
        long score = 0;
        if (!method_match_score(entry, args, arg_types, interp, &score)) continue;

        if (score > best_score) {
            best_score = score;
            best_method = entry.implementation;
            best_count = 1;
            best_match_indices_rev = make_cons(interp, make_int(interp, (long)i), make_nil(interp));
        } else if (score == best_score) {
            best_count++;
            best_match_indices_rev = make_cons(interp, make_int(interp, (long)i), best_match_indices_rev);
        }
    }

    if (best_count > 1) {
        return dispatch_ambiguous_error(mt, args, best_score, best_count, best_match_indices_rev, interp);
    }

    return best_method;
}
