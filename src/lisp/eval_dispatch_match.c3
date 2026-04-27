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
                return runtime_raise(interp, "runtime/out-of-memory", "lambda type check: out of memory");
            }
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return runtime_raise(interp, "runtime/out-of-memory", "lambda type check: out of memory");
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
    if (mt == null) {
        return runtime_raise(interp, "runtime/evaluation-error", "invalid method table state");
    }

    long best_score = -1;
    Value* best_method = null;
    usz best_count = 0;

    TypeId[] arg_types = {};
    TypeId[DISPATCH_INLINE_ARG_CAPACITY] arg_types_inline;
    TypeId* arg_types_storage = null;
    if (args.len > 0) {
        if (args.len <= DISPATCH_INLINE_ARG_CAPACITY) {
            arg_types = arg_types_inline[:args.len];
        } else {
            if (args.len > usz.max / TypeId.sizeof) {
                return runtime_raise(interp, "runtime/out-of-memory", "method dispatch: out of memory");
            }
            arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * args.len);
            if (arg_types_storage == null) {
                return runtime_raise(interp, "runtime/out-of-memory", "method dispatch: out of memory");
            }
            arg_types = arg_types_storage[:args.len];
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    usz[] best_match_indices = {};
    usz[DISPATCH_INLINE_BINDING_CAPACITY] best_match_indices_inline;
    usz* best_match_indices_storage = null;
    if (mt.entry_count > 0) {
        if (mt.entry_count <= DISPATCH_INLINE_BINDING_CAPACITY) {
            best_match_indices = best_match_indices_inline[:mt.entry_count];
        } else {
            if (mt.entry_count > usz.max / usz.sizeof) {
                return runtime_raise(interp, "runtime/out-of-memory", "method dispatch: out of memory");
            }
            best_match_indices_storage = (usz*)mem::malloc(usz.sizeof * mt.entry_count);
            if (best_match_indices_storage == null) {
                return runtime_raise(interp, "runtime/out-of-memory", "method dispatch: out of memory");
            }
            best_match_indices = best_match_indices_storage[:mt.entry_count];
        }
    }
    defer if (best_match_indices_storage != null) mem::free(best_match_indices_storage);

    for (usz a = 0; a < args.len; a++) {
        arg_types[a] = infer_value_type(args[a], interp);
    }

    for (usz i = 0; i < mt.entry_count; i++) {
        MethodEntry* entry = &mt.entries[i];
        DispatchMatchBreakdown breakdown = dispatch_match_breakdown(entry, args, arg_types, interp);
        if (breakdown.failure_reason == DISPATCH_MATCH_FAIL_INVALID_LITERAL) {
            return runtime_raise(interp, "runtime/invalid-state", "method dispatch: invalid literal signature");
        }
        if (!breakdown.matched) continue;

        long score = breakdown.score_total;

        if (score > best_score) {
            best_score = score;
            best_method = entry.implementation;
            best_count = 1;
            best_match_indices[0] = i;
        } else if (score == best_score) {
            if (best_count >= best_match_indices.len) {
                return runtime_raise(interp, "runtime/out-of-memory", "method dispatch: out of memory");
            }
            best_match_indices[best_count] = i;
            best_count++;
        }
    }

    if (best_count > 1) {
        Value* best_match_indices_rev = make_nil(interp);
        for (usz i = 0; i < best_count; i++) {
            Value* idx = make_int(interp, (long)best_match_indices[i]);
            if (idx == null || idx.tag == ERROR) return idx;
            best_match_indices_rev = make_cons_or_error(
                interp,
                idx,
                best_match_indices_rev,
                "method dispatch: out of memory"
            );
            if (best_match_indices_rev == null || best_match_indices_rev.tag == ERROR) return best_match_indices_rev;
        }
        return dispatch_ambiguous_error(mt, args, best_score, best_count, best_match_indices_rev, interp);
    }

    return best_method;
}
