module lisp;

import std::core::mem;

fn bool explain_eval_call_target(
    Expr* target,
    Env* target_env,
    Interp* interp,
    Value** callee_out,
    Value*[]* args_out,
    Value*** args_storage_out
) {
    *callee_out = runtime_eval_expr(target.call.func, target_env, interp);
    if (*callee_out != null && (*callee_out).tag == ERROR) return false;

    usz arg_count = target.call.arg_count;
    *args_out = {};
    *args_storage_out = null;
    if (arg_count > 0) {
        Value** storage = (Value**)mem::malloc(Value*.sizeof * arg_count);
        if (storage == null) {
            *callee_out = raise_error(interp, "explain dispatch: out of memory");
            return false;
        }
        *args_storage_out = storage;
        *args_out = storage[:arg_count];
        for (usz i = 0; i < arg_count; i++) {
            (*args_out)[i] = runtime_eval_expr(target.call.args[i], target_env, interp);
            if ((*args_out)[i] != null && (*args_out)[i].tag == ERROR) {
                *callee_out = (*args_out)[i];
                return false;
            }
        }
    }

    return true;
}

fn Value* explain_input_form_from_source(Interp* interp, char[] source) {
    Expr* parsed = parse(source, interp);
    if (parsed == null) return make_nil(interp);
    return expr_to_value(parsed, interp);
}

fn Value* explain_dispatch_result_from_evaluated_call(
    Interp* interp,
    Value* input_form,
    Value* callee,
    Value*[] call_args
) {
    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.symbols.intern("dispatch"), input_form, decision, trace);
    explain_init_dispatch_decision_shape(interp, decision);
    explain_init_dispatch_trace_shape(interp, trace);

    TypeId[] arg_types = {};
    TypeId* arg_types_storage = null;
    if (call_args.len > 0) {
        arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * call_args.len);
        if (arg_types_storage == null) {
            return raise_error(interp, "explain dispatch: out of memory");
        }
        arg_types = arg_types_storage[:call_args.len];
        for (usz i = 0; i < call_args.len; i++) {
            arg_types[i] = infer_value_type(call_args[i], interp);
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    explain_dict_set(trace, interp, "arg-types", explain_arg_types(interp, arg_types));
    explain_dict_set(trace, interp, "arg-count", make_int(interp, (long)call_args.len));
    explain_dict_set(trace, interp, "target-type", make_symbol(interp, value_type_name(callee, interp)));

    if (callee == null || callee.tag != METHOD_TABLE || callee.method_table_val == null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("dispatch-target-not-method-table"));
        return out;
    }

    MethodTable* mt = callee.method_table_val;
    explain_dict_set(trace, interp, "method-name", make_symbol(interp, mt.name));
    explain_dict_set(trace, interp, "method-count", make_int(interp, (long)mt.entry_count));

    long best_score = -1;
    isz winner_index = -1;
    usz tie_count = 0;
    Value* rev_candidates = make_nil(interp);

    for (usz i = 0; i < mt.entry_count; i++) {
        MethodEntry* entry = &mt.entries[i];
        DispatchMatchBreakdown breakdown = dispatch_match_breakdown(entry, call_args, arg_types, interp);

        Value* candidate = make_hashmap(interp, 24);
        explain_dict_set(candidate, interp, "index", make_int(interp, (long)i));
        explain_dict_set(candidate, interp, "name", make_symbol(interp, mt.name));
        explain_dict_set(candidate, interp, "signature", explain_signature_params(interp, &entry.sig));
        explain_dict_set(candidate, interp, "constraints", explain_constraints(interp, &entry.sig));
        explain_dict_set(candidate, interp, "source", explain_method_source(interp, entry.implementation));
        explain_dict_set(candidate, interp, "applicable", explain_bool_value(interp, breakdown.matched));
        explain_dict_set(candidate, interp, "score-components", explain_score_components(interp, &breakdown));
        explain_dict_set_symbol(candidate, interp, "failure", explain_dispatch_failure_symbol(interp, breakdown.failure_reason));
        explain_dict_set(candidate, interp, "debug_message", make_nil(interp));
        if (breakdown.failure_arg_index != usz.max) {
            explain_dict_set(candidate, interp, "failure-arg-index", make_int(interp, (long)breakdown.failure_arg_index));
        } else {
            explain_dict_set(candidate, interp, "failure-arg-index", make_nil(interp));
        }

        if (breakdown.matched) {
            if (breakdown.score_total > best_score) {
                best_score = breakdown.score_total;
                winner_index = (isz)i;
                tie_count = 1;
            } else if (breakdown.score_total == best_score) {
                tie_count++;
            }
        }

        rev_candidates = make_cons(interp, candidate, rev_candidates);
    }

    Value* candidates = explain_reverse_list(rev_candidates, interp);
    explain_dict_set(out, interp, "candidates", candidates);

    if (winner_index >= 0 && tie_count == 1) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("method-match"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("match"));
        explain_dict_set(decision, interp, "winner-index", make_int(interp, (long)winner_index));
        explain_dict_set(decision, interp, "best-score", make_int(interp, best_score));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, (long)tie_count));
    } else if (winner_index >= 0 && tie_count > 1) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ambiguous"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("ambiguous-equal-specificity"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_int(interp, best_score));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, (long)tie_count));
    } else if (mt.fallback != null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("fallback"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("fallback"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_nil(interp));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, 0));
        explain_dict_set(decision, interp, "fallback-source", explain_method_source(interp, mt.fallback));
    } else {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("no-match"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("no-matching-method"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_nil(interp));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, 0));
    }

    return out;
}

fn Value* explain_dispatch_result_from_compiled_call(
    Interp* interp,
    char[] source,
    Value* callee,
    Value*[] call_args
) {
    Value* input_form = explain_input_form_from_source(interp, source);
    return explain_dispatch_result_from_evaluated_call(interp, input_form, callee, call_args);
}

fn Value* explain_dispatch_result_from_thunk(Interp* interp, Value* thunk_value) {
    if (thunk_value == null || thunk_value.tag != CLOSURE || thunk_value.closure_val == null) {
        return raise_error(interp, "explain dispatch: expected thunked form");
    }
    Closure* thunk = thunk_value.closure_val;
    if (thunk.param_count != 0 || thunk.has_rest) {
        return raise_error(interp, "explain dispatch: expected zero-arg thunk");
    }

    Expr* target = thunk.body;
    Env* target_env = thunk.env != null ? thunk.env : interp.global_env;
    Value* input_form = expr_to_value(target, interp);

    if (target == null || target.tag != E_CALL) {
        Value* out = make_hashmap(interp, 16);
        Value* decision = make_hashmap(interp, 16);
        Value* trace = make_hashmap(interp, 16);
        explain_init_common_output(interp, out, interp.symbols.intern("dispatch"), input_form, decision, trace);
        explain_init_dispatch_decision_shape(interp, decision);
        explain_init_dispatch_trace_shape(interp, trace);
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("dispatch-target-not-call"));
        return out;
    }

    Value* callee = null;
    Value*[] call_args = {};
    Value** call_args_storage = null;
    bool evaluated = explain_eval_call_target(target, target_env, interp, &callee, &call_args, &call_args_storage);
    defer if (call_args_storage != null) mem::free(call_args_storage);
    if (!evaluated) return callee;

    return explain_dispatch_result_from_evaluated_call(interp, input_form, callee, call_args);
}
