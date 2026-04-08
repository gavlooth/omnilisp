module lisp;

fn Value* explain_effect_result_from_thunk(Interp* interp, Value* thunk_value) {
    if (thunk_value == null || thunk_value.tag != CLOSURE || thunk_value.closure_val == null) {
        return raise_error(interp, "explain effect: expected thunked form");
    }
    Closure* thunk = thunk_value.closure_val;
    if (thunk.param_count != 0 || thunk.has_rest) {
        return raise_error(interp, "explain effect: expected zero-arg thunk");
    }

    Expr* target = thunk.body;
    Env* target_env = thunk.env != null ? thunk.env : interp.global_env;
    Value* input_form = expr_to_value(target, interp);

    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace);
    explain_init_effect_decision_shape(interp, decision);
    explain_init_effect_trace_shape(interp, trace);

    if (target == null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-null"));
        return out;
    }

    if (target.tag == E_PERFORM) {
        Value* result = explain_effect_result_from_signal(interp, target, target_env, out, decision, trace);
        if (result != null && result.tag == ERROR) return result;
        return out;
    }

    if (target.tag == E_RESOLVE) {
        return explain_effect_result_from_resolve(interp, target_env, out, decision, trace);
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-not-signal-or-resolve"));
    return out;
}
