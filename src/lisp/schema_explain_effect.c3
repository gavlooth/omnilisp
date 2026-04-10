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

    Value* out = explain_make_dict(interp, 16);
    if (out == null || out.tag != HASHMAP) return out;
    Value* decision = explain_make_dict(interp, 16);
    if (decision == null || decision.tag != HASHMAP) return decision;
    Value* trace = explain_make_dict(interp, 16);
    if (trace == null || trace.tag != HASHMAP) return trace;
    if (Value* err = explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace)) return err;
    if (Value* err = explain_init_effect_decision_shape(interp, decision)) return err;
    if (Value* err = explain_init_effect_trace_shape(interp, trace)) return err;

    if (target == null) {
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-null"))) return err;
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

    if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"))) return err;
    if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-not-signal-or-resolve"))) return err;
    return out;
}
