module lisp;

fn Value* explain_effect_result_from_compiled_signal(
    Interp* interp,
    char[] source,
    SymbolId effect_tag,
    Value* arg_value
) {
    Value* input_form = explain_input_form_from_source(interp, source);
    Value* out = explain_make_dict(interp, 16);
    if (out == null || out.tag != HASHMAP) return out;
    Value* decision = explain_make_dict(interp, 16);
    if (decision == null || decision.tag != HASHMAP) return decision;
    Value* trace = explain_make_dict(interp, 16);
    if (trace == null || trace.tag != HASHMAP) return trace;
    if (Value* err = explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace)) return err;
    if (Value* err = explain_init_effect_decision_shape(interp, decision)) return err;
    if (Value* err = explain_init_effect_trace_shape(interp, trace)) return err;
    Value* result = explain_effect_result_from_signal_value(interp, effect_tag, arg_value, out, decision, trace);
    if (result != null && result.tag == ERROR) return result;
    return out;
}

fn Value* explain_effect_result_from_compiled_resolve(Interp* interp, char[] source, Value* continuation) {
    Value* input_form = explain_input_form_from_source(interp, source);
    Value* out = explain_make_dict(interp, 16);
    if (out == null || out.tag != HASHMAP) return out;
    Value* decision = explain_make_dict(interp, 16);
    if (decision == null || decision.tag != HASHMAP) return decision;
    Value* trace = explain_make_dict(interp, 16);
    if (trace == null || trace.tag != HASHMAP) return trace;
    if (Value* err = explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace)) return err;
    if (Value* err = explain_init_effect_decision_shape(interp, decision)) return err;
    if (Value* err = explain_init_effect_trace_shape(interp, trace)) return err;
    return explain_effect_result_from_resolve_value(interp, continuation, out, decision, trace);
}
