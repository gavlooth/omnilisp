module lisp;

fn Value* explain_effect_result_from_compiled_signal(
    Interp* interp,
    char[] source,
    SymbolId effect_tag,
    Value* arg_value
) {
    Value* input_form = explain_input_form_from_source(interp, source);
    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace);
    explain_init_effect_decision_shape(interp, decision);
    explain_init_effect_trace_shape(interp, trace);
    Value* result = explain_effect_result_from_signal_value(interp, effect_tag, arg_value, out, decision, trace);
    if (result != null && result.tag == ERROR) return result;
    return out;
}

fn Value* explain_effect_result_from_compiled_resolve(Interp* interp, char[] source, Value* continuation) {
    Value* input_form = explain_input_form_from_source(interp, source);
    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace);
    explain_init_effect_decision_shape(interp, decision);
    explain_init_effect_trace_shape(interp, trace);
    return explain_effect_result_from_resolve_value(interp, continuation, out, decision, trace);
}
