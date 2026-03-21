module lisp;

import main;

fn Value* explain_effect_result_from_signal_value(
    Interp* interp,
    SymbolId effect_tag,
    Value* arg_value,
    Value* out,
    Value* decision,
    Value* trace
) {
    TypeId arg_type = infer_value_type(arg_value, interp);
    bool has_stack_ctx = main::g_current_stack_ctx != null;

    explain_dict_set(trace, interp, "effect-tag", make_symbol(interp, effect_tag));
    explain_dict_set(trace, interp, "arg-type", make_symbol(interp, explain_type_symbol(interp, arg_type)));
    explain_dict_set(trace, interp, "handler-count", make_int(interp, (long)interp.handler_count));
    explain_dict_set(trace, interp, "stack-context-active", explain_bool_value(interp, has_stack_ctx));

    Value* rev_candidates = make_nil(interp);
    usz searched_handlers = 0;

    bool matched = false;
    usz matched_handler_index = usz.max;
    usz matched_search_order = usz.max;
    usz matched_clause_index = usz.max;
    Expr* matched_clause_body = null;
    SymbolId matched_clause_arg = 0;
    SymbolId matched_clause_k = 0;

    bool strict_stop = false;
    usz strict_handler_index = usz.max;
    usz search_order = 0;

    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* handler = &interp.handler_stack[(usz)i];
        isz clause_idx = explain_effect_find_clause_index(handler, effect_tag);
        bool clause_match = clause_idx >= 0;
        usz clause_index = clause_match ? (usz)clause_idx : usz.max;
        bool dispatchable = clause_match && has_stack_ctx && handler.effect_state != null;

        Value* candidate = make_hashmap(interp, 20);
        explain_dict_set(candidate, interp, "index", make_int(interp, (long)search_order));
        explain_dict_set(candidate, interp, "handler-index", make_int(interp, (long)i));
        explain_dict_set(candidate, interp, "strict", explain_bool_value(interp, handler.strict_mode));
        explain_dict_set(candidate, interp, "clause-count", make_int(interp, (long)handler.clause_count));
        explain_dict_set(candidate, interp, "tags", explain_effect_handler_tags(interp, handler));
        explain_dict_set(candidate, interp, "matched", explain_bool_value(interp, clause_match));
        explain_dict_set(candidate, interp, "dispatchable", explain_bool_value(interp, dispatchable));
        explain_dict_set_symbol(candidate, interp, "miss-reason",
            explain_effect_miss_reason_symbol(interp, dispatchable, clause_match, handler.strict_mode));
        explain_dict_set(candidate, interp, "debug_message", make_nil(interp));
        if (clause_match) {
            Expr* clause_body = explain_effect_clause_body(handler, clause_index);
            explain_dict_set(candidate, interp, "match-clause-index", make_int(interp, (long)clause_index));
            explain_dict_set(candidate, interp, "match-tag", make_symbol(interp, explain_effect_clause_tag(handler, clause_index)));
            explain_dict_set(candidate, interp, "source", explain_effect_clause_source(interp, clause_body));
            explain_dict_set_symbol(candidate, interp, "outcome",
                explain_effect_handler_outcome_symbol(clause_body, interp));

            SymbolId arg_name = explain_effect_clause_arg_name(handler, clause_index);
            SymbolId k_name = explain_effect_clause_k_name(handler, clause_index, interp);
            if ((uint)arg_name != 0) {
                explain_dict_set(candidate, interp, "arg-name", make_symbol(interp, arg_name));
            } else {
                explain_dict_set(candidate, interp, "arg-name", make_nil(interp));
            }
            if ((uint)k_name != 0) {
                explain_dict_set(candidate, interp, "continuation-name", make_symbol(interp, k_name));
            } else {
                explain_dict_set(candidate, interp, "continuation-name", make_nil(interp));
            }
        } else {
            explain_dict_set(candidate, interp, "match-clause-index", make_nil(interp));
            explain_dict_set(candidate, interp, "match-tag", make_nil(interp));
            explain_dict_set(candidate, interp, "source", make_nil(interp));
            explain_dict_set(candidate, interp, "outcome", make_nil(interp));
            explain_dict_set(candidate, interp, "arg-name", make_nil(interp));
            explain_dict_set(candidate, interp, "continuation-name", make_nil(interp));
        }

        rev_candidates = make_cons(interp, candidate, rev_candidates);
        searched_handlers++;

        if (dispatchable) {
            matched = true;
            matched_handler_index = (usz)i;
            matched_search_order = search_order;
            matched_clause_index = clause_index;
            matched_clause_body = explain_effect_clause_body(handler, clause_index);
            matched_clause_arg = explain_effect_clause_arg_name(handler, clause_index);
            matched_clause_k = explain_effect_clause_k_name(handler, clause_index, interp);
            break;
        }

        if (handler.strict_mode) {
            strict_stop = true;
            strict_handler_index = (usz)i;
            break;
        }

        search_order++;
    }

    Value* candidates = explain_reverse_list(rev_candidates, interp);
    explain_dict_set(out, interp, "candidates", candidates);
    explain_dict_set(trace, interp, "searched-handlers", make_int(interp, (long)searched_handlers));

    if (matched) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("handler-match"));
        explain_dict_set(decision, interp, "handler-index", make_int(interp, (long)matched_handler_index));
        explain_dict_set(decision, interp, "search-order", make_int(interp, (long)matched_search_order));
        explain_dict_set(decision, interp, "clause-index", make_int(interp, (long)matched_clause_index));
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome",
            explain_effect_handler_outcome_symbol(matched_clause_body, interp));
        if ((uint)matched_clause_arg != 0) {
            explain_dict_set(decision, interp, "arg-name", make_symbol(interp, matched_clause_arg));
        } else {
            explain_dict_set(decision, interp, "arg-name", make_nil(interp));
        }
        if ((uint)matched_clause_k != 0) {
            explain_dict_set(decision, interp, "continuation-name", make_symbol(interp, matched_clause_k));
        } else {
            explain_dict_set(decision, interp, "continuation-name", make_nil(interp));
        }
        return out;
    }

    if (strict_stop) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("strict-unhandled-effect"));
        explain_dict_set(decision, interp, "strict-handler-index", make_int(interp, (long)strict_handler_index));
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    usz fast_path_index = usz.max;
    Value* fast_path_prim = explain_effect_fast_path_primitive(interp, effect_tag, &fast_path_index);
    if (fast_path_prim != null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("fast-path"));
        explain_dict_set(decision, interp, "fast-path-index", make_int(interp, (long)fast_path_index));
        if (fast_path_prim.tag == PRIMITIVE) {
            explain_dict_set(decision, interp, "fast-path-primitive", explain_primitive_name_value(interp, fast_path_prim.prim_val));
        } else {
            explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp));
        }
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("unhandled-effect"));
    explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
    explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
    return out;
}

fn Value* explain_effect_result_from_signal(Interp* interp, Expr* target, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* arg_value = runtime_eval_expr(target.perform.arg, target_env, interp);
    if (arg_value != null && arg_value.tag == ERROR) return arg_value;
    return explain_effect_result_from_signal_value(interp, target.perform.tag, arg_value, out, decision, trace);
}

fn Value* explain_effect_result_from_resolve_value(
    Interp* interp,
    Value* k_val,
    Value* out,
    Value* decision,
    Value* trace
) {
    explain_dict_set(trace, interp, "has-continuation-binding", explain_bool_value(interp, k_val != null));

    if (k_val == null || k_val.tag != CONTINUATION) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-invalid-continuation"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    Continuation* k = k_val.cont_val;
    bool effect_cont = k.is_coroutine_based && k.handle_state != null;
    explain_dict_set(trace, interp, "effect-continuation", explain_bool_value(interp, effect_cont));

    if (effect_cont) {
        if (k.ctx == null || k.ctx.status == main::StackCtxStatus.CTX_DEAD || k.ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
            explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
            explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-dead-continuation"));
            explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
            return out;
        }
        if (k.ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
            explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
            explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-continuation-not-suspended"));
            explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
            return out;
        }
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-effect-continuation"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve);
        return out;
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-generic-continuation"));
    explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve);
    return out;
}

fn Value* explain_effect_result_from_resolve(Interp* interp, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* k_val = explain_effect_continuation_lookup(target_env, interp);
    return explain_effect_result_from_resolve_value(interp, k_val, out, decision, trace);
}

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
