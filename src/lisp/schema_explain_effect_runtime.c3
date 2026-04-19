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

    if (Value* err = explain_dict_set(trace, interp, "effect-tag", make_symbol(interp, effect_tag))) return err;
    if (Value* err = explain_dict_set(trace, interp, "arg-type", make_symbol(interp, explain_type_symbol(interp, arg_type)))) return err;
    if (Value* err = explain_dict_set(trace, interp, "handler-count", make_int(interp, (long)interp.handler_count))) return err;
    if (Value* err = explain_dict_set(trace, interp, "stack-context-active", explain_bool_value(interp, has_stack_ctx))) return err;

    Value* rev_candidates = make_nil(interp);
    usz searched_handlers = 0;

    bool matched = false;
    usz matched_handler_index = usz.max;
    usz matched_search_order = usz.max;
    usz matched_clause_index = usz.max;
    Expr* matched_clause_body = null;
    SymbolId matched_clause_arg = (SymbolId)0;
    SymbolId matched_clause_k = (SymbolId)0;

    bool strict_stop = false;
    usz strict_handler_index = usz.max;
    usz search_order = 0;

    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* handler = &interp.handler_stack[(usz)i];
        isz clause_idx = explain_effect_find_clause_index(handler, effect_tag);
        bool clause_match = clause_idx >= 0;
        usz clause_index = clause_match ? (usz)clause_idx : usz.max;
        bool dispatchable = clause_match && has_stack_ctx && handler.effect_state != null;

        Value* candidate = explain_make_dict(interp, 20);
        if (candidate == null || candidate.tag != HASHMAP) return candidate;
        if (Value* err = explain_dict_set(candidate, interp, "index", make_int(interp, (long)search_order))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "handler-index", make_int(interp, (long)i))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "strict", explain_bool_value(interp, handler.strict_mode))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "clause-count", make_int(interp, (long)handler.clause_count))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "tags", explain_effect_handler_tags(interp, handler))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "matched", explain_bool_value(interp, clause_match))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "dispatchable", explain_bool_value(interp, dispatchable))) return err;
        if (Value* err = explain_dict_set_symbol(candidate, interp, "miss-reason",
            explain_effect_miss_reason_symbol(interp, dispatchable, clause_match, handler.strict_mode))) return err;
        if (Value* err = explain_dict_set(candidate, interp, "debug_message", make_nil(interp))) return err;
        if (clause_match) {
            Expr* clause_body = explain_effect_clause_body(handler, clause_index);
            if (Value* err = explain_dict_set(candidate, interp, "match-clause-index", make_int(interp, (long)clause_index))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "match-tag", make_symbol(interp, explain_effect_clause_tag(handler, clause_index)))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "source", explain_effect_clause_source(interp, clause_body))) return err;
            if (Value* err = explain_dict_set_symbol(candidate, interp, "outcome",
                explain_effect_handler_outcome_symbol(clause_body, interp))) return err;

            SymbolId arg_name = explain_effect_clause_arg_name(handler, clause_index);
            SymbolId k_name = explain_effect_clause_k_name(handler, clause_index, interp);
            if ((uint)arg_name != 0) {
                if (Value* err = explain_dict_set(candidate, interp, "arg-name", make_symbol(interp, arg_name))) return err;
            } else {
                if (Value* err = explain_dict_set(candidate, interp, "arg-name", make_nil(interp))) return err;
            }
            if ((uint)k_name != 0) {
                if (Value* err = explain_dict_set(candidate, interp, "continuation-name", make_symbol(interp, k_name))) return err;
            } else {
                if (Value* err = explain_dict_set(candidate, interp, "continuation-name", make_nil(interp))) return err;
            }
        } else {
            if (Value* err = explain_dict_set(candidate, interp, "match-clause-index", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "match-tag", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "source", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "outcome", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "arg-name", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(candidate, interp, "continuation-name", make_nil(interp))) return err;
        }

        rev_candidates = explain_prepend_or_oom(candidate, rev_candidates, interp);
        if (rev_candidates != null && rev_candidates.tag == ERROR) return rev_candidates;
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
    if (Value* err = explain_dict_set(out, interp, "candidates", candidates)) return err;
    if (Value* err = explain_dict_set(trace, interp, "searched-handlers", make_int(interp, (long)searched_handlers))) return err;

    if (matched) {
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("handler-match"))) return err;
        if (Value* err = explain_dict_set(decision, interp, "handler-index", make_int(interp, (long)matched_handler_index))) return err;
        if (Value* err = explain_dict_set(decision, interp, "search-order", make_int(interp, (long)matched_search_order))) return err;
        if (Value* err = explain_dict_set(decision, interp, "clause-index", make_int(interp, (long)matched_clause_index))) return err;
        if (Value* err = explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "outcome",
            explain_effect_handler_outcome_symbol(matched_clause_body, interp))) return err;
        if ((uint)matched_clause_arg != 0) {
            if (Value* err = explain_dict_set(decision, interp, "arg-name", make_symbol(interp, matched_clause_arg))) return err;
        } else {
            if (Value* err = explain_dict_set(decision, interp, "arg-name", make_nil(interp))) return err;
        }
        if ((uint)matched_clause_k != 0) {
            if (Value* err = explain_dict_set(decision, interp, "continuation-name", make_symbol(interp, matched_clause_k))) return err;
        } else {
            if (Value* err = explain_dict_set(decision, interp, "continuation-name", make_nil(interp))) return err;
        }
        return out;
    }

    if (strict_stop) {
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("strict-unhandled-effect"))) return err;
        if (Value* err = explain_dict_set(decision, interp, "strict-handler-index", make_int(interp, (long)strict_handler_index))) return err;
        if (Value* err = explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
        return out;
    }

    usz fast_path_index = usz.max;
    Value* fast_path_prim = explain_effect_fast_path_primitive(interp, effect_tag, &fast_path_index);
    if (fast_path_prim != null) {
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("fast-path"))) return err;
        if (Value* err = explain_dict_set(decision, interp, "fast-path-index", make_int(interp, (long)fast_path_index))) return err;
        if (fast_path_prim.tag == PRIMITIVE) {
            if (Value* err = explain_dict_set(decision, interp, "fast-path-primitive", explain_primitive_name_value(interp, fast_path_prim.prim_val))) return err;
        } else {
            if (Value* err = explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp))) return err;
        }
        if (Value* err = explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
        return out;
    }

    if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"))) return err;
    if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("unhandled-effect"))) return err;
    if (Value* err = explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag))) return err;
    if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
    return out;
}

fn Value* explain_effect_result_from_signal(Interp* interp, Expr* target, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* arg_value = runtime_eval_expr(target.perform.arg, target_env, interp);
    if (arg_value != null && arg_value.tag == ERROR) return arg_value;
    SymbolId effect_tag = (SymbolId)0;
    Value* tag_err = resolve_effect_tag_symbol(
        target.perform.tag,
        target.perform.tag_expr,
        target.perform.has_tag_expr,
        target_env,
        interp,
        &effect_tag,
        "signal"
    );
    if (tag_err != null) return tag_err;
    return explain_effect_result_from_signal_value(interp, effect_tag, arg_value, out, decision, trace);
}

fn Value* explain_effect_result_from_resolve_value(
    Interp* interp,
    Value* k_val,
    Value* out,
    Value* decision,
    Value* trace
) {
    if (Value* err = explain_dict_set(trace, interp, "has-continuation-binding", explain_bool_value(interp, k_val != null))) return err;

    if (k_val == null || k_val.tag != CONTINUATION) {
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-invalid-continuation"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
        return out;
    }

    Continuation* k = k_val.cont_val;
    bool effect_cont = k.is_coroutine_based && k.handle_state != null;
    if (Value* err = explain_dict_set(trace, interp, "effect-continuation", explain_bool_value(interp, effect_cont))) return err;

    if (effect_cont) {
        if (k.ctx == null || k.ctx.status == main::StackCtxStatus.CTX_DEAD || k.ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
            if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"))) return err;
            if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-dead-continuation"))) return err;
            if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
            return out;
        }
        if (k.ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
            if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"))) return err;
            if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-continuation-not-suspended"))) return err;
            if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"))) return err;
            return out;
        }
        if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-effect-continuation"))) return err;
        if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve)) return err;
        return out;
    }

    if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"))) return err;
    if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-generic-continuation"))) return err;
    if (Value* err = explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve)) return err;
    return out;
}

fn Value* explain_effect_result_from_resolve(Interp* interp, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* k_val = explain_effect_continuation_lookup(target_env, interp);
    return explain_effect_result_from_resolve_value(interp, k_val, out, decision, trace);
}
