module lisp;

import main;

fn SymbolId explain_effect_miss_reason_symbol(Interp* interp, bool dispatchable, bool matched, bool strict_mode) {
    if (dispatchable) return interp.symbols.intern("none");
    if (matched) return interp.symbols.intern("handler-inactive");
    if (strict_mode) return interp.symbols.intern("strict-boundary");
    return interp.symbols.intern("tag-miss");
}

fn SymbolId explain_effect_clause_tag(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].effect_tag;
    }
    if (handler.tags != null) {
        return handler.tags[clause_index];
    }
    return 0;
}

fn Expr* explain_effect_clause_body(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].handler_body;
    }
    if (handler.closures != null &&
        handler.closures[clause_index] != null &&
        handler.closures[clause_index].tag == CLOSURE) {
        return handler.closures[clause_index].closure_val.body;
    }
    return null;
}

fn SymbolId explain_effect_clause_k_name(EffectHandler* handler, usz clause_index, Interp* interp) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].k_name;
    }
    return interp.sym__k;
}

fn SymbolId explain_effect_clause_arg_name(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].arg_name;
    }
    return 0;
}

fn isz explain_effect_find_clause_index(EffectHandler* handler, SymbolId effect_tag) {
    if (handler == null) return -1;
    for (usz j = 0; j < handler.clause_count; j++) {
        SymbolId clause_tag = explain_effect_clause_tag(handler, j);
        if ((uint)clause_tag == (uint)effect_tag) return (isz)j;
    }
    return -1;
}

fn Value* explain_effect_handler_tags(Interp* interp, EffectHandler* handler) {
    if (handler == null || handler.clause_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz j = 0; j < handler.clause_count; j++) {
        SymbolId tag = explain_effect_clause_tag(handler, j);
        if ((uint)tag == 0) continue;
        rev = make_cons(interp, make_symbol(interp, tag), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool explain_expr_contains_resolve(Expr* expr) {
    if (expr == null) return false;
    if (expr.tag == E_RESOLVE) return true;
    switch (expr.tag) {
        case E_IF:
            return explain_expr_contains_resolve(expr.if_expr.test) ||
                explain_expr_contains_resolve(expr.if_expr.then_branch) ||
                explain_expr_contains_resolve(expr.if_expr.else_branch);
        case E_LET:
            return explain_expr_contains_resolve(expr.let_expr.init) ||
                explain_expr_contains_resolve(expr.let_expr.body);
        case E_LAMBDA:
            // Resolve inside nested lambdas is not guaranteed on this signal path.
            return false;
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (explain_expr_contains_resolve(expr.block.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (explain_expr_contains_resolve(expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (explain_expr_contains_resolve(expr.call.args[i])) return true;
            }
            return false;
        case E_APP:
            return explain_expr_contains_resolve(expr.app.func) ||
                explain_expr_contains_resolve(expr.app.arg);
        case E_MATCH:
            if (explain_expr_contains_resolve(expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (explain_expr_contains_resolve(expr.match.clauses[i].result)) return true;
            }
            return false;
        case E_AND:
            return explain_expr_contains_resolve(expr.and_expr.left) ||
                explain_expr_contains_resolve(expr.and_expr.right);
        case E_OR:
            return explain_expr_contains_resolve(expr.or_expr.left) ||
                explain_expr_contains_resolve(expr.or_expr.right);
        case E_SET:
            return explain_expr_contains_resolve(expr.set_expr.value);
        case E_DEFINE:
            return explain_expr_contains_resolve(expr.define.value);
        case E_PERFORM:
            return explain_expr_contains_resolve(expr.perform.arg);
        case E_HANDLE:
            // Handle clause executes in its own runtime loop.
            return false;
        case E_RESET:
            return explain_expr_contains_resolve(expr.reset.body);
        case E_SHIFT:
            return explain_expr_contains_resolve(expr.shift.body);
        default:
            return false;
    }
}

fn SymbolId explain_effect_handler_outcome_symbol(Expr* clause_body, Interp* interp) {
    if (clause_body == null) return interp.symbols.intern("unknown");
    if (explain_expr_contains_resolve(clause_body)) return interp.sym_resolve;
    return interp.symbols.intern("abort");
}

fn Value* explain_effect_clause_source(Interp* interp, Expr* clause_body) {
    Value* source = make_hashmap(interp, 6);
    if (clause_body == null) {
        explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"));
        explain_dict_set(source, interp, "line", make_nil(interp));
        explain_dict_set(source, interp, "column", make_nil(interp));
        return source;
    }
    explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("clause"));
    explain_dict_set(source, interp, "line", make_int(interp, (long)clause_body.loc_line));
    explain_dict_set(source, interp, "column", make_int(interp, (long)clause_body.loc_column));
    return source;
}

fn Value* explain_effect_fast_path_primitive(Interp* interp, SymbolId effect_tag, usz* table_index_out) {
    *table_index_out = usz.max;
    for (usz i = 0; i < interp.fast_path_count; i++) {
        if ((uint)interp.fast_path_table[i].tag != (uint)effect_tag) continue;
        *table_index_out = i;
        return interp.fast_path_table[i].prim;
    }
    return null;
}

fn Value* explain_effect_continuation_lookup(Env* target_env, Interp* interp) {
    Value* k_val = null;
    if (target_env != null) {
        k_val = target_env.lookup(interp.sym__k);
    }
    if (k_val == null || k_val.tag == NIL) {
        k_val = interp.global_env.lookup(interp.sym__k);
    }
    return k_val;
}

fn void explain_init_effect_decision_shape(Interp* interp, Value* decision) {
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"));
    explain_dict_set(decision, interp, "outcome", make_nil(interp));
    explain_dict_set(decision, interp, "handler-index", make_nil(interp));
    explain_dict_set(decision, interp, "search-order", make_nil(interp));
    explain_dict_set(decision, interp, "clause-index", make_nil(interp));
    explain_dict_set(decision, interp, "match-tag", make_nil(interp));
    explain_dict_set(decision, interp, "strict-handler-index", make_nil(interp));
    explain_dict_set(decision, interp, "fast-path-index", make_nil(interp));
    explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp));
    explain_dict_set(decision, interp, "arg-name", make_nil(interp));
    explain_dict_set(decision, interp, "continuation-name", make_nil(interp));
    explain_dict_set(decision, interp, "debug_message", make_nil(interp));
}

fn void explain_init_effect_trace_shape(Interp* interp, Value* trace) {
    explain_dict_set(trace, interp, "effect-tag", make_nil(interp));
    explain_dict_set(trace, interp, "arg-type", make_nil(interp));
    explain_dict_set(trace, interp, "handler-count", make_nil(interp));
    explain_dict_set(trace, interp, "searched-handlers", make_nil(interp));
    explain_dict_set(trace, interp, "stack-context-active", make_nil(interp));
    explain_dict_set(trace, interp, "has-continuation-binding", make_nil(interp));
    explain_dict_set(trace, interp, "effect-continuation", make_nil(interp));
    explain_dict_set(trace, interp, "debug_message", make_nil(interp));
}

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
    Value* arg_value = jit_eval(target.perform.arg, target_env, interp);
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
