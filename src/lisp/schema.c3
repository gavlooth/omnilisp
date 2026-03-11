module lisp;

import std::core::mem;

// ============================================================
// explain selector surface:
//   (explain 'dispatch <form>)
//   (explain 'effect <form>)
//
// L1.1 lock:
// - selector must be quoted symbol
// - accepted selectors are 'dispatch and 'effect
// - target form is parser-wrapped as a thunk to avoid eager evaluation
// ============================================================

fn Value* schema_resolve_named_symbol(Value* schema, Interp* interp) @inline {
    if (schema == null || schema.tag != SYMBOL) return schema;
    Value* named = interp.global_env.lookup(schema.sym_val);
    if (named != null && named.tag != NIL) {
        return named;
    }
    return schema;
}

fn Value* explain_bool_value(Interp* interp, bool value) @inline {
    return value ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn void explain_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    hashmap_set(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)), value, interp);
}

fn void explain_dict_set_symbol(Value* dict, Interp* interp, char[] key, SymbolId value) @inline {
    hashmap_set(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)), make_symbol(interp, value), interp);
}

fn Value* explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    return out;
}

fn SymbolId explain_dispatch_failure_symbol(Interp* interp, DispatchMatchFailureReason reason) {
    switch (reason) {
        case DISPATCH_MATCH_FAIL_NONE:
            return interp.symbols.intern("none");
        case DISPATCH_MATCH_FAIL_ARITY:
            return interp.symbols.intern("arity-mismatch");
        case DISPATCH_MATCH_FAIL_VALUE_LITERAL:
            return interp.symbols.intern("value-literal-mismatch");
        case DISPATCH_MATCH_FAIL_TYPE:
            return interp.symbols.intern("type-mismatch");
        case DISPATCH_MATCH_FAIL_CONSTRAINT:
            return interp.symbols.intern("constraint-mismatch");
    }
}

fn Value* explain_literal_value(Interp* interp, ValueLiteralKey* literal) {
    if (literal == null) return make_nil(interp);
    switch (literal.tag) {
        case NIL:
            if ((uint)literal.sym_value == (uint)interp.sym_false) {
                return make_symbol(interp, interp.sym_false);
            }
            return make_nil(interp);
        case INT:
            return make_int(interp, literal.int_value);
        case SYMBOL:
            return make_symbol(interp, literal.sym_value);
        case STRING: {
            char[] text = interp.symbols.get_name(literal.sym_value);
            return make_string(interp, text);
        }
        default:
            return make_nil(interp);
    }
}

fn SymbolId explain_type_symbol(Interp* interp, TypeId type_id) {
    if (type_id == INVALID_TYPE_ID) return interp.sym_Any;
    TypeInfo* ti = interp.types.get(type_id);
    if (ti != null) return ti.name;
    return interp.sym_Any;
}

fn Value* explain_primitive_name_value(Interp* interp, Primitive* prim) {
    if (prim == null) return make_nil(interp);
    usz len = 0;
    while (len < prim.name.len && prim.name[len] != 0) len++;
    if (len == 0) return make_nil(interp);
    return make_string(interp, prim.name[:len]);
}

fn Value* explain_method_source(Interp* interp, Value* impl) {
    Value* source = make_hashmap(interp, 8);
    if (impl == null) {
        explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"));
        explain_dict_set(source, interp, "line", make_nil(interp));
        explain_dict_set(source, interp, "column", make_nil(interp));
        return source;
    }

    switch (impl.tag) {
        case CLOSURE: {
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("closure"));
            Expr* body = impl.closure_val != null ? impl.closure_val.body : null;
            if (body != null) {
                explain_dict_set(source, interp, "line", make_int(interp, (long)body.loc_line));
                explain_dict_set(source, interp, "column", make_int(interp, (long)body.loc_column));
            } else {
                explain_dict_set(source, interp, "line", make_nil(interp));
                explain_dict_set(source, interp, "column", make_nil(interp));
            }
            return source;
        }
        case PRIMITIVE:
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("primitive"));
            explain_dict_set(source, interp, "line", make_nil(interp));
            explain_dict_set(source, interp, "column", make_nil(interp));
            explain_dict_set(source, interp, "name", explain_primitive_name_value(interp, impl.prim_val));
            return source;
        default:
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("runtime"));
            explain_dict_set(source, interp, "line", make_nil(interp));
            explain_dict_set(source, interp, "column", make_nil(interp));
            explain_dict_set_symbol(source, interp, "tag", value_type_name(impl, interp));
            return source;
    }
}

fn Value* explain_signature_params(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.param_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.param_count; i++) {
        Value* param = make_hashmap(interp, 8);
        explain_dict_set(param, interp, "index", make_int(interp, (long)i));
        SymbolId type_var = (sig.param_type_vars != null) ? sig.param_type_vars[i] : (SymbolId)0;
        if ((uint)type_var != 0) {
            explain_dict_set(param, interp, "type-var", make_symbol(interp, type_var));
        } else {
            explain_dict_set(param, interp, "type-var", make_nil(interp));
        }
        if (sig.has_val_literal != null && sig.has_val_literal[i]) {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("value"));
            ValueLiteralKey* literal = (sig.val_literals != null) ? &sig.val_literals[i] : null;
            explain_dict_set(param, interp, "literal", explain_literal_value(interp, literal));
        } else if (sig.param_types != null && sig.param_types[i] != INVALID_TYPE_ID) {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("type"));
            explain_dict_set(param, interp, "type", make_symbol(interp, explain_type_symbol(interp, sig.param_types[i])));
        } else {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("any"));
        }
        rev = make_cons(interp, param, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_constraints(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.constraint_count == 0 || sig.constraints == null) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.constraint_count; i++) {
        Value* c = make_hashmap(interp, 4);
        explain_dict_set(c, interp, "param", make_symbol(interp, sig.constraints[i].param_sym));
        explain_dict_set(c, interp, "bound", make_symbol(interp, explain_type_symbol(interp, sig.constraints[i].bound_type)));
        rev = make_cons(interp, c, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_score_components(Interp* interp, DispatchMatchBreakdown* breakdown) {
    Value* score = make_hashmap(interp, 12);
    explain_dict_set(score, interp, "value", make_int(interp, breakdown.score_value_literal));
    explain_dict_set(score, interp, "exact", make_int(interp, breakdown.score_exact));
    explain_dict_set(score, interp, "widen", make_int(interp, breakdown.score_widen));
    explain_dict_set(score, interp, "subtype", make_int(interp, breakdown.score_subtype));
    explain_dict_set(score, interp, "any", make_int(interp, breakdown.score_any));
    explain_dict_set(score, interp, "total", make_int(interp, breakdown.score_total));
    explain_dict_set(score, interp, "constraints-ok",
        explain_bool_value(interp, breakdown.failure_reason != DISPATCH_MATCH_FAIL_CONSTRAINT));
    return score;
}

fn Value* explain_arg_types(Interp* interp, TypeId[] arg_types) {
    Value* rev = make_nil(interp);
    for (usz i = 0; i < arg_types.len; i++) {
        rev = make_cons(interp, make_symbol(interp, explain_type_symbol(interp, arg_types[i])), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn void explain_init_common_output(
    Interp* interp,
    Value* out,
    SymbolId kind,
    Value* input,
    Value* decision,
    Value* trace
) {
    explain_dict_set(out, interp, "kind", make_symbol(interp, kind));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("pending"));
    explain_dict_set(out, interp, "input", input);
    explain_dict_set(out, interp, "decision", decision);
    explain_dict_set(out, interp, "candidates", make_nil(interp));
    explain_dict_set(out, interp, "trace", trace);
    explain_dict_set(out, interp, "debug_message", make_nil(interp));
}

fn void explain_init_dispatch_decision_shape(Interp* interp, Value* decision) {
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"));
    explain_dict_set(decision, interp, "winner-index", make_nil(interp));
    explain_dict_set(decision, interp, "best-score", make_nil(interp));
    explain_dict_set(decision, interp, "tie-count", make_nil(interp));
    explain_dict_set(decision, interp, "fallback-source", make_nil(interp));
    explain_dict_set(decision, interp, "outcome", make_nil(interp));
    explain_dict_set(decision, interp, "debug_message", make_nil(interp));
}

fn void explain_init_dispatch_trace_shape(Interp* interp, Value* trace) {
    explain_dict_set(trace, interp, "arg-types", make_nil(interp));
    explain_dict_set(trace, interp, "arg-count", make_nil(interp));
    explain_dict_set(trace, interp, "target-type", make_nil(interp));
    explain_dict_set(trace, interp, "method-name", make_nil(interp));
    explain_dict_set(trace, interp, "method-count", make_nil(interp));
    explain_dict_set(trace, interp, "debug_message", make_nil(interp));
}

fn bool explain_eval_call_target(
    Expr* target,
    Env* target_env,
    Interp* interp,
    Value** callee_out,
    Value*[]* args_out,
    Value*** args_storage_out
) {
    *callee_out = jit_eval(target.call.func, target_env, interp);
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
            (*args_out)[i] = jit_eval(target.call.args[i], target_env, interp);
            if ((*args_out)[i] != null && (*args_out)[i].tag == ERROR) {
                *callee_out = (*args_out)[i];
                return false;
            }
        }
    }
    return true;
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

    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.symbols.intern("dispatch"), input_form, decision, trace);
    explain_init_dispatch_decision_shape(interp, decision);
    explain_init_dispatch_trace_shape(interp, trace);

    if (target == null || target.tag != E_CALL) {
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
        explain_dict_set_symbol(candidate, interp, "failure",
            explain_dispatch_failure_symbol(interp, breakdown.failure_reason));
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

fn Value* prim_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error(interp, "explain: expected (explain 'dispatch <form>) or (explain 'effect <form>)");
    }

    Value* selector = args[0];
    if (selector == null || selector.tag != SYMBOL) {
        return raise_error(interp, "explain: selector must be symbol ('dispatch or 'effect)");
    }

    SymbolId dispatch_sym = interp.symbols.intern("dispatch");
    SymbolId effect_sym = interp.sym_effect;
    if ((uint)selector.sym_val != (uint)dispatch_sym && (uint)selector.sym_val != (uint)effect_sym) {
        return raise_error(interp, "explain: unknown selector; expected 'dispatch or 'effect");
    }

    if ((uint)selector.sym_val == (uint)dispatch_sym) {
        return explain_dispatch_result_from_thunk(interp, args[1]);
    }

    return explain_effect_result_from_thunk(interp, args[1]);
}

// ============================================================
// Legacy schema explanation kept under explicit name:
// (schema-explain schema-name value) → list of (path message) or nil if valid
// ============================================================

fn Value* prim_schema_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error_with_payload_names(
            interp,
            "schema",
            "schema/schema-explain-expected-schema-value",
            "schema-explain: expected (schema-explain schema value)"
        );
    }

    Value* schema = schema_resolve_named_symbol(args[0], interp);

    Value* val = args[1];

    if (schema_check(schema, val, interp)) {
        return make_nil(interp);  // valid — no errors
    }

    // Simple explanation: return schema that failed
    Value* msg = make_string(interp, "validation failed");
    return make_cons(interp, msg, make_nil(interp));
}
