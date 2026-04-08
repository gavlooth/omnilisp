module lisp;

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
    hashmap_set_symbol(dict.hashmap_val, interp.symbols.intern(key), value, interp);
}

fn void explain_dict_set_symbol(Value* dict, Interp* interp, char[] key, SymbolId value) @inline {
    hashmap_set_symbol(dict.hashmap_val, interp.symbols.intern(key), make_symbol(interp, value), interp);
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
    explain_dict_set(score, interp, "constraints-ok", explain_bool_value(interp, breakdown.failure_reason != DISPATCH_MATCH_FAIL_CONSTRAINT));
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
