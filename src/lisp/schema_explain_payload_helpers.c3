module lisp;

int g_schema_explain_force_cons_alloc_fail_nth = 0;

fn Value* explain_oom(Interp* interp) @inline {
    return raise_error(interp, "schema explain: out of memory");
}

fn Value* explain_make_dict(Interp* interp, uint capacity) @inline {
    return make_hashmap_checked(interp, capacity, "schema explain: out of memory");
}

fn Value* schema_resolve_named_symbol(Value* schema, Interp* interp) @inline {
    if (schema == null || schema.tag != SYMBOL) return schema;
    Value* named = interp.global_env.lookup(schema.sym_val);
    if (named != null && named.tag != NIL) {
        return named;
    }
    return schema;
}

fn Value* explain_bool_value(Interp* interp, bool value) @inline {
    return value ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false);
}

fn Value* explain_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return explain_oom(interp);
    if (value != null && value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return explain_oom(interp);
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) {
        return explain_oom(interp);
    }
    return null;
}

fn Value* explain_dict_set_symbol(Value* dict, Interp* interp, char[] key, SymbolId value) @inline {
    if (value == INVALID_SYMBOL_ID) return explain_oom(interp);
    return explain_dict_set(dict, interp, key, make_symbol(interp, value));
}

fn bool explain_force_cons_alloc_fail() @inline {
    if (g_schema_explain_force_cons_alloc_fail_nth <= 0) return false;
    g_schema_explain_force_cons_alloc_fail_nth--;
    return g_schema_explain_force_cons_alloc_fail_nth == 0;
}

fn Value* explain_prepend_or_oom(Value* item, Value* rev, Interp* interp) @inline {
    if (explain_force_cons_alloc_fail()) return explain_oom(interp);
    return make_cons(interp, item, rev);
}

fn Value* explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        Value* next = explain_prepend_or_oom(cur.cons_val.car, out, interp);
        if (next != null && next.tag == ERROR) return next;
        out = next;
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
    Value* source = explain_make_dict(interp, 8);
    if (source == null || source.tag != HASHMAP) return source;
    if (impl == null) {
        if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"))) return err;
        if (Value* err = explain_dict_set(source, interp, "line", make_nil(interp))) return err;
        if (Value* err = explain_dict_set(source, interp, "column", make_nil(interp))) return err;
        return source;
    }

    switch (impl.tag) {
        case CLOSURE: {
            if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("closure"))) return err;
            Expr* body = impl.closure_val != null ? impl.closure_val.body : null;
            if (body != null) {
                if (Value* err = explain_dict_set(source, interp, "line", make_int(interp, (long)body.loc_line))) return err;
                if (Value* err = explain_dict_set(source, interp, "column", make_int(interp, (long)body.loc_column))) return err;
            } else {
                if (Value* err = explain_dict_set(source, interp, "line", make_nil(interp))) return err;
                if (Value* err = explain_dict_set(source, interp, "column", make_nil(interp))) return err;
            }
            return source;
        }
        case PRIMITIVE:
            if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("primitive"))) return err;
            if (Value* err = explain_dict_set(source, interp, "line", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(source, interp, "column", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(source, interp, "name", explain_primitive_name_value(interp, impl.prim_val))) return err;
            return source;
        default:
            if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("runtime"))) return err;
            if (Value* err = explain_dict_set(source, interp, "line", make_nil(interp))) return err;
            if (Value* err = explain_dict_set(source, interp, "column", make_nil(interp))) return err;
            if (Value* err = explain_dict_set_symbol(source, interp, "tag", value_type_name(impl, interp))) return err;
            return source;
    }
}

fn Value* explain_signature_params(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.param_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.param_count; i++) {
        Value* param = explain_make_dict(interp, 8);
        if (param == null || param.tag != HASHMAP) return param;
        if (Value* err = explain_dict_set(param, interp, "index", make_int(interp, (long)i))) return err;
        SymbolId type_var = (sig.param_type_vars != null) ? sig.param_type_vars[i] : (SymbolId)0;
        if ((uint)type_var != 0) {
            if (Value* err = explain_dict_set(param, interp, "type-var", make_symbol(interp, type_var))) return err;
        } else {
            if (Value* err = explain_dict_set(param, interp, "type-var", make_nil(interp))) return err;
        }
        if (sig.has_val_literal != null && sig.has_val_literal[i]) {
            if (Value* err = explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("value"))) return err;
            ValueLiteralKey* literal = (sig.val_literals != null) ? &sig.val_literals[i] : null;
            if (Value* err = explain_dict_set(param, interp, "literal", explain_literal_value(interp, literal))) return err;
        } else if (sig.param_types != null && sig.param_types[i] != INVALID_TYPE_ID) {
            if (Value* err = explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("type"))) return err;
            if (Value* err = explain_dict_set(param, interp, "type", make_symbol(interp, explain_type_symbol(interp, sig.param_types[i])))) return err;
        } else {
            if (Value* err = explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("any"))) return err;
        }
        rev = explain_prepend_or_oom(param, rev, interp);
        if (rev != null && rev.tag == ERROR) return rev;
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_constraints(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.constraint_count == 0 || sig.constraints == null) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.constraint_count; i++) {
        Value* c = explain_make_dict(interp, 4);
        if (c == null || c.tag != HASHMAP) return c;
        if (Value* err = explain_dict_set(c, interp, "param", make_symbol(interp, sig.constraints[i].param_sym))) return err;
        if (Value* err = explain_dict_set(c, interp, "bound", make_symbol(interp, explain_type_symbol(interp, sig.constraints[i].bound_type)))) return err;
        rev = explain_prepend_or_oom(c, rev, interp);
        if (rev != null && rev.tag == ERROR) return rev;
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_score_components(Interp* interp, DispatchMatchBreakdown* breakdown) {
    Value* score = explain_make_dict(interp, 12);
    if (score == null || score.tag != HASHMAP) return score;
    if (Value* err = explain_dict_set(score, interp, "value", make_int(interp, breakdown.score_value_literal))) return err;
    if (Value* err = explain_dict_set(score, interp, "exact", make_int(interp, breakdown.score_exact))) return err;
    if (Value* err = explain_dict_set(score, interp, "widen", make_int(interp, breakdown.score_widen))) return err;
    if (Value* err = explain_dict_set(score, interp, "subtype", make_int(interp, breakdown.score_subtype))) return err;
    if (Value* err = explain_dict_set(score, interp, "any", make_int(interp, breakdown.score_any))) return err;
    if (Value* err = explain_dict_set(score, interp, "total", make_int(interp, breakdown.score_total))) return err;
    if (Value* err = explain_dict_set(score, interp, "constraints-ok", explain_bool_value(interp, breakdown.failure_reason != DISPATCH_MATCH_FAIL_CONSTRAINT))) return err;
    return score;
}

fn Value* explain_arg_types(Interp* interp, TypeId[] arg_types) {
    Value* rev = make_nil(interp);
    for (usz i = 0; i < arg_types.len; i++) {
        rev = explain_prepend_or_oom(
            make_symbol(interp, explain_type_symbol(interp, arg_types[i])),
            rev,
            interp);
        if (rev != null && rev.tag == ERROR) return rev;
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_init_common_output(
    Interp* interp,
    Value* out,
    SymbolId kind,
    Value* input,
    Value* decision,
    Value* trace
) {
    if (Value* err = explain_dict_set(out, interp, "kind", make_symbol(interp, kind))) return err;
    if (Value* err = explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("pending"))) return err;
    if (Value* err = explain_dict_set(out, interp, "input", input)) return err;
    if (Value* err = explain_dict_set(out, interp, "decision", decision)) return err;
    if (Value* err = explain_dict_set(out, interp, "candidates", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(out, interp, "trace", trace)) return err;
    if (Value* err = explain_dict_set(out, interp, "debug_message", make_nil(interp))) return err;
    return null;
}

fn Value* explain_init_dispatch_decision_shape(Interp* interp, Value* decision) {
    if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"))) return err;
    if (Value* err = explain_dict_set(decision, interp, "winner-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "best-score", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "tie-count", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "fallback-source", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "outcome", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "debug_message", make_nil(interp))) return err;
    return null;
}

fn Value* explain_init_dispatch_trace_shape(Interp* interp, Value* trace) {
    if (Value* err = explain_dict_set(trace, interp, "arg-types", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "arg-count", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "target-type", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "method-name", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "method-count", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "debug_message", make_nil(interp))) return err;
    return null;
}
