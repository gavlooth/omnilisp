module lisp;

fn SymbolId explain_effect_miss_reason_symbol(Interp* interp, bool dispatchable, bool matched, bool strict_mode) {
    if (dispatchable) return interp.symbols.intern("none");
    if (matched) return interp.symbols.intern("handler-inactive");
    if (strict_mode) return interp.symbols.intern("strict-boundary");
    return interp.symbols.intern("tag-miss");
}

fn SymbolId explain_effect_clause_tag(EffectHandler* handler, usz clause_index) {
    if (handler.tags != null) {
        return handler.tags[clause_index];
    }
    if (handler.clauses != null) {
        return handler.clauses[clause_index].effect_tag;
    }
    return (SymbolId)0;
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
    return (SymbolId)0;
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
        rev = explain_prepend_or_oom(make_symbol(interp, tag), rev, interp);
        if (rev != null && rev.tag == ERROR) return rev;
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
            if (expr.perform.has_tag_expr && explain_expr_contains_resolve(expr.perform.tag_expr)) return true;
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
    Value* source = explain_make_dict(interp, 6);
    if (source == null || source.tag != HASHMAP) return source;
    if (clause_body == null) {
        if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"))) return err;
        if (Value* err = explain_dict_set(source, interp, "line", make_nil(interp))) return err;
        if (Value* err = explain_dict_set(source, interp, "column", make_nil(interp))) return err;
        return source;
    }
    if (Value* err = explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("clause"))) return err;
    if (Value* err = explain_dict_set(source, interp, "line", make_int(interp, (long)clause_body.loc_line))) return err;
    if (Value* err = explain_dict_set(source, interp, "column", make_int(interp, (long)clause_body.loc_column))) return err;
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

fn Value* explain_init_effect_decision_shape(Interp* interp, Value* decision) {
    if (Value* err = explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"))) return err;
    if (Value* err = explain_dict_set(decision, interp, "outcome", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "handler-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "search-order", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "clause-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "match-tag", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "strict-handler-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "fast-path-index", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "arg-name", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "continuation-name", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(decision, interp, "debug_message", make_nil(interp))) return err;
    return null;
}

fn Value* explain_init_effect_trace_shape(Interp* interp, Value* trace) {
    if (Value* err = explain_dict_set(trace, interp, "effect-tag", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "arg-type", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "handler-count", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "searched-handlers", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "stack-context-active", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "has-continuation-binding", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "effect-continuation", make_nil(interp))) return err;
    if (Value* err = explain_dict_set(trace, interp, "debug_message", make_nil(interp))) return err;
    return null;
}
