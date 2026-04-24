module lisp;

import std::collections::list;
import main;

fn void* macro_ast_array_alloc(Interp* interp, usz elem_size, usz count) {
    if (count == 0) return null;
    if (elem_size > usz.max / count) return null;
    return ast_arena_alloc(&interp.ast_arena, elem_size * count);
}

fn bool macro_symbol_name_eq(Interp* interp, SymbolId sym, char[] expected) {
    char[] name = interp.symbols.get_name(sym);
    if (name.len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (name[i] != expected[i]) return false;
    }
    return true;
}

fn bool macro_value_is_symbol(Value* v, SymbolId sym) @inline {
    return v != null && v.tag == SYMBOL && (uint)v.sym_val == (uint)sym;
}

fn bool macro_value_is_symbol_text(Value* v, Interp* interp, char[] expected) @inline {
    return v != null && v.tag == SYMBOL && macro_symbol_name_eq(interp, v.sym_val, expected);
}

fn bool macro_value_name_text(Value* v, Interp* interp, char[] expected) @inline {
    if (macro_value_is_symbol_text(v, interp, expected)) return true;
    return primitive_name_equals(v, expected);
}

fn bool macro_value_is_quoted_symbol(Value* v, Interp* interp, SymbolId sym) {
    if (v == null || v.tag != CONS || !is_list(v)) return false;
    if (list_length(v) != 2) return false;
    Value* head = v.cons_val.car;
    if (!macro_value_is_symbol(head, interp.sym_quote)) return false;
    Value* rest = v.cons_val.cdr;
    if (!is_cons(rest)) return false;
    return macro_value_is_symbol(rest.cons_val.car, sym);
}

fn Value* macro_make_quoted_symbol(Interp* interp, SymbolId sym) {
    return make_tagged_form1(interp, interp.sym_quote, make_symbol(interp, sym));
}

fn Value* macro_symbol_list_to_value(Interp* interp, SymbolId* syms, usz count) {
    Value* result = make_nil(interp);
    for (usz i = count; i > 0; i--) {
        result = make_cons(interp, make_symbol(interp, syms[i - 1]), result);
    }
    return result;
}

fn Value* macro_expr_array_to_value(Interp* interp, Expr** exprs, usz count) {
    Value* result = make_nil(interp);
    for (usz i = count; i > 0; i--) {
        result = make_cons(interp, expr_to_value(exprs[i - 1], interp), result);
    }
    return result;
}

fn Expr** macro_expr_list_to_array(Interp* interp, List{Expr*}* items) {
    Expr** arr = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, items.len());
    if (items.len() > 0 && arr == null) return null;
    for (usz i = 0; i < items.len(); i++) arr[i] = (*items)[i];
    return arr;
}

fn MatchClause* macro_match_clause_list_to_array(Interp* interp, List{MatchClause}* items) {
    MatchClause* arr = (MatchClause*)macro_ast_array_alloc(interp, MatchClause.sizeof, items.len());
    if (items.len() > 0 && arr == null) return null;
    for (usz i = 0; i < items.len(); i++) arr[i] = (*items)[i];
    return arr;
}

fn EffectClause* macro_effect_clause_list_to_array(Interp* interp, List{EffectClause}* items) {
    EffectClause* arr = (EffectClause*)macro_ast_array_alloc(interp, EffectClause.sizeof, items.len());
    if (items.len() > 0 && arr == null) return null;
    for (usz i = 0; i < items.len(); i++) arr[i] = (*items)[i];
    return arr;
}

fn SymbolId* macro_symbol_list_to_array(Interp* interp, List{SymbolId}* items) {
    SymbolId* arr = (SymbolId*)macro_ast_array_alloc(interp, SymbolId.sizeof, items.len());
    if (items.len() > 0 && arr == null) return null;
    for (usz i = 0; i < items.len(); i++) arr[i] = (*items)[i];
    return arr;
}

fn Value* pattern_to_value(Pattern* pat, Interp* interp) {
    if (pat == null) return make_symbol(interp, interp.symbols.intern("_"));

    switch (pat.tag) {
        case PAT_WILDCARD:
            return make_symbol(interp, interp.symbols.intern("_"));
        case PAT_VAR:
            return make_symbol(interp, pat.var_name);
        case PAT_LIT:
            return pat.lit_value;
        case PAT_QUOTE:
            return make_tagged_form1(interp, interp.sym_quote, pat.quote_datum);
        case PAT_AS: {
            Value* inner = pattern_to_value(pat.as_sub, interp);
            return make_list3(interp, inner, macro_make_quoted_symbol(interp, interp.sym_as), make_symbol(interp, pat.as_name));
        }
        case PAT_GUARD: {
            SymbolId q = interp.symbols.intern("?");
            Value* pred = expr_to_value(pat.guard_pred, interp);
            if (pat.guard_sub == null) return make_list2(interp, make_symbol(interp, q), pred);
            return make_list3(interp, make_symbol(interp, q), pred, pattern_to_value(pat.guard_sub, interp));
        }
        case PAT_CONSTRUCTOR: {
            Value* result = make_nil(interp);
            for (usz i = pat.ctor_sub_count; i > 0; i--) {
                result = make_cons(interp, pattern_to_value(pat.ctor_sub_patterns[i - 1], interp), result);
            }
            return make_cons(interp, make_symbol(interp, pat.constructor_name), result);
        }
        default:
            return make_symbol(interp, interp.symbols.intern("_"));
    }
}

fn Pattern* vte_pattern_from_value(Value* val, Interp* interp) {
    Pattern* p = interp.alloc_pattern();
    if (p == null) return null;
    if (val == null || val.tag == NIL) {
        p.tag = PAT_LIT;
        p.lit_value = make_nil(interp);
        return p;
    }

    if (val.tag == INT || val.tag == STRING || val.tag == DOUBLE || val.tag == FLOAT32) {
        p.tag = PAT_LIT;
        p.lit_value = val;
        return p;
    }

    if (val.tag == SYMBOL) {
        if (macro_symbol_name_eq(interp, val.sym_val, "_")) {
            p.tag = PAT_WILDCARD;
        } else if ((uint)val.sym_val == (uint)interp.sym_false) {
            p.tag = PAT_LIT;
            p.lit_value = val;
        } else if ((uint)val.sym_val == (uint)interp.sym_Void) {
            p.tag = PAT_LIT;
            p.lit_value = interp.alloc_value_root();
            if (p.lit_value == null) return null;
            p.lit_value.tag = VOID;
        } else {
            p.tag = PAT_VAR;
            p.var_name = val.sym_val;
        }
        return p;
    }

    if (val.tag != CONS || !is_list(val)) {
        p.tag = PAT_WILDCARD;
        return p;
    }

    if (macro_value_is_symbol(val.cons_val.car, interp.sym_quote) && list_length(val) == 2) {
        p.tag = PAT_QUOTE;
        p.quote_datum = val.cons_val.cdr.cons_val.car;
        return p;
    }

    if (list_length(val) == 3 && macro_value_is_quoted_symbol(val.cons_val.cdr.cons_val.car, interp, interp.sym_as)) {
        p.tag = PAT_AS;
        p.as_sub = vte_pattern_from_value(val.cons_val.car, interp);
        if (p.as_sub == null) return null;
        Value* as_name = val.cons_val.cdr.cons_val.cdr.cons_val.car;
        p.as_name = as_name != null && as_name.tag == SYMBOL ? as_name.sym_val : interp.symbols.intern("_");
        return p;
    }

    Value* head = val.cons_val.car;
    if (head != null && head.tag == SYMBOL && macro_symbol_name_eq(interp, head.sym_val, "?")) {
        p.tag = PAT_GUARD;
        Value* rest = val.cons_val.cdr;
        p.guard_pred = is_cons(rest) ? value_to_expr(rest.cons_val.car, interp) : make_nil_expr(interp);
        rest = is_cons(rest) ? rest.cons_val.cdr : make_nil(interp);
        p.guard_sub = is_cons(rest) ? vte_pattern_from_value(rest.cons_val.car, interp) : null;
        if (is_cons(rest) && p.guard_sub == null) return null;
        return p;
    }

    if (head != null && head.tag == SYMBOL) {
        p.tag = PAT_CONSTRUCTOR;
        p.constructor_name = head.sym_val;
        List{Pattern*} subs;
        Value* rest = val.cons_val.cdr;
        while (is_cons(rest)) {
            Pattern* sub = vte_pattern_from_value(rest.cons_val.car, interp);
            if (sub == null) {
                subs.free();
                return null;
            }
            subs.push(sub);
            rest = rest.cons_val.cdr;
        }
        p.ctor_sub_count = subs.len();
        p.ctor_sub_patterns = (Pattern**)macro_ast_array_alloc(interp, Pattern*.sizeof, subs.len());
        if (subs.len() > 0 && p.ctor_sub_patterns == null) {
            subs.free();
            return null;
        }
        for (usz i = 0; i < subs.len(); i++) p.ctor_sub_patterns[i] = subs[i];
        subs.free();
        return p;
    }

    p.tag = PAT_WILDCARD;
    return p;
}

fn Expr* make_single_lambda_expr(Interp* interp, SymbolId param, Expr* body) {
    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    if (e.lambda == null) return make_error_expr(interp, "macro expansion failed to allocate lambda");
    e.lambda.param = param;
    e.lambda.param_count = 1;
    e.lambda.params = (SymbolId*)macro_ast_array_alloc(interp, SymbolId.sizeof, 1);
    if (e.lambda.params == null) return make_error_expr(interp, "macro expansion failed to allocate lambda parameters");
    e.lambda.params[0] = param;
    e.lambda.has_rest = false;
    e.lambda.rest_param = (SymbolId)0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    return e;
}

fn Expr* make_nullary_lambda_expr(Interp* interp, Expr* body) {
    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    if (e.lambda == null) return make_error_expr(interp, "macro expansion failed to allocate lambda");
    e.lambda.param = (SymbolId)0xFFFFFFFF;
    e.lambda.param_count = 0;
    e.lambda.params = null;
    e.lambda.has_rest = false;
    e.lambda.rest_param = (SymbolId)0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    return e;
}

fn Expr* vte_implicit_body(Value* rest, Interp* interp, char[] missing_msg) {
    if (!is_cons(rest)) return make_error_expr(interp, missing_msg);
    if (!is_cons(rest.cons_val.cdr)) return value_to_expr(rest.cons_val.car, interp);
    return vte_begin(rest, interp);
}

fn Expr* vte_lambda(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: lambda expects parameter list and body");
    Value* params_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = vte_implicit_body(rest, interp, "value->expr: lambda expects body");

    List{SymbolId} params;
    Value* p = params_list;
    while (is_cons(p)) {
        if (p.cons_val.car == null || p.cons_val.car.tag != SYMBOL) {
            params.free();
            return make_error_expr(interp, "value->expr: expected lambda parameters to be symbols");
        }
        params.push(p.cons_val.car.sym_val);
        p = p.cons_val.cdr;
    }
    if (!is_nil(p)) {
        params.free();
        return make_error_expr(interp, "value->expr: expected lambda parameters to be a proper list");
    }
    usz param_count = params.len();

    if (param_count == 0) {
        params.free();
        return make_nullary_lambda_expr(interp, body);
    }
    if (param_count == 1) {
        SymbolId p0 = params[0];
        params.free();
        return make_single_lambda_expr(interp, p0, body);
    }
    Expr* inner = body;
    for (usz i = param_count; i > 1; i--) {
        inner = make_single_lambda_expr(interp, params[i - 1], inner);
    }
    Expr* result = make_single_lambda_expr(interp, params[0], inner);
    params.free();
    return result;
}

fn Expr* vte_let(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "macro expansion emitted malformed let: expected bindings list and body");
    Value* bindings_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = vte_implicit_body(rest, interp, "macro expansion emitted malformed let: expected body");

    char[] invalid_bindings_msg = "macro expansion emitted invalid let bindings; expected flat pairs: (let (x 1 y 2) ...)";
    if (!is_cons(bindings_list)) return make_error_expr(interp, invalid_bindings_msg);

    List{SymbolId} names;
    List{Value*} inits;
    Value* bp = bindings_list;
    while (is_cons(bp)) {
        Value* name_v = bp.cons_val.car;
        if (name_v == null || name_v.tag != SYMBOL) {
            names.free();
            inits.free();
            return make_error_expr(interp, invalid_bindings_msg);
        }

        names.push(name_v.sym_val);
        bp = bp.cons_val.cdr;
        if (!is_cons(bp)) {
            names.free();
            inits.free();
            return make_error_expr(interp, invalid_bindings_msg);
        }
        inits.push(bp.cons_val.car);
        bp = bp.cons_val.cdr;
    }
    if (bp != null && bp.tag != NIL) {
        names.free();
        inits.free();
        return make_error_expr(interp, invalid_bindings_msg);
    }

    if (names.len() == 0) {
        names.free();
        inits.free();
        return make_error_expr(interp, "macro expansion emitted malformed let: expected at least one binding");
    }

    Expr* inner = body;
    for (usz i = names.len(); i > 0; i--) {
        Expr* e = interp.alloc_expr();
        if (e == null) {
            names.free(); inits.free();
            return make_error_expr(interp, "macro expansion failed to allocate let expression");
        }
        e.tag = E_LET;
        e.let_expr.name = names[i - 1];
        e.let_expr.init = value_to_expr(inits[i - 1], interp);
        e.let_expr.body = inner;
        e.let_expr.is_recursive = false;
        inner = e;
    }
    names.free(); inits.free();
    return inner;
}

fn Expr* vte_begin(Value* rest, Interp* interp) {
    List{Expr*} items;
    while (is_cons(rest)) {
        items.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }
    Expr* e = interp.alloc_expr();
    if (e == null) {
        items.free();
        return make_error_expr(interp, "macro expansion failed to allocate block");
    }
    e.tag = E_BLOCK;
    e.block = (ExprBlock*)ast_arena_alloc(&interp.ast_arena, ExprBlock.sizeof);
    if (e.block == null) {
        items.free();
        return make_error_expr(interp, "macro expansion failed to allocate block");
    }
    e.block.expr_count = items.len();
    e.block.exprs = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, items.len());
    if (items.len() > 0 && e.block.exprs == null) {
        items.free();
        return make_error_expr(interp, "macro expansion failed to allocate block expressions");
    }
    for (usz i = 0; i < items.len(); i++) { e.block.exprs[i] = items[i]; }
    items.free();
    return e;
}

fn Expr* vte_call(Value* val, Interp* interp) {
    usz len = list_length(val);
    if (len == 0) return make_nil_expr(interp);

    Expr* func_e = value_to_expr(val.cons_val.car, interp);
    List{Expr*} args;
    Value* rest = val.cons_val.cdr;
    while (is_cons(rest)) {
        args.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }
    Expr* e = interp.alloc_expr();
    if (e == null) {
        args.free();
        return make_error_expr(interp, "macro expansion failed to allocate call");
    }
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    if (e.call == null) {
        args.free();
        return make_error_expr(interp, "macro expansion failed to allocate call");
    }
    e.call.func = func_e;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, args.len());
    if (args.len() > 0 && e.call.args == null) {
        args.free();
        return make_error_expr(interp, "macro expansion failed to allocate call arguments");
    }
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();
    return e;
}
