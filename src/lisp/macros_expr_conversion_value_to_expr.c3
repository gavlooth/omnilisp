module lisp;

import std::collections::list;
import main;

// --- value_to_expr helpers ---

fn Expr* make_nil_expr(Interp* interp) @inline {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = make_nil(interp);
    return e;
}

fn Expr* make_bool_expr(Interp* interp, bool truthy) @inline {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = truthy ? make_symbol(interp, interp.sym_true) : make_nil(interp);
    return e;
}

fn Expr* make_error_expr(Interp* interp, char[] msg) @inline {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = make_error(interp, msg);
    return e;
}

fn Expr* rest_car_expr(Value* rest, Interp* interp) @inline {
    if (is_cons(rest)) return value_to_expr(rest.cons_val.car, interp);
    return make_nil_expr(interp);
}

fn SymbolId val_to_sym(Value* v) @inline {
    return (v != null && v.tag == SYMBOL) ? v.sym_val : 0;
}

fn Expr* make_single_lambda_expr(Interp* interp, SymbolId param, Expr* body) {
    Expr* e = interp.alloc_expr();
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    e.lambda.param = param;
    e.lambda.param_count = 1;
    e.lambda.params = (SymbolId*)ast_arena_alloc(&interp.ast_arena, SymbolId.sizeof);
    e.lambda.params[0] = param;
    e.lambda.has_rest = false;
    e.lambda.rest_param = 0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    e.lambda.is_accessor_shorthand = false;
    return e;
}

fn Expr* make_nullary_lambda_expr(Interp* interp, Expr* body) {
    Expr* e = interp.alloc_expr();
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    e.lambda.param = (SymbolId)0xFFFFFFFF;
    e.lambda.param_count = 0;
    e.lambda.params = null;
    e.lambda.has_rest = false;
    e.lambda.rest_param = 0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    e.lambda.is_accessor_shorthand = false;
    return e;
}

fn Expr* vte_lambda(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Value* params_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = rest_car_expr(rest, interp);

    List{SymbolId} params;
    Value* p = params_list;
    while (is_cons(p)) {
        if (p.cons_val.car != null && p.cons_val.car.tag == SYMBOL) {
            params.push(p.cons_val.car.sym_val);
        }
        p = p.cons_val.cdr;
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
    Expr* body = rest_car_expr(rest, interp);

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
    e.tag = E_BLOCK;
    e.block = (ExprBlock*)ast_arena_alloc(&interp.ast_arena, ExprBlock.sizeof);
    e.block.expr_count = items.len();
    e.block.exprs = (Expr**)ast_arena_alloc(&interp.ast_arena, Expr*.sizeof * items.len());
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
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    e.call.func = func_e;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();
    return e;
}

fn Expr* vte_if_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Expr* e = interp.alloc_expr();
    e.tag = E_IF;
    e.if_expr.test = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    e.if_expr.then_branch = rest_car_expr(rest, interp);
    rest = is_cons(rest) ? rest.cons_val.cdr : rest;
    e.if_expr.else_branch = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_define_or_set_form(Value* rest, Interp* interp, bool is_set) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Expr* e = interp.alloc_expr();
    if (is_set) {
        e.tag = E_SET;
        e.set_expr.is_path = false;
        e.set_expr.path_segments = null;
        e.set_expr.path_segment_count = 0;
        e.set_expr.name = val_to_sym(rest.cons_val.car);
        rest = rest.cons_val.cdr;
        e.set_expr.value = rest_car_expr(rest, interp);
        return e;
    }
    e.tag = E_DEFINE;
    e.define.name = val_to_sym(rest.cons_val.car);
    rest = rest.cons_val.cdr;
    e.define.value = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_and_or_form(Value* rest, Interp* interp, bool is_and) {
    if (!is_cons(rest)) return make_bool_expr(interp, is_and);

    List{Expr*} items;
    Value* cursor = rest;
    while (is_cons(cursor)) {
        items.push(value_to_expr(cursor.cons_val.car, interp));
        cursor = cursor.cons_val.cdr;
    }

    if (items.len() == 0) {
        items.free();
        return make_bool_expr(interp, is_and);
    }
    if (items.len() == 1) {
        Expr* only = items[0];
        items.free();
        return only;
    }

    Expr* nested = items[items.len() - 1];
    for (isz i = (isz)items.len() - 2; i >= 0; i--) {
        Expr* e = interp.alloc_expr();
        e.tag = is_and ? E_AND : E_OR;
        if (is_and) {
            e.and_expr.left = items[(usz)i];
            e.and_expr.right = nested;
        } else {
            e.or_expr.left = items[(usz)i];
            e.or_expr.right = nested;
        }
        nested = e;
    }
    items.free();
    return nested;
}

fn Expr* vte_special_form(SymbolId sym, Value* rest, Interp* interp) {
    if ((uint)sym == (uint)interp.sym_if)               return vte_if_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_quote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_QUOTE;
        e.quote.datum = is_cons(rest) ? rest.cons_val.car : make_nil(interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_block)            return vte_begin(rest, interp);
    if ((uint)sym == (uint)interp.sym_lambda)           return vte_lambda(rest, interp);
    if ((uint)sym == (uint)interp.sym_let)              return vte_let(rest, interp);
    if ((uint)sym == (uint)interp.sym_define)           return vte_define_or_set_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_set)              return vte_define_or_set_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_and)              return vte_and_or_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_or)               return vte_and_or_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_checkpoint) {
        Expr* e = interp.alloc_expr();
        e.tag = E_RESET;
        e.reset.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_capture) {
        if (!is_cons(rest)) return make_nil_expr(interp);
        Value* k_name = rest.cons_val.car;
        rest = rest.cons_val.cdr;
        if (k_name == null || k_name.tag != SYMBOL) return make_nil_expr(interp);
        Expr* e = interp.alloc_expr();
        e.tag = E_SHIFT;
        e.shift.k_name = k_name.sym_val;
        e.shift.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_quasiquote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_QUASIQUOTE;
        e.quasiquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE;
        e.unquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote_splicing) {
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE_SPLICING;
        e.unquote_splicing.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_signal) {
        if (!is_cons(rest)) return make_nil_expr(interp);
        Expr* e = interp.alloc_expr();
        e.tag = E_PERFORM;
        e.perform.tag = val_to_sym(rest.cons_val.car);
        rest = rest.cons_val.cdr;
        e.perform.arg = rest_car_expr(rest, interp);
        return e;
    }
    return null;
}

/**
 * Convert a Value (list representation) back to an Expr AST.
 * Used for macro expansion: transforms the macro output back to executable AST.
 */
fn Expr* value_to_expr(Value* val, Interp* interp) {
    if (val == null || val.tag == NIL) return make_nil_expr(interp);

    if (val.tag == INT || val.tag == STRING || val.tag == DOUBLE) {
        Expr* e = interp.alloc_expr();
        e.tag = E_LIT;
        e.lit.value = val;
        return e;
    }

    if (val.tag == SYMBOL) {
        Expr* e = interp.alloc_expr();
        e.tag = E_VAR;
        e.var_expr.name = val.sym_val;
        return e;
    }

    if (val.tag != CONS) {
        Expr* e = interp.alloc_expr();
        e.tag = E_LIT;
        e.lit.value = val;
        return e;
    }

    Value* head = val.cons_val.car;
    if (head == null || head.tag != SYMBOL) return vte_call(val, interp);

    SymbolId sym = head.sym_val;
    Value* rest = val.cons_val.cdr;
    if (Expr* special = vte_special_form(sym, rest, interp)) return special;

    return vte_call(val, interp);
}
