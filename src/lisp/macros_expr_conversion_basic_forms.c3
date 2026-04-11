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

fn bool vte_rest_has_exact_arity(Value* rest, usz expected) @inline {
    return list_length(rest) == expected;
}

fn Expr* vte_var_expr(Interp* interp, SymbolId sym) @inline {
    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
}

fn Expr* vte_generic_set_call(Value* rest, Interp* interp) {
    Expr* func_e = vte_var_expr(interp, interp.sym_set);
    if (func_e == null) return make_error_expr(interp, "value->expr: failed to allocate set! call");

    List{Expr*} args;
    Value* cursor = rest;
    while (is_cons(cursor)) {
        args.push(value_to_expr(cursor.cons_val.car, interp));
        cursor = cursor.cons_val.cdr;
    }

    Expr* e = interp.alloc_expr();
    if (e == null) {
        args.free();
        return make_error_expr(interp, "value->expr: failed to allocate set! call");
    }
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    if (e.call == null) {
        args.free();
        return make_error_expr(interp, "value->expr: failed to allocate set! call");
    }
    e.call.func = func_e;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, args.len());
    if (args.len() > 0 && e.call.args == null) {
        args.free();
        return make_error_expr(interp, "value->expr: failed to allocate set! call arguments");
    }
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();
    return e;
}

fn Expr* vte_if_form(Value* rest, Interp* interp) {
    if (!vte_rest_has_exact_arity(rest, 3)) return make_error_expr(interp, "value->expr: if expects exactly 3 arguments");
    Expr* e = interp.alloc_expr();
    e.tag = E_IF;
    e.if_expr.test = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    e.if_expr.then_branch = rest_car_expr(rest, interp);
    rest = rest.cons_val.cdr;
    e.if_expr.else_branch = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_define_or_set_form(Value* rest, Interp* interp, bool is_set) {
    usz arity = list_length(rest);
    if (is_set && arity > 2) return vte_generic_set_call(rest, interp);
    if (is_set && arity != 2) return make_error_expr(interp, "value->expr: set! expects name and value");
    if (!is_set && arity != 2) return make_error_expr(interp, "value->expr: define expects name and value");
    if (rest.cons_val.car == null || rest.cons_val.car.tag != SYMBOL) {
        return make_error_expr(interp, is_set ? "value->expr: set! name must be a symbol" : "value->expr: define name must be a symbol");
    }

    Expr* e = interp.alloc_expr();
    if (is_set) {
        e.tag = E_SET;
        e.set_expr.is_path = false;
        e.set_expr.path_segments = null;
        e.set_expr.path_segment_count = 0;
        e.set_expr.name = rest.cons_val.car.sym_val;
        rest = rest.cons_val.cdr;
        e.set_expr.value = rest_car_expr(rest, interp);
        return e;
    }
    e.tag = E_DEFINE;
    e.define.name = rest.cons_val.car.sym_val;
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
