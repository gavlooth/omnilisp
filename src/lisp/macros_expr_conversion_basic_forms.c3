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
