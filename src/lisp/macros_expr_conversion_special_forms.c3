module lisp;

import main;

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
        e.perform.tag_expr = null;
        e.perform.has_tag_expr = false;
        rest = rest.cons_val.cdr;
        e.perform.arg = rest_car_expr(rest, interp);
        return e;
    }
    return null;
}
