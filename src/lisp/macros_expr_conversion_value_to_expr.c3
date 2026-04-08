module lisp;

import main;

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
