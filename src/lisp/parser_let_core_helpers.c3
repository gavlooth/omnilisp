module lisp;

import std::collections::list;

fn Expr* Parser.build_let_expr_chain(Parser* self, Expr* outer, Expr* body, List{SymbolId} names, List{Pattern*} patterns, List{Expr*} inits, bool is_recursive) {
    usz binding_count = names.len();
    if (binding_count == 1) {
        outer.tag = E_LET;
        outer.let_expr.name = names[0];
        outer.let_expr.init = inits[0];
        outer.let_expr.body = body;
        outer.let_expr.is_recursive = is_recursive;
        outer.let_expr.pattern = patterns[0];
        return outer;
    }

    Expr* inner = body;
    for (usz i = binding_count; i > 1; i--) {
        Expr* wrapper = self.alloc_expr();
        if (wrapper == null) return null;
        wrapper.tag = E_LET;
        wrapper.loc_line = outer.loc_line;
        wrapper.loc_column = outer.loc_column;
        wrapper.let_expr.name = names[i - 1];
        wrapper.let_expr.init = inits[i - 1];
        wrapper.let_expr.body = inner;
        wrapper.let_expr.is_recursive = false;
        wrapper.let_expr.pattern = patterns[i - 1];
        inner = wrapper;
    }

    outer.tag = E_LET;
    outer.let_expr.name = names[0];
    outer.let_expr.init = inits[0];
    outer.let_expr.body = inner;
    outer.let_expr.is_recursive = is_recursive;
    outer.let_expr.pattern = patterns[0];
    return outer;
}
