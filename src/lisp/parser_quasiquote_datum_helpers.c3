module lisp;

import std::collections::list;

fn Expr* Parser.parse_qq_string_literal(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = STRING;
    StringVal* builder = strval_new_from_slice(self.lexer.current.text[:self.lexer.current.text_len]);
    if (builder == null) {
        self.set_error("out of memory while parsing string literal");
        return null;
    }
    strval_into_value(builder, e.lit.value);
    self.lexer.advance();
    return e;
}

fn Expr* Parser.parse_qq_list_form(Parser* self) {
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type == T_RPAREN) {
        Expr* e = self.alloc_expr_here();
        if (e == null) return null;
        self.lexer.advance();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
        return e;
    }

    Expr* func = self.parse_qq_template();
    List{Expr*} qq_args;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        qq_args.push(self.parse_qq_template());
    }
    if (self.has_error) { qq_args.free(); return null; }
    self.lexer.advance();  // consume ')'

    Expr* e = self.alloc_expr();
    if (e == null) { qq_args.free(); return null; }
    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating quasiquote call");
    if (e.call == null) { qq_args.free(); return null; }
    e.call.func = func;
    e.call.arg_count = qq_args.len();
    e.call.args = null;
    if (qq_args.len() > 0) {
        e.call.args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * qq_args.len(), "out of memory while allocating quasiquote call arguments");
        if (e.call.args == null) { qq_args.free(); return null; }
    }
    for (usz i = 0; i < qq_args.len(); i++) { e.call.args[i] = qq_args[i]; }
    qq_args.free();
    return e;
}

fn Expr* Parser.parse_qq_backtick(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = self.parse_qq_template();
    return e;
}

fn Expr* Parser.parse_qq_unquote(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_UNQUOTE;
    e.unquote.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_qq_unquote_splicing(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_UNQUOTE_SPLICING;
    e.unquote_splicing.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_qq_quote_datum(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}

fn Expr* Parser.parse_qq_int_literal(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    long val = self.lexer.current.int_value;
    self.lexer.advance();
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = INT;
    e.lit.value.int_val = val;
    return e;
}

fn Expr* Parser.parse_qq_var_symbol(Parser* self, SymbolId sym) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
}
