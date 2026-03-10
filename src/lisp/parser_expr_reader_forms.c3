module lisp;

import std::collections::list;

/**
 * Parse a complete expression.
 */
fn Expr* Parser.parse_backtick_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = self.parse_qq_template();
    return e;
}

fn Expr* Parser.parse_unquote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE;
    e.unquote.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_unquote_splicing_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE_SPLICING;
    e.unquote_splicing.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_form_comment_expr(Parser* self) {
    long skip_count = self.lexer.current.int_value;
    self.lexer.advance();
    for (long i = 0; i < skip_count; i++) {
        Expr* discarded = self.parse_expr();
        if (discarded == null || self.has_error) return null;
    }
    return self.parse_expr();
}

fn Expr* Parser.parse_set_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume #{

    List{Expr*} elems;
    while (self.lexer.current.type != T_RBRACE && self.lexer.current.type != T_EOF && !self.has_error) {
        elems.push(self.parse_expr());
    }
    if (self.has_error) {
        elems.free();
        return null;
    }
    self.expect(T_RBRACE, "}");

    e.tag = E_CALL;
    usz total = elems.len() + 1;
    Expr** args = (Expr**)mem::malloc(Expr*.sizeof * total);
    if (args == null) {
        elems.free();
        self.set_error("out of memory while parsing set literal");
        return null;
    }
    args[0] = self.interp.alloc_expr();
    args[0].tag = E_VAR;
    args[0].var_expr.name = self.interp.symbols.intern("set");
    for (usz i = 0; i < elems.len(); i++) {
        args[i + 1] = elems[i];
    }
    e.call.args = args;
    e.call.arg_count = total;
    elems.free();
    return e;
}

fn Expr* Parser.parse_regex_literal_expr(Parser* self) {
    char[] pat = self.lexer.current.text[:self.lexer.current.text_len];
    Expr* e = self.alloc_expr_here();
    e.tag = E_LIT;
    e.lit.value = make_string(self.interp, pat);
    self.lexer.advance();
    return e;
}

fn Expr* Parser.parse_quote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}
