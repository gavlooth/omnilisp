module lisp;

/**
 * Parse a complete expression.
 */
fn Expr* Parser.parse_backtick_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = self.parse_qq_template();
    return e;
}

fn Expr* Parser.parse_unquote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_UNQUOTE;
    e.unquote.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_unquote_splicing_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
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

fn Expr* Parser.parse_regex_literal_expr(Parser* self) {
    char[] pat = self.lexer.current.text[:self.lexer.current.text_len];
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = make_string(self.interp, pat);
    self.lexer.advance();
    return e;
}

fn Expr* Parser.parse_quote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}
