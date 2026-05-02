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

fn Expr* Parser.parse_reader_tag_expr(Parser* self) {
    // Detect #syntax reader template before treating as ordinary reader tag
    bool is_syntax_tag = self.lexer.current.text_len == 6;
    if (is_syntax_tag) {
        char[] tag_text = self.lexer.current.text[:6];
        is_syntax_tag = tag_text[0] == 's' && tag_text[1] == 'y' && tag_text[2] == 'n'
                     && tag_text[3] == 't' && tag_text[4] == 'a' && tag_text[5] == 'x';
    }
    if (is_syntax_tag) {
        return self.parse_syntax_template();
    }

    SymbolId tag = self.get_current_symbol();
    usz tag_line = self.lexer.current.line;
    usz tag_col = self.lexer.current.column;
    self.lexer.advance();

    Expr* arg = self.parse_expr();
    if (arg == null || self.has_error) return null;

    Expr* func = self.alloc_expr();
    if (func == null) return null;
    func.loc_line = tag_line;
    func.loc_column = tag_col;
    func.tag = E_VAR;
    func.var_expr.name = tag;

    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.loc_line = tag_line;
    e.loc_column = tag_col;
    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating reader tag call");
    if (e.call == null) return null;
    e.call.func = func;
    e.call.arg_count = 1;
    e.call.args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof, "out of memory while allocating reader tag argument");
    if (e.call.args == null) return null;
    e.call.args[0] = arg;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_quote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}
