module lisp;

fn bool Parser.is_lambda_head_symbol(Parser* self, SymbolId head) {
    if ((uint)head == (uint)self.interp.sym_lambda) return true;
    char[] head_text = self.interp.symbols.get_name(head);
    return head_text.len == 2 &&
           head_text[0] == (char)0xCE &&
           head_text[1] == (char)0xBB;
}

fn Expr* Parser.parse_quasiquote_like_form(Parser* self, SymbolId head) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume head symbol
    if ((uint)head == (uint)self.interp.sym_quasiquote) {
        e.tag = E_QUASIQUOTE;
        e.quasiquote.body = self.parse_qq_template();
    } else if ((uint)head == (uint)self.interp.sym_unquote) {
        e.tag = E_UNQUOTE;
        e.unquote.body = self.parse_expr();
    } else {
        e.tag = E_UNQUOTE_SPLICING;
        e.unquote_splicing.body = self.parse_expr();
    }
    self.expect(T_RPAREN, ")");
    return e;
}

fn Expr* Parser.parse_symbol_head_form(Parser* self, SymbolId head, bool* matched) {
    *matched = true;
    if ((uint)head == (uint)self.interp.symbols.intern("^")) return self.parse_constructor_type_application();
    if (self.is_lambda_head_symbol(head)) return self.parse_lambda();
    if ((uint)head == (uint)self.interp.sym_if) return self.parse_if();
    if ((uint)head == (uint)self.interp.sym_let) return self.parse_let();
    if ((uint)head == (uint)self.interp.sym_define) return self.parse_define();
    if ((uint)head == (uint)self.interp.sym_quote) return self.parse_quote();
    if ((uint)head == (uint)self.interp.sym_checkpoint) return self.parse_reset();
    if ((uint)head == (uint)self.interp.sym_capture) return self.parse_shift();
    if ((uint)head == (uint)self.interp.sym_signal) return self.parse_perform();
    if ((uint)head == (uint)self.interp.sym_handle) return self.parse_handle();
    if ((uint)head == (uint)self.interp.sym_resolve) return self.parse_resolve();
    if ((uint)head == (uint)self.interp.symbols.intern("explain")) return self.parse_explain();
    if ((uint)head == (uint)self.interp.sym_with_continuation) return self.parse_with_continuation();
    if ((uint)head == (uint)self.interp.sym_match) return self.parse_match();
    if ((uint)head == (uint)self.interp.sym_and) return self.parse_and();
    if ((uint)head == (uint)self.interp.sym_or) return self.parse_or();
    if ((uint)head == (uint)self.interp.sym_block) return self.parse_block();
    if ((uint)head == (uint)self.interp.sym_set) return self.parse_set();
    if ((uint)head == (uint)self.interp.sym_quasiquote ||
        (uint)head == (uint)self.interp.sym_unquote ||
        (uint)head == (uint)self.interp.sym_unquote_splicing) {
        return self.parse_quasiquote_like_form(head);
    }
    if ((uint)head == (uint)self.interp.sym_module) return self.parse_module();
    if ((uint)head == (uint)self.interp.sym_import) return self.parse_import();
    if ((uint)head == (uint)self.interp.sym_export_from) return self.parse_export_from();
    if ((uint)head == (uint)self.interp.sym_pipe) return self.parse_pipe();
    *matched = false;
    return null;
}
