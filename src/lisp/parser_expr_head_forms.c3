module lisp;

/**
 * Parse a list form after the opening '(' has been consumed.
 * Handles special forms (lambda, if, let, define, ...) and regular application.
 */
fn Expr* Parser.parse_list_form(Parser* self) {
    Lexer* lex = self.lexer;

    if (lex.current.type == T_RPAREN) {
        // Empty list () = nil literal
        Expr* e = self.alloc_expr_here();  // Capture location before consuming
        if (e == null) return null;
        lex.advance();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
        return e;
    }

    if (lex.current.type == T_SYMBOL) {
        SymbolId head = self.get_current_symbol();
        bool matched = false;
        Expr* special = self.parse_symbol_head_form(head, &matched);
        if (matched) {
            return special;
        }
    }

    // Regular application
    return self.parse_application();
}
