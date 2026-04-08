module lisp;

/**
 * Parse import: (import name), (import name (sym1 (sym2 'as alias))), (import name 'all), (import "path")
 */
fn Expr* Parser.parse_import(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume 'import'

    self.init_import_expr(e);
    if (!self.parse_import_target(e)) return null;

    // Check for optional specifiers before closing )
    // (import name 'all) or (import name (sym1 (sym2 'as alias)))
    if (self.lexer.current.type == T_QUOTE) {
        self.lexer.advance();  // consume '
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected 'all or (specifiers...) after import module");
            return null;
        }
        SymbolId maybe_all = self.get_current_symbol();
        if ((uint)maybe_all == (uint)self.interp.sym_all) {
            // (import name 'all)
            e.import_expr.import_all = true;
            self.lexer.advance();
        } else {
            self.set_error("expected 'all or (specifiers...) after import module");
            return null;
        }
    } else if (self.lexer.current.type == T_LPAREN) {
        if (!self.parse_import_selective_list(e)) return null;
    } else if (self.lexer.current.type != T_RPAREN) {
        self.set_error("expected 'all or (specifiers...) after import module");
        return null;
    }

    self.expect(T_RPAREN, ")");  // close import
    return e;
}
