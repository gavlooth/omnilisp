module lisp;

/**
 * Parse export-from: (export-from mod (sym1 sym2)) or (export-from mod 'all)
 */
fn void Parser.init_export_from_expr(Parser* self, Expr* e) {
    e.tag = E_EXPORT_FROM;
    e.export_from.name_count = 0;
    e.export_from.name_capacity = 0;
    e.export_from.names = null;
    e.export_from.all = false;
}

fn bool Parser.parse_export_from_source_module(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected module name after export-from");
        return false;
    }
    e.export_from.source_module = self.get_current_symbol();
    self.lexer.advance();
    return true;
}

fn void Parser.ensure_export_from_name_capacity(Parser* self, Expr* e) {
    if (e.export_from.name_count < e.export_from.name_capacity) return;
    usz new_cap = e.export_from.name_capacity * 2;
    SymbolId* new_names = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * new_cap);
    for (usz i = 0; i < e.export_from.name_count; i++) {
        new_names[i] = e.export_from.names[i];
    }
    e.export_from.names = new_names;
    e.export_from.name_capacity = new_cap;
}

fn bool Parser.parse_export_from_name_list(Parser* self, Expr* e) {
    e.export_from.name_capacity = 16;
    e.export_from.names = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * 16);
    self.lexer.advance();  // consume '('
    while (self.lexer.current.type != T_RPAREN && !self.lexer.at_end() && !self.has_error) {
        self.ensure_export_from_name_capacity(e);
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected symbol in export-from list");
            return false;
        }
        e.export_from.names[e.export_from.name_count] = self.get_current_symbol();
        e.export_from.name_count++;
        self.lexer.advance();
    }
    self.expect(T_RPAREN, ")");  // close name list
    return !self.has_error;
}

fn bool Parser.parse_export_from_specifiers(Parser* self, Expr* e) {
    char[] spec_err = "expected 'all or (names...) after export-from module";
    if (self.lexer.current.type == T_QUOTE) {
        self.lexer.advance();  // consume '
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error(spec_err);
            return false;
        }
        SymbolId maybe_all = self.get_current_symbol();
        if ((uint)maybe_all == (uint)self.interp.sym_all) {
            e.export_from.all = true;
            self.lexer.advance();
            return true;
        }
        self.set_error(spec_err);
        return false;
    }
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_export_from_name_list(e);
    }
    self.set_error(spec_err);
    return false;
}

fn Expr* Parser.parse_export_from(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'export-from'

    self.init_export_from_expr(e);

    // Parse source module name
    if (!self.parse_export_from_source_module(e)) return null;

    // Parse specifiers: 'all or (sym1 sym2 ...)
    if (!self.parse_export_from_specifiers(e)) return null;

    self.expect(T_RPAREN, ")");  // close export-from
    return e;
}
