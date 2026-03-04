module lisp;

fn Expr* Parser.parse_module(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'module'

    // Parse module name
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected module name after module"); return null; }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    e.tag = E_MODULE;
    e.module_expr = mem::malloc(ExprModule.sizeof);
    e.module_expr.name = name;
    e.module_expr.export_count = 0;
    e.module_expr.export_capacity = 32;
    e.module_expr.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * 32);
    e.module_expr.body_count = 0;
    e.module_expr.body_capacity = 32;
    e.module_expr.body = (Expr**)mem::malloc(Expr*.sizeof * 32);

    // Parse (export sym1 sym2 ...) — required
    self.expect(T_LPAREN, "(");
    if (self.has_error) return null;
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected 'export' in module"); return null; }
    SymbolId maybe_export = self.get_current_symbol();
    if ((uint)maybe_export != (uint)self.interp.sym_export) { self.set_error("expected 'export' in module"); return null; }
    self.lexer.advance();  // consume 'export'
    while (self.lexer.current.type == T_SYMBOL) {
        if (e.module_expr.export_count >= e.module_expr.export_capacity) {
            usz new_cap = e.module_expr.export_capacity * 2;
            SymbolId* new_exports = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
            for (usz i = 0; i < e.module_expr.export_count; i++) new_exports[i] = e.module_expr.exports[i];
            mem::free(e.module_expr.exports);
            e.module_expr.exports = new_exports;
            e.module_expr.export_capacity = new_cap;
        }
        e.module_expr.exports[e.module_expr.export_count] = self.get_current_symbol();
        e.module_expr.export_count++;
        self.lexer.advance();
    }
    self.expect(T_RPAREN, ")");  // close export list

    // Parse remaining body expressions
    while (self.lexer.current.type != T_RPAREN && !self.lexer.at_end() && !self.has_error) {
        if (e.module_expr.body_count >= e.module_expr.body_capacity) {
            usz new_cap = e.module_expr.body_capacity * 2;
            Expr** new_body = (Expr**)mem::malloc(Expr*.sizeof * new_cap);
            for (usz i = 0; i < e.module_expr.body_count; i++) new_body[i] = e.module_expr.body[i];
            mem::free(e.module_expr.body);
            e.module_expr.body = new_body;
            e.module_expr.body_capacity = new_cap;
        }
        e.module_expr.body[e.module_expr.body_count] = self.parse_expr();
        e.module_expr.body_count++;
    }

    self.expect(T_RPAREN, ")");  // close module
    return e;
}
