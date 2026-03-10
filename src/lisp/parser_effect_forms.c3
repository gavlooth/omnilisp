module lisp;

fn Expr* Parser.parse_reset(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'reset' location
    self.lexer.advance();  // consume 'reset'

    Expr* body = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESET;
    e.reset.body = body;
    return e;
}

fn Expr* Parser.parse_shift(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'shift' location
    self.lexer.advance();  // consume 'shift'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return null; }
    SymbolId k_name = self.get_current_symbol();
    self.lexer.advance();

    Expr* body = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_SHIFT;
    e.shift.k_name = k_name;
    e.shift.body = body;
    return e;
}

fn Expr* Parser.parse_perform(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'perform' location
    self.lexer.advance();  // consume 'perform'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return null; }
    SymbolId etag = self.get_current_symbol();
    self.lexer.advance();

    Expr* arg = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_PERFORM;
    e.perform.tag = etag;
    e.perform.arg = arg;
    return e;
}

fn Expr* Parser.parse_resolve(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'resolve'

    Expr* val = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESOLVE;
    e.resolve.value = val;
    return e;
}

// (with-continuation k body...) → (let ((k __k)) body...)
fn Expr* Parser.parse_with_continuation(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume 'with-continuation'

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return null; }
    SymbolId k_name = self.get_current_symbol();
    self.lexer.advance();

    // Parse body expressions (implicit block)
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");

    // Desugar to (let ((k __k)) body)
    e.tag = E_LET;
    e.let_expr.name = k_name;
    Expr* var_ref = self.interp.alloc_expr();
    var_ref.tag = E_VAR;
    var_ref.var_expr.name = self.interp.sym__k;
    e.let_expr.init = var_ref;
    e.let_expr.body = body;
    e.let_expr.is_recursive = false;
    return e;
}
