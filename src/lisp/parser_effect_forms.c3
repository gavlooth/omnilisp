module lisp;

fn bool Parser.parse_effect_tag_ref(
    Parser* self,
    SymbolId* out_tag,
    Expr** out_tag_expr,
    bool* out_has_tag_expr
) {
    if (self.lexer.current.type == T_SYMBOL) {
        *out_tag = self.get_current_symbol();
        *out_tag_expr = null;
        *out_has_tag_expr = false;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_PATH) {
        Expr* tag_expr = self.parse_path_expr();
        if (tag_expr == null || self.has_error) return false;
        *out_tag = (SymbolId)0;
        *out_tag_expr = tag_expr;
        *out_has_tag_expr = true;
        return true;
    }

    self.set_error("expected effect tag");
    return false;
}

fn Expr* Parser.parse_reset(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'checkpoint' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'checkpoint'

    Expr* body = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESET;
    e.reset.body = body;
    return e;
}

fn Expr* Parser.parse_shift(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'capture' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'capture'

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
    if (e == null) return null;
    self.lexer.advance();  // consume 'perform'

    SymbolId etag = (SymbolId)0;
    Expr* tag_expr = null;
    bool has_tag_expr = false;
    if (!self.parse_effect_tag_ref(&etag, &tag_expr, &has_tag_expr)) return null;

    Expr* arg = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_PERFORM;
    e.perform.tag = etag;
    e.perform.tag_expr = tag_expr;
    e.perform.has_tag_expr = has_tag_expr;
    e.perform.arg = arg;
    return e;
}

fn Expr* Parser.parse_resolve(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume 'resolve'

    Expr* val = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_RESOLVE;
    e.resolve.value = val;
    return e;
}

// (with-continuation k body...) → (let (k __k) body...)
fn Expr* Parser.parse_with_continuation(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume 'with-continuation'

    if (!self.in_handle_clause) {
        self.set_error("with-continuation is only valid inside handle clauses");
        return null;
    }

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return null; }
    SymbolId k_name = self.get_current_symbol();
    self.lexer.advance();

    // Parse body expressions (implicit block)
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");

    // Desugar to canonical Omni let syntax: (let (k __k) body)
    e.tag = E_LET;
    e.let_expr.name = k_name;
    Expr* var_ref = self.alloc_expr();
    if (var_ref == null) return null;
    var_ref.tag = E_VAR;
    var_ref.var_expr.name = self.interp.sym__k;
    e.let_expr.init = var_ref;
    e.let_expr.body = body;
    e.let_expr.is_recursive = false;
    return e;
}
