module lisp;

fn bool parser_with_module_ensure_body_capacity(Parser* self, ExprWithModule* with_expr) @inline {
    if (with_expr.body_count < with_expr.body_capacity) return true;
    if (with_expr.body_capacity == 0) {
        with_expr.body_capacity = 8;
        with_expr.body = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, with_expr.body_capacity, "out of memory while allocating with body");
        return with_expr.body != null;
    }
    if (with_expr.body_capacity > usz.max / 2) {
        self.set_error("with body is too large");
        return false;
    }
    usz new_cap = with_expr.body_capacity * 2;
    usz body_bytes = 0;
    if (!lisp_checked_mul_usz(Expr*.sizeof, new_cap, &body_bytes)) {
        self.set_error("out of memory while growing with body");
        return false;
    }
    Expr** new_body = (Expr**)self.alloc_ast_bytes(body_bytes, "out of memory while growing with body");
    if (new_body == null) return false;
    for (usz i = 0; i < with_expr.body_count; i++) {
        new_body[i] = with_expr.body[i];
    }
    with_expr.body = new_body;
    with_expr.body_capacity = new_cap;
    return true;
}

fn Expr* Parser.parse_with_module(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume 'with'
    self.init_with_module_expr(e);
    if (self.has_error || e.with_module == null) return null;

    if (!self.parse_import_target_into(&e.with_module.import_expr, "with")) return null;

    while (self.lexer.current.type != T_RPAREN && !self.lexer.at_end() && !self.has_error) {
        if (!parser_with_module_ensure_body_capacity(self, e.with_module)) return null;
        Expr* body_expr = self.parse_expr();
        if (self.has_error || body_expr == null) return null;
        e.with_module.body[e.with_module.body_count] = body_expr;
        e.with_module.body_count++;
    }

    if (e.with_module.body_count == 0) {
        self.set_error("with requires at least one body expression");
        return null;
    }
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    return e;
}
