module lisp;

fn bool parser_module_ensure_export_capacity(Parser* self, ExprModule* module_expr) @inline {
    if (module_expr.export_count < module_expr.export_capacity) return true;
    if (module_expr.export_capacity == 0 || module_expr.exports == null) {
        self.set_error("out of memory while growing module export list");
        return false;
    }
    usz new_cap = module_expr.export_capacity * 2;
    if (new_cap <= module_expr.export_capacity) {
        self.set_error("module export list is too large");
        return false;
    }
    if (new_cap > usz.max / SymbolId.sizeof) {
        self.set_error("module export list is too large");
        return false;
    }
    SymbolId* new_exports = (SymbolId*)self.alloc_ast_bytes(SymbolId.sizeof * new_cap, "out of memory while growing module export list");
    if (new_exports == null) return false;
    for (usz i = 0; i < module_expr.export_count; i++) {
        new_exports[i] = module_expr.exports[i];
    }
    module_expr.exports = new_exports;
    module_expr.export_capacity = new_cap;
    return true;
}

fn bool parser_module_ensure_body_capacity(Parser* self, ExprModule* module_expr) @inline {
    if (module_expr.body_count < module_expr.body_capacity) return true;
    if (module_expr.body_capacity == 0 || module_expr.body == null) {
        self.set_error("out of memory while growing module body");
        return false;
    }
    usz new_cap = module_expr.body_capacity * 2;
    if (new_cap <= module_expr.body_capacity) {
        self.set_error("module body is too large");
        return false;
    }
    if (new_cap > usz.max / Expr*.sizeof) {
        self.set_error("module body is too large");
        return false;
    }
    Expr** new_body = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * new_cap, "out of memory while growing module body");
    if (new_body == null) return false;
    for (usz i = 0; i < module_expr.body_count; i++) {
        new_body[i] = module_expr.body[i];
    }
    module_expr.body = new_body;
    module_expr.body_capacity = new_cap;
    return true;
}

fn Expr* Parser.alloc_module_expr(Parser* self, SymbolId name) {
    Expr* e = self.alloc_expr_here();
    if (e == null) {
        self.set_error("out of memory while creating module expression");
        return null;
    }
    e.tag = E_MODULE;
    e.module_expr = (ExprModule*)self.alloc_ast_bytes(ExprModule.sizeof, "out of memory while creating module expression");
    if (e.module_expr == null) return null;
    e.module_expr.name = name;
    e.module_expr.export_count = 0;
    e.module_expr.export_capacity = 32;
    if (e.module_expr.export_capacity > usz.max / SymbolId.sizeof) {
        self.set_error("module export table is too large");
        return null;
    }
    e.module_expr.exports = (SymbolId*)self.alloc_ast_bytes(SymbolId.sizeof * 32, "out of memory while allocating module export list");
    if (e.module_expr.exports == null) return null;
    e.module_expr.body_count = 0;
    e.module_expr.body_capacity = 32;
    if (e.module_expr.body_capacity > usz.max / Expr*.sizeof) {
        self.set_error("module body is too large");
        return null;
    }
    e.module_expr.body = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * 32, "out of memory while allocating module body");
    if (e.module_expr.body == null) return null;
    return e;
}

fn bool Parser.parse_module_export_keyword(Parser* self) {
    self.expect(T_LPAREN, "(");
    if (self.has_error) return false;
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected 'export' in module");
        return false;
    }
    SymbolId maybe_export = self.get_current_symbol();
    if ((uint)maybe_export != (uint)self.interp.sym_export) {
        self.set_error("expected 'export' in module");
        return false;
    }
    self.lexer.advance();  // consume 'export'
    return true;
}

fn bool Parser.module_append_export(Parser* self, ExprModule* module_expr) {
    if (!parser_module_ensure_export_capacity(self, module_expr)) return false;
    module_expr.exports[module_expr.export_count] = self.get_current_symbol();
    module_expr.export_count++;
    self.lexer.advance();
    return true;
}

fn bool Parser.module_append_body_expr(Parser* self, ExprModule* module_expr) {
    if (!parser_module_ensure_body_capacity(self, module_expr)) return false;
    Expr* body_expr = self.parse_expr();
    if (self.has_error || body_expr == null) return false;
    module_expr.body[module_expr.body_count] = body_expr;
    module_expr.body_count++;
    return true;
}

fn Expr* Parser.parse_module(Parser* self) {
    if (self.has_error) return null;
    self.lexer.advance();  // consume 'module'

    // Parse module name
    if (self.lexer.current.type != T_SYMBOL && self.lexer.current.type != T_PATH) { self.set_error("expected module name after module"); return null; }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    Expr* e = self.alloc_module_expr(name);
    if (self.has_error || e == null) return null;
    ExprModule* module_expr = e.module_expr;

    // Parse (export sym1 sym2 ...) — required
    if (!self.parse_module_export_keyword()) return null;
    while (self.lexer.current.type == T_SYMBOL) {
        if (!self.module_append_export(module_expr)) return null;
    }
    self.expect(T_RPAREN, ")");  // close export list
    if (self.has_error) return null;

    // Parse remaining body expressions
    while (self.lexer.current.type != T_RPAREN && !self.lexer.at_end() && !self.has_error) {
        if (!self.module_append_body_expr(module_expr)) return null;
    }

    self.expect(T_RPAREN, ")");  // close module
    if (self.has_error) return null;
    return e;
}
