module lisp;

fn bool Parser.parse_import_target(Parser* self, Expr* e) {
    if (self.lexer.current.type == T_SYMBOL || self.lexer.current.type == T_PATH) {
        e.import_expr.name = self.get_current_symbol();
        self.lexer.advance();
        return true;
    }
    if (self.lexer.current.type == T_STRING) {
        e.import_expr.has_path = true;
        char[] str = self.lexer.current.text[:self.lexer.current.text_len];
        usz len = str.len;
        if (len == usz.max) {
            self.set_error("import path is too long");
            return false;
        }
        char* path_buf = (char*)self.alloc_ast_bytes(len + 1, "out of memory while parsing import path");
        if (path_buf == null) return false;
        for (usz i = 0; i < len; i++) {
            path_buf[i] = str[i];
        }
        path_buf[len] = 0;
        e.import_expr.path = path_buf;
        e.import_expr.path_len = len;
        e.import_expr.name = self.intern_or_error(str, "failed to intern import path");
        if (e.import_expr.name == INVALID_SYMBOL_ID) return false;
        self.lexer.advance();
        return true;
    }
    self.set_error("expected module name or path after import");
    return false;
}

fn bool Parser.parse_import_symbol_spec(Parser* self, Expr* e, usz idx) {
    e.import_expr.imports[idx] = self.get_current_symbol();
    e.import_expr.aliases[idx] = (SymbolId)0;
    self.lexer.advance();
    if (parser_token_is_legacy_marker(self.lexer, "as")) {
        self.set_error("expected 'as in import specifier");
        return false;
    }
    if (self.lexer.current.type == T_QUOTE) {
        self.lexer.advance();  // consume '
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected 'as in import specifier");
            return false;
        }
        SymbolId maybe_as = self.get_current_symbol();
        if ((uint)maybe_as != (uint)self.interp.sym_as) {
            self.set_error("expected 'as in import specifier");
            return false;
        }
        self.lexer.advance();
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected alias after 'as");
            return false;
        }
        e.import_expr.aliases[idx] = self.get_current_symbol();
        self.lexer.advance();
    }
    return true;
}

fn bool Parser.parse_import_paren_spec(Parser* self, Expr* e, usz idx) {
    self.lexer.advance();  // consume '('
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected symbol in import specifier");
        return false;
    }
    e.import_expr.imports[idx] = self.get_current_symbol();
    self.lexer.advance();
    if (self.lexer.current.type != T_QUOTE) {
        self.set_error("expected 'as in import specifier");
        return false;
    }
    self.lexer.advance();
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected 'as in import specifier");
        return false;
    }
    SymbolId maybe_as = self.get_current_symbol();
    if ((uint)maybe_as != (uint)self.interp.sym_as) {
        self.set_error("expected 'as in import specifier");
        return false;
    }
    self.lexer.advance();
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected alias after 'as");
        return false;
    }
    e.import_expr.aliases[idx] = self.get_current_symbol();
    self.lexer.advance();
    self.expect(T_RPAREN, ")");
    return !self.has_error;
}

fn bool Parser.parse_import_selective_list(Parser* self, Expr* e) {
    self.lexer.advance();  // consume '('
    while (self.lexer.current.type != T_RPAREN && !self.lexer.at_end() && !self.has_error) {
        if (e.import_expr.import_count == usz.max) {
            self.set_error("import specifier count overflow");
            return false;
        }
        if (!self.import_ensure_capacity(e, e.import_expr.import_count + 1)) return false;
        usz idx = e.import_expr.import_count;
        bool ok = false;
        if (self.lexer.current.type == T_LPAREN) {
            ok = self.parse_import_paren_spec(e, idx);
        } else if (self.lexer.current.type == T_SYMBOL) {
            ok = self.parse_import_symbol_spec(e, idx);
        } else {
            self.set_error("expected symbol in import list");
            return false;
        }
        if (!ok) return false;
        e.import_expr.import_count++;
    }
    self.expect(T_RPAREN, ")");
    return !self.has_error;
}
