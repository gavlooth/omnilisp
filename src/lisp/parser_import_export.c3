module lisp;

fn bool parser_token_is_legacy_marker(Lexer* lexer, char[] marker) {
    if (lexer.current.type != T_SYMBOL) return false;
    if (lexer.current.text_len != marker.len + 1) return false;
    if (lexer.current.text[0] != ':') return false;
    for (usz i = 0; i < marker.len; i++) {
        if (lexer.current.text[i + 1] != marker[i]) return false;
    }
    return true;
}

fn void Parser.init_import_expr(Parser* self, Expr* e) {
    e.tag = E_IMPORT;
    e.import_expr.has_path = false;
    e.import_expr.path = null;
    e.import_expr.path_len = 0;
    e.import_expr.import_count = 0;
    e.import_expr.import_capacity = 0;
    e.import_expr.imports = null;
    e.import_expr.aliases = null;
    e.import_expr.import_all = false;
}

fn void Parser.import_ensure_capacity(Parser* self, Expr* e, usz needed) {
    if (e.import_expr.import_capacity >= needed) return;
    usz new_cap = e.import_expr.import_capacity == 0 ? 16 : e.import_expr.import_capacity;
    while (new_cap < needed) {
        new_cap *= 2;
    }
    SymbolId* new_imports = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
    SymbolId* new_aliases = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
    for (usz i = 0; i < e.import_expr.import_count; i++) {
        new_imports[i] = e.import_expr.imports[i];
        new_aliases[i] = e.import_expr.aliases[i];
    }
    if (e.import_expr.imports != null) mem::free(e.import_expr.imports);
    if (e.import_expr.aliases != null) mem::free(e.import_expr.aliases);
    e.import_expr.imports = new_imports;
    e.import_expr.aliases = new_aliases;
    e.import_expr.import_capacity = new_cap;
}

fn bool Parser.parse_import_target(Parser* self, Expr* e) {
    if (self.lexer.current.type == T_SYMBOL) {
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
        char* path_buf = (char*)mem::malloc(len + 1);
        if (path_buf == null) {
            self.set_error("out of memory while parsing import path");
            return false;
        }
        for (usz i = 0; i < len; i++) {
            path_buf[i] = str[i];
        }
        path_buf[len] = 0;
        e.import_expr.path = path_buf;
        e.import_expr.path_len = len;
        e.import_expr.name = self.interp.symbols.intern(str);
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
        self.import_ensure_capacity(e, e.import_expr.import_count + 1);
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

/**
 * Parse import: (import name), (import name (sym1 (sym2 'as alias))), (import name 'all), (import "path")
 */
fn Expr* Parser.parse_import(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
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
    } else if (parser_token_is_legacy_marker(self.lexer, "all")) {
        self.set_error("expected 'all or (specifiers...) after import module");
        return null;
    } else if (self.lexer.current.type != T_RPAREN) {
        self.set_error("expected 'all or (specifiers...) after import module");
        return null;
    }

    self.expect(T_RPAREN, ")");  // close import
    return e;
}

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
    SymbolId* new_names = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
    for (usz i = 0; i < e.export_from.name_count; i++) {
        new_names[i] = e.export_from.names[i];
    }
    mem::free(e.export_from.names);
    e.export_from.names = new_names;
    e.export_from.name_capacity = new_cap;
}

fn bool Parser.parse_export_from_name_list(Parser* self, Expr* e) {
    e.export_from.name_capacity = 16;
    e.export_from.names = (SymbolId*)mem::malloc(SymbolId.sizeof * 16);
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
