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

fn bool Parser.import_ensure_capacity(Parser* self, Expr* e, usz needed) {
    if (e.import_expr.import_capacity >= needed) return true;
    usz new_cap = e.import_expr.import_capacity == 0 ? 16 : e.import_expr.import_capacity;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            self.set_error("import specifier list is too large");
            return false;
        }
        new_cap *= 2;
    }
    SymbolId* new_imports = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * new_cap);
    SymbolId* new_aliases = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * new_cap);
    if (new_imports == null || new_aliases == null) {
        self.set_error("out of memory while growing import specifier list");
        return false;
    }
    if (e.import_expr.import_count > 0 &&
        (e.import_expr.imports == null || e.import_expr.aliases == null)) {
        self.set_error("out of memory while growing import specifier list");
        return false;
    }
    for (usz i = 0; i < e.import_expr.import_count; i++) {
        new_imports[i] = e.import_expr.imports[i];
        new_aliases[i] = e.import_expr.aliases[i];
    }
    e.import_expr.imports = new_imports;
    e.import_expr.aliases = new_aliases;
    e.import_expr.import_capacity = new_cap;
    return true;
}
