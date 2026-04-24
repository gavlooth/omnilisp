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
    self.init_import_spec(&e.import_expr);
}

fn void Parser.init_import_spec(Parser* self, ExprImport* imp) {
    imp.name = (SymbolId)0;
    imp.has_path = false;
    imp.path = null;
    imp.path_len = 0;
    imp.import_count = 0;
    imp.import_capacity = 0;
    imp.imports = null;
    imp.aliases = null;
    imp.import_all = false;
}

fn void Parser.init_with_module_expr(Parser* self, Expr* e) {
    e.tag = E_WITH_MODULE;
    e.with_module = (ExprWithModule*)self.alloc_ast_bytes(ExprWithModule.sizeof, "out of memory while allocating scoped module open");
    if (e.with_module == null) return;
    self.init_import_spec(&e.with_module.import_expr);
    e.with_module.body = null;
    e.with_module.body_count = 0;
    e.with_module.body_capacity = 0;
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
    usz buffer_bytes = 0;
    if (!lisp_checked_mul_usz(SymbolId.sizeof, new_cap, &buffer_bytes)) {
        self.set_error("out of memory while growing import specifier list");
        return false;
    }
    SymbolId* new_imports = (SymbolId*)self.alloc_ast_bytes(buffer_bytes, "out of memory while growing import specifier list");
    SymbolId* new_aliases = (SymbolId*)self.alloc_ast_bytes(buffer_bytes, "out of memory while growing import specifier list");
    if (new_imports == null || new_aliases == null) return false;
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
