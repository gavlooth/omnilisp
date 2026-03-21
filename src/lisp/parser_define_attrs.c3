module lisp;

import std::collections::list;

/**
 * Handle special bracket defines: [macro], [effect], [relation], [schema]
 */
fn Expr* Parser.parse_define_macro_attr(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected macro name");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    e.tag = E_DEFMACRO;
    e.define_macro.name = name;
    e.define_macro.clauses = null;
    e.define_macro.clause_count = 0;

    List{MacroClause} clauses;
    defer clauses.free();

    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("macro definition requires (syntax-match ...); legacy clause syntax was removed");
        return null;
    }

    self.lexer.advance();  // consume '('
    if (self.lexer.current.type != T_SYMBOL ||
        (uint)self.get_current_symbol() != (uint)self.interp.symbols.intern("syntax-match")) {
        self.set_error(
            "legacy macro clause syntax was removed; use (define [macro] name (syntax-match ([...] (template ...)) ...))"
        );
        return null;
    }
    if (!self.parse_macro_syntax_match_body(&clauses)) return null;

    if (clauses.len() > 0) {
        e.define_macro.clauses = (MacroClause*)ast_arena_alloc(&self.interp.ast_arena, MacroClause.sizeof * clauses.len());
        if (e.define_macro.clauses == null) {
            self.set_error("out of memory while parsing macro clauses");
            return null;
        }
        for (usz i = 0; i < clauses.len(); i++) e.define_macro.clauses[i] = clauses[i];
        e.define_macro.clause_count = clauses.len();
    }
    self.expect(T_RPAREN, ")");
    return e;
}

fn Expr* Parser.parse_define_schema_attr(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected schema name");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();
    Value* body = self.parse_datum();
    self.expect(T_RPAREN, ")");

    e.tag = E_DEFINE;
    e.define.name = name;
    Expr* quote_expr = self.interp.alloc_expr();
    quote_expr.tag = E_QUOTE;
    quote_expr.quote.datum = body;
    e.define.value = quote_expr;
    return e;
}

fn Expr* Parser.parse_define_special(Parser* self, Expr* e, SymbolId annotation, List{SymbolId}* attrs) {
    if (annotation == self.interp.sym_macro) {
        return self.parse_define_macro_attr(e);
    }

    if (annotation == self.interp.sym_effect) {
        return self.parse_defeffect(e);
    }

    if ((uint)annotation == (uint)self.interp.symbols.intern("relation")) {
        return self.parse_define_relation_attr(e, attrs);
    }

    if ((uint)annotation == (uint)self.interp.symbols.intern("schema")) {
        return self.parse_define_schema_attr(e);
    }

    self.set_error("unknown define attribute");
    return null;
}
