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

    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();
        Pattern* pat = self.parse_pattern();
        Value* tmpl = self.parse_template_datum();
        self.expect(T_RPAREN, ")");
        clauses.push({ .pattern = pat, .tmpl = tmpl });
    }
    if (clauses.len() > 0) {
        e.define_macro.clauses = (MacroClause*)mem::malloc(MacroClause.sizeof * clauses.len());
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

    SymbolId sym_relation = self.interp.symbols.intern("relation");
    if (annotation == sym_relation) {
        return self.parse_define_relation_attr(e, attrs);
    }

    SymbolId sym_schema = self.interp.symbols.intern("schema");
    if (annotation == sym_schema) {
        return self.parse_define_schema_attr(e);
    }

    self.set_error("unknown define attribute");
    return null;
}
