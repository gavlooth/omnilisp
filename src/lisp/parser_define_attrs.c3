module lisp;

import std::collections::list;

fn bool parser_symbol_is(Parser* self, SymbolId sym, char[] text) @inline {
    return (uint)sym == (uint)self.interp.symbols.intern(text);
}

fn bool parser_is_nil_datum(Value* v) @inline {
    return v != null && v.tag == NIL;
}

fn Value* Parser.rewrite_template_interpolation_datum(Parser* self, Value* tmpl) {
    if (tmpl == null || self.has_error) return null;
    if (tmpl.tag != CONS) return tmpl;

    Value* head = tmpl.cons_val.car;
    if (head != null && head.tag == SYMBOL && is_cons(tmpl.cons_val.cdr)) {
        Value* arg_cell = tmpl.cons_val.cdr;
        Value* arg = arg_cell.cons_val.car;
        Value* tail = arg_cell.cons_val.cdr;

        if (parser_symbol_is(self, head.sym_val, "insert")) {
            if (!parser_is_nil_datum(tail)) {
                self.set_error("template insert expects exactly one symbol argument");
                return null;
            }
            if (arg == null || arg.tag != SYMBOL) {
                self.set_error("template insert expects a symbol binding name");
                return null;
            }
            return arg;
        }

        if (parser_symbol_is(self, head.sym_val, "splice")) {
            if (!parser_is_nil_datum(tail)) {
                self.set_error("template splice expects exactly one symbol argument");
                return null;
            }
            if (arg == null || arg.tag != SYMBOL) {
                self.set_error("template splice expects a symbol binding name");
                return null;
            }
            Value* dotdot_sym = parser_make_symbol_datum(self.interp, self.interp.sym_dotdot);
            Value* arg_list = parser_make_cons_datum(self.interp, arg, parser_make_nil_datum(self.interp));
            return parser_make_cons_datum(self.interp, dotdot_sym, arg_list);
        }
    }

    Value* new_car = self.rewrite_template_interpolation_datum(tmpl.cons_val.car);
    if (self.has_error) return null;
    Value* new_cdr = self.rewrite_template_interpolation_datum(tmpl.cons_val.cdr);
    if (self.has_error) return null;
    tmpl.cons_val.car = new_car;
    tmpl.cons_val.cdr = new_cdr;
    return tmpl;
}

fn Value* Parser.parse_macro_template_form(Parser* self) {
    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("expected (template <datum>) in syntax-match clause");
        return null;
    }
    self.lexer.advance();  // consume '('
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected template form in syntax-match clause");
        return null;
    }
    SymbolId form_sym = self.get_current_symbol();
    if (!parser_symbol_is(self, form_sym, "template")) {
        self.set_error("expected (template <datum>) in syntax-match clause");
        return null;
    }
    self.lexer.advance();  // consume 'template'

    Value* tmpl = self.parse_template_datum();
    if (self.has_error) return null;
    tmpl = self.rewrite_template_interpolation_datum(tmpl);
    if (self.has_error) return null;

    self.expect(T_RPAREN, ")");
    return tmpl;
}

fn bool Parser.parse_macro_syntax_match_body(Parser* self, List{MacroClause}* clauses) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected syntax-match in single macro definition");
        return false;
    }
    SymbolId head = self.get_current_symbol();
    if (!parser_symbol_is(self, head, "syntax-match")) {
        self.set_error("expected syntax-match in single macro definition");
        return false;
    }
    self.lexer.advance();  // consume 'syntax-match'

    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume clause '('
        Pattern* pat = self.parse_pattern();
        if (self.has_error) return false;

        Value* tmpl = self.parse_macro_template_form();
        if (self.has_error) return false;

        self.expect(T_RPAREN, ")");
        if (self.has_error) return false;
        clauses.push({ .pattern = pat, .tmpl = tmpl });
    }

    if (clauses.len() == 0) {
        self.set_error("syntax-match requires at least one clause");
        return false;
    }

    self.expect(T_RPAREN, ")");  // consume ')' of syntax-match form
    return !self.has_error;
}

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
        !parser_symbol_is(self, self.get_current_symbol(), "syntax-match")) {
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
