module lisp;

import std::collections::list;

fn bool parser_symbol_is(Parser* self, SymbolId sym, char[] text) @inline {
    SymbolId expected = self.intern_or_error(text, "parser: failed to intern comparison symbol");
    return expected != INVALID_SYMBOL_ID && (uint)sym == (uint)expected;
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
