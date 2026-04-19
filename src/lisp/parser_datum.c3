module lisp;

/**
 * Parse a datum (for quote).
 */
fn Value* Parser.parse_datum(Parser* self) {
    return self.parse_datum_impl(false);
}

/**
 * Parse a template datum (for macro templates).
 * Like parse_datum() but also handles T_DOTDOT as a ".." symbol,
 * and T_UNDERSCORE as a "_" symbol.
 */
fn Value* Parser.parse_template_datum(Parser* self) {
    return self.parse_datum_impl(true);
}

fn Value* Parser.parse_datum_template_only(
    Parser* self,
    bool template_mode,
    SymbolId sym
) {
    if (!template_mode) {
        self.set_error("unexpected token in datum");
        return null;
    }
    self.lexer.advance();
    return parser_make_symbol_datum(self.interp, sym);
}

fn Value* Parser.parse_datum_string(Parser* self) {
    Value* v = parser_make_string_datum(self);
    if (v == null) return null;
    self.lexer.advance();
    return v;
}

fn Value* Parser.parse_datum_quote(Parser* self, bool template_mode) {
    self.lexer.advance();
    Value* inner = self.parse_datum_impl(template_mode);
    if (self.has_error) return null;
    return parser_make_quote_datum(self.interp, inner);
}

fn Value* Parser.parse_datum_impl(Parser* self, bool template_mode) {
    if (self.has_error) return null;
    Lexer* lex = self.lexer;

    switch (lex.current.type) {
        case T_DOTDOT:
            return self.parse_datum_template_only(template_mode, self.interp.sym_dotdot);
        case T_UNDERSCORE:
            return self.parse_datum_template_only(template_mode, self.intern_or_error("_", "parser: failed to intern template underscore symbol"));
        case T_INT: {
            long val = lex.current.int_value;
            lex.advance();
            return parser_make_int_datum(self.interp, val);
        }
        case T_SYMBOL: {
            SymbolId sym = self.get_current_symbol();
            lex.advance();
            return parser_make_symbol_datum(self.interp, sym);
        }
        case T_STRING:
            return self.parse_datum_string();
        case T_QUOTE:
            return self.parse_datum_quote(template_mode);
        case T_LPAREN:
            return self.parse_datum_list(template_mode);
        case T_LBRACE:
            if (template_mode) return self.parse_datum_collection_template(true);
            self.set_error("dict literal is not supported in datum");
            return null;
        case T_LBRACKET:
            if (template_mode) return self.parse_datum_collection_template(false);
            self.set_error("array literal is not supported in datum");
            return null;
        default:
            self.set_error(template_mode ? "unexpected token in macro template datum" : "unexpected token in datum");
            return null;
    }
}
