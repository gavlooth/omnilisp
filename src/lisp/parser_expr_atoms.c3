module lisp;

fn Expr* Parser.parse_int_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    long val = self.lexer.current.int_value;
    self.lexer.advance();

    e.tag = E_LIT;
    e.lit.value = self.alloc_root_value_or_error("out of memory while parsing root literal");
    if (e.lit.value == null) return null;
    e.lit.value.tag = INT;
    e.lit.value.int_val = val;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_float_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    double dval = self.lexer.current.double_value;
    self.lexer.advance();

    e.tag = E_LIT;
    e.lit.value = self.alloc_root_value_or_error("out of memory while parsing root literal");
    if (e.lit.value == null) return null;
    e.lit.value.tag = DOUBLE;
    e.lit.value.double_val = dval;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_string_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = self.alloc_root_value_or_error("out of memory while parsing string literal");
    if (e.lit.value == null) return null;
    e.lit.value.tag = STRING;

    StringVal* builder = strval_new_from_slice(self.lexer.current.text[:self.lexer.current.text_len]);
    if (builder == null) {
        self.set_error("out of memory while parsing string literal");
        return null;
    }
    strval_into_value(builder, e.lit.value);
    self.lexer.advance();
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_symbol_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    char[] tok = self.lexer.current.text[:self.lexer.current.text_len];
    if (tok.len > 0 && tok[0] == '.') {
        self.set_error("leading-dot accessors were removed; use ref or access syntax like expr.[key]");
        return null;
    }

    SymbolId sym = self.get_current_symbol();
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_placeholder_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = self.interp.sym_placeholder;
    return e;
}

fn Expr* Parser.parse_expr(Parser* self) {
    if (self.has_error) return null;
    self.depth++;
    defer self.depth--;
    if (self.depth > self.max_depth) { self.set_error("expression nesting too deep"); return null; }
    switch (self.lexer.current.type) {
        case T_BACKTICK: return self.parse_backtick_shorthand();
        case T_COMMA: return self.parse_unquote_shorthand();
        case T_COMMA_AT: return self.parse_unquote_splicing_shorthand();
        case T_HASH_UNDERSCORE: return self.parse_form_comment_expr();
        case T_REGEX: return self.parse_regex_literal_expr();
        case T_READER_TAG: return self.parse_reader_tag_expr();
        case T_QUOTE: return self.parse_quote_shorthand();
        case T_INT: return self.parse_int_literal_expr();
        case T_FLOAT: return self.parse_float_literal_expr();
        case T_STRING: return self.parse_string_literal_expr();
        case T_SYMBOL: return self.parse_symbol_expr();
        case T_PATH: return self.parse_path_expr();
        case T_LBRACE:
            self.lexer.advance();  // consume {
            return self.parse_dict_literal();
        case T_LBRACKET:
            self.lexer.advance();  // consume [
            return self.parse_array_literal();
        case T_LPAREN:
            self.lexer.advance();
            return self.parse_list_form();
        case T_UNDERSCORE:
            return self.parse_placeholder_expr();
        case T_EOF:
            self.set_error("unexpected end of input");
            return null;
        case T_ERROR:
            if (self.lexer.current.text_len > 0) {
                self.set_error(self.lexer.current.text[:self.lexer.current.text_len]);
            } else {
                self.set_error("unexpected lexer error");
            }
            return null;
        default:
            self.set_error("unexpected token in expression");
            return null;
    }

}
