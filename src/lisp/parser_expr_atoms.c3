module lisp;

fn Expr* Parser.parse_int_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    long val = self.lexer.current.int_value;
    self.lexer.advance();

    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = INT;
    e.lit.value.int_val = val;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_float_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    double dval = self.lexer.current.double_value;
    self.lexer.advance();

    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = DOUBLE;
    e.lit.value.double_val = dval;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_string_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = STRING;

    usz len = self.lexer.current.text_len;
    StringVal* builder = strval_new(len + 1);
    for (usz i = 0; i < len; i++) {
        builder.chars[i] = self.lexer.current.text[i];
    }
    builder.chars[len] = 0;
    builder.len = len;
    strval_into_value(builder, e.lit.value);
    self.lexer.advance();
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_symbol_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    char[] tok = self.lexer.current.text[:self.lexer.current.text_len];
    if (tok.len > 0 && tok[0] == '.') {
        Expr* acc = self.parse_dot_accessor_shorthand(tok, e.loc_line, e.loc_column);
        if (acc != null || self.has_error) return acc;
    }

    SymbolId sym = self.get_current_symbol();
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_placeholder_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
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
