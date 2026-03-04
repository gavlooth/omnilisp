module lisp;

/**
 * Parse a complete expression.
 */
fn Expr* Parser.parse_backtick_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = self.parse_qq_template();
    return e;
}

fn Expr* Parser.parse_unquote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE;
    e.unquote.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_unquote_splicing_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE_SPLICING;
    e.unquote_splicing.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_form_comment_expr(Parser* self) {
    long skip_count = self.lexer.current.int_value;
    self.lexer.advance();
    for (long i = 0; i < skip_count; i++) {
        Expr* discarded = self.parse_expr();
        if (discarded == null || self.has_error) return null;
    }
    return self.parse_expr();
}

fn Expr* Parser.parse_set_literal_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();  // consume #{

    Expr*[64] elems;
    usz elem_count = 0;
    while (self.lexer.current.type != T_RBRACE && self.lexer.current.type != T_EOF && !self.has_error) {
        if (elem_count >= 64) {
            self.set_error("set literal too large (max 64 elements)");
            return null;
        }
        elems[elem_count++] = self.parse_expr();
    }
    self.expect(T_RBRACE, "}");

    e.tag = E_CALL;
    usz total = elem_count + 1;
    Expr** args = (Expr**)mem::malloc(Expr*.sizeof * total);
    args[0] = self.interp.alloc_expr();
    args[0].tag = E_VAR;
    args[0].var_expr.name = self.interp.symbols.intern("set");
    for (usz i = 0; i < elem_count; i++) {
        args[i + 1] = elems[i];
    }
    e.call.args = args;
    e.call.arg_count = total;
    return e;
}

fn Expr* Parser.parse_regex_literal_expr(Parser* self) {
    char[] pat = self.lexer.current.text[:self.lexer.current.text_len];
    Expr* e = self.alloc_expr_here();
    e.tag = E_LIT;
    e.lit.value = make_string(self.interp, pat);
    self.lexer.advance();
    return e;
}

fn Expr* Parser.parse_quote_shorthand(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}

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
    StringVal* builder = strval_new(len);
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
    SymbolId sym = self.get_current_symbol();
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return self.parse_postfix_index(e);
}

fn Expr* Parser.parse_path_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    e.tag = E_PATH;
    e.path.segment_count = 0;

    usz text_len = self.lexer.current.text_len;
    usz start = 0;
    for (usz i = 0; i <= text_len; i++) {
        if (i == text_len || self.lexer.current.text[i] == '.') {
            if (i > start) {
                if (e.path.segment_count >= MAX_PATH_SEGMENTS) {
                    self.set_error("too many path segments");
                    return null;
                }
                usz seg_len = i - start;
                SymbolId seg_sym = self.interp.symbols.intern(self.lexer.current.text[start:seg_len]);
                e.path.segments[e.path.segment_count++] = seg_sym;
            }
            start = i + 1;
        }
    }
    self.lexer.advance();
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
    if (self.depth > 256) { self.set_error("expression nesting too deep (max 256)"); return null; }
    switch (self.lexer.current.type) {
        case T_BACKTICK: return self.parse_backtick_shorthand();
        case T_COMMA: return self.parse_unquote_shorthand();
        case T_COMMA_AT: return self.parse_unquote_splicing_shorthand();
        case T_HASH_UNDERSCORE: return self.parse_form_comment_expr();
        case T_HASH_LBRACE: return self.parse_set_literal_expr();
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
        default:
            self.set_error("unexpected token in expression");
            return null;
    }

}
