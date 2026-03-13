module lisp;

import std::collections::list;

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
    e.path.segments = null;

    List{SymbolId} segments;
    usz text_len = self.lexer.current.text_len;
    usz start = 0;
    for (usz i = 0; i <= text_len; i++) {
        if (i == text_len || self.lexer.current.text[i] == '.') {
            if (i > start) {
                usz seg_len = i - start;
                SymbolId seg_sym = self.interp.symbols.intern(self.lexer.current.text[start:seg_len]);
                segments.push(seg_sym);
            }
            start = i + 1;
        }
    }
    if (segments.len() == 0) {
        segments.free();
        self.set_error("empty path expression");
        return null;
    }
    e.path.segment_count = segments.len();
    e.path.segments = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * segments.len());
    if (e.path.segments == null) {
        segments.free();
        self.set_error("out of memory while parsing path expression");
        return null;
    }
    for (usz i = 0; i < segments.len(); i++) {
        e.path.segments[i] = segments[i];
    }
    segments.free();
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
