module lisp;

import std::collections::list;

fn Expr* Parser.parse_qq_string_literal(Parser* self) {
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
    return e;
}

fn Expr* Parser.parse_qq_list_form(Parser* self) {
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type == T_RPAREN) {
        Expr* e = self.alloc_expr_here();
        self.lexer.advance();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
        return e;
    }

    Expr* func = self.parse_qq_template();
    List{Expr*} qq_args;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        qq_args.push(self.parse_qq_template());
    }
    if (self.has_error) { qq_args.free(); return null; }
    self.lexer.advance();  // consume ')'

    Expr* e = self.interp.alloc_expr();
    e.tag = E_CALL;
    e.call = mem::malloc(ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = qq_args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * qq_args.len());
    for (usz i = 0; i < qq_args.len(); i++) { e.call.args[i] = qq_args[i]; }
    qq_args.free();
    return e;
}

fn Expr* Parser.parse_qq_backtick(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = self.parse_qq_template();
    return e;
}

fn Expr* Parser.parse_qq_unquote(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE;
    e.unquote.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_qq_unquote_splicing(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_UNQUOTE_SPLICING;
    e.unquote_splicing.body = self.parse_expr();
    return e;
}

fn Expr* Parser.parse_qq_quote_datum(Parser* self) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_QUOTE;
    e.quote.datum = self.parse_datum();
    return e;
}

fn Expr* Parser.parse_qq_int_literal(Parser* self) {
    Expr* e = self.alloc_expr_here();
    long val = self.lexer.current.int_value;
    self.lexer.advance();
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = INT;
    e.lit.value.int_val = val;
    return e;
}

fn Expr* Parser.parse_qq_var_symbol(Parser* self, SymbolId sym) {
    Expr* e = self.alloc_expr_here();
    self.lexer.advance();
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
}

fn Expr* Parser.parse_qq_template(Parser* self) {
    if (self.has_error) return null;
    switch (self.lexer.current.type) {
        case T_BACKTICK:
            return self.parse_qq_backtick();
        case T_COMMA:
            return self.parse_qq_unquote();
        case T_COMMA_AT:
            return self.parse_qq_unquote_splicing();
        case T_QUOTE:
            return self.parse_qq_quote_datum();
        case T_INT:
            return self.parse_qq_int_literal();
        case T_STRING:
            return self.parse_qq_string_literal();
        case T_SYMBOL:
            return self.parse_qq_var_symbol(self.get_current_symbol());
        case T_LPAREN:
            return self.parse_qq_list_form();
        case T_UNDERSCORE:
            return self.parse_qq_var_symbol(self.interp.symbols.intern("_"));
        default:
            self.set_error("unexpected token in quasiquote template");
            return null;
    }
}
