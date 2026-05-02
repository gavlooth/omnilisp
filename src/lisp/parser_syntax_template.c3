module lisp;

import std::collections::list;

/**
 * Parse a #syntax reader template.
 * #syntax datum produces E_QUASIQUOTE AST nodes, reusing the JIT/AOT
 * quasiquote infrastructure. Inside a #syntax body:
 *   #{expr}       means unquote (like ,expr in quasiquote)
 *   #{.. expr}    means splice (like ,@expr in quasiquote)
 */
fn Expr* Parser.parse_syntax_template(Parser* self) {
    if (self.has_error) return null;
    self.lexer.advance();  // consume #syntax tag token
    Expr* body = self.parse_syntax_datum();
    if (body == null || self.has_error) return null;

    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_QUASIQUOTE;
    e.quasiquote.body = body;
    return e;
}

fn Expr* Parser.parse_syntax_datum(Parser* self) {
    if (self.has_error) return null;
    switch (self.lexer.current.type) {
        case T_HASH_LBRACE:
            return self.parse_syntax_interpolation();
        case T_BACKTICK:
            return self.parse_qq_backtick();
        case T_COMMA:
            self.set_error("unexpected comma in #syntax template (use #{expr} for unquote)");
            return null;
        case T_COMMA_AT:
            self.set_error("unexpected comma-at in #syntax template (use #{.. expr} for splice)");
            return null;
        case T_QUOTE:
            return self.parse_qq_quote_datum();
        case T_INT:
            return self.parse_qq_int_literal();
        case T_STRING:
            return self.parse_qq_string_literal();
        case T_SYMBOL:
            return self.parse_qq_var_symbol(self.get_current_symbol());
        case T_LPAREN:
            return self.parse_syntax_list_form();
        case T_UNDERSCORE:
            return self.parse_qq_var_symbol(self.intern_or_error("_", "parser: failed to intern syntax underscore symbol"));
        case T_READER_TAG:
            // Allow nested #syntax templates
            bool is_nested_syntax = self.lexer.current.text_len == 6;
            if (is_nested_syntax) {
                char[] tag_text = self.lexer.current.text[:6];
                is_nested_syntax = tag_text[0] == 's' && tag_text[1] == 'y' && tag_text[2] == 'n'
                                 && tag_text[3] == 't' && tag_text[4] == 'a' && tag_text[5] == 'x';
            }
            if (is_nested_syntax) {
                return self.parse_syntax_template();
            }
            self.set_error("unexpected reader tag in #syntax template");
            return null;
        default:
            self.set_error("unexpected token in #syntax template");
            return null;
    }
}

fn Expr* Parser.parse_syntax_interpolation(Parser* self) {
    if (self.has_error) return null;
    self.lexer.advance();  // consume #{

    bool is_splice = false;
    if (self.lexer.current.type == T_DOTDOT) {
        is_splice = true;
        self.lexer.advance();  // consume ..
    }

    Expr* inner = self.parse_expr();
    if (inner == null || self.has_error) return null;

    if (self.lexer.current.type != T_RBRACE) {
        self.set_error(is_splice
            ? "expected } to close #{.. expr} splice in #syntax template"
            : "expected } to close #{expr} unquote in #syntax template");
        return null;
    }
    self.lexer.advance();  // consume }

    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    if (is_splice) {
        e.tag = E_UNQUOTE_SPLICING;
        e.unquote_splicing.body = inner;
    } else {
        e.tag = E_UNQUOTE;
        e.unquote.body = inner;
    }
    return e;
}

fn Expr* Parser.parse_syntax_list_form(Parser* self) {
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type == T_RPAREN) {
        Expr* e = self.alloc_expr_here();
        if (e == null) return null;
        self.lexer.advance();
        e.tag = E_LIT;
        e.lit.value = self.alloc_root_value_or_error("out of memory while parsing syntax literal");
        if (e.lit.value == null) return null;
        e.lit.value.tag = NIL;
        return e;
    }

    Expr* func = self.parse_syntax_datum();
    List{Expr*} syntax_args;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        syntax_args.push(self.parse_syntax_datum());
    }
    if (self.has_error) { syntax_args.free(); return null; }
    self.lexer.advance();  // consume ')'

    Expr* e = self.alloc_expr();
    if (e == null) { syntax_args.free(); return null; }
    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating syntax call");
    if (e.call == null) { syntax_args.free(); return null; }
    e.call.func = func;
    e.call.arg_count = syntax_args.len();
    e.call.args = null;
    if (syntax_args.len() > 0) {
        e.call.args = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, syntax_args.len(), "out of memory while allocating syntax call arguments");
        if (e.call.args == null) { syntax_args.free(); return null; }
    }
    for (usz i = 0; i < syntax_args.len(); i++) { e.call.args[i] = syntax_args[i]; }
    syntax_args.free();
    return e;
}
