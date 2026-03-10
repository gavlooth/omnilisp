module lisp;

import std::collections::list;

fn bool Parser.parse_shorthand_rest_param(Parser* self, bool* has_rest_out, SymbolId* rest_param_out) {
    *has_rest_out = false;
    *rest_param_out = 0;
    if (self.lexer.current.type != T_DOTDOT) return true;

    self.lexer.advance();  // consume '..'
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected rest parameter name after ..");
        return false;
    }

    *rest_param_out = self.get_current_symbol();
    *has_rest_out = true;
    self.lexer.advance();
    return true;
}

fn Expr* Parser.parse_shorthand_define_body(
    Parser* self,
    Expr* e,
    List{Pattern*} destr_patterns,
    List{SymbolId} destr_param_names
) {
    self.expect(T_RPAREN, ")");  // close param list
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    return self.wrap_body_with_destructuring(e, body, destr_patterns, destr_param_names);
}

fn Expr* Parser.build_shorthand_define_lambda(
    Parser* self,
    Expr* e,
    List{SymbolId} params,
    List{TypeAnnotation} define_param_anns,
    bool define_has_typed,
    bool define_has_rest,
    SymbolId define_rest_param,
    Expr* body
) {
    Expr* lambda_expr = self.interp.alloc_expr();
    lambda_expr.loc_line = e.loc_line;
    lambda_expr.loc_column = e.loc_column;
    self.finalize_lambda_expr(
        lambda_expr,
        params,
        define_param_anns,
        define_has_typed,
        define_has_rest,
        define_rest_param,
        body
    );
    return lambda_expr;
}

fn Expr* Parser.parse_shorthand_define(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in shorthand define");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    List{SymbolId} params;
    List{TypeAnnotation} define_param_anns;
    bool define_has_typed = false;
    bool define_has_rest = false;
    SymbolId define_rest_param = 0;
    List{Pattern*} destr_patterns;
    List{SymbolId} destr_param_names;

    if (!self.collect_callable_params(&params, &define_param_anns, &define_has_typed, &destr_patterns, &destr_param_names)) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    if (!self.parse_shorthand_rest_param(&define_has_rest, &define_rest_param)) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    Expr* body = self.parse_shorthand_define_body(e, destr_patterns, destr_param_names);
    if (body == null) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    Expr* lambda_expr = self.build_shorthand_define_lambda(
        e,
        params,
        define_param_anns,
        define_has_typed,
        define_has_rest,
        define_rest_param,
        body
    );
    params.free();
    define_param_anns.free();
    destr_patterns.free();
    destr_param_names.free();

    e.tag = E_DEFINE;
    e.define.name = name;
    e.define.value = lambda_expr;
    return e;
}

fn Expr* Parser.parse_normal_define(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected name after define");
        return null;
    }

    SymbolId name = self.get_current_symbol();
    self.lexer.advance();
    Expr* value = self.parse_expr();
    self.expect(T_RPAREN, ")");

    e.tag = E_DEFINE;
    e.define.name = name;
    e.define.value = value;
    return e;
}

fn Expr* Parser.parse_define(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'define' location
    self.lexer.advance();  // consume 'define'

    // Check for bracket annotation: (define [attr...] ...)
    if (self.lexer.current.type == T_LBRACKET) {
        self.lexer.advance();  // consume '['
        List{SymbolId} attrs;
        defer attrs.free();
        if (!self.parse_define_attrs(&attrs)) return null;
        return self.parse_define_with_annotation(e, &attrs);
    }

    // Shorthand define: (define (name params...) body) => (define name (lambda (params...) body))
    // Also supports typed params: (define (name (^Int x) (^String y)) body)
    if (self.lexer.current.type == T_LPAREN) {
        self.lexer.advance();  // consume '('
        return self.parse_shorthand_define(e);
    }

    return self.parse_normal_define(e);
}
