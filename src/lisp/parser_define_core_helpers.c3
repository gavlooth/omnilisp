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
