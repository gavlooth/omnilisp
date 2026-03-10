module lisp;

import std::collections::list;

fn Expr* Parser.parse_lambda(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'lambda' location
    self.lexer.advance();  // consume 'lambda'

    self.expect(T_LPAREN, "(");

    if (self.lexer.current.type == T_RPAREN) {
        self.lexer.advance();  // consume ')'
        return self.finalize_lambda_no_fixed_params(e, false, 0);
    }

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        SymbolId rest_param;
        if (!self.parse_rest_param(&rest_param)) return null;
        self.expect(T_RPAREN, ")");
        return self.finalize_lambda_no_fixed_params(e, true, rest_param);
    }

    List{SymbolId} params;
    defer params.free();
    List{TypeAnnotation} param_anns;
    defer param_anns.free();
    bool has_any_typed = false;
    List{Pattern*} lam_destr_patterns;
    defer lam_destr_patterns.free();
    List{SymbolId} lam_destr_param_names;
    defer lam_destr_param_names.free();

    if (!self.collect_callable_params(&params, &param_anns, &has_any_typed, &lam_destr_patterns, &lam_destr_param_names)) {
        return null;
    }

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        SymbolId rest_param;
        if (!self.parse_rest_param(&rest_param)) return null;
        self.expect(T_RPAREN, ")");
        return self.finalize_lambda_with_params(e, params, param_anns, has_any_typed, true, rest_param, lam_destr_patterns, lam_destr_param_names, false);
    }

    self.expect(T_RPAREN, ")");
    return self.finalize_lambda_with_params(e, params, param_anns, has_any_typed, false, 0, lam_destr_patterns, lam_destr_param_names, true);
}

fn Expr* Parser.parse_if(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'if' location
    self.lexer.advance();  // consume 'if'

    Expr* test = self.parse_expr();
    Expr* then_branch = self.parse_expr();
    Expr* else_branch = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_IF;
    e.if_expr.test = test;
    e.if_expr.then_branch = then_branch;
    e.if_expr.else_branch = else_branch;
    return e;
}
