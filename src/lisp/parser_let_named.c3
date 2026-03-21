module lisp;

import std::collections::list;

/**
 * Parse a named let (loop).
 *
 * (let name (x init-x y init-y) body...)
 *
 * Desugars to:
 * (let (x init-x y init-y)
 *   (let ^rec (name (lambda (x y) body))
 *     (name x y)))
 */
fn bool Parser.parse_named_let_bindings(Parser* self, List{SymbolId}* params, List{Expr*}* inits) {
    if (self.lexer.current.type != T_LPAREN) {
        self.reject_legacy_let_binding_shape(true);
        if (self.has_error) return false;
    }

    self.expect(T_LPAREN, "(");  // consume '(' of bindings list
    if (self.has_error) return false;
    self.reject_legacy_let_binding_shape(true);
    if (self.has_error) return false;

    while (self.lexer.current.type == T_SYMBOL && !self.has_error) {
        (*params).push(self.get_current_symbol());
        self.lexer.advance();
        (*inits).push(self.parse_expr());
    }
    if (self.has_error) return false;

    self.expect(T_RPAREN, ")");  // consume ')' of bindings list
    if (self.has_error) return false;
    if ((*params).len() < 1) {
        self.set_error("expected at least one binding in named let");
        return false;
    }
    return true;
}

fn Expr* Parser.build_named_let_lambda(Parser* self, Expr* e, List{SymbolId}* params, Expr* body) {
    usz param_count = (*params).len();
    Expr* lambda_expr = self.interp.alloc_expr();
    lambda_expr.tag = E_LAMBDA;
    lambda_expr.lambda = (ExprLambda*)ast_arena_alloc(&self.interp.ast_arena, ExprLambda.sizeof);
    lambda_expr.loc_line = e.loc_line;
    lambda_expr.loc_column = e.loc_column;
    lambda_expr.lambda.param = (*params)[0];
    lambda_expr.lambda.param_count = param_count;
    lambda_expr.lambda.params = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * param_count);
    for (usz i = 0; i < param_count; i++) {
        lambda_expr.lambda.params[i] = (*params)[i];
    }
    lambda_expr.lambda.has_rest = false;
    lambda_expr.lambda.rest_param = 0;
    lambda_expr.lambda.has_typed_params = false;
    lambda_expr.lambda.param_annotations = null;
    lambda_expr.lambda.body = body;
    lambda_expr.lambda.is_accessor_shorthand = false;
    return lambda_expr;
}

fn Expr* Parser.parse_named_let(Parser* self, Expr* e, SymbolId loop_name) {
    if (self.has_error) return null;
    List{SymbolId} params;
    List{Expr*} inits;
    if (!self.parse_named_let_bindings(&params, &inits)) {
        params.free();
        inits.free();
        return null;
    }

    // Parse body (implicit block for multiple expressions)
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");  // consume outer ')' of let form

    Expr* lambda_expr = self.build_named_let_lambda(e, &params, body);
    Expr* result = self.build_named_let_sequential_body(e, loop_name, &params, &inits, lambda_expr);
    params.free();
    inits.free();
    return result;
}
