module lisp;

import std::collections::list;

/**
 * Parse a named let (loop).
 *
 * (let name (x init-x y init-y) body...)
 *
 * Desugars to:
 * (let ^rec (name (lambda (x y) body)) (name init-x init-y))
 */
fn bool Parser.parse_named_let_bindings(Parser* self, List{SymbolId}* params, List{Expr*}* inits) {
    self.expect(T_LPAREN, "(");  // consume '(' of bindings list
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
    lambda_expr.lambda = mem::malloc(ExprLambda.sizeof);
    lambda_expr.loc_line = e.loc_line;
    lambda_expr.loc_column = e.loc_column;
    lambda_expr.lambda.param = (*params)[0];
    lambda_expr.lambda.param_count = param_count;
    lambda_expr.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * param_count);
    for (usz i = 0; i < param_count; i++) {
        lambda_expr.lambda.params[i] = (*params)[i];
    }
    lambda_expr.lambda.has_rest = false;
    lambda_expr.lambda.rest_param = 0;
    lambda_expr.lambda.has_typed_params = false;
    lambda_expr.lambda.param_annotations = null;
    lambda_expr.lambda.body = body;
    return lambda_expr;
}

fn Expr* Parser.build_named_let_call(Parser* self, Expr* e, SymbolId loop_name, List{Expr*}* inits) {
    usz arg_count = (*inits).len();
    Expr* call_expr = self.interp.alloc_expr();
    call_expr.tag = E_CALL;
    call_expr.call = mem::malloc(ExprCall.sizeof);
    call_expr.loc_line = e.loc_line;
    call_expr.loc_column = e.loc_column;
    Expr* name_ref = self.interp.alloc_expr();
    name_ref.tag = E_VAR;
    name_ref.loc_line = e.loc_line;
    name_ref.loc_column = e.loc_column;
    name_ref.var_expr.name = loop_name;
    call_expr.call.func = name_ref;
    call_expr.call.arg_count = arg_count;
    call_expr.call.args = (Expr**)mem::malloc(Expr*.sizeof * arg_count);
    for (usz i = 0; i < arg_count; i++) {
        call_expr.call.args[i] = (*inits)[i];
    }
    return call_expr;
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
    Expr* call_expr = self.build_named_let_call(e, loop_name, &inits);
    params.free();
    inits.free();

    // Assemble: (let ^rec ((name lambda)) call)
    e.tag = E_LET;
    e.let_expr.name = loop_name;
    e.let_expr.init = lambda_expr;
    e.let_expr.body = call_expr;
    e.let_expr.is_recursive = true;  // named let is always recursive
    return e;
}
