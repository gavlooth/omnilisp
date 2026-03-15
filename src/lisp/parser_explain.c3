module lisp;

fn bool parse_explain_selector_symbol(Parser* self, Expr* selector_expr, SymbolId* selector_out, bool* quoted_symbol_out) {
    *quoted_symbol_out = false;
    if (selector_expr == null || selector_expr.tag != E_QUOTE) return false;

    Value* datum = selector_expr.quote.datum;
    if (datum == null || datum.tag != SYMBOL) return false;

    *quoted_symbol_out = true;
    SymbolId selector = datum.sym_val;
    SymbolId dispatch_sym = self.interp.symbols.intern("dispatch");
    if ((uint)selector == (uint)dispatch_sym || (uint)selector == (uint)self.interp.sym_effect) {
        *selector_out = selector;
        return true;
    }
    return false;
}

fn Expr* make_explain_target_thunk(Parser* self, Expr* body) {
    Expr* thunk = self.interp.alloc_expr();
    thunk.tag = E_LAMBDA;
    thunk.loc_line = body != null ? body.loc_line : 0;
    thunk.loc_column = body != null ? body.loc_column : 0;
    thunk.lambda = (ExprLambda*)ast_arena_alloc(&self.interp.ast_arena, ExprLambda.sizeof);
    thunk.lambda.param = INVALID_SYMBOL_ID;
    thunk.lambda.params = null;
    thunk.lambda.param_count = 0;
    thunk.lambda.has_rest = false;
    thunk.lambda.rest_param = INVALID_SYMBOL_ID;
    thunk.lambda.body = body;
    thunk.lambda.param_annotations = null;
    thunk.lambda.has_typed_params = false;
    thunk.lambda.is_accessor_shorthand = false;
    return thunk;
}

fn Expr* Parser.parse_explain(Parser* self) {
    if (self.has_error) return null;
    Expr* call_expr = self.alloc_expr_here();
    self.lexer.advance();  // consume 'explain'

    Expr* selector_expr = self.parse_expr();
    Expr* target_expr = self.parse_expr();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;

    SymbolId selector = 0;
    bool quoted_symbol = false;
    if (!parse_explain_selector_symbol(self, selector_expr, &selector, &quoted_symbol)) {
        if (quoted_symbol) {
            self.set_error("explain: unknown selector; expected 'dispatch or 'effect");
        } else {
            self.set_error("explain: selector must be quoted symbol ('dispatch or 'effect)");
        }
        return null;
    }

    // Keep explain target unevaluated by wrapping it in a nullary thunk.
    Expr* thunk_expr = make_explain_target_thunk(self, target_expr);

    Expr* explain_fn = self.interp.alloc_expr();
    explain_fn.tag = E_VAR;
    explain_fn.var_expr.name = self.interp.symbols.intern("explain");
    explain_fn.loc_line = call_expr.loc_line;
    explain_fn.loc_column = call_expr.loc_column;

    call_expr.tag = E_CALL;
    call_expr.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    call_expr.call.func = explain_fn;
    call_expr.call.arg_count = 2;
    call_expr.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * 2);
    call_expr.call.args[0] = selector_expr;
    call_expr.call.args[1] = thunk_expr;
    return call_expr;
}
