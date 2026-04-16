module lisp;

fn bool parse_explain_selector_symbol(Parser* self, Expr* selector_expr, SymbolId* selector_out, bool* quoted_symbol_out) {
    *quoted_symbol_out = false;
    if (selector_expr == null || selector_expr.tag != E_QUOTE) return false;

    Value* datum = selector_expr.quote.datum;
    if (datum == null || datum.tag != SYMBOL) return false;

    *quoted_symbol_out = true;
    SymbolId selector = datum.sym_val;
    SymbolId dispatch_sym = self.intern_or_error("dispatch", "explain: failed to intern dispatch selector");
    if (dispatch_sym == INVALID_SYMBOL_ID) return false;
    if ((uint)selector == (uint)dispatch_sym || (uint)selector == (uint)self.interp.sym_effect) {
        *selector_out = selector;
        return true;
    }
    return false;
}

fn Expr* make_explain_target_thunk(Parser* self, Expr* body) {
    Expr* thunk = self.alloc_expr();
    if (thunk == null) return null;
    thunk.tag = E_LAMBDA;
    thunk.loc_line = body != null ? body.loc_line : 0;
    thunk.loc_column = body != null ? body.loc_column : 0;
    thunk.lambda = (ExprLambda*)self.alloc_ast_bytes(ExprLambda.sizeof, "out of memory while allocating explain thunk");
    if (thunk.lambda == null) return null;
    thunk.lambda.param = INVALID_SYMBOL_ID;
    thunk.lambda.params = null;
    thunk.lambda.param_count = 0;
    thunk.lambda.has_rest = false;
    thunk.lambda.rest_param = INVALID_SYMBOL_ID;
    thunk.lambda.body = body;
    thunk.lambda.param_annotations = null;
    thunk.lambda.has_typed_params = false;
    return thunk;
}

fn Expr* Parser.parse_explain(Parser* self) {
    if (self.has_error) return null;
    Expr* call_expr = self.alloc_expr_here();
    if (call_expr == null) return null;
    self.lexer.advance();  // consume 'explain'

    Expr* selector_expr = self.parse_expr();
    Expr* target_expr = self.parse_expr();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;

    SymbolId selector = (SymbolId)0;
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
    if (thunk_expr == null || self.has_error) return null;

    Expr* explain_fn = self.alloc_expr();
    if (explain_fn == null) return null;
    explain_fn.tag = E_VAR;
    explain_fn.var_expr.name = self.intern_or_error("explain", "explain: failed to intern explain function name");
    if (explain_fn.var_expr.name == INVALID_SYMBOL_ID) return null;
    explain_fn.loc_line = call_expr.loc_line;
    explain_fn.loc_column = call_expr.loc_column;

    call_expr.tag = E_CALL;
    call_expr.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating explain call");
    if (call_expr.call == null) return null;
    call_expr.call.func = explain_fn;
    call_expr.call.arg_count = 2;
    call_expr.call.args = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, 2, "out of memory while allocating explain call args");
    if (call_expr.call.args == null) return null;
    call_expr.call.args[0] = selector_expr;
    call_expr.call.args[1] = thunk_expr;
    return call_expr;
}
