module lisp;

import std::core::mem;

fn Expr* Parser.parse_constructor_type_application(Parser* self) {
    if (self.has_error) return null;

    Expr* call_expr = self.alloc_expr_here();
    self.lexer.advance();  // consume '^'

    if (self.lexer.current.type == T_RPAREN) {
        self.set_error("^: expected type form and value");
        return null;
    }

    // Parse the type form as datum so constructor/type args are not evaluated.
    Value* type_form = self.parse_datum();
    if (self.has_error || type_form == null) return null;

    if (self.lexer.current.type == T_RPAREN) {
        self.set_error("^: expected value expression");
        return null;
    }

    Expr* target_expr = self.parse_expr();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;

    Expr* type_form_expr = self.interp.alloc_expr();
    type_form_expr.tag = E_QUOTE;
    type_form_expr.quote.datum = type_form;
    type_form_expr.loc_line = call_expr.loc_line;
    type_form_expr.loc_column = call_expr.loc_column;

    Expr* type_apply_fn = self.interp.alloc_expr();
    type_apply_fn.tag = E_VAR;
    type_apply_fn.var_expr.name = self.interp.symbols.intern("^");
    type_apply_fn.loc_line = call_expr.loc_line;
    type_apply_fn.loc_column = call_expr.loc_column;

    call_expr.tag = E_CALL;
    call_expr.call = (ExprCall*)mem::malloc(ExprCall.sizeof);
    call_expr.call.func = type_apply_fn;
    call_expr.call.arg_count = 2;
    call_expr.call.args = (Expr**)mem::malloc(Expr*.sizeof * 2);
    call_expr.call.args[0] = type_form_expr;
    call_expr.call.args[1] = target_expr;
    return call_expr;
}
