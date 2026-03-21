module lisp;

import std::collections::list;

fn bool Parser.collect_application_args(Parser* self, List{Expr*}* args) {
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        args.push(self.parse_expr());
    }
    return !self.has_error;
}

fn Expr* Parser.build_call_expr(Parser* self, Expr* func, List{Expr*}* args, usz line, usz col) {
    Expr* e = self.interp.alloc_expr();
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    e.loc_line = line;
    e.loc_column = col;
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) {
        e.call.args[i] = (*args)[i];
    }
    return e;
}

fn bool Parser.reject_nullary_accessor_shorthand_call(Parser* self, Expr* func, usz arg_count) {
    if (func == null || func.tag != E_LAMBDA || func.lambda == null) return false;
    if (!func.lambda.is_accessor_shorthand || arg_count != 0) return false;

    self.set_error("leading-dot accessor cannot be parenthesized on its own; use .expr as a value or apply it directly");
    return true;
}
