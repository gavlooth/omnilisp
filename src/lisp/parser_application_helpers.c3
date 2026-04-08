module lisp;

import std::collections::list;

fn bool Parser.collect_application_args(Parser* self, List{Expr*}* args) {
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        args.push(self.parse_expr());
    }
    return !self.has_error;
}

fn Expr* Parser.build_call_expr(Parser* self, Expr* func, List{Expr*}* args, usz line, usz col) {
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating call expression");
    if (e.call == null) return null;
    e.loc_line = line;
    e.loc_column = col;
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = null;
    if (args.len() > 0) {
        e.call.args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * args.len(), "out of memory while allocating call arguments");
        if (e.call.args == null) return null;
    }
    for (usz i = 0; i < args.len(); i++) {
        e.call.args[i] = (*args)[i];
    }
    return e;
}
