module lisp;

import std::io;
import std::collections::list;
import main;

fn bool Compiler.call_head_is(Compiler* self, Expr* expr, char[] name) {
    if (expr.call.func.tag != E_VAR) return false;
    char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
    return self.str_eq(fname, name);
}

fn bool Compiler.call_is_explain_thunked(Compiler* self, Expr* expr) {
    if (!self.call_head_is(expr, "explain")) return false;
    if (expr.call.arg_count != 2) return false;

    Expr* thunk = expr.call.args[1];
    if (thunk == null || thunk.tag != E_LAMBDA || thunk.lambda == null) return false;
    if (thunk.lambda.param_count != 0 || thunk.lambda.has_rest) return false;
    return true;
}
