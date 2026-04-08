module lisp;

import std::collections::list;

fn Expr* Parser.build_named_let_call(Parser* self, Expr* e, SymbolId loop_name, List{Expr*}* inits) {
    usz arg_count = (*inits).len();
    Expr* call_expr = self.alloc_expr();
    if (call_expr == null) return null;
    call_expr.tag = E_CALL;
    call_expr.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating named let call");
    if (call_expr.call == null) return null;
    call_expr.loc_line = e.loc_line;
    call_expr.loc_column = e.loc_column;
    Expr* name_ref = self.alloc_expr();
    if (name_ref == null) return null;
    name_ref.tag = E_VAR;
    name_ref.loc_line = e.loc_line;
    name_ref.loc_column = e.loc_column;
    name_ref.var_expr.name = loop_name;
    call_expr.call.func = name_ref;
    call_expr.call.arg_count = arg_count;
    call_expr.call.args = null;
    if (arg_count > 0) {
        call_expr.call.args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * arg_count, "out of memory while allocating named let call arguments");
        if (call_expr.call.args == null) return null;
    }
    for (usz i = 0; i < arg_count; i++) {
        call_expr.call.args[i] = (*inits)[i];
    }
    return call_expr;
}

fn Expr* Parser.build_named_let_arg_ref(Parser* self, Expr* e, SymbolId name) {
    Expr* name_ref = self.alloc_expr();
    if (name_ref == null) return null;
    name_ref.tag = E_VAR;
    name_ref.loc_line = e.loc_line;
    name_ref.loc_column = e.loc_column;
    name_ref.var_expr.name = name;
    return name_ref;
}

fn Expr* Parser.build_named_let_sequential_body(Parser* self, Expr* e, SymbolId loop_name, List{SymbolId}* params, List{Expr*}* inits, Expr* lambda_expr) {
    usz param_count = (*params).len();
    Expr* call_expr = self.build_named_let_call(e, loop_name, inits);
    if (call_expr == null) return null;
    for (usz i = 0; i < param_count; i++) {
        call_expr.call.args[i] = self.build_named_let_arg_ref(e, (*params)[i]);
        if (call_expr.call.args[i] == null) return null;
    }

    Expr* recursive_let = self.alloc_expr();
    if (recursive_let == null) return null;
    recursive_let.tag = E_LET;
    recursive_let.loc_line = e.loc_line;
    recursive_let.loc_column = e.loc_column;
    recursive_let.let_expr.name = loop_name;
    recursive_let.let_expr.init = lambda_expr;
    recursive_let.let_expr.body = call_expr;
    recursive_let.let_expr.is_recursive = true;
    recursive_let.let_expr.pattern = null;

    List{Pattern*} patterns;
    for (usz i = 0; i < param_count; i++) {
        patterns.push(null);
    }

    Expr* result = self.build_let_expr_chain(e, recursive_let, *params, patterns, *inits, false);
    patterns.free();
    return result;
}
