module lisp;

import std::collections::list;

fn Expr* Parser.build_named_let_call(Parser* self, Expr* e, SymbolId loop_name, List{Expr*}* inits) {
    usz arg_count = (*inits).len();
    Expr* call_expr = self.interp.alloc_expr();
    call_expr.tag = E_CALL;
    call_expr.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    call_expr.loc_line = e.loc_line;
    call_expr.loc_column = e.loc_column;
    Expr* name_ref = self.interp.alloc_expr();
    name_ref.tag = E_VAR;
    name_ref.loc_line = e.loc_line;
    name_ref.loc_column = e.loc_column;
    name_ref.var_expr.name = loop_name;
    call_expr.call.func = name_ref;
    call_expr.call.arg_count = arg_count;
    call_expr.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * arg_count);
    for (usz i = 0; i < arg_count; i++) {
        call_expr.call.args[i] = (*inits)[i];
    }
    return call_expr;
}

fn Expr* Parser.build_named_let_arg_ref(Parser* self, Expr* e, SymbolId name) {
    Expr* name_ref = self.interp.alloc_expr();
    name_ref.tag = E_VAR;
    name_ref.loc_line = e.loc_line;
    name_ref.loc_column = e.loc_column;
    name_ref.var_expr.name = name;
    return name_ref;
}

fn Expr* Parser.build_named_let_sequential_body(Parser* self, Expr* e, SymbolId loop_name, List{SymbolId}* params, List{Expr*}* inits, Expr* lambda_expr) {
    usz param_count = (*params).len();
    Expr* call_expr = self.build_named_let_call(e, loop_name, inits);
    for (usz i = 0; i < param_count; i++) {
        call_expr.call.args[i] = self.build_named_let_arg_ref(e, (*params)[i]);
    }

    Expr* recursive_let = self.interp.alloc_expr();
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
