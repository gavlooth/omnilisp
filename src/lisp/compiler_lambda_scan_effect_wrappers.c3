module lisp;

import std::collections::list;

fn Expr* Compiler.make_synthetic_lambda(Compiler* self, Expr* source_expr, SymbolId param, Expr* body) {
    Expr* wrapper = self.interp.alloc_expr();
    wrapper.tag = E_LAMBDA;
    wrapper.lambda = (ExprLambda*)ast_arena_alloc(&self.interp.ast_arena, ExprLambda.sizeof);
    wrapper.loc_line = source_expr.loc_line;
    wrapper.loc_column = source_expr.loc_column;
    wrapper.lambda.param = param;
    wrapper.lambda.param_count = 1;
    wrapper.lambda.params = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof);
    wrapper.lambda.params[0] = param;
    wrapper.lambda.param_annotations = null;
    wrapper.lambda.has_rest = false;
    wrapper.lambda.rest_param = 0;
    wrapper.lambda.has_typed_params = false;
    wrapper.lambda.body = body;
    return wrapper;
}

fn Expr* Compiler.make_pair_projection_app(Compiler* self, SymbolId fn_name, SymbolId pair_param) {
    Expr* pair_ref = self.interp.alloc_expr();
    pair_ref.tag = E_VAR;
    pair_ref.var_expr.name = pair_param;
    Expr* proj_fn = self.interp.alloc_expr();
    proj_fn.tag = E_VAR;
    proj_fn.var_expr.name = fn_name;
    Expr* proj_call = self.interp.alloc_expr();
    proj_call.tag = E_APP;
    proj_call.app.func = proj_fn;
    proj_call.app.arg = pair_ref;
    return proj_call;
}

fn Expr* Compiler.wrap_handle_clause_body(Compiler* self, Expr* source_expr, Expr* orig_handler, SymbolId pair_param, SymbolId k_name, SymbolId arg_name, SymbolId sym_car, SymbolId sym_cdr) {
    Expr* cdr_call = self.make_pair_projection_app(sym_cdr, pair_param);
    Expr* let_arg = self.interp.alloc_expr();
    let_arg.tag = E_LET;
    let_arg.let_expr.name = arg_name;
    let_arg.let_expr.init = cdr_call;
    let_arg.let_expr.body = orig_handler;
    let_arg.let_expr.is_recursive = false;

    Expr* car_call = self.make_pair_projection_app(sym_car, pair_param);
    Expr* let_k = self.interp.alloc_expr();
    let_k.tag = E_LET;
    let_k.let_expr.name = k_name;
    let_k.let_expr.init = car_call;
    let_k.let_expr.body = let_arg;
    let_k.let_expr.is_recursive = false;

    return self.make_synthetic_lambda(source_expr, pair_param, let_k);
}

fn bool Compiler.scan_lambdas_reset(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.reset.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    expr.reset.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_shift(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    Expr* orig_body = expr.shift.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, expr.shift.k_name, orig_body);
    expr.shift.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_handle(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.handle.body;
    Expr* body_wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    expr.handle.body = body_wrapper;
    bool h_found = self.scan_lambdas_with_scope(body_wrapper, enclosing_bound);

    SymbolId pair_param = self.interp.symbols.intern("_eff_pair");
    SymbolId sym_car = self.interp.symbols.intern("car");
    SymbolId sym_cdr = self.interp.symbols.intern("cdr");
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        if (expr.handle.clauses[i].has_effect_tag_expr) {
            h_found |= self.scan_lambdas_with_scope(expr.handle.clauses[i].effect_tag_expr, enclosing_bound);
        }
        Expr* orig_handler = expr.handle.clauses[i].handler_body;
        SymbolId k_name = expr.handle.clauses[i].k_name;
        SymbolId arg_name = expr.handle.clauses[i].arg_name;
        Expr* handler_wrapper = self.wrap_handle_clause_body(expr, orig_handler, pair_param, k_name, arg_name, sym_car, sym_cdr);
        expr.handle.clauses[i].handler_body = handler_wrapper;
        h_found |= self.scan_lambdas_with_scope(handler_wrapper, enclosing_bound);
    }
    return h_found;
}
