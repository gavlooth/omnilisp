module lisp;

import std::collections::list;

fn Expr* Compiler.make_synthetic_lambda(Compiler* self, Expr* source_expr, SymbolId param, Expr* body) {
    if (param == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: failed to intern synthetic lambda parameter");
        return null;
    }
    Expr* wrapper = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    if (wrapper == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic lambda");
        return null;
    }
    wrapper.tag = E_LAMBDA;
    wrapper.lambda = (ExprLambda*)ast_arena_alloc(&self.interp.ast_arena, ExprLambda.sizeof);
    if (wrapper.lambda == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic lambda");
        return null;
    }
    wrapper.loc_line = source_expr.loc_line;
    wrapper.loc_column = source_expr.loc_column;
    wrapper.lambda.param = param;
    wrapper.lambda.param_count = 1;
    wrapper.lambda.params = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof);
    if (wrapper.lambda.params == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic lambda params");
        return null;
    }
    wrapper.lambda.params[0] = param;
    wrapper.lambda.param_annotations = null;
    wrapper.lambda.has_rest = false;
    wrapper.lambda.rest_param = (SymbolId)0;
    wrapper.lambda.has_typed_params = false;
    wrapper.lambda.body = body;
    return wrapper;
}

fn Expr* Compiler.make_pair_projection_app(Compiler* self, SymbolId fn_name, SymbolId pair_param) {
    if (fn_name == INVALID_SYMBOL_ID || pair_param == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: invalid synthetic pair projection symbol");
        return null;
    }
    Expr* pair_ref = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    Expr* proj_fn = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    Expr* proj_call = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    if (pair_ref == null || proj_fn == null || proj_call == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic pair projection");
        return null;
    }
    pair_ref.tag = E_VAR;
    pair_ref.var_expr.name = pair_param;
    proj_fn.tag = E_VAR;
    proj_fn.var_expr.name = fn_name;
    proj_call.tag = E_APP;
    proj_call.app.func = proj_fn;
    proj_call.app.arg = pair_ref;
    return proj_call;
}

fn Expr* Compiler.make_projection_app_from_expr(Compiler* self, SymbolId fn_name, Expr* arg_expr) {
    if (fn_name == INVALID_SYMBOL_ID || arg_expr == null) {
        self.set_compile_error("compiler: invalid synthetic projection expression");
        return null;
    }
    Expr* proj_fn = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    Expr* proj_call = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    if (proj_fn == null || proj_call == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic projection");
        return null;
    }
    proj_fn.tag = E_VAR;
    proj_fn.var_expr.name = fn_name;
    proj_call.tag = E_APP;
    proj_call.app.func = proj_fn;
    proj_call.app.arg = arg_expr;
    return proj_call;
}

fn Expr* Compiler.wrap_handle_clause_body(Compiler* self, Expr* source_expr, Expr* orig_handler, SymbolId pair_param, SymbolId k_name, SymbolId arg_name, SymbolId sym_car, SymbolId sym_cdr) {
    Expr* cdr_call = self.make_pair_projection_app(sym_cdr, pair_param);
    if (cdr_call == null) return null;
    Expr* arg_call = self.make_projection_app_from_expr(sym_car, cdr_call);
    if (arg_call == null) return null;
    Expr* let_arg = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    if (let_arg == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic handle arg binding");
        return null;
    }
    let_arg.tag = E_LET;
    let_arg.let_expr.name = arg_name;
    let_arg.let_expr.init = arg_call;
    let_arg.let_expr.body = orig_handler;
    let_arg.let_expr.is_recursive = false;

    Expr* car_call = self.make_pair_projection_app(sym_car, pair_param);
    if (car_call == null) return null;
    Expr* let_k = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
    if (let_k == null) {
        self.set_compile_error("compiler: out of memory while allocating synthetic handle continuation binding");
        return null;
    }
    let_k.tag = E_LET;
    let_k.let_expr.name = k_name;
    let_k.let_expr.init = car_call;
    let_k.let_expr.body = let_arg;
    let_k.let_expr.is_recursive = false;

    return self.make_synthetic_lambda(source_expr, pair_param, let_k);
}

fn bool Compiler.scan_lambdas_reset(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.reset.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    if (wrapper == null) return false;
    expr.reset.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound, enclosing_mutable);
}

fn bool Compiler.scan_lambdas_shift(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    Expr* orig_body = expr.shift.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, expr.shift.k_name, orig_body);
    if (wrapper == null) return false;
    expr.shift.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound, enclosing_mutable);
}

fn bool Compiler.scan_lambdas_handle(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.handle.body;
    Expr* body_wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    if (body_wrapper == null) return false;
    expr.handle.body = body_wrapper;
    bool h_found = self.scan_lambdas_with_scope(body_wrapper, enclosing_bound, enclosing_mutable);

    SymbolId pair_param = self.interp.symbols.intern("_eff_pair");
    SymbolId sym_car = self.interp.symbols.intern("car");
    SymbolId sym_cdr = self.interp.symbols.intern("cdr");
    if (pair_param == INVALID_SYMBOL_ID || sym_car == INVALID_SYMBOL_ID || sym_cdr == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: failed to intern synthetic handle wrapper symbols");
        return h_found;
    }
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        if (expr.handle.clauses[i].has_effect_tag_expr) {
            h_found |= self.scan_lambdas_with_scope(expr.handle.clauses[i].effect_tag_expr, enclosing_bound, enclosing_mutable);
        }
        Expr* orig_handler = expr.handle.clauses[i].handler_body;
        SymbolId k_name = expr.handle.clauses[i].k_name;
        SymbolId arg_name = expr.handle.clauses[i].arg_name;
        Expr* handler_wrapper = self.wrap_handle_clause_body(expr, orig_handler, pair_param, k_name, arg_name, sym_car, sym_cdr);
        if (handler_wrapper == null) return h_found;
        expr.handle.clauses[i].handler_body = handler_wrapper;
        h_found |= self.scan_lambdas_with_scope(handler_wrapper, enclosing_bound, enclosing_mutable);
    }
    return h_found;
}
